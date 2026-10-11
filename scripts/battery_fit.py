"""Fit battle-outcome predictors to a battle battery and compare them by cross-validation.

Usage: python scripts/battery_fit.py battles.jsonl [more.jsonl ...] [--weights out.json]

Each line of a battery (scripts/Invoke-Battery.ps1) holds both armies, the status and both sides' survivors by type.
Four predictors of P(we win) are compared on the same folds:
  value      our army is worth more (minerals + 1.5 gas)
  lanchester Lanchester's square law from the unit stats: we win if our DPS times our hit points (both against the
             other army's mix of air and ground, sizes and armor) beats theirs
  learned    the same square law with every matchup's DPS and every type's durability learned from the battles,
             pulled towards the stats by a prior
  mlp        a small neural net on the log unit counts
Besides P(win), the learned Lanchester and the net predict the margin: our army's surviving share of its value minus
theirs, in [-1, 1].
"""
import json
import math
import sys

import numpy as np

TERRAN = ['mar', 'bat', 'med', 'vul', 'gol', 'tnk', 'wra', 'bcr']
PROTOSS = ['zea', 'drg', 'arc', 'sco', 'cor']
NAMES = {
    'Terran_Marine': 'mar', 'Terran_Firebat': 'bat', 'Terran_Medic': 'med', 'Terran_Vulture': 'vul',
    'Terran_Goliath': 'gol', 'Terran_Siege_Tank_Tank_Mode': 'tnk', 'Terran_Siege_Tank_Siege_Mode': 'tnk',
    'Terran_Wraith': 'wra', 'Terran_Battlecruiser': 'bcr', 'Protoss_Zealot': 'zea', 'Protoss_Dragoon': 'drg',
    'Protoss_Archon': 'arc', 'Protoss_Scout': 'sco', 'Protoss_Corsair': 'cor',
}
# hit points plus shields, armor, size, flies, ground weapon and air weapon as (damage per hit, hits, type, cooldown)
N, C, E = 'normal', 'concussive', 'explosive'
STATS = {
    'mar': (40, 0, 'small', False, (6, 1, N, 15), (6, 1, N, 15)),
    'bat': (50, 1, 'small', False, (8, 2, C, 22), None),
    'med': (60, 1, 'small', False, None, None),
    'vul': (80, 0, 'medium', False, (20, 1, C, 30), None),
    'gol': (125, 1, 'large', False, (12, 1, N, 22), (10, 2, E, 22)),
    'tnk': (150, 1, 'large', False, (30, 1, E, 37), None),
    'wra': (120, 0, 'large', True, (8, 1, N, 30), (20, 1, E, 22)),
    'bcr': (500, 3, 'large', True, (25, 1, N, 30), (25, 1, N, 30)),
    'zea': (160, 1, 'small', False, (8, 2, N, 22), None),
    'drg': (180, 1, 'large', False, (20, 1, E, 30), (20, 1, E, 30)),
    'arc': (360, 0, 'large', False, (30, 1, N, 20), (30, 1, N, 20)),
    'sco': (250, 0, 'large', True, (8, 1, N, 30), (14, 2, E, 22)),
    'cor': (180, 1, 'medium', True, None, (5, 1, E, 8)),
}
SIZE_FACTOR = {N: {'small': 1, 'medium': 1, 'large': 1}, C: {'small': 1, 'medium': .5, 'large': .25},
               E: {'small': .5, 'medium': .75, 'large': 1}}
FRAMES_PER_SECOND = 24


def dps(attacker, target):
    """Damage a second of `attacker` against `target`, after armor and the size factor; 0 if it cannot hit it."""
    weapon = STATS[attacker][5] if STATS[target][3] else STATS[attacker][4]
    if weapon is None:
        return 0.0
    damage, hits, kind, cooldown = weapon
    per_hit = max(0.5, damage * SIZE_FACTOR[kind][STATS[target][2]] - STATS[target][1])
    return per_hit * hits * FRAMES_PER_SECOND / cooldown


def load(paths):
    battles = []
    for path in paths:
        with open(path, encoding='utf-8-sig') as fh:
            battles += [json.loads(line) for line in fh if line.strip()]
    return battles


def army_vector(groups, types):
    v = np.zeros(len(types))
    for g in groups:
        v[types.index(g['code'])] += g['count']
    return v


def left_share(groups, left, types):
    """The surviving share of an army's value, each type's survivors weighted by their remaining durability."""
    value = {g['code']: g['value'] for g in groups}
    total = sum(g['value'] * g['count'] for g in groups)
    kept = 0.0
    for name, rest in left.get('byType', {}).items():
        code = NAMES.get(name)
        if code in value:
            kept += value[code] * rest['durability'] / STATS[code][0]
    return min(1.0, kept / total) if total else 0.0


def dataset(battles):
    ours = np.array([army_vector(b['ours'], TERRAN) for b in battles])
    theirs = np.array([army_vector(b['theirs'], PROTOSS) for b in battles])
    win = np.array([1.0 if b['status'] == 'win' else 0.0 for b in battles])
    margin = np.array([left_share(b['ours'], b['oursLeft'], TERRAN) - left_share(b['theirs'], b['theirsLeft'], PROTOSS)
                       for b in battles])
    values = np.array([[b['oursValue'], b['theirsValue']] for b in battles])
    return ours, theirs, win, margin, values


HP_US = np.array([STATS[t][0] for t in TERRAN], dtype=float)
HP_THEM = np.array([STATS[t][0] for t in PROTOSS], dtype=float)
# a matchup no weapon can fight stays at zero; the rest start from the stats, medics from a small stand-in
CAN_US = np.array([[dps(i, j) > 0 or i == 'med' for j in PROTOSS] for i in TERRAN])
CAN_THEM = np.array([[dps(j, i) > 0 for i in TERRAN] for j in PROTOSS])
PRIOR_US = np.array([[dps(i, j) if dps(i, j) > 0 else (1.0 if i == 'med' else 1e-9) for j in PROTOSS] for i in TERRAN])
PRIOR_THEM = np.array([[max(dps(j, i), 1e-9) for i in TERRAN] for j in PROTOSS])


def lanchester(ours, theirs, a, b, hp_us, hp_them):
    """log of our square-law strength over theirs: total DPS against the other army's hit-point mix times hit points."""
    h_us = ours @ hp_us
    h_them = theirs @ hp_them
    mix_them = theirs * hp_them / np.maximum(h_them, 1e-9)[:, None]
    mix_us = ours * hp_us / np.maximum(h_us, 1e-9)[:, None]
    fire_us = np.einsum('bi,ij,bj->b', ours, a, mix_them)
    fire_them = np.einsum('bj,ji,bi->b', theirs, b, mix_us)
    return np.log(fire_us * h_us + 1e-6) - np.log(fire_them * h_them + 1e-6)


def sigmoid(z):
    return 1 / (1 + np.exp(-np.clip(z, -30, 30)))


def log_loss(p, y):
    p = np.clip(p, 1e-4, 1 - 1e-4)
    return float(-np.mean(y * np.log(p) + (1 - y) * np.log(1 - p)))


def margin_of(z):
    """The square law's margin: the winner keeps sqrt(1 - weaker / stronger) of its army."""
    return np.sign(z) * np.sqrt(np.clip(1 - np.exp(-np.abs(z)), 0, 1))


class Learned:
    """The square law with log-parameters for every possible matchup's DPS and every type's durability."""

    def __init__(self, prior=0.5, steps=400, rate=0.05):
        self.prior, self.steps, self.rate = prior, steps, rate

    def unpack(self, w):
        k = CAN_US.sum()
        a = np.where(CAN_US, PRIOR_US, 0.0)
        a[CAN_US] *= np.exp(w[:k])
        m = CAN_THEM.sum()
        b = np.where(CAN_THEM, PRIOR_THEM, 0.0)
        b[CAN_THEM] *= np.exp(w[k:k + m])
        hp_us = HP_US * np.exp(w[k + m:k + m + len(TERRAN)])
        hp_them = HP_THEM * np.exp(w[k + m + len(TERRAN):k + m + len(TERRAN) + len(PROTOSS)])
        scale, bias = w[-2], w[-1]
        return a, b, hp_us, hp_them, scale, bias

    def strength(self, w, ours, theirs):
        a, b, hp_us, hp_them, _, _ = self.unpack(w)
        return lanchester(ours, theirs, a, b, hp_us, hp_them)

    def z(self, w, ours, theirs):
        return np.exp(w[-2]) * self.strength(w, ours, theirs) + w[-1]

    def loss(self, w, ours, theirs, y):
        return log_loss(sigmoid(self.z(w, ours, theirs)), y) + self.prior * np.mean(w[:-2] ** 2)

    def fit(self, ours, theirs, y):
        size = CAN_US.sum() + CAN_THEM.sum() + len(TERRAN) + len(PROTOSS) + 2
        w = np.zeros(size)
        m, v = np.zeros(size), np.zeros(size)
        for step in range(1, self.steps + 1):
            grad = np.zeros(size)
            base = self.loss(w, ours, theirs, y)
            for i in range(size):
                w[i] += 1e-4
                grad[i] = (self.loss(w, ours, theirs, y) - base) / 1e-4
                w[i] -= 1e-4
            m = 0.9 * m + 0.1 * grad
            v = 0.999 * v + 0.001 * grad ** 2
            w -= self.rate * (m / (1 - 0.9 ** step)) / (np.sqrt(v / (1 - 0.999 ** step)) + 1e-8)
        self.w = w
        return self

    def predict(self, ours, theirs):
        return sigmoid(self.z(self.w, ours, theirs)), margin_of(self.strength(self.w, ours, theirs))


class Mlp:
    """log(1 + count) of every type -> tanh hidden layer -> P(win) and the margin."""

    def __init__(self, hidden=12, l2=1e-3, epochs=1500, rate=0.01, seed=0):
        self.hidden, self.l2, self.epochs, self.rate, self.seed = hidden, l2, epochs, rate, seed

    @staticmethod
    def features(ours, theirs):
        return np.log1p(np.hstack([ours, theirs]))

    def forward(self, x):
        h = np.tanh(x @ self.w1 + self.b1)
        out = h @ self.w2 + self.b2
        return h, sigmoid(out[:, 0]), np.tanh(out[:, 1])

    def fit(self, ours, theirs, y, margin):
        x = self.features(ours, theirs)
        rng = np.random.default_rng(self.seed)
        self.w1 = rng.normal(0, 1 / math.sqrt(x.shape[1]), (x.shape[1], self.hidden))
        self.b1 = np.zeros(self.hidden)
        self.w2 = rng.normal(0, 1 / math.sqrt(self.hidden), (self.hidden, 2))
        self.b2 = np.zeros(2)
        params = [self.w1, self.b1, self.w2, self.b2]
        moments = [(np.zeros_like(p), np.zeros_like(p)) for p in params]
        n = len(y)
        for step in range(1, self.epochs + 1):
            h, p, m = self.forward(x)
            # cross-entropy on the win, squared error on the margin
            d_out = np.stack([(p - y) / n, (m - margin) * (1 - m ** 2) / n], axis=1)
            g_w2 = h.T @ d_out + self.l2 * self.w2
            g_b2 = d_out.sum(axis=0)
            d_h = d_out @ self.w2.T * (1 - h ** 2)
            g_w1 = x.T @ d_h + self.l2 * self.w1
            g_b1 = d_h.sum(axis=0)
            for param, grad, (mo, ve) in zip(params, [g_w1, g_b1, g_w2, g_b2], moments):
                mo *= 0.9
                mo += 0.1 * grad
                ve *= 0.999
                ve += 0.001 * grad ** 2
                param -= self.rate * (mo / (1 - 0.9 ** step)) / (np.sqrt(ve / (1 - 0.999 ** step)) + 1e-8)
        return self

    def predict(self, ours, theirs):
        _, p, m = self.forward(self.features(ours, theirs))
        return p, m


def cross_validate(ours, theirs, win, margin, values, folds=5, seed=1):
    order = np.random.default_rng(seed).permutation(len(win))
    results = {name: {'p': np.zeros(len(win)), 'm': np.full(len(win), np.nan)}
               for name in ['value', 'lanchester', 'learned', 'mlp']}
    stats_z = lanchester(ours, theirs, np.where(CAN_US & ~np.array([[i == 'med'] * len(PROTOSS) for i in TERRAN]),
                                                PRIOR_US, 0.0),
                         np.where(CAN_THEM, PRIOR_THEM, 0.0), HP_US, HP_THEM)
    for k in range(folds):
        test = order[k::folds]
        train = np.setdiff1d(order, test)
        # the value rule and the plain square law have nothing to learn but how sure to be: one scale on the train set
        for name, z in [('value', np.log(values[:, 0] / values[:, 1])), ('lanchester', stats_z)]:
            best = min((log_loss(sigmoid(s * z[train]), win[train]), s) for s in np.linspace(0.1, 10, 100))[1]
            results[name]['p'][test] = sigmoid(best * z[test])
        results['lanchester']['m'][test] = margin_of(stats_z[test])
        learned = Learned().fit(ours[train], theirs[train], win[train])
        results['learned']['p'][test], results['learned']['m'][test] = learned.predict(ours[test], theirs[test])
        mlp = Mlp().fit(ours[train], theirs[train], win[train], margin[train])
        results['mlp']['p'][test], results['mlp']['m'][test] = mlp.predict(ours[test], theirs[test])
    return results


def main(args):
    weights = None
    if '--weights' in args:
        weights = args[args.index('--weights') + 1]
        args = [a for a in args if a not in ('--weights', weights)]
    battles = load(args)
    ours, theirs, win, margin, values = dataset(battles)
    timeouts = sum(1 for b in battles if b['status'] != 'win' and b['oursLeft']['units'] and b['theirsLeft']['units'])
    print(f'{len(battles)} battles: {int(win.sum())} won, {timeouts} timed out with both sides standing')
    results = cross_validate(ours, theirs, win, margin, values)
    print(f'{"predictor":<11} {"accuracy":>8} {"log loss":>8} {"brier":>6} {"margin MAE":>10}')
    for name, r in results.items():
        accuracy = float(np.mean((r['p'] > 0.5) == (win > 0.5)))
        brier = float(np.mean((r['p'] - win) ** 2))
        mae = float(np.nanmean(np.abs(r['m'] - margin))) if not np.all(np.isnan(r['m'])) else float('nan')
        print(f'{name:<11} {accuracy:>8.3f} {log_loss(r["p"], win):>8.3f} {brier:>6.3f} {mae:>10.3f}')
    if weights:
        learned = Learned().fit(ours, theirs, win)
        a, b, hp_us, hp_them, scale, bias = learned.unpack(learned.w)
        with open(weights, 'w', encoding='utf-8') as fh:
            json.dump({'terran': TERRAN, 'protoss': PROTOSS, 'ourDps': a.round(3).tolist(),
                       'theirDps': b.round(3).tolist(), 'ourHp': hp_us.round(1).tolist(),
                       'theirHp': hp_them.round(1).tolist(), 'scale': float(np.exp(scale)), 'bias': float(bias),
                       'battles': len(battles)}, fh, indent=1)
        print(f'learned square law written to {weights}')


if __name__ == '__main__':
    main(sys.argv[1:])
