# TWAILIGHT-28 implementation handoff

This is an unfinished WIP checkpoint, published at the owner's request for a faster-agent handoff. It is not Verify, Done, an independent PASS, or a winning producer. **Accepted native wins: zero.** The ticket's implementation and review difficulty are both `medium` (the supported normal tier), read back at version 8.

## Current source

Use the existing `twailight-28-default-terran-ai` feature branch in `HamsterofDeath/StarcraftAI`. Preserve its published history, exclusive implementation tree, retained implementation session, native run evidence, and the older baseline tree/logs. Canonical `master` has not been changed. Do not restart website work, create a reminder, run hosted CI, rewrite published commits, or promote this checkpoint.

The last compiled and tested source is `fe7de2856e75b00a61f8a1d8b26356f5d068f756`: 51 campaign checks passed, including deterministic ranked bunker selection, rejected top candidates, and a bound of four/six expensive joint checks in the regression fields. Its bounded implementation pre-review was clear; this is not independent review. The nine inherited baseline checks last passed in the earlier 54-check full suite. Local logs are retained under `target/validation/`.

The latest WIP additionally changes `src/main/scala/pony/brain/unitmanaging.scala` and `src/main/scala/pony/brain/twilightsparkle.scala`. These additions have **not yet compiled or run tests**:

- Default Terran bunker/depot construction measures closest approach to the target rather than treating remote circling as progress. The progress object is shared across worker replacement. Add meaningful circling, legitimate travel/detour, arrival/refusal, actual construction, and replacement-history regressions; check that valid long detours remain safe.
- Rejected final-worker background handoffs fail an unassigned job and run its normal terminal listeners, which should release exact funding and its reserved footprint. Assigned jobs remain on UnitManager's ordinary cleanup path. Test real no-candidate completion, failed-before-assignment, successful same-worker admission, and successful worker substitution without duplicate cleanup.

These are source-proven recovery mechanisms. They are **not proven causes of the last native game's mineral-income failure**. Exact funding-holder/constructor custody and mineral-patch/miner telemetry were planned but are not implemented in this checkpoint.

## Required behavior

- No early scouts. Defend throughout.
- Prebuild an extra Command Center safely at home and use both completed CCs for SCVs.
- Saturate the starting mineral field using actual distance-based capacity before lifting the spare. Finish queued SCVs, fly to the nearest defensible free reachable field, land, and assign miners.
- Two home CCs never count as two resource bases. Reconnaissance/offense requires a completed landed CC at a distinct field with actual local mining, literal mineral/gas bank thresholds, and a qualifying expedition after reserving mobile defenders.
- Starting saturation and actual second-field establishment remain historical milestones after depletion.
- Cover every mineral patch, working approach, and solved mineral-to-serving-depot return route with the minimum sufficient jointly safe bunker sites. Preserve mining corridors, depot buffers and static masks; no silent generic fallback. Require four actual native loaded Marines per active site. Replace casualties and repair damaged bunkers. Obsolete loaded crews must not suppress active expansion demand or occupy mobile defender slots.
- Ordinary player vision only; unchanged native Protoss opponent. No reveal, CompleteMapInformation enable, opponent weakening, synthetic win, timeout success, or early offensive-gate relaxation.

## Native evidence and remaining blockers

Portable launcher: `scripts/Start-TerranCampaign.ps1`. Compatible preserved lane: StarCraft 1.16.1, BWAPI 4.1.0 Beta2 revision 4615, BWMirror 2.4, Java 8u504 x86, Scala 2.11.7 and SBT 0.13.8. Current settings are heap 256 MiB, two native worker threads, `-Headless`, BWAPI GUI off/local speed zero, native MELEE and Protoss Computer, bank 1000 minerals/300 gas, army minimum 12/value 1500 minerals/300 gas, expansion reserve zero, auto-restart off. The playable watched match has already occurred; subsequent tests use rendering off with the same full simulation.

The launcher checks exact source, runtime hashes, ordinary slot availability, and produces unique `target/native-runs/<run>/` manifests/logs/results. Supply the compatible runtime and Java through parameters on the next machine; do not put private credentials or absolute machine paths in committed evidence. Do not compile over a running native classpath. Use birth/run/source-guarded ordinary cleanup of owned game/JVM processes only.

Recent runs:

- `campaign-background-snapshot-20261008-0832`, producer `c07c6f01dfa340db950a3323363d82cbeadc1c52`: northern start `(31,7)`, frame-32 coverage refusal (131 points/382 safe candidates), then expensive greedy whole-grid connectivity stall proven by own CPU and `own-jvm-threads-0834.txt`. Normally stopped as aborted-unwon; post-close false is separate. Fix in `fe7de2856e75b00a61f8a1d8b26356f5d068f756` preserves the original first safe ranked winner. Northern full coverage/performance still needs real native validation; geometry must be proved rather than weakened.
- `campaign-ranked-bunker-20261008-0844`, producer `fe7de2856e75b00a61f8a1d8b26356f5d068f756`: southern start `(64,118)`, actual frame advancement and ordinary vision; four native cargo-four bunker observations, starting saturation at 6545, then a bunker casualty and failed spare-CC recovery. Three loaded surviving bunkers did not establish current full-field readiness. Aggregate construction locks later cleared, so no reservation-leak claim is supported. At 25297 through 29265 minerals/spendable/locks/queued jobs were zero while gas rose, with one CC and no second-field milestone. Normally stopped as aborted-unwon; raw post-close false/callback 29724 remains separate. The cause of zero mineral income (depletion versus staffing) is unproved without actual patch/miner readback.

Earlier evidence contains genuine native losses, aborted gate stalls and crashes; none is a win. Successful native partial behavior includes bunker completion/cargo-four, repair HP recovery, active-site replacement, START saturation, spare-CC lift/flight/landing, actual distinct-field mining and continuous ordinary frame coverage in prior producers. These partial milestones must not be combined across changing sources into a winning proof.

BWAPI 4.1's terminal batch synthetically returns CompleteMapInformation true after native player victory/defeat. The adapter records those native terminal samples separately and skips AI use. Any true flag in genuine nonterminal gameplay remains sticky invalid. Keep raw native `onEnd(Boolean)`, last gameplay frame, terminal frame and callback frame separate; do not relabel older invalid runs.

## Safe continuation

The owner requested no further game from this context. After the WIP publication, the current owner parks through its own supported fenced path; the final terminal receipt must confirm null claim and absent owned controls. A future useful owner must obtain its own ordinary checked identity/admission, a fresh authoritative joined ownership audit, matching own claim readback, and independently verified exclusive allocation. Preserve the kept context and old credentials privately; never borrow them or manufacture inherited custody.

First compile and add the pending recovery regressions. Add bounded main-thread diagnostics tying each construction proof to its live request/job/assignment, worker position/order/attempt/progress state, actual building state, and active field patch resources/local miners. Do not infer orphan locks from totals. Then publish a tested exact feature producer with `[skip ci]` and iterate fair native matches toward a genuine `onEnd(true)`. Repeated unchanged wins, sustained stability, distinct independent review and dedicated guarded landing remain outstanding. Performance tickets wait until the winning milestone completes.
