param(
 [Parameter(Mandatory)][string]$Runtime,
 [Parameter(Mandatory)][string]$Java,
 [Parameter(Mandatory)][string]$SourceCommit,
 [string]$Repository = (Split-Path $PSScriptRoot -Parent),
 # Repository-relative e2e maps; by default every generated micro map.
 [string[]]$Maps,
 [ValidateRange(1, 50)][int]$Repeat = 1,
 [string]$Strategy = 'idle',
 # Upper bound per game; the whole session may take this times the number of games.
 [ValidateRange(10, 1800)][int]$TimeoutSeconds = 120,
 # A game whose bot output and results stay unchanged this long is stuck and gets stopped.
 [ValidateRange(1, 300)][int]$StallSeconds = 5,
 # StarCraft's cold start until the first game begins (about 17 s on this machine).
 [ValidateRange(1, 300)][int]$StartupSeconds = 25,
 # StarCraft's return to its menus between the games of a session (about 3-4 s).
 [ValidateRange(1, 300)][int]$RestartSeconds = 10,
 # Watch the games rendered (2x window, 3x speed) instead of running them headless at full speed.
 [switch]$Headed,
 # Passed on as -BotProperties, for example kiteShot=stop or traceKite=true.
 [string[]]$BotProperties = @()
)
$ErrorActionPreference = 'Stop'
if (-not $Maps) {
  $Maps = Get-ChildItem -LiteralPath (Join-Path $Repository 'e2e/maps') -Filter '*.scx' |
    ForEach-Object { 'e2e/maps/' + $_.Name }
}
# Every game of the batch runs in one warm StarCraft process: BWAPI restarts with the next map after each game.
$plan = @(foreach ($map in $Maps) { foreach ($attempt in 1..$Repeat) { $map } })
$launcher = Join-Path $PSScriptRoot 'Start-TerranCampaign.ps1'
$runName = 'e2e-micro-' + (Get-Date).ToString('yyyyMMdd-HHmmss')
$launch = @{ Runtime = $Runtime; Java = $Java; RunName = $runName; SourceCommit = $SourceCommit
             Repository = $Repository; Strategy = $Strategy; E2EMaps = $plan; BotProperties = $BotProperties }
if (-not $Headed) { $launch.Headless = $true }
$started = Get-Date
$receipt = & $launcher @launch | ConvertFrom-Json
$run = Join-Path $Repository ('target/native-runs/' + $runName)
$resultFiles = if ($plan.Count -gt 1) { 1..$plan.Count | ForEach-Object { Join-Path $run "native-result-$_.json" } }
               else { @(Join-Path $run 'native-result.json') }
function Read-Result([string]$file) {
  try { Get-Content -LiteralPath $file -Raw -ErrorAction Stop | ConvertFrom-Json } catch { $null }
}
# A result is final once its status is anything but the 'unfinished' written at the start of each game.
# Each phase has its own limit, so a stuck StarCraft is stopped quickly instead of waited for:
#   startup  - until the first game begins (StarCraft's cold start)          $StartupSeconds
#   playing  - a game has begun; bot output or a result must keep changing    $StallSeconds
#   restart  - between session games, while StarCraft returns to its menus   $RestartSeconds
$deadline = $started.AddSeconds($TimeoutSeconds * $plan.Count)
$botLog = Join-Path $run 'bot-stdout.log'
function Get-Progress {
  $files = @($botLog) + $resultFiles | Where-Object { Test-Path -LiteralPath $_ }
  ($files | ForEach-Object { $i = Get-Item -LiteralPath $_; "$($i.Length)@$($i.LastWriteTimeUtc.Ticks)" }) -join ';'
}
function Get-Phase {
  $results = @($resultFiles | ForEach-Object { Read-Result $_ })
  if (-not ($results | Where-Object { $_ })) { 'startup' }
  elseif ($results | Where-Object { $_ -and $_.status -eq 'unfinished' }) { 'playing' }
  else { 'restart' }
}
$lastProgress = Get-Progress
$lastProgressAt = Get-Date
$stalledIn = $null
while ((Get-Process -Id $receipt.botPid -ErrorAction SilentlyContinue) -and (Get-Date) -lt $deadline -and
       ($resultFiles | Where-Object { $r = Read-Result $_; -not $r -or $r.status -eq 'unfinished' })) {
  Start-Sleep -Milliseconds 250
  $progress = Get-Progress
  if ($progress -ne $lastProgress) { $lastProgress = $progress; $lastProgressAt = Get-Date; continue }
  $phase = Get-Phase
  $limit = switch ($phase) { 'startup' { $StartupSeconds } 'playing' { $StallSeconds } default { $RestartSeconds } }
  if (((Get-Date) - $lastProgressAt).TotalSeconds -ge $limit) { $stalledIn = $phase; break }
}
if ($stalledIn) { "Stopped: no progress during $stalledIn (see $botLog)" }
Start-Sleep -Milliseconds 500
foreach ($ownedPid in @($receipt.botPid, $receipt.gamePid)) {
  Get-Process -Id $ownedPid -ErrorAction SilentlyContinue | Stop-Process -Force
}
$elapsed = (Get-Date) - $started
$rows = for ($i = 0; $i -lt $plan.Count; $i++) {
  $r = Read-Result $resultFiles[$i]
  [pscustomobject]@{
    game = $i + 1; map = [IO.Path]::GetFileNameWithoutExtension($plan[$i])
    status = if ($r) { $r.status } else { 'no-result' }; frames = $r.nativeFrame
    ownUnits = $r.selfForces.units; ownDurability = $r.selfForces.durability
    enemyUnits = $r.enemyForces.units; enemyDurability = $r.enemyForces.durability
  }
}
$rows | Format-Table -AutoSize | Out-String -Width 200
"{0} games in {1:N1} s (run {2})" -f $plan.Count, $elapsed.TotalSeconds, $runName
$rows | ConvertTo-Json -Depth 3 | Set-Content -LiteralPath (Join-Path $run 'e2e-micro-summary.json')
