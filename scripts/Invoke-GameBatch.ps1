param(
 [Parameter(Mandatory)][string]$Runtime,
 [Parameter(Mandatory)][string]$Java,
 # A checkout the games run from, so the working tree may change meanwhile; cloned from the source when missing.
 [Parameter(Mandatory)][string]$RunCheckout,
 [string]$SourceRepository = (Split-Path $PSScriptRoot -Parent),
 # The commit to play; the source repository's HEAD by default.
 [string]$Commit,
 # sbt, for compiling the run checkout.
 [string]$Sbt = 'sbt',
 [ValidatePattern('^[a-z0-9-]+$')][string]$Strategy = 'default',
 [ValidateRange(1, 100)][int]$Games = 1,
 # Upper bound in real minutes per game.
 [ValidateRange(1, 120)][int]$TimeoutMinutes = 25,
 # Watch the games rendered; -LocalSpeedMs 21 plays them at twice the 'Fastest' speed.
 [switch]$Headed,
 [ValidateRange(-1, 500)][int]$LocalSpeedMs = -1,
 # Passed on as -BotProperties, for example focusFire=threat.
 [string[]]$BotProperties = @(),
 # Trace events counted per game.
 [string[]]$Count = @('launch', 'raid-start', 'comsat-sweep', 'rule-failure'),
 # The economy heartbeat reported at this game minute; 0 for none.
 [ValidateRange(0, 300)][int]$HeartbeatMinute = 18,
 # Where the batch summary goes; target/native-runs/batch-<time>.json in the run checkout by default.
 [string]$Summary
)
$ErrorActionPreference = 'Stop'
# Plays a batch of full games against the native computer from a fixed commit and reports each one in a line: start,
# result, length, the counted trace events, the economy at a given minute and, for a crash, the first stack lines.

function Stop-Leftovers {
  Get-Process StarCraft -ErrorAction SilentlyContinue | Stop-Process -Force -ErrorAction SilentlyContinue
  Get-CimInstance Win32_Process -Filter "Name='java.exe'" |
    Where-Object { $_.CommandLine -match 'pony\.Controller' } | ForEach-Object { Stop-Process -Id $_.ProcessId -Force }
  Get-CimInstance Win32_Process -Filter "Name='powershell.exe'" |
    Where-Object { $_.CommandLine -match 'Hide-GameWindow' } | ForEach-Object { Stop-Process -Id $_.ProcessId -Force }
}

if (-not $Commit) { $Commit = (git -C $SourceRepository rev-parse HEAD).Trim() }
if (-not (Test-Path -LiteralPath (Join-Path $RunCheckout '.git'))) {
  git clone -q --no-checkout $SourceRepository $RunCheckout
  if ($LASTEXITCODE -ne 0) { throw "Cannot clone $SourceRepository" }
}
git -C $RunCheckout fetch -q $SourceRepository '+refs/heads/*:refs/remotes/source/*'
if ($LASTEXITCODE -ne 0) { throw "Cannot fetch from $SourceRepository" }
git -C $RunCheckout checkout -q --detach $Commit
if ($LASTEXITCODE -ne 0) { throw "Cannot check out $Commit" }

$env:JAVA_HOME = Split-Path (Split-Path $Java -Parent) -Parent
$env:PATH = (Join-Path $env:JAVA_HOME 'bin') + ';' + $env:PATH
Push-Location $RunCheckout
try {
  $compiled = & $Sbt --batch -no-colors compile 2>&1 | ForEach-Object { "$_" } | Where-Object { $_ -match '^\[(error|success)\]' }
} finally { Pop-Location }
if (-not ($compiled | Where-Object { $_ -match '^\[success\]' }) -or ($compiled | Where-Object { $_ -match '^\[error\]' })) {
  throw "Compilation of $Commit failed:`n$($compiled -join "`n")"
}

$runs = Join-Path $RunCheckout 'target/native-runs'
$rows = foreach ($n in 1..$Games) {
  Stop-Leftovers
  $before = @(Get-ChildItem -LiteralPath $runs -Directory -Filter "game-$Strategy-*" -ErrorAction SilentlyContinue).Name
  $game = @{ Runtime = $Runtime; Java = $Java; SourceCommit = $Commit; Repository = $RunCheckout
             Strategy = $Strategy; TimeoutMinutes = $TimeoutMinutes; BotProperties = $BotProperties }
  if ($Headed) { $game.Headed = $true }
  if ($LocalSpeedMs -ge 0) { $game.LocalSpeedMs = $LocalSpeedMs }
  $outcome = (& (Join-Path $RunCheckout 'scripts/Run-NativeGame.ps1') @game 2>&1 | ForEach-Object { "$_" } |
    Select-Object -First 1)
  Stop-Leftovers
  $run = Get-ChildItem -LiteralPath $runs -Directory -Filter "game-$Strategy-*" |
    Where-Object { $before -notcontains $_.Name } | Sort-Object Name | Select-Object -Last 1
  $log = if ($run) { Join-Path $run.FullName 'bot-stdout.log' }
  $lines = if ($log -and (Test-Path -LiteralPath $log)) { Get-Content -LiteralPath $log } else { @() }
  $start = ($lines | Select-String -Pattern 'event=base-completed' | Select-Object -First 1).Line -replace '.*tile=', ''
  $counts = [ordered]@{}
  foreach ($event in $Count) { $counts[$event] = @($lines | Select-String -SimpleMatch "event=$event ").Count }
  $heartbeat = if ($HeartbeatMinute -gt 0) {
    $frame = $HeartbeatMinute * 1440
    ($lines | Select-String -Pattern 'event=economy-heartbeat' | Where-Object {
      [int]($_.Line -replace '.*frame=(\d+).*', '$1') -ge $frame } | Select-Object -First 1).Line -replace
      '.*fps=\d+ ', '' -replace ' constructing=.*ccs=', ' ccs='
  }
  $stderr = if ($run) { Join-Path $run.FullName 'bot-stderr.log' }
  $crash = if ($stderr -and (Test-Path -LiteralPath $stderr)) {
    (Get-Content -LiteralPath $stderr | Select-String -Pattern 'Exception|Error|^\s+at pony' |
      Select-Object -First 3 | ForEach-Object { $_.Line.Trim() }) -join ' | '
  }
  $row = [pscustomobject][ordered]@{
    game = $n; start = $start; outcome = $outcome; run = $(if ($run) { $run.Name })
    counts = $counts; heartbeat = $heartbeat; crash = $crash
  }
  '{0,3} {1,-10} {2}' -f $n, $start, $outcome | Write-Host
  '    ' + (($counts.GetEnumerator() | ForEach-Object { "$($_.Key)=$($_.Value)" }) -join ' ') | Write-Host
  if ($heartbeat) { "    at minute ${HeartbeatMinute}: $heartbeat" | Write-Host }
  if ($crash) { "    crash: $crash" | Write-Host }
  $row
}
if (-not $Summary) { $Summary = Join-Path $runs ('batch-' + (Get-Date).ToString('yyyyMMdd-HHmmss') + '.json') }
[pscustomobject]@{ commit = $Commit; strategy = $Strategy; games = @($rows) } | ConvertTo-Json -Depth 5 |
  Set-Content -LiteralPath $Summary
$wins = @($rows | Where-Object { $_.outcome -match '^win' }).Count
'{0} of {1} games won on {2} ({3}); summary {4}' -f $wins, $Games, $Commit.Substring(0, 7), $Strategy, $Summary
