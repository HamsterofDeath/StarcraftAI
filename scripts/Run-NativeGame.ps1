param(
 [Parameter(Mandatory)][string]$Runtime,
 [Parameter(Mandatory)][string]$Java,
 [Parameter(Mandatory)][string]$SourceCommit,
 [string]$Repository = (Split-Path $PSScriptRoot -Parent),
 # A strategy key the bot knows, or 'default' to let it choose.
 [ValidatePattern('^[a-z0-9-]+$')][string]$Strategy = 'default',
 # Upper bound in real minutes for the whole game.
 [ValidateRange(1, 120)][int]$TimeoutMinutes = 30,
 # A game whose bot output and result stay unchanged this long is stuck and gets stopped.
 [ValidateRange(1, 300)][int]$StallSeconds = 5,
 # StarCraft's cold start until the game begins (about 17 s on this machine).
 [ValidateRange(1, 300)][int]$StartupSeconds = 25,
 # Watch the game rendered (2x window, 3x speed) instead of running it headless at full speed.
 [switch]$Headed,
 # Milliseconds per frame for a rendered game: 21 for twice the 42 ms 'Fastest' speed; the launcher's 14 by default.
 [ValidateRange(-1, 500)][int]$LocalSpeedMs = -1,
 # Passed on as -BotProperties, for example traceKite=true.
 [string[]]$BotProperties = @()
)
$ErrorActionPreference = 'Stop'
# One melee game against the native Protoss computer on the launcher's default map, stopped as soon as the bot writes
# its final result. Afterwards: the result, the economy heartbeat as a table and the most frequent trace events.
$launcher = Join-Path $PSScriptRoot 'Start-TerranCampaign.ps1'
$runName = 'game-' + $Strategy + '-' + (Get-Date).ToString('yyyyMMdd-HHmmss')
$launch = @{ Runtime = $Runtime; Java = $Java; RunName = $runName; SourceCommit = $SourceCommit
             Repository = $Repository; Strategy = $Strategy; BotProperties = $BotProperties; HeapMb = 384 }
if (-not $Headed) { $launch.Headless = $true }
if ($LocalSpeedMs -ge 0) { $launch.LocalSpeedMs = $LocalSpeedMs }
$started = Get-Date
$receipt = & $launcher @launch | ConvertFrom-Json
$run = Join-Path $Repository ('target/native-runs/' + $runName)
$resultFile = Join-Path $run 'native-result.json'
$botLog = Join-Path $run 'bot-stdout.log'
function Read-Result {
  try { Get-Content -LiteralPath $resultFile -Raw -ErrorAction Stop | ConvertFrom-Json } catch { $null }
}
function Get-Progress {
  (@($botLog, $resultFile) | Where-Object { Test-Path -LiteralPath $_ } |
    ForEach-Object { $i = Get-Item -LiteralPath $_; "$($i.Length)@$($i.LastWriteTimeUtc.Ticks)" }) -join ';'
}
$deadline = $started.AddMinutes($TimeoutMinutes)
$lastProgress = Get-Progress
$lastProgressAt = Get-Date
$stalledIn = $null
while ((Get-Process -Id $receipt.botPid -ErrorAction SilentlyContinue) -and (Get-Date) -lt $deadline) {
  $r = Read-Result
  if ($r -and $r.status -ne 'unfinished') { break }
  Start-Sleep -Milliseconds 500
  $progress = Get-Progress
  if ($progress -ne $lastProgress) { $lastProgress = $progress; $lastProgressAt = Get-Date; continue }
  $phase = if (Read-Result) { 'playing' } else { 'startup' }
  $limit = if ($phase -eq 'startup') { $StartupSeconds } else { $StallSeconds }
  if (((Get-Date) - $lastProgressAt).TotalSeconds -ge $limit) { $stalledIn = $phase; break }
}
if ($stalledIn) { "Stopped: no progress during $stalledIn (see $botLog)" }
elseif ((Get-Date) -ge $deadline) { "Stopped: still running after $TimeoutMinutes minutes" }
Start-Sleep -Milliseconds 500
foreach ($ownedPid in @($receipt.botPid, $receipt.gamePid)) {
  Get-Process -Id $ownedPid -ErrorAction SilentlyContinue | Stop-Process -Force
}
$elapsed = (Get-Date) - $started
$r = Read-Result
$status = if ($r) { $r.status } else { 'no-result' }
"{0} after {1} frames ({2:N1} game minutes) in {3:N1} real minutes (run {4})" -f $status, $r.nativeFrame,
  ($r.nativeFrame / 24 / 60), $elapsed.TotalMinutes, $runName
$lines = if (Test-Path -LiteralPath $botLog) { Get-Content -LiteralPath $botLog } else { @() }
# economy heartbeat: key=value pairs after "detail="
$economy = foreach ($line in ($lines | Where-Object { $_ -match 'event=economy-heartbeat' })) {
  $row = [ordered]@{}
  foreach ($pair in ($line.Substring($line.IndexOf('detail=') + 7) -split ' ')) {
    $kv = $pair -split '=', 2
    if ($kv.Count -eq 2) { $row[$kv[0]] = $kv[1] }
  }
  $minute = [int]$row.nativeFrame / 24 / 60
  [pscustomobject]@{
    min = '{0:N1}' -f $minute; minerals = $row.minerals; gas = $row.gas; gathered = $row.gathered
    supply = $row.supply; scvs = $row.scvs; onMin = $row.onMinerals; onGas = $row.onGas
    building = $row.constructing; idle = $row.idle; ccs = $row.ccs; refineries = $row.refineries
    fighters = $row.fighters
  }
}
$economy | Format-Table -AutoSize | Out-String -Width 200
'expansion:'
$lines | Where-Object { $_ -match 'event=(base-completed|expansion-request)' } |
  ForEach-Object { $_.Substring($_.IndexOf('frame=')) -replace 'detail=', '' }
'most frequent events:'
$lines | ForEach-Object { if ($_ -match 'event=([a-z0-9-]+)') { $Matches[1] } } | Group-Object |
  Sort-Object Count -Descending | Select-Object -First 15 | ForEach-Object { '{0,7} {1}' -f $_.Count, $_.Name }
