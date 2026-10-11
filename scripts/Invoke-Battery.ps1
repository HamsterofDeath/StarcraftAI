param(
 [Parameter(Mandatory)][string]$Runtime,
 [Parameter(Mandatory)][string]$Java,
 # A checkout the battles run from, so the working tree may change meanwhile; cloned from the source when missing.
 [Parameter(Mandatory)][string]$RunCheckout,
 [string]$SourceRepository = (Split-Path $PSScriptRoot -Parent),
 # The commit to play; the source repository's HEAD by default.
 [string]$Commit,
 # sbt, for compiling the run checkout and generating the maps.
 [string]$Sbt = 'sbt',
 # The map every scenario is built on: a Badlands map such as AIIDE's (2)Destination.scx.
 [Parameter(Mandatory)][string]$Template,
 [ValidateRange(1, 5000)][int]$Battles = 100,
 # The armies are drawn from this seed, so a battery can be replayed on another commit.
 [long]$Seed = 1,
 # Battles per warm StarCraft session; a crashed session loses at most these.
 [ValidateRange(2, 200)][int]$SessionSize = 50,
 [ValidateRange(1, 300)][int]$StallSeconds = 20,
 # Passed on as -BotProperties, for example focusFire=threat.
 [string[]]$BotProperties = @(),
 # One JSON line per battle; target/battery/seed-<seed>/battles.jsonl in the run checkout by default.
 [string]$Dataset
)
$ErrorActionPreference = 'Stop'
# Plays a battery of random battles (pony.e2e.GenerateBattery) and records each outcome next to its armies: the
# status, the length and the survivors of both sides by type. A learner fits the outcome of a fight to these lines.

function Stop-Leftovers {
  Get-Process StarCraft -ErrorAction SilentlyContinue | Stop-Process -Force -ErrorAction SilentlyContinue
  Get-CimInstance Win32_Process -Filter "Name='java.exe'" |
    Where-Object { $_.CommandLine -match 'pony\.Controller' } | ForEach-Object { Stop-Process -Id $_.ProcessId -Force }
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
$folder = "target/battery/seed-$Seed"
$out = Join-Path $RunCheckout $folder
New-Item -ItemType Directory -Path $out -Force | Out-Null
# sbt splits its command line at blanks, and Windows PowerShell mangles embedded quotes: a plain copy of the template
Copy-Item -LiteralPath $Template -Destination (Join-Path $out 'template.scx') -Force
Push-Location $RunCheckout
try {
  $generated = & $Sbt --batch -no-colors "runMain pony.e2e.GenerateBattery $folder/template.scx $folder $Battles $Seed" 2>&1 |
    ForEach-Object { "$_" } | Where-Object { $_ -match '^\[(error|success)\]|^wrote ' }
} finally { Pop-Location }
if (-not ($generated | Where-Object { $_ -match '^wrote \d+ battles' }) -or ($generated | Where-Object { $_ -match '^\[error\]' })) {
  throw "Generating the battery on $Commit failed:`n$($generated -join "`n")"
}
$manifest = @{}
# Windows PowerShell passes a parsed JSON array on as one object; the parentheses unroll it
(Get-Content -LiteralPath (Join-Path $out 'manifest.json') -Raw | ConvertFrom-Json) | ForEach-Object { $manifest[$_.id] = $_ }
$ids = @($manifest.Keys | Sort-Object)
if (-not $Dataset) { $Dataset = Join-Path $out 'battles.jsonl' }
Set-Content -LiteralPath $Dataset -Value @() -Encoding ascii

$runs = Join-Path $RunCheckout 'target/native-runs'
$started = Get-Date
for ($first = 0; $first -lt $ids.Count; $first += $SessionSize) {
  $session = @($ids[$first..([math]::Min($first + $SessionSize, $ids.Count) - 1)])
  Stop-Leftovers
  $before = @(Get-ChildItem -LiteralPath $runs -Directory -Filter 'e2e-micro-*' -ErrorAction SilentlyContinue).Name
  $micro = @{ Runtime = $Runtime; Java = $Java; SourceCommit = $Commit; Repository = $RunCheckout
              Maps = @($session | ForEach-Object { "$folder/$_.scx" }); TimeoutSeconds = 600
              StallSeconds = $StallSeconds; BotProperties = $BotProperties }
  $report = @(& (Join-Path $RunCheckout 'scripts/Run-E2EMicro.ps1') @micro 2>&1 | ForEach-Object { "$_" })
  Stop-Leftovers
  $run = Get-ChildItem -LiteralPath $runs -Directory -Filter 'e2e-micro-*' |
    Where-Object { $before -notcontains $_.Name } | Sort-Object Name | Select-Object -Last 1
  $recorded = 0
  for ($i = 0; $i -lt $session.Count; $i++) {
    $name = if ($session.Count -gt 1) { "native-result-$($i + 1).json" } else { 'native-result.json' }
    $file = if ($run) { Join-Path $run.FullName $name }
    $result = if ($file -and (Test-Path -LiteralPath $file)) { Get-Content -LiteralPath $file -Raw | ConvertFrom-Json }
    if (-not $result -or $result.status -eq 'unfinished') { continue }
    $battle = $manifest[$session[$i]]
    $line = [ordered]@{ id = $battle.id; seed = $Seed; commit = $Commit; run = $run.Name; mode = $battle.mode
                        ours = $battle.ours; theirs = $battle.theirs; oursValue = $battle.oursValue
                        theirsValue = $battle.theirsValue; status = $result.status; frames = $result.nativeFrame
                        oursLeft = $result.selfForces; theirsLeft = $result.enemyForces }
    Add-Content -LiteralPath $Dataset -Value ([pscustomobject]$line | ConvertTo-Json -Depth 6 -Compress) -Encoding ascii
    $recorded++
  }
  $stopped = $report | Where-Object { $_ -match '^Stopped: ' } | Select-Object -First 1
  '{0}..{1}: {2} of {3} recorded in {4}{5}' -f $session[0], $session[-1], $recorded, $session.Count, $run.Name,
    $(if ($stopped) { " ($stopped)" } else { '' })
}
$lines = @(Get-Content -LiteralPath $Dataset | ForEach-Object { $_ | ConvertFrom-Json })
$wins = @($lines | Where-Object { $_.status -eq 'win' }).Count
'{0} battles recorded, {1} won, in {2:N1} minutes on {3}: {4}' -f $lines.Count, $wins,
  ((Get-Date) - $started).TotalMinutes, $Commit.Substring(0, 7), $Dataset
