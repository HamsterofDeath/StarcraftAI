param(
 [Parameter(Mandatory)][string]$Runtime,
 [Parameter(Mandatory)][string]$Java,
 [Parameter(Mandatory)][string]$SourceCommit,
 [string]$Repository = (Split-Path $PSScriptRoot -Parent),
 # Repository-relative e2e maps; by default every generated micro map.
 [string[]]$Maps,
 [ValidateRange(1, 50)][int]$Repeat = 1,
 [string]$Strategy = 'idle',
 [ValidateRange(10, 1800)][int]$TimeoutSeconds = 300,
 # Watch the games rendered (2x window, 3x speed) instead of running them headless at full speed.
 [switch]$Headed
)
$ErrorActionPreference = 'Stop'
if (-not $Maps) {
  $Maps = Get-ChildItem -LiteralPath (Join-Path $Repository 'e2e/maps') -Filter 'micro-*.scm' |
    ForEach-Object { 'e2e/maps/' + $_.Name }
}
$launcher = Join-Path $PSScriptRoot 'Start-TerranCampaign.ps1'
$stamp = (Get-Date).ToString('yyyyMMdd-HHmmss')
$rows = foreach ($map in $Maps) {
  foreach ($attempt in 1..$Repeat) {
    $name = [IO.Path]::GetFileNameWithoutExtension($map)
    $runName = "$name-$stamp-$attempt"
    $launch = @{ Runtime = $Runtime; Java = $Java; RunName = $runName; SourceCommit = $SourceCommit
                 Repository = $Repository; Strategy = $Strategy; E2EMap = $map }
    if (-not $Headed) { $launch.Headless = $true }
    $receipt = & $launcher @launch | ConvertFrom-Json
    $run = Join-Path $Repository ('target/native-runs/' + $runName)
    $deadline = (Get-Date).AddSeconds($TimeoutSeconds)
    while ((Get-Process -Id $receipt.botPid -ErrorAction SilentlyContinue) -and (Get-Date) -lt $deadline) {
      Start-Sleep -Milliseconds 500
    }
    # The game stays on its score screen after the bot leaves; close the owned processes of this run only.
    foreach ($ownedPid in @($receipt.botPid, $receipt.gamePid)) {
      Get-Process -Id $ownedPid -ErrorAction SilentlyContinue | Stop-Process -Force
    }
    $resultFile = Join-Path $run 'native-result.json'
    if (Test-Path -LiteralPath $resultFile) {
      $r = Get-Content -LiteralPath $resultFile -Raw | ConvertFrom-Json
      [pscustomobject]@{ map = $name; attempt = $attempt; status = $r.status; frames = $r.nativeFrame
                         ownUnits = $r.selfForces.units; ownDurability = $r.selfForces.durability
                         enemyUnits = $r.enemyForces.units; enemyDurability = $r.enemyForces.durability; run = $runName }
    } else {
      [pscustomobject]@{ map = $name; attempt = $attempt; status = 'no-result'; frames = $null; ownUnits = $null
                         ownDurability = $null; enemyUnits = $null; enemyDurability = $null; run = $runName }
    }
  }
}
$rows | Format-Table -AutoSize | Out-String -Width 200
$rows | ConvertTo-Json -Depth 3 | Set-Content -LiteralPath (Join-Path $Repository "target/native-runs/e2e-micro-$stamp.json")
