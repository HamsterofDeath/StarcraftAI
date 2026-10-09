param(
 [Parameter(Mandatory)][string]$Runtime,
 [Parameter(Mandatory)][string]$Java,
 [Parameter(Mandatory)][string]$RunName,
 [Parameter(Mandatory)][string]$SourceCommit,
 [string]$Repository = (Split-Path $PSScriptRoot -Parent),
 [int]$HeapMb = 256,
 [int]$MinFighters = 12,
 [int]$ArmyMinerals = 1500,
 [int]$ArmyGas = 300,
 [int]$ExpansionReserve = 0,
 [int]$BankMinerals = 1000,
 [int]$BankGas = 300,
 [int]$RequiredFields = 2,
 [int]$MinScoutFighters = 6,
 [int]$MinScouts = 1,
 # A field at or below this fraction of its minerals is mined out; at or below the healthy fraction the next command
 # center is requested.
 [double]$FieldUsefulFraction = 0.05,
 [double]$FieldHealthyFraction = 0.4,
 # A strategy key the bot knows, or 'default' to let it choose; the bot rejects unknown keys at start.
 [ValidatePattern('^[a-z0-9-]+$')][string]$Strategy = 'default',
 # Repository-relative e2e map (for example e2e/maps/vul4-zea6.scx), played with map settings.
 [string]$E2EMap,
 # Several repository-relative e2e maps played in order in one warm StarCraft process (auto restart).
 [string[]]$E2EMaps = @(),
 # Extra bot settings as key=value; each becomes -Dtwailight.<key>=<value> (for example kiteShot=stop).
 [ValidatePattern('^[A-Za-z0-9]+=[A-Za-z0-9.,-]*$')][string[]]$BotProperties = @(),
 # Milliseconds per frame; by default 14 (3x the 42 ms 'Fastest' speed) when rendered and 0 (unthrottled) headless.
 [int]$LocalSpeedMs = -1,
 # Rendered window size as a multiple of StarCraft's 640x480.
 [ValidateRange(1, 4)][int]$WindowScale = 2,
 [int]$AiTickFrames = 24,
 [switch]$Headless,
 [switch]$NoAutoCamera,
 [switch]$CheckOnly
)
$ErrorActionPreference = 'Stop'
if ($RunName -notmatch '^[a-zA-Z0-9-]+$') { throw 'RunName must be unique and simple.' }
if ($SourceCommit -notmatch '^[a-f0-9]{40}$') { throw 'A full producer commit is required.' }
$Repository = (Resolve-Path -LiteralPath $Repository).Path
$Runtime = (Resolve-Path -LiteralPath $Runtime).Path
$Java = (Resolve-Path -LiteralPath $Java).Path
if ((& git -C $Repository rev-parse HEAD).Trim() -ne $SourceCommit) { throw 'Producer HEAD mismatch.' }
if (& git -C $Repository status --porcelain --untracked-files=no) { throw 'Tracked source must match the producer.' }
if ($HeapMb -lt 128 -or $HeapMb -gt 384) { throw 'Use a bounded x86 heap with native address headroom.' }
if ($MinFighters -lt 1 -or $ArmyMinerals -lt 0 -or $ArmyGas -lt 0 -or $ExpansionReserve -lt 0 -or $BankMinerals -lt 0 -or $BankGas -lt 0) { throw 'Campaign thresholds must be nonnegative with at least one fighter.' }
if ($RequiredFields -lt 1 -or $MinScoutFighters -lt 1 -or $MinScouts -lt 1 -or $FieldUsefulFraction -le 0 -or $FieldUsefulFraction -ge $FieldHealthyFraction -or $FieldHealthyFraction -ge 1) { throw 'Field policy needs at least one field/scout/fighter and a useful fraction between 0 and 1.' }
if (-not $PSBoundParameters.ContainsKey('LocalSpeedMs')) { $LocalSpeedMs = if ($Headless) { 0 } else { 14 } }
if ($LocalSpeedMs -lt 0 -or $LocalSpeedMs -gt 500) { throw 'LocalSpeedMs must be between 0 (fastest) and 500 milliseconds per frame.' }
if ($AiTickFrames -lt 1 -or $AiTickFrames -gt 240) { throw 'AiTickFrames must be between 1 (every frame) and 240 frames per heavy AI tick.' }
if (Get-Process -Name StarCraft,injectory_x86 -ErrorAction SilentlyContinue) { throw 'Native runtime is occupied.' }
$foreignBot = Get-CimInstance Win32_Process -Filter "Name='java.exe'" | Where-Object { $_.CommandLine -match 'pony\.Controller' }
if ($foreignBot) { throw 'A bot already owns the native runtime.' }
$dll = Join-Path $Runtime 'bwapi-data/BWAPI.dll'
$dllHash = (Get-FileHash -LiteralPath $dll -Algorithm SHA256).Hash
if ($dllHash -ne 'F2E0F937E9592157656118FA7E5FF30C2327694ED56C1D8F55687972AD97D308') { throw 'Expected verified BWAPI 4.4.0 revision 5016.' }
if ($E2EMap) { $E2EMaps = @($E2EMap) + $E2EMaps }
$mapPlan = @()
if ($E2EMaps.Count -gt 0) {
  # A session plays numbered copies from maps/e2e-session through a wildcard map path, which BWAPI iterates in sequence
  # after each restart. The auto menu stalls on maps two folders below maps/, so this one folder is emptied and reused.
  $mapFolder = if ($E2EMaps.Count -gt 1) { 'maps/e2e-session' } else { 'maps/e2e' }
  New-Item -ItemType Directory -Path (Join-Path $Runtime $mapFolder) -Force | Out-Null
  if ($E2EMaps.Count -gt 1) {
    Get-ChildItem -LiteralPath (Join-Path $Runtime $mapFolder) -Filter '*.sc?' | ForEach-Object { $_.Delete() }
  }
  $number = 0
  foreach ($e2eMap in $E2EMaps) {
    $e2eSource = Join-Path $Repository $e2eMap
    if (-not (Test-Path -LiteralPath $e2eSource)) { throw ('E2E map not found in the repository: ' + $e2eMap) }
    $number++
    $leaf = Split-Path $e2eSource -Leaf
    if ($E2EMaps.Count -gt 1) { $leaf = $number.ToString('000') + '-' + $leaf }
    $runtimeMap = $mapFolder + '/' + $leaf
    Copy-Item -LiteralPath $e2eSource -Destination (Join-Path $Runtime $runtimeMap) -Force
    $mapPlan += $runtimeMap
  }
  $mapRelative = if ($mapPlan.Count -gt 1) { $mapFolder + '/*.scx' } else { $mapPlan[0] }
  $gameType = 'USE_MAP_SETTINGS'
} else {
  $mapRelative = 'maps/BroodWar/aiide/(2)Destination.scx'
  $gameType = 'MELEE'
}
$mapHash = (Get-FileHash -LiteralPath (Join-Path $Runtime $(if ($mapPlan.Count -gt 1) { $mapPlan[0] } else { $mapRelative })) -Algorithm SHA256).Hash
$buildOut = Join-Path $Repository 'target/out/jvm/scala-3.10.0/starcrafter'
$classpathFile = Join-Path $buildOut 'streams/compile/dependencyClasspath/_global/streams/export'
if (-not (Test-Path -LiteralPath $classpathFile)) { throw 'Build the exact producer locally first.' }
$registry = [Microsoft.Win32.RegistryKey]::OpenBaseKey([Microsoft.Win32.RegistryHive]::CurrentUser,[Microsoft.Win32.RegistryView]::Registry32)
$key = $registry.OpenSubKey('SOFTWARE/Blizzard Entertainment/Starcraft'.Replace('/','\'))
try {
  $registered = if ($key) { $key.GetValue('InstallPath') } else { $null }
  if (-not $registered -or [System.IO.Path]::GetFullPath($registered) -ne [System.IO.Path]::GetFullPath($Runtime)) {
    throw 'Compatible native installation is not registered at this runtime.'
  }
}
finally { if ($key) { $key.Close() }; $registry.Close() }
if ($CheckOnly) { 'Runtime, source and ordinary native slot checks passed.'; return }
if (-not (Test-Path -LiteralPath (Join-Path $buildOut 'classes/META-INF/services/pony.brain.modules.strategy.StrategyPlugin'))) {
  throw 'The build output lacks the strategy plugin services file; run sbt compile in the repository first.'
}
$run = Join-Path $Repository ('target/native-runs/' + $RunName)
if (Test-Path -LiteralPath $run) { throw 'Preserve old evidence; choose a new RunName.' }
New-Item -ItemType Directory -Path $run | Out-Null
$iniPath = Join-Path $Runtime 'bwapi-data/bwapi.ini'
$ini = Get-Content -LiteralPath $iniPath -Raw
Copy-Item -LiteralPath $iniPath -Destination (Join-Path $run 'bwapi-before.ini')
$pins = [ordered]@{ ai='NULL'; ai_dbg='NULL'; auto_menu='SINGLE_PLAYER'; auto_restart=$(if ($mapPlan.Count -gt 1) { 'ON' } else { 'OFF' }); mapiteration='SEQUENCE'; map=$mapRelative; race='Terran'; enemy_race='Protoss'; enemy_count='1'; game_type=$gameType; shared_memory='ON'; windowed='ON'; sound='OFF' }
1..7 | ForEach-Object { $pins['enemy_race_' + $_] = 'Protoss' }
if ($Headless) {
  # Headless games draw nothing; their window sits far outside every monitor so it never shows up on the desktop.
  $pins['left'] = '-10000'; $pins['top'] = '-10000'; $pins['width'] = '640'; $pins['height'] = '480'
} else {
  $pins['left'] = '0'; $pins['top'] = '0'
  $pins['width'] = [string](640 * $WindowScale); $pins['height'] = [string](480 * $WindowScale)
}
foreach ($entry in $pins.GetEnumerator()) {
  $pattern = '(?m)^\s*' + [regex]::Escape($entry.Key) + '\s*=.*$'
  if (-not [regex]::IsMatch($ini,$pattern)) { throw ('Missing documented INI key: ' + $entry.Key) }
  $ini = [regex]::Replace($ini,$pattern,($entry.Key + ' = ' + $entry.Value))
}
Set-Content -LiteralPath $iniPath -Value $ini -Encoding ASCII
Copy-Item -LiteralPath $iniPath -Destination (Join-Path $run 'bwapi.ini')
New-Item -ItemType Directory -Path (Join-Path $Runtime 'log') -Force | Out-Null
$rawClasspath = (Get-Content -LiteralPath $classpathFile -Raw).Trim()
if ($rawClasspath -match '^List\((.*)\)$') { $entries = $Matches[1] -split ',\s*' } else { $entries = $rawClasspath -split ';' }
$coursierCache = Join-Path $env:LOCALAPPDATA 'Coursier\cache\v1'
$entries = $entries | ForEach-Object { $_.Replace('${BASE}', $Repository).Replace('${CSR_CACHE}', $coursierCache) }
foreach ($entry in $entries) { if (-not (Test-Path -LiteralPath $entry)) { throw ('Missing runtime classpath entry: ' + $entry) } }
$dependencies = ($entries -join ';')
$classpath = (Join-Path $buildOut 'classes') + ';' + $dependencies
$fieldUsefulText = $FieldUsefulFraction.ToString([System.Globalization.CultureInfo]::InvariantCulture)
$fieldHealthyText = $FieldHealthyFraction.ToString([System.Globalization.CultureInfo]::InvariantCulture)
$options = @("-Xmx${HeapMb}M",'-XX:ParallelGCThreads=2','-Dscala.concurrent.context.numThreads=2','-Dscala.concurrent.context.maxThreads=2',"-Dtwailight.run=$RunName","-Dtwailight.producer=$SourceCommit",('-Dtwailight.resultDirectory="'+$run+'"'),"-Dtwailight.mapInputSha256=$mapHash","-Dtwailight.minFighters=$MinFighters","-Dtwailight.armyMinerals=$ArmyMinerals","-Dtwailight.armyGas=$ArmyGas","-Dtwailight.expansionReserve=$ExpansionReserve","-Dtwailight.requiredFields=$RequiredFields","-Dtwailight.minScoutFighters=$MinScoutFighters","-Dtwailight.minScouts=$MinScouts","-Dtwailight.fieldUsefulFraction=$fieldUsefulText","-Dtwailight.fieldHealthyFraction=$fieldHealthyText","-Dtwailight.strategy=$Strategy","-Dtwailight.localSpeed=$LocalSpeedMs","-Dtwailight.aiTickFrames=$AiTickFrames",'-cp',('"'+$classpath+'"'),'pony.Controller')
$options = $options[0..($options.Count-4)] + @($BotProperties | ForEach-Object { '-Dtwailight.' + $_ }) + @($(if ($mapPlan.Count -gt 1) { '-Dtwailight.mapPlan=' + ($mapPlan -join ',') })) + @("-Dtwailight.bankMinerals=$BankMinerals", "-Dtwailight.bankGas=$BankGas", ('-Dtwailight.headless=' + $Headless.IsPresent.ToString().ToLowerInvariant()), ('-Dtwailight.autoCamera=' + (-not $NoAutoCamera.IsPresent).ToString().ToLowerInvariant())) + $options[($options.Count-3)..($options.Count-1)]
$receipt = [ordered]@{ schema=1; run=$RunName; owner='twilight_ai_impl'; producer=$SourceCommit; launchedAt=(Get-Date).ToUniversalTime().ToString('o'); status='unfinished'; game='StarCraft 1.16.1'; bwapiRevision=5016; javaClient='JBWAPI 2.2.0'; bwapiSha256=$dllHash; map=$mapRelative; mapInputSha256=$mapHash; javaSha256=(Get-FileHash -LiteralPath $Java -Algorithm SHA256).Hash; heapMb=$HeapMb; nativeWorkerThreads=2; ordinaryVision=$true; revealCheat=$false; opponent='unmodified native Protoss computer'; configuration=@{minFighters=$MinFighters;armyMinerals=$ArmyMinerals;armyGas=$ArmyGas;expansionReserve=$ExpansionReserve;requiredFields=$RequiredFields;minScoutFighters=$MinScoutFighters;minScouts=$MinScouts;fieldUsefulFraction=$FieldUsefulFraction;fieldHealthyFraction=$FieldHealthyFraction} }
$receipt.renderingEnabled = !$Headless.IsPresent
$receipt.configuration.bankMinerals = $BankMinerals
$receipt.configuration.bankGas = $BankGas
$receipt.configuration.localSpeed = $LocalSpeedMs
$receipt.configuration.windowScale = if ($Headless) { $null } else { $WindowScale }
$receipt.configuration.aiTickFrames = $AiTickFrames
$receipt.configuration.strategy = $Strategy
$receipt.configuration.gameType = $gameType
$receipt.configuration.mapPlan = $mapPlan
$receipt.configuration.botProperties = $BotProperties
$receipt.configuration.autoCamera = !$Headless.IsPresent -and !$NoAutoCamera.IsPresent
$bot = Start-Process -FilePath $Java -ArgumentList $options -WorkingDirectory $Runtime -WindowStyle Hidden -RedirectStandardOutput (Join-Path $run 'bot-stdout.log') -RedirectStandardError (Join-Path $run 'bot-stderr.log') -PassThru
$receipt.botPid=$bot.Id; $receipt.botStart=$bot.StartTime.ToUniversalTime().ToString('o')
$receipt | ConvertTo-Json -Depth 6 | Set-Content -LiteralPath (Join-Path $run 'manifest.json')
$injectorExe = Join-Path $Runtime 'injectory_x86.exe'
$injectorPid = (Start-Process -FilePath $injectorExe -ArgumentList @('--launch','StarCraft.exe','--inject','bwapi-data/BWAPI.dll') -WorkingDirectory $Runtime -WindowStyle Hidden -RedirectStandardOutput (Join-Path $run 'injector-stdout.log') -RedirectStandardError (Join-Path $run 'injector-stderr.log') -PassThru).Id
$receipt.injectorPid=$injectorPid
# injectory starts the game a moment after its own start; wait for that child instead of racing it
$game = @()
$deadline = (Get-Date).AddSeconds(30)
while ($game.Count -eq 0 -and (Get-Date) -lt $deadline) {
  Start-Sleep -Milliseconds 250
  $game = @(Get-CimInstance Win32_Process -Filter "Name='StarCraft.exe' AND ParentProcessId=$injectorPid")
}
if ($game.Count -ne 1) { $receipt | ConvertTo-Json -Depth 6 | Set-Content -LiteralPath (Join-Path $run 'manifest.json'); throw 'Exact owned game child not identified; preserve receipt and do not launch again.' }
$receipt.gamePid=$game[0].ProcessId; $receipt.gameStart=$game[0].CreationDate.ToUniversalTime().ToString('o')
if ($Headless) {
  # BWAPI clamps the ini position to the screen; keep the running game's window off-screen until StarCraft exits.
  $hider = Join-Path $PSScriptRoot 'Hide-GameWindow.ps1'
  $receipt.windowHiderPid = (Start-Process -FilePath 'powershell.exe' -WindowStyle Hidden -PassThru -ArgumentList @(
    '-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', ('"' + $hider + '"'), '-GamePid', $receipt.gamePid,
    '-BotLog', ('"' + (Join-Path $run 'bot-stdout.log') + '"'))).Id
}
$receipt | ConvertTo-Json -Depth 6 | Set-Content -LiteralPath (Join-Path $run 'manifest.json')
Write-Output ($receipt | ConvertTo-Json -Depth 6)
