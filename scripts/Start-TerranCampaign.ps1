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
 [switch]$Headless,
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
if (Get-Process -Name StarCraft,injectory_x86 -ErrorAction SilentlyContinue) { throw 'Native runtime is occupied.' }
$foreignBot = Get-CimInstance Win32_Process -Filter "Name='java.exe'" | Where-Object { $_.CommandLine -match 'pony\.Controller' }
if ($foreignBot) { throw 'A bot already owns the native runtime.' }
$dll = Join-Path $Runtime 'bwapi-data/BWAPI.dll'
$dllHash = (Get-FileHash -LiteralPath $dll -Algorithm SHA256).Hash
if ($dllHash -ne 'D06D2A3E338BD92E963D93BCEC86E4B576A949C2E8C4622AC70FBCF1CC7BFC86') { throw 'Expected verified BWAPI 4.1.0 Beta2 revision 4615.' }
$mapRelative = 'maps/BroodWar/aiide/(2)Destination.scx'
$mapHash = (Get-FileHash -LiteralPath (Join-Path $Runtime $mapRelative) -Algorithm SHA256).Hash
$classpathFile = Join-Path $Repository 'target/streams/compile/dependencyClasspath/$global/streams/export'
if (-not (Test-Path -LiteralPath $classpathFile)) { throw 'Build the exact producer locally first.' }
$registry = [Microsoft.Win32.RegistryKey]::OpenBaseKey([Microsoft.Win32.RegistryHive]::CurrentUser,[Microsoft.Win32.RegistryView]::Registry32)
$key = $registry.OpenSubKey('SOFTWARE/Blizzard Entertainment/Starcraft'.Replace('/','\'))
try { if (-not $key -or $key.GetValue('InstallPath') -ne $Runtime) { throw 'Compatible native installation is not registered at this runtime.' } }
finally { if ($key) { $key.Close() }; $registry.Close() }
if ($CheckOnly) { 'Runtime, source and ordinary native slot checks passed.'; return }
$run = Join-Path $Repository ('target/native-runs/' + $RunName)
if (Test-Path -LiteralPath $run) { throw 'Preserve old evidence; choose a new RunName.' }
New-Item -ItemType Directory -Path $run | Out-Null
$iniPath = Join-Path $Runtime 'bwapi-data/bwapi.ini'
$ini = Get-Content -LiteralPath $iniPath -Raw
Copy-Item -LiteralPath $iniPath -Destination (Join-Path $run 'bwapi-before.ini')
$pins = [ordered]@{ ai='NULL'; ai_dbg='NULL'; auto_menu='SINGLE_PLAYER'; auto_restart='OFF'; map=$mapRelative; race='Terran'; enemy_race='Protoss'; enemy_count='1'; game_type='MELEE'; shared_memory='ON'; windowed='ON'; sound='OFF' }
1..7 | ForEach-Object { $pins['enemy_race_' + $_] = 'Protoss' }
foreach ($entry in $pins.GetEnumerator()) {
  $pattern = '(?m)^\s*' + [regex]::Escape($entry.Key) + '\s*=.*$'
  if (-not [regex]::IsMatch($ini,$pattern)) { throw ('Missing documented INI key: ' + $entry.Key) }
  $ini = [regex]::Replace($ini,$pattern,($entry.Key + ' = ' + $entry.Value))
}
Set-Content -LiteralPath $iniPath -Value $ini -Encoding ASCII
Copy-Item -LiteralPath $iniPath -Destination (Join-Path $run 'bwapi.ini')
New-Item -ItemType Directory -Path (Join-Path $Runtime 'log') -Force | Out-Null
$dependencies = (Get-Content -LiteralPath $classpathFile -Raw).Trim()
$classpath = (Join-Path $Repository 'target/scala-2.11/classes') + ';' + $dependencies
$options = @("-Xmx${HeapMb}M",'-XX:ParallelGCThreads=2','-Dscala.concurrent.context.numThreads=2','-Dscala.concurrent.context.maxThreads=2',"-Dtwailight.run=$RunName","-Dtwailight.producer=$SourceCommit",('-Dtwailight.resultDirectory="'+$run+'"'),"-Dtwailight.mapInputSha256=$mapHash","-Dtwailight.minFighters=$MinFighters","-Dtwailight.armyMinerals=$ArmyMinerals","-Dtwailight.armyGas=$ArmyGas","-Dtwailight.expansionReserve=$ExpansionReserve",'-cp',('"'+$classpath+'"'),'pony.Controller')
$options = $options[0..($options.Count-4)] + @("-Dtwailight.bankMinerals=$BankMinerals", "-Dtwailight.bankGas=$BankGas", ('-Dtwailight.headless=' + $Headless.IsPresent.ToString().ToLowerInvariant())) + $options[($options.Count-3)..($options.Count-1)]
$receipt = [ordered]@{ schema=1; run=$RunName; owner='twilight_ai_impl'; producer=$SourceCommit; launchedAt=(Get-Date).ToUniversalTime().ToString('o'); status='unfinished'; game='StarCraft 1.16.1'; bwapiRevision=4615; bwapiSha256=$dllHash; map=$mapRelative; mapInputSha256=$mapHash; javaSha256=(Get-FileHash -LiteralPath $Java -Algorithm SHA256).Hash; heapMb=$HeapMb; nativeWorkerThreads=2; ordinaryVision=$true; revealCheat=$false; opponent='unmodified native Protoss computer'; configuration=@{minFighters=$MinFighters;armyMinerals=$ArmyMinerals;armyGas=$ArmyGas;expansionReserve=$ExpansionReserve} }
$receipt.renderingEnabled = !$Headless.IsPresent
$receipt.configuration.bankMinerals = $BankMinerals
$receipt.configuration.bankGas = $BankGas
$receipt.configuration.localSpeed = 0
$bot = Start-Process -FilePath $Java -ArgumentList $options -WorkingDirectory $Runtime -WindowStyle Hidden -RedirectStandardOutput (Join-Path $run 'bot-stdout.log') -RedirectStandardError (Join-Path $run 'bot-stderr.log') -PassThru
$receipt.botPid=$bot.Id; $receipt.botStart=$bot.StartTime.ToUniversalTime().ToString('o')
$receipt | ConvertTo-Json -Depth 6 | Set-Content -LiteralPath (Join-Path $run 'manifest.json')
$injector = Start-Process -FilePath (Join-Path $Runtime 'injectory_x86.exe') -ArgumentList @('--launch','StarCraft.exe','--inject','bwapi-data/BWAPI.dll') -WorkingDirectory $Runtime -WindowStyle Hidden -RedirectStandardOutput (Join-Path $run 'injector-stdout.log') -RedirectStandardError (Join-Path $run 'injector-stderr.log') -PassThru
$receipt.injectorPid=$injector.Id; $receipt.injectorStart=$injector.StartTime.ToUniversalTime().ToString('o')
$game = @(Get-CimInstance Win32_Process -Filter "Name='StarCraft.exe' AND ParentProcessId=$($injector.Id)")
if ($game.Count -ne 1) { $receipt | ConvertTo-Json -Depth 6 | Set-Content -LiteralPath (Join-Path $run 'manifest.json'); throw 'Exact owned game child not identified; preserve receipt and do not launch again.' }
$receipt.gamePid=$game[0].ProcessId; $receipt.gameStart=$game[0].CreationDate.ToUniversalTime().ToString('o')
$receipt | ConvertTo-Json -Depth 6 | Set-Content -LiteralPath (Join-Path $run 'manifest.json')
Write-Output ($receipt | ConvertTo-Json -Depth 6)
