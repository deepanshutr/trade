if ([Environment]::OSVersion.Platform -eq 'Win32NT' -and ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) { [Console]::Error.WriteLine('Run these checks in a PowerShell that is not elevated.'); exit 1 }

# Checks for tools/trade-windows.ps1 and the two entry scripts.
# The line above stops an elevated run, so a mutant that writes the real machine PATH is refused.
# The pure checks run everywhere. The registry, folder, process and entry script checks need
# Windows. Elsewhere they print one SKIP line.
# Windows, in the native 64-bit Windows PowerShell, not elevated:
#   C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe -NoProfile -ExecutionPolicy Bypass
#     -File test\test-trade-windows.ps1
# Elsewhere: pwsh -NoProfile -File test/test-trade-windows.ps1
# The file is ASCII and uses nothing newer than Windows PowerShell 5.1.

Set-StrictMode -Version 2.0
$ErrorActionPreference = 'Stop'
$onWindows = [Environment]::OSVersion.Platform -eq 'Win32NT'
$repo = Split-Path $PSScriptRoot -Parent
$tools = Join-Path $repo 'tools'
$F = 'C:\Program Files\Desk'
$base = '%SystemRoot%\system32;C:\x'
$script:passed = 0
$script:failed = 0
$script:calls = 0
$stub = { $script:calls++ }

function Invoke-Check {
  param([string]$Name, [scriptblock]$Body)
  try {
    $null = & $Body
    $script:passed++
    Write-Output "ok   $Name"
  } catch {
    $script:failed++
    Write-Output "FAIL $Name"
    Write-Output "     $($_.Exception.Message)"
  }
}

function Assert-Same {
  param($Actual, [string]$Expected)
  if ($Actual -isnot [string]) {
    if ($null -eq $Actual) { $kind = 'nothing' } else { $kind = $Actual.GetType().FullName }
    throw "expected the string '$Expected', got $kind"
  }
  if ($Actual -cne $Expected) { throw "expected '$Expected', got '$Actual'" }
}

function Assert-Bool {
  param($Actual, [bool]$Expected)
  if (-not ($Actual -is [bool] -and $Actual -eq $Expected)) { throw "expected $Expected, got '$Actual'" }
}

function Assert-True {
  param([bool]$Condition, [string]$What)
  if (-not $Condition) { throw $What }
}

# The body must throw, and the message must hold every given text.
function Assert-Throws {
  param([scriptblock]$Body, [string[]]$Text)
  try {
    $null = & $Body
  } catch {
    $message = $_.Exception.Message
    foreach ($part in $Text) {
      if (-not $message.Contains($part)) { throw "expected an error that says '$part', got: $message" }
    }
    return
  }
  throw ('expected an error that says ' + ($Text -join ' and ') + ', got none')
}

$library = Join-Path $tools 'trade-windows.ps1'
try {
  . $library
} catch {
  $script:failed++
  Write-Output "FAIL load $library"
  Write-Output "     $($_.Exception.Message)"
}

# B2: the PATH string rules. F stands for C:\Program Files\Desk.

Invoke-Check 'B2 add puts the folder after the last entry' {
  Assert-Same (Add-TradePath 'A;B' $F) "A;B;$F"
}
Invoke-Check 'B2 add puts the folder before a trailing separator' {
  Assert-Same (Add-TradePath 'A;B;' $F) "A;B;$F;"
}
Invoke-Check 'B2 add keeps %Name% entries as written' {
  Assert-Same (Add-TradePath '%SystemRoot%\system32;%SystemRoot%' $F) "%SystemRoot%\system32;%SystemRoot%;$F"
}
Invoke-Check 'B2 add returns its input when the folder is there in lower case with a trailing backslash' {
  $raw = 'A;c:\program files\desk\;B'
  Assert-Same (Add-TradePath $raw $F) $raw
}
Invoke-Check 'B2 remove drops both forms of the folder and keeps its three neighbours' {
  $mixed = "A;$F;C:\Program Files\Desktop;C:\Program Files\Desk\bin;D:\Program Files\Desk;$F\;B"
  Assert-Same (Remove-TradePath $mixed $F) 'A;C:\Program Files\Desktop;C:\Program Files\Desk\bin;D:\Program Files\Desk;B'
}
foreach ($raw in @('A;B', 'A;B;', 'A;;B', '%SystemRoot%\system32;%SystemRoot%;')) {
  Invoke-Check "B2 remove after add gives back '$raw'" {
    Assert-Same (Remove-TradePath (Add-TradePath $raw $F) $F) $raw
  }
}
foreach ($raw in @('', ';;')) {
  Invoke-Check "B2 add refuses '$raw'" {
    Assert-Throws { Add-TradePath $raw $F } @('has no entry')
  }
  Invoke-Check "B2 remove refuses '$raw'" {
    Assert-Throws { Remove-TradePath $raw $F } @('has no entry')
  }
}

# More cases of the same rules, found while building them.

Invoke-Check 'B2x add after a single entry' {
  Assert-Same (Add-TradePath 'A' $F) "A;$F"
}
Invoke-Check 'B2x add keeps a leading empty entry and every trailing separator' {
  Assert-Same (Add-TradePath ';A;B;;' $F) ";A;B;$F;;"
}
Invoke-Check 'B2x add returns its input when the folder is there in upper case with two trailing backslashes' {
  $raw = 'A;C:\PROGRAM FILES\DESK\\'
  Assert-Same (Add-TradePath $raw $F) $raw
}
Invoke-Check 'B2x add compares a folder given in lower case with a trailing backslash' {
  $raw = "A;$F"
  Assert-Same (Add-TradePath $raw 'c:\program files\desk\') $raw
}
Invoke-Check 'B2x add does not take a neighbour for the folder' {
  $raw = 'C:\Program Files\Desk\bin;C:\Program Files\Desktop;D:\Program Files\Desk;Program Files\Desk'
  Assert-Same (Add-TradePath $raw $F) "$raw;$F"
}
Invoke-Check 'B2x add counts %ProgramFiles%\Desk as another entry' {
  Assert-Same (Add-TradePath 'A;%ProgramFiles%\Desk' $F) "A;%ProgramFiles%\Desk;$F"
}
Invoke-Check 'B2x remove keeps %ProgramFiles%\Desk' {
  Assert-Same (Remove-TradePath "A;%ProgramFiles%\Desk;$F" $F) 'A;%ProgramFiles%\Desk'
}
Invoke-Check 'B2x remove returns its input when the folder is absent' {
  Assert-Same (Remove-TradePath 'A;;B;' $F) 'A;;B;'
}
Invoke-Check 'B2x remove keeps the empty entries around a removed folder' {
  Assert-Same (Remove-TradePath 'A;;c:\program files\desk;;B' $F) 'A;;;B'
}
Invoke-Check 'B2x remove drops the folder when it is the first entry' {
  Assert-Same (Remove-TradePath "$F;A" $F) 'A'
}
foreach ($raw in @('A', ';A', 'A;B;;', 'C:\Program Files\Desktop;C:\Program Files\Desk\bin')) {
  Invoke-Check "B2x remove after add gives back '$raw'" {
    Assert-Same (Remove-TradePath (Add-TradePath $raw $F) $F) $raw
  }
}
Invoke-Check 'B2x remove keeps an entry with a space before the folder' {
  $raw = "A; $F;B"
  Assert-Same (Remove-TradePath $raw $F) $raw
}
Invoke-Check 'B2x add counts a space as a non-empty entry and keeps it' {
  Assert-Same (Add-TradePath ' A; ' $F) " A; ;$F"
}
Invoke-Check 'B2x remove does not read an entry as a wildcard' {
  $raw = 'A;C:\Program Files\D*;B'
  Assert-Same (Remove-TradePath $raw $F) $raw
}
Invoke-Check 'B2x add does not read the folder as a wildcard' {
  $raw = 'A;C:\x[1]\Desk'
  Assert-Same (Add-TradePath $raw 'C:\x[1]\Desk') $raw
}
Invoke-Check 'B2x add and remove refuse a value that holds a NUL character' {
  $raw = 'A;B' + [char]0 + 'C'
  Assert-Throws { Add-TradePath $raw $F } @('NUL')
  Assert-Throws { Remove-TradePath $raw $F } @('NUL')
}
Invoke-Check 'B2x add and remove refuse a value that starts with a NUL character' {
  $raw = [string][char]0 + 'A;B'
  Assert-Throws { Add-TradePath $raw $F } @('NUL')
  Assert-Throws { Remove-TradePath $raw $F } @('NUL')
}
Invoke-Check "B2x add and remove refuse a value of one separator" {
  Assert-Throws { Add-TradePath ';' $F } @('has no entry')
  Assert-Throws { Remove-TradePath ';' $F } @('has no entry')
}
Invoke-Check 'file the library holds only functions, so dot-sourcing runs nothing' {
  $tokens = $null
  $errors = $null
  $ast = [System.Management.Automation.Language.Parser]::ParseFile($library, [ref]$tokens, [ref]$errors)
  Assert-True (@($errors).Count -eq 0) 'the library does not parse'
  Assert-True ($null -eq $ast.ParamBlock -and $null -eq $ast.BeginBlock -and $null -eq $ast.ProcessBlock) 'the library has a param, begin or process block'
  $other = @($ast.EndBlock.Statements | Where-Object { $_ -isnot [System.Management.Automation.Language.FunctionDefinitionAst] })
  Assert-True ($other.Count -eq 0) ('the library runs statements: ' + (@($other | ForEach-Object { $_.Extent.Text }) -join ' / '))
}
foreach ($file in @('tools\trade-windows.ps1', 'tools\install-trade.ps1', 'tools\uninstall-trade.ps1', 'test\test-trade-windows.ps1')) {
  Invoke-Check "file $file is ASCII" {
    $path = Join-Path $repo $file
    $high = @([IO.File]::ReadAllBytes($path) | Where-Object { $_ -gt 127 })
    Assert-True ($high.Count -eq 0) "$path holds $($high.Count) bytes above 127"
  }
}

# Helpers. The registry and process ones run only on Windows.

function New-TestKey {
  param([string]$Value, [string]$Kind)
  $name = 'Software\DeskTradeTest-' + [guid]::NewGuid().ToString('N')
  $key = [Microsoft.Win32.Registry]::CurrentUser.CreateSubKey($name)
  Set-TestPath $key $Value $Kind
  $key
}

function Set-TestPath {
  param($Key, [string]$Value, [string]$Kind)
  $Key.SetValue('Path', $Value, [Microsoft.Win32.RegistryValueKind]$Kind)
}

# An object that answers like an open key with an ExpandString Path, and counts its writes.
function New-FakeKey {
  param([string]$Value)
  $fake = [pscustomobject]@{ Name = 'fake key'; Value = $Value; Writes = 0 }
  $fake | Add-Member ScriptMethod GetValueKind { param($Name) [Microsoft.Win32.RegistryValueKind]::ExpandString }
  $fake | Add-Member ScriptMethod GetValue { param($Name, $Default, $Options) $this.Value }
  $fake | Add-Member ScriptMethod SetValue { param($Name, $Value, $Kind) $this.Value = $Value; $this.Writes++ }
  $fake
}

# Deletes only a key that New-TestKey made.
function Remove-TestKey {
  param($Key)
  $name = $Key.Name.Substring($Key.Name.IndexOf('\') + 1)
  $Key.Close()
  if (-not $name.StartsWith('Software\DeskTradeTest-')) { throw "refusing to delete the key $name" }
  [Microsoft.Win32.Registry]::CurrentUser.DeleteSubKeyTree($name)
}

function Get-RawPath {
  param($Key)
  [string]$Key.GetValue('Path', $null, [Microsoft.Win32.RegistryValueOptions]::DoNotExpandEnvironmentNames)
}

function Get-PathKind {
  param($Key)
  [string]$Key.GetValueKind('Path')
}

function Get-Listing {
  param([string]$Folder)
  if (-not (Test-Path -LiteralPath $Folder)) { return 'absent' }
  $lines = Get-ChildItem -LiteralPath $Folder -Recurse -Force | ForEach-Object {
    if ($_.PSIsContainer) { $_.FullName } else { $_.FullName + ' ' + $_.Length }
  }
  @($lines | Sort-Object) -join ', '
}

# The temporary folder. On Windows its long form, so a process path read back from Windows
# matches it.
function Get-TestRoot {
  if ($onWindows) { Join-Path ([Environment]::GetFolderPath('LocalApplicationData')) 'Temp' } else { [IO.Path]::GetTempPath() }
}

function Write-TestFile {
  param([string]$Path, [string]$Text)
  $null = [IO.Directory]::CreateDirectory((Split-Path $Path -Parent))
  [IO.File]::WriteAllText($Path, $Text)
}

# An image, a Desk folder from an earlier install and a key, as B4 describes them.
function New-Case {
  $root = Join-Path (Get-TestRoot) ('desk-trade-test-' + [guid]::NewGuid().ToString('N'))
  $case = @{ Root = $root; Image = (Join-Path $root 'image'); Folder = (Join-Path $root 'Desk') }
  Write-TestFile (Join-Path $case.Image 'trade.exe') 'new launcher'
  Write-TestFile (Join-Path $case.Image 'app\desk.jar') 'new jar'
  Write-TestFile (Join-Path $case.Image 'runtime\release') 'new runtime'
  Write-TestFile (Join-Path $case.Folder 'Desk.exe') 'old launcher'
  Write-TestFile (Join-Path $case.Folder 'app\desk.jar') 'old jar'
  $case.Key = New-TestKey $base 'ExpandString'
  $script:calls = 0
  $case
}

# Windows reports a stopped stand-in as exited a moment before it lets go of its program file,
# so the removal is tried again every 50 ms for up to 5 seconds.
function Remove-Case {
  param($Subject)
  Remove-TestKey $Subject.Key
  for ($try = 1; Test-Path -LiteralPath $Subject.Root; $try++) {
    try { Remove-Item -LiteralPath $Subject.Root -Recurse -Force } catch { if ($try -ge 100) { throw }; Start-Sleep -Milliseconds 50 }
  }
}

function Assert-Calls {
  param([int]$Count)
  if ($script:calls -ne $Count) { throw "expected $Count broadcast calls, got $script:calls" }
}

function Get-State {
  param($Subject)
  (Get-RawPath $Subject.Key) + ' | ' + (Get-PathKind $Subject.Key) + ' | ' + $script:calls + ' | ' + (Get-Listing $Subject.Folder)
}

# The run must throw with every given text and leave the value, its kind, the stub's count and
# the folder's listing as they were.
function Assert-Refused {
  param($Subject, [scriptblock]$Run, [string[]]$Text)
  $before = Get-State $Subject
  Assert-Throws $Run $Text
  $after = Get-State $Subject
  if ($after -cne $before) { throw "the refusal changed something. Before: $before. After: $after" }
}

# Makes each file unreadable until Unlock-TestFile. On Windows a handle with no sharing holds it.
# Elsewhere mode 000 does, which stops any user but root.
function Lock-TestFile {
  param([string[]]$Path)
  foreach ($one in $Path) {
    if ($onWindows) { [IO.File]::Open($one, 'Open', 'Read', 'None') } else { chmod 000 $one; $one }
  }
}

function Unlock-TestFile {
  param($Handle)
  foreach ($one in $Handle) {
    if ($one -is [string]) { chmod 644 $one } else { $one.Close() }
  }
}

# A copy of cmd.exe that waits in a hidden window, started from the given path.
function Start-StandIn {
  param([string]$Path)
  Copy-Item -LiteralPath (Join-Path $env:SystemRoot 'System32\cmd.exe') -Destination $Path -Force
  $process = Start-Process -FilePath $Path -ArgumentList '/d /k' -WindowStyle Hidden -PassThru
  for ($i = 0; $i -lt 50; $i++) {
    $seen = $null
    try { $seen = (Get-Process -Id $process.Id).Path } catch { $seen = $null }
    if ($seen) { return $process }
    Start-Sleep -Milliseconds 200
  }
  Stop-StandIn $process
  throw "the stand-in at $Path did not start"
}

function Stop-StandIn {
  param($Process)
  Stop-Process -Id $Process.Id -Force -ErrorAction SilentlyContinue
  $null = $Process.WaitForExit(10000)
}

# The raw machine PATH, its kind and the listing of the real install folder.
function Get-MachineState {
  $key = [Microsoft.Win32.Registry]::LocalMachine.OpenSubKey('SYSTEM\CurrentControlSet\Control\Session Manager\Environment')
  try {
    (Get-RawPath $key) + ' | ' + (Get-PathKind $key) + ' | ' + (Get-Listing (Join-Path $env:ProgramFiles 'Desk'))
  } finally {
    $key.Close()
  }
}

# Runs an entry script in the given Windows PowerShell and returns its exit code and output.
function Invoke-Entry {
  param([string]$Shell, [string]$Entry)
  $stem = Join-Path (Get-TestRoot) ('desk-trade-test-' + [guid]::NewGuid().ToString('N'))
  $arguments = @('-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', ('"' + $Entry + '"'))
  try {
    $process = Start-Process -FilePath $Shell -ArgumentList $arguments -NoNewWindow -Wait -PassThru `
      -RedirectStandardOutput "$stem.out" -RedirectStandardError "$stem.err"
    @{ Code = $process.ExitCode; Text = [IO.File]::ReadAllText("$stem.out") + [IO.File]::ReadAllText("$stem.err") }
  } finally {
    foreach ($file in @("$stem.out", "$stem.err")) {
      if (Test-Path -LiteralPath $file) { Remove-Item -LiteralPath $file -Force }
    }
  }
}

# Pure checks of the folder and process rules, on any system.

Invoke-Check 'pure Get-DeskProcess needs the whole folder name, not a prefix of it' {
  $shell = (Get-Process -Id $PID).Path
  Assert-True ([bool]$shell) 'Get-Process gives no path for this shell'
  $folder = Split-Path $shell -Parent
  $cut = $folder.Substring(0, $folder.Length - 2)
  $found = @(Get-DeskProcess $cut)
  Assert-True ($found.Count -eq 0) ("the cut folder $cut matched " + (@($found | ForEach-Object { $_.Path }) -join ', '))
  if ($onWindows) {
    Assert-True (@(Get-DeskProcess $folder | Where-Object { $_.Id -eq $PID }).Count -eq 1) "the whole folder $folder did not match this shell"
  }
}

Invoke-Check 'pure the image copy lands app\desk.jar before anything else' {
  $root = Join-Path (Get-TestRoot) ('desk-trade-test-' + [guid]::NewGuid().ToString('N'))
  try {
    $image = Join-Path $root 'image'
    Write-TestFile (Join-Path $image 'trade.exe') 'new launcher'
    Write-TestFile (Join-Path $image 'app\.jpackage.xml') 'image record'
    Write-TestFile (Join-Path $image 'app\desk.jar') 'new jar'
    Write-TestFile (Join-Path $image 'runtime\release') 'new runtime'
    $others = @(Get-ChildItem -LiteralPath $image -Recurse -Force | Where-Object { -not $_.PSIsContainer -and $_.Name -ne 'desk.jar' } | ForEach-Object { $_.FullName })
    $locks = @(Lock-TestFile $others)
    try {
      Assert-Throws { Copy-TradeImage $image (Join-Path $root 'Desk') } @()
    } finally {
      Unlock-TestFile $locks
    }
    $jar = Join-Path $root 'Desk\app\desk.jar'
    Assert-True (Test-Path -LiteralPath $jar) 'the copy stopped before app\desk.jar landed'
    Assert-Same ([IO.File]::ReadAllText($jar)) 'new jar'
  } finally {
    if (Test-Path -LiteralPath $root) { Remove-Item -LiteralPath $root -Recurse -Force }
  }
}

Invoke-Check 'pure the remove refuses to leave the PATH with no entry and writes nothing' {
  $fake = New-FakeKey "A;$F"
  Assert-Bool (Update-TradePath $fake $F -Remove) $true
  Assert-Same $fake.Value 'A'
  Assert-True ($fake.Writes -eq 1) "the fake key got $($fake.Writes) writes, not 1"
  $fake = New-FakeKey "$F;"
  Assert-Throws { Update-TradePath $fake $F -Remove } @('has no entry')
  Assert-Same $fake.Value "$F;"
  Assert-True ($fake.Writes -eq 0) "the fake key got $($fake.Writes) writes"
}

# A removal can stop after the jar, when a terminal still works in the folder. The folder left
# holds no file, and both scripts must still accept it.
Invoke-Check 'pure a folder that holds no file passes the Desk folder check and is removed' {
  $root = Join-Path (Get-TestRoot) ('desk-trade-test-' + [guid]::NewGuid().ToString('N'))
  try {
    $folder = Join-Path $root 'Desk'
    foreach ($made in @($folder, (Join-Path $folder 'app'), (Join-Path $folder 'runtime\bin'))) {
      $null = [IO.Directory]::CreateDirectory($made)
      Assert-TradeFolder $folder
      Remove-TradeFolder $folder
      Assert-True (-not (Test-Path -LiteralPath $folder)) "the folder made as $made stayed"
    }
    Write-TestFile (Join-Path $folder 'runtime\bin\other.dll') 'another file'
    Assert-Throws { Assert-TradeFolder $folder } @('not a Desk folder')
  } finally {
    if (Test-Path -LiteralPath $root) { Remove-Item -LiteralPath $root -Recurse -Force }
  }
}

Invoke-Check 'pure a folder that holds only a hidden file is not a Desk folder' {
  $root = Join-Path (Get-TestRoot) ('desk-trade-test-' + [guid]::NewGuid().ToString('N'))
  try {
    $file = Join-Path $root 'Desk\runtime\.other'
    Write-TestFile $file 'a hidden file'
    if ($onWindows) { [IO.File]::SetAttributes($file, [IO.FileAttributes]::Hidden) }
    Assert-Throws { Assert-TradeFolder (Join-Path $root 'Desk') } @('not a Desk folder')
  } finally {
    if (Test-Path -LiteralPath $root) { Remove-Item -LiteralPath $root -Recurse -Force }
  }
}

# The broadcast comes after the PATH write, so it must never stop the sequence. Off Windows it
# cannot reach any window, so it warns. On Windows it reaches them and does not warn.
Invoke-Check 'pure Send-SettingChange never stops its caller' {
  $warnings = @(Send-SettingChange 3>&1)
  if ($onWindows) {
    Assert-True ($warnings.Count -eq 0) ('the broadcast was not confirmed: ' + ($warnings -join ' '))
  } else {
    Assert-True ($warnings.Count -eq 1) "expected one warning off Windows, got $($warnings.Count)"
  }
}

if (-not $onWindows) {
  Write-Output 'SKIP B3 B4 B5: the registry, folder, process and entry script checks need Windows'
} else {
  # B3: Update-TradePath on a new key under HKCU\Software.
  $key = New-TestKey $base 'ExpandString'
  try {
    Invoke-Check 'B3 the first update adds the folder at the end, unexpanded, as ExpandString' {
      Assert-Bool (Update-TradePath $key $F) $true
      Assert-Same (Get-RawPath $key) "$base;$F"
      Assert-Same (Get-PathKind $key) 'ExpandString'
    }
    Invoke-Check 'B3 a second update returns false and changes nothing' {
      Assert-Bool (Update-TradePath $key $F) $false
      Assert-Same (Get-RawPath $key) "$base;$F"
      Assert-Same (Get-PathKind $key) 'ExpandString'
    }
    Invoke-Check 'B3 the remove gives back the value as it was' {
      Assert-Bool (Update-TradePath $key $F -Remove) $true
      Assert-Same (Get-RawPath $key) $base
      Assert-Same (Get-PathKind $key) 'ExpandString'
    }
    Invoke-Check 'B3 a String value gains the folder and stays String, and the remove gives it back' {
      Set-TestPath $key $base 'String'
      Assert-Bool (Update-TradePath $key $F) $true
      Assert-Same (Get-RawPath $key) "$base;$F"
      Assert-Same (Get-PathKind $key) 'String'
      Assert-Bool (Update-TradePath $key $F) $false
      Assert-Bool (Update-TradePath $key $F -Remove) $true
      Assert-Same (Get-RawPath $key) $base
      Assert-Same (Get-PathKind $key) 'String'
    }
    Invoke-Check 'B3 a MultiString value throws and stays as it was' {
      $key.SetValue('Path', [string[]]@($base), [Microsoft.Win32.RegistryValueKind]::MultiString)
      Assert-Throws { Update-TradePath $key $F } @('REG_SZ', 'REG_EXPAND_SZ')
      Assert-Throws { Update-TradePath $key $F -Remove } @('REG_SZ', 'REG_EXPAND_SZ')
      Assert-Same (Get-RawPath $key) $base
      Assert-Same (Get-PathKind $key) 'MultiString'
    }
  } finally {
    Remove-TestKey $key
  }

  # B4: the two sequences on a new HKCU key, temporary folders and a stub broadcast.
  Invoke-Check 'B4 install, install again, uninstall and uninstall again' {
    $case = New-Case
    try {
      $null = Install-Trade -Image $case.Image -Key $case.Key -Folder $case.Folder -Broadcast $stub
      Assert-Same (Get-RawPath $case.Key) "$base;$($case.Folder)"
      Assert-Same (Get-PathKind $case.Key) 'ExpandString'
      Assert-True (Test-Path -LiteralPath (Join-Path $case.Folder 'trade.exe')) 'the folder has no trade.exe'
      Assert-True (-not (Test-Path -LiteralPath (Join-Path $case.Folder 'Desk.exe'))) 'the old Desk.exe stayed'
      Assert-Same ([IO.File]::ReadAllText((Join-Path $case.Folder 'app\desk.jar'))) 'new jar'
      Assert-True (Test-Path -LiteralPath (Join-Path $case.Folder 'runtime\release')) 'the folder has no runtime'
      Assert-Calls 1
      $null = Install-Trade -Image $case.Image -Key $case.Key -Folder $case.Folder -Broadcast $stub
      Assert-Same (Get-RawPath $case.Key) "$base;$($case.Folder)"
      Assert-True (Test-Path -LiteralPath (Join-Path $case.Folder 'trade.exe')) 'the rerun lost trade.exe'
      Assert-Calls 1
      $null = Uninstall-Trade -Key $case.Key -Folder $case.Folder -Broadcast $stub
      Assert-Same (Get-RawPath $case.Key) $base
      Assert-Same (Get-PathKind $case.Key) 'ExpandString'
      Assert-True (-not (Test-Path -LiteralPath $case.Folder)) 'the folder stayed'
      Assert-Calls 2
      $null = Uninstall-Trade -Key $case.Key -Folder $case.Folder -Broadcast $stub
      Assert-Same (Get-RawPath $case.Key) $base
      Assert-Calls 2
    } finally {
      Remove-Case $case
    }
  }

  Invoke-Check 'B4 an image without trade.exe is refused and nothing changes' {
    $case = New-Case
    try {
      Remove-Item -LiteralPath (Join-Path $case.Image 'trade.exe') -Force
      Assert-Refused $case { Install-Trade -Image $case.Image -Key $case.Key -Folder $case.Folder -Broadcast $stub } @('trade.exe')
    } finally {
      Remove-Case $case
    }
  }

  Invoke-Check 'B4 an image without app\desk.jar is refused and nothing changes' {
    $case = New-Case
    try {
      Remove-Item -LiteralPath (Join-Path $case.Image 'app\desk.jar') -Force
      Assert-Refused $case { Install-Trade -Image $case.Image -Key $case.Key -Folder $case.Folder -Broadcast $stub } @('The image', 'app\desk.jar')
    } finally {
      Remove-Case $case
    }
  }

  Invoke-Check 'B4 a value with no entry is refused by the install and nothing changes' {
    $case = New-Case
    try {
      Set-TestPath $case.Key ';;' 'ExpandString'
      Assert-Refused $case { Install-Trade -Image $case.Image -Key $case.Key -Folder $case.Folder -Broadcast $stub } @('has no entry')
    } finally {
      Remove-Case $case
    }
  }

  Invoke-Check 'B4 Get-DeskProcess matches the folder in another case' {
    $case = New-Case
    try {
      $standIn = Start-StandIn (Join-Path $case.Folder 'Desk.exe')
      try {
        foreach ($folder in @($case.Folder.ToUpperInvariant(), $case.Folder.ToLowerInvariant())) {
          $found = @(Get-DeskProcess $folder | Where-Object { $_.Id -eq $standIn.Id })
          Assert-True ($found.Count -eq 1) "the folder written as $folder did not match the stand-in"
        }
      } finally {
        Stop-StandIn $standIn
      }
    } finally {
      Remove-Case $case
    }
  }

  Invoke-Check 'B4 a folder with only other.exe is refused by both sequences and nothing changes' {
    $case = New-Case
    try {
      Remove-Item -LiteralPath (Join-Path $case.Folder 'Desk.exe'), (Join-Path $case.Folder 'app') -Recurse -Force
      Write-TestFile (Join-Path $case.Folder 'other.exe') 'another program'
      Assert-Refused $case { Install-Trade -Image $case.Image -Key $case.Key -Folder $case.Folder -Broadcast $stub } @('app\desk.jar')
      Set-TestPath $case.Key "$base;$($case.Folder)" 'ExpandString'
      Assert-Refused $case { Uninstall-Trade -Key $case.Key -Folder $case.Folder -Broadcast $stub } @('app\desk.jar')
    } finally {
      Remove-Case $case
    }
  }

  Invoke-Check 'B4 a folder that holds no file is accepted by both sequences, and the install fills it' {
    $case = New-Case
    try {
      Remove-Item -LiteralPath (Join-Path $case.Folder 'Desk.exe'), (Join-Path $case.Folder 'app\desk.jar') -Force
      $null = Install-Trade -Image $case.Image -Key $case.Key -Folder $case.Folder -Broadcast $stub
      $expected = (Get-Listing $case.Image).Replace($case.Image, $case.Folder)
      Assert-Same (Get-Listing $case.Folder) $expected
      Assert-Same (Get-RawPath $case.Key) "$base;$($case.Folder)"
      Assert-Calls 1
      Remove-Item -LiteralPath $case.Folder -Recurse -Force
      $null = [IO.Directory]::CreateDirectory((Join-Path $case.Folder 'app'))
      $null = Uninstall-Trade -Key $case.Key -Folder $case.Folder -Broadcast $stub
      Assert-True (-not (Test-Path -LiteralPath $case.Folder)) 'the folder stayed'
      Assert-Same (Get-RawPath $case.Key) $base
      Assert-Calls 2
    } finally {
      Remove-Case $case
    }
  }

  Invoke-Check 'B4 a program running from the folder is refused by both sequences and nothing changes' {
    $case = New-Case
    try {
      $standIn = Start-StandIn (Join-Path $case.Folder 'Desk.exe')
      try {
        Assert-Refused $case { Install-Trade -Image $case.Image -Key $case.Key -Folder $case.Folder -Broadcast $stub } @('Quit Desk', 'Desk.exe')
        Set-TestPath $case.Key "$base;$($case.Folder)" 'ExpandString'
        Assert-Refused $case { Uninstall-Trade -Key $case.Key -Folder $case.Folder -Broadcast $stub } @('Quit Desk', 'Desk.exe')
      } finally {
        Stop-StandIn $standIn
      }
    } finally {
      Remove-Case $case
    }
  }

  Invoke-Check 'B4 a String value gains the folder and stays String, and the uninstall gives it back' {
    $case = New-Case
    try {
      Set-TestPath $case.Key $base 'String'
      $null = Install-Trade -Image $case.Image -Key $case.Key -Folder $case.Folder -Broadcast $stub
      Assert-Same (Get-RawPath $case.Key) "$base;$($case.Folder)"
      Assert-Same (Get-PathKind $case.Key) 'String'
      Assert-True (Test-Path -LiteralPath (Join-Path $case.Folder 'trade.exe')) 'the folder has no trade.exe'
      Assert-Calls 1
      $null = Uninstall-Trade -Key $case.Key -Folder $case.Folder -Broadcast $stub
      Assert-Same (Get-RawPath $case.Key) $base
      Assert-Same (Get-PathKind $case.Key) 'String'
      Assert-True (-not (Test-Path -LiteralPath $case.Folder)) 'the folder stayed'
      Assert-Calls 2
    } finally {
      Remove-Case $case
    }
  }

  Invoke-Check 'B4 a MultiString value is refused by both sequences and the old Desk.exe stays' {
    $case = New-Case
    try {
      $case.Key.SetValue('Path', [string[]]@($base), [Microsoft.Win32.RegistryValueKind]::MultiString)
      Assert-Refused $case { Install-Trade -Image $case.Image -Key $case.Key -Folder $case.Folder -Broadcast $stub } @('REG_SZ', 'REG_EXPAND_SZ')
      Assert-True (Test-Path -LiteralPath (Join-Path $case.Folder 'Desk.exe')) 'the old Desk.exe went'
      $case.Key.SetValue('Path', [string[]]@("$base;$($case.Folder)"), [Microsoft.Win32.RegistryValueKind]::MultiString)
      Assert-Refused $case { Uninstall-Trade -Key $case.Key -Folder $case.Folder -Broadcast $stub } @('REG_SZ', 'REG_EXPAND_SZ')
    } finally {
      Remove-Case $case
    }
  }

  Invoke-Check 'B4 a held-open file stops the uninstall part way, and a rerun finishes it' {
    $case = New-Case
    try {
      $null = Install-Trade -Image $case.Image -Key $case.Key -Folder $case.Folder -Broadcast $stub
      $held = [IO.File]::Open((Join-Path $case.Folder 'runtime\release'), 'Open', 'Read', 'Read')
      try {
        Assert-Throws { Uninstall-Trade -Key $case.Key -Folder $case.Folder -Broadcast $stub } @('release')
      } finally {
        $held.Close()
      }
      Assert-True (Test-Path -LiteralPath (Join-Path $case.Folder 'app\desk.jar')) 'app\desk.jar went before the rest'
      Assert-Same (Get-RawPath $case.Key) $base
      Assert-Calls 2
      $null = Uninstall-Trade -Key $case.Key -Folder $case.Folder -Broadcast $stub
      Assert-True (-not (Test-Path -LiteralPath $case.Folder)) 'the folder stayed'
      Assert-Same (Get-RawPath $case.Key) $base
      Assert-Calls 2
    } finally {
      Remove-Case $case
    }
  }

  # jpackage writes app\.jpackage.xml, which Windows lists before app\desk.jar. Every image file
  # but the jar is locked, so a copy in listing order stops before the jar lands.
  Invoke-Check 'B4 a copy that stops part way leaves app\desk.jar, and a rerun repairs it' {
    $case = New-Case
    try {
      Write-TestFile (Join-Path $case.Image 'app\.jpackage.xml') 'image record'
      $others = @(Get-ChildItem -LiteralPath $case.Image -Recurse -Force | Where-Object { -not $_.PSIsContainer -and $_.Name -ne 'desk.jar' } | ForEach-Object { $_.FullName })
      $locks = @(Lock-TestFile $others)
      try {
        Assert-Throws { Install-Trade -Image $case.Image -Key $case.Key -Folder $case.Folder -Broadcast $stub } @()
      } finally {
        Unlock-TestFile $locks
      }
      Assert-True (Test-Path -LiteralPath (Join-Path $case.Folder 'app\desk.jar')) 'the copy stopped before app\desk.jar landed'
      Assert-Same ([IO.File]::ReadAllText((Join-Path $case.Folder 'app\desk.jar'))) 'new jar'
      Assert-Same (Get-RawPath $case.Key) $base
      Assert-Calls 0
      $null = Install-Trade -Image $case.Image -Key $case.Key -Folder $case.Folder -Broadcast $stub
      $expected = (Get-Listing $case.Image).Replace($case.Image, $case.Folder)
      Assert-Same (Get-Listing $case.Folder) $expected
      Assert-Same (Get-RawPath $case.Key) "$base;$($case.Folder)"
      Assert-Calls 1
    } finally {
      Remove-Case $case
    }
  }

  # B5: the entry scripts refuse a 32-bit and a non-elevated PowerShell, and touch nothing.
  $native = Join-Path $env:SystemRoot 'System32\WindowsPowerShell\v1.0\powershell.exe'
  $wow = Join-Path $env:SystemRoot 'SysWOW64\WindowsPowerShell\v1.0\powershell.exe'
  foreach ($entry in @((Join-Path $tools 'install-trade.ps1'), (Join-Path $tools 'uninstall-trade.ps1'))) {
    Invoke-Check "B5 $entry in the 32-bit PowerShell gives the 64-bit refusal" {
      Assert-True ([Environment]::Is64BitProcess) 'run this file in the 64-bit powershell.exe, since a 32-bit one reads System32 as SysWOW64'
      $before = Get-MachineState
      $result = Invoke-Entry $wow $entry
      Assert-True ($result.Code -ne 0) "the exit code was $($result.Code)"
      Assert-True ($result.Text.Contains('64-bit PowerShell')) "the output was: $($result.Text)"
      Assert-Same (Get-MachineState) $before
    }
    Invoke-Check "B5 $entry in the native PowerShell, not elevated, gives the administrator refusal" {
      Assert-True ([Environment]::Is64BitProcess) 'run this file in the 64-bit powershell.exe, since a 32-bit one reads System32 as SysWOW64'
      $before = Get-MachineState
      $result = Invoke-Entry $native $entry
      Assert-True ($result.Code -ne 0) "the exit code was $($result.Code)"
      Assert-True ($result.Text.Contains('Run as administrator')) "the output was: $($result.Text)"
      Assert-Same (Get-MachineState) $before
    }
  }
}

Write-Output "$script:passed passed, $script:failed failed"
if ($script:failed -gt 0) { exit 1 }
exit 0
