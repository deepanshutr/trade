# Functions that install Desk as trade.exe in one folder and put that folder on the machine PATH.
# Dot-source this file. It only defines functions, so nothing runs when it is read.
# tools/install-trade.ps1 and tools/uninstall-trade.ps1 call them, and so does
# test/test-trade-windows.ps1 with an HKCU key, temporary folders and a stub broadcast.
# Callers set $ErrorActionPreference to Stop, so any failed step stops the sequence.
# ASCII only, and nothing newer than Windows PowerShell 5.1.

# True when a PATH entry names the folder, compared without case and without trailing backslashes.
function Test-TradeEntry {
  param([string]$Entry, [string]$Folder)
  [string]::Equals($Entry.TrimEnd([char]'\'), $Folder.TrimEnd([char]'\'), [StringComparison]::OrdinalIgnoreCase)
}

# A value made only of separators has no entry. Writing the folder alone there would drop every
# other entry, so both string functions refuse it. Windows stops reading a value at a NUL
# character, so a folder added after one would never be found. Such a value is refused too.
function Assert-TradeValue {
  param([string]$Raw)
  if ($Raw.IndexOf([char]0) -ge 0) { throw 'The PATH value holds a NUL character, so nothing changed.' }
  if ($Raw.Replace(';', '') -eq '') { throw 'The PATH value has no entry, so nothing changed.' }
}

# Puts the folder after the last non-empty entry, before any trailing separators. When an entry
# already names the folder, the value comes back as it was.
function Add-TradePath {
  param([string]$Raw, [string]$Folder)
  Assert-TradeValue $Raw
  foreach ($entry in $Raw -split ';') {
    if (Test-TradeEntry $entry $Folder) { return $Raw }
  }
  $body = $Raw.TrimEnd([char]';')
  $body + ';' + $Folder + $Raw.Substring($body.Length)
}

# Drops every entry that names the folder and keeps every other character, empty entries too.
function Remove-TradePath {
  param([string]$Raw, [string]$Folder)
  Assert-TradeValue $Raw
  $kept = foreach ($entry in $Raw -split ';') {
    if (-not (Test-TradeEntry $entry $Folder)) { $entry }
  }
  @($kept) -join ';'
}

# Reads Path from the open key with its %Name% parts kept. Only REG_EXPAND_SZ passes, since a
# plain string would stop Windows filling in those parts.
function Read-TradePath {
  param($Key)
  if ($Key.GetValueKind('Path') -ne [Microsoft.Win32.RegistryValueKind]::ExpandString) {
    throw "The Path value in $($Key.Name) is not REG_EXPAND_SZ, so nothing changed."
  }
  [string]$Key.GetValue('Path', $null, [Microsoft.Win32.RegistryValueOptions]::DoNotExpandEnvironmentNames)
}

# Adds the folder to Path in the open key, or removes it with -Remove. Writes REG_EXPAND_SZ only
# when the value changes, and returns whether it wrote.
# [Environment]::SetEnvironmentVariable is never used. In Windows PowerShell 5.1 it writes a plain
# string, and its read side expands every entry.
function Update-TradePath {
  param($Key, [string]$Folder, [switch]$Remove)
  $raw = Read-TradePath $Key
  if ($Remove) { $new = Remove-TradePath $raw $Folder } else { $new = Add-TradePath $raw $Folder }
  if ($new -ceq $raw) { return $false }
  # A remove can leave no entry when the folder was the only one. That value is never written.
  Assert-TradeValue $new
  $Key.SetValue('Path', $new, [Microsoft.Win32.RegistryValueKind]::ExpandString)
  $true
}

# Tells Explorer, and every terminal it starts afterwards, to read the environment again.
# WM_SETTINGCHANGE with the text Environment goes to every top-level window. A hung window is
# skipped, and each one gets at most 5 seconds.
# It runs after the PATH write, and a rerun that writes nothing sends nothing. So a failure here
# only warns and never stops the sequence.
function Send-SettingChange {
  $sent = [IntPtr]::Zero
  try {
    if (-not ('DeskTrade.User32' -as [type])) {
      Add-Type -ErrorAction Stop -Namespace DeskTrade -Name User32 -MemberDefinition @'
[DllImport("user32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
public static extern IntPtr SendMessageTimeout(IntPtr hWnd, uint msg, UIntPtr wParam, string lParam, uint flags, uint timeout, out UIntPtr result);
'@
    }
    $result = [UIntPtr]::Zero
    # HWND_BROADCAST is 0xffff, WM_SETTINGCHANGE is 0x1a and SMTO_ABORTIFHUNG is 2.
    $sent = [DeskTrade.User32]::SendMessageTimeout([IntPtr]0xffff, 0x1a, [UIntPtr]::Zero, 'Environment', 2, 5000, [ref]$result)
  } catch {
    $sent = [IntPtr]::Zero
  }
  if ($sent -eq [IntPtr]::Zero) {
    Write-Warning 'Windows did not confirm the PATH change. If a new terminal does not find trade, sign out and in.'
  }
}

# Processes whose program lies inside the folder. A process whose path cannot be read is skipped.
function Get-DeskProcess {
  param([string]$Folder)
  $prefix = $Folder.TrimEnd([char]'\') + '\'
  foreach ($process in Get-Process) {
    $path = $null
    try { $path = $process.Path } catch { $path = $null }
    if ($path -and $path.StartsWith($prefix, [StringComparison]::OrdinalIgnoreCase)) { $process }
  }
}

# Removes everything but app\desk.jar, then the jar, then the folder. A removal that stops part
# way leaves the jar, or no file at all when only the folder itself stayed. Either way the folder
# still passes the Desk folder check on the rerun.
function Remove-TradeFolder {
  param([string]$Folder)
  $app = Join-Path $Folder 'app'
  $jar = Join-Path $app 'desk.jar'
  Get-ChildItem -LiteralPath $Folder -Force | Where-Object { $_.Name -ne 'app' } | Remove-Item -Recurse -Force -ErrorAction Stop
  if (Test-Path -LiteralPath $app) {
    Get-ChildItem -LiteralPath $app -Force | Where-Object { $_.Name -ne 'desk.jar' } | Remove-Item -Recurse -Force -ErrorAction Stop
  }
  if (Test-Path -LiteralPath $jar) { Remove-Item -LiteralPath $jar -Force -ErrorAction Stop }
  Remove-Item -LiteralPath $Folder -Recurse -Force -ErrorAction Stop
}

# Copies the image into a new folder, app\desk.jar first, then everything else. A copy that stops
# part way leaves the jar, so the folder still passes the Desk folder check on the rerun.
function Copy-TradeImage {
  param([string]$Image, [string]$Folder)
  $app = Join-Path $Folder 'app'
  $null = [IO.Directory]::CreateDirectory($app)
  Copy-Item -LiteralPath (Join-Path $Image 'app\desk.jar') -Destination (Join-Path $app 'desk.jar') -Force -ErrorAction Stop
  Get-ChildItem -LiteralPath $Image -Force | Where-Object { $_.Name -ne 'app' } | Copy-Item -Destination $Folder -Recurse -Force -ErrorAction Stop
  Get-ChildItem -LiteralPath (Join-Path $Image 'app') -Force | Where-Object { $_.Name -ne 'desk.jar' } | Copy-Item -Destination $app -Recurse -Force -ErrorAction Stop
}

# The folder, when it exists, holds app\desk.jar or no file at all, so another program's folder is
# never removed. No process runs from it.
function Assert-TradeFolder {
  param([string]$Folder)
  if ((Test-Path -LiteralPath $Folder) -and -not (Test-Path -LiteralPath (Join-Path $Folder 'app\desk.jar') -PathType Leaf) -and
      @(Get-ChildItem -LiteralPath $Folder -Recurse -Force | Where-Object { -not $_.PSIsContainer }).Count -gt 0) {
    throw "$Folder holds files but no app\desk.jar, so it is not a Desk folder. Nothing changed."
  }
  $running = @(Get-DeskProcess $Folder)
  if ($running.Count -gt 0) {
    $paths = @($running | ForEach-Object { $_.Path }) -join ', '
    throw "Quit Desk first. $paths runs from $Folder. Nothing changed."
  }
}

# The 64-bit process and the administrator check of both entry scripts.
# A 32-bit PowerShell reads $env:ProgramFiles as C:\Program Files (x86).
function Assert-TradeShell {
  if (-not [Environment]::Is64BitProcess) {
    throw 'Run this script in the 64-bit PowerShell, C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe. Nothing changed.'
  }
  $principal = [Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()
  if (-not $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    throw 'Run this script in a PowerShell opened with Run as administrator. Nothing changed.'
  }
}

# Copies the image into the folder and adds the folder to Path in the key. Nothing changes until
# every check passes. A rerun is the upgrade: it replaces the folder, adds no second entry and
# makes no broadcast. Prints one line per change.
function Install-Trade {
  param([string]$Image, $Key, [string]$Folder, [scriptblock]$Broadcast)
  foreach ($part in @('trade.exe', 'app\desk.jar')) {
    if (-not (Test-Path -LiteralPath (Join-Path $Image $part) -PathType Leaf)) {
      throw "The image $Image holds no $part. Run make windows-image first. Nothing changed."
    }
  }
  Assert-TradeFolder $Folder
  $null = Add-TradePath (Read-TradePath $Key) $Folder
  if (Test-Path -LiteralPath $Folder) {
    Remove-TradeFolder $Folder
    "Removed the earlier $Folder."
  }
  Copy-TradeImage $Image $Folder
  "Copied $Image to $Folder."
  if (Update-TradePath $Key $Folder) {
    "Added $Folder to the end of the machine PATH."
    & $Broadcast
  }
}

# Removes the folder from Path in the key, then removes the folder. The same checks as the
# install come first. Update-TradePath checks the value before it writes, and the folder goes
# only after it. Settings and state in the user profile stay. Prints one line per change.
function Uninstall-Trade {
  param($Key, [string]$Folder, [scriptblock]$Broadcast)
  Assert-TradeFolder $Folder
  if (Update-TradePath $Key $Folder -Remove) {
    "Removed $Folder from the machine PATH."
    & $Broadcast
  }
  if (Test-Path -LiteralPath $Folder) {
    Remove-TradeFolder $Folder
    "Removed $Folder."
  }
}
