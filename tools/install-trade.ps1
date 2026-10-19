# Installs Desk as C:\Program Files\Desk\trade.exe and adds that folder to the end of the
# machine PATH. Build the image with make windows-image, quit Desk, then run this in a
# PowerShell opened with Run as administrator:
#   powershell -NoProfile -ExecutionPolicy Bypass -File tools\install-trade.ps1
# A rerun is the upgrade. tools\uninstall-trade.ps1 removes both.
$ErrorActionPreference = 'Stop'
. (Join-Path $PSScriptRoot 'trade-windows.ps1')
try {
  Assert-TradeShell
  $folder = Join-Path $env:ProgramFiles 'Desk'
  $image = Join-Path (Split-Path $PSScriptRoot -Parent) 'bin\image\trade'
  $key = [Microsoft.Win32.Registry]::LocalMachine.OpenSubKey('SYSTEM\CurrentControlSet\Control\Session Manager\Environment', $true)
  try {
    # The PATH step compares entries as written. A quoted "C:\Program Files\Desk" or
    # %ProgramFiles%\Desk counts as a different entry, so trade can show twice. Remove it by hand.
    Install-Trade -Image $image -Key $key -Folder $folder -Broadcast { Send-SettingChange }
  } finally {
    $key.Close()
  }
} catch {
  [Console]::Error.WriteLine($_.Exception.Message)
  exit 1
}
"Installed $folder\trade.exe. Point the firewall rule for Desk at it, then open a new terminal from Start and type trade."
