# Removes C:\Program Files\Desk from the machine PATH, then removes the folder. Desk's settings
# and state in the user profile stay. Quit Desk, then run this in a PowerShell opened with
# Run as administrator:
#   powershell -NoProfile -ExecutionPolicy Bypass -File tools\uninstall-trade.ps1
$ErrorActionPreference = 'Stop'
. (Join-Path $PSScriptRoot 'trade-windows.ps1')
try {
  Assert-TradeShell
  $folder = Join-Path $env:ProgramFiles 'Desk'
  $key = [Microsoft.Win32.Registry]::LocalMachine.OpenSubKey('SYSTEM\CurrentControlSet\Control\Session Manager\Environment', $true)
  try {
    Uninstall-Trade -Key $key -Folder $folder -Broadcast { Send-SettingChange }
  } finally {
    $key.Close()
  }
} catch {
  [Console]::Error.WriteLine($_.Exception.Message)
  exit 1
}
