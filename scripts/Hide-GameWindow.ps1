param(
 [Parameter(Mandatory)][int]$GamePid,
 # The bot's output; its frame-0 "native-rendering" trace marks a running game.
 [Parameter(Mandatory)][string]$BotLog
)
# BWAPI clamps the ini window position to the screen, so a headless game would show at 0,0. StarCraft needs its window
# active while BWAPI clicks through the menus, so the window moves off-screen only once a game runs, and back off
# again whenever a warm session's next game puts it on screen.
$ErrorActionPreference = 'Stop'
Add-Type -TypeDefinition @'
using System; using System.Runtime.InteropServices;
public static class GameWindow {
  public struct RECT { public int L, T, R, B; }
  [DllImport("user32.dll")] public static extern bool GetWindowRect(IntPtr h, out RECT r);
  [DllImport("user32.dll")] public static extern bool SetWindowPos(IntPtr h, IntPtr after, int x, int y, int cx, int cy, uint flags);
}
'@
$NoSizeNoZOrderNoActivate = 0x0001 -bor 0x0004 -bor 0x0010
while ($game = Get-Process -Id $GamePid -ErrorAction SilentlyContinue) {
  $running = (Test-Path -LiteralPath $BotLog) -and (Select-String -LiteralPath $BotLog -Pattern 'event=native-rendering' -Quiet)
  if ($running -and $game.MainWindowHandle -ne 0) {
    $rect = New-Object GameWindow+RECT
    if ([GameWindow]::GetWindowRect($game.MainWindowHandle, [ref]$rect) -and $rect.L -gt -5000) {
      [void][GameWindow]::SetWindowPos($game.MainWindowHandle, [IntPtr]::Zero, -10000, -10000, 0, 0, $NoSizeNoZOrderNoActivate)
    }
  }
  Start-Sleep -Milliseconds 1000
}
