$ErrorActionPreference = 'Stop'

function Get-NormalizedPath([string]$path) {
  if ([string]::IsNullOrWhiteSpace($path)) { return $null }
  try {
    return ([IO.Path]::GetFullPath($path)).TrimEnd('\')
  } catch {
    return $path.Trim().TrimEnd('\')
  }
}

function Test-PathInSemicolonList([string]$candidate, [string]$list) {
  $candidateNorm = Get-NormalizedPath $candidate
  if (-not $candidateNorm) { return $false }

  $parts = @()
  if (-not [string]::IsNullOrWhiteSpace($list)) {
    $parts = $list -split ';' | ForEach-Object { Get-NormalizedPath $_ } | Where-Object { $_ }
  }

  return $parts -contains $candidateNorm
}

function Get-RInstallations([string]$rRoot) {
  if (-not (Test-Path -LiteralPath $rRoot)) { return @() }

  $dirs =
    Get-ChildItem -LiteralPath $rRoot -Directory -ErrorAction SilentlyContinue |
    Where-Object { $_.Name -like 'R-*' }

  $candidates = foreach ($d in $dirs) {
    $verText = $d.Name.Substring(2)
    $ver = $null
    try { $ver = [Version]$verText } catch { }
    [PSCustomObject]@{
      VersionText = $verText
      Version     = $ver
      Path        = $d.FullName
    }
  }

  $sorted = $candidates | Sort-Object -Property @{
    Expression = { if ($_.Version) { $_.Version } else { [Version]'0.0' } }
  }, @{
    Expression = { $_.VersionText }
  } -Descending

  return @($sorted)
}

$rRoot = Join-Path -Path $env:ProgramFiles -ChildPath 'R'
$installs = Get-RInstallations -rRoot $rRoot

$rscriptExe = $null
foreach ($inst in $installs) {
  $primary = Join-Path -Path $inst.Path -ChildPath 'bin\Rscript.exe'
  if (Test-Path -LiteralPath $primary) { $rscriptExe = $primary; break }

  $x64 = Join-Path -Path $inst.Path -ChildPath 'bin\x64\Rscript.exe'
  if (Test-Path -LiteralPath $x64) { $rscriptExe = $x64; break }
}

if (-not $rscriptExe) {
  Write-Error "Rscript.exe not found under '$rRoot'. Install R (e.g., to Program Files) and re-run this script."
  exit 1
}

$binDir = Split-Path -Path $rscriptExe -Parent

$userPath = [Environment]::GetEnvironmentVariable('Path', 'User')
if (-not (Test-PathInSemicolonList -candidate $binDir -list $userPath)) {
  $newUserPath = if ([string]::IsNullOrWhiteSpace($userPath)) { $binDir } else { "$binDir;$userPath" }
  try {
    [Environment]::SetEnvironmentVariable('Path', $newUserPath, 'User')
  } catch {
    Write-Error ("Failed to persist USER Path update (registry permission). Re-run this script from an elevated PowerShell. Details: " + $_.Exception.Message)
    exit 1
  }
  Write-Output "Added to USER Path: $binDir"
} else {
  Write-Output "USER Path already contains: $binDir"
}

if (-not (Test-PathInSemicolonList -candidate $binDir -list $env:Path)) {
  $env:Path = "$binDir;$env:Path"
}

$resolved = (Get-Command -Name 'Rscript.exe' -ErrorAction Stop).Source
Write-Output "Rscript resolved to: $resolved"

try {
  Add-Type -Namespace Win32 -Name NativeMethods -MemberDefinition @'
[System.Runtime.InteropServices.DllImport("user32.dll", SetLastError=true, CharSet=System.Runtime.InteropServices.CharSet.Auto)]
public static extern System.IntPtr SendMessageTimeout(System.IntPtr hWnd, int Msg, System.IntPtr wParam, string lParam, int fuFlags, int uTimeout, out System.IntPtr lpdwResult);
'@ -ErrorAction Stop | Out-Null

  $HWND_BROADCAST = [IntPtr]0xffff
  $WM_SETTINGCHANGE = 0x001A
  $SMTO_ABORTIFHUNG = 0x0002
  $result = [IntPtr]::Zero
  [void][Win32.NativeMethods]::SendMessageTimeout($HWND_BROADCAST, $WM_SETTINGCHANGE, [IntPtr]::Zero, 'Environment', $SMTO_ABORTIFHUNG, 5000, [ref]$result)
} catch {
  Write-Output "Note: Unable to broadcast environment change notification (non-fatal)."
}
