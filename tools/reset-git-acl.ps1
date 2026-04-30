$ErrorActionPreference = 'Stop'

if (-not (Test-Path -LiteralPath '.git')) {
  Write-Error "No .git directory found. Run this from the repository root."
  exit 1
}

Write-Output "Resetting ACLs on .git (may require an elevated PowerShell)..."
icacls .git /reset /T /C | Out-Null

Write-Output "Current .git ACL:"
icacls .git

Write-Output "Done."

