[CmdletBinding()]
param ( )

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

Add-Type -AssemblyName 'System.IO.Compression.FileSystem'

$OutputDirectory = [System.IO.Path]::Combine(
    [System.IO.Path]::GetTempPath(),
    [System.IO.Path]::GetRandomFileName())

if (-not (Test-Path -LiteralPath $OutputDirectory -PathType Container))
{
    mkdir $OutputDirectory | Out-Null
}

$stackInstaller = [System.IO.Path]::Combine($OutputDirectory, 'stack.zip')
Write-Debug "Downloading to $stackInstaller"

[System.Net.ServicePointManager]::SecurityProtocol='TLS12'
Invoke-WebRequest `
    -Uri https://www.stackage.org/stack/windows-x86_64 `
    -OutFile $stackInstaller

$stackInstallPath = "$env:temp\stack-$(New-Guid)\"
Write-Debug "Extracting to $stackInstallPath"
mkdir $stackInstallPath | Out-Null

[System.IO.Compression.ZipFile]::ExtractToDirectory($stackInstaller, $stackInstallPath)

Write-Host "Stack: $(& "$stackInstallPath\stack.exe" --version)"
Write-Output $stackInstallPath
