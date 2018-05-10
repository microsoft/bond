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

$stackInstaller = [System.IO.Path]::Combine($OutputDirectory, 'stack-x86_64.zip')
Write-Debug "Downloading to $stackInstaller"

[System.Net.ServicePointManager]::SecurityProtocol='TLS12'
Invoke-WebRequest `
    -Uri https://www.stackage.org/stack/windows-x86_64 `
    -OutFile $stackInstaller

$InstallDirectory = [System.IO.Path]::Combine($OutputDirectory, 'stack')
Write-Debug "Extracting to $InstallDirectory"

[System.IO.Compression.ZipFile]::ExtractToDirectory($stackInstaller, $InstallDirectory)

Write-Host "Stack: $(& "$InstallDirectory\stack.exe" --version)"
Write-Output $InstallDirectory
