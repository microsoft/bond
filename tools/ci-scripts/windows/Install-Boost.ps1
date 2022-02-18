<#
.SYNOPSIS

Downloads Boost via NuGet and returns the BOOST_ROOT and BOOST_LIBRARYDIR
locations.

.PARAMETER Version

The Boost version to install.

.PARAMETER Components

The Boost binary components to install. The Boost headers are always installed.

.PARAMETER VcToolsetVer

Which MSVC toolset version to install libraries for, in Visual Studio toolset
version number format. (E.g., 12.0, 14.1).

.PARAMETER OutputDirectory

Where to install Boost. If not set, creates a directory in the TEMP folder.

#>
[CmdletBinding(SupportsShouldProcess=$True)]
param
(
    [string]
    $Version,

    [ValidateSet('12.0', '14.0', '14.1', '14.2')]
    [string]
    $VcToolsetVer,
    
    [string[]]
    $Components = (
        'boost_chrono',
        'boost_date_time',
        'boost_thread',
        'boost_system',
        'boost_unit_test_framework'),

    [string]
    $OutputDirectory = $null
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function ConvertVcToolsetVer-ToBoostPackageFormat([string]$vcToolsetVer)
{
    return $vcToolsetVer.Replace('.', '')
}

function Install-NuGetPackage([string]$InstallDir, [string]$PackageId, [string]$PackageVersion)
{
    $nugetPrereleaseFlag = @()
    if ($packageVersion.Contains('-')) {
        $nugetPrereleaseFlag += 'Prerelease'
    }

    nuget install $packageId `
        -OutputDirectory $installDir `
        -Version $packageVersion `
        -ExcludeVersion `
        $nugetPrereleaseFlag

    if (-not $?)
    {
        throw "NuGet install of '$packageId' '$packageVersion' failed. Exit code $LastExitCode"
    }
}

if (-not $OutputDirectory)
{
    $OutputDirectory = [System.IO.Path]::Combine(
        [System.IO.Path]::GetTempPath(),
        [System.IO.Path]::GetRandomFileName())
    Write-Debug "OutputDirectory not set. Generated temp folder name: '$OutputDirectory'"
}

if (-not (Test-Path -LiteralPath $OutputDirectory -PathType Container))
{
    mkdir $OutputDirectory | Out-Null
}

$workDir = [System.IO.Path]::Combine($OutputDirectory, 'w')
$destDir = [System.IO.Path]::Combine($OutputDirectory, 'd')
$lib32Dir = [System.IO.Path]::Combine($destDir, 'lib32')
$lib64Dir = [System.IO.Path]::Combine($destDir, 'lib64')
mkdir $workDir | Out-Null
mkdir $destDir | Out-Null
mkdir $lib32Dir | Out-Null
mkdir $lib64Dir | Out-Null

function Install-BoostHeaders
{
    Write-Progress -Activity 'Installing Boost' -Status "Installing 'boost' (headers) version $Version"

    if ($PSCmdlet.ShouldProcess("boost version $Version", 'Install NuGet package'))
    {
        Install-NuGetPackage `
            -PackageId 'boost' `
            -InstallDir $workDir `
            -PackageVersion $Version

        Move-Item `
            -Path ([System.IO.Path]::Combine($workDir, 'boost', 'lib', 'native', 'include', 'boost')) `
            -Destination $destDir
    }
}

function Install-BoostComponent([string]$Component)
{
    $packageId = "$Component-vc$(ConvertVcToolsetVer-ToBoostPackageFormat $VcToolsetVer)"

    Write-Progress -Activity 'Installing Boost' -Status "Installing '$packageId' version $Version"

    if ($PSCmdlet.ShouldProcess("$packageId version $Version", 'Install NuGet package'))
    {
        Install-NuGetPackage `
            -PackageId $packageId `
            -InstallDir $workDir `
            -PackageVersion $Version

        Move-Item `
            -Path ([System.IO.Path]::Combine($workDir, $packageId, 'lib', 'native', 'address-model-32', 'lib', '*')) `
            -Destination $lib32Dir

        Move-Item `
            -Path ([System.IO.Path]::Combine($workDir, $packageId, 'lib', 'native', 'address-model-64', 'lib', '*')) `
            -Destination $lib64Dir
    }
}

Install-BoostHeaders | Out-Null

$Components | % `
{
    Install-BoostComponent -Component $PSItem | Out-Null
}

Write-Progress -Activity 'Installing Boost' -Completed

return @{'BOOST_ROOT' = $destDir; 'BOOST_LIBRARYDIR'= @{'32' = $lib32Dir; '64' = $lib64Dir}}
