<#
.SYNOPSIS

Get's the location of Boost that is installed in the AppVeyor-provided images.

Details for these locations can be found at
https://www.appveyor.com/docs/build-environment/#pre-installed-software

.PARAMETER Version

The Boost version to find.

.PARAMETER VcToolsetVer

Which MSVC toolset version to find libraries for, in Visual Studio toolset
version number format. (E.g., 12.0, 14.1).

#>
[CmdletBinding()]
param
(
    [string]
    $Version,

    [ValidateSet('12.0', '14.0', '14.1', '14.2')]
    [string]
    $VcToolsetVer
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function FindBoostLibPath([string]$vcToolsetVer, [string]$boostRoot, [string]$platform)
{
    $boostLibPath = "$boostRoot\lib$platform-msvc-$vcToolsetVer"

    Write-Debug "Checking for libs under '$boostLibPath'"

    if (Test-Path -PathType Leaf "$boostLibPath\*.lib")
    {
        return $boostLibPath
    }
    else
    {
        return $null
    }
}

function FindBoostLibPaths([string]$vcToolsetVer, [string]$boostRoot)
{
    $boostLibDirs = @{}
    $lib32Dir = FindBoostLibPath $vcToolsetVer $boostRoot '32'
    $lib64Dir = FindBoostLibPath $vcToolsetVer $boostRoot '64'

    if ((-not $lib32Dir) -and (-not $lib64Dir))
    {
        Write-Debug "Could not find any libraries. Assuming Boost installation is broken."
        return $null
    }

    if ($lib32Dir)
    {
        $boostLibDirs['32'] = $lib32Dir
    }

    if ($lib64Dir)
    {
        $boostLibDirs['64'] = $lib64Dir
    }

    return $boostLibDirs
}

$boostRoot = "C:\Libraries\boost_$($Version.Replace('.', '_'))"
Write-Debug "Computed boostRoot: '$boostRoot'"

$configHpp = "$boostRoot\boost\config.hpp"

if (-not (Test-Path -PathType Leaf $configHpp))
{
    Write-Debug "Could not find '$configHpp'. Assuming Boost installation is broken."
    return $null
}

$boostLibDirs = FindBoostLibPaths $VcToolsetVer $boostRoot
if (-not $boostLibDirs)
{
    return $null
}

return @{'BOOST_ROOT' = $boostRoot; 'BOOST_LIBRARYDIR' = $boostLibDirs }
