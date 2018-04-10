<#
.SYNOPSIS

Downloads Boost via NuGet and returns the BOOST_ROOT and BOOST_LIBRARYDIR
locations.

.PARAMETER Version

The Boost version to install.

.PARAMETER Components

The Boost binary components to install. The Boost headers are always installed.

.PARAMETER VsNum

Which MSVC compiler version to find libraries for, in Visual Studio version
number format. (E.g., 12.0, 14.1).

.PARAMETER OutputDirectory

Where to install Boost. If not set, creates a directory in the TEMP folder.

#>
[CmdletBinding(SupportsShouldProcess=$True)]
param
(
    [string]
    $Version,

    [ValidateSet('12.0', '14.0', '14.1')]
    [string]
    $VsNum,
    
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

function Get-TempFolder
{
    $base = [System.IO.Path]::GetTempPath()
    $result = ''

    do
    {
        $randomPart = ''
        while ($randomPart.Length -lt 5)
        {
            $asciiBase = 97 # ASCII for 'a'

            $r = Get-Random -Minimum 0 -Maximum 36
            if ($r -ge 26) # We use [26-35] for numbers
            {
                $asciiBase = 48 # ASCII for '0'
                $r -= 26 # adjust to be in [0-9] from [26-36].
            }

            $randomPart += [char]($r + $asciiBase)
        }

        $result = [System.IO.Path]::Combine($base, $randomPart)
    } while (Test-Path -LiteralPath $result -PathType Container)

    # There's a TOCTTOU race in this check, but it's very unlikely.
    Write-Debug "Generated temp folder name: '$result'"
    return $result
}

function ConvertVsNum-ToBoostPackageFormat([string]$vsNum)
{
    return $vsNum.Replace('.', '')
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
    $OutputDirectory = Get-TempFolder
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
    Write-Progress -Activity 'Installing Boost' -Status "Installing 'boost' (headers)"

    if ($PSCmdlet.ShouldProcess('boost', 'Install NuGet package'))
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
    $packageId = "$Component-vc$(ConvertVsNum-ToBoostPackageFormat $VsNum)"

    Write-Progress -Activity 'Installing Boost' -Status "Installing '$packageId'"

    if ($PSCmdlet.ShouldProcess($packageId, 'Install NuGet package'))
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
