<#
.SYNOPSIS

Convert an AppVeyor build worker image string into a MSVC version number.

.PARAMETER ImageName

The AppVeyor build worker image name.

.PARAMETER Format

The output format. One of 'VcToolsetVer', 'Year', or 'CMakeGeneratorNum'.

VcToolsetVer will produce something like '12.0' or '14.1'.
Year will produce something like '2013' or '2017'.
CMakeGeneratorNum will produce something like '12' or '15'.

#>
[CmdletBinding()]
param
(
    [string]
    $ImageName,

    [ValidateSet('VcToolsetVer','Year', 'CMakeGeneratorNum')]
    [string]
    $Format = 'VcToolsetVer'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function ToVcToolsetVer([string]$imageName)
{
    switch ($imageName)
    {
        'Visual Studio 2013' { '12.0' }
        'Visual Studio 2015' { '14.0' }
        'Visual Studio 2017' { '14.1' }
        'Visual Studio 2019' { '14.2' }
        default { throw "Unknown image name '$imageName'" }
    }
}

function ToYear([string]$vcToolsetVer)
{
    switch ($vcToolsetVer)
    {
        '12.0' { '2013' }
        '14.0' { '2015' }
        '14.1' { '2017' }
        '14.2' { '2019' }
        default { throw "Unknown VcToolsetVer '$vcToolsetVer'" }
    }
}

function ToCMakeGeneratorNum([string]$vcToolsetVer)
{
    switch ($vcToolsetVer)
    {
        '12.0' { '12' }
        '14.0' { '14' }
        '14.1' { '15' }
        '14.2' { '16' }
        default { throw "Unknown VcToolsetVer '$vcToolsetVer'" }
    }
}

switch ($Format)
{
    'VcToolsetVer' { return ToVcToolsetVer $ImageName }
    'Year' { return ToYear (ToVcToolsetVer $ImageName) }
    'CMakeGeneratorNum' { return ToCMakeGeneratorNum (ToVcToolsetVer $ImageName) }
    default { throw "Unknown Format '$Format'" }
}
