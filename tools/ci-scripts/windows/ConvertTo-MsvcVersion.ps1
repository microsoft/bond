<#
.SYNOPSIS

Convert an AppVeyor build worker image string into a MSVC version number.

.PARAMETER ImageName

The AppVeyor build worker image name.

.PARAMETER Format

The output format. One of 'VsNum', 'Year', or 'CMakeGeneratorNum'.

VsNum will produce something like '12.0' or '14.1'.
Year will produce something like '2013' or '2017'.
CMakeGeneratorNum will produce something like '12' or '15'.

#>
[CmdletBinding()]
param
(
    [string]
    $ImageName,

    [ValidateSet('VsNum','Year', 'CMakeGeneratorNum')]
    [string]
    $Format = 'VsNum'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function ToVsNum([string]$imageName)
{
    switch ($imageName)
    {
        'Visual Studio 2013' { '12.0' }
        'Visual Studio 2015' { '14.0' }
        'Visual Studio 2017' { '14.1' }
        default { throw "Unknown image name '$imageName'" }
    }
}

function ToYear([string]$vsNum)
{
    switch ($vsNum)
    {
        '12.0' { '2013' }
        '14.0' { '2015' }
        '14.1' { '2017' }
        default { throw "Unknown VsNum '$vsNum'" }
    }
}

function ToCMakeGeneratorNum([string]$vsNum)
{
    switch ($vsNum)
    {
        '12.0' { '12' }
        '14.0' { '14' }
        '14.1' { '15' }
        default { throw "Unknown VsNum '$vsNum'" }
    }
}

switch ($Format)
{
    'VsNum' { return ToVsNum $ImageName }
    'Year' { return ToYear (ToVsNum $ImageName) }
    'CMakeGeneratorNum' { return ToCMakeGeneratorNum (ToVsNum $ImageName) }
    default { throw "Unknown Format '$Format'" }
}
