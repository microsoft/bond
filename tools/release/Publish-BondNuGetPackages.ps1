<#
    .SYNOPSIS
        Publishes Bond NuGet packages to NuGet feeds.

    .PARAMETER PackageSource
        The directory with the packages to publish.

    .PARAMETER PackageFeed
        The feed to publish to.

    .PARAMETER OmitSymbols
        Whether to skip pushing ".symbols.nupkg" packages and just push
        ".nupkg" pakages. Defaults to false

    .PARAMETER NuGetPath
        The full path to NuGet.exe to use. If not specificed, attempts to use
        NuGet.exe from $env:PATH.

    .PARAMETER ApiKey
        The API Key to use to authenticate to the feed.
        An ApiKey is not needed for VSTS feeds, as these need to use a
        credential provider. One is needed for NuGet.org, however.
#>

[CmdletBinding(SupportsShouldProcess=$True)]
param
(
    [Parameter(Mandatory=$True)]
    [string]
    $PackageSource,
    [Parameter(Mandatory=$True)]
    [string]
    $PackageFeed,
    [switch]
    $OmitSymbols = $false,
    [string]
    $NuGetPath = $null,
    [string]
    $ApiKey = "VSTS"
)

Set-StrictMode -Version Latest

function FindNuGet
{
    $getNugetError = $null
    $nugetCommands = @()
    $nugetCommands += Get-Command nuget -ErrorVariable getNugetError 2> $null

    if ($getNugetError)
    {
        throw "Could not find 'nuget' on path. Ensure it is on the path or use -NuGetPath to specify its full path"
    }
    elseif ($nugetCommands.Length -gt 1)
    {
        throw "Multiple 'nuget' commands found: $nugetCommands"
    }

    return $nugetCommands[0].Source
}

function UploadPackages
{
    $SymbolPackageExt = '.symbols.nupkg'
    $NormalPackageExt = '.nupkg'

    $BasePackageNames = `
      Get-ChildItem -File -Path $PackageSource | `
      where { $psitem.Extension -match "nupkg" } | `
      % { ($psitem.Name -replace ($SymbolPackageExt,'')) -replace ($NormalPackageExt, '') } | `
      select -Unique

    $BasePackageNames | foreach {
        $symbolPackage = Join-Path $PackageSource "$psitem$SymbolPackageExt"
        $normalPackage = Join-Path $PackageSource "$psitem$NormalPackageExt"

        $pushPackage = $normalPackage

        if (-not $OmitSymbols)
        {
            if (Test-Path -PathType Leaf -Path $symbolPackage)
            {
                $pushPackage = $symbolPackage
            }
            else
            {
                Write-Verbose "Symbols requested, but could not be found for '$psitem'. Tried '$symbolPackage'. Falling back to normal package."
                $pushPackage = $normalPackage
            }
        }

        if (-not (Test-Path -PathType Leaf -Path "$pushPackage"))
        {
            Write-Error "Could not find package to push for '$psitem'. Tried '$symbolPackage' and '$normalPackage', but neither could be found."
            return
        }

        if ($pscmdlet.ShouldProcess($pushPackage, "Push to $PackageFeed"))
        {
            & $NuGetPath push $pushPackage -Source $PackageFeed -ApiKey $ApiKey
            if (-not $?)
            {
                Write-Error "NuGet push failed for $($pushPackage): $LASTEXITCODE"
                return
            }
        }
    }
}


if (-not $NuGetPath)
{
    $NuGetPath = FindNuGet
}

UploadPackages