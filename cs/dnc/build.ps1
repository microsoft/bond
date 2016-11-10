<#
.SYNOPSIS

Builds Bond with the .NET Core toolchain

.PARAMETER Configuration

The configuration to build. Valid values are:
- debug
- release

.PARAMETER DelaySignKey

The optional delay sign key to use.

.PARAMETER Test

Whether to run the tests.

.EXAMPLE

PS c:\src\bond\cs\dnc> .\build.ps1

This will build just the debug configuration.

.EXAMPLE

PS c:\src\bond> .\cs\dnc\build.ps1 -Configuration release -DelaySignKey c:\src\private\delay-key.snk -Test

This will build the release configuration, delay sign it with the specified
key, and run all the test projects.

#>
[CmdletBinding()]
param
(
    [ValidateSet("debug", "release")]
    [string]
    $Configuration = "debug",

    [string]
    $DelaySignKey = "",

    [switch]
    $Test = $false
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

try
{
    $script:dotnet_cfg = $Configuration

    # We push to a stack other than the default one so that we don't
    # accidentally change its contents.
    #
    # We have to set our location to the same directory as this script as
    # dotnet globbing doesn't work with paths like
    # c:\src\bond\cs\dnc\**\project.json
    $script:script_dir = Split-Path -LiteralPath $PSCommandPath
    Push-Location -Path (Split-Path -LiteralPath $PSCommandPath) -StackName bond_dnc_build

    if ($DelaySignKey) {
        mkdir -Force 'keys'
        if (-not $?) {
            throw "Could not create directory '$script:script_dir\keys'"
        }

        Copy-Item -Force $DelaySignKey '.\keys\delay-sign.snk'
        if (-not $?) {
            throw "Copying delay sign key '$DelaySignKey' to '$script:script_dir\keys\delay-sign.snk' failed"
        }

        $script:dotnet_cfg += "-delay"
    }

    msbuild /m /p:Configuration=$Configuration /p:Platform=Win32 '..\Compiler.vcxproj'
    if (-not $?) {
        throw "Building GBC failed."
    }

    msbuild /m /p:Configuration=$Configuration 'dirs.proj'
    if (-not $?) {
        throw "Code generation failed."
    }

    dotnet restore
    if (-not $?) {
        throw "Package restore failed."
    }

    # Due to a bug in the way 'dotnet build' resolves project references, we
    # need to list out attributes and reflection first so that they get built
    # before any of the other projects.
    #
    # See also: https://github.com/dotnet/cli/issues/2639
    dotnet build --configuration $script:dotnet_cfg '.\src\attributes\project.json' '.\src\reflection\project.json' '**\project.json'
    if (-not $?) {
        throw ".NET Core build failed."
    }

    if ($Test) {
        $testProjectJsonPaths =  Get-ChildItem -File -Recurse project.json |
          Select-String 'testRunner' |
          Select-Object -ExpandProperty Path |
          Select-Object -Unique

        $testProjectJsonPaths | foreach {
            dotnet test --no-build --configuration $script:dotnet_cfg $psitem
            if (-not $?) {
                throw "Tests failed for $psitem"
            }
        }
    }
} finally {
    Pop-Location -StackName bond_dnc_build
}
