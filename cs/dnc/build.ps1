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

.PARAMETER Verbosity

The verbosity of the build process. Valid values are, in increasing order of
verbosity:
- quiet
- minimal
- normal
- detailed

.PARAMETER MSBuildLogger

An optional logger to pass to MSBuild. Consult the MSBuild help for the
switch /logger: for details about the syntax.

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
    $Test = $false,

    [ValidateSet("quiet", "minimal", "normal", "detailed")]
    [string]
    $Verbosity = "minimal",

    [string]
    $MSBuildLogger = ""
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function ComputeMSBuildVerbosity() {
    # The set of values we use map directly to MSBuild levels.
    $Verbosity
}

function ComputeDotNetRestoreVerbosity() {
    switch ($Verbosity) {
        'quiet' { 'minimal' }
        'minimal' { 'minimal' }
        'normal' { 'information' }
        'detailed' { 'debug' }
    }
}

try
{
    $script:msb_common = ("/m", "/verbosity:$(ComputeMSBuildVerbosity)")
    if ($MSBuildLogger) {
        $script:msb_common += "/logger:$($MSBuildLogger)"
    }

    # We push to a stack other than the default one so that we don't
    # accidentally change its contents.
    #
    # We have to set our location to the same directory as this script as
    # dotnet globbing doesn't work with paths like
    # c:\src\bond\cs\dnc\**\project.json
    $script:script_dir = Split-Path -LiteralPath $PSCommandPath
    Push-Location -Path (Split-Path -LiteralPath $PSCommandPath) -StackName bond_dnc_build

    # The dotnet configurations have a -delay if we use delay signing. We
    # also need to copy the key to the fixed location.
    $script:dotnet_cfg = $Configuration
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

    msbuild $script:msb_common /p:Configuration=$Configuration /p:Platform=Win32 '..\Compiler.vcxproj'
    if (-not $?) {
        throw "Building GBC failed."
    }

    msbuild $script:msb_common /p:Configuration=$Configuration 'dirs.proj'
    if (-not $?) {
        throw "Code generation failed."
    }

    dotnet restore --verbosity (ComputeDotNetRestoreVerbosity)
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

    $script:to_publish = (
        @{'source'='attributes'; 'files'='Bond.Attributes.*'},
        @{'source'='core'; 'files'='Bond.*'},
        @{'source'='io'; 'files'='Bond.IO.*'},
        @{'source'='json'; 'files'='Bond.JSON.*'},
        @{'source'='reflection'; 'files'='Bond.Reflection.*'}
    )

    $script:publish_dir = "bin\$script:dotnet_cfg\"
    mkdir -Force $script:publish_dir
    if (-not $?) {
        throw "Creating of publish directory '$script:publish_dir' failed"
    }

    $script:to_publish | foreach {
        $source_xcopy_path = "src\$($psitem.source)\bin\$script:dotnet_cfg\*$($psitem.files)"
        xcopy /i /s /y $source_xcopy_path $script:publish_dir
        if (-not $?) {
            throw "Copying files '$source_xcopy_path' failed"
        }
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
