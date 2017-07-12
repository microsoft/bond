<#
.SYNOPSIS

Builds Bond with the .NET Core toolchain

.PARAMETER Configuration

The configuration to build. Valid values are:
- debug
- release

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

PS c:\src\bond> .\cs\dnc\build.ps1 -Configuration release -Test

This will build the release configuration and run all the test projects.

#>
[CmdletBinding()]
param
(
    [ValidateSet("debug", "release")]
    [string]
    $Configuration = "debug",

    [switch]
    $Test = $false,

    [string]
    $Version = "",

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

    msbuild $script:msb_common /p:Configuration=$Configuration /p:Platform=Win32 '..\Compiler.vcxproj'
    if (-not $?) {
        throw "Building GBC failed."
    }

    mkdir -Force gen

    if ([string]::IsNullOrWhiteSpace($Version)) {
        Copy-Item '..\build\internal\DevVersions.cs' '.\gen\AssemblyInfo_Generated.cs'
        if (-not $?) {
            throw "Version copy failed"
        }
    } else {
        $genFullPath = (Resolve-Path gen\).Path
        msbuild $script:msb_common /p:BondVersionNum=$Version "/p:IntermediateOutputPath=$genFullPath\" '..\build\internal\Versions.targets'
        if (-not $?) {
            throw "Version generation failed"
        }
    }

    msbuild $script:msb_common /p:Configuration=$Configuration 'dirs.proj'
    if (-not $?) {
        throw "Bond code generation failed."
    }

    dotnet restore --verbosity (ComputeDotNetRestoreVerbosity)
    if (-not $?) {
        throw "Package restore failed."
    }

    # Due to a bug in the way 'dotnet build' resolves project references, we
    # need to list out the projects in the order that they need to be build.
    # Otherwise, dotnet tried to build projects before their dependencies
    # are built.
    #
    # See also: https://github.com/dotnet/cli/issues/2639
    dotnet build --configuration $Configuration `
      '.\src\attributes\project.json' `
      '.\src\reflection\project.json' `
      '.\src\core\project.json' `
      '.\src\io\project.json' `
      '.\src\json\project.json' `
      '.\src\grpc\project.json' `
      '.\test\core.tests\project.json' `
      '.\test\internal.tests\project.json' `
      '.\test\expressions.tests\project.json' `
      '.\test\grpc.tests\project.json' `
      '.\test\compat\compat.core.tests\project.json' `
      '.\test\codegen\no-warnings.tests\project.json' `
      '**\project.json'

    if (-not $?) {
        throw ".NET Core build failed."
    }

    $script:to_publish = (
        @{'source'='attributes'; 'files'='Bond.Attributes.*'},
        @{'source'='core'; 'files'='Bond.*'},
        @{'source'='grpc'; 'files'='Bond.Grpc.*'},
        @{'source'='io'; 'files'='Bond.IO.*'},
        @{'source'='json'; 'files'='Bond.JSON.*'},
        @{'source'='reflection'; 'files'='Bond.Reflection.*'}
    )

    $script:publish_dir = "bin\$Configuration\"
    mkdir -Force $script:publish_dir
    if (-not $?) {
        throw "Creating of publish directory '$script:publish_dir' failed"
    }

    $script:to_publish | foreach {
        $source_xcopy_path = "src\$($psitem.source)\bin\$Configuration\*$($psitem.files)"
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
            dotnet test --no-build --configuration $Configuration $psitem
            if (-not $?) {
                throw "Tests failed for $psitem"
            }
        }
    }
} finally {
    Pop-Location -StackName bond_dnc_build
}
