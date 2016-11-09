<#
    .SYNOPSIS
        Builds Bond with the .NET Core toolchain

    .PARAMETER Configuration
        The configuration to build. Valid values are:
        - debug
        - release

    .PARAMETER DelaySignKey
        The optional delay sign key to use.

    .EXAMPLE
        PS c:\src\bond> .\build.ps1

        This will build the debug configuration.

    .EXAMPLE
        PS c:\src\bond> .\build.ps1 -Configuration release -DelaySignKey c:\src\private\delay-key.snk

        This will build the release configuration and delay sign it with the specified key.
#>
[CmdletBinding(SupportsShouldProcess=$True)]
param
(
    [ValidateSet("debug", "release")]
    [string]
    $Configuration = "debug",

    [string]
    $DelaySignKey = ""
)

$script:dotnet_cfg = $Configuration

if ($DelaySignKey) {
    mkdir -Force 'keys'
    if (-not $?) {
        throw "Could not create keys/ directory"
    }

    Copy-Item -Force $DelaySignKey 'keys/delay-sign.snk'
    if (-not $?) {
        throw "Copying delay sign key '$DelaySignKey' to 'keys/delay-sign.snk' failed"
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
