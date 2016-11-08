msbuild /m /p:Configuration=Debug /p:Platform=Win32 '..\Compiler.vcxproj'
if (-not $?) {
    throw "Building GBC failed."
}

msbuild /m 'dirs.proj'
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
dotnet build '.\src\attributes\project.json' '.\src\reflection\project.json' '**\project.json'
if (-not $?) {
    throw ".NET Core build failed."
}
