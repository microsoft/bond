@echo off
setlocal

set GENERATED=%~dp0\tests\generated
set COMPILER=%~dp0\dist\build\gbc\gbc
set SCHEMAS=%~dp0\tests\schema

if exist %GENERATED% rmdir /q/s %GENERATED%

call :CodeGen "c++"
call :CodeGen "c++" "--allocator=arena" allocator
call :CodeGen "c#"
call :CodeGen "c#" --collection-interfaces collection-interfaces

%COMPILER% c++ --no-banner -o %GENERATED% ^
    --allocator=arena ^
    --using="List=my::list<{0}, arena>" ^
    --using="Vector=my::vector<{0}, arena>" ^
    --using="Set=my::set<{0}, arena>" ^
    --using="Map=my::map<{0}, {1}, arena>" ^
    --using="String=my::string<arena>" ^
    %SCHEMAS%\custom_alias_with_allocator.bond

%COMPILER% c++ --no-banner -o %GENERATED% ^
    --allocator=arena ^
    --using="List=my::list<{0}>" ^
    --using="Vector=my::vector<{0}>" ^
    --using="Set=my::set<{0}>" ^
    --using="Map=my::map<{0}, {1}>" ^
    --using=String=my::string ^
    %SCHEMAS%\custom_alias_without_allocator.bond

%COMPILER% c++ --no-banner -o %GENERATED% ^
    --allocator=arena ^
    %SCHEMAS%\alias_with_allocator.bond

%COMPILER% c++ --no-banner -o %GENERATED%\apply ^
    --apply-attribute=DllExport ^
    %SCHEMAS%\basic_types.bond

goto :eof

:CodeGen
echo %1 codegen %2
REM Common schema files for C++ and C#
%COMPILER% %1 %2 --no-banner -o %GENERATED%\%3 ^
    %SCHEMAS%\aliases.bond ^
    %SCHEMAS%\attributes.bond ^
    %SCHEMAS%\basic_types.bond ^
    %SCHEMAS%\complex_types.bond ^
    %SCHEMAS%\defaults.bond ^
    %SCHEMAS%\empty.bond ^
    %SCHEMAS%\field_modifiers.bond ^
    %SCHEMAS%\generics.bond ^
    %SCHEMAS%\inheritance.bond

REM C++ codegen tests
if %1 == "c++" (
    %COMPILER% %1 %2 --no-banner -o %GENERATED%\%3 ^
        %SCHEMAS%\alias_key.bond)

REM C# codegen tests
if %1 == "c#" (
    %COMPILER% %1 %2 --no-banner -o %GENERATED%\%3 ^
        %SCHEMAS%\nullable_alias.bond)

goto :eof

