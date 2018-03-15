<#
.SYNOPSIS

Runs Bond C# examples for testing purposes. Writes an error record to
the pipeline for each example that fails.

.PARAMETER ExamplesRoot

Path to the directory containing the built examples. Defaults to ..\..\examples\cs

.PARAMETER IgnorePatterns

A collection of -Like patterns specifying which examples to ignore.

.EXAMPLE

PS c:\src\bond\cs\test> .\test-examples.ps1 -IgnorePatterns '*bar*'

This will run all the examples except those with "bar" in their paths.

#>
[CmdletBinding()]
param
(
    [string]
    $ExamplesRoot = '',

    [string[]]
    $IgnorePatterns = @()
)

Set-StrictMode -Version Latest

function ShouldIgnore([string[]]$ignorePats, [string]$fullPath) {
    Write-Debug "Checking $fullPath against ignore patterns"
    ForEach ($pat in $ignorePats) {
        if ($fullPath -Like $pat) {
            Write-Debug "Ignoring ""$fullPath"". Matched pattern ""$pat"""
            return $true
        }
    }

    return $false
}

if ([string]::IsNullOrEmpty($ExamplesRoot)) {
    $likelyExamplesDirectory = "$PSScriptRoot\..\..\examples\cs"
    if (Test-Path $likelyExamplesDirectory -PathType Container) {
        $ExamplesRoot = $likelyExamplesDirectory
    } else {
        throw "No ExamplesRoot was passed and the guessed ""$likelyExamplesDirectory"" didn't exist. Specify an ExamplesRoot."
    }
}

$exampleExes = @()
$exampleExes += dir -Recurse -Path $ExamplesRoot -Include '*.exe' -File |
    Where-Object { -not (ShouldIgnore $IgnorePatterns $PSItem.FullName) } |
    Where-Object -Property FullName -Like '*\bin\*'

Write-Debug "Found examples: $exampleExes"

try {
    $exampleExes | ForEach-Object {
        $exampleFullPath = $PSItem.FullName
        Write-Progress -Activity 'Running examples' -Status "Running ""$exampleFullPath"""
        Write-Debug "Running ""$exampleFullPath"""

        $sw = [System.Diagnostics.Stopwatch]::StartNew()
        & $exampleFullPath 2>&1 | Tee-Object -Variable output | Write-Debug
        if (-not $?) {
            Write-Error -Message "Example ""$exampleFullPath"" exited with $LASTEXITCODE. Output:`n$output" -TargetObject $PSItem
        }

        Write-Debug """$exampleFullPath"" took $($sw.Elapsed)"
    }
} finally {
    Write-Progress -Activity 'Running examples' -Completed
}
