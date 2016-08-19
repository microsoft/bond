<#
    .SYNOPSIS
        Removes all Bond example certificates installed.

    .DESCRIPTION
        Removes all Bond example certificates installed from the user's My
        and Root stores. Bond example certificates are identified by
        matching the friendly name with 'bond-example-cert' exactly.
    #>

[CmdletBinding(SupportsShouldProcess=$True)]
param ()

function script:RemoveFromStore($store) {
    Get-ChildItem $store | Where-Object -Property FriendlyName -CEQ 'bond-example-cert' | Remove-Item
}

Write-Host "Please click 'Yes' when prompted to DELETE each certificate."
script:RemoveFromStore('Cert:\CurrentUser\My')
script:RemoveFromStore('Cert:\CurrentUser\Root')
