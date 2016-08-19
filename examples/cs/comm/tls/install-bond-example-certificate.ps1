<#
    .SYNOPSIS
        Creates a test, self-signed, trusted certificate for localhost.

    .DESCRIPTION
        Creates a test, self-signed certificate for localhost with the
        friendly name 'bond-example-cert' and installs it into the current
        user's trusted root store.
    #>

[CmdletBinding(SupportsShouldProcess=$True)]
param ()

if ($PSCmdlet.ShouldProcess("New self-signed certificate", "Create and install")) {

    # Create a new self-signed certificate
    $script:ssc = New-SelfSignedCertificate `
        -CertStoreLocation 'Cert:\CurrentUser\My' `
        -DnsName 'localhost' `
        -FriendlyName 'bond-example-cert' `
        -WhatIf:$false -Confirm:$false

    $script:userRootStore = $null
    try {
        $certId = "$($script:ssc.Subject) ($($script:ssc.Thumbprint))"

        Write-Host "About to install $certId as a trusted certificate for the current user. Please click 'Yes' when prompted."

        $script:userRootStore = New-Object `
            -TypeName System.Security.Cryptography.X509Certificates.X509Store `
            -ArgumentList ([System.Security.Cryptography.X509Certificates.StoreName]::Root, [System.Security.Cryptography.X509Certificates.StoreLocation]::CurrentUser)

        $script:userRootStore.Open([System.Security.Cryptography.X509Certificates.OpenFlags]::ReadWrite)
        $script:userRootStore.Add($script:ssc)

        Write-Host "Installed $certId as trusted for the current user"
    } finally {
        if ($script:userRootStore) {
            $script:userRootStore.Close()
            $script:userRootStore = $null
        }
    }
}
