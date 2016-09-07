# Test Certificates

These are test certificates that are used for Bond TLS tests. There is a root
certificate and two host certificates. The two host certificates are signed
by the root.

These certificates should not be trusted for *anything*. Their private keys
are public to all.

The password for all of these .pfx files is "bond".

## Creating a new certificate

To create a new certificate like the client and server ones here, run the
following sequence of commands in a PowerShell window on a Windows 10 (or
later) machine.

### Import the Bond test root certificate

We need to import the test root certificate so that we can sign the new
certificate. From the directory with the `bond-test-root.pfx file` (which
should be the same directory that this README is in), run
`Import-PfxCertificate`:

    Import-PfxCertificate `
        -CertStoreLocation Cert:\CurrentUser\My\ `
        -Exportable `
        -FilePath .\bond-test-root.pfx `
        -Password (ConvertTo-SecureString -AsPlainText -Force "bond")

You should get output like the following:

    PS C:\src\bond\cs\test\comm\Epoxy\certs> Import-PfxCertificate `
        -CertStoreLocation Cert:\CurrentUser\My\ `
        -Exportable `
        -FilePath .\bond-test-root.pfx `
        -Password (ConvertTo-SecureString -AsPlainText -Force "bond")

    PSParentPath: Microsoft.PowerShell.Security\Certificate::CurrentUser\My

    Thumbprint                                Subject
    ----------                                -------
    29D6B3199BE91CB38D94FD1F2883A9FD2126C91D  CN=bond-test-root

### Create a new certificate

Create the new certificate using `New-SelfSignedCertificate`, adjusting the
**DNSName**, **FriendlyName**, and **KeyDescription** as needed. Notice that
the `Signer` paramater points to the root certificate that we just imported.
Tab completion will help here, if you need to use a different certificate to
sign.

    New-SelfSignedCertificate `
        -KeyUsage KeyEncipherment,DigitalSignature `
        -NotAfter (Get-Date "2036-06-16") `
        -Signer Cert:\CurrentUser\My\29D6B3199BE91CB38D94FD1F2883A9FD2126C91D `
        -CertStoreLocation Cert:\CurrentUser\My\ `
        -DNSName bond-test-client1 `
        -FriendlyName bond-test-client1 `
        -KeyDescription "Bond TLS Test Client 1"

You should get output like the following:

    PS C:\src\bond\cs\test\comm\Epoxy\certs> New-SelfSignedCertificate `
        -KeyUsage KeyEncipherment,DigitalSignature `
        -NotAfter (Get-Date "2036-06-16") `
        -Signer Cert:\CurrentUser\My\29D6B3199BE91CB38D94FD1F2883A9FD2126C91D `
        -CertStoreLocation Cert:\CurrentUser\My\ `
        -DNSName bond-test-client1 `
        -FriendlyName bond-test-client1 `
        -KeyDescription "Bond TLS Test Client 1"

    PSParentPath: Microsoft.PowerShell.Security\Certificate::CurrentUser\My

    Thumbprint                                Subject
    ----------                                -------
    FA4CEFE05A9CE55226C82E388B1EA873F7A75A5B  CN=bond-test-client1

### Export the new certificate to a PFX file

Now, we need to export the new certificate from the user's store to a
portable PFX file using `Export-PfxCertificate`. We have to set a password
for the private key. For all of these certificates, we use "bond" as the
password. Notice that the `Cert` parameter points to the new certificate that
we just created. (It does _not_ point to the test root certificate.)

    Export-PfxCertificate `
        -Password (ConvertTo-SecureString -AsPlainText -Force "bond") `
        -FilePath .\bond-test-client1.pfx `
        -Cert Cert:\CurrentUser\My\FA4CEFE05A9CE55226C82E388B1EA873F7A75A5B

You should get output like the following:

    PS C:\src\bond\cs\test\comm\Epoxy\certs> Export-PfxCertificate `
        -Password (ConvertTo-SecureString -AsPlainText -Force "bond") `
        -FilePath .\bond-test-client1.pfx `
        -Cert Cert:\CurrentUser\My\FA4CEFE05A9CE55226C82E388B1EA873F7A75A5B

        Directory: C:\src\bond\cs\test\comm\Epoxy\certs

    Mode                LastWriteTime         Length Name
    ----                -------------         ------ ----
    -a----         9/7/2016   3:42 PM           3718 bond-test-client1.pfx

### Clean up the user certificate store

After adding the .pfx file to source control and using it in tests or
examples, you'll want to clean up the current user's certificate store.
Delete all of the certificates that you created or imported:

    # Delete the newly created certificate
    del Cert:\CurrentUser\My\FA4CEFE05A9CE55226C82E388B1EA873F7A75A5B -Confirm
    # Delete the imported root certificate.
    del Cert:\CurrentUser\My\29D6B3199BE91CB38D94FD1F2883A9FD2126C91D -Confirm
