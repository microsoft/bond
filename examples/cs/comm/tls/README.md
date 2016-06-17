# Bond TLS Example

This example shows how to configure an Epoxy-based TLS server and connect a
client to it.

To run the example:

    .\tls server-certificate-friendly-name

# Certificates

The program expects to be given the _friendly name_ of a valid, trusted
certificate for `localhost`. It will search the user and machine certificate
stores (in that order) for such a certificate and use the first matching
certificate when starting the server.

## Creating a certificate

If you are running on Windows, the included PowerShell script
`install-bond-example-certificate.ps1` can be used to create a self-signed
certificate for `localhost` with the friendly name `bond-example-cert`. It
will also install this newly minted certificate into the current user's
trusted root.

Then, you can run

    .\tls bond-example-cert

You can clean up any test certificates that were installed by running
`remove-bond-example-certificates.ps1`. All certificates with the friendly
name `bond-example-cert` will be remoted from the current user's My and Root
stores.
