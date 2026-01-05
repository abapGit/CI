# abapGit CI backend

Latest build: https://ci.abapgit.org

This repo provides basic continous intergration capabilities for abapGit.

For details, see the [abapGit Development Documentation](https://docs.abapgit.org/development/ci.html).

![Example](/img/ZABAPGIT_CI_result.png)

## Required Certificates

abapGit CI requires certificates for `github.com` and `raw.githubusercontent.com` to be installed in transaction STRUST (see [abapGit Documentation](https://docs.abapgit.org/user-guide/setup/ssl-setup.html#sap-trust-manager)).

Note: The certificate for `raw.githubusercontent.com` contains subject `*.github.io`.
