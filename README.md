# abapGit CI

This repo provides basic continous intergration capabilities for abapGit.
It fetches abapGit test repos from https://github.com/abapGit-tests. Then
for each repo the following steps are executed:
- create package (if needed)
- clone
- pull
- syntax check
- purge (uninstall)

If there is any error in these steps abapGit CI will indicate that.

Run report ZABAPGIT_CI_UPDATE_ABAPGIT to update the abapGit repo.

There are currently two possibilities to run abapGit CI:

**1) Report ZABAPGIT_CI**

Selection screen:

![abapGit CI selection screen](/img/ZABAPGIT_CI_selection_screen.png)

Result list:

![abapGit CI results](/img/ZABAPGIT_CI_result.png)

**2) REST API**

POST /sap/zabapgit_ci/run

[response.json](/test/response.json)


**Prerequisites:**
- abapGit repo
- NW >= 7.50
