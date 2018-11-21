# abapGit CI backend

Latest nightly build: https://ci.abapGit.org

This repo provides basic continous intergration capabilities for abapGit.
It fetches abapGit test repos from https://github.com/abapGit-tests. Then
for each repo the following steps are executed:
- create package (if needed)
- clone
- pull
- syntax check
- purge (uninstall)

Additionally the latest abapGit build is checked.

If there is any error in these steps abapGit CI will indicate that.

Run report ZABAPGIT_CI_UPDATE_ABAPGIT to update the abapGit repo.

Run report ZABAPGIT_CI_UPDATE_ABAPGIT_CI to update the abapGit CI repo.

There are currently three possibilities to run abapGit CI:

**1) Adhoc CI with report ZABAPGIT_CI**

Selection screen:

![abapGit CI selection screen](/img/ZABAPGIT_CI_selection_screen.png)

Result list:

![abapGit CI results](/img/ZABAPGIT_CI_result.png)

**2) REST API**

POST /sap/zabapgit_ci/run

[response.json](/test/response.json)

**3) Schedule job sequence with report ZABAPGIT_CI_JOB_SCHEDULER**

A job sequence is created with

1) update abapGit repo

2) update abapGit CI repo

3) run abapGit CI

![abapGit CI job scheduler](/img/ZABAPGIT_CI_JOB_SCHEDULER.png)

**Prerequisites:**
- abapGit repo
- NW >= 7.50
