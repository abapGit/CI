*&---------------------------------------------------------------------*
*& Report zabapgit_ci_update_abapgit_ci
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapgit_ci_update_abapgit_ci.

TRY.
    zcl_abapgit_ci=>update_abapgit_ci_repo( ).

    MESSAGE |abapGit CI updated successfully| TYPE 'S'.

  CATCH zcx_abapgit_exception INTO DATA(lx_error).
    MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
ENDTRY.
