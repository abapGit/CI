*&---------------------------------------------------------------------*
*& Report zabapgit_ci_update_abapgit
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapgit_ci_update_abapgit.

TRY.
    zcl_abapgit_ci=>update_abapgit_repo( ).

    MESSAGE |abapGit updated successfully| TYPE 'S'.

  CATCH zcx_abapgit_exception INTO DATA(lx_error).
    MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
ENDTRY.
