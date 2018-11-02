*&---------------------------------------------------------------------*
*& Report zabapgit_ci
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapgit_ci.

START-OF-SELECTION.
  TRY.
      NEW zcl_abapgit_ci_controller(
        ii_repo_provider = NEW zcl_abapgit_ci_test_repos( )
        ii_view          = NEW zcl_abapgit_ci_alv_view( )
      )->run( ).

    CATCH zcx_abapgit_exception INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
