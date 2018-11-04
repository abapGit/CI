*&---------------------------------------------------------------------*
*& Report zabapgit_ci
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapgit_ci.

PARAMETERS:
  p_push TYPE abap_bool AS CHECKBOX,
  p_url  TYPE string LOWER CASE.

START-OF-SELECTION.
  TRY.
      NEW zcl_abapgit_ci_controller(
        ii_repo_provider = NEW zcl_abapgit_ci_test_repos( )
        ii_view          = NEW zcl_abapgit_ci_alv_view( )
        is_options       = VALUE #(
          push_results_to_git      = p_push
          destination_git_repo_url = p_url
        )
      )->run( ).

      MESSAGE |abapGit CI run completed| TYPE 'S'.

    CATCH zcx_abapgit_exception INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
