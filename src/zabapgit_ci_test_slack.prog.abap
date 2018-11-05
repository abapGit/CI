*&---------------------------------------------------------------------*
*& Report zabapgit_ci_test_slack
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapgit_ci_test_slack.

PARAMETERS:
  token TYPE string LOWER CASE OBLIGATORY,
  msg   TYPE string LOWER CASE OBLIGATORY.

TRY.
    NEW zcl_abapgit_ci_slack( token )->post( msg ).

  CATCH zcx_abapgit_exception INTO DATA(lx_error).
    MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
ENDTRY.
