*&---------------------------------------------------------------------*
*& Report zabapgit_ci_update_abapgit_ci
*&---------------------------------------------------------------------*
*& Update abapGit CI to latest version from GitHub
*& See https://github.com/abapGit/CI
*&---------------------------------------------------------------------*
REPORT zabapgit_ci_update_abapgit_ci.

PARAMETERS:
  slack TYPE abap_bool AS CHECKBOX,
  token TYPE string LOWER CASE.

CLASS lcl_abapgit_ci_update DEFINITION.

  PUBLIC SECTION.
    METHODS:
      run.

  PRIVATE SECTION.
    METHODS:
      send_to_slack
        IMPORTING
          ix_error TYPE REF TO zcx_abapgit_exception.

ENDCLASS.

CLASS lcl_abapgit_ci_update IMPLEMENTATION.

  METHOD run.

    TRY.
        zcl_abapgit_ci_repos=>update_abapgit_ci_repo( ).
        MESSAGE |abapGit CI updated successfully| TYPE 'S'.

      CATCH zcx_abapgit_exception INTO DATA(lx_error).

        IF slack = abap_true.
          send_to_slack( lx_error ).
        ENDIF.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

  ENDMETHOD.


  METHOD send_to_slack.

    TRY.
        NEW zcl_abapgit_ci_slack( token )->post(
                |abapGit CI error: abapGit CI update failed with |
             && |"{ ix_error->get_text( ) }"| ).

      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_abapgit_ci_update( )->run( ).
