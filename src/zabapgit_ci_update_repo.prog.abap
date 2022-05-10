*&---------------------------------------------------------------------*
*& Report zabapgit_ci_update_repo
*&---------------------------------------------------------------------*
*& Update a repository to latest version
*& See https://github.com/abapGit/CI
*&---------------------------------------------------------------------*
REPORT zabapgit_ci_update_repo.

SELECTION-SCREEN BEGIN OF BLOCK repo WITH FRAME TITLE TEXT-b01.
  PARAMETERS:
    repo   TYPE string LOWER CASE OBLIGATORY,
    branch TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK repo.

SELECTION-SCREEN BEGIN OF BLOCK slack WITH FRAME TITLE TEXT-b02.
  PARAMETERS:
    slack TYPE abap_bool AS CHECKBOX,
    token TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK slack.

CLASS lcl_abapgit_update DEFINITION.

  PUBLIC SECTION.
    METHODS:
      run.

  PRIVATE SECTION.
    METHODS:
      send_to_slack
        IMPORTING
          ix_error TYPE REF TO zcx_abapgit_exception.

ENDCLASS.

CLASS lcl_abapgit_update IMPLEMENTATION.

  METHOD run.

    TRY.
        IF branch IS INITIAL.
          zcl_abapgit_ci_repos=>update_repository( repo ).
        ELSE.
          zcl_abapgit_ci_repos=>update_repository( iv_repo_name = repo iv_branch = branch ).
        ENDIF.

        MESSAGE |Repository { repo } updated successfully| TYPE 'S'.

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
                |abapGit CI error: { repo } update failed with |
             && |"{ ix_error->get_text( ) }"| ).

      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_abapgit_update( )->run( ).
