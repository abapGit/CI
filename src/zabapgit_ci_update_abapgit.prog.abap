*&---------------------------------------------------------------------*
*& Report zabapgit_ci_update_abapgit
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapgit_ci_update_abapgit.


PARAMETERS:
  slack TYPE abap_bool AS CHECKBOX,
  token TYPE string LOWER CASE.

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
        zcl_abapgit_ci=>update_abapgit_repo( ).
        MESSAGE |abapGit updated successfully| TYPE 'S'.

      CATCH zcx_abapgit_exception INTO DATA(lx_error).

        IF slack = abap_true.
          send_to_slack( lx_error ).
        ENDIF.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

  ENDMETHOD.


  METHOD send_to_slack.

    TRY.
        NEW zcl_abapgit_ci_slack( token )->post( |abapGit CI error: abapGit update failed with "{ ix_error->get_text( ) }"| ).

      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_abapgit_update( )->run( ).
