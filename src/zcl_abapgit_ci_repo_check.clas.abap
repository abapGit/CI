CLASS zcl_abapgit_ci_repo_check DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get
      IMPORTING
        !io_repo         TYPE REF TO zcl_abapgit_repo_online
      RETURNING
        VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_deserialize_checks
      RAISING
        zcx_abapgit_cancel
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_ci_repo_check IMPLEMENTATION.


  METHOD get.

    rs_checks = io_repo->deserialize_checks( ).

    " Auto-confirm all decisions
    LOOP AT rs_checks-overwrite ASSIGNING FIELD-SYMBOL(<ls_overwrite>).
      <ls_overwrite>-decision = zif_abapgit_definitions=>gc_yes.
    ENDLOOP.

    LOOP AT rs_checks-warning_package ASSIGNING FIELD-SYMBOL(<ls_warning_package>).
      <ls_warning_package>-decision = zif_abapgit_definitions=>gc_yes.
    ENDLOOP.

    " If requirements are not met, cancel process
    IF rs_checks-requirements-met <> zif_abapgit_definitions=>gc_yes.
      zcx_abapgit_cancel=>raise( 'Requirements not met' ).
    ENDIF.

    " If dependencies are not met, cancel process
    IF rs_checks-dependencies-met <> zif_abapgit_definitions=>gc_yes.
      zcx_abapgit_cancel=>raise( 'Dependencies not met' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
