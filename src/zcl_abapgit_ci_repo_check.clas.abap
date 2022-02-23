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



CLASS ZCL_ABAPGIT_CI_REPO_CHECK IMPLEMENTATION.


  METHOD get.

    rs_checks = io_repo->deserialize_checks( ).

    " Auto-confirm all decisions
    LOOP AT rs_checks-overwrite ASSIGNING FIELD-SYMBOL(<ls_overwrite>).
      <ls_overwrite>-decision = zif_abapgit_definitions=>c_yes.
    ENDLOOP.

    LOOP AT rs_checks-warning_package ASSIGNING FIELD-SYMBOL(<ls_warning_package>).
      <ls_warning_package>-decision = zif_abapgit_definitions=>c_yes.
    ENDLOOP.

    " If requirements are not met, cancel process
    IF rs_checks-requirements-met <> zif_abapgit_definitions=>c_yes.
      zcx_abapgit_cancel=>raise( 'Requirements not met' ).
    ENDIF.

    " If dependencies are not met, cancel process
    IF rs_checks-dependencies-met <> zif_abapgit_definitions=>c_yes.
      zcx_abapgit_cancel=>raise( 'Dependencies not met' ).
    ENDIF.

    " Check if this is an abapGit repo
    DATA(lo_dot) = io_repo->get_dot_abapgit( ).

    IF lo_dot IS INITIAL.
      zcx_abapgit_cancel=>raise( '.abapGit.xml not found' ).
    ENDIF.

    " Check if there are any files to deserialize
    DATA(lt_files) = io_repo->get_files_remote( ).

    READ TABLE lt_files TRANSPORTING NO FIELDS WITH KEY file_path
      COMPONENTS path     = lo_dot->get_starting_folder( )
                 filename = 'package.devc.xml'.
    IF sy-subrc <> 0.
      zcx_abapgit_cancel=>raise( 'No files found to deserialize' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
