CLASS zcl_abapgit_ci_repo_check DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get
      IMPORTING
        !io_repo         TYPE REF TO zcl_abapgit_repo_online
        !iv_check_exists TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_deserialize_checks
      RAISING
        zcx_abapgit_cancel
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS check_exists
      IMPORTING
        iv_devclass TYPE devclass
        io_dot      TYPE REF TO zcl_abapgit_dot_abapgit
        it_files    TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_ci_repo_check IMPLEMENTATION.


  METHOD check_exists.

    DATA:
      ls_item    TYPE zif_abapgit_definitions=>ty_item,
      lv_is_xml  TYPE abap_bool,
      lv_is_json TYPE abap_bool.

    LOOP AT it_files ASSIGNING FIELD-SYMBOL(<ls_file>) WHERE sha1 IS NOT INITIAL.

      zcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_file>-filename
          iv_path     = <ls_file>-path
          io_dot      = io_dot
          iv_devclass = iv_devclass
        IMPORTING
          es_item     = ls_item
          ev_is_xml   = lv_is_xml
          ev_is_json  = lv_is_json ).

      CHECK lv_is_xml = abap_true OR lv_is_json = abap_true. " only object definitions

      CHECK ls_item-obj_type <> 'DEVC' OR ls_item-obj_name <> iv_devclass. " skip the root package

      IF zcl_abapgit_objects=>exists( ls_item ) = abap_true.
        zcx_abapgit_exception=>raise( |Object { ls_item-obj_type } { ls_item-obj_name } already exists| ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


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

    " Check if any item already exits
    IF iv_check_exists = abap_true.
      check_exists(
        iv_devclass = io_repo->get_package( )
        io_dot      = lo_dot
        it_files    = lt_files ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
