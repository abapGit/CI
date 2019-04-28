CLASS zcl_abapgit_ci_repo DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor,

      run
        CHANGING
          cs_ri_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      create_package
        CHANGING
          cs_ri_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      clone
        CHANGING
          cs_ri_repo TYPE zabapgit_ci_result
          co_repo    TYPE REF TO zcl_abapgit_repo_online
        RAISING
          zcx_abapgit_exception,

      pull
        IMPORTING
          io_repo    TYPE REF TO zcl_abapgit_repo_online
        CHANGING
          cs_ri_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      purge
        IMPORTING
          io_repo    TYPE REF TO zcl_abapgit_repo_online
        CHANGING
          cs_ri_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      syntax_check
        CHANGING
          cs_ri_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      check_objects
        IMPORTING
          io_repo    TYPE REF TO zcl_abapgit_repo_online
        CHANGING
          cs_ri_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      check_leftovers
        CHANGING
          cs_ri_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_ci_repo IMPLEMENTATION.


  METHOD check_leftovers.

    cs_ri_repo-check_leftovers = zif_abapgit_ci_definitions=>co_status-not_ok.

    DATA(lt_tadir) = zcl_abapgit_factory=>get_tadir(
                                       )->read( cs_ri_repo-package ).

    LOOP AT lt_tadir ASSIGNING FIELD-SYMBOL(<ls_tadir>)
                     WHERE object <> 'DEVC'.
      zcx_abapgit_exception=>raise( |Left over object { <ls_tadir>-object } { <ls_tadir>-obj_name }| ).
    ENDLOOP.

    LOOP AT lt_tadir ASSIGNING <ls_tadir>
                     WHERE object = 'DEVC'.
      zcx_abapgit_exception=>raise( |Left over package { <ls_tadir>-obj_name }| ).
    ENDLOOP.

    cs_ri_repo-check_leftovers = zif_abapgit_ci_definitions=>co_status-ok.

  ENDMETHOD.


  METHOD check_objects.

    DATA: lt_items TYPE zif_abapgit_definitions=>ty_items_ts,
          ls_files TYPE zif_abapgit_definitions=>ty_stage_files.

    FIELD-SYMBOLS: <ls_local_file>  TYPE zif_abapgit_definitions=>ty_file_item,
                   <ls_remote_file> LIKE LINE OF ls_files-remote.

    cs_ri_repo-object_check = zif_abapgit_ci_definitions=>co_status-not_ok.

    DATA(lt_tadir) = zcl_abapgit_factory=>get_tadir( )->read( io_repo->get_package( ) ).

    LOOP AT lt_tadir ASSIGNING FIELD-SYMBOL(<ls_tadir>).

      DATA(ls_item) = CORRESPONDING zif_abapgit_definitions=>ty_item( <ls_tadir> MAPPING obj_name = obj_name
                                                                                         obj_type = object ).

      IF zcl_abapgit_objects=>is_active( ls_item ) = abap_false.
        zcx_abapgit_exception=>raise( |Object { ls_item-obj_type } { ls_item-obj_name } isn't active| ).
      ENDIF.

    ENDLOOP.

    ls_files = zcl_abapgit_factory=>get_stage_logic( )->get( io_repo ).

    LOOP AT ls_files-local ASSIGNING <ls_local_file>.
      zcx_abapgit_exception=>raise( |Local file diffs to remote: { <ls_local_file>-file-filename }| ).
    ENDLOOP.

    LOOP AT ls_files-remote ASSIGNING <ls_remote_file>.
      zcx_abapgit_exception=>raise( |Remote file diffs to local: { <ls_remote_file>-filename }| ).
    ENDLOOP.

    cs_ri_repo-object_check = zif_abapgit_ci_definitions=>co_status-ok.

  ENDMETHOD.


  METHOD clone.

    cs_ri_repo-clone = zif_abapgit_ci_definitions=>co_status-not_ok.

    TRY.
        co_repo = zcl_abapgit_repo_srv=>get_instance( )->new_online(
          iv_url         = |{ cs_ri_repo-clone_url }|
          iv_branch_name = 'refs/heads/master'
          iv_package     = cs_ri_repo-package ).

        COMMIT WORK AND WAIT.

      CATCH zcx_abapgit_cancel INTO DATA(error).
        zcx_abapgit_exception=>raise( error->get_text( ) ).
    ENDTRY.

    cs_ri_repo-clone = zif_abapgit_ci_definitions=>co_status-ok.

  ENDMETHOD.


  METHOD constructor.

    zcl_abapgit_ui_injector=>set_gui_functions( NEW lcl_mock_ui_functions( ) ).

  ENDMETHOD.


  METHOD create_package.

    DATA(li_package) = zcl_abapgit_factory=>get_sap_package( cs_ri_repo-package ).

    IF li_package->exists( ) = abap_false.

      cs_ri_repo-create_package = zif_abapgit_ci_definitions=>co_status-not_ok.

      li_package->create( VALUE #(
                            as4user  = sy-uname
                            devclass = cs_ri_repo-package
                            ctext    = |abapGit CI run|
                          ) ).

      cs_ri_repo-create_package = zif_abapgit_ci_definitions=>co_status-ok.

    ENDIF.

  ENDMETHOD.


  METHOD pull.

    cs_ri_repo-pull = zif_abapgit_ci_definitions=>co_status-not_ok.

    DATA(ls_checks) = io_repo->deserialize_checks( ).

    LOOP AT ls_checks-overwrite ASSIGNING FIELD-SYMBOL(<ls_checks>).
      <ls_checks>-decision = abap_true.
    ENDLOOP.

    LOOP AT ls_checks-warning_package ASSIGNING FIELD-SYMBOL(<ls_warning_package>).
      <ls_warning_package>-decision = abap_true.
    ENDLOOP.

    io_repo->deserialize( ls_checks ).

    io_repo->refresh( iv_drop_cache = abap_true ).

    cs_ri_repo-pull = zif_abapgit_ci_definitions=>co_status-ok.

  ENDMETHOD.


  METHOD purge.

    IF io_repo IS NOT BOUND.
      RETURN.
    ENDIF.

    cs_ri_repo-purge = zif_abapgit_ci_definitions=>co_status-not_ok.

    TRY.
        DATA(ls_checks) = io_repo->delete_checks( ).

        zcl_abapgit_repo_srv=>get_instance( )->purge( io_repo   = io_repo
                                                      is_checks = ls_checks ).

        COMMIT WORK AND WAIT.
      CATCH zcx_abapgit_cancel INTO DATA(error).
        zcx_abapgit_exception=>raise( error->get_text( ) ).
    ENDTRY.

    cs_ri_repo-purge = zif_abapgit_ci_definitions=>co_status-ok.

    CALL FUNCTION 'DEQUEUE_ALL'
      EXPORTING
        _synchron = abap_true.

    check_leftovers( CHANGING cs_ri_repo = cs_ri_repo ).

  ENDMETHOD.


  METHOD run.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_online.

    create_package( CHANGING cs_ri_repo = cs_ri_repo ).

    TRY.
        clone( CHANGING cs_ri_repo = cs_ri_repo
                        co_repo    = lo_repo ).

        pull( EXPORTING io_repo    = lo_repo
              CHANGING  cs_ri_repo = cs_ri_repo ).

        syntax_check( CHANGING cs_ri_repo = cs_ri_repo ).

        check_objects( EXPORTING io_repo    = lo_repo
                       CHANGING  cs_ri_repo = cs_ri_repo ).

      CATCH zcx_abapgit_exception INTO DATA(lx_error).

        " ensure uninstall
        purge( EXPORTING io_repo    = lo_repo
               CHANGING  cs_ri_repo = cs_ri_repo ).

        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).

    ENDTRY.

    purge( EXPORTING io_repo    = lo_repo
           CHANGING  cs_ri_repo = cs_ri_repo ).

  ENDMETHOD.


  METHOD syntax_check.

    cs_ri_repo-syntax_check = zif_abapgit_ci_definitions=>co_status-not_ok.

    DATA(li_syntax_check) = zcl_abapgit_factory=>get_code_inspector( cs_ri_repo-package ).

    DATA(lt_list) = li_syntax_check->run( 'SYNTAX_CHECK' ).

    READ TABLE lt_list TRANSPORTING NO FIELDS
                       WITH KEY kind = 'E'.
    IF sy-subrc = 0.
      cs_ri_repo-syntax_check = zif_abapgit_ci_definitions=>co_status-not_ok.
    ELSE.
      cs_ri_repo-syntax_check = zif_abapgit_ci_definitions=>co_status-ok.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
