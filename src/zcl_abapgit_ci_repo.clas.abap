CLASS zcl_abapgit_ci_repo DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS run
      CHANGING
        !cs_ci_repo TYPE zabapgit_ci_result
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_log_type,
        diff   TYPE string VALUE 'DIFF',
        enq    TYPE string VALUE 'ENQ',
        msg    TYPE string VALUE 'MSG',
        obj    TYPE string VALUE 'OBJ',
        syntax TYPE string VALUE 'SYNTAX',
        tadir  TYPE string VALUE 'TADIR',
      END OF c_log_type.

    DATA:
      mo_log   TYPE REF TO zcl_abapgit_ci_log,
      mt_items TYPE zif_abapgit_definitions=>ty_items_tt.

    METHODS:
      create_package
        IMPORTING
          iv_transport TYPE trkorr OPTIONAL
        CHANGING
          cs_ci_repo   TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      clone
        CHANGING
          cs_ci_repo TYPE zabapgit_ci_result
          co_repo    TYPE REF TO zcl_abapgit_repo_online
        RAISING
          zcx_abapgit_exception,

      check_repo
        IMPORTING
          io_repo TYPE REF TO zcl_abapgit_repo_online
        RAISING
          zcx_abapgit_exception,

      check_repo_exists
        IMPORTING
          io_repo          TYPE REF TO zcl_abapgit_repo_online
        RETURNING
          VALUE(rv_exists) TYPE abap_bool
        RAISING
          zcx_abapgit_exception,

      check_exists
        IMPORTING
          iv_package TYPE devclass
          io_dot     TYPE REF TO zcl_abapgit_dot_abapgit
          it_files   TYPE zif_abapgit_git_definitions=>ty_files_tt
        RAISING
          zcx_abapgit_exception,

      pull
        IMPORTING
          io_repo      TYPE REF TO zcl_abapgit_repo_online
          iv_transport TYPE trkorr OPTIONAL
        CHANGING
          cs_ci_repo   TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_cancel
          zcx_abapgit_exception,

      uninstall
        IMPORTING
          io_repo    TYPE REF TO zcl_abapgit_repo_online
          iv_cleanup TYPE abap_bool DEFAULT abap_false
        CHANGING
          cs_ci_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      purge
        IMPORTING
          io_repo      TYPE REF TO zcl_abapgit_repo_online
          iv_transport TYPE trkorr OPTIONAL
          iv_cleanup   TYPE abap_bool DEFAULT abap_false
        CHANGING
          cs_ci_repo   TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      syntax_check
        CHANGING
          cs_ci_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      check_objects
        IMPORTING
          io_repo    TYPE REF TO zcl_abapgit_repo_online
        CHANGING
          cs_ci_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      check_leftovers
        CHANGING
          cs_ci_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      cleanup_tadir
        IMPORTING
          is_tadir             TYPE zif_abapgit_definitions=>ty_tadir
        RETURNING
          VALUE(rv_cleaned_up) TYPE abap_bool
        RAISING
          zcx_abapgit_exception,

      create_transport
        IMPORTING
          iv_repo_name        TYPE zabapgit_ci_repo_name
          iv_package          TYPE devclass
          iv_deletion         TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(rv_transport) TYPE trkorr
        RAISING
          zcx_abapgit_exception,

      read_transport
        IMPORTING
          iv_transport      TYPE trkorr
        RETURNING
          VALUE(rt_objects) TYPE tr_objects
        RAISING
          zcx_abapgit_exception,

      check_transport
        IMPORTING
          io_repo      TYPE REF TO zcl_abapgit_repo_online
          iv_transport TYPE trkorr
          iv_deletion  TYPE abap_bool DEFAULT abap_false
        CHANGING
          cs_ci_repo   TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      release_transport
        IMPORTING
          iv_transport TYPE trkorr
        RAISING
          zcx_abapgit_exception,

      cleanup_transport
        IMPORTING
          iv_transport TYPE trkorr
        RAISING
          zcx_abapgit_exception,

      check_item
        IMPORTING
          is_item         TYPE zif_abapgit_definitions=>ty_item
          iv_deletion     TYPE abap_bool
        RETURNING
          VALUE(rv_check) TYPE abap_bool,

      adjust_item
        CHANGING
          cs_item TYPE zif_abapgit_definitions=>ty_item
        RAISING
          zcx_abapgit_exception,

      log_tadir
        IMPORTING
          is_ci_repo TYPE zabapgit_ci_result
          iv_phase   TYPE string
        RAISING
          zcx_abapgit_exception,

      log_enq
        IMPORTING
          is_ci_repo TYPE zabapgit_ci_result
          iv_phase   TYPE string
        RAISING
          zcx_abapgit_exception,

      log_diffs
        IMPORTING
          is_ci_repo TYPE zabapgit_ci_result
          is_files   TYPE zif_abapgit_definitions=>ty_stage_files
        RAISING
          zcx_abapgit_exception,

      log_messages
        IMPORTING
          is_ci_repo      TYPE zabapgit_ci_result
          ii_log          TYPE REF TO zif_abapgit_log
          iv_phase        TYPE string
        RETURNING
          VALUE(rv_error) TYPE string
        RAISING
          zcx_abapgit_exception,

      log_objects
        IMPORTING
          is_ci_repo TYPE zabapgit_ci_result
          it_objects TYPE tr_objects
        RAISING
          zcx_abapgit_exception,

      log_syntax_errors
        IMPORTING
          is_ci_repo TYPE zabapgit_ci_result
          it_list    TYPE scit_alvlist
        RAISING
          zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_ci_repo IMPLEMENTATION.


  METHOD adjust_item.

    IF cs_item-obj_type = 'SICF'.
      " Object name of ICF services are encoded using hash of URL and need to be decoded for comparison with TADIR
      cs_item-obj_name = zcl_abapgit_object_sicf=>read_tadir_sicf( cs_item-obj_name )-obj_name.
    ENDIF.

  ENDMETHOD.


  METHOD check_exists.

    DATA:
      ls_item    TYPE zif_abapgit_definitions=>ty_item,
      ls_tadir   TYPE tadir,
      lv_is_xml  TYPE abap_bool,
      lv_is_json TYPE abap_bool.

    CLEAR mt_items.

    LOOP AT it_files ASSIGNING FIELD-SYMBOL(<ls_file>) WHERE sha1 IS NOT INITIAL.

      zcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_file>-filename
          iv_path     = <ls_file>-path
          io_dot      = io_dot
          iv_devclass = iv_package
        IMPORTING
          es_item     = ls_item
          ev_is_xml   = lv_is_xml
          ev_is_json  = lv_is_json ).

      CHECK lv_is_xml = abap_true OR lv_is_json = abap_true. " only object definitions

      " skip the root package and namespaces
      CHECK ls_item-obj_type <> 'DEVC' OR ls_item-obj_name <> iv_package.
      CHECK ls_item-obj_type <> 'NSPC'.

      IF zcl_abapgit_objects=>exists( ls_item ) = abap_true.
        " Note: If the zcl_abapgit_object_<type>~exists fails with exception, the check returns true!
        " So check if the implementation of `exists` is correct
        zcx_abapgit_cancel=>raise( |Object { ls_item-obj_type } { ls_item-obj_name } already exists| ).
      ENDIF.

      SELECT SINGLE * FROM tadir INTO @ls_tadir
        WHERE pgmid = 'R3TR' AND object = @ls_item-obj_type AND obj_name = @ls_item-obj_name.
      IF sy-subrc = 0.
        zcx_abapgit_cancel=>raise( |TADIR entry { ls_item-obj_type } { ls_item-obj_name } already exists| ).
      ENDIF.

      " Remember objects for check_leftover process
      INSERT ls_item INTO TABLE mt_items.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_item.

    DATA lv_category TYPE seoclassdf-category.

    rv_check = abap_true.

    " In some cases, object directory (tadir) and transport entries (e07x) do not match.
    " If this is the case, we skip such objects
    IF is_item-obj_type = 'DEVC'.
      " Packages cannot be deleted in the same transport as its contents. abapGit does not delete transportable
      " packages on uninstall. Therefore these might still exist from the last run and might not be contained
      " in this transport.
      rv_check = abap_false.
    ELSEIF is_item-obj_type = 'SICF' AND iv_deletion = abap_true.
      " Object name of ICF services can not be decoded after deletion since this needs the TADIR entry
      rv_check = abap_false.
    ELSEIF is_item-obj_type = 'SOTR' AND iv_deletion = abap_true.
      " SOTR is not part of the repo but deletion creates an transport entry
      rv_check = abap_false.
    ELSEIF is_item-obj_type = 'TDAT' AND is_item-obj_name = 'EDISEGMENT'.
      " IDOC and IEXT create additional TDAT entries
      rv_check = abap_false.
    ELSEIF is_item-obj_type = 'INTF' AND ( is_item-obj_name CP '+IWCI*' OR is_item-obj_name CP '/*/IWCI*' ).
      " WDYN generates an interface
      rv_check = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD check_leftovers.

    DATA:
      lv_count TYPE i,
      ls_tadir TYPE tadir.

    cs_ci_repo-check_leftovers = zif_abapgit_ci_definitions=>co_status-not_ok.

    " Check for tadir entries
    DATA(lt_tadir) = zcl_abapgit_factory=>get_tadir( )->read( cs_ci_repo-package ).

    LOOP AT lt_tadir ASSIGNING FIELD-SYMBOL(<ls_tadir>) WHERE object <> 'DEVC' AND object <> 'NSPC'.
      IF cleanup_tadir( <ls_tadir> ) = abap_false.
        zcx_abapgit_exception=>raise( |Leftover TADIR entry { <ls_tadir>-object } { <ls_tadir>-obj_name }| ).
      ENDIF.
    ENDLOOP.

    IF cs_ci_repo-layer IS INITIAL. " Only check leftover of local packages
      LOOP AT lt_tadir ASSIGNING <ls_tadir> WHERE object = 'DEVC'.
        zcx_abapgit_exception=>raise( |Leftover TADIR entry { <ls_tadir>-object } { <ls_tadir>-obj_name }| ).
      ENDLOOP.
    ENDIF.

    " Check for leftover objects
    LOOP AT mt_items ASSIGNING FIELD-SYMBOL(<ls_item>) WHERE obj_type <> 'DEVC' AND obj_type <> 'NSPC'.
      IF zcl_abapgit_objects=>exists( <ls_item> ) = abap_true.
        zcx_abapgit_exception=>raise( |Leftover object { <ls_item>-obj_type } { <ls_item>-obj_name }| ).
      ENDIF.

      SELECT SINGLE * FROM tadir INTO @ls_tadir
        WHERE pgmid = 'R3TR' AND object = @<ls_item>-obj_type AND obj_name = @<ls_item>-obj_name.
      IF sy-subrc = 0.
        " Should have been deleted latest during release of transport
        zcx_abapgit_exception=>raise( |Obsolete TADIR entry { <ls_item>-obj_type } { <ls_item>-obj_name }| ).
      ENDIF.
    ENDLOOP.

    " Check for leftover texts in OTR
    SELECT COUNT(*) FROM sotr_head INTO @lv_count WHERE paket = @cs_ci_repo-package.
    IF lv_count > 0.
      zcx_abapgit_exception=>raise( |Leftover short texts: { lv_count }| ).
    ENDIF.

    SELECT COUNT(*) FROM sotr_headu INTO @lv_count WHERE paket = @cs_ci_repo-package.
    IF lv_count > 0.
      zcx_abapgit_exception=>raise( |Leftover long texts: { lv_count }| ).
    ENDIF.

    " Check if package is still registered in abapGit
    DATA(lt_repos) = zcl_abapgit_persist_factory=>get_repo( )->list( ).
    READ TABLE lt_repos TRANSPORTING NO FIELDS WITH KEY package = cs_ci_repo-package.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( |Package still registered in abapGit| ).
    ENDIF.

    cs_ci_repo-check_leftovers = zif_abapgit_ci_definitions=>co_status-ok.

  ENDMETHOD.


  METHOD check_objects.

    DATA: lt_items TYPE zif_abapgit_definitions=>ty_items_ts,
          ls_files TYPE zif_abapgit_definitions=>ty_stage_files.

    FIELD-SYMBOLS: <ls_local_file>  TYPE zif_abapgit_definitions=>ty_file_item,
                   <ls_remote_file> LIKE LINE OF ls_files-remote.

    cs_ci_repo-object_check = zif_abapgit_ci_definitions=>co_status-not_ok.

    DATA(lt_tadir) = zcl_abapgit_factory=>get_tadir( )->read( io_repo->get_package( ) ).

    LOOP AT lt_tadir ASSIGNING FIELD-SYMBOL(<ls_tadir>).

      DATA(ls_item) = CORRESPONDING zif_abapgit_definitions=>ty_item( <ls_tadir> MAPPING obj_name = obj_name
                                                                                         obj_type = object ).

      IF zcl_abapgit_objects=>is_active( ls_item ) = abap_false.
        zcx_abapgit_exception=>raise( |Object { ls_item-obj_type } { ls_item-obj_name } isn't active| ).
      ENDIF.

    ENDLOOP.

    io_repo->refresh_local_objects( ).
    io_repo->reset_status( ).

    ls_files = zcl_abapgit_factory=>get_stage_logic( )->get( io_repo ).

    log_diffs(
      is_ci_repo = cs_ci_repo
      is_files   = ls_files ).

    LOOP AT ls_files-local ASSIGNING <ls_local_file>.
      zcx_abapgit_exception=>raise( |Local file diffs to remote: { <ls_local_file>-file-filename }| ).
    ENDLOOP.

    LOOP AT ls_files-remote ASSIGNING <ls_remote_file>.
      zcx_abapgit_exception=>raise( |Remote file diffs to local: { <ls_remote_file>-filename }| ).
    ENDLOOP.

    cs_ci_repo-object_check = zif_abapgit_ci_definitions=>co_status-ok.

  ENDMETHOD.


  METHOD check_repo.

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
    check_exists(
      iv_package = io_repo->get_package( )
      io_dot     = lo_dot
      it_files   = lt_files ).

  ENDMETHOD.


  METHOD check_repo_exists.

    DATA lt_list TYPE zif_abapgit_repo_srv=>ty_repo_list.

    lt_list = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    READ TABLE lt_list TRANSPORTING NO FIELDS WITH TABLE KEY table_line = io_repo.
    rv_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD check_transport.

    DATA:
      ls_item                   TYPE zif_abapgit_definitions=>ty_item,
      lv_repo_object_count      TYPE i,
      lv_transport_object_count TYPE i,
      lv_objects_in_tr          TYPE i,
      lv_first_not_found        TYPE string,
      lv_first_too_much         TYPE string.

    IF iv_transport IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_deletion = abap_false.
      cs_ci_repo-check_create_transport = zif_abapgit_ci_definitions=>co_status-not_ok.
    ELSE.
      cs_ci_repo-check_delete_transport = zif_abapgit_ci_definitions=>co_status-not_ok.
    ENDIF.

    DATA(lt_objects) = read_transport( iv_transport ).

    DATA(lt_files_local) = io_repo->get_files_local( ).

    LOOP AT lt_files_local ASSIGNING FIELD-SYMBOL(<ls_file>) WHERE item-obj_type IS NOT INITIAL.
      IF check_item( is_item     = <ls_file>-item
                     iv_deletion = iv_deletion ) = abap_false.
        CONTINUE.
      ENDIF.

      adjust_item( CHANGING cs_item = <ls_file>-item ).

      lv_repo_object_count = lv_repo_object_count + 1.

      IF line_exists( lt_objects[ pgmid    = 'R3TR'
                                  object   = <ls_file>-item-obj_type
                                  obj_name = <ls_file>-item-obj_name
                                  objfunc  = COND #( WHEN iv_deletion = abap_true THEN 'D' ELSE space ) ] ).
        lv_transport_object_count = lv_transport_object_count + 1.
      ELSEIF lv_first_not_found IS INITIAL.
        lv_first_not_found = |First missing: { <ls_file>-item-obj_type } { <ls_file>-item-obj_name }|.
      ENDIF.
    ENDLOOP.

    IF lv_repo_object_count <> lv_transport_object_count.
      log_objects(
        is_ci_repo = cs_ci_repo
        it_objects = lt_objects ).

      zcx_abapgit_exception=>raise( |{ COND #( WHEN iv_deletion = abap_true THEN 'DELETE' ELSE 'CREATE' ) } | &&
                                    |transport { iv_transport }: Incorrect object count | &&
                                    |({ lv_transport_object_count NUMBER = USER } instead of | &&
                                    |{ lv_repo_object_count NUMBER = USER }) | &&
                                    |\n{ lv_first_not_found }| ).
    ENDIF.

    LOOP AT lt_objects ASSIGNING FIELD-SYMBOL(<ls_object>) WHERE object <> 'DEVC'.
      CLEAR ls_item.
      ls_item-obj_type = <ls_object>-object.
      ls_item-obj_name = <ls_object>-obj_name.

      IF check_item( is_item     = ls_item
                     iv_deletion = iv_deletion ) = abap_false.
        CONTINUE.
      ENDIF.

      lv_objects_in_tr = lv_objects_in_tr + 1.

      IF NOT line_exists( lt_files_local[ item-obj_type = <ls_object>-object
                                          item-obj_name = <ls_object>-obj_name ] ) AND lv_first_too_much IS INITIAL.
        lv_first_too_much = |First extra: { <ls_object>-object } { <ls_object>-obj_name }|.
      ENDIF.
    ENDLOOP.

    IF lv_objects_in_tr > lv_repo_object_count.
      log_objects(
        is_ci_repo = cs_ci_repo
        it_objects = lt_objects ).

      zcx_abapgit_exception=>raise( |{ COND #( WHEN iv_deletion = abap_true THEN 'DELETE' ELSE 'CREATE' ) } | &&
                                    |transport { iv_transport }: Too many objects | &&
                                    |({ lv_objects_in_tr NUMBER = USER } instead of | &&
                                    |{ lv_repo_object_count NUMBER = USER }) | &&
                                    |\n{ lv_first_too_much }| ).
    ENDIF.

    IF iv_deletion = abap_false.
      cs_ci_repo-check_create_transport = zif_abapgit_ci_definitions=>co_status-ok.
    ELSE.
      cs_ci_repo-check_delete_transport = zif_abapgit_ci_definitions=>co_status-ok.
    ENDIF.

  ENDMETHOD.


  METHOD cleanup_tadir.

    DATA lv_cproject TYPE tadir-cproject.

    " In case of transportable packages, some objects cannot be deleted (error TR 024):
    " Object directory entry cannot be deleted, since the object is distributed
    " This is normal but for the CI tests we ignore such entries and clean them up

    IF is_tadir-devclass(1) <> '$'.
      SELECT SINGLE cproject FROM tadir INTO @lv_cproject
        WHERE pgmid = @is_tadir-pgmid AND object = @is_tadir-object AND obj_name = @is_tadir-obj_name.
      IF sy-subrc = 0 AND lv_cproject+1(1) CA ' S'.
        DELETE FROM tadir
          WHERE pgmid = @is_tadir-pgmid AND object = @is_tadir-object AND obj_name = @is_tadir-obj_name.
        IF sy-subrc = 0.
          rv_cleaned_up = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD cleanup_transport.

    DATA lt_requests TYPE trwbo_request_headers.

    IF iv_transport IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING
        iv_trkorr          = iv_transport
      IMPORTING
        et_request_headers = lt_requests
      EXCEPTIONS
        invalid_input      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " Exit if transport is already released
    READ TABLE lt_requests TRANSPORTING NO FIELDS WITH KEY trkorr = iv_transport trstatus = 'R'.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    " First, delete all empty tasks
    LOOP AT lt_requests ASSIGNING FIELD-SYMBOL(<ls_task>) WHERE trkorr <> iv_transport AND trstatus <> 'R'.
      SELECT COUNT(*) FROM e071 INTO @DATA(lv_count) WHERE trkorr = @<ls_task>-trkorr.
      IF lv_count = 0.
        CALL FUNCTION 'TR_DELETE_COMM'
          EXPORTING
            wi_dialog                     = abap_false
            wi_trkorr                     = <ls_task>-trkorr
          EXCEPTIONS
            file_access_error             = 1
            order_already_released        = 2
            order_contains_c_member       = 3
            order_contains_locked_entries = 4
            order_is_refered              = 5
            repair_order                  = 6
            user_not_owner                = 7
            delete_was_cancelled          = 8
            ordernumber_empty             = 9
            tr_enqueue_failed             = 10
            objects_free_but_still_locks  = 11
            order_lock_failed             = 12
            no_authorization              = 13
            wrong_client                  = 14
            project_still_referenced      = 15
            successors_already_released   = 16
            OTHERS                        = 17.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise_t100( ).
        ENDIF.
      ELSE.
        DATA(lv_objects_found) = abap_true.
      ENDIF.
    ENDLOOP.

    " Then, delete empty transport request
    IF lv_objects_found = abap_false.
      SELECT COUNT(*) FROM e071 INTO @lv_count WHERE trkorr = @iv_transport.
      IF lv_count = 0.
        CALL FUNCTION 'TR_DELETE_COMM'
          EXPORTING
            wi_dialog                     = abap_false
            wi_trkorr                     = iv_transport
          EXCEPTIONS
            file_access_error             = 1
            order_already_released        = 2
            order_contains_c_member       = 3
            order_contains_locked_entries = 4
            order_is_refered              = 5
            repair_order                  = 6
            user_not_owner                = 7
            delete_was_cancelled          = 8
            ordernumber_empty             = 9
            tr_enqueue_failed             = 10
            objects_free_but_still_locks  = 11
            order_lock_failed             = 12
            no_authorization              = 13
            wrong_client                  = 14
            project_still_referenced      = 15
            successors_already_released   = 16
            OTHERS                        = 17.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise_t100( ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD clone.

    cs_ci_repo-clone = zif_abapgit_ci_definitions=>co_status-not_ok.

    " Use default branch
    co_repo ?= zcl_abapgit_repo_srv=>get_instance( )->new_online(
      iv_url     = |{ cs_ci_repo-clone_url }|
      iv_labels  = 'ci'
      iv_package = cs_ci_repo-package ).

    COMMIT WORK AND WAIT.

    check_repo( co_repo ).

    cs_ci_repo-clone = zif_abapgit_ci_definitions=>co_status-ok.

  ENDMETHOD.


  METHOD constructor.

    zcl_abapgit_ui_injector=>set_frontend_services( NEW lcl_mock_frontend_services( ) ).

    mo_log = NEW #( ).

  ENDMETHOD.


  METHOD create_package.

    IF cs_ci_repo-create_package = zif_abapgit_ci_definitions=>co_status-skipped.
      cs_ci_repo-create_package = zif_abapgit_ci_definitions=>co_status-undefined.
      RETURN.
    ENDIF.

    DATA(li_package) = zcl_abapgit_factory=>get_sap_package( cs_ci_repo-package ).

    IF li_package->exists( ) = abap_true.
      RETURN.
    ENDIF.

    cs_ci_repo-create_package = zif_abapgit_ci_definitions=>co_status-not_ok.

    DATA(ls_package_data) = VALUE scompkdtln(
      as4user   = sy-uname
      devclass  = cs_ci_repo-package
      ctext     = |abapGit CI run|
      pdevclass = cs_ci_repo-layer
      dlvunit   = 'HOME' ).

    IF cs_ci_repo-layer IS INITIAL.
      li_package->create( ls_package_data ).
    ELSE.
      " Assume not initial layer means transports are required;
      " zcl_abapgit_sap_package does not allow specifying a transport to create a package and will instead show
      " a popup (refactor?). Use SAP API directly instead

      ASSERT iv_transport IS NOT INITIAL.

      cl_package_factory=>create_new_package(
        EXPORTING
          i_suppress_dialog            = abap_true
        IMPORTING
          e_package                    = DATA(li_sap_package)
        CHANGING
          c_package_data               = ls_package_data
        EXCEPTIONS
          object_already_existing      = 1
          object_just_created          = 2
          not_authorized               = 3
          wrong_name_prefix            = 4
          undefined_name               = 5
          reserved_local_name          = 6
          invalid_package_name         = 7
          short_text_missing           = 8
          software_component_invalid   = 9
          layer_invalid                = 10
          author_not_existing          = 11
          component_not_existing       = 12
          component_missing            = 13
          prefix_in_use                = 14
          unexpected_error             = 15
          intern_err                   = 16
          no_access                    = 17
          invalid_translation_depth    = 18
          wrong_mainpack_value         = 19
          superpackage_invalid         = 20
          error_in_cts_checks          = 21
          OTHERS                       = 22 ).
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      li_sap_package->save(
        EXPORTING
          i_transport_request    = iv_transport
          i_suppress_dialog      = abap_true
        EXCEPTIONS
          object_invalid         = 1
          object_not_changeable  = 2
          cancelled_in_corr      = 3
          permission_failure     = 4
          unexpected_error       = 5
          intern_err             = 6
          OTHERS                 = 7 ).
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      li_sap_package->set_changeable(
        EXPORTING
          i_changeable                 = abap_false
          i_suppress_dialog            = abap_true
        EXCEPTIONS
          object_locked_by_other_user  = 1
          permission_failure           = 2
          object_already_changeable    = 3
          object_already_unlocked      = 4
          object_just_created          = 5
          object_deleted               = 6
          object_modified              = 7
          object_not_existing          = 8
          object_invalid               = 9
          unexpected_error             = 10
          OTHERS                       = 11 ).
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    cs_ci_repo-create_package = zif_abapgit_ci_definitions=>co_status-ok.

  ENDMETHOD.


  METHOD create_transport.

    DATA ls_request_header TYPE trwbo_request_header.

    IF iv_package(1) = '$'.
      RETURN.
    ENDIF.

    DATA(lv_text) = |abapGit CI { COND #( WHEN iv_deletion = abap_false THEN 'CREATE' ELSE 'DELETE' ) } | &&
                    |{ iv_repo_name } in { iv_package }|. " Might be too long but does not really matter

    " Reset standard request (to avoid confusion)
    zcl_abapgit_default_transport=>get_instance( )->reset( ).

    " Create new request for this repository and package
    CALL FUNCTION 'TR_INSERT_REQUEST_WITH_TASKS'
      EXPORTING
        iv_type           = 'K'
        iv_text           = CONV as4text( lv_text )
        it_users          = VALUE scts_users( ( user = cl_abap_syst=>get_user_name( ) type = 'S' ) )
      IMPORTING
        es_request_header = ls_request_header
      EXCEPTIONS
        insert_failed     = 1
        enqueue_failed    = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    rv_transport = ls_request_header-trkorr.

  ENDMETHOD.


  METHOD log_diffs.

    DATA lv_log TYPE abap_bool.

    IF is_ci_repo-logging = abap_false OR mo_log->is_active( c_log_type-diff ) = abap_false.
      RETURN.
    ENDIF.

    IF is_files-local IS NOT INITIAL.
      mo_log->add(
        iv_log_object = |{ is_ci_repo-name }, { is_ci_repo-package(1) }: Diff Local|
        ig_data       = is_files-local ).
      lv_log = abap_true.
    ENDIF.

    IF is_files-remote IS NOT INITIAL.
      mo_log->add(
        iv_log_object = |{ is_ci_repo-name }, { is_ci_repo-package(1) }: Diff Remote|
        ig_data       = is_files-remote ).
      lv_log = abap_true.
    ENDIF.

    IF lv_log = abap_true.
      mo_log->add(
        iv_log_object = |{ is_ci_repo-name }, { is_ci_repo-package(1) }: Diff Status|
        ig_data       = is_files-status ).
    ENDIF.

  ENDMETHOD.


  METHOD log_enq.

    DATA:
      lv_subrc TYPE sy-subrc,
      lt_enq   TYPE STANDARD TABLE OF seqg3 WITH DEFAULT KEY.

    IF is_ci_repo-logging = abap_false OR mo_log->is_active( c_log_type-enq ) = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        gclient               = sy-mandt
        guname                = sy-uname
      IMPORTING
        subrc                 = lv_subrc
      TABLES
        enq                   = lt_enq
      EXCEPTIONS
        communication_failure = 1
        OTHERS                = 2.
    ASSERT sy-subrc = 0.

    mo_log->add(
      iv_log_object = |{ is_ci_repo-name }, { is_ci_repo-package(1) }: Locks ({ iv_phase })|
      ig_data       = lt_enq ).

  ENDMETHOD.


  METHOD log_messages.

    DATA lv_errors TYPE abap_bool.

    IF ii_log IS BOUND.
      DATA(lt_messages) = ii_log->get_messages( ).
    ENDIF.

    LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_message>).
      CLEAR <ls_message>-exception.
    ENDLOOP.

    LOOP AT lt_messages ASSIGNING <ls_message> WHERE type CA 'AEX'.
      DATA(ls_message) = <ls_message>.
      lv_errors = abap_true.
      EXIT.
    ENDLOOP.

    IF lv_errors = abap_false.
      RETURN.
    ENDIF.

    IF is_ci_repo-logging = abap_true AND mo_log->is_active( c_log_type-msg ) = abap_true.
      mo_log->add(
        iv_log_object = |{ is_ci_repo-name }, { is_ci_repo-package(1) }: Messages ({ iv_phase })|
        ig_data       = lt_messages ).
    ENDIF.

    rv_error = |{ iv_phase } error: { ls_message-text }|.
    IF ls_message-obj_name IS NOT INITIAL.
      rv_error = rv_error && | \nObject: { ls_message-obj_type } { ls_message-obj_name }|.
    ENDIF.

  ENDMETHOD.


  METHOD log_objects.

    IF is_ci_repo-logging = abap_false OR mo_log->is_active( c_log_type-obj ) = abap_false.
      RETURN.
    ENDIF.

    mo_log->add(
      iv_log_object = |{ is_ci_repo-name }, { is_ci_repo-package(1) }: Objects in Transport|
      ig_data       = it_objects ).

  ENDMETHOD.


  METHOD log_syntax_errors.

    IF is_ci_repo-logging = abap_false OR mo_log->is_active( c_log_type-syntax ) = abap_false.
      RETURN.
    ENDIF.

    mo_log->add(
      iv_log_object = |{ is_ci_repo-name }, { is_ci_repo-package(1) }: Syntax Errors|
      ig_data       = it_list ).

  ENDMETHOD.


  METHOD log_tadir.

    DATA lt_tadir TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY.

    IF is_ci_repo-logging = abap_false OR mo_log->is_active( c_log_type-tadir ) = abap_false.
      RETURN.
    ENDIF.

    SELECT * FROM tadir INTO TABLE @lt_tadir
      WHERE devclass = @is_ci_repo-package
      ORDER BY PRIMARY KEY.

    mo_log->add(
      iv_log_object = |{ is_ci_repo-name }, { is_ci_repo-package(1) }: TADIR ({ iv_phase })|
      ig_data       = lt_tadir ).

  ENDMETHOD.


  METHOD pull.

    cs_ci_repo-pull = zif_abapgit_ci_definitions=>co_status-not_ok.

    log_tadir(
      is_ci_repo = cs_ci_repo
      iv_phase   = 'Before Pull' ).

    log_enq(
      is_ci_repo = cs_ci_repo
      iv_phase   = 'Before Pull' ).

    DATA(ls_checks) = zcl_abapgit_ci_repo_check=>get( io_repo ).

    ls_checks-transport-transport = iv_transport.

    DATA(li_log) = io_repo->create_new_log( 'Pull Log' ).

    io_repo->deserialize(
      is_checks = ls_checks
      ii_log    = li_log ).

    log_messages(
      is_ci_repo = cs_ci_repo
      ii_log     = li_log
      iv_phase   = 'Install' ).

    log_tadir(
      is_ci_repo = cs_ci_repo
      iv_phase   = 'After Pull' ).

    io_repo->refresh( iv_drop_cache = abap_true ).

    cs_ci_repo-pull = zif_abapgit_ci_definitions=>co_status-ok.

  ENDMETHOD.


  METHOD purge.

    DATA lv_error TYPE string.

    IF check_repo_exists( io_repo ) = abap_false OR cs_ci_repo-do_not_purge = abap_true.
      RETURN.
    ENDIF.

    cs_ci_repo-purge = zif_abapgit_ci_definitions=>co_status-not_ok.

    log_tadir(
      is_ci_repo = cs_ci_repo
      iv_phase   = 'Before Purge' ).

    TRY.
        DATA(ls_checks) = io_repo->delete_checks( ).

        ls_checks-transport-transport = iv_transport.

        zcl_abapgit_repo_srv=>get_instance( )->purge( ii_repo   = io_repo
                                                      is_checks = ls_checks ).

        COMMIT WORK AND WAIT.
      CATCH zcx_abapgit_cancel INTO DATA(lx_cancel).
        lv_error = lx_cancel->get_text( ).
      CATCH zcx_abapgit_exception INTO DATA(lx_exception).
        lv_error = log_messages(
          is_ci_repo = cs_ci_repo
          ii_log     = io_repo->get_log( )
          iv_phase   = 'Uninstall' ).

        IF lv_error IS INITIAL.
          lv_error = lx_exception->get_text( ).
        ENDIF.
    ENDTRY.

    log_enq(
      is_ci_repo = cs_ci_repo
      iv_phase   = 'After Purge' ).

    CALL FUNCTION 'DEQUEUE_ALL'
      EXPORTING
        _synchron = abap_true.

    IF lv_error IS NOT INITIAL.
      IF iv_cleanup = abap_true.
        RETURN.
      ELSE.
        zcx_abapgit_exception=>raise( lv_error ).
      ENDIF.
    ENDIF.

    log_tadir(
      is_ci_repo = cs_ci_repo
      iv_phase   = 'After Purge' ).

    cs_ci_repo-purge = zif_abapgit_ci_definitions=>co_status-ok.

  ENDMETHOD.


  METHOD read_transport.

    DATA:
      ls_request                TYPE trwbo_request,
      lt_converted_r3tr_objects TYPE tr_objects.

    CALL FUNCTION 'TR_READ_REQUEST'
      EXPORTING
        iv_trkorr        = iv_transport
      CHANGING
        cs_request       = ls_request
      EXCEPTIONS
        error_occured    = 1
        no_authorization = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'TR_GET_OBJECTS_OF_REQ_AN_TASKS'
      EXPORTING
        is_request_header      = ls_request-h
        iv_condense_objectlist = abap_true
      IMPORTING
        et_objects             = rt_objects
      EXCEPTIONS
        invalid_input          = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " Ignore entries for SOTR/SOTS
    DELETE rt_objects WHERE pgmid = 'LIMU' AND object = 'ADIR' AND obj_name CP 'R3TRSOT*'.

    " TODO: LIMUs are suspicious, should raise error but currently unknown how many test cases would be impacted

    " Convert LIMU to R3TR
    CALL FUNCTION 'TRINT_COMPLETE_REQUEST'
      EXPORTING
        it_e071              = rt_objects
        iv_without_update    = abap_true
      IMPORTING
        et_e071_complement   = lt_converted_r3tr_objects
      EXCEPTIONS
        invalid_request_type = 1
        invalid_request      = 2
        no_authority         = 3
        object_append_error  = 4
        no_systemname        = 5
        no_systemtype        = 6
        OTHERS               = 7.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    APPEND LINES OF lt_converted_r3tr_objects TO rt_objects.

    SORT rt_objects BY pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM rt_objects COMPARING pgmid object obj_name.

    DELETE rt_objects WHERE pgmid <> 'R3TR' OR ( pgmid  = 'R3TR' AND object = 'DEVC' ).

  ENDMETHOD.


  METHOD release_transport.

    DATA lt_requests TYPE trwbo_request_headers.

    IF iv_transport IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING
        iv_trkorr          = iv_transport
      IMPORTING
        et_request_headers = lt_requests
      EXCEPTIONS
        invalid_input      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " First, release all open tasks
    LOOP AT lt_requests ASSIGNING FIELD-SYMBOL(<ls_task>) WHERE trkorr <> iv_transport AND trstatus <> 'R'.
      CALL FUNCTION 'TR_RELEASE_REQUEST'
        EXPORTING
          iv_trkorr                  = <ls_task>-trkorr
          iv_dialog                  = abap_false
          iv_success_message         = abap_false
        EXCEPTIONS
          cts_initialization_failure = 1
          enqueue_failed             = 2
          no_authorization           = 3
          invalid_request            = 4
          request_already_released   = 5
          repeat_too_early           = 6
          error_in_export_methods    = 7
          object_check_error         = 8
          docu_missing               = 9
          db_access_error            = 10
          action_aborted_by_user     = 11
          export_failed              = 12
          OTHERS                     = 13.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

    " Then, release transport request
    CALL FUNCTION 'TR_RELEASE_REQUEST'
      EXPORTING
        iv_trkorr                  = iv_transport
        iv_dialog                  = abap_false
        iv_success_message         = abap_false
      EXCEPTIONS
        cts_initialization_failure = 1
        enqueue_failed             = 2
        no_authorization           = 3
        invalid_request            = 4
        request_already_released   = 5
        repeat_too_early           = 6
        error_in_export_methods    = 7
        object_check_error         = 8
        docu_missing               = 9
        db_access_error            = 10
        action_aborted_by_user     = 11
        export_failed              = 12
        OTHERS                     = 13.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " Wait for the release to finish
    DATA(lv_wait_time) = 0.
    DO.
      SELECT COUNT(*) FROM e070
        WHERE trkorr = iv_transport
          AND trstatus = 'R'.
      IF sy-dbcnt = 1.
        EXIT.
      ELSE.
        " Some test repos take a while longer to release
        IF lv_wait_time > 10.
          zcx_abapgit_exception=>raise( |Transport { iv_transport } could not be released| ).
        ENDIF.
        CALL FUNCTION 'RZL_SLEEP'.
        lv_wait_time = lv_wait_time + 1.
      ENDIF.
    ENDDO.

    COMMIT WORK AND WAIT.

    " Release of transport updates TADIR (deleted entries) but the TADIR changes aren't
    " visible to this process. Reset to table buffer will make sure that the process goes
    " back to DB for the latest TADIR data.
    CALL FUNCTION 'SBUF_RESET_BUFFERED_TABLE'
      EXPORTING
        sync    = abap_true
        reset   = abap_true
        tabname = 'TADIR'.

    " Reset standard request (to avoid confusion)
    zcl_abapgit_default_transport=>get_instance( )->reset( ).

  ENDMETHOD.


  METHOD run.

    DATA lo_repo TYPE REF TO zcl_abapgit_repo_online.

    TRY.
        DATA(lv_transport) = create_transport( iv_repo_name = cs_ci_repo-name
                                               iv_package   = cs_ci_repo-package ).

        create_package( EXPORTING iv_transport = lv_transport
                        CHANGING  cs_ci_repo   = cs_ci_repo ).

        clone( CHANGING cs_ci_repo = cs_ci_repo
                        co_repo    = lo_repo ).

        pull( EXPORTING io_repo      = lo_repo
                        iv_transport = lv_transport
              CHANGING  cs_ci_repo   = cs_ci_repo ).

        syntax_check( CHANGING cs_ci_repo = cs_ci_repo ).

        check_objects( EXPORTING io_repo    = lo_repo
                       CHANGING  cs_ci_repo = cs_ci_repo ).

        release_transport( lv_transport ).

        check_transport( EXPORTING io_repo      = lo_repo
                                   iv_transport = lv_transport
                         CHANGING  cs_ci_repo   = cs_ci_repo ).

        uninstall( EXPORTING io_repo    = lo_repo
                   CHANGING  cs_ci_repo = cs_ci_repo ).

      CATCH zcx_abapgit_cancel INTO DATA(lx_cancel).

        " ensure transport is deleted (if empty) or released after cancel
        cleanup_transport( lv_transport ).

        release_transport( lv_transport ).

        zcl_abapgit_ci_repos=>fail_message(
          EXPORTING
            iv_message = lx_cancel->get_text( )
          CHANGING
            cs_ci_repo = cs_ci_repo ).

      CATCH zcx_abapgit_exception INTO DATA(lx_error).

        " Ensure uninstall after error
        uninstall( EXPORTING io_repo    = lo_repo
                             iv_cleanup = abap_true
                   CHANGING  cs_ci_repo = cs_ci_repo ).

        zcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

    check_leftovers( CHANGING cs_ci_repo = cs_ci_repo ).

    cleanup_transport( lv_transport ).

    " Done. Release any locks
    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD syntax_check.

    cs_ci_repo-syntax_check = zif_abapgit_ci_definitions=>co_status-not_ok.

    DATA(lt_list) = zcl_abapgit_factory=>get_code_inspector( cs_ci_repo-package )->run( 'SYNTAX_CHECK' ).

    READ TABLE lt_list INTO DATA(ls_list) WITH KEY kind = 'E'.
    IF sy-subrc = 0.
      cs_ci_repo-message = |Syntax error: { ls_list-param1 }\nObject: { ls_list-objtype } { ls_list-objname }|.

      log_syntax_errors(
        is_ci_repo = cs_ci_repo
        it_list    = lt_list ).
    ELSE.
      cs_ci_repo-syntax_check = zif_abapgit_ci_definitions=>co_status-ok.
    ENDIF.

  ENDMETHOD.


  METHOD uninstall.

    COMMIT WORK AND WAIT.

    " Check if we want to keep installed repos
    IF cs_ci_repo-do_not_purge = abap_true.
      RETURN.
    ENDIF.

    DATA(lv_transport) = create_transport( iv_repo_name = cs_ci_repo-name
                                           iv_package   = cs_ci_repo-package
                                           iv_deletion  = abap_true ).

    purge( EXPORTING io_repo      = io_repo
                     iv_transport = lv_transport
           CHANGING  cs_ci_repo   = cs_ci_repo ).

    release_transport( lv_transport ).

    IF iv_cleanup = abap_false.
      check_transport( EXPORTING io_repo      = io_repo
                                 iv_transport = lv_transport
                                 iv_deletion  = abap_true
                       CHANGING  cs_ci_repo   = cs_ci_repo ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
