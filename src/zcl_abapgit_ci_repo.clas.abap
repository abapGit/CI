CLASS zcl_abapgit_ci_repo DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS run
      CHANGING
        !cs_ri_repo TYPE zabapgit_ci_result
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:
      create_package
        IMPORTING
          iv_transport TYPE trkorr OPTIONAL
        CHANGING
          cs_ri_repo   TYPE zabapgit_ci_result
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
          io_repo      TYPE REF TO zcl_abapgit_repo_online
          iv_transport TYPE trkorr OPTIONAL
        CHANGING
          cs_ri_repo   TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_cancel
          zcx_abapgit_exception,

      purge
        IMPORTING
          io_repo      TYPE REF TO zcl_abapgit_repo_online
          iv_transport TYPE trkorr OPTIONAL
        CHANGING
          cs_ri_repo   TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      log_messages
        IMPORTING
          is_ri_repo  TYPE zabapgit_ci_result
          it_messages TYPE zif_abapgit_log=>ty_log_outs
        RAISING
          zcx_abapgit_exception,

      syntax_check
        CHANGING
          cs_ri_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      log_syntax_errors
        IMPORTING
          is_ri_repo TYPE zabapgit_ci_result
          it_list    TYPE scit_alvlist
        RAISING
          zcx_abapgit_exception,

      check_objects
        IMPORTING
          io_repo    TYPE REF TO zcl_abapgit_repo_online
        CHANGING
          cs_ri_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      log_diffs
        IMPORTING
          is_ri_repo TYPE zabapgit_ci_result
          is_files   TYPE zif_abapgit_definitions=>ty_stage_files
        RAISING
          zcx_abapgit_exception,

      check_leftovers
        CHANGING
          cs_ri_repo TYPE zabapgit_ci_result
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
          iv_deletion         TYPE abap_bool
        RETURNING
          VALUE(rv_transport) TYPE trkorr
        RAISING
          zcx_abapgit_exception,

      check_transport_request
        IMPORTING
          io_repo           TYPE REF TO zcl_abapgit_repo_online
          iv_transport      TYPE trkorr
          iv_check_deletion TYPE abap_bool
        CHANGING
          cs_ri_repo        TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      log_objects
        IMPORTING
          is_ri_repo TYPE zabapgit_ci_result
          it_objects TYPE tr_objects
        RAISING
          zcx_abapgit_exception,

      release_transport
        IMPORTING
          iv_transport TYPE trkorr
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_ci_repo IMPLEMENTATION.


  METHOD check_leftovers.

    DATA lv_count TYPE i.

    cs_ri_repo-check_leftovers = zif_abapgit_ci_definitions=>co_status-not_ok.

    DATA(lt_tadir) = zcl_abapgit_factory=>get_tadir(
                                       )->read( cs_ri_repo-package ).

    LOOP AT lt_tadir ASSIGNING FIELD-SYMBOL(<ls_tadir>)
                     WHERE object <> 'DEVC'.
      IF cleanup_tadir( <ls_tadir> ) = abap_false.
        zcx_abapgit_exception=>raise( |Leftover object { <ls_tadir>-object } { <ls_tadir>-obj_name }| ).
      ENDIF.
    ENDLOOP.

    IF cs_ri_repo-layer IS INITIAL. " Only check leftover local packages
      LOOP AT lt_tadir ASSIGNING <ls_tadir>
                       WHERE object = 'DEVC'.
        zcx_abapgit_exception=>raise( |Leftover package { <ls_tadir>-obj_name }| ).
      ENDLOOP.
    ENDIF.

    " Check for leftover texts in OTR
    SELECT COUNT(*) FROM sotr_head INTO @lv_count WHERE paket = @cs_ri_repo-package.
    IF lv_count > 0.
      zcx_abapgit_exception=>raise( |Leftover short texts: { lv_count }| ).
    ENDIF.

    SELECT COUNT(*) FROM sotr_headu INTO @lv_count WHERE paket = @cs_ri_repo-package.
    IF lv_count > 0.
      zcx_abapgit_exception=>raise( |Leftover long texts: { lv_count }| ).
    ENDIF.

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

    io_repo->reset_status( ).
    ls_files = zcl_abapgit_factory=>get_stage_logic( )->get( io_repo ).

    log_diffs(
      is_ri_repo = cs_ri_repo
      is_files   = ls_files ).

    LOOP AT ls_files-local ASSIGNING <ls_local_file>.
      zcx_abapgit_exception=>raise( |Local file diffs to remote: { <ls_local_file>-file-filename }| ).
    ENDLOOP.

    LOOP AT ls_files-remote ASSIGNING <ls_remote_file>.
      zcx_abapgit_exception=>raise( |Remote file diffs to local: { <ls_remote_file>-filename }| ).
    ENDLOOP.

    cs_ri_repo-object_check = zif_abapgit_ci_definitions=>co_status-ok.

  ENDMETHOD.


  METHOD check_transport_request.
    DATA: ls_request                TYPE trwbo_request,
          lt_objects                TYPE tr_objects,
          lv_repo_object_count      TYPE i,
          lv_transport_object_count TYPE i,
          lv_objects_in_tr          TYPE i,
          lv_first_not_found        TYPE string,
          lv_first_too_much         TYPE string,
          lt_converted_r3tr_objects TYPE tr_objects.

    IF iv_check_deletion = abap_false.
      cs_ri_repo-check_create_transport = zif_abapgit_ci_definitions=>co_status-not_ok.
    ELSE.
      cs_ri_repo-check_delete_transport = zif_abapgit_ci_definitions=>co_status-not_ok.
    ENDIF.

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
        et_objects             = lt_objects
      EXCEPTIONS
        invalid_input          = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " Ignore entries for SOTR/SOTS
    DELETE lt_objects WHERE pgmid = 'LIMU' AND object = 'ADIR' AND obj_name CP 'R3TRSOT*'.

    " TODO: LIMUs are suspicious, should raise error but currently unknown how many test cases would be impacted

    " Convert LIMU to R3TR
    CALL FUNCTION 'TRINT_COMPLETE_REQUEST'
      EXPORTING
        it_e071              = lt_objects
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

    APPEND LINES OF lt_converted_r3tr_objects TO lt_objects.
    SORT lt_objects BY pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM lt_objects COMPARING pgmid object obj_name.

    DELETE lt_objects WHERE pgmid <> 'R3TR'.
    DELETE lt_objects WHERE pgmid  = 'R3TR'
                        AND object = 'DEVC'.

    DATA(lt_files_local) = io_repo->get_files_local( ).

    LOOP AT lt_files_local ASSIGNING FIELD-SYMBOL(<ls_file>) WHERE item-obj_type IS NOT INITIAL.
      IF <ls_file>-item-obj_type = 'DEVC'.
        " Packages cannot be deleted in the same transport as its contents. abapGit does not delete transportable
        " packages on uninstall. Therefore these might still exist from the last run and might not be contained
        " in this transport. -> ignore for now
        CONTINUE.
      ENDIF.

      lv_repo_object_count = lv_repo_object_count + 1.

      IF line_exists( lt_objects[ pgmid    = 'R3TR'
                                  object   = <ls_file>-item-obj_type
                                  obj_name = <ls_file>-item-obj_name
                                  objfunc  = COND #( WHEN iv_check_deletion = abap_true THEN 'D' ELSE space ) ] ).
        lv_transport_object_count = lv_transport_object_count + 1.
      ELSEIF lv_first_not_found IS INITIAL.
        lv_first_not_found = | First missing: { <ls_file>-item-obj_type }-{ <ls_file>-item-obj_name }|.
      ENDIF.
    ENDLOOP.

    IF lv_repo_object_count <> lv_transport_object_count.
      log_objects(
        is_ri_repo = cs_ri_repo
        it_objects = lt_objects ).

      zcx_abapgit_exception=>raise( |{ COND #( WHEN iv_check_deletion = abap_true THEN 'DELETE' ELSE 'CREATE' ) } | &&
                                    |transport { iv_transport }: Incorrect object count | &&
                                    |({ lv_transport_object_count NUMBER = USER } instead of | &&
                                    |{ lv_repo_object_count NUMBER = USER })| &&
                                    |\n{ lv_first_not_found }| ).
    ENDIF.

    LOOP AT lt_objects ASSIGNING FIELD-SYMBOL(<ls_object>) WHERE object <> 'DEVC'.
      lv_objects_in_tr = lv_objects_in_tr + 1.

      IF NOT line_exists( lt_files_local[ item-obj_type = <ls_object>-object
                                          item-obj_name = <ls_object>-obj_name ] ) AND lv_first_too_much IS INITIAL.
        lv_first_too_much = | First extra: { <ls_object>-object }-{ <ls_object>-obj_name }|.
      ENDIF.
    ENDLOOP.

    IF lv_objects_in_tr > lv_repo_object_count.
      log_objects(
        is_ri_repo = cs_ri_repo
        it_objects = lt_objects ).

      zcx_abapgit_exception=>raise( |{ COND #( WHEN iv_check_deletion = abap_true THEN 'DELETE' ELSE 'CREATE' ) } | &&
                                    |transport { iv_transport }: Too many objects (| &&
                                    |({ lv_objects_in_tr NUMBER = USER } instead of | &&
                                    |{ lv_repo_object_count NUMBER = USER })| &&
                                    |\n{ lv_first_too_much }| ).
    ENDIF.

    IF iv_check_deletion = abap_false.
      cs_ri_repo-check_create_transport = zif_abapgit_ci_definitions=>co_status-ok.
    ELSE.
      cs_ri_repo-check_delete_transport = zif_abapgit_ci_definitions=>co_status-ok.
    ENDIF.
  ENDMETHOD.


  METHOD cleanup_tadir.

    DATA lv_cproject TYPE tadir-cproject.

    " In case of transportable packages, some objects cannot be deleted (error TR 024):
    " Object directory entry cannot be deleted, since the object is distributed
    " This is normal but for the CI tests we ignore such entries and clean them up
    DATA(lv_transportable) = xsdbool( is_tadir-devclass(1) <> '$' ).

    IF lv_transportable = abap_true.
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


  METHOD clone.

    cs_ri_repo-clone = zif_abapgit_ci_definitions=>co_status-not_ok.

    TRY.
        " Use default branch
        co_repo ?= zcl_abapgit_repo_srv=>get_instance( )->new_online(
          iv_url     = |{ cs_ri_repo-clone_url }|
          iv_package = cs_ri_repo-package ).

        COMMIT WORK AND WAIT.

      CATCH zcx_abapgit_cancel INTO DATA(error).
        zcx_abapgit_exception=>raise( error->get_text( ) ).
    ENDTRY.

    cs_ri_repo-clone = zif_abapgit_ci_definitions=>co_status-ok.

  ENDMETHOD.


  METHOD constructor.

    zcl_abapgit_ui_injector=>set_frontend_services( NEW lcl_mock_frontend_services( ) ).

  ENDMETHOD.


  METHOD create_package.

    DATA(li_package) = zcl_abapgit_factory=>get_sap_package( cs_ri_repo-package ).

    IF li_package->exists( ) = abap_false.

      cs_ri_repo-create_package = zif_abapgit_ci_definitions=>co_status-not_ok.

      DATA(ls_package_data) = VALUE scompkdtln(
        as4user   = sy-uname
        devclass  = cs_ri_repo-package
        ctext     = |abapGit CI run|
        pdevclass = cs_ri_repo-layer
        dlvunit   = 'HOME' ).

      IF cs_ri_repo-layer IS INITIAL.
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

      cs_ri_repo-create_package = zif_abapgit_ci_definitions=>co_status-ok.

    ENDIF.

  ENDMETHOD.


  METHOD create_transport.
    DATA: ls_request_header TYPE trwbo_request_header.

    DATA(lv_text) = |abapGit CI { COND #( WHEN iv_deletion = abap_false THEN 'CREATE' ELSE 'DELETE' ) } | &&
                    |{ iv_repo_name } in { iv_package }|. " Might be too long but does not really matter

    " Reset standard request (to avoid confusion)
    zcl_abapgit_default_transport=>get_instance( )->reset( ).

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

    IF is_ri_repo-logging = abap_false OR is_files IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_log) = NEW zcl_abapgit_ci_log( ).

    IF is_files-local IS NOT INITIAL.
      lo_log->add(
        iv_log_object = |{ is_ri_repo-name }, { is_ri_repo-package }: Diff Local|
        ig_data       = is_files-local ).
      lv_log = abap_true.
    ENDIF.

    IF is_files-remote IS NOT INITIAL.
      LOOP AT is_files-remote INTO DATA(ls_remote).
        lo_log->add(
          iv_log_object = |Diff Remote File|
          ig_data       = ls_remote-filename ).
      ENDLOOP.
      lo_log->add(
        iv_log_object = |{ is_ri_repo-name }, { is_ri_repo-package }: Diff Remote|
        ig_data       = is_files-remote ).
      lv_log = abap_true.
    ENDIF.

    IF lv_log = abap_true.
      lo_log->add(
        iv_log_object = |{ is_ri_repo-name }, { is_ri_repo-package }: Diff Status|
        ig_data       = is_files-status ).
    ENDIF.

  ENDMETHOD.


  METHOD log_messages.

    IF is_ri_repo-logging = abap_false OR it_messages IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_log) = NEW zcl_abapgit_ci_log( ).

    lo_log->add(
      iv_log_object = |{ is_ri_repo-name }, { is_ri_repo-package }: Log Messages|
      ig_data       = it_messages ).

  ENDMETHOD.


  METHOD log_objects.

    IF is_ri_repo-logging = abap_false OR it_objects IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_log) = NEW zcl_abapgit_ci_log( ).

    lo_log->add(
      iv_log_object = |{ is_ri_repo-name }, { is_ri_repo-package }: Objects in Transport|
      ig_data       = it_objects ).

  ENDMETHOD.


  METHOD log_syntax_errors.

    IF is_ri_repo-logging = abap_false OR it_list IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_log) = NEW zcl_abapgit_ci_log( ).

    lo_log->add(
      iv_log_object = |{ is_ri_repo-name }, { is_ri_repo-package }: Syntax Errors|
      ig_data       = it_list ).

  ENDMETHOD.


  METHOD pull.

    cs_ri_repo-pull = zif_abapgit_ci_definitions=>co_status-not_ok.

    DATA(ls_checks) = zcl_abapgit_ci_repo_check=>get(
                        io_repo         = io_repo
                        iv_check_exists = abap_true ).

    ls_checks-transport-transport = iv_transport.

    DATA(li_log) = io_repo->create_new_log( 'Pull Log' ).

    io_repo->deserialize(
      is_checks = ls_checks
      ii_log    = li_log ).

    io_repo->refresh( iv_drop_cache = abap_true ).

    log_messages(
      is_ri_repo  = cs_ri_repo
      it_messages = li_log->get_messages( ) ).

    cs_ri_repo-pull = zif_abapgit_ci_definitions=>co_status-ok.

  ENDMETHOD.


  METHOD purge.

    IF io_repo IS NOT BOUND OR cs_ri_repo-do_not_purge = abap_true.
      RETURN.
    ENDIF.

    cs_ri_repo-purge = zif_abapgit_ci_definitions=>co_status-not_ok.

    TRY.
        DATA(ls_checks) = io_repo->delete_checks( ).

        ls_checks-transport-transport = iv_transport.

        DATA(li_log) = zcl_abapgit_repo_srv=>get_instance( )->purge( ii_repo   = io_repo
                                                                     is_checks = ls_checks ).

        COMMIT WORK AND WAIT.
      CATCH zcx_abapgit_cancel INTO DATA(lx_error).
        zcx_abapgit_exception=>raise( lx_error->get_text( ) ).
      CATCH zcx_abapgit_exception INTO DATA(lx_exc).
        log_messages(
          is_ri_repo  = cs_ri_repo
          it_messages = li_log->get_messages( ) ).

        DATA(lv_exc) = lx_exc->get_text( ).
        IF lv_exc CP '*Check*log*'.
          READ TABLE li_log->get_messages( ) INTO DATA(ls_log) INDEX 1.
          IF sy-subrc = 0.
            lv_exc = |Uninstall error: { ls_log-text }|.
          ENDIF.
        ENDIF.
        zcx_abapgit_exception=>raise( lv_exc ).
    ENDTRY.

    cs_ri_repo-purge = zif_abapgit_ci_definitions=>co_status-ok.

    CALL FUNCTION 'DEQUEUE_ALL'
      EXPORTING
        _synchron = abap_true.

  ENDMETHOD.


  METHOD release_transport.
    DATA: lt_requests TYPE trwbo_request_headers.

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

    " First release all open tasks
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

    " Then release transport request
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
        IF lv_wait_time > 5.
          zcx_abapgit_exception=>raise( |Transport { iv_transport } could not be released| ).
        ENDIF.
        CALL FUNCTION 'RZL_SLEEP'.
        lv_wait_time = lv_wait_time + 1.
      ENDIF.
    ENDDO.

    " Reset standard request (to avoid confusion)
    zcl_abapgit_default_transport=>get_instance( )->reset( ).

  ENDMETHOD.


  METHOD run.
    DATA: lo_repo      TYPE REF TO zcl_abapgit_repo_online,
          lv_transport TYPE trkorr.

    DATA(lv_transportable) = xsdbool( cs_ri_repo-package(1) <> '$' ).

    IF lv_transportable = abap_true.
      lv_transport = create_transport( iv_repo_name = cs_ri_repo-name
                                       iv_package   = cs_ri_repo-package
                                       iv_deletion  = abap_false ).
    ENDIF.

    IF cs_ri_repo-create_package = zif_abapgit_ci_definitions=>co_status-skipped.
      cs_ri_repo-create_package = zif_abapgit_ci_definitions=>co_status-undefined.
    ELSE.
      create_package( EXPORTING iv_transport = COND #( WHEN lv_transportable = abap_true THEN lv_transport )
                      CHANGING  cs_ri_repo   = cs_ri_repo ).
    ENDIF.

    TRY.
        clone( CHANGING cs_ri_repo = cs_ri_repo
                        co_repo    = lo_repo ).

        pull( EXPORTING io_repo    = lo_repo
                        iv_transport = COND #( WHEN lv_transportable = abap_true THEN lv_transport )
              CHANGING  cs_ri_repo = cs_ri_repo ).

        syntax_check( CHANGING cs_ri_repo = cs_ri_repo ).

        check_objects( EXPORTING io_repo    = lo_repo
                       CHANGING  cs_ri_repo = cs_ri_repo ).

        IF lv_transportable = abap_true.
          release_transport( lv_transport ).
          check_transport_request( EXPORTING io_repo           = lo_repo
                                             iv_transport      = lv_transport
                                             iv_check_deletion = abap_false
                                   CHANGING  cs_ri_repo        = cs_ri_repo ).
        ENDIF.

      CATCH zcx_abapgit_cancel INTO DATA(lx_cancel).
        cs_ri_repo-message = lx_cancel->get_text( ).

      CATCH zcx_abapgit_exception INTO DATA(lx_error).

        " ensure uninstall
        IF lv_transportable = abap_true.
          COMMIT WORK AND WAIT.
          lv_transport = create_transport( iv_repo_name = cs_ri_repo-name
                                           iv_package   = cs_ri_repo-package
                                           iv_deletion  = abap_true ).
        ENDIF.
        purge( EXPORTING io_repo      = lo_repo
                         iv_transport = COND #( WHEN lv_transportable = abap_true THEN lv_transport )
               CHANGING  cs_ri_repo   = cs_ri_repo ).

        IF lv_transportable = abap_true.
          release_transport( lv_transport ).
        ENDIF.

        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).

    ENDTRY.

    " Check if we want to keep installed repos
    IF cs_ri_repo-do_not_purge = abap_true.
      RETURN.
    ENDIF.

    IF lv_transportable = abap_true.
      COMMIT WORK AND WAIT.
      lv_transport = create_transport( iv_repo_name = cs_ri_repo-name
                                       iv_package   = cs_ri_repo-package
                                       iv_deletion  = abap_true ).
    ENDIF.

    purge( EXPORTING io_repo      = lo_repo
                     iv_transport = COND #( WHEN lv_transportable = abap_true THEN lv_transport )
           CHANGING  cs_ri_repo   = cs_ri_repo ).

    IF lv_transportable = abap_true.
      release_transport( lv_transport ).
      check_transport_request( EXPORTING io_repo           = lo_repo
                                         iv_transport      = lv_transport
                                         iv_check_deletion = abap_true
                               CHANGING  cs_ri_repo        = cs_ri_repo ).
    ENDIF.

    check_leftovers( CHANGING cs_ri_repo = cs_ri_repo ).

  ENDMETHOD.


  METHOD syntax_check.

    cs_ri_repo-syntax_check = zif_abapgit_ci_definitions=>co_status-not_ok.

    DATA(li_syntax_check) = zcl_abapgit_factory=>get_code_inspector( cs_ri_repo-package ).

    DATA(lt_list) = li_syntax_check->run( 'SYNTAX_CHECK' ).

    READ TABLE lt_list TRANSPORTING NO FIELDS
                       WITH KEY kind = 'E'.
    IF sy-subrc = 0.
      cs_ri_repo-syntax_check = zif_abapgit_ci_definitions=>co_status-not_ok.

      log_syntax_errors(
        is_ri_repo = cs_ri_repo
        it_list    = lt_list ).
    ELSE.
      cs_ri_repo-syntax_check = zif_abapgit_ci_definitions=>co_status-ok.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
