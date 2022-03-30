REPORT zabapgit_ci_cleanup LINE-SIZE 255.

DATA: gv_package TYPE devclass.

PARAMETERS: p_uninst TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_pack FOR gv_package.
PARAMETERS: p_list  TYPE abap_bool RADIOBUTTON GROUP r2 DEFAULT 'X',
            p_purge TYPE abap_bool RADIOBUTTON GROUP r2,
            p_remov TYPE abap_bool RADIOBUTTON GROUP r2,
            p_obj   TYPE abap_bool RADIOBUTTON GROUP r2,
            p_otr   TYPE abap_bool RADIOBUTTON GROUP r2,
            p_pack  TYPE abap_bool RADIOBUTTON GROUP r2.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN SKIP.

PARAMETERS: p_trrel TYPE abap_bool RADIOBUTTON GROUP r1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_txt  TYPE as4text DEFAULT 'abapGit CI*',
            p_prev TYPE abap_bool AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK b2.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS:
      run RAISING zcx_abapgit_exception,
      list RAISING zcx_abapgit_exception,
      uninstall_repos RAISING zcx_abapgit_exception,
      check_packages RAISING zcx_abapgit_exception,
      drop_packages RAISING zcx_abapgit_exception,
      drop_objects RAISING zcx_abapgit_exception,
      drop_otr RAISING zcx_abapgit_exception,
      delete_package
        IMPORTING
          iv_package   TYPE devclass
          iv_transport TYPE trkorr
        RAISING
          zcx_abapgit_exception,
      delete_object
        IMPORTING
          iv_package   TYPE devclass
          iv_obj_type  TYPE tadir-object
          iv_obj_name  TYPE tadir-obj_name
          iv_transport TYPE trkorr
        RAISING
          zcx_abapgit_exception,
      delete_tadir
        IMPORTING
          iv_obj_type TYPE tadir-object
          iv_obj_name TYPE tadir-obj_name
        RAISING
          zcx_abapgit_exception,
      release_transports RAISING zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_width TYPE i VALUE 200.
    TYPES: ty_devc_tt TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY.
    METHODS:
      get_packages RETURNING VALUE(rt_devclass) TYPE ty_devc_tt,
      list_packages RAISING zcx_abapgit_exception,
      list_objects,
      list_otr.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD run.
    CASE abap_true.
      WHEN p_uninst.
        check_packages( ).
        IF p_pack = abap_true.
          drop_packages( ).
        ELSEIF p_obj = abap_true.
          drop_objects( ).
        ELSEIF p_otr = abap_true.
          drop_otr( ).
        ELSEIF p_list = abap_true.
          list( ).
        ELSE.
          uninstall_repos( ).
        ENDIF.
      WHEN p_trrel.
        release_transports( ).
    ENDCASE.
  ENDMETHOD.

  METHOD list.

    list_packages( ).

    list_objects( ).

    list_otr( ).

  ENDMETHOD.

  METHOD list_packages.

    DATA:
      lt_devclass TYPE STANDARD TABLE OF devclass,
      li_repo     TYPE REF TO zif_abapgit_repo.

    DATA(lv_found) = abap_false.

    SELECT devclass FROM tdevc INTO TABLE @lt_devclass WHERE devclass IN @s_pack[] ORDER BY devclass.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Packages:', lines( lt_devclass ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    IF sy-subrc = 0.
      LOOP AT lt_devclass INTO DATA(lv_devclass).
        FORMAT COLOR COL_NORMAL.
        WRITE: AT /5 lv_devclass, AT c_width space.
        FORMAT COLOR OFF.
      ENDLOOP.
    ELSE.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
    ENDIF.
    SKIP.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Repositories:', AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    DATA(li_repo_srv) = zcl_abapgit_repo_srv=>get_instance( ).

    LOOP AT lt_devclass INTO lv_devclass.
      TRY.
          li_repo_srv->get_repo_from_package(
            EXPORTING
              iv_package = lv_devclass
            IMPORTING
              ei_repo    = li_repo ).
          IF li_repo IS NOT INITIAL.
            lv_found = abap_true.
            FORMAT COLOR COL_NORMAL.
            WRITE: AT /5 'Repository:', li_repo->get_name( ), AT c_width space.
            SKIP.
          ENDIF.
        CATCH zcx_abapgit_exception.
      ENDTRY.
    ENDLOOP.

    IF lv_found = abap_false.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
      SKIP.
    ENDIF.

  ENDMETHOD.

  METHOD list_objects.
    DATA lt_tadir TYPE STANDARD TABLE OF tadir.

    SELECT * FROM tadir INTO TABLE @lt_tadir WHERE devclass IN @s_pack[]
      ORDER BY devclass, pgmid, object, obj_name.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Objects:', lines( lt_tadir ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    IF sy-subrc = 0.
      LOOP AT lt_tadir INTO DATA(ls_tadir).
        FORMAT COLOR COL_NORMAL.
        WRITE: AT /5 ls_tadir-object, ls_tadir-obj_name, ls_tadir-devclass, AT c_width space.
        FORMAT COLOR OFF.
      ENDLOOP.
    ELSE.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
    ENDIF.
    SKIP.

  ENDMETHOD.

  METHOD list_otr.
    DATA:
      lt_head  TYPE STANDARD TABLE OF sotr_head,
      lt_headu TYPE STANDARD TABLE OF sotr_headu,
      ls_use   TYPE sotr_use,
      ls_useu  TYPE sotr_useu.

    SELECT * FROM sotr_head INTO TABLE @lt_head WHERE paket IN @s_pack[]
      ORDER BY paket, concept.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Short Texts:', lines( lt_head ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    IF sy-subrc = 0.
      LOOP AT lt_head INTO DATA(ls_head).
        FORMAT COLOR COL_NORMAL.
        WRITE: AT /5 ls_head-concept.
        SELECT SINGLE * FROM sotr_use INTO @ls_use WHERE concept = @ls_head-concept.
        IF sy-subrc = 0.
          WRITE: ls_use-object, ls_use-obj_name.
        ENDIF.
        WRITE AT c_width space.
        FORMAT COLOR OFF.
      ENDLOOP.
    ELSE.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
    ENDIF.
    SKIP.

    SELECT * FROM sotr_headu INTO TABLE @lt_headu WHERE paket IN @s_pack[]
      ORDER BY paket, concept.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Long Texts:', lines( lt_headu ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    IF sy-subrc = 0.
      LOOP AT lt_headu INTO DATA(ls_headu).
        FORMAT COLOR COL_NORMAL.
        WRITE: AT /5 ls_headu-concept.
        SELECT SINGLE * FROM sotr_useu INTO @ls_useu WHERE concept = @ls_headu-concept.
        IF sy-subrc = 0.
          WRITE: ls_useu-object, ls_useu-obj_name.
        ENDIF.
        WRITE AT c_width space.
        FORMAT COLOR OFF.
      ENDLOOP.
    ELSE.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
    ENDIF.
    SKIP.

  ENDMETHOD.

  METHOD uninstall_repos.
    DATA: lv_transport TYPE trkorr,
          lo_repo      TYPE REF TO zcl_abapgit_repo.

    DATA(li_repo_srv) = zcl_abapgit_repo_srv=>get_instance( ).

    LOOP AT li_repo_srv->list( ) INTO DATA(li_repo).
      lo_repo ?= li_repo.

      IF lo_repo->get_package( ) NOT IN s_pack[].
        CONTINUE.
      ENDIF.

      DATA(ls_checks) = lo_repo->delete_checks( ).

      IF ls_checks-transport-required = abap_true.
        IF lv_transport IS INITIAL.
          lv_transport = zcl_abapgit_ui_factory=>get_popups( )->popup_transport_request(
            VALUE #( request = 'K' task = 'S' ) ).
        ENDIF.
        ls_checks-transport-transport = lv_transport.
      ENDIF.

      CASE abap_true.
        WHEN p_purge.
          WRITE: / |Purge { lo_repo->get_name( ) } in { lo_repo->get_package( ) }|.
          li_repo_srv->purge( ii_repo = lo_repo is_checks = ls_checks ).
        WHEN p_remov.
          WRITE: / |Delete { lo_repo->get_name( ) } in { lo_repo->get_package( ) }|.
          li_repo_srv->delete( lo_repo ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_packages.
    DATA lv_count TYPE i.

    IF s_pack[] IS INITIAL.
      zcx_abapgit_exception=>raise( 'No selection for packages found' ).
    ENDIF.

    SELECT COUNT(*) FROM tadir INTO @lv_count
      WHERE pgmid = 'R3TR' AND object = 'DEVC' AND obj_name IN @s_pack[]
      AND ( srcsystem = 'SAP' OR author = 'SAP' ).

    IF lv_count > 0.
      zcx_abapgit_exception=>raise( 'Selection contains standard SAP packages' ).
    ENDIF.
  ENDMETHOD.

  METHOD get_packages.
    SELECT devclass FROM tdevc INTO TABLE @rt_devclass WHERE devclass IN @s_pack[] ORDER BY devclass.

    SELECT obj_name FROM tadir APPENDING TABLE @rt_devclass
      WHERE pgmid = 'R3TR' AND object = 'DEVC' AND obj_name IN @s_pack[] ORDER BY PRIMARY KEY.

    SORT rt_devclass.
    DELETE ADJACENT DUPLICATES FROM rt_devclass.
  ENDMETHOD.

  METHOD drop_packages.
    DATA lv_transport TYPE trkorr.
    DATA lv_count TYPE i.

    DATA(lt_devclass) = get_packages( ).

    FORMAT COLOR COL_KEY.
    WRITE: / 'Packages:', lines( lt_devclass ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    LOOP AT lt_devclass INTO DATA(lv_devclass).
      FORMAT COLOR COL_NORMAL.
      WRITE: AT /5 lv_devclass.

      SELECT COUNT(*) FROM tadir INTO @lv_count
        WHERE pgmid = 'R3TR' AND object <> 'DEVC' AND object <> 'SOTR' AND object <> 'SOTS' AND devclass = @lv_devclass.

      IF lv_count = 0.
        TRY.
            IF lv_devclass(1) <> '$' AND lv_transport IS INITIAL.
              lv_transport = zcl_abapgit_ui_factory=>get_popups( )->popup_transport_request(
                VALUE #( request = 'K' task = 'S' ) ).
            ENDIF.

            delete_package(
              iv_package   = lv_devclass
              iv_transport = lv_transport ).

            delete_tadir(
              iv_obj_type = 'DEVC'
              iv_obj_name = |{ lv_devclass }| ).

            WRITE: 'Deleted' COLOR COL_POSITIVE.
          CATCH zcx_abapgit_exception INTO DATA(lx_ex).
            WRITE: 'Error' COLOR COL_NEGATIVE, lx_ex->get_text( ).
        ENDTRY.

      ELSE.
        WRITE: 'Not empty' COLOR COL_TOTAL.
      ENDIF.
      WRITE AT c_width space.
      FORMAT COLOR OFF.
    ENDLOOP.

    IF sy-subrc <> 0.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
    ENDIF.

  ENDMETHOD.

  METHOD drop_objects.
    DATA lv_transport TYPE trkorr.
    DATA lt_object TYPE TABLE OF tadir.
    DATA ls_object TYPE tadir.

    DATA(lt_devclass) = get_packages( ).

    FORMAT COLOR COL_KEY.
    WRITE: / 'Packages:', lines( lt_devclass ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    LOOP AT lt_devclass INTO DATA(lv_devclass).
      FORMAT COLOR COL_NORMAL.
      WRITE: AT /5 lv_devclass, AT c_width space.
      FORMAT COLOR OFF.
      SKIP.

      IF lv_devclass(1) <> '$' AND lv_transport IS INITIAL.
        lv_transport = zcl_abapgit_ui_factory=>get_popups( )->popup_transport_request(
          VALUE #( request = 'K' task = 'S' ) ).
      ENDIF.

      SELECT * FROM tadir INTO TABLE @lt_object
        WHERE pgmid = 'R3TR' AND object <> 'DEVC'
          AND delflag = '' AND devclass = @lv_devclass
        ORDER BY PRIMARY KEY.

      LOOP AT lt_object INTO ls_object.
        FORMAT COLOR COL_NORMAL.
        WRITE: AT /10 ls_object-object, ls_object-obj_name.

        TRY.
            delete_object(
              iv_package   = lv_devclass
              iv_obj_type  = ls_object-object
              iv_obj_name  = ls_object-obj_name
              iv_transport = lv_transport ).

            WRITE: 'Deleted' COLOR COL_POSITIVE.
          CATCH zcx_abapgit_exception INTO DATA(lx_ex).
            WRITE: 'Error' COLOR COL_NEGATIVE, lx_ex->get_text( ).
        ENDTRY.

        delete_tadir(
          iv_obj_type = ls_object-object
          iv_obj_name = ls_object-obj_name ).

        WRITE AT c_width space.
        FORMAT COLOR OFF.
      ENDLOOP.

      IF sy-subrc = 0.
        SKIP.
      ENDIF.
    ENDLOOP.

    IF sy-subrc <> 0.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
    ENDIF.

  ENDMETHOD.

  METHOD drop_otr.
    DATA lt_head TYPE STANDARD TABLE OF sotr_head.
    DATA lt_headu TYPE STANDARD TABLE OF sotr_headu.

    DATA(lt_devclass) = get_packages( ).

    SELECT * FROM sotr_head INTO TABLE @lt_head WHERE paket IN @s_pack[] ORDER BY paket, concept.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Short Texts:', lines( lt_head ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    LOOP AT lt_head INTO DATA(ls_head).
      FORMAT COLOR COL_NORMAL.
      WRITE: AT /5 ls_head-concept, ls_head-paket, ls_head-crea_name, ls_head-crea_tstut.

      CALL FUNCTION 'BTFR_DELETE_SINGLE_TEXT'
        EXPORTING
          concept               = ls_head-concept
          flag_string           = abap_false
          flag_correction_entry = abap_false
        EXCEPTIONS
          text_not_found        = 1
          invalid_package       = 2
          text_not_changeable   = 3
          text_enqueued         = 4
          no_correction         = 5
          parameter_error       = 6
          OTHERS                = 7.
      IF sy-subrc <> 0.
        WRITE: 'Error' COLOR COL_NEGATIVE.
      ELSE.
        WRITE: 'Deleted' COLOR COL_POSITIVE.
      ENDIF.
      WRITE AT c_width space.
      FORMAT COLOR OFF.
    ENDLOOP.
    SKIP.

    IF sy-subrc <> 0.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
      SKIP.
    ENDIF.

    SELECT * FROM sotr_headu INTO TABLE @lt_headu WHERE paket IN @s_pack ORDER BY paket, concept.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Long Texts:', lines( lt_headu ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    LOOP AT lt_headu INTO DATA(ls_headu).
      FORMAT COLOR COL_NORMAL.
      WRITE: AT /5 ls_headu-concept, ls_headu-paket, ls_headu-crea_name, ls_headu-crea_tstut.

      CALL FUNCTION 'BTFR_DELETE_SINGLE_TEXT'
        EXPORTING
          concept               = ls_headu-concept
          flag_string           = abap_true
          flag_correction_entry = abap_false
        EXCEPTIONS
          text_not_found        = 1
          invalid_package       = 2
          text_not_changeable   = 3
          text_enqueued         = 4
          no_correction         = 5
          parameter_error       = 6
          OTHERS                = 7.
      IF sy-subrc <> 0.
        WRITE: 'Error' COLOR COL_NEGATIVE.
      ELSE.
        WRITE: 'Deleted' COLOR COL_POSITIVE.
      ENDIF.
      WRITE AT c_width space.
      FORMAT COLOR OFF.
    ENDLOOP.
    SKIP.

    IF sy-subrc <> 0.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
      SKIP.
    ENDIF.

    " Drop TADIR
    LOOP AT lt_devclass INTO DATA(lv_devclass).
      SELECT * FROM sotr_head INTO TABLE @lt_head WHERE paket = @lv_devclass ORDER BY PRIMARY KEY.
      IF sy-subrc = 4.
        delete_tadir(
          iv_obj_type = 'SOTR'
          iv_obj_name = |{ lv_devclass }| ).
      ENDIF.
      SELECT * FROM sotr_headu INTO TABLE @lt_headu WHERE paket = @lv_devclass ORDER BY PRIMARY KEY.
      IF sy-subrc = 4.
        delete_tadir(
          iv_obj_type = 'SOTS'
          iv_obj_name = |{ lv_devclass }| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD delete_package.
    DATA li_package TYPE REF TO if_package.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = iv_package
        i_force_reload             = abap_true
      IMPORTING
        e_package                  = li_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5
        OTHERS                     = 6 ).

    IF li_package IS NOT BOUND.
      RETURN.
    ENDIF.

    li_package->set_changeable(
      EXPORTING
        i_changeable                = abap_true
        i_suppress_dialog           = abap_true
      EXCEPTIONS
        object_locked_by_other_user = 1
        permission_failure          = 2
        object_already_changeable   = 3
        object_already_unlocked     = 4
        object_just_created         = 5
        object_deleted              = 6
        object_modified             = 7
        object_not_existing         = 8
        object_invalid              = 9
        unexpected_error            = 10
        OTHERS                      = 11 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    li_package->delete(
      EXPORTING
        i_suppress_dialog     = abap_true
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        intern_err            = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    li_package->save(
      EXPORTING
        i_suppress_dialog     = abap_true
        i_transport_request   = iv_transport
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        cancelled_in_corr     = 3
        permission_failure    = 4
        unexpected_error      = 5
        intern_err            = 6
        OTHERS                = 7 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD delete_object.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_class_name TYPE seoclsname.
    DATA lx_error TYPE REF TO zcx_abapgit_exception.
    DATA li_obj TYPE REF TO zif_abapgit_object.

    ls_item-obj_type = iv_obj_type.
    ls_item-obj_name = iv_obj_name.

    IF zcl_abapgit_objects=>is_type_supported( ls_item-obj_type ) = abap_false.
      RETURN.
    ENDIF.

    lv_class_name = 'ZCL_ABAPGIT_OBJECT_' && iv_obj_type.

    CREATE OBJECT li_obj TYPE (lv_class_name)
      EXPORTING
        is_item     = ls_item
        iv_language = zif_abapgit_definitions=>c_english.

    li_obj->delete( iv_package   = iv_package
                    iv_transport = iv_transport ).

    delete_tadir(
      iv_obj_type = ls_item-obj_type
      iv_obj_name = ls_item-obj_name ).

  ENDMETHOD.

  METHOD delete_tadir.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_delete_tadir_entry = abap_true
        wi_tadir_pgmid        = 'R3TR'
        wi_tadir_object       = iv_obj_type
        wi_tadir_obj_name     = iv_obj_name
        wi_test_modus         = abap_false
      EXCEPTIONS
        OTHERS                = 1 ##FM_SUBRC_OK.
    IF sy-subrc <> 0 AND sy-msgid = 'TR' AND sy-msgno = '024'.
      " Object directory entry cannot be deleted, since the object is distributed (TR 024)
      " Force deletion of TADIR
      DELETE FROM tadir
        WHERE pgmid = 'R3TR' AND object = @iv_obj_type AND obj_name = @iv_obj_name.
    ENDIF.

  ENDMETHOD.

  METHOD release_transports.
    DATA: ls_ranges   TYPE trsel_ts_ranges,
          lt_requests TYPE trwbo_request_headers,
          lv_msg_text TYPE string.
    FIELD-SYMBOLS: <ls_request> TYPE trwbo_request_header.

    ls_ranges-as4text = p_txt.
    ls_ranges-request_status = VALUE #( ( sign = 'I' option = 'EQ' low = 'D' ) ).
    ls_ranges-task_status = VALUE #( ( sign = 'I' option = 'EQ' low = 'D' ) ).

    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      IMPORTING
        et_requests = lt_requests
      CHANGING
        cs_ranges   = ls_ranges.

    IF lines( lt_requests ) = 0.
      zcx_abapgit_exception=>raise( 'No transport requests found' ).
    ENDIF.

    IF p_prev = abap_true.
      cl_demo_output=>display( lt_requests ).
      IF zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
           iv_titlebar      = 'Confirm release'
           iv_text_question = |Release all { lines( lt_requests ) } requests / tasks?|
         ) <> '1'.
        zcx_abapgit_exception=>raise( 'Cancelled by user' ).
      ENDIF.
    ENDIF.

    LOOP AT lt_requests ASSIGNING <ls_request> WHERE trfunction = 'S' AND trstatus <> 'R'.
      WRITE: / |Releasing task { <ls_request>-trkorr }|.
      CALL FUNCTION 'TR_RELEASE_REQUEST'
        EXPORTING
          iv_trkorr                  = <ls_request>-trkorr
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
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO lv_msg_text.
        WRITE: / |Error: { lv_msg_text }|.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_requests ASSIGNING <ls_request> WHERE trfunction = 'K'.
      WRITE: / |Releasing request { <ls_request>-trkorr }|.
      CALL FUNCTION 'TR_RELEASE_REQUEST'
        EXPORTING
          iv_trkorr                  = <ls_request>-trkorr
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
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO lv_msg_text.
        WRITE: / |Error: { lv_msg_text }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  s_pack[] = VALUE #( ( sign = 'I' option = 'CP' low = 'Z___*' )
                      ( sign = 'I' option = 'CP' low = '$___*' ) ).

START-OF-SELECTION.
  TRY.
      NEW lcl_main( )->run( ).
    CATCH zcx_abapgit_exception INTO DATA(gx_ex).
      WRITE: / gx_ex->get_text( ).
      MESSAGE gx_ex TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
