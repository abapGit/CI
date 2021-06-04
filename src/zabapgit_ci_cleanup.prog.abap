REPORT zabapgit_ci_cleanup.

DATA: gv_package TYPE devclass.

PARAMETERS: p_uninst TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_pack FOR gv_package.
PARAMETERS: p_purge TYPE abap_bool RADIOBUTTON GROUP r2 DEFAULT 'X',
            p_remov TYPE abap_bool RADIOBUTTON GROUP r2,
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
      uninstall_repos RAISING zcx_abapgit_exception,
      check_packages RAISING zcx_abapgit_exception,
      drop_packages RAISING zcx_abapgit_exception,
      delete_package IMPORTING iv_package TYPE devclass RAISING zcx_abapgit_exception,
      release_transports RAISING zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD run.
    CASE abap_true.
      WHEN p_uninst.
        check_packages( ).
        IF p_pack = abap_true.
          drop_packages( ).
        ELSE.
          uninstall_repos( ).
        ENDIF.
      WHEN p_trrel.
        release_transports( ).
    ENDCASE.
  ENDMETHOD.

  METHOD uninstall_repos.
    DATA: lv_transport TYPE trkorr.
    DATA(li_repo_srv) = zcl_abapgit_repo_srv=>get_instance( ).

    LOOP AT li_repo_srv->list( ) INTO DATA(lo_repo).
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
          li_repo_srv->purge( io_repo = lo_repo is_checks = ls_checks ).
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

    SELECT COUNT(*) FROM tadir INTO lv_count
      WHERE pgmid = 'R3TR' AND object = 'DEVC' AND obj_name IN s_pack[]
      AND ( srcsystem = 'SAP' OR author = 'SAP' ).

    IF lv_count > 0.
      zcx_abapgit_exception=>raise( 'Selection contains standard SAP packages' ).
    ENDIF.
  ENDMETHOD.

  METHOD drop_packages.
    DATA lv_devclass TYPE devclass.
    DATA lv_count TYPE i.

    SELECT devclass FROM tdevc INTO lv_devclass WHERE devclass IN s_pack[] ORDER BY devclass.

      SELECT COUNT(*) FROM tadir INTO lv_count
        WHERE pgmid = 'R3TR' AND ( object <> 'DEVC' AND object <> 'SOTR' ) AND devclass = lv_devclass.

      IF lv_count = 0.
        delete_package( lv_devclass ).
      ENDIF.

    ENDSELECT.
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

    TRY.
        li_package->set_changeable(
          i_changeable      = abap_true
          i_suppress_dialog = abap_true ).

        li_package->delete( i_suppress_dialog = abap_true ).

        li_package->save( i_suppress_dialog = abap_true ).

      CATCH cx_root INTO DATA(lx_error).
        zcx_abapgit_exception=>raise( |Error deleting package { iv_package }: { lx_error->get_text( ) }| ).
    ENDTRY.

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
