REPORT zabapgit_ci_cleanup.

DATA: gv_package TYPE devclass.

PARAMETERS: p_uninst TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_pack FOR gv_package.
PARAMETERS: p_purge TYPE abap_bool RADIOBUTTON GROUP r2 DEFAULT 'X',
            p_remov TYPE abap_bool RADIOBUTTON GROUP r2.
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
      release_transports RAISING zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD run.
    CASE abap_true.
      WHEN p_uninst.
        uninstall_repos( ).
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
            VALUE #( request = 'K' task = 'S' )
          ).
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

  METHOD release_transports.
    DATA: ls_ranges   TYPE trsel_ts_ranges,
          lt_requests TYPE trwbo_request_headers.
    FIELD-SYMBOLS: <ls_request> TYPE trwbo_request_header.

    ls_ranges-as4text = p_txt.
    ls_ranges-request_status = VALUE #( ( sign = 'I' option = 'EQ' low = 'D' ) ).

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

    LOOP AT lt_requests ASSIGNING <ls_request> WHERE trfunction = 'S'.
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
        zcx_abapgit_exception=>raise_t100( ).
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
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  s_pack[] = VALUE #( ( sign = 'I' option = 'CP' low = 'Z___*' ) ).

START-OF-SELECTION.
  TRY.
      NEW lcl_main( )->run( ).
    CATCH zcx_abapgit_exception INTO DATA(gx_ex).
      WRITE: / gx_ex->get_text( ).
      MESSAGE gx_ex TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
