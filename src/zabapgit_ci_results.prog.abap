*&---------------------------------------------------------------------*
*& Report zabapgit_ci_results
*&---------------------------------------------------------------------*
*& View abapGit CI results (https://ci.abapgit.org/ or MIME repository)
*& See https://github.com/abapGit/CI
*&---------------------------------------------------------------------*
REPORT zabapgit_ci_results.

SELECTION-SCREEN BEGIN OF SCREEN 1001.
* dummy for triggering screen
SELECTION-SCREEN END OF SCREEN 1001.

CLASS lcl_mime DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS get_content
      IMPORTING
        iv_loio           TYPE sdok_loid
      RETURNING
        VALUE(rv_content) TYPE xstring.

  PRIVATE SECTION.

    CLASS-METHODS get_url
      IMPORTING
        iv_loio       TYPE sdok_loid
      RETURNING
        VALUE(rv_url) TYPE string.

ENDCLASS.

CLASS lcl_mime IMPLEMENTATION.

  METHOD get_content.

    DATA li_api TYPE REF TO if_mr_api.

    DATA(lv_url) = get_url( iv_loio ).

    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
    li_api->get(
      EXPORTING
        i_url              = lv_url
      IMPORTING
        e_content          = rv_content
      EXCEPTIONS
        parameter_missing  = 1
        error_occured      = 2
        not_found          = 3
        permission_failure = 4
        OTHERS             = 5 ).
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD get_url.

    DATA:
      ls_smimloio TYPE smimloio,
      ls_io       TYPE skwf_io,
      lv_url      TYPE skwf_url.

    SELECT SINGLE * FROM smimloio INTO @ls_smimloio WHERE loio_id = @iv_loio.
    ASSERT sy-subrc = 0.

    ls_io-objtype = skwfc_obtype_loio.
    ls_io-class = ls_smimloio-lo_class.
    ls_io-objid = ls_smimloio-loio_id.

    CALL FUNCTION 'SKWF_NMSPC_IO_ADDRESS_GET'
      EXPORTING
        io  = ls_io
      IMPORTING
        url = lv_url.

    rv_url = lv_url.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui DEFINITION.

  PUBLIC SECTION.
    METHODS on_event
      FOR EVENT sapevent OF zif_abapgit_html_viewer
      IMPORTING
        !action
        !frame
        !getdata
        !postdata
        !query_table.

    METHODS startup
      RAISING
        zcx_abapgit_exception.

    METHODS cache_mime
      IMPORTING
        iv_loio       TYPE csequence
      RETURNING
        VALUE(rv_url) TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS show_mime
      IMPORTING
        iv_loio TYPE csequence
      RAISING
        zcx_abapgit_exception.

    METHODS handle_action
      IMPORTING
        !iv_action   TYPE c
        !iv_getdata  TYPE c OPTIONAL
        !it_postdata TYPE cnht_post_data_tab OPTIONAL ##NEEDED.

    METHODS back
      RETURNING
        VALUE(rv_exit) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

    METHODS free
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.

    DATA mi_html_viewer TYPE REF TO zif_abapgit_html_viewer.

ENDCLASS.

CLASS lcl_gui IMPLEMENTATION.

  METHOD startup.

    DATA:
      ls_event  TYPE cntl_simple_event,
      lt_events TYPE cntl_simple_events.

    mi_html_viewer = zcl_abapgit_ui_core_factory=>get_html_viewer( ).

    ls_event-eventid    = mi_html_viewer->c_id_sapevent.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.

    mi_html_viewer->set_registered_events( lt_events ).
    SET HANDLER on_event FOR mi_html_viewer.

  ENDMETHOD.

  METHOD cache_mime.

    TYPES ty_hex TYPE x LENGTH 1000.

    DATA:
      lv_size  TYPE i,
      lt_xdata TYPE STANDARD TABLE OF ty_hex WITH DEFAULT KEY.

    DATA(lv_xdata) = lcl_mime=>get_content( iv_loio ).

    zcl_abapgit_convert=>xstring_to_bintab(
      EXPORTING
        iv_xstr   = lv_xdata
      IMPORTING
        ev_size   = lv_size
        et_bintab = lt_xdata ).

    mi_html_viewer->load_data(
      EXPORTING
        iv_type         = 'text'
        iv_subtype      = 'html'
        iv_size         = lv_size
      IMPORTING
        ev_assigned_url = rv_url
      CHANGING
        ct_data_table   = lt_xdata ).

  ENDMETHOD.

  METHOD show_mime.

    mi_html_viewer->show_url( cache_mime( iv_loio ) ).

  ENDMETHOD.

  METHOD on_event.

    handle_action(
      iv_action   = action
      iv_getdata  = getdata
      it_postdata = postdata ).

  ENDMETHOD.

  METHOD handle_action.

    TRY.
        IF iv_action = 'go-back'.
          back( ).
        ENDIF.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD back.

    rv_exit = abap_true.

  ENDMETHOD.

  METHOD free.

    SET HANDLER on_event FOR mi_html_viewer ACTIVATION space.
    mi_html_viewer->close_document( ).
    mi_html_viewer->free( ).
    FREE mi_html_viewer.

  ENDMETHOD.

ENDCLASS.

DATA go_gui TYPE REF TO lcl_gui.

INITIALIZATION.

  DATA lt_ucomm TYPE TABLE OF sy-ucomm.

  PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.

  APPEND 'CRET' TO lt_ucomm.  "Button Execute
  APPEND 'SPOS' TO lt_ucomm.  "Button Save

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = lt_ucomm.

  PERFORM remove_toolbar IN PROGRAM zabapgit IF FOUND USING '1001'.

AT SELECTION-SCREEN ON EXIT-COMMAND.

  CASE sy-ucomm.
    WHEN 'CBAC' OR 'CCAN'.  "Back & Escape
      IF go_gui->back( ) = abap_true.
        go_gui->free( ).
      ELSE.
        LEAVE TO SCREEN 1001.
      ENDIF.
  ENDCASE.

START-OF-SELECTION.

  CREATE OBJECT go_gui.

  go_gui->startup( ).

  go_gui->show_mime( '0242AC1100021EEBB1BBC829FAE4FE35' ).

  CALL SELECTION-SCREEN 1001.
