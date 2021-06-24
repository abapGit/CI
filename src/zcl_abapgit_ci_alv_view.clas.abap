CLASS zcl_abapgit_ci_alv_view DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_ci_view .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS prepare_splitter
      IMPORTING
        !iv_gen   TYPE abap_bool
        !iv_repo  TYPE abap_bool
      EXPORTING
        !eo_row_1 TYPE REF TO cl_gui_container
        !eo_row_2 TYPE REF TO cl_gui_container
        !eo_row_3 TYPE REF TO cl_gui_container
        !eo_row_4 TYPE REF TO cl_gui_container
      RAISING
        zcx_abapgit_exception .
    METHODS prepare_header
      IMPORTING
        !iv_text      TYPE string
        !io_container TYPE REF TO cl_gui_container
      RAISING
        zcx_abapgit_exception .
    METHODS display_alv
      CHANGING
        !cs_result TYPE zif_abapgit_ci_definitions=>ty_result
      RAISING
        zcx_abapgit_exception .
    METHODS display_list
      CHANGING
        !cs_result TYPE zif_abapgit_ci_definitions=>ty_result
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_CI_ALV_VIEW IMPLEMENTATION.


  METHOD display_alv.

    prepare_splitter(
      EXPORTING
        iv_gen   = xsdbool( cs_result-generic_result_list IS NOT INITIAL )
        iv_repo  = xsdbool( cs_result-repo_result_list IS NOT INITIAL )
      IMPORTING
        eo_row_1 = DATA(lo_row_1)
        eo_row_2 = DATA(lo_row_2)
        eo_row_3 = DATA(lo_row_3)
        eo_row_4 = DATA(lo_row_4) ).

    IF cs_result-generic_result_list IS NOT INITIAL.
      prepare_header( iv_text      = zif_abapgit_ci_definitions=>co_title_generic
                      io_container = lo_row_1 ).

      NEW lcl_alv( io_container = lo_row_2
                   it_table     = cs_result-generic_result_list )->display( ).
    ENDIF.

    IF cs_result-repo_result_list IS NOT INITIAL.
      prepare_header( iv_text      = zif_abapgit_ci_definitions=>co_title_repos
                      io_container = lo_row_3 ).

      NEW lcl_alv( io_container = lo_row_4
                   it_table     = cs_result-repo_result_list )->display( ).
    ENDIF.

    WRITE: ''.

  ENDMETHOD.


  METHOD display_list.

    DATA lt_header TYPE TABLE OF zabapgit_ci_result_header.
    DATA ls_print TYPE slis_print_alv.

    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_INIT'
      EXPORTING
        i_callback_program = sy-cprog.

    IF cs_result-generic_result_list IS NOT INITIAL.
      lt_header = VALUE #( ( title = zif_abapgit_ci_definitions=>co_title_generic ) ).

      NEW lcl_list( it_table   = lt_header
                    iv_tabname = 'ZABAPGIT_CI_RESULT_HEADER' )->display( ).

      NEW lcl_list( it_table   = cs_result-generic_result_list
                    iv_tabname = 'ZABAPGIT_CI_RESULT_GEN' )->display( ).
    ENDIF.

    IF cs_result-repo_result_list IS NOT INITIAL.
      lt_header = VALUE #( ( title = zif_abapgit_ci_definitions=>co_title_repos ) ).

      NEW lcl_list( it_table   = lt_header
                    iv_tabname = 'ZABAPGIT_CI_RESULT_HEADER' )->display( ).

      NEW lcl_list( it_table   = cs_result-repo_result_list
                    iv_tabname = 'ZABAPGIT_CI_RESULT' )->display( ).
    ENDIF.

    ls_print-no_print_listinfos = abap_true.

    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_DISPLAY'
      EXPORTING
        is_print      = ls_print
      EXCEPTIONS
        program_error = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD prepare_header.

    DATA: lt_html TYPE STANDARD TABLE OF char255,
          lv_url  TYPE char255.

    lt_html = VALUE #( ( |<html>| )
                       ( |  <head>| )
                       ( |  <style>| )
                       ( |    body \{ font-family: arial\}| )
                       ( |  </style>| )
                       ( |  </head>| )
                       ( |  <body>| )
                       ( |    <h1>{ iv_text }</h1>| )
                       ( |  </body>| )
                       ( |</html>| ) ).

    DATA(lo_html_viewer) = NEW cl_gui_html_viewer( io_container ).

    lo_html_viewer->load_data(
      IMPORTING
        assigned_url           = lv_url
      CHANGING
        data_table             = lt_html
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_html_viewer->show_url(
      EXPORTING
        url                    = lv_url
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD prepare_splitter.

    DATA(lo_splitter) = NEW cl_gui_splitter_container(
        parent                  = cl_gui_container=>default_screen
        rows                    = 4
        columns                 = 1
        no_autodef_progid_dynnr = abap_true ).

    eo_row_1 = lo_splitter->get_container( row    = 1
                                           column = 1 ).

    eo_row_2 = lo_splitter->get_container( row    = 2
                                           column = 1 ).

    eo_row_3 = lo_splitter->get_container( row    = 3
                                           column = 1 ).

    eo_row_4 = lo_splitter->get_container( row    = 4
                                           column = 1 ).

    lo_splitter->set_row_sash(
      EXPORTING
        id                = 1
        type              = cl_gui_splitter_container=>type_movable
        value             = cl_gui_splitter_container=>true
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_splitter->set_row_height(
      EXPORTING
        id                = 1
        height            = COND #( WHEN iv_gen = abap_true THEN 6 ELSE 0 )
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_splitter->set_row_sash(
      EXPORTING
        id                = 2
        type              = cl_gui_splitter_container=>type_movable
        value             = cl_gui_splitter_container=>true
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF iv_repo = abap_true.
      lo_splitter->set_row_height(
        EXPORTING
          id                = 2
          height            = COND #( WHEN iv_gen = abap_true THEN 10 ELSE 0 )
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3 ).

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    lo_splitter->set_row_sash(
      EXPORTING
        id                = 3
        type              = cl_gui_splitter_container=>type_movable
        value             = cl_gui_splitter_container=>true
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_splitter->set_row_height(
      EXPORTING
        id                = 3
        height            = COND #( WHEN iv_repo = abap_true THEN 6 ELSE 0 )
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_splitter->set_row_sash(
      EXPORTING
        id                = 4
        type              = cl_gui_splitter_container=>type_movable
        value             = cl_gui_splitter_container=>true
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF iv_gen = abap_true.
      lo_splitter->set_row_height(
        EXPORTING
          id                = 4
          height            = COND #( WHEN iv_repo = abap_true THEN 40 ELSE 0 )
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3 ).

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ci_view~display.

    IF zcl_abapgit_ui_factory=>get_gui_functions( )->gui_is_available( ).
      display_alv( CHANGING cs_result = cs_result ).
    ELSE.
      display_list( CHANGING cs_result = cs_result ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
