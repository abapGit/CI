CLASS zcl_abapgit_ci_alv_view DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_ci_view.

  PRIVATE SECTION.
    DATA:
      mo_alv TYPE REF TO cl_salv_table.

    METHODS:
      config_column
        IMPORTING
          iv_column TYPE lvc_fname
          iv_text   TYPE csequence
          iv_width  TYPE lvc_outlen
        RAISING
          cx_salv_error,

      get_table_descr
        IMPORTING
          it_table              TYPE INDEX TABLE
        RETURNING
          VALUE(ro_table_descr) TYPE REF TO cl_abap_tabledescr,

      map
        IMPORTING
          it_table TYPE ANY TABLE
          ir_table TYPE REF TO data,

      prepare_splitter
        EXPORTING
          eo_row_1 TYPE REF TO cl_gui_container
          eo_row_2 TYPE REF TO cl_gui_container
          eo_row_3 TYPE REF TO cl_gui_container
          eo_row_4 TYPE REF TO cl_gui_container
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_ci_alv_view IMPLEMENTATION.


  METHOD config_column.

    DATA(lo_column) = CAST cl_salv_column_table( mo_alv->get_columns( )->get_column( iv_column ) ).

    lo_column->set_short_text( CONV #( iv_text ) ).
    lo_column->set_medium_text( CONV #( iv_text ) ).
    lo_column->set_long_text( CONV #( iv_text ) ).

    lo_column->set_output_length( iv_width ).

  ENDMETHOD.


  METHOD get_table_descr.

    DATA(lo_table_descr) = CAST cl_abap_tabledescr(
                             cl_abap_tabledescr=>describe_by_data( it_table ) ).

    DATA(components) = CAST cl_abap_structdescr( lo_table_descr->get_table_line_type( )
                        )->get_components( ).

    LOOP AT components ASSIGNING FIELD-SYMBOL(<component>)
                       WHERE type->absolute_name CP '*STATUS'.
      <component>-type ?= cl_abap_datadescr=>describe_by_name( |ICON_D| ).
    ENDLOOP.

    ro_table_descr = cl_abap_tabledescr=>create( cl_abap_structdescr=>create( components )  ).

  ENDMETHOD.


  METHOD map.

    DATA: lr_line TYPE REF TO data.

    FIELD-SYMBOLS:
      <table> TYPE table,
      <right> TYPE data,
      <left>  TYPE data,
      <line>  TYPE data.

    ASSIGN ir_table->* TO <table>.
    ASSERT sy-subrc = 0.

    DATA(components) = CAST cl_abap_structdescr(
                         CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data( it_table ) )->get_table_line_type( )
                       )->get_components( ).

    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<ls_line>).

      CREATE DATA lr_line LIKE LINE OF <table>.
      ASSIGN lr_line->* TO <line>.

      LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).

        ASSIGN COMPONENT <component>-name
               OF STRUCTURE <ls_line>
               TO <right>.
        ASSERT sy-subrc = 0.

        ASSIGN COMPONENT <component>-name
               OF STRUCTURE <line>
               TO <left>.
        ASSERT sy-subrc = 0.

        IF <component>-type->absolute_name CS |STATUS|.
          <left> = SWITCH icon_d(
                     <right>
                       WHEN zif_abapgit_ci_definitions=>co_status-ok        THEN icon_checked
                       WHEN zif_abapgit_ci_definitions=>co_status-not_ok    THEN icon_incomplete
                       WHEN zif_abapgit_ci_definitions=>co_status-undefined THEN icon_led_inactive ).
        ELSE.
          <left> = <right>.
        ENDIF.

      ENDLOOP.

      INSERT <line> INTO TABLE <table>.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_ci_view~display.

    DATA: lr_table TYPE REF TO data,
          lt_html  TYPE STANDARD TABLE OF char255,
          lv_url   TYPE char255.
    FIELD-SYMBOLS: <table> TYPE INDEX TABLE.

    DATA(lo_table_descr) = get_table_descr( cs_result-repo_result_list ).
    CREATE DATA lr_table TYPE HANDLE lo_table_descr.

    map( it_table = cs_result-repo_result_list
         ir_table = lr_table ).

    ASSIGN lr_table->* TO <table>.
    ASSERT sy-subrc = 0.

    prepare_splitter(
      IMPORTING
        eo_row_1 = DATA(lo_row_1)
        eo_row_2 = DATA(lo_row_2)
        eo_row_3 = DATA(lo_row_3)
        eo_row_4 = DATA(lo_row_4) ).

    lt_html = VALUE #( ( |<html>| )
                       ( |  <head>| )
                       ( |  <style>| )
                       ( |    body \{ font-family: arial\}| )
                       ( |  </style>| )
                       ( |  </head>| )
                       ( |  <body>| )
                       ( |    <h1>abapGit CI repository tests from: https://github.com/abapGit-tests</h1>| )
                       ( |  </body>| )
                       ( |</html>| ) ).

    DATA(lo_html_viewer) = NEW cl_gui_html_viewer( lo_row_1 ).

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
        url                    = lv_url    " URL
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = lo_row_2
          IMPORTING
            r_salv_table = mo_alv
          CHANGING
            t_table      = <table> ).

        mo_alv->get_functions( )->set_all( ).

        config_column( iv_column = 'NAME'
                       iv_text   = 'Name'
                       iv_width  = 20 ).

        config_column( iv_column = 'CLONE_URL'
                       iv_text   = 'Clone url'
                       iv_width  = 40 ).

        config_column( iv_column = 'PACKAGE'
                       iv_text   = 'Package'
                       iv_width  = 10 ).

        config_column( iv_column = 'CREATE_PACKAGE'
                       iv_text   = 'Create package'
                       iv_width  = 15 ).

        config_column( iv_column = 'CLONE'
                       iv_text   = 'Clone'
                       iv_width  = 8 ).

        config_column( iv_column = 'PULL'
                       iv_text   = 'Pull'
                       iv_width  = 8 ).

        config_column( iv_column = 'SYNTAX_CHECK'
                       iv_text   = 'Syntax check'
                       iv_width  = 15 ).

        config_column( iv_column = 'OBJECT_CHECK'
                       iv_text   = 'Object check'
                       iv_width  = 15 ).

        config_column( iv_column = 'PURGE'
                       iv_text   = 'Purge'
                       iv_width  = 8 ).

        config_column( iv_column = 'CHECK_LEFTOVERS'
                       iv_text   = 'Check leftovers'
                       iv_width  = 15 ).

        config_column( iv_column = 'STATUS'
                       iv_text   = 'Status'
                       iv_width  = 8 ).

        config_column( iv_column = 'MESSAGE'
                       iv_text   = 'Message'
                       iv_width  = 60 ).

        mo_alv->display( ).

      CATCH cx_salv_error INTO DATA(lx_error).
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).
    ENDTRY.

    lt_html = VALUE #( ( |<html>| )
                       ( |  <head>| )
                       ( |  <style>| )
                       ( |    body \{ font-family: arial\}| )
                       ( |  </style>| )
                       ( |  </head>| )
                       ( |  <body>| )
                       ( |    <h1>abapGit CI: Generic tests</h1>| )
                       ( |  </body>| )
                       ( |</html>| ) ).

    lo_html_viewer = NEW cl_gui_html_viewer( lo_row_3 ).

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

    lo_table_descr = get_table_descr( cs_result-generic_result_list ).
    CREATE DATA lr_table TYPE HANDLE lo_table_descr.

    map( it_table = cs_result-generic_result_list
         ir_table = lr_table ).

    ASSIGN lr_table->* TO <table>.
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = lo_row_4
          IMPORTING
            r_salv_table = DATA(lo_alv)
          CHANGING
            t_table      = <table> ).

        lo_alv->get_columns( )->set_optimize( ).

        lo_alv->display( ).

      CATCH cx_salv_error INTO lx_error.
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).
    ENDTRY.

    WRITE: ''.

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
        height            = 5
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

    lo_splitter->set_row_height(
      EXPORTING
        id                = 2
        height            = 30
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
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
        height            = 5
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

    lo_splitter->set_row_height(
      EXPORTING
        id                = 4
        height            = 30
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
