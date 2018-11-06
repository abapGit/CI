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
          !iv_column TYPE lvc_fname
          !iv_text   TYPE csequence
          !iv_width  TYPE lvc_outlen
        RAISING
          cx_salv_error,

      get_table_descr
        IMPORTING
          !it_table             TYPE INDEX TABLE
        RETURNING
          VALUE(ro_table_descr) TYPE REF TO cl_abap_tabledescr,

      map
        IMPORTING
          !it_table TYPE zif_abapgit_ci_definitions=>tty_result_list
          !ir_table TYPE REF TO data.

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

    DATA: lr_table TYPE REF TO data.
    FIELD-SYMBOLS: <table> TYPE INDEX TABLE.

    DATA(lo_table_descr) = get_table_descr( cs_result-list ).
    CREATE DATA lr_table TYPE HANDLE lo_table_descr.

    map( it_table = cs_result-list
         ir_table = lr_table ).

    ASSIGN lr_table->* TO <table>.
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory(
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

        config_column( iv_column = 'STATUS'
                       iv_text   = 'Status'
                       iv_width  = 8 ).

        config_column( iv_column = 'MESSAGE'
                       iv_text   = 'Message'
                       iv_width  = 60 ).

        mo_alv->set_top_of_list(
          NEW cl_salv_form_header_info(
            text = |abapGit CI: https://github.com/abapGit-tests| ) ).

        mo_alv->display( ).

      CATCH cx_salv_error INTO DATA(lx_error).
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
