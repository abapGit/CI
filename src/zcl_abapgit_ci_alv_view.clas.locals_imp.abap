*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_view IMPLEMENTATION.

  METHOD constructor.

    CONSTANTS lc_width TYPE lvc_outlen VALUE 11.

    CREATE DATA mr_table LIKE it_table.

    map_status_to_icon( it_table = it_table
                        ir_table = mr_table ).

    config_column( iv_column = 'NAME'
                   iv_width  = 20 ).

    config_column( iv_column = 'CLONE_URL'
                   iv_width  = 60 ).

    config_column( iv_column = 'PACKAGE'
                   iv_width  = 25 ).

    config_column( iv_column = 'LAYER'
                   iv_width  = 5 ).

    config_column( iv_column = 'CREATE_PACKAGE'
                   iv_width  = 12 ).

    config_column( iv_column = 'SKIP'
                   iv_width  = lc_width ).

    config_column( iv_column = 'DO_NOT_PURGE'
                   iv_width  = lc_width ).

    config_column( iv_column = 'CLONE'
                   iv_width  = lc_width ).

    config_column( iv_column = 'PULL'
                   iv_width  = lc_width ).

    config_column( iv_column = 'SYNTAX_CHECK'
                   iv_width  = lc_width ).

    config_column( iv_column = 'OBJECT_CHECK'
                   iv_width  = lc_width ).

    config_column( iv_column = 'CHECK_CREATE_TRANSPORT'
                   iv_width  = lc_width ).

    config_column( iv_column = 'PURGE'
                   iv_width  = lc_width ).

    config_column( iv_column = 'CHECK_DELETE_TRANSPORT'
                   iv_width  = lc_width ).

    config_column( iv_column = 'CHECK_LEFTOVERS'
                   iv_width  = lc_width ).

    config_column( iv_column = 'STATUS'
                   iv_width  = lc_width ).

    config_column( iv_column = 'DURATION'
                   iv_width  = 10 ).

    config_column( iv_column = 'MESSAGE'
                   iv_width  = 80 ).

    config_column( iv_column = 'DESCRIPTION'
                   iv_width  = 60 ).

    config_column( iv_column = 'TITLE'
                   iv_width  = 120 ).

  ENDMETHOD.


  METHOD map_status_to_icon.

    DATA: lr_line TYPE REF TO data.

    FIELD-SYMBOLS:
      <lt_table> TYPE table,
      <lv_right> TYPE data,
      <lv_left>  TYPE data,
      <lv_line>  TYPE data.

    ASSIGN ir_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    DATA(components) = CAST cl_abap_structdescr(
                         CAST cl_abap_tabledescr(
                           cl_abap_tabledescr=>describe_by_data( it_table )
                                            )->get_table_line_type( )
                       )->get_components( ).

    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<ls_line>).

      CREATE DATA lr_line LIKE LINE OF <lt_table>.
      ASSIGN lr_line->* TO <lv_line>.

      LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).

        ASSIGN COMPONENT <component>-name
               OF STRUCTURE <ls_line>
               TO <lv_right>.
        ASSERT sy-subrc = 0.

        ASSIGN COMPONENT <component>-name
               OF STRUCTURE <lv_line>
               TO <lv_left>.
        ASSERT sy-subrc = 0.

        IF <component>-type->get_ddic_header( )-refname CS |STATUS|.
          <lv_left> = SWITCH icon_d(
                        <lv_right>
                          WHEN zif_abapgit_ci_definitions=>co_status-ok        THEN icon_checked
                          WHEN zif_abapgit_ci_definitions=>co_status-not_ok    THEN icon_incomplete
                          WHEN zif_abapgit_ci_definitions=>co_status-undefined THEN icon_led_inactive
                          WHEN zif_abapgit_ci_definitions=>co_status-skipped   THEN icon_total_right
                          ELSE icon_space ).
        ELSE.
          <lv_left> = <lv_right>.
        ENDIF.

      ENDLOOP.

      INSERT <lv_line> INTO TABLE <lt_table>.

    ENDLOOP.

  ENDMETHOD.


  METHOD config_column.

    INSERT VALUE #( column = iv_column
                    width  = iv_width )
           INTO TABLE mt_column_width.

  ENDMETHOD.


ENDCLASS.


CLASS lcl_alv IMPLEMENTATION.

  METHOD constructor.

    super->constructor( it_table = it_table ).

    mo_container = io_container.

  ENDMETHOD.

  METHOD display.

    DATA: ls_color TYPE lvc_s_colo.

    FIELD-SYMBOLS: <lt_table> TYPE INDEX TABLE.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = mo_container
          IMPORTING
            r_salv_table = mo_alv
          CHANGING
            t_table      = <lt_table> ).

        mo_alv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

        mo_alv->get_functions( )->set_all( ).

        LOOP AT mt_column_width ASSIGNING FIELD-SYMBOL(<ls_column_width>).

          TRY.
              DATA(lo_column) = CAST cl_salv_column_table(
                                       mo_alv->get_columns(
                                            )->get_column( <ls_column_width>-column ) ).
              lo_column->set_output_length( <ls_column_width>-width ).

              IF <ls_column_width>-column = |NAME| OR
                 <ls_column_width>-column = |PACKAGE| OR
                 <ls_column_width>-column = |DESCRIPTION|.
                lo_column->set_key( ).
                lo_column->set_optimized( ).
              ENDIF.

              IF <ls_column_width>-column = |SKIP| OR
                 <ls_column_width>-column = |CATEGORY|.
                lo_column->set_technical( ).
              ENDIF.

              IF <ls_column_width>-column = |STATUS|.
                ls_color-col = col_total.
                lo_column->set_color( ls_color ).
              ENDIF.

            CATCH cx_salv_error.
              CONTINUE.
          ENDTRY.

        ENDLOOP.

        mo_alv->display( ).

      CATCH cx_salv_error INTO DATA(lx_error).
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_list IMPLEMENTATION.

  METHOD constructor.

    super->constructor( it_table = it_table ).
    mv_tabname = iv_tabname.

  ENDMETHOD.

  METHOD display.

    DATA:
      lt_fieldcat TYPE slis_t_fieldcat_alv,
      ls_layout   TYPE slis_layout_alv,
      lt_events   TYPE slis_t_event.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = mv_tabname
      CHANGING
        ct_fieldcat            = lt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).

      ASSIGN mt_column_width[ column = <ls_fieldcat>-fieldname ] TO FIELD-SYMBOL(<ls_column_width>).
      IF sy-subrc = 0.
        <ls_fieldcat>-outputlen = <ls_column_width>-width.

        IF <ls_fieldcat>-fieldname = |NAME| OR
           <ls_fieldcat>-fieldname = |PACKAGE| OR
           <ls_fieldcat>-fieldname = |DESCRIPTION| OR
           <ls_fieldcat>-fieldname = |TITLE|.
          <ls_fieldcat>-key = abap_true.
        ENDIF.

        IF <ls_fieldcat>-fieldname = |SKIP| OR
           <ls_fieldcat>-fieldname = |CATEGORY|.
          <ls_fieldcat>-no_out = abap_true.
        ENDIF.
      ENDIF.

    ENDLOOP.

    ls_layout-box_tabname = 'ZABAPGIT_CI_RESULT'.
    ls_layout-no_colhead  = xsdbool( mv_tabname CS 'HEADER' ).

    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
      EXPORTING
        is_layout                  = ls_layout
        it_fieldcat                = lt_fieldcat
        i_tabname                  = mv_tabname
        it_events                  = lt_events
      TABLES
        t_outtab                   = <lt_table>
      EXCEPTIONS
        program_error              = 1
        maximum_of_appends_reached = 2
        OTHERS                     = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
