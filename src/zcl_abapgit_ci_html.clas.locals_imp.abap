*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_table_renderer IMPLEMENTATION.

  METHOD constructor.

    mr_table = REF #( it_table ).
    mo_table_descr ?= cl_abap_tabledescr=>describe_by_data( it_table ).
    mo_struct_descr ?= mo_table_descr->get_table_line_type( ).

  ENDMETHOD.

  METHOD render.

    rv_html = |<table class="tg">\n|
           && |  { render_table_head( ) }|
           && |  { render_table_lines( ) }|
           && |</table>\n|.

  ENDMETHOD.


  METHOD render_table_head.

    rv_html = |<tr>\n|.

    LOOP AT mo_struct_descr->get_components( ) ASSIGNING FIELD-SYMBOL(<ls_component>).

      rv_html = rv_html
            && |<th class="tg-kiyi">|
            && |{ CAST cl_abap_elemdescr( <ls_component>-type
                           )->get_ddic_field(
                           )-scrtext_m }|
            && |</th>\n|.

    ENDLOOP.

    rv_html = rv_html && |</tr>\n|.

  ENDMETHOD.


  METHOD render_table_lines.

    FIELD-SYMBOLS: <lt_table> TYPE INDEX TABLE.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_line>).

      rv_html = rv_html && |<tr>\n|.

      LOOP AT mo_struct_descr->get_components( ) ASSIGNING FIELD-SYMBOL(<ls_component>).

        ASSIGN COMPONENT <ls_component>-name
               OF STRUCTURE <ls_line>
               TO FIELD-SYMBOL(<field>).
        ASSERT sy-subrc = 0.

        IF <ls_component>-name CS |URL|.
          rv_html = rv_html && |<td class="tg-xldj { get_css_class_for_status( <field> ) }">|
                            && |<a href="{ <field> }">Repo</a></td>\n|.
        ELSE.
          rv_html = rv_html && |<td class="tg-xldj { get_css_class_for_status( <field> ) }">{ <field> }</td>\n|.
        ENDIF.

      ENDLOOP.

      rv_html = rv_html && |</tr>\n|.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_css_class_for_status.

    rv_css_class = SWITCH #(
                     iv_status
                       WHEN zif_abapgit_ci_definitions=>co_status-ok THEN |ok|
                       WHEN zif_abapgit_ci_definitions=>co_status-not_ok THEN |not_ok|
                       ELSE || ).

  ENDMETHOD.

ENDCLASS.
