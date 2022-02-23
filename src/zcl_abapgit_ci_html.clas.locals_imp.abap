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

    rv_html = |<thead>\n|.
    rv_html = rv_html && |<tr>\n|.

    LOOP AT mo_struct_descr->get_components( ) ASSIGNING FIELD-SYMBOL(<ls_component>)
      WHERE name <> 'SKIP'.

      rv_html = rv_html
            && |<th class="tg-kiyi">|
            && |{ CAST cl_abap_elemdescr( <ls_component>-type
                           )->get_ddic_field(
                           )-scrtext_m }|
            && |</th>\n|.

    ENDLOOP.

    rv_html = rv_html && |</tr>\n|.
    rv_html = rv_html && |</thead>\n|.

  ENDMETHOD.


  METHOD render_table_lines.

    FIELD-SYMBOLS: <lt_table> TYPE INDEX TABLE.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    rv_html = |<tbody>\n|.

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_line>).

      rv_html = rv_html && |<tr>\n|.

      LOOP AT mo_struct_descr->get_components( ) ASSIGNING FIELD-SYMBOL(<ls_component>)
        WHERE name <> 'SKIP'.

        ASSIGN COMPONENT <ls_component>-name
               OF STRUCTURE <ls_line>
               TO FIELD-SYMBOL(<lv_field>).
        ASSERT sy-subrc = 0.

        IF <ls_component>-name CS |URL|.
          rv_html = rv_html && |<td class="tg-xldj">|
                            && |<a href="{ <lv_field> }">Repo</a></td>\n|.
        ELSEIF <ls_component>-type->get_ddic_header( )-refname CS |STATUS|.
          rv_html = rv_html && |<td class="tg-xldj { get_css_class_for_status( <lv_field> ) }|
                            && | { get_css_class_for_keys( <ls_component>-name ) }">|
                            && |{ get_value_for_status( <lv_field> ) }</td>\n|.
        ELSEIF <ls_component>-name = |MESSAGE|.
          rv_html = rv_html && |<td class="tg-xldj">|
                            && |{ replace_url_with_link( <lv_field> ) }</td>\n|.
        ELSE.
          rv_html = rv_html && |<td class="tg-xldj { get_css_class_for_keys( <ls_component>-name ) }">|
                            && |{ <lv_field> }</td>\n|.
        ENDIF.

      ENDLOOP.

      rv_html = rv_html && |</tr>\n|.

    ENDLOOP.

    rv_html = rv_html && |</tbody>\n|.

  ENDMETHOD.

  METHOD get_css_class_for_keys.

    IF iv_name = |NAME| OR iv_name = |PACKAGE| OR iv_name = |DESCRIPTION|.
      rv_css_class = 'key'.
    ELSEIF iv_name = |STATUS|.
      rv_css_class = 'total'.
    ENDIF.

  ENDMETHOD.

  METHOD get_css_class_for_status.

    rv_css_class = SWITCH #(
                     |{ iv_status }|
                       WHEN zif_abapgit_ci_definitions=>co_status-ok THEN |status ok|
                       WHEN zif_abapgit_ci_definitions=>co_status-not_ok THEN |status not_ok|
                       WHEN zif_abapgit_ci_definitions=>co_status-undefined THEN |status undefined|
                       WHEN zif_abapgit_ci_definitions=>co_status-skipped THEN |skipped|
                       ELSE || ).

  ENDMETHOD.

  METHOD get_value_for_status.

    rv_value = SWITCH #(
                 |{ iv_status }|
                   WHEN zif_abapgit_ci_definitions=>co_status-ok THEN |&#10003;|      "check mark
                   WHEN zif_abapgit_ci_definitions=>co_status-not_ok THEN |&#10007;|  "cross mark
                   WHEN zif_abapgit_ci_definitions=>co_status-skipped THEN |&#8677;|  "arrow right
                   ELSE |{ iv_status }| ).

  ENDMETHOD.

  METHOD replace_url_with_link.

    rv_value = replace( val   = iv_field
                        regex = '(http.*/)(\d*)'
                        with  = '<a href="$1$2">#$2</a>'
                        occ   = 0 ).

  ENDMETHOD.

ENDCLASS.
