CLASS lcl_table_renderer IMPLEMENTATION.

  METHOD constructor.

    mr_table = REF #( it_table ).
    mo_table_descr ?= cl_abap_tabledescr=>describe_by_data( it_table ).
    mo_struct_descr ?= mo_table_descr->get_table_line_type( ).
    mt_excl_columns = VALUE #( ( sign = 'I' option = 'EQ' low = 'SKIP' )
                               ( sign = 'I' option = 'EQ' low = 'CREATE_PACKAGE' )
                               ( sign = 'I' option = 'EQ' low = 'DO_NOT_PURGE' )
                               ( sign = 'I' option = 'EQ' low = 'CATEGORY' )
                               ( sign = 'I' option = 'EQ' low = 'LOGGING' ) ).

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
      WHERE name NOT IN mt_excl_columns.

      rv_html = rv_html
            && |<th class="header">|
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
        WHERE name NOT IN mt_excl_columns.

        ASSIGN COMPONENT <ls_component>-name
               OF STRUCTURE <ls_line>
               TO FIELD-SYMBOL(<lv_field>).
        ASSERT sy-subrc = 0.

        IF <ls_component>-name CS |URL|.
          rv_html = rv_html && |<td class="row url">|
                            && |<a href="{ <lv_field> }">Repo</a></td>\n|.
        ELSEIF <ls_component>-type->get_ddic_header( )-refname CS |STATUS|.
          rv_html = rv_html && |<td class="row { get_css_class_for_status( <lv_field> ) }|
                            && | { get_css_class_for_field( <ls_component>-name ) }">|
                            && |{ get_value_for_status( <lv_field> ) }</td>\n|.
        ELSEIF <ls_component>-name = |MESSAGE|.
          rv_html = rv_html && |<td class="row">|
                            && |{ get_message_text( <lv_field> ) }</td>\n|.
        ELSEIF <ls_component>-name = |PACKAGE|.
          rv_html = rv_html && |<td class="row { get_css_class_for_field( <ls_component>-name ) }">|
                            && |{ get_package_text( <lv_field> ) }</td>\n|.
        ELSE.
          rv_html = rv_html && |<td class="row { get_css_class_for_field( <ls_component>-name ) }">|
                            && |{ <lv_field> }</td>\n|.
        ENDIF.

      ENDLOOP.

      rv_html = rv_html && |</tr>\n|.

    ENDLOOP.

    rv_html = rv_html && |</tbody>\n|.

  ENDMETHOD.

  METHOD get_css_class_for_field.

    CASE iv_name.
      WHEN 'NAME'.
        rv_css_class = 'key repo_name'.
      WHEN 'PACKAGE'.
        rv_css_class = 'key repo_pack'.
      WHEN 'DESCRIPTION'.
        rv_css_class = 'key'.
      WHEN 'STATUS'.
        rv_css_class = 'total'.
      WHEN 'LAYER'.
        rv_css_class = 'layer'.
      WHEN 'DURATION'.
        rv_css_class = 'duration'.
    ENDCASE.

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
                   WHEN zif_abapgit_ci_definitions=>co_status-ok THEN |&check;|               "check mark
                   WHEN zif_abapgit_ci_definitions=>co_status-not_ok THEN |&cross;|           "cross mark
                   WHEN zif_abapgit_ci_definitions=>co_status-skipped THEN |&RightTeeArrow;|  "arrow right
                   ELSE |{ iv_status }| ).

  ENDMETHOD.

  METHOD get_message_text.

    rv_value = replace( val   = iv_field
                        regex = '(http.*/)(\d*)'
                        with  = '<a href="$1$2">#$2</a>'
                        occ   = 0 ) ##REGEX_POSIX.

    rv_value = replace( val   = rv_value
                        regex = '\n'
                        with  = '<br/>'
                        occ   = 0 ) ##REGEX_POSIX.

  ENDMETHOD.

  METHOD get_package_text.

    rv_value = SWITCH #(
                 |{ iv_package(1) }|
                   WHEN '$' THEN |<span title="{ iv_package }">Local ($)</span>|
                   WHEN 'Z' THEN |<span title="{ iv_package }">Transport (Z)</span>|
                   ELSE |{ iv_package }| ).

  ENDMETHOD.

ENDCLASS.
