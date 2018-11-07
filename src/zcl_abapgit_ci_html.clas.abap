CLASS zcl_abapgit_ci_html DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          is_result TYPE zif_abapgit_ci_definitions=>ty_result,

      render
        RETURNING
          VALUE(rv_html) TYPE string
        RAISING
          zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA:
      ms_result TYPE zif_abapgit_ci_definitions=>ty_result.

    METHODS:
      render_head
        RETURNING
          VALUE(rv_html) TYPE string
        RAISING
          zcx_abapgit_exception,

      render_style
        RETURNING
          VALUE(rv_html) TYPE string,

      render_table
        RETURNING
          VALUE(rv_html) TYPE string,

      render_table_head
        RETURNING
          VALUE(rv_html) TYPE string,

      render_table_lines
        RETURNING
          VALUE(rv_html) TYPE string,

      get_css_class_for_status
        IMPORTING
          iv_status           TYPE zabapgit_ci_status
        RETURNING
          VALUE(rv_css_class) TYPE string.

ENDCLASS.



CLASS zcl_abapgit_ci_html IMPLEMENTATION.


  METHOD constructor.

    ms_result = is_result.

  ENDMETHOD.


  METHOD render.

    rv_html = |<html>|
           && |  <head>|
           && |    { render_style(  ) }|
           && |  </head>|
           && |  <body>|
           && |    { render_head( ) }|
           && |    { render_table( ) }|
           && |  </body>|
           && |</html>|.

  ENDMETHOD.


  METHOD render_style.

    rv_html = |<style type="text/css">|
           && |body \{ font-family:Arial, sans-serif; font-size:14px;\}|
           && |.tg  \{border-collapse:collapse;border-spacing:0;\}|
           && |.tg td\{font-family:Arial, sans-serif;font-size:14px;padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;border-color:black;\}|
           && |.tg th\{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;border-color:black;\}|
           && |.tg .tg-kiyi\{font-weight:bold;border-color:inherit;text-align:left\}|
           && |.tg .tg-xldj\{border-color:inherit;text-align:left\}|
           && |.ok \{ background-color: lightgreen \}|
           && |.not_ok \{ background-color: red \}|
           && |</style>|.

  ENDMETHOD.


  METHOD render_table.

    rv_html = |<table class="tg">|
           && |  { render_table_head( ) }|
           && |  { render_table_lines( ) }|
           && |</table>|.

  ENDMETHOD.


  METHOD render_table_head.

    rv_html = |<tr>|
           && |  <th class="tg-kiyi">Name</th>|
           && |  <th class="tg-kiyi">Clone url</th>|
           && |  <th class="tg-kiyi">Package</th>|
           && |  <th class="tg-kiyi">Create package</th>|
           && |  <th class="tg-kiyi">Clone</th>|
           && |  <th class="tg-kiyi">Pull</th>|
           && |  <th class="tg-kiyi">Syntax check</th>|
           && |  <th class="tg-kiyi">Purge </th>|
           && |  <th class="tg-kiyi">Status</th>|
           && |  <th class="tg-kiyi">Message</th>|
           && |</tr>|.

  ENDMETHOD.


  METHOD render_table_lines.

    LOOP AT ms_result-repo_result_list ASSIGNING FIELD-SYMBOL(<ls_line>).

      rv_html = rv_html
             && |<tr>|
             && |  <td class="tg-xldj">{ <ls_line>-name }</td>|
             && |  <td class="tg-xldj">{ <ls_line>-clone_url }</td>|
             && |  <td class="tg-xldj">{ <ls_line>-package }</td>|
             && |  <td class="tg-xldj { get_css_class_for_status( <ls_line>-create_package ) }">{ <ls_line>-create_package }</td>|
             && |  <td class="tg-xldj { get_css_class_for_status( <ls_line>-clone ) }">{ <ls_line>-clone }</td>|
             && |  <td class="tg-xldj { get_css_class_for_status( <ls_line>-pull ) }">{ <ls_line>-pull }</td>|
             && |  <td class="tg-xldj { get_css_class_for_status( <ls_line>-syntax_check ) }">{ <ls_line>-syntax_check }</td>|
             && |  <td class="tg-xldj { get_css_class_for_status( <ls_line>-purge ) }">{ <ls_line>-purge }</td>|
             && |  <td class="tg-xldj { get_css_class_for_status( <ls_line>-status ) }">{ <ls_line>-status }</td>|
             && |  <td class="tg-xldj">{ <ls_line>-message }</td>|
             && |</tr>|.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_css_class_for_status.

    rv_css_class = SWITCH #(
                     iv_status
                       WHEN zif_abapgit_ci_definitions=>co_status-ok THEN |ok|
                       WHEN zif_abapgit_ci_definitions=>co_status-not_ok THEN |not_ok|
                       ELSE || ).

  ENDMETHOD.

  METHOD render_head.

    DATA: lv_timezone TYPE timezone.

    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = lv_timezone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CONVERT TIME STAMP ms_result-timestamp
       TIME ZONE lv_timezone
       INTO DATE DATA(date) TIME DATA(time).

    rv_html = |<h2 class="{
                     COND #( WHEN ms_result-ci_has_errors = abap_true
                             THEN |not_ok|
                             ELSE |ok|
                       ) }">{
                     COND #( WHEN ms_result-ci_has_errors = abap_true
                             THEN |CI failed|
                             ELSE |CI successful| ) }</h2>|
           && |<h3>Date: { date DATE = USER } Time: { time TIME = USER }</h3>|.

  ENDMETHOD.

ENDCLASS.
