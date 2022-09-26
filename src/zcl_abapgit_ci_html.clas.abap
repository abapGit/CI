CLASS zcl_abapgit_ci_html DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !is_result TYPE zif_abapgit_ci_definitions=>ty_result .
    METHODS render
      RETURNING
        VALUE(rv_html) TYPE string
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      ms_result TYPE zif_abapgit_ci_definitions=>ty_result.

    METHODS:
      get_id
        IMPORTING
          iv_value     TYPE string
        RETURNING
          VALUE(rv_id) TYPE string,

      render_style
        RETURNING
          VALUE(rv_html) TYPE string,

      render_head
        RETURNING
          VALUE(rv_html) TYPE string
        RAISING
          zcx_abapgit_exception,

      render_table
        IMPORTING
          it_table       TYPE INDEX TABLE
        RETURNING
          VALUE(rv_html) TYPE string
        RAISING
          zcx_abapgit_exception,

      render_table_by_category
        IMPORTING
          it_table       TYPE zif_abapgit_ci_definitions=>ty_result-repo_result_list
          iv_category    TYPE string
        RETURNING
          VALUE(rv_html) TYPE string
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_ci_html IMPLEMENTATION.


  METHOD constructor.

    ms_result = is_result.

  ENDMETHOD.


  METHOD get_id.

    rv_id = replace(
      val  = condense( to_lower( iv_value ) )
      sub  = ` `
      with = `_`
      occ  = 0 ).

  ENDMETHOD.


  METHOD render.

    rv_html = |<!DOCTYPE html>\n|
           && |<html>\n|
           && |  <head>\n|
           && |    { render_style( ) }\n|
           && |  <link rel="shortcut icon" type="image/x-icon" href="favicon.png" />\n|
           && |  <title>{ zif_abapgit_ci_definitions=>co_title }</title>\n|
           && |  </head>\n|
           && |  <body>\n|
           && |    <h1>{ zif_abapgit_ci_definitions=>co_title }</h1>\n|
           && |    { render_head( ) }\n|
           && |    <br/><br/>\n|.

    IF ms_result-generic_result_list IS NOT INITIAL.
      rv_html = rv_html
           && |    <h2>{ zif_abapgit_ci_definitions=>co_title_generic }</h2>\n|
           && |    { render_table( ms_result-generic_result_list ) }\n|
           && |    <br/><br/>\n|.
    ENDIF.

    IF ms_result-repo_result_list IS NOT INITIAL.
      rv_html = rv_html
           && |    <h2>{ zif_abapgit_ci_definitions=>co_title_repos }</h2>\n|.
      IF ms_result-category_list IS INITIAL.
        rv_html = rv_html
             && |    { render_table( ms_result-repo_result_list ) }\n|
             && |    <br/><br/>\n|.
      ELSE.
        rv_html = rv_html
             && |      Categories: |.
        LOOP AT ms_result-category_list INTO DATA(lv_category).
          rv_html = rv_html && |<a href="#{ get_id( lv_category ) }">{ lv_category }</a>|.
          IF lines( ms_result-category_list ) <> sy-tabix.
            rv_html = rv_html && | \| \n|.
          ELSE.
            rv_html = rv_html && |\n|.
          ENDIF.
        ENDLOOP.
        rv_html = rv_html
             && |      <br/><br/>\n|.
        LOOP AT ms_result-category_list INTO lv_category.
          rv_html = rv_html
               && |      <h3 id="{ get_id( lv_category ) }">{ lv_category }</h3>\n|
               && |      { render_table_by_category(
                             it_table    = ms_result-repo_result_list
                             iv_category = lv_category ) }\n|
               && |      <br/><br/>\n|.
        ENDLOOP.
      ENDIF.
    ENDIF.

    rv_html = rv_html
         && |  </body>\n|
         && |</html>|.

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

    CONVERT TIME STAMP ms_result-statistics-finish_timestamp
       TIME ZONE lv_timezone
       INTO DATE DATA(lv_date) TIME DATA(lv_time).

    DATA(ls_release) = zcl_abapgit_factory=>get_environment( )->get_basis_release( ).

    rv_html = |<h2 class="{ COND #( WHEN ms_result-ci_has_errors = abap_true THEN |not_ok| ELSE |ok| ) }">|
           && |{ COND #( WHEN ms_result-ci_has_errors = abap_true THEN |CI Failed| ELSE |CI Successful| ) }</h2>\n|
           && |<h3>Date: { lv_date DATE = USER } |
           && |- Time: { lv_time TIME = USER } { lv_timezone } |
           && |- Duration: { ms_result-statistics-duration_in_seconds } seconds</h3>\n|
           && |<h3>abapGit Version: { zif_abapgit_version=>c_abap_version }</h3>\n|
           && |<h3>SAP Release: { ls_release-release } SP { ls_release-sp ALPHA = OUT }</h3>\n|
           && |<h3>Repo Links: \n|
           && |<a href='https://github.com/abapGit/abapGit'>abapGit</a> \| \n|
           && |<a href='https://github.com/abapGit/CI'>abapGit CI</a> \| \n|
           && |<a href='https://github.com/abapGit/ci.abapgit.org'>abapGit CI results</a></h3>\n|
           && |<h3>Test Cases: <span class="box key">Total: { ms_result-statistics-test_cases-total }</span> |
           && |<span class="box { COND #( WHEN ms_result-statistics-test_cases-successful > 0 THEN |ok| ) }">|
           && |Successful: { ms_result-statistics-test_cases-successful }</span> |
           && |<span class="box { COND #( WHEN ms_result-statistics-test_cases-skipped > 0 THEN |skipped| ) }">|
           && |Skipped: { ms_result-statistics-test_cases-skipped }</span> |
           && |<span class="box { COND #( WHEN ms_result-statistics-test_cases-failed > 0 THEN |not_ok| ) }">|
           && |Failed: { ms_result-statistics-test_cases-failed }</span>\n|.

  ENDMETHOD.


  METHOD render_style.

    rv_html = `<style type="text/css">`
      && `body {`
      && `  font-family: Arial,sans-serif;`
      && `  font-size: 14px;`
      && `}`
      && `.tg {`
      && `  border-collapse: collapse;`
      && `  border-spacing: 0;`
      && `  width: 100%;`
      && `}`
      && `.tg td {`
      && `  font-family: Arial,sans-serif;`
      && `  font-size: 14px;`
      && `  padding: 10px 5px;`
      && `  border-style: solid;`
      && `  border-width: 1px;`
      && `  overflow: hidden;`
      && `  word-break: normal;`
      && `  border-color: black;`
      && `}`
      && `.tg th {`
      && `  font-family: Arial,sans-serif;`
      && `  font-size: 14px;`
      && `  font-weight: normal;`
      && `  padding: 10px 5px;`
      && `  border-style: solid;`
      && `  border-width: 1px;`
      && `  overflow: hidden;`
      && `  word-break: normal;`
      && `  border-color: black;`
      && `  background-color: darkgray;`
      && `}`
      && `.tg .header {`
      && `  font-weight: bold;`
      && `  border-color: inherit;`
      && `  text-align: left;`
      && `}`
      && `.tg .row {`
      && `  border-color: inherit;`
      && `  text-align: left;`
      && `}`
      && `.status {`
      && `  width: 65px;`
      && `}`
      && `.box {`
      && `  border-style: solid;`
      && `  border-width: 1px;`
      && `  border-radius: 3px;`
      && `  padding: 3px;`
      && `  margin-right: 5px;`
      && `}`
      && `.ok {`
      && `  background-color: #2dc937;`
      && `}`
      && `.not_ok {`
      && `  background-color: #cc3232;`
      && `}`
      && `.undefined {`
      && `  background-color: lightgray;`
      && `}`
      && `.skipped {`
      && `  background-color: #e7b416;`
      && `}`
      && `.key {`
      && `  background-color: lightblue;`
      && `}`
      && `.repo_name {`
      && `  width: 170px;`
      && `}`
      && `.repo_pack {`
      && `  width: 100px;`
      && `}`
      && `.total {`
      && `  font-weight: bold;`
      && `  width: 65px;`
      && `}`
      && `.layer {`
      && `  width: 65px;`
      && `}`
      && `.duration {`
      && `  width: 65px;`
      && `  text-align: right;`
      && `}`
      && `.url {`
      && `  width: 45px;`
      && `}`
      && `</style>`.

    REPLACE ALL OCCURRENCES OF `>` IN rv_html WITH |>\n|.
    REPLACE ALL OCCURRENCES OF `{` IN rv_html WITH |\{\n|.
    REPLACE ALL OCCURRENCES OF `}` IN rv_html WITH |\}\n|.
    REPLACE ALL OCCURRENCES OF `;` IN rv_html WITH |;\n|.

  ENDMETHOD.


  METHOD render_table.

    rv_html = NEW lcl_table_renderer( it_table )->render( ).

  ENDMETHOD.


  METHOD render_table_by_category.

    DATA lt_table LIKE it_table.

    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<ls_row>) WHERE category = iv_category.
      APPEND <ls_row> TO lt_table.
    ENDLOOP.

    rv_html = NEW lcl_table_renderer( lt_table )->render( ).

  ENDMETHOD.
ENDCLASS.
