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
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_ci_html IMPLEMENTATION.


  METHOD constructor.

    ms_result = is_result.

  ENDMETHOD.


  METHOD render.

    rv_html = |<!DOCTYPE html>\n|
           && |<html>\n|
           && |  <head>\n|
           && |    { render_style( ) }\n|
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
           && |    <h2>{ zif_abapgit_ci_definitions=>co_title_repos }</h2>\n|
           && |    { render_table( ms_result-repo_result_list ) }\n|
           && |    <br/><br/>\n|.
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
       INTO DATE DATA(date) TIME DATA(time).

    rv_html = |<h2 class="{
                     COND #( WHEN ms_result-ci_has_errors = abap_true
                             THEN |not_ok|
                             ELSE |ok|
                       ) }">{
                     COND #( WHEN ms_result-ci_has_errors = abap_true
                             THEN |CI Failed|
                             ELSE |CI Successful| ) }</h2>\n|
           && |<h3>Date: { date DATE = USER } |
           && |- Time: { time TIME = USER } { lv_timezone } |
           && |- Duration: { ms_result-statistics-duration_in_seconds } seconds</h3>\n|
           && |<h3>abapGit Version: { zif_abapgit_version=>c_abap_version }</h3>\n|
           && |<h3>Repo Links: \n|
           && |<a href='https://github.com/abapGit/abapGit'>abapGit</a> \| \n|
           && |<a href='https://github.com/abapGit/CI'>abapGit CI</a> \| \n|
           && |<a href='https://github.com/abapGit/ci.abapgit.org'>abapGit CI results</a></h3>\n|
           && |<h3>Test Cases: Total { ms_result-statistics-test_cases-total } \| |
           && |Successful { ms_result-statistics-test_cases-successful } \| |
           && |Failed { ms_result-statistics-test_cases-failed } \n|.

  ENDMETHOD.


  METHOD render_style.

    rv_html = |<style type="text/css">\n|
           && |body \{ font-family:Arial, sans-serif; font-size:14px;\}\n|
           && |.tg  \{border-collapse:collapse;border-spacing:0;\}\n|
           && |.tg td\{font-family:Arial, sans-serif;font-size:14px;padding:10px 5px;border-style:solid;|
           && |border-width:1px;overflow:hidden;word-break:normal;border-color:black;\}\n|
           && |.tg th\{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:10px 5px;|
           && |border-style:solid;border-width:1px;overflow:hidden;word-break:normal;border-color:black;|
           && |background-color:darkgray;\}\n|
           && |.tg .tg-kiyi\{font-weight:bold;border-color:inherit;text-align:left;\}\n|
           && |.tg .tg-xldj\{border-color:inherit;text-align:left;\}\n|
           && |td.status \{ width:80px; \}\n|
           && |.ok \{ background-color: lightgreen; \}\n|
           && |.not_ok \{ background-color: red; \}\n|
           && |.undefined \{ background-color: lightgray; \}\n|
           && |.key \{ background-color: lightblue; \}\n|
           && |</style>\n|.

  ENDMETHOD.


  METHOD render_table.

    rv_html = NEW lcl_table_renderer( it_table )->render( ).

  ENDMETHOD.
ENDCLASS.
