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



CLASS ZCL_ABAPGIT_CI_HTML IMPLEMENTATION.


  METHOD constructor.

    ms_result = is_result.

  ENDMETHOD.


  METHOD render.

    rv_html = |<!DOCTYPE html>\n|
           && |<html>\n|
           && |  <head>\n|
           && |    { render_style(  ) }\n|
           && |  <title>ci.abapgit.org</title>\n|
           && |  </head>\n|
           && |  <body>\n|
           && |    { render_head( ) }\n|
           && |    { render_table( ms_result-generic_result_list ) }\n|
           && |    <br/><br/>\n|
           && |    { render_table( ms_result-repo_result_list ) }\n|
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
                             THEN |CI failed|
                             ELSE |CI successful| ) }</h2>|
           && |<h3>Date: { date DATE = USER } - Time: { time TIME = USER } { lv_timezone } - Duration { ms_result-statistics-duration_in_seconds } seconds</h3>|
           && |<h3>Repo links: |
           && |<a href='https://github.com/larshp/abapGit'>abapGit</a> \| |
           && |<a href='https://github.com/abapGit/CI'>abapGit CI</a> \| |
           && |<a href='https://github.com/abapGit/ci.abapgit.org'>abapGit CI results</a></h3>|
           && |<h3>Test cases: Total { ms_result-statistics-test_cases-total } \| Successful { ms_result-statistics-test_cases-successful } \| Failed { ms_result-statistics-test_cases-failed } |
           && |<br/><br/>|.

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

    rv_html = NEW lcl_table_renderer( it_table )->render( ).

  ENDMETHOD.
ENDCLASS.
