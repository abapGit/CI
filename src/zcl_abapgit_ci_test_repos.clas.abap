CLASS zcl_abapgit_ci_test_repos DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_ci_repo_provider.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:
      fetch_repo_page
        IMPORTING
          VALUE(iv_page_count) TYPE i
        RETURNING
          VALUE(rt_repos)      TYPE zif_abapgit_ci_definitions=>tty_repo
        RAISING
          zcx_abapgit_exception,

      do_we_have_an_ads_connection
        RETURNING
          VALUE(rv_is_ads_on) TYPE abap_bool.

ENDCLASS.



CLASS ZCL_ABAPGIT_CI_TEST_REPOS IMPLEMENTATION.


  METHOD do_we_have_an_ads_connection.

    SELECT SINGLE FROM fpconnect
           FIELDS destination
           INTO @DATA(lv_destination).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'RFC_READ_DESTINATION_TYPE'
      EXPORTING
        destination             = lv_destination
        authority_check         = abap_false
        bypass_buf              = abap_false
      EXCEPTIONS
        authority_not_available = 1
        destination_not_exist   = 2
        information_failure     = 3
        internal_failure        = 4
        OTHERS                  = 5.

    IF sy-subrc = 0.
      rv_is_ads_on = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD fetch_repo_page.

    DATA: li_http_client TYPE REF TO if_http_client,
          lv_rfcdes      TYPE rfcdes-rfcdest.

    lv_rfcdes = |API_GITHUB_{ sy-uname }|.

    SELECT SINGLE FROM rfcdes
           FIELDS rfcdest
           WHERE rfcdest = @lv_rfcdes
           INTO @lv_rfcdes.

    IF sy-subrc = 0.

      cl_http_client=>create_by_destination(
        EXPORTING
          destination              = lv_rfcdes
        IMPORTING
          client                   = li_http_client
        EXCEPTIONS
          argument_not_found       = 1
          destination_not_found    = 2
          destination_no_authority = 3
          plugin_not_active        = 4
          internal_error           = 5
          OTHERS                   = 6 ).

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ELSE.

      cl_http_client=>create_by_url(
        EXPORTING
          url                = 'https://api.github.com'
          ssl_id             = 'ANONYM'
        IMPORTING
          client             = li_http_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDIF.

    DATA(lo_rest_client) = NEW cl_rest_http_client( li_http_client ).

    lo_rest_client->if_rest_client~create_request_entity( )->set_header_field(
        iv_name  = '~request_uri'
        iv_value = |/orgs/abapGit-tests/repos?page={ iv_page_count }| ).

    lo_rest_client->if_rest_client~get( ).

    DATA(lo_response) = lo_rest_client->if_rest_client~get_response_entity( ).

    DATA(lv_status) = lo_rest_client->if_rest_client~get_status( ).

    IF lv_status <> cl_rest_status_code=>gc_success_ok.
      zcx_abapgit_exception=>raise( |HTTP status code { lv_status } from api.github.com| ).
    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lo_response->get_string_data( )
      CHANGING
        data = rt_repos ).

  ENDMETHOD.


  METHOD zif_abapgit_ci_repo_provider~get_repos.

    DATA: lt_repos TYPE zif_abapgit_ci_definitions=>tty_repo.

    DO.

      TRY.
          lt_repos = fetch_repo_page( iv_page_count = sy-index ).

        CATCH zcx_abapgit_exception cx_rest_client_exception INTO DATA(lx_error).
          zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                        ix_previous = lx_error ).
      ENDTRY.

      IF lines( lt_repos ) = 0.
        EXIT.
      ENDIF.

      INSERT LINES OF lt_repos INTO TABLE rt_repos.

    ENDDO.

    LOOP AT rt_repos ASSIGNING FIELD-SYMBOL(<ls_repo>).

      IF <ls_repo>-name = |CUS0|.
        <ls_repo>-skip        = abap_true.
        <ls_repo>-skip_reason =  |skip because UI is called|.
      ENDIF.

      IF <ls_repo>-name = |FDT0|.
        <ls_repo>-skip        = abap_true.
        <ls_repo>-skip_reason =  |https://github.com/larshp/abapGit/pull/2688|.
      ENDIF.

      IF <ls_repo>-name CS |SPRX|.
        <ls_repo>-skip        = abap_true.
        <ls_repo>-skip_reason = |https://github.com/larshp/abapGit/issues/87|.
      ENDIF.

      IF <ls_repo>-name = |SFSW|.
        <ls_repo>-skip        = abap_true.
        <ls_repo>-skip_reason = |https://github.com/larshp/abapGit/issues/2083|.
      ENDIF.

      IF <ls_repo>-name = |DDLX_old|.
        <ls_repo>-skip        = abap_true.
        <ls_repo>-skip_reason = |Skip because it's an old testcase. |
                             && |abapGit indicates diff because migration to new format|.
      ENDIF.

      IF <ls_repo>-name = |DEVC_component|.
        <ls_repo>-skip        = abap_true.
        <ls_repo>-skip_reason = |https://github.com/larshp/abapGit/issues/1880|.
      ENDIF.

      IF <ls_repo>-name = |SQSC|
      AND cl_db_sys=>is_in_memory_db = abap_false.
        <ls_repo>-skip        = abap_true.
        <ls_repo>-skip_reason = |Runs only on HANA|.
      ENDIF.

      IF <ls_repo>-name = |SOTS|.
        <ls_repo>-skip        = abap_true.
        <ls_repo>-skip_reason = |Cannot be installed in local $-package|.
      ENDIF.

      IF <ls_repo>-name = |SFPF|
      AND do_we_have_an_ads_connection( ) = abap_false.
        <ls_repo>-skip        = abap_true.
        <ls_repo>-skip_reason = |Adobe Document Service (ADS) connection neccessary|.
      ENDIF.

      IF <ls_repo>-name = |TTYP_with_CLAS_reference|.
        <ls_repo>-skip        = abap_true.
        <ls_repo>-skip_reason = |circular dependency https://github.com/larshp/abapGit/issues/2338|.
      ENDIF.

      IF <ls_repo>-name = |SHLP_with_exit|.
        <ls_repo>-skip        = abap_true.
        <ls_repo>-skip_reason = |circular dependency https://github.com/larshp/abapGit/issues/2338|.
      ENDIF.

      IF <ls_repo>-name = |PROG_ci_variant|.
        <ls_repo>-skip        = abap_true.
        <ls_repo>-skip_reason = |activation error https://github.com/larshp/abapGit/issues/2338|.
      ENDIF.

      IF <ls_repo>-name = |DOMA_fixed_values_single_translated|.
        <ls_repo>-skip        = abap_true.
        <ls_repo>-skip_reason = |https://github.com/larshp/abapGit/issues/2385|.
      ENDIF.

      IF <ls_repo>-name = |SFBF|
      OR <ls_repo>-name = |SFBS|.
        <ls_repo>-skip        = abap_true.
        <ls_repo>-skip_reason = |https://github.com/larshp/abapGit/issues/2469|.
      ENDIF.

    ENDLOOP.

    SORT rt_repos BY name.

  ENDMETHOD.
ENDCLASS.
