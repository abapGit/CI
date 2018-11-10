CLASS zcl_abapgit_ci_controller DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          !ii_view          TYPE REF TO zif_abapgit_ci_view
          !ii_repo_provider TYPE REF TO zif_abapgit_ci_repo_provider
          is_options        TYPE zif_abapgit_ci_definitions=>ty_options OPTIONAL,

      run
        RAISING
          zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA:
      mi_view          TYPE REF TO zif_abapgit_ci_view,
      mi_repo_provider TYPE REF TO zif_abapgit_ci_repo_provider,
      mo_ci_repos      TYPE REF TO zcl_abapgit_ci_repos,
      ms_options       TYPE zif_abapgit_ci_definitions=>ty_options,
      mo_ci_generic    TYPE REF TO zcl_abapgit_ci_generic_tests.

    METHODS:
      post_errors_to_slack
        IMPORTING
          is_result TYPE zif_abapgit_ci_definitions=>ty_result
        RAISING
          zcx_abapgit_exception,

      calculate_statistics
        CHANGING
          cs_result TYPE zif_abapgit_ci_definitions=>ty_result,

      count_by_status
        IMPORTING
          is_result       TYPE zif_abapgit_ci_definitions=>ty_result
          iv_status       TYPE zabapgit_ci_status
        RETURNING
          VALUE(rv_count) TYPE i.

ENDCLASS.



CLASS zcl_abapgit_ci_controller IMPLEMENTATION.


  METHOD constructor.

    mi_view          = ii_view.
    mi_repo_provider = ii_repo_provider.
    ms_options       = is_options.

    mo_ci_repos = NEW zcl_abapgit_ci_repos( ).
    mo_ci_generic = NEW zcl_abapgit_ci_generic_tests( ).

  ENDMETHOD.


  METHOD run.

    DATA: ls_result          TYPE zif_abapgit_ci_definitions=>ty_result,
          lv_start_timestamp TYPE timestampl.

    GET TIME STAMP FIELD lv_start_timestamp.

    IF ms_options-exec_repository_checks = abap_true.
      DATA(lt_repos)  = mi_repo_provider->get_repos( ).
      ls_result-repo_result_list = mo_ci_repos->process_repos( lt_repos ).
    ENDIF.

    IF ms_options-exec_generic_checks = abap_true.
      ls_result-generic_result_list = mo_ci_generic->execute( ).
    ENDIF.

    ls_result-ci_has_errors = boolc( line_exists(
                                       ls_result-repo_result_list[ status = zif_abapgit_ci_definitions=>co_status-not_ok ] )
                                  OR line_exists(
                                       ls_result-generic_result_list[ status = zif_abapgit_ci_definitions=>co_status-not_ok ] ) ).

    GET TIME STAMP FIELD ls_result-statistics-finish_timestamp.

    ls_result-statistics-duration_in_seconds = cl_abap_timestamp_util=>get_instance( )->tstmpl_seconds_between(
                                                 iv_timestamp0 = lv_start_timestamp
                                                 iv_timestamp1 = ls_result-statistics-finish_timestamp ).

    calculate_statistics( CHANGING cs_result = ls_result ).

    IF ms_options-result_git_repo_url IS NOT INITIAL.
      NEW zcl_abapgit_ci_distributor( ms_options-result_git_repo_url  )->push_to_git_repo( is_result = ls_result ).
    ENDIF.

    IF ls_result-ci_has_errors = abap_true
    AND ms_options-post_errors_to_slack = abap_true.

      post_errors_to_slack( ls_result ).

    ENDIF.

    mi_view->display( CHANGING cs_result = ls_result ).

  ENDMETHOD.

  METHOD post_errors_to_slack.

    CONSTANTS: co_url TYPE string VALUE `https://ci.abapgit.org`.

    DATA(lv_error_text) = REDUCE string( INIT result = ||
                                         FOR line IN is_result-repo_result_list
                                         WHERE ( status = zif_abapgit_ci_definitions=>co_status-not_ok )
                                         NEXT result = result && |\nRepo: { line-name } Message: { line-message }\n| ).

    NEW zcl_abapgit_ci_slack( ms_options-slack_oauth_token )->post( |*abapGit CI errors:*\n { lv_error_text } \n|
                                                                 && |Details: { co_url } | ).

  ENDMETHOD.


  METHOD calculate_statistics.

    cs_result-statistics-test_cases-successful = count_by_status( is_result = cs_result
                                                                  iv_status = zif_abapgit_ci_definitions=>co_status-ok ).

    cs_result-statistics-test_cases-failed = count_by_status( is_result = cs_result
                                                              iv_status = zif_abapgit_ci_definitions=>co_status-not_ok ).

    cs_result-statistics-test_cases-total = cs_result-statistics-test_cases-successful + cs_result-statistics-test_cases-failed.

  ENDMETHOD.


  METHOD count_by_status.

    rv_count = REDUCE #( INIT result = 0
                         FOR gen_result IN is_result-generic_result_list
                         WHERE ( status = iv_status )
                         NEXT result = result + 1 )
             + REDUCE #( INIT result = 0
                         FOR repo_result IN is_result-repo_result_list
                         WHERE ( status = iv_status )
                         NEXT result = result + 1 ).

  ENDMETHOD.

ENDCLASS.
