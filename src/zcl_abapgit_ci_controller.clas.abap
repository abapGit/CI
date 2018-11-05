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
      mo_ci            TYPE REF TO zcl_abapgit_ci,
      ms_options       TYPE zif_abapgit_ci_definitions=>ty_options.

    METHODS:
      post_errors_to_slack
        IMPORTING
          is_result TYPE zif_abapgit_ci_definitions=>ty_result
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_ci_controller IMPLEMENTATION.


  METHOD constructor.

    mi_view          = ii_view.
    mi_repo_provider = ii_repo_provider.
    ms_options       = is_options.

    mo_ci = NEW zcl_abapgit_ci( ).

  ENDMETHOD.


  METHOD run.

    DATA(lt_repos)  = mi_repo_provider->get_repos( ).
    DATA(ls_result) = mo_ci->process_repos( lt_repos ).

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

    CONSTANTS: co_url TYPE string VALUE `https://christianguenter2.github.io/abapGit_CI_results/src/08002743b1381ed8b8a29429c189dd50.smim.abapgit_ci_result.html`.

    DATA(lv_error_text) = REDUCE string( INIT result = ||
                                         FOR line IN is_result-list
                                         WHERE ( status = zif_abapgit_ci_definitions=>co_status-not_ok )
                                         NEXT result = result && |\nRepo: { line-name } Message: { line-message }\n| ).

    NEW zcl_abapgit_ci_slack( ms_options-slack_oauth_token )->post( |*abapGit CI errors:*\n { lv_error_text } \n|
                                                                 && |Details: { co_url } | ).

  ENDMETHOD.

ENDCLASS.
