INTERFACE zif_abapgit_ci_definitions
  PUBLIC .


  TYPES:
    BEGIN OF ty_repo,
      name      TYPE string,
      clone_url TYPE string,
    END OF ty_repo .
  TYPES:
    tty_repo TYPE STANDARD TABLE OF ty_repo
             WITH NON-UNIQUE DEFAULT KEY .
  TYPES:
    tty_repo_result_list    TYPE STANDARD TABLE OF zabapgit_ci_result
                                 WITH NON-UNIQUE DEFAULT KEY .
  TYPES:
    tty_generic_result_list TYPE STANDARD TABLE OF zabapgit_ci_result_gen
                                 WITH NON-UNIQUE DEFAULT KEY .
  TYPES:
    BEGIN OF ty_result,
      ci_has_errors       TYPE abap_bool,
      BEGIN OF statistics,
        finish_timestamp    TYPE timestampl,
        duration_in_seconds TYPE i,
        BEGIN OF test_cases,
          total      TYPE i,
          successful TYPE i,
          skipped    TYPE i,
          failed     TYPE i,
        END OF test_cases,
      END OF statistics,
      repo_result_list    TYPE tty_repo_result_list,
      generic_result_list TYPE tty_generic_result_list,
    END OF ty_result .
  TYPES:
    BEGIN OF ty_repo_check_options,
      check_local         TYPE abap_bool,
      check_transportable TYPE abap_bool,
      layer               TYPE devlayer,
      no_purge            TYPE abap_bool,
    END OF ty_repo_check_options .
  TYPES:
    BEGIN OF ty_options,
      result_git_repo_url    TYPE string,
      save_to_history        TYPE abap_bool,
      post_errors_to_slack   TYPE abap_bool,
      slack_oauth_token      TYPE string,
      exec_generic_checks    TYPE abap_bool,
      exec_repository_checks TYPE abap_bool,
      repo_check_options     TYPE ty_repo_check_options,
      sync_processing        TYPE abap_bool,
    END OF ty_options .

  CONSTANTS co_title TYPE string VALUE 'abapGit CI Results' ##NO_TEXT.
  CONSTANTS co_title_generic TYPE string VALUE 'abapGit CI - Generic Tests' ##NO_TEXT.
  CONSTANTS co_title_repos TYPE string VALUE 'abapGit CI - Repository Tests' ##NO_TEXT.

  CONSTANTS:
    BEGIN OF co_status,
      undefined TYPE char6 VALUE ' ',
      skipped   TYPE char6 VALUE 'X',
      ok        TYPE char6 VALUE 'OK',
      not_ok    TYPE char6 VALUE 'NOT_OK',
    END OF co_status .
ENDINTERFACE.
