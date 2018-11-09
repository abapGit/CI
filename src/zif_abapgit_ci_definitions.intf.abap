INTERFACE zif_abapgit_ci_definitions
  PUBLIC .


  TYPES:
    BEGIN OF ty_repo,
      name      TYPE string,
      clone_url TYPE string,
    END OF ty_repo,
    tty_repo TYPE STANDARD TABLE OF ty_repo
             WITH NON-UNIQUE DEFAULT KEY .

  TYPES: ty_status TYPE char6.

  TYPES:
    tty_repo_result_list    TYPE STANDARD TABLE OF zabapgit_ci_result
                                 WITH NON-UNIQUE DEFAULT KEY,
    tty_generic_result_list TYPE STANDARD TABLE OF zabapgit_ci_result_gen
                                 WITH NON-UNIQUE DEFAULT KEY,
    BEGIN OF ty_result,
      ci_has_errors       TYPE abap_bool,
      BEGIN OF statistics,
        BEGIN OF test_cases,
          total      TYPE i,
          successful TYPE i,
          failed     TYPE i,
        END OF test_cases,
      END OF statistics,
      repo_result_list    TYPE tty_repo_result_list,
      generic_result_list TYPE tty_generic_result_list,
    END OF ty_result.

  TYPES:
    BEGIN OF ty_options,
      result_git_repo_url  TYPE string,
      post_errors_to_slack TYPE abap_bool,
      slack_oauth_token    TYPE string,
    END OF ty_options.

  CONSTANTS:
    BEGIN OF co_status,
      undefined TYPE char6 VALUE ' ',
      ok        TYPE char6 VALUE 'OK',
      not_ok    TYPE char6 VALUE 'NOT_OK',
    END OF co_status.

ENDINTERFACE.
