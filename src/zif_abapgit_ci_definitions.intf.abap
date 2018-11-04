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

  CONSTANTS:
    BEGIN OF co_status,
      undefined TYPE char6 VALUE ' ',
      ok        TYPE char6 VALUE 'OK',
      not_ok    TYPE char6 VALUE 'NOT_OK',
    END OF co_status.

  TYPES:
    tty_result TYPE STANDARD TABLE OF zabapgit_ci_result
               WITH NON-UNIQUE DEFAULT KEY .

ENDINTERFACE.
