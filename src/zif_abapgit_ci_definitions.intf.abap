INTERFACE zif_abapgit_ci_definitions
  PUBLIC .


  TYPES:
    BEGIN OF ty_repo,
      name      TYPE string,
      clone_url TYPE string,
    END OF ty_repo,
    tty_repo TYPE STANDARD TABLE OF ty_repo
             WITH NON-UNIQUE DEFAULT KEY .

  TYPES:
    BEGIN OF ENUM ty_status STRUCTURE status BASE TYPE char6,
      undefined VALUE IS INITIAL,
      ok        VALUE 'OK',
      not_ok    VALUE 'NOT_OK',
    END OF ENUM ty_status STRUCTURE status .

  TYPES:
    BEGIN OF ty_result,
      name           TYPE string,
      clone_url      TYPE string,
      package        TYPE devclass,
      create_package TYPE ty_status,
      clone          TYPE ty_status,
      pull           TYPE ty_status,
      syntax_check   TYPE ty_status,
      purge          TYPE ty_status,
      status         TYPE ty_status,
      message        TYPE char255,
    END OF ty_result,
    tty_result TYPE STANDARD TABLE OF ty_result
               WITH NON-UNIQUE DEFAULT KEY .

ENDINTERFACE.
