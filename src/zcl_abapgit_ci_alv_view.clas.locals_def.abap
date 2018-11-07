*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_alv DEFINITION ABSTRACT.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          io_container TYPE REF TO cl_gui_container
          it_table     TYPE INDEX TABLE,

      display
        RAISING
          zcx_abapgit_exception.

  PROTECTED SECTION.
    METHODS:
      before_display ABSTRACT
        RAISING
          cx_salv_error,

      config_column
        IMPORTING
          iv_column TYPE lvc_fname
          iv_text   TYPE csequence
          iv_width  TYPE lvc_outlen
        RAISING
          cx_salv_error.

    DATA:
      mo_alv TYPE REF TO cl_salv_table.

  PRIVATE SECTION.
    METHODS:
      get_table_descr
        IMPORTING
          it_table              TYPE INDEX TABLE
        RETURNING
          VALUE(ro_table_descr) TYPE REF TO cl_abap_tabledescr,

      map
        IMPORTING
          it_table TYPE ANY TABLE
          ir_table TYPE REF TO data.

    DATA:
      mr_table     TYPE REF TO data,
      mo_container TYPE REF TO cl_gui_container.

ENDCLASS.

CLASS lcl_repo_result_list_alv DEFINITION INHERITING FROM lcl_alv.

  PROTECTED SECTION.
    METHODS:
      before_display REDEFINITION.

ENDCLASS.

CLASS lcl_generic_result_list_alv DEFINITION INHERITING FROM lcl_alv.

  PROTECTED SECTION.
    METHODS:
      before_display REDEFINITION.

ENDCLASS.
