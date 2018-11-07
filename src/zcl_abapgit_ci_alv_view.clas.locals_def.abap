*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_view DEFINITION ABSTRACT.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          it_table TYPE INDEX TABLE,

      display ABSTRACT
        RAISING
          zcx_abapgit_exception.

  PROTECTED SECTION.
    TYPES: BEGIN OF ty_column_width,
             column TYPE lvc_fname,
             width  TYPE lvc_outlen,
           END OF ty_column_width,
           tty_column_width TYPE HASHED TABLE OF ty_column_width
                            WITH UNIQUE KEY column.

    DATA:
      mo_alv          TYPE REF TO cl_salv_table,
      mr_table        TYPE REF TO data,
      mt_column_width TYPE tty_column_width.

  PRIVATE SECTION.
    METHODS:
      map_status_to_icon
        IMPORTING
          it_table TYPE ANY TABLE
          ir_table TYPE REF TO data,

      config_column
        IMPORTING
          iv_column TYPE lvc_fname
          iv_width  TYPE lvc_outlen.

ENDCLASS.

CLASS lcl_alv DEFINITION INHERITING FROM lcl_view.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          it_table     TYPE INDEX TABLE
          io_container TYPE REF TO cl_gui_container,

      display REDEFINITION.

  PRIVATE SECTION.
    DATA:
      mo_container TYPE REF TO cl_gui_container.

ENDCLASS.


CLASS lcl_list DEFINITION INHERITING FROM lcl_view.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          it_table   TYPE INDEX TABLE
          iv_tabname TYPE slis_tabname,

      display REDEFINITION.

  PRIVATE SECTION.
    DATA mv_tabname TYPE slis_tabname.

ENDCLASS.
