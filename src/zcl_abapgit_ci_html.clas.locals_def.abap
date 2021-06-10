*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_table_renderer DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          it_table TYPE INDEX TABLE,

      render
        RETURNING
          VALUE(rv_html) TYPE string
        RAISING
          zcx_abapgit_exception.
  PRIVATE SECTION.
    METHODS:
      render_table_head
        RETURNING
          VALUE(rv_html) TYPE string,

      render_table_lines
        RETURNING
          VALUE(rv_html) TYPE string,

      get_css_class_for_keys
        IMPORTING
          iv_name             TYPE csequence
        RETURNING
          VALUE(rv_css_class) TYPE string,

      get_css_class_for_status
        IMPORTING
          iv_status           TYPE any
        RETURNING
          VALUE(rv_css_class) TYPE string.

    DATA:
      mr_table        TYPE REF TO data,
      mo_table_descr  TYPE REF TO cl_abap_tabledescr,
      mo_struct_descr TYPE REF TO cl_abap_structdescr.

ENDCLASS.
