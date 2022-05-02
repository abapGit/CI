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

      get_css_class_for_field
        IMPORTING
          iv_name             TYPE csequence
        RETURNING
          VALUE(rv_css_class) TYPE string,

      get_css_class_for_status
        IMPORTING
          iv_status           TYPE any
        RETURNING
          VALUE(rv_css_class) TYPE string,

      get_message_text
        IMPORTING
          iv_field        TYPE any
        RETURNING
          VALUE(rv_value) TYPE string,

      get_value_for_status
        IMPORTING
          iv_status       TYPE csequence
        RETURNING
          VALUE(rv_value) TYPE string.

    DATA:
      mv_class        TYPE string,
      mt_excl_columns TYPE RANGE OF fieldname,
      mr_table        TYPE REF TO data,
      mo_table_descr  TYPE REF TO cl_abap_tabledescr,
      mo_struct_descr TYPE REF TO cl_abap_structdescr.

ENDCLASS.
