*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section



CLASS lcl_mock_ui_functions DEFINITION.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_gui_functions.

ENDCLASS.

CLASS lcl_abapgit_popup_provider DEFINITION.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_popups.

    METHODS:
      set_sha1
        IMPORTING
          iv_sha1 TYPE zif_abapgit_definitions=>ty_sha1,
      set_url
        IMPORTING
          iv_url TYPE string,
      set_package
        IMPORTING
          iv_package TYPE devclass,
      set_popup_to_confirm_answer
        IMPORTING
          iv_answer TYPE char01.

  PRIVATE SECTION.
    DATA:
      mv_sha1                   TYPE zif_abapgit_definitions=>ty_sha1,
      m_package                 TYPE devclass,
      m_url                     TYPE string,
      m_popup_to_confirm_answer TYPE char01.

ENDCLASS.
