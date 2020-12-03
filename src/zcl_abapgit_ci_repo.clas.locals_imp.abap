*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_mock_ui_functions IMPLEMENTATION.

  METHOD zif_abapgit_gui_functions~gui_is_available.

    rv_gui_is_available = abap_false.

  ENDMETHOD.

  METHOD zif_abapgit_gui_functions~is_sapgui_for_java.

    rv_result = abap_false.

  ENDMETHOD.

  METHOD zif_abapgit_gui_functions~is_sapgui_for_windows.

    rv_result = abap_false.

  ENDMETHOD.

ENDCLASS.
