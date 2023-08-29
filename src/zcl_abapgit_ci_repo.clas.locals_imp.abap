CLASS lcl_mock_frontend_services IMPLEMENTATION.

* Disable GUI for CI tests

  METHOD zif_abapgit_frontend_services~clipboard_export.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~directory_browse.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~directory_create.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~directory_exist.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~execute.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~file_download.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~file_upload.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~get_file_separator.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~get_gui_version.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~get_system_directory.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~gui_is_available.
    rv_gui_is_available = abap_false.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~is_sapgui_for_java.
    rv_result = abap_false.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~is_sapgui_for_windows.
    rv_result = abap_false.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~is_webgui.
    rv_is_webgui = abap_false.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~open_ie_devtools.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~show_file_open_dialog.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~show_file_save_dialog.
    RETURN.
  ENDMETHOD.

ENDCLASS.
