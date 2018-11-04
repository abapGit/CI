*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_mock_ui_functions IMPLEMENTATION.

  METHOD zif_abapgit_gui_functions~gui_is_available.

    rv_gui_is_available = abap_false.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_abapgit_popup_provider IMPLEMENTATION.

  METHOD set_sha1.

    mv_sha1 = iv_sha1.

  ENDMETHOD.


  METHOD set_url.

    m_url = iv_url.

  ENDMETHOD.

  METHOD set_package.

    m_package = iv_package.

  ENDMETHOD.

  METHOD zif_abapgit_popups~repo_popup.

    rs_popup-package     = m_package.
    rs_popup-url         = m_url.
    rs_popup-branch_name = iv_branch.

  ENDMETHOD.


  METHOD set_popup_to_confirm_answer.

    m_popup_to_confirm_answer = iv_answer.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_confirm.

    rv_answer = m_popup_to_confirm_answer.

  ENDMETHOD.

  METHOD zif_abapgit_popups~branch_list_popup.

  ENDMETHOD.

  METHOD zif_abapgit_popups~branch_popup_callback.

  ENDMETHOD.

  METHOD zif_abapgit_popups~create_branch_popup.

  ENDMETHOD.

  METHOD zif_abapgit_popups~package_popup_callback.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_folder_logic.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_object.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_package_export.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_create_package.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_create_transp_branch.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_inform.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_select_from_list.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_select_transports.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_transport_request.

  ENDMETHOD.

  METHOD zif_abapgit_popups~repo_new_offline.

  ENDMETHOD.

  METHOD zif_abapgit_popups~run_page_class_popup.

  ENDMETHOD.

ENDCLASS.
