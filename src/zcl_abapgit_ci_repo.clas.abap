CLASS zcl_abapgit_ci_repo DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor,

      run
        CHANGING
          cs_ri_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception.

  PRIVATE SECTION.
    METHODS:
      create_package
        CHANGING
          cs_ri_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      clone
        CHANGING
          cs_ri_repo     TYPE zabapgit_ci_result
        RETURNING
          VALUE(ro_repo) TYPE REF TO zcl_abapgit_repo_online
        RAISING
          zcx_abapgit_exception,

      pull
        IMPORTING
          io_repo    TYPE REF TO zcl_abapgit_repo_online
        CHANGING
          cs_ri_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      purge
        IMPORTING
          io_repo    TYPE REF TO zcl_abapgit_repo_online
        CHANGING
          cs_ri_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,

      syntax_check
        CHANGING
          cs_ri_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception.

    DATA:
      mo_popup_provider TYPE REF TO lcl_abapgit_popup_provider.

ENDCLASS.


CLASS zcl_abapgit_ci_repo IMPLEMENTATION.

  METHOD syntax_check.

    cs_ri_repo-syntax_check = zif_abapgit_ci_definitions=>co_status-not_ok.

    DATA(li_syntax_check) = zcl_abapgit_factory=>get_syntax_check( cs_ri_repo-package ).

    DATA(lt_list) = li_syntax_check->run( ).

    LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<ls_list>)
                    WHERE kind = 'E'.
    ENDLOOP.
    IF sy-subrc = 0.
      cs_ri_repo-syntax_check = zif_abapgit_ci_definitions=>co_status-not_ok.
    ELSE.
      cs_ri_repo-syntax_check = zif_abapgit_ci_definitions=>co_status-ok.
    ENDIF.

  ENDMETHOD.

  METHOD purge.

    CHECK io_repo IS BOUND.

    cs_ri_repo-purge = zif_abapgit_ci_definitions=>co_status-not_ok.

    mo_popup_provider->set_popup_to_confirm_answer( '1' ).

    TRY.
        zcl_abapgit_services_repo=>purge( io_repo->get_key( ) ).
      CATCH zcx_abapgit_cancel INTO DATA(error).
        zcx_abapgit_exception=>raise( error->get_text( ) ).
    ENDTRY.

    cs_ri_repo-purge = zif_abapgit_ci_definitions=>co_status-ok.

    CALL FUNCTION 'DEQUEUE_ALL'
      EXPORTING
        _synchron = abap_true.

  ENDMETHOD.

  METHOD pull.

    cs_ri_repo-pull = zif_abapgit_ci_definitions=>co_status-not_ok.

    DATA(ls_checks) = io_repo->deserialize_checks( ).

    LOOP AT ls_checks-overwrite ASSIGNING FIELD-SYMBOL(<ls_checks>).
      <ls_checks>-decision = abap_true.
    ENDLOOP.

    LOOP AT ls_checks-warning_package ASSIGNING FIELD-SYMBOL(<ls_warning_package>).
      <ls_warning_package>-decision = abap_true.
    ENDLOOP.

    io_repo->deserialize( ls_checks ).

    cs_ri_repo-pull = zif_abapgit_ci_definitions=>co_status-ok.

  ENDMETHOD.

  METHOD constructor.

    CREATE OBJECT mo_popup_provider.
    zcl_abapgit_ui_injector=>set_popups( mo_popup_provider ).

    zcl_abapgit_ui_injector=>set_gui_functions( NEW lcl_mock_ui_functions( ) ).

  ENDMETHOD.

  METHOD clone.

    cs_ri_repo-clone = zif_abapgit_ci_definitions=>co_status-not_ok.

    mo_popup_provider->set_url( cs_ri_repo-clone_url ).
    mo_popup_provider->set_package( cs_ri_repo-package ).

    TRY.
        ro_repo = zcl_abapgit_services_repo=>new_online( cs_ri_repo-clone_url ).
      CATCH zcx_abapgit_cancel INTO DATA(error).
        zcx_abapgit_exception=>raise( error->get_text( ) ).
    ENDTRY.

    cs_ri_repo-clone = zif_abapgit_ci_definitions=>co_status-ok.

  ENDMETHOD.

  METHOD create_package.

    DATA(li_package) = zcl_abapgit_factory=>get_sap_package( cs_ri_repo-package ).

    IF li_package->exists( ) = abap_false.

      cs_ri_repo-create_package = zif_abapgit_ci_definitions=>co_status-not_ok.

      li_package->create( VALUE #(
                            as4user  = sy-uname
                            devclass = cs_ri_repo-package
                            ctext    = |abapGit CI run|
                          ) ).

      cs_ri_repo-create_package = zif_abapgit_ci_definitions=>co_status-ok.

    ENDIF.

  ENDMETHOD.

  METHOD run.

    create_package( CHANGING cs_ri_repo = cs_ri_repo ).

    TRY.
        DATA(lo_repo) = clone( CHANGING cs_ri_repo = cs_ri_repo ).

        pull( EXPORTING io_repo  = lo_repo
              CHANGING cs_ri_repo = cs_ri_repo ).

        syntax_check( CHANGING cs_ri_repo = cs_ri_repo ).

      CATCH zcx_abapgit_exception INTO DATA(lx_error).

        " ensure uninstall
        purge( EXPORTING io_repo   = lo_repo
               CHANGING  cs_ri_repo = cs_ri_repo ).

        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).

    ENDTRY.

    purge( EXPORTING io_repo   = lo_repo
           CHANGING  cs_ri_repo = cs_ri_repo ).

  ENDMETHOD.

ENDCLASS.
