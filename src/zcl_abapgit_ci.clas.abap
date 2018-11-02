CLASS zcl_abapgit_ci DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
    METHODS process_repos
      IMPORTING
        !it_repos        TYPE zif_abapgit_ci_definitions=>tty_repo
      RETURNING
        VALUE(rt_result) TYPE zif_abapgit_ci_definitions=>tty_result .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mo_popup_provider TYPE REF TO lcl_abapgit_popup_provider.

    METHODS:
      process_repo
        IMPORTING
          is_repo   TYPE zif_abapgit_ci_definitions=>ty_repo
        CHANGING
          cs_output TYPE zif_abapgit_ci_definitions=>ty_result
        RAISING
          zcx_abapgit_exception,

      create_package
        CHANGING
          cs_output TYPE zif_abapgit_ci_definitions=>ty_result
        RAISING
          zcx_abapgit_exception,

      clone
        IMPORTING
          is_repo        TYPE zif_abapgit_ci_definitions=>ty_repo
        CHANGING
          cs_output      TYPE zif_abapgit_ci_definitions=>ty_result
        RETURNING
          VALUE(ro_repo) TYPE REF TO zcl_abapgit_repo_online
        RAISING
          zcx_abapgit_exception,

      pull
        IMPORTING
          io_repo   TYPE REF TO zcl_abapgit_repo_online
        CHANGING
          cs_output TYPE zif_abapgit_ci_definitions=>ty_result
        RAISING
          zcx_abapgit_exception,

      syntax_check
        CHANGING
          cs_output TYPE zif_abapgit_ci_definitions=>ty_result
        RAISING
          zcx_abapgit_exception,

      purge
        IMPORTING
          io_repo   TYPE REF TO zcl_abapgit_repo_online
        CHANGING
          cs_output TYPE zif_abapgit_ci_definitions=>ty_result
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_ci IMPLEMENTATION.


  METHOD clone.

    cs_output-clone = zif_abapgit_ci_definitions=>status-not_ok.

    mo_popup_provider->set_url( is_repo-clone_url ).
    mo_popup_provider->set_package( cs_output-package ).

    TRY.
        ro_repo = zcl_abapgit_services_repo=>new_online( is_repo-clone_url ).
      CATCH zcx_abapgit_cancel INTO DATA(error).
        zcx_abapgit_exception=>raise( error->get_text( ) ).
    ENDTRY.

    cs_output-clone = zif_abapgit_ci_definitions=>status-ok.

  ENDMETHOD.


  METHOD constructor.

    CREATE OBJECT mo_popup_provider.
    zcl_abapgit_ui_injector=>set_popups( mo_popup_provider ).

    zcl_abapgit_ui_injector=>set_gui_functions( NEW lcl_mock_ui_functions( ) ).

  ENDMETHOD.


  METHOD create_package.

    DATA(li_package) = zcl_abapgit_factory=>get_sap_package( cs_output-package ).

    IF li_package->exists( ) = abap_false.

      cs_output-create_package = zif_abapgit_ci_definitions=>status-not_ok.

      li_package->create( VALUE #(
                            as4user  = sy-uname
                            devclass = cs_output-package
                            ctext    = |abapGit CI run|
                          ) ).

      cs_output-create_package = zif_abapgit_ci_definitions=>status-ok.

    ENDIF.

  ENDMETHOD.


  METHOD process_repo.

    cs_output-package = CONV devclass( |$___{ to_upper( is_repo-name ) }| ).

    create_package( CHANGING cs_output = cs_output ).

    TRY.
        DATA(lo_repo) = clone( EXPORTING is_repo  = is_repo
                               CHANGING cs_output = cs_output ).

        pull( EXPORTING io_repo  = lo_repo
              CHANGING cs_output = cs_output ).

        syntax_check( CHANGING cs_output = cs_output ).

      CATCH zcx_abapgit_exception INTO DATA(lx_error).

        " ensure uninstall
        purge( EXPORTING io_repo   = lo_repo
               CHANGING  cs_output = cs_output ).

        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).

    ENDTRY.

    purge( EXPORTING io_repo   = lo_repo
           CHANGING  cs_output = cs_output ).

  ENDMETHOD.


  METHOD process_repos.

    LOOP AT it_repos ASSIGNING FIELD-SYMBOL(<ls_repo>).

      INSERT CORRESPONDING #( <ls_repo> )
             INTO TABLE rt_result
             ASSIGNING FIELD-SYMBOL(<ls_output>).

      TRY.
          process_repo(
            EXPORTING
              is_repo   = <ls_repo>
            CHANGING
              cs_output = <ls_output> ).

        CATCH zcx_abapgit_exception INTO DATA(lx_error).
          <ls_output>-status  = zif_abapgit_ci_definitions=>status-not_ok.
          <ls_output>-message = lx_error->get_text( ).
      ENDTRY.

      IF <ls_output>-create_package = zif_abapgit_ci_definitions=>status-not_ok
      OR <ls_output>-clone          = zif_abapgit_ci_definitions=>status-not_ok
      OR <ls_output>-pull           = zif_abapgit_ci_definitions=>status-not_ok
      OR <ls_output>-syntax_check   = zif_abapgit_ci_definitions=>status-not_ok
      OR <ls_output>-purge          = zif_abapgit_ci_definitions=>status-not_ok
      OR <ls_output>-status         = zif_abapgit_ci_definitions=>status-not_ok.

        <ls_output>-status = zif_abapgit_ci_definitions=>status-not_ok.

      ELSE.

        <ls_output>-status = zif_abapgit_ci_definitions=>status-ok.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD pull.

    cs_output-pull = zif_abapgit_ci_definitions=>status-not_ok.

    DATA(ls_checks) = io_repo->deserialize_checks( ).

    LOOP AT ls_checks-overwrite ASSIGNING FIELD-SYMBOL(<ls_checks>).
      <ls_checks>-decision = abap_true.
    ENDLOOP.

    LOOP AT ls_checks-warning_package ASSIGNING FIELD-SYMBOL(<ls_warning_package>).
      <ls_warning_package>-decision = abap_true.
    ENDLOOP.

    io_repo->deserialize( ls_checks ).

    cs_output-pull = zif_abapgit_ci_definitions=>status-ok.

  ENDMETHOD.


  METHOD purge.

    CHECK io_repo IS BOUND.

    cs_output-purge = zif_abapgit_ci_definitions=>status-not_ok.

    CALL FUNCTION 'DEQUEUE_ALL'
      EXPORTING
        _synchron = abap_true.

    mo_popup_provider->set_popup_to_confirm_answer( '1' ).

    TRY.
        zcl_abapgit_services_repo=>purge( io_repo->get_key( ) ).
      CATCH zcx_abapgit_cancel INTO DATA(error).
        zcx_abapgit_exception=>raise( error->get_text( ) ).
    ENDTRY.

    cs_output-purge = zif_abapgit_ci_definitions=>status-ok.

  ENDMETHOD.


  METHOD syntax_check.

    cs_output-syntax_check = zif_abapgit_ci_definitions=>status-not_ok.

    DATA(li_syntax_check) = zcl_abapgit_factory=>get_syntax_check( cs_output-package ).

    DATA(lt_list) = li_syntax_check->run( ).

    LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<ls_list>)
                    WHERE kind = 'E'.
    ENDLOOP.
    IF sy-subrc = 0.
      cs_output-syntax_check = zif_abapgit_ci_definitions=>status-not_ok.
    ELSE.
      cs_output-syntax_check = zif_abapgit_ci_definitions=>status-ok.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
