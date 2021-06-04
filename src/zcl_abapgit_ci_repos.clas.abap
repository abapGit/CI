CLASS zcl_abapgit_ci_repos DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      update_abapgit_repo
        RAISING
          zcx_abapgit_exception,

      update_abapgit_ci_repo
        RAISING
          zcx_abapgit_exception.

    METHODS:
      process_repos
        IMPORTING
          it_repos              TYPE zif_abapgit_ci_definitions=>tty_repo
          is_options            TYPE zif_abapgit_ci_definitions=>ty_repo_check_options
        RETURNING
          VALUE(rt_result_list) TYPE zif_abapgit_ci_definitions=>ty_result-repo_result_list.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
      update_repo
        IMPORTING
          iv_repo_name TYPE string
        RAISING
          zcx_abapgit_exception,

      syntax_check
        IMPORTING
          iv_package TYPE devclass
        RAISING
          zcx_abapgit_exception.

    METHODS:
      process_repo
        CHANGING
          cs_ci_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,
      get_repo_list_with_packages
        IMPORTING
          it_repos        TYPE zif_abapgit_ci_definitions=>tty_repo
          is_options      TYPE zif_abapgit_ci_definitions=>ty_repo_check_options
        RETURNING
          VALUE(rt_repos) TYPE zif_abapgit_ci_definitions=>tty_repo_result_list.

ENDCLASS.



CLASS zcl_abapgit_ci_repos IMPLEMENTATION.


  METHOD get_repo_list_with_packages.
    FIELD-SYMBOLS: <ls_ci_repo> TYPE zabapgit_ci_result.

    " Copy it_repos into rt_repos, creating an entry for local and transportable packages each

    DATA(lo_skip) = NEW zcl_abapgit_ci_skip( ).

    LOOP AT it_repos ASSIGNING FIELD-SYMBOL(<ls_repo>).
      IF is_options-check_local = abap_true.
        INSERT CORRESPONDING #( <ls_repo> )
               INTO TABLE rt_repos
               ASSIGNING <ls_ci_repo>.
        <ls_ci_repo>-package = CONV devclass( |$___{ to_upper( <ls_ci_repo>-name ) }| ).
        <ls_ci_repo>-do_not_purge = is_options-no_purge.

        lo_skip->complete_skip_components( CHANGING cs_repo = <ls_ci_repo> ).
      ENDIF.

      IF is_options-check_transportable = abap_true.
        INSERT CORRESPONDING #( <ls_repo> )
               INTO TABLE rt_repos
               ASSIGNING <ls_ci_repo>.
        <ls_ci_repo>-package = CONV devclass( |Z___{ to_upper( <ls_ci_repo>-name ) }| ).
        <ls_ci_repo>-layer = is_options-layer.
        <ls_ci_repo>-do_not_purge = is_options-no_purge.

        lo_skip->complete_skip_components( CHANGING cs_repo = <ls_ci_repo> ).
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD process_repo.
    DATA: lv_message TYPE c LENGTH 255.

    " You should remember that we process the repo in synchron RFC because of
    " shortdumps there doesn't crash the main process.

    ASSERT cs_ci_repo-package IS NOT INITIAL.

    CALL FUNCTION 'ZABAPGIT_CI_PROCESS_REPO'
      DESTINATION 'NONE'
      CHANGING
        cs_ci_repo            = cs_ci_repo
      EXCEPTIONS
        communication_failure = 1 MESSAGE lv_message
        system_failure        = 2 MESSAGE lv_message
        OTHERS                = 3.

    IF sy-subrc <> 0.
      cs_ci_repo-message = |Failure in ZABAPGIT_CI_PROCESS_REPO. Subrc = { sy-subrc } { lv_message }|.
      cs_ci_repo-status  = zif_abapgit_ci_definitions=>co_status-not_ok.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD process_repos.

    DATA:
      lv_start_timestamp TYPE timestampl,
      lv_finish_timestamp TYPE timestampl.

    rt_result_list = get_repo_list_with_packages( it_repos = it_repos is_options = is_options ).

    LOOP AT rt_result_list ASSIGNING FIELD-SYMBOL(<ls_ci_repo>).
      IF <ls_ci_repo>-skip = abap_true.
        CONTINUE.
      ENDIF.

      GET TIME STAMP FIELD lv_start_timestamp.

      TRY.
          process_repo(
            CHANGING
              cs_ci_repo = <ls_ci_repo> ).

        CATCH zcx_abapgit_exception INTO DATA(lx_error).
          <ls_ci_repo>-status  = zif_abapgit_ci_definitions=>co_status-not_ok.
          <ls_ci_repo>-message = lx_error->get_text( ).
      ENDTRY.

      IF <ls_ci_repo>-create_package = zif_abapgit_ci_definitions=>co_status-not_ok
      OR <ls_ci_repo>-clone          = zif_abapgit_ci_definitions=>co_status-not_ok
      OR <ls_ci_repo>-pull           = zif_abapgit_ci_definitions=>co_status-not_ok
      OR <ls_ci_repo>-syntax_check   = zif_abapgit_ci_definitions=>co_status-not_ok
      OR <ls_ci_repo>-purge          = zif_abapgit_ci_definitions=>co_status-not_ok
      OR <ls_ci_repo>-status         = zif_abapgit_ci_definitions=>co_status-not_ok
      OR ( <ls_ci_repo>-layer IS NOT INITIAL AND
           ( <ls_ci_repo>-check_create_transport = zif_abapgit_ci_definitions=>co_status-not_ok OR
             <ls_ci_repo>-check_delete_transport = zif_abapgit_ci_definitions=>co_status-not_ok ) ).

        <ls_ci_repo>-status = zif_abapgit_ci_definitions=>co_status-not_ok.

      ELSE.

        <ls_ci_repo>-status = zif_abapgit_ci_definitions=>co_status-ok.

      ENDIF.

      GET TIME STAMP FIELD lv_finish_timestamp.

      <ls_ci_repo>-duration = cl_abap_timestamp_util=>get_instance( )->tstmpl_seconds_between(
        iv_timestamp0 = lv_start_timestamp
        iv_timestamp1 = lv_finish_timestamp ).

    ENDLOOP.

  ENDMETHOD.


  METHOD syntax_check.

    DATA(lt_list) = zcl_abapgit_factory=>get_code_inspector( iv_package )->run( 'SYNTAX_CHECK' ).

    ASSIGN lt_list[ kind = 'E' ] TO FIELD-SYMBOL(<ls_error>).
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( |Syntax error in repo { iv_package } |
                                 && |object { <ls_error>-objtype } { <ls_error>-objname } { <ls_error>-text } |
                                 && | [ @{ CONV i( <ls_error>-line ) } ]| ).
    ENDIF.

  ENDMETHOD.


  METHOD update_abapgit_ci_repo.

    update_repo( 'CI' ).

  ENDMETHOD.


  METHOD update_abapgit_repo.

    update_repo( 'abapGit' ).

  ENDMETHOD.


  METHOD update_repo.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_online.

    DATA(lt_repo_list) = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repo_list ASSIGNING FIELD-SYMBOL(<repo>).

      IF <repo>->get_name( ) = iv_repo_name.
        lo_repo ?= <repo>.
        EXIT.
      ENDIF.

    ENDLOOP.

    IF lo_repo IS NOT BOUND.
      zcx_abapgit_exception=>raise( |Couldn't find { iv_repo_name } repo| ).
    ENDIF.

    lo_repo->select_branch( 'refs/heads/main' ).

    TRY.
        DATA(ls_checks) = zcl_abapgit_ci_repo_check=>get( lo_repo ).
      CATCH zcx_abapgit_cancel INTO DATA(lx_cancel).
        zcx_abapgit_exception=>raise(
          iv_text     = lx_cancel->get_text( )
          ix_previous = lx_cancel ).
    ENDTRY.

    lo_repo->deserialize(
      is_checks = ls_checks
      ii_log    = NEW zcl_abapgit_log( ) ).

    syntax_check( lo_repo->get_package( ) ).

  ENDMETHOD.
ENDCLASS.
