CLASS zcl_abapgit_ci DEFINITION
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
          it_repos         TYPE zif_abapgit_ci_definitions=>tty_repo
        RETURNING
          VALUE(rs_result) TYPE zif_abapgit_ci_definitions=>ty_result.

  PRIVATE SECTION.
    METHODS:
      process_repo
        CHANGING
          cs_ci_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception.

ENDCLASS.

CLASS zcl_abapgit_ci IMPLEMENTATION.

  METHOD process_repo.

    " You should remember that we process the repo in synchron RFC because of
    " shortdumps there doesn't crash the main process.

    cs_ci_repo-package = CONV devclass( |$___{ to_upper( cs_ci_repo-name ) }| ).

    CALL FUNCTION 'ZABAPGIT_CI_PROCESS_REPO'
      DESTINATION 'NONE'
      CHANGING
        cs_ci_repo            = cs_ci_repo
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.

    IF sy-subrc <> 0.
      cs_ci_repo-message = |Failure in ZABAPGIT_CI_PROCESS_REPO. Subrc = { sy-subrc } |.
      cs_ci_repo-status  = zif_abapgit_ci_definitions=>co_status-not_ok.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD process_repos.

    GET TIME STAMP FIELD rs_result-timestamp.

    LOOP AT it_repos ASSIGNING FIELD-SYMBOL(<ls_repo>).

      INSERT CORRESPONDING #( <ls_repo> )
             INTO TABLE rs_result-list
             ASSIGNING FIELD-SYMBOL(<ls_ci_repo>).

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
      OR <ls_ci_repo>-status         = zif_abapgit_ci_definitions=>co_status-not_ok.

        <ls_ci_repo>-status = zif_abapgit_ci_definitions=>co_status-not_ok.

      ELSE.

        <ls_ci_repo>-status = zif_abapgit_ci_definitions=>co_status-ok.

      ENDIF.

    ENDLOOP.

    rs_result-ci_has_errors = boolc(
                                line_exists(
                                  rs_result-list[ status = zif_abapgit_ci_definitions=>co_status-not_ok ] ) ).

  ENDMETHOD.


  METHOD update_abapgit_repo.

    DATA: lo_abapgit TYPE REF TO zcl_abapgit_repo_online.

    DATA(lt_list) = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<repo>).

      IF <repo>->get_name( ) = 'abapGit'.
        lo_abapgit ?= <repo>.
      ENDIF.

    ENDLOOP .

    IF lo_abapgit IS NOT BOUND.
      zcx_abapgit_exception=>raise( |Couldn't find abapGit repo| ).
    ENDIF.

    lo_abapgit->set_branch_name( 'refs/heads/master' ).

    DATA(ls_checks) = lo_abapgit->deserialize_checks( ).

    LOOP AT ls_checks-overwrite ASSIGNING FIELD-SYMBOL(<ls_overwrite>).
      <ls_overwrite>-decision = abap_true.
    ENDLOOP.

    lo_abapgit->deserialize( ls_checks ).

  ENDMETHOD.

  METHOD update_abapgit_ci_repo.

    DATA: lo_abapgit_ci TYPE REF TO zcl_abapgit_repo_online.

    DATA(lt_list) = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<repo>).

      IF <repo>->get_name( ) = 'CI'.
        lo_abapgit_ci ?= <repo>.
      ENDIF.

    ENDLOOP .

    IF lo_abapgit_ci IS NOT BOUND.
      zcx_abapgit_exception=>raise( |Couldn't find abapGit CI repo| ).
    ENDIF.

    lo_abapgit_ci->set_branch_name( 'refs/heads/master' ).

    DATA(ls_checks) = lo_abapgit_ci->deserialize_checks( ).

    LOOP AT ls_checks-overwrite ASSIGNING FIELD-SYMBOL(<ls_overwrite>).
      <ls_overwrite>-decision = abap_true.
    ENDLOOP.

    lo_abapgit_ci->deserialize( ls_checks ).

  ENDMETHOD.

ENDCLASS.
