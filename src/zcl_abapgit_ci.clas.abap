CLASS zcl_abapgit_ci DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor,

      process_repos
        IMPORTING
          !it_repos         TYPE zif_abapgit_ci_definitions=>tty_repo
        RETURNING
          VALUE(rt_ci_repo) TYPE zif_abapgit_ci_definitions=>tty_result.

  PRIVATE SECTION.
    METHODS:
      process_repo
        CHANGING
          cs_ci_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception.

ENDCLASS.

CLASS zcl_abapgit_ci IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

  ENDMETHOD.

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

    LOOP AT it_repos ASSIGNING FIELD-SYMBOL(<ls_repo>).

      INSERT CORRESPONDING #( <ls_repo> )
             INTO TABLE rt_ci_repo
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

  ENDMETHOD.

ENDCLASS.
