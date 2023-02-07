FUNCTION zabapgit_ci_process_repo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     VALUE(CS_CI_REPO) TYPE  ZABAPGIT_CI_RESULT
*"----------------------------------------------------------------------

  TRY.

      NEW zcl_abapgit_ci_repo( )->run( CHANGING cs_ci_repo = cs_ci_repo ).

    CATCH zcx_abapgit_exception INTO DATA(lx_error).

      zcl_abapgit_ci_repos=>fail_message(
        EXPORTING
          iv_message = lx_error->get_text( )
        CHANGING
          cs_ci_repo = cs_ci_repo ).

  ENDTRY.

ENDFUNCTION.
