FUNCTION ZABAPGIT_CI_PROCESS_REPO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     VALUE(CS_CI_REPO) TYPE  ZABAPGIT_CI_RESULT
*"----------------------------------------------------------------------
  TRY.
      NEW zcl_abapgit_ci_repo( )->run(
        CHANGING
          cs_ri_repo = cs_ci_repo ).

    CATCH zcx_abapgit_exception INTO DATA(lx_error).
      cs_ci_repo-message = lx_error->get_text( ).
      cs_ci_repo-status = zif_abapgit_ci_definitions=>co_status-not_ok.
  ENDTRY.


ENDFUNCTION.
