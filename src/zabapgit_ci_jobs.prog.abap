*&---------------------------------------------------------------------*
*& Report zabapgit_ci_jobs
*&---------------------------------------------------------------------*
*& Display abapGit CI jobs
*& See https://github.com/abapGit/CI
*&---------------------------------------------------------------------*
REPORT zabapgit_ci_jobs.

DATA gs_jobsel TYPE btcselect.

gs_jobsel-jobname   = 'ZABAPGIT_CI*'.
gs_jobsel-username  = '*'.
gs_jobsel-from_date = sy-datum - 1.
gs_jobsel-to_date   = sy-datum.
gs_jobsel-aborted   = abap_true.
gs_jobsel-finished  = abap_true.
gs_jobsel-ready     = abap_true.
gs_jobsel-running   = abap_true.
gs_jobsel-prelim    = abap_true.
gs_jobsel-schedul   = abap_true.

CALL FUNCTION 'BP_JOB_MAINTENANCE_SM37B'
  EXPORTING
    o_jobsel_params         = gs_jobsel
  EXCEPTIONS
    unknown_selection_error = 1
    OTHERS                  = 2.
IF sy-subrc <> 0.
  MESSAGE 'Selection error' TYPE 'E'.
ENDIF.
