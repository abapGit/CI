*&---------------------------------------------------------------------*
*& Report zabapgit_ci_submit_job_queue
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapgit_ci_job_scheduler.

CLASS controller DEFINITION.

  PUBLIC SECTION.
    METHODS: start
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF co_job_name,
        update_abapgit    TYPE btcjob VALUE 'ABAPGIT CI: UPDATE ABAPGIT',
        update_abapgit_ci TYPE btcjob VALUE 'ABAPGIT CI: UPDATE ABAPGIT CI',
        run_abapgit_ci    TYPE btcjob VALUE 'ABAPGIT CI: RUN ABAPGIT CI',
      END OF co_job_name.

    METHODS:
      schedule_update_abapgit
        RETURNING
          VALUE(rv_jobcount) TYPE tbtcjob-jobcount
        RAISING
          zcx_abapgit_exception,

      schedule_job_with_predecessor
        IMPORTING
          iv_pred_jobcount       TYPE tbtcjob-jobcount
          iv_new_jobname         TYPE btcjob
          iv_report              TYPE sy-repid
          iv_pred_jobname        TYPE tbtcjob-jobname
          iv_variant             TYPE raldb-variant OPTIONAL
        RETURNING
          VALUE(rv_new_jobcount) TYPE tbtcjob-jobcount
        RAISING
          zcx_abapgit_exception.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    DATA(jobcount_update_abapgit) = schedule_update_abapgit( ).

    schedule_job_with_predecessor(
      EXPORTING
        iv_pred_jobcount = jobcount_update_abapgit
        iv_pred_jobname  = co_job_name-update_abapgit
        iv_new_jobname   = co_job_name-update_abapgit_ci
        iv_report        = |ZABAPGIT_CI_UPDATE_ABAPGIT_CI|
        iv_variant       = |BACKGROUND|
      RECEIVING
        rv_new_jobcount  = DATA(jobcount_update_abapgit_ci) ).

    schedule_job_with_predecessor(
      EXPORTING
        iv_pred_jobcount = jobcount_update_abapgit_ci
        iv_pred_jobname  = co_job_name-update_abapgit_ci
        iv_new_jobname   = co_job_name-run_abapgit_ci
        iv_report        = |ZABAPGIT_CI|
        iv_variant       = |BACKGROUND| ).

  ENDMETHOD.


  METHOD schedule_update_abapgit.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = co_job_name-update_abapgit
      IMPORTING
        jobcount         = rv_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'JOB_SUBMIT'
      EXPORTING
        authcknam               = sy-uname
        jobcount                = rv_jobcount
        jobname                 = co_job_name-update_abapgit
        report                  = 'ZABAPGIT_CI_UPDATE_ABAPGIT'
        variant                 = 'BACKGROUND'
      EXCEPTIONS
        bad_priparams           = 1
        bad_xpgflags            = 2
        invalid_jobdata         = 3
        jobname_missing         = 4
        job_notex               = 5
        job_submit_failed       = 6
        lock_failed             = 7
        program_missing         = 8
        prog_abap_and_extpg_set = 9
        OTHERS                  = 10.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = rv_jobcount
        jobname              = co_job_name-update_abapgit
        strtimmed            = abap_true
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        invalid_target       = 8
        invalid_time_zone    = 9
        OTHERS               = 10.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD schedule_job_with_predecessor.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = iv_new_jobname
      IMPORTING
        jobcount         = rv_new_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'JOB_SUBMIT'
      EXPORTING
        authcknam               = sy-uname
        jobcount                = rv_new_jobcount
        jobname                 = iv_new_jobname
        report                  = iv_report
        variant                 = iv_variant
      EXCEPTIONS
        bad_priparams           = 1
        bad_xpgflags            = 2
        invalid_jobdata         = 3
        jobname_missing         = 4
        job_notex               = 5
        job_submit_failed       = 6
        lock_failed             = 7
        program_missing         = 8
        prog_abap_and_extpg_set = 9
        OTHERS                  = 10.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = rv_new_jobcount
        jobname              = iv_new_jobname
        pred_jobname         = iv_pred_jobname
        pred_jobcount        = iv_pred_jobcount
        predjob_checkstat    = abap_true
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        invalid_target       = 8
        invalid_time_zone    = 9
        OTHERS               = 10.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      NEW controller( )->start( ).
      MESSAGE |Job scheduled successfully| TYPE 'S'.

    CATCH zcx_abapgit_exception INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
