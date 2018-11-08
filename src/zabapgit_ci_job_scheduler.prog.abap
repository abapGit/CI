*&---------------------------------------------------------------------*
*& Report zabapgit_ci_submit_job_queue
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapgit_ci_job_scheduler.

TABLES: sscrfields.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-000.
SELECTION-SCREEN COMMENT:
   1(79) comm1,
  /1(79) comm2,
  /1(79) comm3,
  /1(79) comm4.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT:
  /1(79) comm5.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) cvar1.
PARAMETERS: p_var1 TYPE raldb-variant.
SELECTION-SCREEN PUSHBUTTON 50(25) but1 USER-COMMAND cli1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) cvar2.
PARAMETERS:  p_var2 TYPE raldb-variant.
SELECTION-SCREEN PUSHBUTTON 50(25) but2 USER-COMMAND cli2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) cvar3.
PARAMETERS: p_var3 TYPE raldb-variant.
SELECTION-SCREEN PUSHBUTTON 50(25) but3 USER-COMMAND cli3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

CLASS controller DEFINITION.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF co_report,
        update_abapgit    TYPE sy-repid VALUE 'ZABAPGIT_CI_UPDATE_ABAPGIT',
        update_abapgit_ci TYPE sy-repid VALUE 'ZABAPGIT_CI_UPDATE_ABAPGIT_CI',
        run_abapgit_ci    TYPE sy-repid VALUE 'ZABAPGIT_CI',
      END OF co_report.

    CLASS-METHODS:
      program_variant_f4
        IMPORTING
          iv_program TYPE sy-repid
        CHANGING
          cv_variant TYPE raldb-variant,

      at_selection_screen.

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
        iv_report        = co_report-update_abapgit_ci
        iv_variant       = p_var2
      RECEIVING
        rv_new_jobcount  = DATA(jobcount_update_abapgit_ci) ).

    schedule_job_with_predecessor(
      EXPORTING
        iv_pred_jobcount = jobcount_update_abapgit_ci
        iv_pred_jobname  = co_job_name-update_abapgit_ci
        iv_new_jobname   = co_job_name-run_abapgit_ci
        iv_report        = co_report-run_abapgit_ci
        iv_variant       = p_var3 ).

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
        report                  = co_report-update_abapgit
        variant                 = p_var1
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


  METHOD program_variant_f4.

    CALL FUNCTION 'RS_VARIANT_CATALOG'
      EXPORTING
        report               = iv_program
      IMPORTING
        sel_variant          = cv_variant
      EXCEPTIONS
        no_report            = 1
        report_not_existent  = 2
        report_not_supplied  = 3
        no_variants          = 4
        no_variant_selected  = 5
        variant_not_existent = 6
        OTHERS               = 7.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
              TYPE 'S'
              NUMBER sy-msgno
              DISPLAY LIKE sy-msgty
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD at_selection_screen.

    CASE sscrfields-ucomm.
      WHEN 'CLI1'.

        CALL FUNCTION 'RS_VARIANT_SCREEN'
          EXPORTING
            report  = co_report-update_abapgit
            variant = p_var1
          IMPORTING
            variant = p_var1.

      WHEN 'CLI2'.

        CALL FUNCTION 'RS_VARIANT_SCREEN'
          EXPORTING
            report  = co_report-update_abapgit_ci
            variant = p_var2
          IMPORTING
            variant = p_var2.

      WHEN 'CLI3'.

        CALL FUNCTION 'RS_VARIANT_SCREEN'
          EXPORTING
            report  = co_report-run_abapgit_ci
            variant = p_var3
          IMPORTING
            variant = p_var3.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var1.
  controller=>program_variant_f4(
    EXPORTING
      iv_program = controller=>co_report-update_abapgit
    CHANGING
      cv_variant = p_var1 ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var2.
  controller=>program_variant_f4(
    EXPORTING
      iv_program = controller=>co_report-update_abapgit_ci
    CHANGING
      cv_variant = p_var2 ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var3.
  controller=>program_variant_f4(
    EXPORTING
      iv_program = controller=>co_report-run_abapgit_ci
    CHANGING
      cv_variant = p_var3 ).

AT SELECTION-SCREEN.
  controller=>at_selection_screen( ).

INITIALIZATION.
  comm1 = TEXT-c01.
  comm2 = TEXT-c02.
  comm3 = TEXT-c03.
  comm4 = TEXT-c04.
  comm5 = TEXT-c05.

  cvar1 = TEXT-v01.
  cvar2 = TEXT-v02.
  cvar3 = TEXT-v03.

  but1 = |{ icon_change } { TEXT-b01 }|.
  but2 = |{ icon_change } { TEXT-b01 }|.
  but3 = |{ icon_change } { TEXT-b01 }|.

START-OF-SELECTION.
  TRY.
      NEW controller( )->start( ).
      MESSAGE |Job scheduled successfully| TYPE 'S'.

    CATCH zcx_abapgit_exception INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
