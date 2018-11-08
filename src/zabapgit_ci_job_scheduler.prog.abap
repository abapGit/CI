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
    CLASS-METHODS:
      program_variant_f4
        IMPORTING
          iv_program TYPE sy-repid
        CHANGING
          cv_variant TYPE raldb-variant,

      at_selection_screen.

    METHODS:
      start
        RAISING
          zcx_abapgit_exception.

  PRIVATE SECTION.
    METHODS:
      call_job_overview.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    NEW zcl_abapgit_ci_job_scheduler( )->schedule_jobs( ).

    MESSAGE |Job scheduled successfully| TYPE 'S'.

    IF zcl_abapgit_ui_factory=>get_gui_functions( )->gui_is_available( ).
      call_job_overview( ).
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
            report  = zcl_abapgit_ci_job_scheduler=>co_report-update_abapgit
            variant = p_var1
          IMPORTING
            variant = p_var1.

      WHEN 'CLI2'.

        CALL FUNCTION 'RS_VARIANT_SCREEN'
          EXPORTING
            report  = zcl_abapgit_ci_job_scheduler=>co_report-update_abapgit_ci
            variant = p_var2
          IMPORTING
            variant = p_var2.

      WHEN 'CLI3'.

        CALL FUNCTION 'RS_VARIANT_SCREEN'
          EXPORTING
            report  = zcl_abapgit_ci_job_scheduler=>co_report-run_abapgit_ci
            variant = p_var3
          IMPORTING
            variant = p_var3.

    ENDCASE.

  ENDMETHOD.


  METHOD call_job_overview.

    DATA: ls_jobsel_params TYPE btcselect.

    ls_jobsel_params-abapname  = |*ZABAPGIT_CI*|.
    ls_jobsel_params-jobname   = |*ABAPGIT*CI*|.
    ls_jobsel_params-username  = sy-uname.
    ls_jobsel_params-from_date = sy-datum.
    ls_jobsel_params-to_date   = sy-datum + 1.
    ls_jobsel_params-from_time = sy-uzeit - 30.
    ls_jobsel_params-to_time   = |235959|.
    ls_jobsel_params-prelim    = abap_true.
    ls_jobsel_params-schedul   = abap_true.
    ls_jobsel_params-ready     = abap_true.
    ls_jobsel_params-running   = abap_true.
    ls_jobsel_params-finished  = abap_true.
    ls_jobsel_params-aborted   = abap_true.

    CALL FUNCTION 'BP_JOB_MAINTENANCE_SM37B'
      EXPORTING
        o_jobsel_params         = ls_jobsel_params
      EXCEPTIONS
        unknown_selection_error = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
              TYPE 'S'
              NUMBER sy-msgno
              DISPLAY LIKE sy-msgty
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var1.
  controller=>program_variant_f4(
    EXPORTING
      iv_program = zcl_abapgit_ci_job_scheduler=>co_report-update_abapgit
    CHANGING
      cv_variant = p_var1 ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var2.
  controller=>program_variant_f4(
    EXPORTING
      iv_program = zcl_abapgit_ci_job_scheduler=>co_report-update_abapgit_ci
    CHANGING
      cv_variant = p_var2 ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var3.
  controller=>program_variant_f4(
    EXPORTING
      iv_program = zcl_abapgit_ci_job_scheduler=>co_report-run_abapgit_ci
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

    CATCH zcx_abapgit_exception INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
