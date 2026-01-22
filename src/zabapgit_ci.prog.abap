*&---------------------------------------------------------------------*
*& Report zabapgit_ci
*&---------------------------------------------------------------------*
*& abapGit CI Runner
*& See https://github.com/abapGit/CI
*&---------------------------------------------------------------------*
REPORT zabapgit_ci.

DATA: gv_repo_name TYPE c LENGTH 60.
DATA: gv_cat_name TYPE c LENGTH 60.

*-----------------------------------------------------------------------

" Function Keys
TABLES: sscrfields.

SELECTION-SCREEN FUNCTION KEY 1.

" Selection
SELECTION-SCREEN BEGIN OF SCREEN 100 AS SUBSCREEN.

  SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-b05.
    SELECT-OPTIONS: s_repos FOR gv_repo_name LOWER CASE.
    SELECT-OPTIONS: s_cats FOR gv_cat_name LOWER CASE.
  SELECTION-SCREEN END OF BLOCK b5.

  SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-b04.
    PARAMETERS:
      generic TYPE abap_bool AS CHECKBOX DEFAULT 'X',
      repo    TYPE abap_bool AS CHECKBOX DEFAULT 'X' USER-COMMAND u1.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 4.
      PARAMETERS repol TYPE abap_bool AS CHECKBOX DEFAULT abap_true MODIF ID m1.
      SELECTION-SCREEN COMMENT 9(30) FOR FIELD repol MODIF ID m1.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 4.
      PARAMETERS repot TYPE abap_bool AS CHECKBOX DEFAULT abap_false MODIF ID m1 USER-COMMAND u2.
      SELECTION-SCREEN COMMENT 9(30) FOR FIELD repot MODIF ID m1.
      SELECTION-SCREEN POSITION 40.
      SELECTION-SCREEN COMMENT 40(20) FOR FIELD layer MODIF ID m1.
      PARAMETERS layer TYPE devlayer MODIF ID m1.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 4.
      PARAMETERS createp TYPE abap_bool AS CHECKBOX DEFAULT abap_false MODIF ID m1.
      SELECTION-SCREEN COMMENT 9(30) FOR FIELD createp MODIF ID m1.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 4.
      PARAMETERS no_purge TYPE abap_bool AS CHECKBOX DEFAULT abap_false MODIF ID m1.
      SELECTION-SCREEN COMMENT 9(30) FOR FIELD no_purge MODIF ID m1.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN END OF SCREEN 100.

*-----------------------------------------------------------------------

" Options
SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN.

  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
    SELECTION-SCREEN COMMENT 1(77) opt01.
    SELECTION-SCREEN COMMENT /1(77) opt02.
    SELECTION-SCREEN COMMENT /1(77) opt03.
    SELECTION-SCREEN SKIP.
    PARAMETERS:
      p_url  TYPE string LOWER CASE,
      p_save TYPE abap_bool AS CHECKBOX,
      p_hist TYPE abap_bool AS CHECKBOX.
  SELECTION-SCREEN END OF BLOCK b2.

  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
    PARAMETERS:
      slack TYPE abap_bool AS CHECKBOX,
      token TYPE string LOWER CASE.
  SELECTION-SCREEN END OF BLOCK b3.

  SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-b06.
    PARAMETERS:
      p_sync   TYPE abap_bool AS CHECKBOX,
      p_log    TYPE abap_bool AS CHECKBOX,
      p_noskip TYPE abap_bool AS CHECKBOX.
  SELECTION-SCREEN END OF BLOCK b6.

SELECTION-SCREEN END OF SCREEN 200.

*-----------------------------------------------------------------------

" Main
SELECTION-SCREEN BEGIN OF BLOCK scr_header WITH FRAME TITLE TEXT-b01.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN COMMENT 1(77) descr01.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN COMMENT /1(77) descr03.
  SELECTION-SCREEN COMMENT /1(77) descr04.
  SELECTION-SCREEN COMMENT /1(77) descr05.
  SELECTION-SCREEN COMMENT /1(77) descr06.
  SELECTION-SCREEN COMMENT /1(77) descr07.
  SELECTION-SCREEN COMMENT /1(77) descr08.
  SELECTION-SCREEN COMMENT /1(77) descr09.
SELECTION-SCREEN END OF BLOCK scr_header.
SELECTION-SCREEN BEGIN OF TABBED BLOCK scr_tab FOR 18 LINES.
  SELECTION-SCREEN TAB (40) scr_tab1 USER-COMMAND scr_push1 DEFAULT SCREEN 100.
  SELECTION-SCREEN TAB (40) scr_tab2 USER-COMMAND scr_push2 DEFAULT SCREEN 200.
SELECTION-SCREEN END OF BLOCK scr_tab.

INITIALIZATION.
  sscrfields-functxt_01 = icon_biw_scheduler && 'Schedule Immediately'.
  WRITE icon_selection AS ICON TO scr_tab1.
  scr_tab1+6 = 'Selection'.
  WRITE icon_icon_list AS ICON TO scr_tab2.
  scr_tab2+6 = 'Options'.
  descr01 = TEXT-d01.
  descr03 = TEXT-d03.
  descr04 = TEXT-d04.
  descr05 = TEXT-d05.
  descr06 = TEXT-d06.
  descr07 = TEXT-d07.
  descr08 = TEXT-d08.
  descr09 = TEXT-d09.
  opt01 = TEXT-o01.
  opt02 = TEXT-o02.
  opt03 = TEXT-o03.

  " Get default transport layer
  CALL FUNCTION 'TR_GET_TRANSPORT_TARGET'
    EXPORTING
      iv_use_default             = abap_true
      iv_get_layer_only          = abap_true
    IMPORTING
      ev_layer                   = layer
    EXCEPTIONS
      wrong_call                 = 1
      invalid_input              = 2
      cts_initialization_failure = 3
      OTHERS                     = 4.
  IF sy-subrc <> 0.
    CLEAR layer.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_cats-low.

  s_cats-low = NEW zcl_abapgit_ci_repo_category( )->f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_cats-high.

  s_cats-high = NEW zcl_abapgit_ci_repo_category( )->f4( ).

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'M1'.
      screen-input = COND #( WHEN repo = abap_true THEN '1' ELSE '0' ).
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'LAYER'.
      screen-input = COND #( WHEN repo = abap_true AND repot = abap_true THEN '1' ELSE '0' ).
      MODIFY SCREEN.
    ENDIF.

    IF repo = abap_false.
      repol = abap_false.
      repot = abap_false.
    ENDIF.
  ENDLOOP.

CLASS lcl_abapgit_ci DEFINITION.

  PUBLIC SECTION.
    METHODS:
      run,
      schedule.

  PRIVATE SECTION.
    METHODS:
      send_to_slack
        IMPORTING
          ix_error TYPE REF TO zcx_abapgit_exception.

ENDCLASS.

CLASS lcl_abapgit_ci IMPLEMENTATION.

  METHOD run.

    TRY.
        NEW zcl_abapgit_ci_controller(
          ii_repo_provider = NEW zcl_abapgit_ci_test_repos( CORRESPONDING #( s_repos[] ) )
          ii_view = NEW zcl_abapgit_ci_alv_view( )
          is_options = VALUE #(
          result_git_repo_url    = p_url
          save_without_push      = p_save
          save_to_history        = p_hist
          post_errors_to_slack   = slack
          slack_oauth_token      = token
          exec_generic_checks    = generic
          exec_repository_checks = repo
          repo_check_options     = VALUE #(
                                        check_local         = repol
                                        check_transportable = repot
                                        layer               = layer
                                        create_package      = createp
                                        no_purge            = no_purge
                                        logging             = p_log
                                        ignore_skipping     = p_noskip
                                        categories          = CORRESPONDING #( s_cats[] )
          )
          sync_processing        = p_sync
          logging                = p_log
        )
          )->run( ).

        MESSAGE |abapGit CI run completed| TYPE 'S'.

      CATCH zcx_abapgit_exception INTO DATA(lx_error).

        IF slack = abap_true.
          send_to_slack( lx_error ).
        ENDIF.

        MESSAGE lx_error TYPE 'E'.

    ENDTRY.

  ENDMETHOD.

  METHOD schedule.

    DATA:
      lv_code     TYPE c LENGTH 50,
      lv_jobcount TYPE tbtcjob-jobcount,
      lv_jobname  TYPE tbtcjob-jobname.

    lv_jobname = sy-repid.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_jobname
        jobclass         = 'A'
      IMPORTING
        jobcount         = lv_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    CASE sy-subrc.
      WHEN 0.
        SUBMIT zabapgit_ci
          WITH createp  = createp
          WITH generic  = generic
          WITH layer    = layer
          WITH no_purge = no_purge
          WITH p_hist   = p_hist
          WITH p_log    = p_log
          WITH p_save   = p_save
          WITH p_sync   = p_sync
          WITH p_noskip = p_noskip
          WITH p_url    = p_url
          WITH repo     = repo
          WITH repol    = repol
          WITH repot    = repot
          WITH slack    = slack
          WITH s_cats   IN s_cats
          WITH s_repos  IN s_repos
          WITH token    = token
          VIA JOB lv_jobname NUMBER lv_jobcount
          AND RETURN.

        IF sy-subrc <> 0.
          DATA(ls_symsg) = cl_abap_submit_handling=>get_error_message( ).

          " Delete inconsistent job
          CALL FUNCTION 'BP_JOB_DELETE'
            EXPORTING
              jobcount                 = lv_jobcount
              jobname                  = lv_jobname
            EXCEPTIONS
              cant_delete_event_entry  = 1
              cant_delete_job          = 2
              cant_delete_joblog       = 3
              cant_delete_steps        = 4
              cant_delete_time_entry   = 5
              cant_derelease_successor = 6
              cant_enq_predecessor     = 7
              cant_enq_successor       = 8
              cant_enq_tbtco_entry     = 9
              cant_update_predecessor  = 10
              cant_update_successor    = 11
              commit_failed            = 12
              jobcount_missing         = 13
              jobname_missing          = 14
              job_does_not_exist       = 15
              job_is_already_running   = 16
              no_delete_authority      = 17
              OTHERS                   = 18 ##FM_SUBRC_OK.

          MESSAGE ID ls_symsg-msgid TYPE ls_symsg-msgty NUMBER ls_symsg-msgno
            WITH ls_symsg-msgv1 ls_symsg-msgv2 ls_symsg-msgv3 ls_symsg-msgv4
            DISPLAY LIKE 'I'.

          RETURN.
        ENDIF.

        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = lv_jobcount
            jobname              = lv_jobname
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
        CASE sy-subrc.
          WHEN 0.
            MESSAGE s398(00) WITH 'Job scheduled successfully'.
            RETURN.
          WHEN 1.
            lv_code = 'cant_start_immediate'.
          WHEN 2.
            lv_code = 'invalid_startdate'.
          WHEN 3.
            lv_code = 'jobname_missing'.
          WHEN 4.
            lv_code = 'job_close_failed'.
          WHEN 5.
            lv_code = 'job_nosteps'.
          WHEN 6.
            lv_code = 'job_notex'.
          WHEN 7.
            lv_code = 'lock_failed'.
          WHEN 8.
            lv_code = 'invalid_target'.
          WHEN 9.
            lv_code = 'invalid_time_zone'.
          WHEN 10.
            lv_code = 'others'.
        ENDCASE.
        MESSAGE e398(00) DISPLAY LIKE 'I'
          WITH 'JOB_CLOSE Error:' lv_code 'Program aborted'.
      WHEN 1.
        lv_code = 'cant_create_job'.
      WHEN 2.
        lv_code = 'invalid_job_data'.
      WHEN 3.
        lv_code = 'jobname_missing'.
      WHEN 4.
        lv_code = 'others'.
    ENDCASE.
    MESSAGE e398(00) DISPLAY LIKE 'I'
      WITH 'JOB_OPEN Error:' lv_code 'Program aborted'.

  ENDMETHOD.


  METHOD send_to_slack.
    TRY.
        NEW zcl_abapgit_ci_slack( token )->post(
                |abapGit CI error: abapGit CI run failed with |
             && |"{ ix_error->get_text( ) }"| ).

      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        MESSAGE lx_error TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN.
  IF sy-dynnr <> '1000' AND sscrfields-ucomm = 'FC01'.
    NEW lcl_abapgit_ci( )->schedule( ).
  ENDIF.

START-OF-SELECTION.
  NEW lcl_abapgit_ci( )->run( ).
