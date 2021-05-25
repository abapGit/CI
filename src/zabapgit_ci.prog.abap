* https://github.com/abapGit/CI
REPORT zabapgit_ci.

DATA: gv_repo_name TYPE c LENGTH 60.

SELECTION-SCREEN BEGIN OF BLOCK b1  WITH FRAME TITLE TEXT-b01.
SELECTION-SCREEN COMMENT 1(79) descr01.
SELECTION-SCREEN COMMENT /1(79) descr02.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT /1(79) descr03.
SELECTION-SCREEN COMMENT /1(79) descr04.
SELECTION-SCREEN COMMENT /1(79) descr05.
SELECTION-SCREEN COMMENT /1(79) descr06.
SELECTION-SCREEN COMMENT /1(79) descr07.
SELECTION-SCREEN COMMENT /1(79) descr08.
SELECTION-SCREEN COMMENT /1(79) descr09.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2  WITH FRAME TITLE TEXT-b02.
SELECTION-SCREEN COMMENT 1(79) opt01.
SELECTION-SCREEN COMMENT /1(79) opt02.
SELECTION-SCREEN COMMENT /1(79) opt03.
SELECTION-SCREEN SKIP.
PARAMETERS:
  p_url  TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-b05.
SELECT-OPTIONS: s_repos FOR gv_repo_name LOWER CASE.
SELECTION-SCREEN END OF BLOCK b5.

SELECTION-SCREEN BEGIN OF BLOCK b3  WITH FRAME TITLE TEXT-b03.
PARAMETERS:
  slack TYPE abap_bool AS CHECKBOX,
  token TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4  WITH FRAME TITLE TEXT-b04.
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
PARAMETERS no_purge TYPE abap_bool AS CHECKBOX DEFAULT abap_false MODIF ID m1.
SELECTION-SCREEN COMMENT 9(30) FOR FIELD no_purge MODIF ID m1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b4.

INITIALIZATION.
  descr01 = TEXT-d01.
  descr02 = TEXT-d02.
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

    IF repot = abap_false.
      CLEAR layer.
    ENDIF.

    IF repo = abap_false.
      repol = repot = abap_false.
    ENDIF.
  ENDLOOP.

CLASS lcl_abapgit_ci DEFINITION.

  PUBLIC SECTION.
    METHODS:
      run.

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
          ii_view          = NEW zcl_abapgit_ci_alv_view( )
          is_options       = VALUE #(
            result_git_repo_url    = p_url
            post_errors_to_slack   = slack
            slack_oauth_token      = token
            exec_generic_checks    = generic
            exec_repository_checks = repo
            repo_check_options = VALUE #(
              check_local         = repol
              check_transportable = repot
              layer               = layer
              no_purge            = no_purge
            )
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

START-OF-SELECTION.
  NEW lcl_abapgit_ci( )->run( ).
