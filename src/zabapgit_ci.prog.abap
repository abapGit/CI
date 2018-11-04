*&---------------------------------------------------------------------*
*& Report zabapgit_ci
*&---------------------------------------------------------------------*
*&
*&       https://github.com/abapGit/CI
*&
*&---------------------------------------------------------------------*
REPORT zabapgit_ci.

SELECTION-SCREEN BEGIN OF BLOCK b1  WITH FRAME TITLE TEXT-b01.
SELECTION-SCREEN COMMENT  1(79) descr01.
SELECTION-SCREEN COMMENT /1(79) descr02.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT /1(79) descr03.
SELECTION-SCREEN COMMENT /1(79) descr04.
SELECTION-SCREEN COMMENT /1(79) descr05.
SELECTION-SCREEN COMMENT /1(79) descr06.
SELECTION-SCREEN COMMENT /1(79) descr07.
SELECTION-SCREEN COMMENT /1(79) descr08.
SELECTION-SCREEN COMMENT /1(79) descr09.
SELECTION-SCREEN COMMENT /1(79) descr10.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2  WITH FRAME TITLE TEXT-b02.
SELECTION-SCREEN COMMENT  1(79) opt01.
SELECTION-SCREEN COMMENT /1(79) opt02.
SELECTION-SCREEN COMMENT /1(79) opt03.
SELECTION-SCREEN SKIP.
PARAMETERS:
  p_url  TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  descr01 = TEXT-d01.
  descr02 = TEXT-d02.
  descr03 = TEXT-d03.
  descr04 = TEXT-d04.
  descr05 = TEXT-d05.
  descr06 = text-d06.
  descr07 = TEXT-d07.
  descr08 = TEXT-d08.
  descr09 = TEXT-d09.
  descr10 = TEXT-d10.
  opt01 = TEXT-o01.
  opt02 = TEXT-o02.
  opt03 = TEXT-o03.

START-OF-SELECTION.
  TRY.
      NEW zcl_abapgit_ci_controller(
        ii_repo_provider = NEW zcl_abapgit_ci_test_repos( )
        ii_view          = NEW zcl_abapgit_ci_alv_view( )
        is_options       = VALUE #(
          result_git_repo_url = p_url
        )
      )->run( ).

      MESSAGE |abapGit CI run completed| TYPE 'S'.

    CATCH zcx_abapgit_exception INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
