*"* use this source file for your ABAP unit test classes
CLASS ltc_repos DEFINITION
FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

  PUBLIC SECTION.
    METHODS:
      check_repos_filled FOR TESTING.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_ci_test_repos.
    METHODS:
      setup.
ENDCLASS.


CLASS ltc_repos IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW zcl_abapgit_ci_test_repos(
*       it_repo_name_range =
    ).

  ENDMETHOD.

  METHOD check_repos_filled.
    DATA(lt_repos)  = mo_cut->zif_abapgit_ci_repo_provider~get_repos( ).
    cl_aunit_assert=>assert_not_initial(
      EXPORTING
        act              = lt_repos
    ).
  ENDMETHOD.


ENDCLASS.
