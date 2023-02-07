CLASS ltcl_tests DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_ci_repo_category.

    METHODS:
      setup,
      test_categories FOR TESTING,
      test_category FOR TESTING,
      test_objects FOR TESTING,
      test_others FOR TESTING,
      test_aff FOR TESTING,
      test_bw FOR TESTING.

ENDCLASS.

CLASS ltcl_tests IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test_categories.

    DATA(lv_bool) = xsdbool( lines( mo_cut->get_categories( ) ) > 20 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_bool
      exp = abap_true ).

  ENDMETHOD.

  METHOD test_category.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_category_label( 'dictionary' )
      exp = 'Dictionary' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_category_label( 'customizing' )
      exp = 'Customizing' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_category_label( 'other' )
      exp = 'Others' ).

  ENDMETHOD.

  METHOD test_objects.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_repo_category( 'PROG' )
      exp = 'source_library' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_repo_category( 'CLAS_with_ENHO' )
      exp = 'source_library' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_repo_category( 'TABL' )
      exp = 'dictionary' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_repo_category( 'TTYP_with_CLAS_ref' )
      exp = 'dictionary' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_repo_category( 'DDIC_ref' )
      exp = 'dictionary' ).

  ENDMETHOD.

  METHOD test_others.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_repo_category( 'SICF' )
      exp = 'other' ).

  ENDMETHOD.

  METHOD test_aff.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_repo_category( 'CHKC' )
      exp = 'aff' ).

  ENDMETHOD.

  METHOD test_bw.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_repo_category( 'IOBJ' )
      exp = 'bw' ).

  ENDMETHOD.

ENDCLASS.
