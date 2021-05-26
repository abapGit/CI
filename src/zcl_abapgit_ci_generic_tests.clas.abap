CLASS zcl_abapgit_ci_generic_tests DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      execute
        RETURNING
          VALUE(rt_result) TYPE zif_abapgit_ci_definitions=>tty_generic_result_list
        RAISING
          zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      get_test_cases
        RETURNING
          VALUE(rt_test_cases) TYPE seo_relkeys
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_ci_generic_tests IMPLEMENTATION.


  METHOD execute.

    DATA: li_test   TYPE REF TO zif_abapgit_ci_test,
          ls_result LIKE LINE OF rt_result.

    DATA(lt_tests) = get_test_cases( ).

    LOOP AT lt_tests ASSIGNING FIELD-SYMBOL(<ls_test>).

      CLEAR: ls_result.

      CREATE OBJECT li_test TYPE (<ls_test>-clsname).

      ls_result-status = zif_abapgit_ci_definitions=>co_status-not_ok.

      TRY.
          ls_result-description = li_test->get_description( ).
          DATA(lt_list) = li_test->execute( ).

          READ TABLE lt_list ASSIGNING FIELD-SYMBOL(<ls_error>) WITH KEY kind = 'E'.
          IF sy-subrc = 0.
            ls_result-message = 'At least one unit test failed (see below)'.
            ls_result-status = zif_abapgit_ci_definitions=>co_status-not_ok.
          ELSE.
            ls_result-status = zif_abapgit_ci_definitions=>co_status-ok.
          ENDIF.

        CATCH zcx_abapgit_exception INTO DATA(lx_error).
          ls_result-message = lx_error->get_text( ).
      ENDTRY.

      INSERT ls_result INTO TABLE rt_result.

      " Add individual tests as result rows
      LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<ls_list>) WHERE kind = 'E'.
        CLEAR ls_result.
        ls_result-description = |UT: { <ls_list>-objtype } { <ls_list>-objname }|.
        ls_result-status      = zif_abapgit_ci_definitions=>co_status-not_ok.
        ls_result-message     = |{ <ls_list>-text } [ @{ CONV i( <ls_list>-line ) } ]|.
        INSERT ls_result INTO TABLE rt_result.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_test_cases.

    TRY.
        rt_test_cases = CAST cl_oo_interface(
                               cl_oo_interface=>get_instance( |ZIF_ABAPGIT_CI_TEST| )
                                 )->get_implementing_classes( ).

      CATCH cx_class_not_existent INTO DATA(lx_error).
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
