"* use this source file for your ABAP unit test classes
CLASS ltc_connection DEFINITION
FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

  PUBLIC SECTION.

    METHODS:
      is_http_client_bound FOR TESTING.

ENDCLASS.


CLASS ltc_connection IMPLEMENTATION.

  METHOD is_http_client_bound.
    DATA li_http_client TYPE REF TO if_http_client.
    TRY.
        li_http_client = NEW zcl_abapgit_ci_github_conn( )->create_http_client( ).
        cl_aunit_assert=>assert_bound(
          EXPORTING
            act              = li_http_client
        ).
      CATCH zcx_abapgit_exception.
        cl_aunit_assert=>assert_bound(
          EXPORTING
            act              = li_http_client
        ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
