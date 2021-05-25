CLASS zcl_abapgit_ci_rest DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_http_handler
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS if_rest_application~get_root_handler
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_ci_rest IMPLEMENTATION.


  METHOD if_rest_application~get_root_handler.

    DATA(lo_handler) = NEW cl_rest_router( ).

    lo_handler->attach( iv_template      = '/run'
                        iv_handler_class = |ZCL_ABAPGIT_CI_RESOURCE| ).

    ro_root_handler = lo_handler.

  ENDMETHOD.
ENDCLASS.
