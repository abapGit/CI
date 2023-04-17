INTERFACE zif_abapgit_ci_test
  PUBLIC .


  METHODS get_description
    RETURNING
      VALUE(rv_description) TYPE zabapgit_ci_description .
  METHODS execute
    RETURNING
      VALUE(rt_list) TYPE zif_abapgit_code_inspector=>ty_results
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
