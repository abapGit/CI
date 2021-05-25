INTERFACE zif_abapgit_ci_view
  PUBLIC .


  METHODS display
    CHANGING
      !cs_result TYPE zif_abapgit_ci_definitions=>ty_result
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
