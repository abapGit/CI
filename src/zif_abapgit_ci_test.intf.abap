INTERFACE zif_abapgit_ci_test
  PUBLIC .
  METHODS:
    get_description
      RETURNING
        VALUE(rv_description) TYPE char255,

    execute
      RAISING
        zcx_abapgit_exception.

ENDINTERFACE.
