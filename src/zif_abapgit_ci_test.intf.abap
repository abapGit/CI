INTERFACE zif_abapgit_ci_test
  PUBLIC .
  METHODS:
    get_description
      RETURNING
        VALUE(rv_description) TYPE zabapgit_ci_description,

    execute
      RAISING
        zcx_abapgit_exception.

ENDINTERFACE.
