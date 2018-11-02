INTERFACE zif_abapgit_ci_view
  PUBLIC .


  METHODS:
    display
      CHANGING
        ct_result TYPE zif_abapgit_ci_definitions=>tty_result
      RAISING
        zcx_abapgit_exception.

ENDINTERFACE.
