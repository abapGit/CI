CLASS zcl_abapgit_ci_latest_build DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_ci_test.

ENDCLASS.



CLASS zcl_abapgit_ci_latest_build IMPLEMENTATION.

  METHOD zif_abapgit_ci_test~get_description.
    rv_description = |Check latest abapGit build|.
  ENDMETHOD.

  METHOD zif_abapgit_ci_test~execute.
    zcx_abapgit_exception=>raise( |Check failed| ).
  ENDMETHOD.

ENDCLASS.
