CLASS zcl_abapgit_ci_controller DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          !ii_view          TYPE REF TO zif_abapgit_ci_view
          !ii_repo_provider TYPE REF TO zif_abapgit_ci_repo_provider,

      run
        RAISING
          zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA:
      mi_view          TYPE REF TO zif_abapgit_ci_view,
      mi_repo_provider TYPE REF TO zif_abapgit_ci_repo_provider,
      mo_ci            TYPE REF TO zcl_abapgit_ci.

ENDCLASS.


CLASS zcl_abapgit_ci_controller IMPLEMENTATION.

  METHOD constructor.

    mi_view = ii_view.
    mi_repo_provider = ii_repo_provider.

    mo_ci = NEW zcl_abapgit_ci( ).

  ENDMETHOD.

  METHOD run.

    DATA(lt_repos)  = mi_repo_provider->get_repos( ).
    DATA(lt_result) = mo_ci->process_repos( lt_repos ).
    mi_view->display( CHANGING ct_result = lt_result ).

  ENDMETHOD.

ENDCLASS.
