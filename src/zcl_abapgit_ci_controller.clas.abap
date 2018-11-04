CLASS zcl_abapgit_ci_controller DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          !ii_view          TYPE REF TO zif_abapgit_ci_view
          !ii_repo_provider TYPE REF TO zif_abapgit_ci_repo_provider
          is_options        TYPE zif_abapgit_ci_definitions=>ty_options OPTIONAL,

      run
        RAISING
          zcx_abapgit_exception.

protected section.
  PRIVATE SECTION.
    DATA:
      mi_view          TYPE REF TO zif_abapgit_ci_view,
      mi_repo_provider TYPE REF TO zif_abapgit_ci_repo_provider,
      mo_ci            TYPE REF TO zcl_abapgit_ci,
      ms_options       TYPE zif_abapgit_ci_definitions=>ty_options.

ENDCLASS.



CLASS ZCL_ABAPGIT_CI_CONTROLLER IMPLEMENTATION.


  METHOD constructor.

    mi_view          = ii_view.
    mi_repo_provider = ii_repo_provider.
    ms_options       = is_options.

    mo_ci = NEW zcl_abapgit_ci( ).

  ENDMETHOD.


  METHOD run.

    DATA(lt_repos)  = mi_repo_provider->get_repos( ).
    DATA(ls_result) = mo_ci->process_repos( lt_repos ).

    IF ms_options-push_results_to_git = abap_true.
      NEW zcl_abapgit_ci_distributor( ms_options-destination_git_repo_url  )->push_to_git_repo( is_result = ls_result ).
    ENDIF.

    mi_view->display( CHANGING cs_result = ls_result ).

  ENDMETHOD.
ENDCLASS.
