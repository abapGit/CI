CLASS zcl_abapgit_ci_run_abapgit_ut DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_ci_test.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_CI_RUN_ABAPGIT_UT IMPLEMENTATION.


  METHOD zif_abapgit_ci_test~execute.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_online.

    DATA(lt_repo_list) = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repo_list ASSIGNING FIELD-SYMBOL(<repo>).

      IF <repo>->get_name( ) = 'abapGit'.
        lo_repo ?= <repo>.
      ENDIF.

    ENDLOOP.

    IF lo_repo IS NOT BOUND.
      zcx_abapgit_exception=>raise( |Couldn't find abapGit repo| ).
    ENDIF.

    DATA(lt_list) = zcl_abapgit_factory=>get_code_inspector(
      iv_package            = lo_repo->get_package( )
      iv_check_variant_name = 'CL_SAUNIT_LEGACY_CI_CHECK'
      )->run( ).

    ASSIGN lt_list[ kind = 'E' ] TO FIELD-SYMBOL(<ls_error>).
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( |Unit test failed: |
                                 && |Object { <ls_error>-objtype } { <ls_error>-text } |
                                 && |{ <ls_error>-text }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ci_test~get_description.

    rv_description = |Run abapGit unit tests|.

  ENDMETHOD.
ENDCLASS.
