CLASS zcl_abapgit_ci_run_abapgit_ut DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_ci_test.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_ci_run_abapgit_ut IMPLEMENTATION.


  METHOD zif_abapgit_ci_test~execute.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_online.

    DATA(lt_repo_list) = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repo_list ASSIGNING FIELD-SYMBOL(<repo>).

      IF <repo>->get_name( ) = 'abapGit'.
        lo_repo ?= <repo>.
        EXIT.
      ENDIF.

    ENDLOOP.

    IF lo_repo IS NOT BOUND.
      zcx_abapgit_exception=>raise( |Couldn't find abapGit repo| ).
    ENDIF.

    rt_list = zcl_abapgit_factory=>get_code_inspector( lo_repo->get_package( )
      )->run( 'SWF_ABAP_UNIT' ).

  ENDMETHOD.


  METHOD zif_abapgit_ci_test~get_description.

    rv_description = |Run abapGit unit tests|.

  ENDMETHOD.
ENDCLASS.
