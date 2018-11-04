CLASS zcl_abapgit_ci_resource DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: if_rest_resource~post REDEFINITION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_ci_result,
        ci_has_errors TYPE abap_bool,
        result_table  TYPE zif_abapgit_ci_definitions=>tty_result,
      END OF ty_ci_result.

ENDCLASS.



CLASS zcl_abapgit_ci_resource IMPLEMENTATION.

  METHOD if_rest_resource~post.

    DATA: ls_ci_result TYPE ty_ci_result.

    DATA(lo_entity) = mo_response->create_entity( ).
    DATA(lo_receiver) = NEW lcl_data_receiver( ).

    TRY.
        NEW zcl_abapgit_ci_controller(
            ii_view          = lo_receiver
            ii_repo_provider = NEW zcl_abapgit_ci_test_repos( )
        )->run( ).

      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        lo_entity->set_string_data( lx_error->get_text( ) ).
        mo_response->set_status( cl_rest_status_code=>gc_server_error_internal ).
        RETURN.
    ENDTRY.

    ls_ci_result-result_table  = lo_receiver->mt_result.
    ls_ci_result-ci_has_errors = boolc(
                                   line_exists(
                                     ls_ci_result-result_table[ status = zif_abapgit_ci_definitions=>co_status-not_ok ] ) ).

    lo_entity->set_string_data( /ui2/cl_json=>serialize( ls_ci_result ) ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).

  ENDMETHOD.

ENDCLASS.
