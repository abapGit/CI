CLASS zcl_abapgit_ci_resource DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: if_rest_resource~post REDEFINITION.

ENDCLASS.



CLASS zcl_abapgit_ci_resource IMPLEMENTATION.

  METHOD if_rest_resource~post.

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

    lo_entity->set_string_data( /ui2/cl_json=>serialize( lo_receiver->ms_result ) ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).

  ENDMETHOD.

ENDCLASS.
