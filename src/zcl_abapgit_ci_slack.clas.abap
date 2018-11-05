CLASS zcl_abapgit_ci_slack DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_token TYPE string
        RAISING
          zcx_abapgit_exception,

      post
        IMPORTING
          iv_message TYPE string
        RAISING
          zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA:
      mv_token TYPE string.

ENDCLASS.



CLASS zcl_abapgit_ci_slack IMPLEMENTATION.


  METHOD constructor.

    IF iv_token IS INITIAL.
      zcx_abapgit_exception=>raise( |Please supply OAuth token| ).
    ENDIF.

    mv_token = iv_token.

  ENDMETHOD.


  METHOD post.

    TYPES:
      BEGIN OF ty_request,
        channel TYPE string,
        text    TYPE string,
      END OF ty_request.

    DATA: li_http_client TYPE REF TO if_http_client,
          ls_request     TYPE ty_request.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = 'https://slack.com'
        ssl_id             = 'ANONYM'
      IMPORTING
        client             = li_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    TRY.
        DATA(lo_rest_client) = NEW cl_rest_http_client( li_http_client ).

        DATA(lo_entity) = lo_rest_client->if_rest_client~create_request_entity( ).

        lo_entity->set_header_field(
            iv_name  = '~request_uri'
            iv_value = '/api/chat.postMessage' ).

        lo_entity->set_header_field(
            iv_name  = 'Content-Type'
            iv_value = 'application/json' ).

        lo_entity->set_header_field(
            iv_name  = 'Authorization'
            iv_value = |Bearer { mv_token }| ).

        ls_request-channel = |CDV0211MW|.
        ls_request-text    = iv_message.

        lo_entity->set_string_data( /ui2/cl_json=>serialize( data = ls_request
                                                             pretty_name = /ui2/cl_json=>pretty_mode-low_case ) ).

        lo_rest_client->if_rest_client~post( lo_entity ).

        DATA(lo_response) = lo_rest_client->if_rest_client~get_response_entity( ).

        DATA(lv_status) = lo_rest_client->if_rest_client~get_status( ).

      CATCH cx_rest_client_exception INTO DATA(lx_rest_error).
        zcx_abapgit_exception=>raise( iv_text     = lx_rest_error->get_text( )
                                      ix_previous = lx_rest_error ).
    ENDTRY.

    IF lv_status <> cl_rest_status_code=>gc_success_ok.
      zcx_abapgit_exception=>raise( |HTTP status code { lv_status } from api.github.com| ).
    ENDIF.

    cl_demo_output=>display( lo_response->get_string_data( ) ).

  ENDMETHOD.
ENDCLASS.
