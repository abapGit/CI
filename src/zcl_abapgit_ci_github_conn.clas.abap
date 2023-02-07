CLASS zcl_abapgit_ci_github_conn DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS create_http_client
      RETURNING
        VALUE(ri_http_client) TYPE REF TO if_http_client
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_ci_github_conn IMPLEMENTATION.


  METHOD create_http_client.

    DATA lv_rfcdes TYPE rfcdes-rfcdest.

    lv_rfcdes = |API_GITHUB_{ sy-uname }|.

    SELECT SINGLE FROM rfcdes
           FIELDS rfcdest
           WHERE rfcdest = @lv_rfcdes
           INTO @lv_rfcdes.

    IF sy-subrc = 0.

      cl_http_client=>create_by_destination(
        EXPORTING
          destination              = lv_rfcdes
        IMPORTING
          client                   = ri_http_client
        EXCEPTIONS
          argument_not_found       = 1
          destination_not_found    = 2
          destination_no_authority = 3
          plugin_not_active        = 4
          internal_error           = 5
          OTHERS                   = 6 ).

    ELSE.

      cl_http_client=>create_by_url(
        EXPORTING
          url                = 'https://api.github.com'
          ssl_id             = 'ANONYM'
        IMPORTING
          client             = ri_http_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).

    ENDIF.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
