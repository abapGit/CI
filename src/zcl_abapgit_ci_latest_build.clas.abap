CLASS zcl_abapgit_ci_latest_build DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_ci_test.

    METHODS:
      constructor
        RAISING
          zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      co_report_name TYPE char30 VALUE 'Z___ABAPGIT_LATEST_BUILD',
      co_package     TYPE devclass VALUE '$___ABAPGIT_LATEST_BUILD'.

    DATA:
      mv_latest_build TYPE string,
      mt_latest_build TYPE string_table,
      mi_package      TYPE REF TO zif_abapgit_sap_package.

    METHODS:
      fetch_latest_build
        RAISING
          zcx_abapgit_exception,

      install
        RAISING
          zcx_abapgit_exception,

      check
        RAISING
          zcx_abapgit_exception,

      delete
        RAISING
          zcx_abapgit_exception,

      delete_tadir_entry
        RAISING
          zcx_abapgit_exception,

      post_check
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_CI_LATEST_BUILD IMPLEMENTATION.


  METHOD check.

    DATA(lt_list) = zcl_abapgit_factory=>get_code_inspector( co_package )->run( 'SYNTAX_CHECK' ).

    ASSIGN lt_list[ kind = 'E' ] TO FIELD-SYMBOL(<ls_error>).
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( |Latest build - Syntax Error: { <ls_error>-text }, line { <ls_error>-line } | ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    mi_package = zcl_abapgit_factory=>get_sap_package( co_package ).

  ENDMETHOD.


  METHOD delete.

    DELETE REPORT co_report_name.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Latest build: Deletion failed. Subrc = { sy-subrc }| ).
    ENDIF.

    delete_tadir_entry( ).

    DATA(lo_package) = NEW zcl_abapgit_object_devc( is_item = VALUE #( obj_type = 'DEVC'
                                                                       obj_name = co_package
                                                                       devclass = co_package )
                                                    iv_language = sy-langu ).

    lo_package->zif_abapgit_object~delete( co_package ).

  ENDMETHOD.


  METHOD delete_tadir_entry.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_delete_tadir_entry          = 'X'
        wi_test_modus                  = ' '
        wi_tadir_pgmid                 = 'R3TR'
        wi_tadir_object                = 'PROG'
        wi_tadir_obj_name              = CONV sobj_name( co_report_name )
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Latest build: Deletion failed. TR_TADIR_INTERFACE Subrc = { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD fetch_latest_build.

    DATA: li_http_client TYPE REF TO if_http_client.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = 'https://raw.githubusercontent.com'
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

    DATA(lo_rest_client) = NEW cl_rest_http_client( li_http_client ).

    lo_rest_client->if_rest_client~create_request_entity( )->set_header_field(
        iv_name  = '~request_uri'
        iv_value = |/abapGit/build/master/zabapgit.abap| ).

    lo_rest_client->if_rest_client~get( ).

    DATA(lo_response) = lo_rest_client->if_rest_client~get_response_entity( ).

    DATA(lv_status) = lo_rest_client->if_rest_client~get_status( ).

    IF lv_status <> cl_rest_status_code=>gc_success_ok.
      zcx_abapgit_exception=>raise(
          |HTTP status code { lv_status } |
       && |from https://raw.githubusercontent.com/abapGit/build/master/zabapgit.abap | ).
    ENDIF.

    SPLIT lo_response->get_string_data( )
      AT zif_abapgit_definitions=>c_newline
      INTO TABLE mt_latest_build.

  ENDMETHOD.


  METHOD install.

    IF mi_package->exists( ) = abap_false.

      mi_package->create( VALUE #(
                            as4user  = sy-uname
                            devclass = co_package
                            ctext    = |abapGit latest build|
                          ) ).

    ENDIF.

    INSERT REPORT co_report_name
           FROM mt_latest_build
           STATE 'A'
           PROGRAM TYPE '1'.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Latest build: Installation failed. Subrc = { sy-subrc }| ).
    ENDIF.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = co_report_name
        object_class        = 'ABAP'
        devclass            = co_package
        master_language     = sy-langu
        mode                = 'INSERT'
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Latest build: Installation failed. RS_CORR_INSERT Subrc = { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD post_check.

    SELECT SINGLE FROM tadir
           FIELDS object,
                  obj_name
           WHERE devclass = @co_package
           INTO @DATA(ls_object).

    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( |Latest build: Post check failed. |
                                 && |Left over object { ls_object-object } { ls_object-obj_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ci_test~execute.

    fetch_latest_build( ).
    install( ).
    check( ).
    delete( ).
    post_check( ).

  ENDMETHOD.


  METHOD zif_abapgit_ci_test~get_description.
    rv_description = |Check latest abapGit build|.
  ENDMETHOD.
ENDCLASS.
