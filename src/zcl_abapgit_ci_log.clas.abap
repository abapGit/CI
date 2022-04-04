CLASS zcl_abapgit_ci_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:
      co_package TYPE devclass VALUE '$TMP',
      co_prefix  TYPE string VALUE 'ZABAPGIT_CI_',
      co_all     TYPE string VALUE 'ZABAPGIT_CI_*'.

    METHODS add
      IMPORTING
        !iv_log_object TYPE string
        !is_data       TYPE any OPTIONAL
        !it_data       TYPE ANY TABLE OPTIONAL
      RETURNING
        VALUE(rv_key)  TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS drop_all
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_wwwparams_tt TYPE STANDARD TABLE OF wwwparams WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF co_param_names,
        version  TYPE w3_name VALUE 'version',
        fileext  TYPE w3_name VALUE 'fileextension',
        filesize TYPE w3_name VALUE 'filesize',
        filename TYPE w3_name VALUE 'filename',
        mimetype TYPE w3_name VALUE 'mimetype',
      END OF co_param_names.

    METHODS get_next_objid
      RETURNING
        VALUE(rv_objid) TYPE string.

    METHODS set_params
      IMPORTING
        !is_key          TYPE wwwdatatab
        !iv_size         TYPE i
        !iv_name         TYPE string
      RETURNING
        VALUE(rt_params) TYPE ty_wwwparams_tt
      RAISING
        zcx_abapgit_exception.

    METHODS stringify
      IMPORTING
        !ig_data         TYPE any
      RETURNING
        VALUE(rv_string) TYPE string
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_ci_log IMPLEMENTATION.


  METHOD add.

    DATA:
      lv_xstring  TYPE xstring,
      lv_obj_name TYPE tadir-obj_name,
      lv_size     TYPE i,
      lv_name     TYPE string,
      lt_w3params TYPE STANDARD TABLE OF wwwparams,
      lt_w3mime   TYPE STANDARD TABLE OF w3mime,
      lt_w3html   TYPE STANDARD TABLE OF w3html.

    lv_obj_name = get_next_objid( ).

    DATA(ls_key) = VALUE wwwdatatab(
                     relid    = 'MI'
                     objid    = lv_obj_name
                     text     = iv_log_object
                     tdate    = sy-datum
                     ttime    = sy-uzeit
                     chname   = sy-uname
                     devclass = co_package ).

    IF is_data IS INITIAL.
      lv_xstring = zcl_abapgit_convert=>string_to_xstring_utf8( stringify( it_data ) ).
    ELSE.
      lv_xstring = zcl_abapgit_convert=>string_to_xstring_utf8( stringify( is_data ) ).
    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xstring
      IMPORTING
        output_length = lv_size
      TABLES
        binary_tab    = lt_w3mime.

    lt_w3params = set_params(
      is_key    = ls_key
      iv_size   = lv_size
      iv_name   = iv_log_object ).

    CALL FUNCTION 'WWWPARAMS_UPDATE'
      TABLES
        params       = lt_w3params
      EXCEPTIONS
        update_error = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Cannot update W3MI params' ).
    ENDIF.

    CALL FUNCTION 'WWWDATA_EXPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
        html              = lt_w3html
      EXCEPTIONS
        wrong_object_type = 1
        export_error      = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Cannot upload W3MI data' ).
    ENDIF.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid                 = 'R3TR'
        wi_tadir_object                = 'W3MI'
        wi_tadir_obj_name              = lv_obj_name
        wi_tadir_devclass              = co_package
        wi_test_modus                  = space
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
        OTHERS                         = 99.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Cannot update TADIR for W3MI' ).
    ENDIF.

  ENDMETHOD.


  METHOD drop_all.

    DATA:
      lv_obj_name TYPE tadir-obj_name,
      lt_tadir    TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY.

    SELECT * FROM tadir INTO TABLE @lt_tadir
      WHERE pgmid = 'R3TR' AND object = 'W3MI' AND obj_name LIKE @co_all AND devclass = @co_package
      ORDER BY PRIMARY KEY.

    LOOP AT lt_tadir INTO DATA(ls_tadir).

      DATA(ls_key) = VALUE wwwdatatab(
                       relid    = 'MI'
                       objid    = ls_tadir-obj_name
                       devclass = co_package ).

      CALL FUNCTION 'WWWDATA_DELETE'
        EXPORTING
          key               = ls_key
        EXCEPTIONS
          wrong_object_type = 1
          delete_error      = 2
          OTHERS            = 3.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Cannot delete W3MI data' ).
      ENDIF.

      CALL FUNCTION 'WWWPARAMS_DELETE_ALL'
        EXPORTING
          key          = ls_key
        EXCEPTIONS
          delete_error = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Cannot delete W3MI params' ).
      ENDIF.

      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry          = abap_true
          wi_tadir_pgmid                 = ls_tadir-pgmid
          wi_tadir_object                = ls_tadir-object
          wi_tadir_obj_name              = ls_tadir-obj_name
          wi_tadir_devclass              = ls_tadir-devclass
          wi_test_modus                  = space
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
          OTHERS                         = 99.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Cannot delete TADIR for W3MI' ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_next_objid.

    DATA:
      lv_obj_name TYPE tadir-obj_name,
      lv_counter  TYPE n LENGTH 10.

    SELECT MAX( obj_name ) FROM tadir INTO @lv_obj_name
      WHERE pgmid = 'R3TR' AND object = 'W3MI' AND obj_name LIKE @co_all AND devclass = @co_package.
    IF sy-subrc = 0.
      DATA(lv_len) = strlen( co_prefix ).
      lv_counter = lv_obj_name+lv_len(*).
    ENDIF.

    lv_counter = lv_counter + 1.

    " Format: ZABAPGIT_CI_nnnnnnnnnn
    rv_objid = co_prefix && lv_counter.

  ENDMETHOD.


  METHOD set_params.

    DATA ls_param LIKE LINE OF rt_params.

    ls_param-relid = is_key-relid.
    ls_param-objid = is_key-objid.
    ls_param-name  = co_param_names-version.
    ls_param-value = '00001'.
    INSERT ls_param INTO TABLE rt_params.
    ls_param-name  = co_param_names-filename.
    ls_param-value = iv_name.
    INSERT ls_param INTO TABLE rt_params.
    ls_param-name  = co_param_names-fileext.
    ls_param-value = '.json'.
    INSERT ls_param INTO TABLE rt_params.
    ls_param-name  = co_param_names-filesize.
    ls_param-value = iv_size.
    ls_param-value = condense( ls_param-value ).
    INSERT ls_param INTO TABLE rt_params.
    ls_param-name  = co_param_names-mimetype.
    ls_param-value = 'application/json'.
    INSERT ls_param INTO TABLE rt_params.

  ENDMETHOD.


  METHOD stringify.

    DATA li_json TYPE REF TO zif_abapgit_ajson.

    TRY.
        li_json = zcl_abapgit_ajson=>create_empty( ).
        li_json->keep_item_order( ).
        li_json->set(
          iv_path = '/'
          iv_val  = ig_data ).
        rv_string = li_json->stringify( 2 ).
      CATCH zcx_abapgit_ajson_error INTO DATA(lx_error).
        zcx_abapgit_exception=>raise( 'Error converting data to JSON:' && lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
