CLASS zcl_abapgit_ci_repo_category DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      ty_object_type TYPE tadir-object,

      BEGIN OF ty_category,
        category       TYPE string,
        category_label TYPE string,
      END OF ty_category,
      ty_categories TYPE STANDARD TABLE OF ty_category WITH KEY category,

      BEGIN OF ty_object_category,
        object_type    TYPE ty_object_type,
        category       TYPE string,
        category_label TYPE string,
      END OF ty_object_category,
      ty_object_categories TYPE STANDARD TABLE OF ty_object_category WITH KEY object_type.

    CLASS-METHODS class_constructor.

    METHODS get_categories
      RETURNING
        VALUE(rt_result) TYPE ty_categories.

    METHODS get_category_label
      IMPORTING
        iv_category      TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS get_repo_category
      IMPORTING
        iv_repo_name     TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS f4
      RETURNING
        VALUE(rv_result) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      c_category_gateway_bse  TYPE string VALUE 'rap_services',
      c_category_ddic         TYPE string VALUE 'dictionary',
      c_category_auth         TYPE string VALUE 'apsiam',
      c_category_mime         TYPE string VALUE 'mime_objects',
      c_category_aff          TYPE string VALUE 'aff',
      c_category_aff_label    TYPE string VALUE 'ABAP File Format',
      c_category_cust         TYPE string VALUE 'customizing',
      c_category_cust_label   TYPE string VALUE 'Customizing',
      c_category_hier         TYPE string VALUE 'hier_storage',
      c_category_hier_label   TYPE string VALUE 'Hierarchy Storage',
      c_category_bw           TYPE string VALUE 'bw',
      c_category_bw_label     TYPE string VALUE 'Business Warehouse',
      c_category_data         TYPE string VALUE 'data',
      c_category_data_label   TYPE string VALUE 'Data',
      c_category_aq           TYPE string VALUE 'sapquery',
      c_category_aq_label     TYPE string VALUE 'SAP Query',
      c_category_docs         TYPE string VALUE 'documents',
      c_category_docs_label   TYPE string VALUE 'Documents (Longtexts)',
      c_category_idoc         TYPE string VALUE 'idoc',
      c_category_idoc_label   TYPE string VALUE 'IDoc',
      c_category_iac          TYPE string VALUE 'iac',
      c_category_iac_label    TYPE string VALUE 'Internet Application Components',
      c_category_nspace       TYPE string VALUE 'namespace',
      c_category_nspace_label TYPE string VALUE 'Namespace',
      c_category_others       TYPE string VALUE 'other',
      c_category_others_label TYPE string VALUE 'Others'.

    CLASS-DATA:
      gt_categories         TYPE ty_categories,
      gt_object_categrories TYPE ty_object_categories,
      go_aff_registry       TYPE REF TO zif_abapgit_aff_registry.

    CLASS-METHODS build_categories
      IMPORTING
        it_object_categories TYPE ty_object_categories
      RETURNING
        VALUE(rt_result)     TYPE ty_categories.

    CLASS-METHODS build_object_categories
      RETURNING
        VALUE(rt_result) TYPE ty_object_categories.

    CLASS-METHODS get_wb_types
      RETURNING
        VALUE(rt_result) TYPE seu_adt_repository_type_list.
ENDCLASS.



CLASS zcl_abapgit_ci_repo_category IMPLEMENTATION.


  METHOD build_categories.

    " Build list of categories

    DATA ls_category TYPE ty_category.

    LOOP AT it_object_categories REFERENCE INTO DATA(lr_object_category) WHERE category <> c_category_others.

      READ TABLE rt_result TRANSPORTING NO FIELDS WITH TABLE KEY category = lr_object_category->category.
      IF sy-subrc <> 0.
        CLEAR ls_category.
        ls_category-category       = lr_object_category->category.
        ls_category-category_label = lr_object_category->category_label.
        INSERT ls_category INTO TABLE rt_result.
      ENDIF.

    ENDLOOP.

    " Add "ABAP File Format"
    ls_category-category       = c_category_aff.
    ls_category-category_label = c_category_aff_label.
    INSERT ls_category INTO TABLE rt_result.

    " Add "Business Warehouse"
    ls_category-category       = c_category_bw.
    ls_category-category_label = c_category_bw_label.
    INSERT ls_category INTO TABLE rt_result.

    " Add "Customizing"
    ls_category-category       = c_category_cust.
    ls_category-category_label = c_category_cust_label.
    INSERT ls_category INTO TABLE rt_result.

    " Add "Hierarchy Storage"
    ls_category-category       = c_category_hier.
    ls_category-category_label = c_category_hier_label.
    INSERT ls_category INTO TABLE rt_result.

    " Add "Data"
    ls_category-category       = c_category_data.
    ls_category-category_label = c_category_data_label.
    INSERT ls_category INTO TABLE rt_result.

    " Add "SAP Query"
    ls_category-category       = c_category_aq.
    ls_category-category_label = c_category_aq_label.
    INSERT ls_category INTO TABLE rt_result.

    " Add "Documents"
    ls_category-category       = c_category_docs.
    ls_category-category_label = c_category_docs_label.
    INSERT ls_category INTO TABLE rt_result.

    " Add "IDoc"
    ls_category-category       = c_category_idoc.
    ls_category-category_label = c_category_idoc_label.
    INSERT ls_category INTO TABLE rt_result.

    " Add "Internet Application Components"
    ls_category-category       = c_category_iac.
    ls_category-category_label = c_category_iac_label.
    INSERT ls_category INTO TABLE rt_result.

    " Add "Namespace"
    ls_category-category       = c_category_nspace.
    ls_category-category_label = c_category_nspace_label.
    INSERT ls_category INTO TABLE rt_result.

    SORT rt_result BY category_label.

    " Add "Other" to the end
    ls_category-category       = c_category_others.
    ls_category-category_label = c_category_others_label.
    INSERT ls_category INTO TABLE rt_result.

  ENDMETHOD.


  METHOD build_object_categories.

    " Build list of object types

    DATA ls_object_category TYPE ty_object_category.

    DATA(lt_types) = get_wb_types( ).

    LOOP AT lt_types REFERENCE INTO DATA(lr_type) WHERE object_type NP '/*'.

      READ TABLE rt_result TRANSPORTING NO FIELDS WITH TABLE KEY object_type = lr_type->object_type.
      IF sy-subrc <> 0.
        CLEAR ls_object_category.
        ls_object_category-object_type       = lr_type->object_type.
        ls_object_category-category          = lr_type->category.
        ls_object_category-category_label    = lr_type->category_label.

        CASE ls_object_category-category.
          WHEN '<none>'.
            ls_object_category-category = c_category_others.
          WHEN 'dummygroup'.
            CONTINUE.
          WHEN 'fiori_ui'.
            ls_object_category-category_label = 'Fiori UI'.
        ENDCASE.

        INSERT ls_object_category INTO TABLE rt_result.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD class_constructor.

    gt_object_categrories = build_object_categories( ).

    gt_categories = build_categories( gt_object_categrories ).

    CREATE OBJECT go_aff_registry TYPE zcl_abapgit_aff_registry.

  ENDMETHOD.


  METHOD f4.

    TYPES:
      BEGIN OF ty_value,
        category       TYPE rstxtlg,
        category_label TYPE rstxtlg,
      END OF ty_value.

    DATA:
      ls_value  TYPE ty_value,
      lt_value  TYPE STANDARD TABLE OF ty_value WITH DEFAULT KEY,
      ls_return TYPE ddshretval,
      lt_return TYPE STANDARD TABLE OF ddshretval.

    LOOP AT gt_categories INTO DATA(ls_category).
      ls_value = CORRESPONDING #( ls_category ).
      APPEND ls_value TO lt_value.
    ENDLOOP.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CATEGORY'
        window_title    = 'Repo Category'
        value_org       = 'S'
      TABLES
        value_tab       = lt_value
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    LOOP AT lt_return INTO ls_return.
      rv_result = ls_return-fieldval.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_categories.
    rt_result = gt_categories.
  ENDMETHOD.


  METHOD get_category_label.

    READ TABLE gt_categories REFERENCE INTO DATA(lr_category)
      WITH TABLE KEY category = iv_category.
    IF sy-subrc = 0.
      rv_result = lr_category->category_label.
    ELSE.
      rv_result = 'Other'.
    ENDIF.

  ENDMETHOD.


  METHOD get_repo_category.

    IF strlen( iv_repo_name ) < 4.
      rv_result = c_category_others.
      RETURN.
    ENDIF.

    " First four letters of repo name represent the major test object (repo might include others)
    DATA(lv_object_type) = iv_repo_name(4).

    " Find category of test object
    READ TABLE gt_object_categrories REFERENCE INTO DATA(lr_object_category)
      WITH TABLE KEY object_type = lv_object_type.
    IF sy-subrc = 0.
      rv_result = lr_object_category->category.
    ENDIF.

    IF go_aff_registry->is_supported_object_type( |{ lv_object_type }| ).
      rv_result = c_category_aff.
    ENDIF.

    IF rv_result IS INITIAL OR rv_result = c_category_others.
      " Assign some more cases, rest goes to "other"
      CASE to_upper( lv_object_type ).
        WHEN 'DDIC'.
          rv_result = c_category_ddic.
        WHEN 'CUS0' OR 'CUS1' OR 'CUS2' OR 'SCP1'.
          rv_result = c_category_cust.
        WHEN 'SHI3' OR 'SHI5' OR 'SHI8'.
          rv_result = c_category_hier.
        WHEN 'IWPR' OR 'IWVB'.
          rv_result = c_category_gateway_bse.
        WHEN 'SUCU' OR 'SUSC'.
          rv_result = c_category_auth.
        WHEN 'AREA' OR 'IOBJ' OR 'ODSO'.
          rv_result = c_category_bw.
        WHEN 'W3HT' OR 'W3MI'.
          rv_result = c_category_mime.
        WHEN 'AQBG' OR 'AQQU' OR 'AQSG'.
          rv_result = c_category_aq.
        WHEN 'DOCT' OR 'DOCV' OR 'DSYS'.
          rv_result = c_category_docs.
        WHEN 'IDOC' OR 'IEXT'.
          rv_result = c_category_idoc.
        WHEN 'NSPC'.
          rv_result = c_category_nspace.
        WHEN 'IARP' OR 'IASP' OR 'IATU' OR 'IAXU'.
          rv_result = c_category_iac.
        WHEN OTHERS.
          IF lv_object_type CP 'DATA*' OR lv_object_type CP 'TABU*'.
            rv_result = c_category_data.
          ELSE.
            rv_result = c_category_others.
          ENDIF.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD get_wb_types.

    " Returns object types by category like ADT (CL_SEU_ADT_RES_REPO_TYPES)

    DATA:
      lo_wbobjtype_data TYPE REF TO if_wb_objtype_data,
      ls_type_info      TYPE seu_adt_object_type_descriptor,
      ls_type_conv      TYPE wbobjtype.

    cl_wb_registry=>get_objtype_provider( )->get_objtypes( IMPORTING p_objtype_data = lo_wbobjtype_data ).

    LOOP AT lo_wbobjtype_data->mt_wbobjtype REFERENCE INTO DATA(lr_wbobjtype).
      CLEAR ls_type_info.

      ls_type_conv-objtype_tr = lr_wbobjtype->objecttype.
      ls_type_conv-subtype_wb = lr_wbobjtype->subtype_wb.

      DATA(ls_category_info) = cl_adt_type_group_util=>get_group( type = ls_type_conv ).

      IF ls_category_info IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(lv_object_type) = cl_wb_object_type=>get_global_id_from_global_type( p_global_type = ls_type_conv ).

      ls_type_info-object_type       = lv_object_type.
      ls_type_info-object_type_label = lr_wbobjtype->uiname_singular.
      ls_type_info-category          = ls_category_info-category.
      ls_type_info-category_label    = ls_category_info-category_label.

      APPEND ls_type_info TO rt_result.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
