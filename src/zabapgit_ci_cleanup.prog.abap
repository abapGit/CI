*&---------------------------------------------------------------------*
*& Report zabapgit_ci_cleanup
*&---------------------------------------------------------------------*
*& Various clean-up tools to use after abapGit CI run
*& See https://github.com/abapGit/CI
*&---------------------------------------------------------------------*
REPORT zabapgit_ci_cleanup LINE-SIZE 255.

DATA:
  gx_ex        TYPE REF TO zcx_abapgit_exception,
  gv_package   TYPE devclass,
  gv_transport TYPE trkorr,
  gs_item      TYPE zif_abapgit_definitions=>ty_item.

SELECTION-SCREEN SKIP.
PARAMETERS: p_list TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X'.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN SKIP.
PARAMETERS: p_uninst TYPE abap_bool RADIOBUTTON GROUP r1.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_pack FOR gv_package.
  PARAMETERS: p_purge TYPE abap_bool RADIOBUTTON GROUP r2 DEFAULT 'X',
              p_remov TYPE abap_bool RADIOBUTTON GROUP r2,
              p_obj   TYPE abap_bool RADIOBUTTON GROUP r2,
              p_otr   TYPE abap_bool RADIOBUTTON GROUP r2,
              p_log   TYPE abap_bool RADIOBUTTON GROUP r2,
              p_pack  TYPE abap_bool RADIOBUTTON GROUP r2,
              p_popup TYPE abap_bool AS CHECKBOX DEFAULT abap_false,
              p_forc  TYPE abap_bool AS CHECKBOX DEFAULT abap_false.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN SKIP.
PARAMETERS: p_trrel TYPE abap_bool RADIOBUTTON GROUP r1.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_txt   TYPE as4text DEFAULT 'abapGit CI*',
              p_prev  TYPE abap_bool AS CHECKBOX DEFAULT abap_false,
              p_force TYPE abap_bool AS CHECKBOX DEFAULT abap_false.
SELECTION-SCREEN END OF BLOCK b2.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS:
      run RAISING zcx_abapgit_exception,
      list RAISING zcx_abapgit_exception,
      uninstall_repos RAISING zcx_abapgit_exception,
      check_packages RAISING zcx_abapgit_exception,
      drop_packages RAISING zcx_abapgit_exception,
      drop_objects RAISING zcx_abapgit_exception,
      drop_otr RAISING zcx_abapgit_exception,
      drop_logs RAISING zcx_abapgit_exception,
      delete_package
        IMPORTING
          iv_package   TYPE devclass
          iv_transport TYPE trkorr
        RAISING
          zcx_abapgit_exception,
      delete_object
        IMPORTING
          iv_package   TYPE devclass
          iv_obj_type  TYPE tadir-object
          iv_obj_name  TYPE tadir-obj_name
          iv_transport TYPE trkorr
        RAISING
          zcx_abapgit_exception,
      delete_tadir
        IMPORTING
          iv_obj_type TYPE tadir-object
          iv_obj_name TYPE tadir-obj_name
        RAISING
          zcx_abapgit_exception,
      create_transport
        IMPORTING
          iv_text             TYPE string
        RETURNING
          VALUE(rv_transport) TYPE trkorr
        RAISING
          zcx_abapgit_exception,
      release_transports RAISING zcx_abapgit_exception,
      force_release_transport
        IMPORTING
          iv_trkorr TYPE trkorr
        RAISING
          zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_width TYPE i VALUE 200 ##NEEDED.
    CONSTANTS c_count TYPE i VALUE 20 ##NEEDED.
    TYPES: ty_devc_tt TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY.
    METHODS:
      get_packages
        RETURNING
          VALUE(rt_devclass) TYPE ty_devc_tt,
      drop_otr_short
        IMPORTING
          is_head TYPE sotr_head
        RAISING
          zcx_abapgit_exception,
      drop_otr_long
        IMPORTING
          is_headu TYPE sotr_headu
        RAISING
          zcx_abapgit_exception,
      list_packages RAISING zcx_abapgit_exception,
      list_objects,
      list_logs,
      list_otr,
      list_transports.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD run.
    CASE abap_true.
      WHEN p_list.
        check_packages( ).
        list( ).
      WHEN p_uninst.
        check_packages( ).
        IF p_pack = abap_true.
          drop_packages( ).
        ELSEIF p_obj = abap_true.
          drop_objects( ).
        ELSEIF p_otr = abap_true.
          drop_otr( ).
        ELSEIF p_log = abap_true.
          drop_logs( ).
        ELSE.
          uninstall_repos( ).
        ENDIF.
      WHEN p_trrel.
        release_transports( ).
    ENDCASE.
  ENDMETHOD.

  METHOD list.

    list_packages( ).

    list_objects( ).

    list_otr( ).

    list_logs( ).

    list_transports( ).

  ENDMETHOD.

  METHOD list_packages.

    DATA li_repo_online TYPE REF TO zcl_abapgit_repo_online.

    DATA(lv_found) = abap_false.

    SELECT devclass FROM tdevc INTO TABLE @DATA(lt_devclass) WHERE devclass IN @s_pack[] ORDER BY devclass.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Packages:', AT c_count lines( lt_devclass ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    IF sy-subrc = 0.
      LOOP AT lt_devclass INTO DATA(lv_devclass).
        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
        WRITE: AT /5 lv_devclass HOTSPOT, AT c_width space.
        gv_package = lv_devclass.
        HIDE gv_package.
        FORMAT COLOR OFF INTENSIFIED ON.
      ENDLOOP.
    ELSE.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
    ENDIF.
    SKIP.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Repositories:', AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    DATA(li_repo_srv) = zcl_abapgit_repo_srv=>get_instance( ).

    LOOP AT li_repo_srv->list( ) INTO DATA(li_repo).
      TRY.
          IF li_repo->get_package( ) IN s_pack.
            lv_found = abap_true.
            FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
            WRITE: AT /5 'Repository:',
              li_repo->get_name( ), li_repo->get_package( ), AT c_width space.
            FORMAT COLOR OFF INTENSIFIED ON.
            SKIP.
          ELSEIF li_repo->is_offline( ) = abap_false.
            " Show test repos that were cloned to a non-CI package (needs to be uninstalled manually)
            li_repo_online ?= li_repo.
            IF li_repo_online->get_url( ) CS 'abapGit-test'.
              lv_found = abap_true.
              FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
              WRITE: AT /5 'Repository:',
                li_repo->get_name( ), li_repo->get_package( ) COLOR COL_NEGATIVE, AT c_width space.
              FORMAT COLOR OFF INTENSIFIED ON.
              SKIP.
            ENDIF.
          ENDIF.
        CATCH zcx_abapgit_exception.
      ENDTRY.
    ENDLOOP.

    IF lv_found = abap_false.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
      SKIP.
    ENDIF.

  ENDMETHOD.

  METHOD list_objects.

    SELECT * FROM tadir INTO TABLE @DATA(lt_tadir) WHERE devclass IN @s_pack[]
      ORDER BY devclass, pgmid, object, obj_name.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Objects:', AT c_count lines( lt_tadir ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    LOOP AT lt_tadir INTO DATA(ls_tadir).
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      WRITE: AT /5 ls_tadir-object, ls_tadir-obj_name HOTSPOT,
        ls_tadir-delflag COLOR COL_TOTAL, ls_tadir-devclass, AT c_width space.
      gs_item-obj_type = ls_tadir-object.
      gs_item-obj_name = ls_tadir-obj_name.
      HIDE gs_item.
      FORMAT COLOR OFF INTENSIFIED ON.
    ENDLOOP.

    IF sy-subrc <> 0.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
    ENDIF.
    SKIP.

  ENDMETHOD.

  METHOD list_logs.

    SELECT * FROM wwwdata INTO TABLE @DATA(lt_logs)
      WHERE objid LIKE @zcl_abapgit_ci_log=>co_all
      ORDER BY PRIMARY KEY.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Logs:', AT c_count lines( lt_logs ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    LOOP AT lt_logs INTO DATA(ls_log).
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      WRITE: AT /5 ls_log-objid, ls_log-text, AT c_width space.
      FORMAT COLOR OFF INTENSIFIED ON.
    ENDLOOP.

    IF sy-subrc <> 0.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
    ENDIF.
    SKIP.

  ENDMETHOD.

  METHOD list_otr.

    SELECT * FROM sotr_head INTO TABLE @DATA(lt_head) WHERE paket IN @s_pack[]
      ORDER BY paket, concept.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Short Texts:', AT c_count lines( lt_head ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    LOOP AT lt_head INTO DATA(ls_head).
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      WRITE: AT /5 ls_head-concept, ls_head-paket, AT c_width space.
      SELECT * FROM sotr_use INTO @DATA(ls_use) WHERE concept = @ls_head-concept ORDER BY PRIMARY KEY.
        WRITE: AT /10 ls_use-object, ls_use-obj_name(70) HOTSPOT, AT c_width space.
        gs_item-obj_type = ls_use-object.
        gs_item-obj_name = ls_use-obj_name.
        HIDE gs_item.
      ENDSELECT.
      SELECT * FROM sotr_text INTO @DATA(ls_text) WHERE concept = @ls_head-concept ORDER BY PRIMARY KEY.
        WRITE: AT /10 ls_text-langu, ls_text-lfd_num, ls_text-text(120), AT c_width space.
      ENDSELECT.
      FORMAT COLOR OFF INTENSIFIED ON.
    ENDLOOP.

    IF sy-subrc <> 0.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
    ENDIF.
    SKIP.

    SELECT * FROM sotr_headu INTO TABLE @DATA(lt_headu) WHERE paket IN @s_pack[]
      ORDER BY paket, concept.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Long Texts:', AT c_count lines( lt_headu ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    LOOP AT lt_headu INTO DATA(ls_headu).
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      WRITE: AT /5 ls_headu-concept, ls_headu-paket, AT c_width space.
      SELECT * FROM sotr_useu INTO @DATA(ls_useu) WHERE concept = @ls_headu-concept ORDER BY PRIMARY KEY.
        WRITE: AT /10 ls_useu-object, ls_useu-obj_name(70) HOTSPOT, AT c_width space.
        gs_item-obj_type = ls_useu-object.
        gs_item-obj_name = ls_useu-obj_name.
        HIDE gs_item.
      ENDSELECT.
      SELECT * FROM sotr_textu INTO @DATA(ls_textu) WHERE concept = @ls_headu-concept ORDER BY PRIMARY KEY.
        WRITE: AT /10 ls_textu-langu, ls_textu-lfd_num, ls_textu-text(120), AT c_width space.
      ENDSELECT.
      FORMAT COLOR OFF INTENSIFIED ON.
    ENDLOOP.

    IF sy-subrc <> 0.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
    ENDIF.
    SKIP.

    SELECT * FROM tadir INTO TABLE @DATA(lt_tadir) WHERE devclass IN @s_pack[] AND object = 'SOTR'
      ORDER BY PRIMARY KEY.

    FORMAT COLOR COL_KEY.
    WRITE: / 'SOTR Objects:', AT c_count lines( lt_tadir ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    LOOP AT lt_tadir INTO DATA(ls_tadir).
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      WRITE: AT /5 ls_tadir-object, ls_tadir-obj_name,
        ls_tadir-delflag COLOR COL_TOTAL, ls_tadir-devclass, AT c_width space.
      gs_item-obj_type = ls_tadir-object.
      gs_item-obj_name = ls_tadir-obj_name.
      HIDE gs_item.
      FORMAT COLOR OFF INTENSIFIED ON.
    ENDLOOP.

    IF sy-subrc <> 0.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
    ENDIF.
    SKIP.

  ENDMETHOD.

  METHOD list_transports.
    DATA:
      ls_ranges   TYPE trsel_ts_ranges,
      lt_requests TYPE trwbo_request_headers.

    ls_ranges-as4text = p_txt.
    ls_ranges-request_status = VALUE #( ( sign = 'I' option = 'EQ' low = 'D' ) ).
    ls_ranges-task_status = VALUE #( ( sign = 'I' option = 'EQ' low = 'D' ) ).

    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      IMPORTING
        et_requests = lt_requests
      CHANGING
        cs_ranges   = ls_ranges.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Transports:', AT c_count lines( lt_requests ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    LOOP AT lt_requests INTO DATA(ls_request).
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      IF ls_request-strkorr IS INITIAL.
        WRITE: AT /5 ls_request-trkorr HOTSPOT, ls_request-as4text, AT c_width space.
        gv_transport = ls_request-trkorr.
        HIDE gv_transport.
      ELSE.
        WRITE: AT /10 ls_request-trkorr, ls_request-as4text, AT c_width space.
        SKIP.
      ENDIF.
      FORMAT COLOR OFF INTENSIFIED ON.
    ENDLOOP.

    IF sy-subrc <> 0.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
    ENDIF.
    SKIP.

  ENDMETHOD.

  METHOD uninstall_repos.
    DATA: lv_transport TYPE trkorr,
          lo_repo      TYPE REF TO zcl_abapgit_repo.

    DATA(li_repo_srv) = zcl_abapgit_repo_srv=>get_instance( ).

    LOOP AT li_repo_srv->list( ) INTO DATA(li_repo).
      lo_repo ?= li_repo.

      IF lo_repo->get_package( ) NOT IN s_pack[].
        CONTINUE.
      ENDIF.

      DATA(ls_checks) = lo_repo->delete_checks( ).

      IF ls_checks-transport-required = abap_true.
        IF lv_transport IS INITIAL.
          lv_transport = create_transport( 'Clean-up Repositories' ).
        ENDIF.
        ls_checks-transport-transport = lv_transport.
      ENDIF.

      TRY.
          CASE abap_true.
            WHEN p_purge.
              WRITE: / |Purge { lo_repo->get_name( ) } in { lo_repo->get_package( ) }|.
              li_repo_srv->purge( 
                ii_repo   = lo_repo
                is_checks = ls_checks ).
              WRITE: / 'Purged' COLOR COL_POSITIVE.
            WHEN p_remov.
              WRITE: / |Delete { lo_repo->get_name( ) } in { lo_repo->get_package( ) }|.
              li_repo_srv->delete( lo_repo ).
              WRITE: / 'Deleted' COLOR COL_POSITIVE.
          ENDCASE.
        CATCH zcx_abapgit_exception INTO DATA(lx_ex).
          WRITE: / 'Error' COLOR COL_NEGATIVE, lx_ex->get_text( ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_packages.

    IF s_pack[] IS INITIAL.
      zcx_abapgit_exception=>raise( 'No selection for packages found' ).
    ENDIF.

    SELECT COUNT(*) FROM tadir INTO @DATA(lv_count)
      WHERE pgmid = 'R3TR' AND object = 'DEVC' AND obj_name IN @s_pack[]
      AND ( srcsystem = 'SAP' OR author = 'SAP' ).

    IF lv_count > 0.
      zcx_abapgit_exception=>raise( 'Selection contains standard SAP packages' ).
    ENDIF.

  ENDMETHOD.

  METHOD get_packages.
    SELECT devclass FROM tdevc INTO TABLE @rt_devclass WHERE devclass IN @s_pack[] ORDER BY devclass.

    SELECT obj_name FROM tadir APPENDING TABLE @rt_devclass
      WHERE pgmid = 'R3TR' AND object = 'DEVC' AND obj_name IN @s_pack[] ORDER BY PRIMARY KEY.

    SORT rt_devclass.
    DELETE ADJACENT DUPLICATES FROM rt_devclass.
  ENDMETHOD.

  METHOD drop_packages.
    DATA lv_transport TYPE trkorr.

    DATA(lt_devclass) = get_packages( ).

    FORMAT COLOR COL_KEY.
    WRITE: / 'Packages:', AT c_count lines( lt_devclass ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    LOOP AT lt_devclass INTO DATA(lv_devclass).
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      WRITE: AT /5 lv_devclass.

      SELECT COUNT(*) FROM tadir INTO @DATA(lv_count)
        WHERE pgmid = 'R3TR' AND object <> 'DEVC' AND object <> 'SOTR' AND object <> 'SOTS' AND devclass = @lv_devclass.

      IF lv_count = 0.
        TRY.
            IF lv_devclass(1) <> '$' AND lv_transport IS INITIAL.
              lv_transport = create_transport( 'Clean-up Packages' ).
            ENDIF.

            delete_package(
              iv_package   = lv_devclass
              iv_transport = lv_transport ).

            delete_tadir(
              iv_obj_type = 'DEVC'
              iv_obj_name = |{ lv_devclass }| ).

            WRITE: 'Deleted' COLOR COL_POSITIVE.
          CATCH zcx_abapgit_exception INTO DATA(lx_ex).
            WRITE: 'Error' COLOR COL_NEGATIVE, lx_ex->get_text( ).
        ENDTRY.

      ELSE.
        WRITE: 'Not empty' COLOR COL_TOTAL.
      ENDIF.
      WRITE AT c_width space.
      FORMAT COLOR OFF.
    ENDLOOP.

    IF sy-subrc <> 0.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
    ENDIF.

  ENDMETHOD.

  METHOD drop_objects.
    DATA lv_transport TYPE trkorr.

    DATA(lt_devclass) = get_packages( ).

    FORMAT COLOR COL_KEY.
    WRITE: / 'Packages:', AT c_count lines( lt_devclass ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    LOOP AT lt_devclass INTO DATA(lv_devclass).
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      WRITE: AT /5 lv_devclass, AT c_width space.
      FORMAT COLOR OFF.
      SKIP.

      IF lv_devclass(1) <> '$' AND lv_transport IS INITIAL.
        lv_transport = create_transport( 'Clean-up Objects' ).
      ENDIF.

      SELECT * FROM tadir INTO TABLE @DATA(lt_object)
        WHERE pgmid = 'R3TR' AND object <> 'DEVC'
          AND devclass = @lv_devclass
        ORDER BY PRIMARY KEY.

      LOOP AT lt_object INTO DATA(ls_object).
        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
        WRITE: AT /10 ls_object-object, ls_object-obj_name, ls_object-delflag.

        IF ls_object-delflag IS INITIAL.
          TRY.
              delete_object(
                iv_package   = lv_devclass
                iv_obj_type  = ls_object-object
                iv_obj_name  = ls_object-obj_name
                iv_transport = lv_transport ).

              WRITE: 'Deleted' COLOR COL_POSITIVE.
            CATCH zcx_abapgit_exception INTO DATA(lx_ex).
              WRITE: 'Error' COLOR COL_NEGATIVE, lx_ex->get_text( ).
          ENDTRY.

          delete_tadir(
            iv_obj_type = ls_object-object
            iv_obj_name = ls_object-obj_name ).
        ELSE.
          WRITE: 'Flagged for deletion' COLOR COL_POSITIVE.

          IF p_forc = abap_true.
            delete_tadir(
              iv_obj_type = ls_object-object
              iv_obj_name = ls_object-obj_name ).
          ENDIF.
        ENDIF.
        WRITE AT c_width space.
        FORMAT COLOR OFF.
      ENDLOOP.

      IF sy-subrc = 0.
        SKIP.
      ENDIF.
    ENDLOOP.

    IF sy-subrc <> 0.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
    ENDIF.

  ENDMETHOD.

  METHOD drop_otr.

    SELECT * FROM sotr_head INTO TABLE @DATA(lt_head) WHERE paket IN @s_pack[] ORDER BY paket, concept.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Short Texts:', AT c_count lines( lt_head ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    LOOP AT lt_head INTO DATA(ls_head).
      drop_otr_short( ls_head ).
    ENDLOOP.
    SKIP.

    IF sy-subrc <> 0.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
      SKIP.
    ENDIF.

    SELECT * FROM sotr_headu INTO TABLE @DATA(lt_headu) WHERE paket IN @s_pack ORDER BY paket, concept.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Long Texts:', AT c_count lines( lt_headu ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    LOOP AT lt_headu INTO DATA(ls_headu).
      drop_otr_long( ls_headu ).
    ENDLOOP.
    SKIP.

    IF sy-subrc <> 0.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
      SKIP.
    ENDIF.

    " Drop TADIR
    SELECT * FROM tadir INTO TABLE @DATA(lt_tadir) WHERE devclass IN @s_pack[] AND object = 'SOTR'
      ORDER BY PRIMARY KEY.

    FORMAT COLOR COL_KEY.
    WRITE: / 'SOTR Objects:', AT c_count lines( lt_tadir ), AT c_width space.
    FORMAT COLOR OFF.
    SKIP.

    LOOP AT lt_tadir INTO DATA(ls_tadir).
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      WRITE: AT /5 ls_tadir-object, ls_tadir-obj_name,
        ls_tadir-delflag COLOR COL_TOTAL, ls_tadir-devclass, AT c_width space.

      SELECT * FROM sotr_head INTO TABLE @lt_head WHERE paket = @ls_tadir-devclass ORDER BY PRIMARY KEY.
      IF sy-subrc <> 0.
        SELECT * FROM sotr_headu INTO TABLE @lt_headu WHERE paket = @ls_tadir-devclass ORDER BY PRIMARY KEY.
        IF sy-subrc <> 0.
          delete_tadir(
            iv_obj_type = ls_tadir-object
            iv_obj_name = ls_tadir-obj_name ).
        ELSE.
          WRITE: 'Error' COLOR COL_NEGATIVE, 'SOTR_HEADU not empty'.
        ENDIF.
      ELSE.
        WRITE: 'Error' COLOR COL_NEGATIVE, 'SOTR_HEAD not empty'.
      ENDIF.

      WRITE AT c_width space.
      FORMAT COLOR OFF.
    ENDLOOP.

    IF sy-subrc <> 0.
      FORMAT COLOR COL_POSITIVE.
      WRITE: AT /5 'None', AT c_width space.
    ENDIF.
    SKIP.

  ENDMETHOD.

  METHOD drop_otr_long.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    WRITE: AT /5 is_headu-concept, is_headu-paket, is_headu-crea_name, is_headu-crea_tstut.

    CALL FUNCTION 'BTFR_DELETE_SINGLE_TEXT'
      EXPORTING
        concept               = is_headu-concept
        flag_string           = abap_true
        flag_correction_entry = abap_false
      EXCEPTIONS
        text_not_found        = 1
        invalid_package       = 2
        text_not_changeable   = 3
        text_enqueued         = 4
        no_correction         = 5
        parameter_error       = 6
        OTHERS                = 7.
    IF sy-subrc <> 0.
      WRITE: 'Error' COLOR COL_NEGATIVE.
      IF p_forc = abap_true.
        DELETE FROM sotr_headu WHERE concept = @is_headu-concept.
        DELETE FROM sotr_textu WHERE concept = @is_headu-concept.
        DELETE FROM sotr_useu  WHERE concept = @is_headu-concept.
        DELETE FROM sotr_aliau WHERE concept = @is_headu-concept.
        WRITE: 'Force Deleted' COLOR COL_POSITIVE.
      ENDIF.
    ELSE.
      WRITE: 'Deleted' COLOR COL_POSITIVE.
    ENDIF.
    WRITE AT c_width space.
    FORMAT COLOR OFF.
  ENDMETHOD.

  METHOD drop_otr_short.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    WRITE: AT /5 is_head-concept, is_head-paket, is_head-crea_name, is_head-crea_tstut.

    CALL FUNCTION 'BTFR_DELETE_SINGLE_TEXT'
      EXPORTING
        concept               = is_head-concept
        flag_string           = abap_false
        flag_correction_entry = abap_false
      EXCEPTIONS
        text_not_found        = 1
        invalid_package       = 2
        text_not_changeable   = 3
        text_enqueued         = 4
        no_correction         = 5
        parameter_error       = 6
        OTHERS                = 7.
    IF sy-subrc <> 0.
      WRITE: 'Error' COLOR COL_NEGATIVE.
      IF p_forc = abap_true.
        DELETE FROM sotr_head WHERE concept = @is_head-concept.
        DELETE FROM sotr_text WHERE concept = @is_head-concept.
        DELETE FROM sotr_use  WHERE concept = @is_head-concept.
        DELETE FROM sotr_alia WHERE concept = @is_head-concept.
        WRITE: 'Force Deleted' COLOR COL_POSITIVE.
      ENDIF.
    ELSE.
      WRITE: 'Deleted' COLOR COL_POSITIVE.
    ENDIF.
    WRITE AT c_width space.
    FORMAT COLOR OFF.
  ENDMETHOD.

  METHOD drop_logs.
    DATA(lo_log) = NEW zcl_abapgit_ci_log( ).

    lo_log->drop_all( ).

    list_logs( ).
  ENDMETHOD.

  METHOD delete_package.
    DATA li_package TYPE REF TO if_package.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = iv_package
        i_force_reload             = abap_true
      IMPORTING
        e_package                  = li_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5
        OTHERS                     = 6 ).

    IF li_package IS NOT BOUND.
      RETURN.
    ENDIF.

    li_package->set_changeable(
      EXPORTING
        i_changeable                = abap_true
        i_suppress_dialog           = abap_true
      EXCEPTIONS
        object_locked_by_other_user = 1
        permission_failure          = 2
        object_already_changeable   = 3
        object_already_unlocked     = 4
        object_just_created         = 5
        object_deleted              = 6
        object_modified             = 7
        object_not_existing         = 8
        object_invalid              = 9
        unexpected_error            = 10
        OTHERS                      = 11 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    li_package->delete(
      EXPORTING
        i_suppress_dialog     = abap_true
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        intern_err            = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    li_package->save(
      EXPORTING
        i_suppress_dialog     = abap_true
        i_transport_request   = iv_transport
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        cancelled_in_corr     = 3
        permission_failure    = 4
        unexpected_error      = 5
        intern_err            = 6
        OTHERS                = 7 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD delete_object.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA li_obj TYPE REF TO zif_abapgit_object.

    ls_item-obj_type = iv_obj_type.
    ls_item-obj_name = iv_obj_name.

    IF zcl_abapgit_objects=>is_type_supported( ls_item-obj_type ) = abap_false.
      RETURN.
    ENDIF.

    DATA(lv_class_name) = 'ZCL_ABAPGIT_OBJECT_' && iv_obj_type.

    CREATE OBJECT li_obj TYPE (lv_class_name)
      EXPORTING
        is_item     = ls_item
        iv_language = zif_abapgit_definitions=>c_english.

    li_obj->delete( iv_package   = iv_package
                    iv_transport = iv_transport ).

  ENDMETHOD.

  METHOD delete_tadir.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_delete_tadir_entry          = abap_true
        wi_tadir_pgmid                 = 'R3TR'
        wi_tadir_object                = iv_obj_type
        wi_tadir_obj_name              = iv_obj_name
        wi_test_modus                  = abap_false
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
    IF ( sy-subrc <> 0 AND p_forc = abap_true ) OR sy-subrc = 9.
      DELETE FROM tadir
        WHERE pgmid = 'R3TR' AND object = @iv_obj_type AND obj_name = @iv_obj_name.
    ENDIF.

  ENDMETHOD.

  METHOD create_transport.

    DATA ls_request_header TYPE trwbo_request_header.

    " Reset standard request (to avoid confusion)
    zcl_abapgit_default_transport=>get_instance( )->reset( ).

    " Get transport via popup or create one automatically
    IF p_popup = abap_true.
      rv_transport = zcl_abapgit_ui_factory=>get_popups( )->popup_transport_request(
        VALUE #( request = 'K' task = 'S' ) ).
    ELSE.
      DATA(lv_text) = |abapGit CI { iv_text }|.

      CALL FUNCTION 'TR_INSERT_REQUEST_WITH_TASKS'
        EXPORTING
          iv_type           = 'K'
          iv_text           = CONV as4text( lv_text )
          it_users          = VALUE scts_users( ( user = cl_abap_syst=>get_user_name( ) type = 'S' ) )
        IMPORTING
          es_request_header = ls_request_header
        EXCEPTIONS
          insert_failed     = 1
          enqueue_failed    = 2
          OTHERS            = 3.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      rv_transport = ls_request_header-trkorr.
    ENDIF.

  ENDMETHOD.

  METHOD release_transports.
    DATA: ls_ranges   TYPE trsel_ts_ranges,
          lt_requests TYPE trwbo_request_headers,
          lv_msg_text TYPE string.

    ls_ranges-as4text = p_txt.
    ls_ranges-request_status = VALUE #( ( sign = 'I' option = 'EQ' low = 'D' ) ).
    ls_ranges-task_status = VALUE #( ( sign = 'I' option = 'EQ' low = 'D' ) ).

    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      IMPORTING
        et_requests = lt_requests
      CHANGING
        cs_ranges   = ls_ranges.

    IF lines( lt_requests ) = 0.
      zcx_abapgit_exception=>raise( 'No transport requests found' ).
    ENDIF.

    IF p_prev = abap_true.
      cl_demo_output=>display( lt_requests ).
      IF zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
           iv_titlebar      = 'Confirm release'
           iv_text_question = |Release all { lines( lt_requests ) } requests / tasks?|
         ) <> '1'.
        zcx_abapgit_exception=>raise( 'Cancelled by user' ).
      ENDIF.
    ENDIF.

    LOOP AT lt_requests ASSIGNING FIELD-SYMBOL(<ls_request>) WHERE trfunction = 'S' AND trstatus <> 'R'.
      WRITE: / |Releasing task { <ls_request>-trkorr }|.
      CALL FUNCTION 'TR_RELEASE_REQUEST'
        EXPORTING
          iv_trkorr                  = <ls_request>-trkorr
          iv_dialog                  = abap_false
          iv_success_message         = abap_false
        EXCEPTIONS
          cts_initialization_failure = 1
          enqueue_failed             = 2
          no_authorization           = 3
          invalid_request            = 4
          request_already_released   = 5
          repeat_too_early           = 6
          error_in_export_methods    = 7
          object_check_error         = 8
          docu_missing               = 9
          db_access_error            = 10
          action_aborted_by_user     = 11
          export_failed              = 12
          OTHERS                     = 13.
      IF sy-subrc = 0.
        WRITE: AT 40 'Released' COLOR COL_POSITIVE.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO lv_msg_text.
        WRITE: / 'Error:' COLOR COL_NEGATIVE, lv_msg_text.

        force_release_transport( <ls_request>-trkorr ).
      ENDIF.
    ENDLOOP.

    LOOP AT lt_requests ASSIGNING <ls_request> WHERE trfunction = 'K'.
      WRITE: / |Releasing request { <ls_request>-trkorr }|.
      CALL FUNCTION 'TR_RELEASE_REQUEST'
        EXPORTING
          iv_trkorr                  = <ls_request>-trkorr
          iv_dialog                  = abap_false
          iv_success_message         = abap_false
        EXCEPTIONS
          cts_initialization_failure = 1
          enqueue_failed             = 2
          no_authorization           = 3
          invalid_request            = 4
          request_already_released   = 5
          repeat_too_early           = 6
          error_in_export_methods    = 7
          object_check_error         = 8
          docu_missing               = 9
          db_access_error            = 10
          action_aborted_by_user     = 11
          export_failed              = 12
          OTHERS                     = 13.
      IF sy-subrc = 0.
        WRITE: AT 40 'Released' COLOR COL_POSITIVE.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO lv_msg_text.
        WRITE: / 'Error:' COLOR COL_NEGATIVE, lv_msg_text.

        force_release_transport( <ls_request>-trkorr ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD force_release_transport.

    DATA:
      lt_messages TYPE ctsgerrmsgs,
      lv_msg_text TYPE string.

    IF p_force = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'CTS_LOCK_TRKORR'
      EXPORTING
        iv_trkorr = iv_trkorr
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc = 1.
      WRITE: / 'Error:' COLOR COL_NEGATIVE, |Transport { iv_trkorr } can't be locked|.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TRINT_RELEASE_REQUEST'
      EXPORTING
        iv_trkorr                   = iv_trkorr
        iv_dialog                   = abap_false
        iv_success_message          = abap_false
        iv_without_objects_check    = abap_true
        iv_without_locking          = abap_true
      IMPORTING
        et_messages                 = lt_messages
      EXCEPTIONS
        cts_initialization_failure  = 1
        enqueue_failed              = 2
        no_authorization            = 3
        invalid_request             = 4
        request_already_released    = 5
        repeat_too_early            = 6
        object_lock_error           = 7
        object_check_error          = 8
        docu_missing                = 9
        db_access_error             = 10
        action_aborted_by_user      = 11
        export_failed               = 12
        execute_objects_check       = 13
        release_in_bg_mode          = 14
        release_in_bg_mode_w_objchk = 15
        error_in_export_methods     = 16
        object_lang_error           = 17.

    DATA(lv_subrc) = sy-subrc.

    CALL FUNCTION 'CTS_UNLOCK_TRKORR'
      EXPORTING
        iv_trkorr = iv_trkorr
      EXCEPTIONS
        OTHERS    = 0.

    IF lv_subrc <> 0.
      LOOP AT lt_messages INTO DATA(ls_message).
        MESSAGE ID ls_message-msgid TYPE ls_message-msgty NUMBER ls_message-msgno
                WITH ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                INTO lv_msg_text.
        WRITE: / 'Error:' COLOR COL_NEGATIVE, lv_msg_text.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  s_pack[] = VALUE #( ( sign = 'I' option = 'CP' low = 'Z___*' )
                      ( sign = 'I' option = 'CP' low = '$___*' ) ).

AT LINE-SELECTION.
  IF gv_transport IS NOT INITIAL.
    CALL FUNCTION 'TR_DISPLAY_REQUEST'
      EXPORTING
        i_trkorr = gv_transport.
  ELSEIF gs_item IS NOT INITIAL.
    TRY.
        zcl_abapgit_objects=>jump( gs_item ).
      CATCH zcx_abapgit_exception INTO gx_ex.
        MESSAGE gx_ex TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDIF.

  CLEAR: gv_transport, gv_package, gs_item.

START-OF-SELECTION.
  TRY.
      NEW lcl_main( )->run( ).
    CATCH zcx_abapgit_exception INTO gx_ex.
      WRITE: / gx_ex->get_text( ).
      MESSAGE gx_ex TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

  CLEAR: gv_transport, gv_package, gs_item.
