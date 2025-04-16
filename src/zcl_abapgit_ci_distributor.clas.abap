CLASS zcl_abapgit_ci_distributor DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_url     TYPE string
        !iv_save    TYPE abap_bool DEFAULT abap_false
        !iv_history TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .

    METHODS push_to_git_repo
      IMPORTING
        !is_result TYPE zif_abapgit_ci_definitions=>ty_result
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      co_package           TYPE devclass VALUE '$_ABAPGIT_CI_RESULTS',
      co_mime_public       TYPE string VALUE '/SAP/PUBLIC',
      co_mime_abapgit_ci   TYPE string VALUE 'ZABAPGIT_CI',
      co_mime_history      TYPE string VALUE 'HISTORY',
      co_mime_result_html  TYPE string VALUE 'abapGit_CI_result.html',
      co_mime_result_json  TYPE string VALUE 'abapGit_CI_result.json',
      co_mime_history_html TYPE string VALUE 'result_&.html',
      co_mime_history_json TYPE string VALUE 'result_&.json'.

    DATA:
      mv_history TYPE abap_bool,
      mv_save    TYPE abap_bool,
      mv_url     TYPE string.

    METHODS:
      get_repo
        RETURNING
          VALUE(ro_repo) TYPE REF TO zcl_abapgit_repo_online
        RAISING
          zcx_abapgit_exception,

      create_package
        RAISING
          zcx_abapgit_exception,

      save_results_in_mime_repo
        IMPORTING
          is_result TYPE zif_abapgit_ci_definitions=>ty_result
        RAISING
          zcx_abapgit_exception,

      stage
        IMPORTING
          io_repo         TYPE REF TO zcl_abapgit_repo_online
        RETURNING
          VALUE(ro_stage) TYPE REF TO zcl_abapgit_stage
        RAISING
          zcx_abapgit_exception,

      create_mime_folder
        IMPORTING
          !iv_folder      TYPE string
          !iv_description TYPE string
          !iv_loio        TYPE smimloio-loio_id
        RAISING
          zcx_abapgit_exception,

      add_file_to_mime_repo
        IMPORTING
          !iv_url         TYPE string
          !iv_description TYPE string
          !iv_data        TYPE string
        RAISING
          zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_ci_distributor IMPLEMENTATION.


  METHOD add_file_to_mime_repo.

    TRY.
        DATA(lv_xstring) = cl_bcs_convert=>string_to_xstring( iv_data ).

      CATCH cx_bcs INTO DATA(lx_bcs).
        zcx_abapgit_exception=>raise_with_text( lx_bcs ).
    ENDTRY.

    DATA(lo_mime_api) = cl_mime_repository_api=>get_api( ).

    lo_mime_api->put(
      EXPORTING
        i_url                   = iv_url
        i_content               = lv_xstring
        i_description           = zif_abapgit_ci_definitions=>co_title && iv_description
        i_dev_package           = co_package
      EXCEPTIONS
        parameter_missing       = 1
        error_occured           = 2
        cancelled               = 3
        permission_failure      = 4
        data_inconsistency      = 5
        new_loio_already_exists = 6
        is_folder               = 7
        OTHERS                  = 8 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    IF iv_url IS INITIAL.
      zcx_abapgit_exception=>raise( |Please supply Git repository URL| ).
    ENDIF.

    mv_url = iv_url.
    mv_save = iv_save.
    mv_history = iv_history.

  ENDMETHOD.


  METHOD create_mime_folder.

    DATA ls_loio TYPE skwf_io.

    SELECT SINGLE loio_id FROM smimloio INTO @ls_loio-objid WHERE prop09 = @iv_folder.
    IF sy-subrc <> 0.

      " Create folders with hardcoded IDs (otherwise system assigns new IDs which lead to diffs)
      ls_loio-objtype = 'F'. "folder
      ls_loio-class   = 'M_FOLDER'.
      ls_loio-objid   = iv_loio.

      DATA(lo_mime_api) = cl_mime_repository_api=>get_api( ).

      lo_mime_api->create_folder(
        EXPORTING
          i_url              = co_mime_public && iv_folder
          i_description      = zif_abapgit_ci_definitions=>co_title && iv_description
          i_dev_package      = co_package
          i_folder_loio      = ls_loio
        EXCEPTIONS
          parameter_missing  = 1
          error_occured      = 2
          cancelled          = 3
          permission_failure = 4
          folder_exists      = 5
          OTHERS             = 6 ).
      IF sy-subrc <> 5 AND sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD create_package.

    DATA(lo_package) = zcl_abapgit_factory=>get_sap_package( co_package ).

    IF lo_package->exists( ) = abap_false.

      lo_package->create( VALUE #(
                            as4user  = sy-uname
                            devclass = co_package
                            ctext    = zif_abapgit_ci_definitions=>co_title
                          ) ).

    ENDIF.

  ENDMETHOD.


  METHOD get_repo.

    DATA: lo_repo_online TYPE REF TO zcl_abapgit_repo_online.

    DATA(lt_list) = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<lo_repo>).

      IF <lo_repo>->is_offline( ) = abap_false.

        lo_repo_online ?= <lo_repo>.

        IF lo_repo_online->get_url( ) = mv_url.

          ro_repo = lo_repo_online.
          RETURN.

        ENDIF.

      ENDIF.

    ENDLOOP.

    create_package( ).

    create_mime_folder(
      iv_folder      = co_mime_abapgit_ci
      iv_description = ''
      iv_loio        = '0242AC1100021EEBB1BAD91FD0049E35' ).

    create_mime_folder(
      iv_folder      = co_mime_history
      iv_description = ': History'
      iv_loio        = '0242AC1100021EEBB1BAAF746A2D3E35' ).

    ro_repo ?= zcl_abapgit_repo_srv=>get_instance( )->new_online(
      iv_url         = mv_url
      iv_branch_name = 'refs/heads/main'
      iv_package     = co_package ).

  ENDMETHOD.


  METHOD push_to_git_repo.

    DATA:
      ls_comment  TYPE zif_abapgit_git_definitions=>ty_comment,
      lv_timezone TYPE timezone.

    DATA(lo_repo) = get_repo( ).

    TRY.
        DATA(ls_checks) = zcl_abapgit_ci_repo_check=>get( lo_repo ).
      CATCH zcx_abapgit_cancel INTO DATA(lx_cancel).
        zcx_abapgit_exception=>raise_with_text( lx_cancel ).
    ENDTRY.

    lo_repo->deserialize(
      is_checks = ls_checks
      ii_log    = NEW zcl_abapgit_log( ) ).

    save_results_in_mime_repo( is_result ).

    " Save without pushing results to repo
    IF mv_save = abap_true.
      RETURN.
    ENDIF.

    DATA(lo_stage) = stage( lo_repo ).

    DATA(lo_user) = zcl_abapgit_user_record=>get_instance( ).

    ls_comment-committer-name  = lo_user->get_name( sy-uname ).
    ls_comment-committer-email = lo_user->get_email( sy-uname ).

    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = lv_timezone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CONVERT TIME STAMP is_result-statistics-finish_timestamp
      TIME ZONE lv_timezone
      INTO DATE DATA(lv_start_date)
           TIME DATA(lv_start_time).

    ls_comment-comment = |{ zif_abapgit_ci_definitions=>co_title }|
                      && | from { lv_start_date DATE = USER } { lv_start_time TIME = USER }|.

    lo_repo->push( is_comment = ls_comment
                   io_stage   = lo_stage ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD save_results_in_mime_repo.

    " Get results as JSON and HTML
    TRY.
        DATA(lo_json) = zcl_abapgit_ajson=>create_empty( ).

        lo_json->set(
          iv_path = '/'
          iv_val  = is_result ).

        DATA(lv_json) = lo_json->stringify( 2 ).
      CATCH zcx_abapgit_ajson_error INTO DATA(lx_error).
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    DATA(lv_html) = NEW zcl_abapgit_ci_html( is_result )->render( ).

    " Save results to MIME repo (overwrites previous results)
    DATA(lv_path) = co_mime_public && '/' && co_mime_abapgit_ci && '/'.

    add_file_to_mime_repo(
      iv_description = ` (JSON)`
      iv_data        = lv_json
      iv_url         = lv_path && co_mime_result_json ).

    add_file_to_mime_repo(
      iv_description = ` (HTML)`
      iv_data        = lv_html
      iv_url         = lv_path && co_mime_result_html ).

    " Add results to history folder
    IF mv_history = abap_true.
      lv_path = lv_path && co_mime_history && '/'.

      DATA(lv_html_url) = replace(
        val  = co_mime_history_html
        sub  = '&'
        with = |{ sy-datum DATE = ISO }| ).

      add_file_to_mime_repo(
        iv_description = |: { sy-datum DATE = ISO } (HTML)|
        iv_data        = lv_html
        iv_url         = lv_path && lv_html_url ).

      DATA(lv_json_url) = replace(
        val  = co_mime_history_json
        sub  = '&'
        with = |{ sy-datum DATE = ISO }| ).

      add_file_to_mime_repo(
        iv_description = |: { sy-datum DATE = ISO } (JSON)|
        iv_data        = lv_json
        iv_url         = lv_path && lv_json_url ).
    ENDIF.

  ENDMETHOD.


  METHOD stage.

    ro_stage = NEW zcl_abapgit_stage( ).
    DATA(ls_files) = zcl_abapgit_stage_logic=>get_stage_logic( )->get( io_repo ).

    LOOP AT ls_files-local ASSIGNING FIELD-SYMBOL(<ls_local>).

      ro_stage->add(
          iv_path     = <ls_local>-file-path
          iv_filename = <ls_local>-file-filename
          iv_data     = <ls_local>-file-data ).

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
