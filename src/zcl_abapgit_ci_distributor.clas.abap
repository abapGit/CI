CLASS zcl_abapgit_ci_distributor DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_url TYPE string
        RAISING
          zcx_abapgit_exception,

      push_to_git_repo
        IMPORTING
          is_result TYPE zif_abapgit_ci_definitions=>ty_result
        RAISING
          zcx_abapgit_exception.


  PRIVATE SECTION.
    CONSTANTS:
      co_package TYPE devclass VALUE '$_ABAPGIT_CI_RESULTS' ##NO_TEXT.

    DATA:
      mv_url TYPE string.

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
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_ci_distributor IMPLEMENTATION.

  METHOD constructor.

    IF iv_url IS INITIAL.
      zcx_abapgit_exception=>raise( |Please supply git repo url| ).
    ENDIF.

    mv_url = iv_url.

  ENDMETHOD.

  METHOD push_to_git_repo.

    DATA:
      ls_comment  TYPE zif_abapgit_definitions=>ty_comment,
      lv_timezone TYPE timezone.

    DATA(lo_repo) = get_repo( ).

    DATA(ls_checks) = lo_repo->deserialize_checks( ).

    LOOP AT ls_checks-overwrite ASSIGNING FIELD-SYMBOL(<ls_overwrite>).
      <ls_overwrite>-decision = abap_true.
    ENDLOOP.

    lo_repo->deserialize( ls_checks ).

    save_results_in_mime_repo( is_result ).

    DATA(lo_stage) = stage( lo_repo ).

    DATA(lo_user) = zcl_abapgit_user_master_record=>get_instance( sy-uname ).

    ls_comment-committer-name  = lo_user->get_name( ).
    ls_comment-committer-email = lo_user->get_email( ).

    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = lv_timezone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CONVERT TIME STAMP is_result-timestamp
      TIME ZONE lv_timezone
      INTO DATE DATA(start_date)
           TIME DATA(start_time).

    ls_comment-comment = |abapGit CI results from { start_date DATE = USER } { start_time TIME = USER }|.

    lo_repo->push( is_comment = ls_comment
                   io_stage   = lo_stage ).

  ENDMETHOD.


  METHOD get_repo.

    DATA: lo_repo_online TYPE REF TO zcl_abapgit_repo_online.

    DATA(lt_list) = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<repo>).

      IF <repo>->is_offline( ) = abap_false.

        lo_repo_online ?= <repo>.

        IF lo_repo_online->get_url( ) = mv_url.

          ro_repo = lo_repo_online.
          RETURN.

        ENDIF.

      ENDIF.

    ENDLOOP.

    create_package( ).

    ro_repo ?= zcl_abapgit_repo_srv=>get_instance( )->new_online(
        iv_url         = mv_url
        iv_branch_name = 'refs/heads/master'
        iv_package     = co_package
    ).

  ENDMETHOD.


  METHOD create_package.

    DATA(lo_package) = zcl_abapgit_factory=>get_sap_package( co_package ).

    IF lo_package->exists( ) = abap_false.

      lo_package->create( VALUE #(
                            as4user  = sy-uname
                            devclass = co_package
                            ctext    = |abapGit CI run results|
                          ) ).

    ENDIF.

  ENDMETHOD.


  METHOD save_results_in_mime_repo.

    DATA(lo_mime_api) = cl_mime_repository_api=>get_api( ).

    DATA(json) = /ui2/cl_json=>serialize( is_result ).


    TRY.
        DATA(xstring) = cl_bcs_convert=>string_to_xstring( json ).

      CATCH cx_bcs INTO DATA(lx_bcs).
        zcx_abapgit_exception=>raise( iv_text     = lx_bcs->get_text( )
                                      ix_previous = lx_bcs ).
    ENDTRY.

    lo_mime_api->put(
      EXPORTING
        i_url                   = '/SAP/PUBLIC/abapGit_CI_result.json'
        i_content               = xstring
        i_description           = 'abapGit CI results'
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

    TRY.
        xstring = cl_bcs_convert=>string_to_xstring( NEW zcl_abapgit_ci_html( is_result )->render( ) ).

      CATCH cx_bcs INTO lx_bcs.
        zcx_abapgit_exception=>raise( iv_text     = lx_bcs->get_text( )
                                      ix_previous = lx_bcs ).
    ENDTRY.

    lo_mime_api->put(
      EXPORTING
        i_url                   = '/SAP/PUBLIC/abapGit_CI_result.html'
        i_content               = xstring
        i_description           = 'abapGit CI results'
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


  METHOD stage.

    ro_stage = NEW zcl_abapgit_stage( ).
    DATA(ls_files) = zcl_abapgit_factory=>get_stage_logic( )->get( io_repo ).

    LOOP AT ls_files-local ASSIGNING FIELD-SYMBOL(<ls_local>).

      ro_stage->add(
          iv_path     = <ls_local>-file-path
          iv_filename = <ls_local>-file-filename
          iv_data     = <ls_local>-file-data ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
