CLASS zcl_abapgit_ci_repos DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      gc_default_branch TYPE string VALUE 'refs/heads/main'.

    CLASS-METHODS:
      update_abapgit_repo
        RAISING
          zcx_abapgit_exception,

      update_abapgit_ci_repo
        RAISING
          zcx_abapgit_exception,

      update_repository
        IMPORTING
          iv_repo_name TYPE string
          iv_branch    TYPE string DEFAULT gc_default_branch
        RAISING
          zcx_abapgit_exception.

    METHODS:
      constructor
        IMPORTING
          !iv_sync TYPE abap_bool OPTIONAL,

      process_repos
        IMPORTING
          !it_repos             TYPE zif_abapgit_ci_definitions=>tty_repo
          !is_options           TYPE zif_abapgit_ci_definitions=>ty_repo_check_options
        RETURNING
          VALUE(rt_result_list) TYPE zif_abapgit_ci_definitions=>ty_result-repo_result_list.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_sync TYPE abap_bool.

    CLASS-METHODS:
      update_repo
        IMPORTING
          iv_repo_name TYPE string
          iv_branch    TYPE string DEFAULT gc_default_branch
        RAISING
          zcx_abapgit_exception,

      syntax_check
        IMPORTING
          iv_package TYPE devclass
        RAISING
          zcx_abapgit_exception.

    METHODS:
      process_repo
        CHANGING
          cs_ci_repo TYPE zabapgit_ci_result
        RAISING
          zcx_abapgit_exception,
      get_repo_list_with_packages
        IMPORTING
          it_repos        TYPE zif_abapgit_ci_definitions=>tty_repo
          is_options      TYPE zif_abapgit_ci_definitions=>ty_repo_check_options
        RETURNING
          VALUE(rt_repos) TYPE zif_abapgit_ci_definitions=>tty_repo_result_list.

ENDCLASS.



CLASS zcl_abapgit_ci_repos IMPLEMENTATION.


  METHOD constructor.
    mv_sync = iv_sync.
  ENDMETHOD.


  METHOD get_repo_list_with_packages.
    FIELD-SYMBOLS: <ls_ci_repo> TYPE zabapgit_ci_result.

    " Copy it_repos into rt_repos, creating an entry for local and transportable packages each

    DATA(lo_skip) = NEW zcl_abapgit_ci_skip( ).
    DATA(lo_repo_cat) = NEW zcl_abapgit_ci_repo_category( ).

    LOOP AT it_repos ASSIGNING FIELD-SYMBOL(<ls_repo>).

      DATA(lv_category) = lo_repo_cat->get_repo_category( <ls_repo>-name ).

      IF NOT lv_category IN is_options-categories.
        CONTINUE.
      ENDIF.

      IF is_options-check_local = abap_true.
        INSERT CORRESPONDING #( <ls_repo> )
               INTO TABLE rt_repos
               ASSIGNING <ls_ci_repo>.
        <ls_ci_repo>-package      = CONV devclass( |$___{ to_upper( <ls_ci_repo>-name ) }| ).
        <ls_ci_repo>-do_not_purge = is_options-no_purge.
        <ls_ci_repo>-category     = lo_repo_cat->get_category_label( lv_category ).

        lo_skip->complete_skip_components( CHANGING cs_repo = <ls_ci_repo> ).
      ENDIF.

      IF is_options-check_transportable = abap_true.
        INSERT CORRESPONDING #( <ls_repo> )
               INTO TABLE rt_repos
               ASSIGNING <ls_ci_repo>.
        <ls_ci_repo>-package      = CONV devclass( |Z___{ to_upper( <ls_ci_repo>-name ) }| ).
        <ls_ci_repo>-layer        = is_options-layer.
        <ls_ci_repo>-do_not_purge = is_options-no_purge.
        <ls_ci_repo>-category     = lo_repo_cat->get_category_label( lv_category ).

        lo_skip->complete_skip_components( CHANGING cs_repo = <ls_ci_repo> ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD process_repo.
    DATA lv_message TYPE c LENGTH 255.
    DATA lv_errid TYPE c LENGTH 30.

    " You should remember that we process the repo in synchron RFC because of
    " shortdumps there doesn't crash the main process.

    ASSERT cs_ci_repo-package IS NOT INITIAL.

    IF mv_sync = abap_true.
      " Synchronous for debugging
      CALL FUNCTION 'ZABAPGIT_CI_PROCESS_REPO'
        CHANGING
          cs_ci_repo = cs_ci_repo.
    ELSE.
      " Asynchronous for mass processing
      CALL FUNCTION 'ZABAPGIT_CI_PROCESS_REPO'
        DESTINATION 'NONE'
        CHANGING
          cs_ci_repo            = cs_ci_repo
        EXCEPTIONS
          communication_failure = 1 MESSAGE lv_message
          system_failure        = 2 MESSAGE lv_message
          OTHERS                = 3.
    ENDIF.

    IF sy-subrc <> 0.
      " Try to get error code for dump
      " (doesn't work if description contains parameters)
      SELECT SINGLE errid FROM snapt INTO @lv_errid
        WHERE langu = @sy-langu AND ttype = 'K' AND seqno = '0001' AND tline = @lv_message.
      IF sy-subrc <> 0.
        lv_errid = 'DUMP'.
      ENDIF.
      cs_ci_repo-message = |Failure processing repo: { lv_errid } { lv_message }|.
      cs_ci_repo-status  = zif_abapgit_ci_definitions=>co_status-not_ok.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD process_repos.

    DATA:
      lv_start_timestamp  TYPE timestampl,
      lv_finish_timestamp TYPE timestampl.

    rt_result_list = get_repo_list_with_packages( it_repos = it_repos is_options = is_options ).

    LOOP AT rt_result_list ASSIGNING FIELD-SYMBOL(<ls_ci_repo>).
      IF <ls_ci_repo>-skip = abap_true.
        <ls_ci_repo>-status = zif_abapgit_ci_definitions=>co_status-skipped.
        CONTINUE.
      ENDIF.

      IF sy-batch = abap_true.
        DATA(lv_counter) = |{ sy-tabix } of { lines( rt_result_list ) }:|.
        MESSAGE |{ lv_counter } Start processing repo { <ls_ci_repo>-name } { <ls_ci_repo>-package }| TYPE 'I'.
      ENDIF.

      GET TIME STAMP FIELD lv_start_timestamp.

      TRY.
          process_repo(
            CHANGING
              cs_ci_repo = <ls_ci_repo> ).

        CATCH zcx_abapgit_exception INTO DATA(lx_error).
          <ls_ci_repo>-status  = zif_abapgit_ci_definitions=>co_status-not_ok.
          <ls_ci_repo>-message = lx_error->get_text( ).
      ENDTRY.

      IF <ls_ci_repo>-create_package = zif_abapgit_ci_definitions=>co_status-not_ok
      OR <ls_ci_repo>-clone          = zif_abapgit_ci_definitions=>co_status-not_ok
      OR <ls_ci_repo>-pull           = zif_abapgit_ci_definitions=>co_status-not_ok
      OR <ls_ci_repo>-syntax_check   = zif_abapgit_ci_definitions=>co_status-not_ok
      OR <ls_ci_repo>-purge          = zif_abapgit_ci_definitions=>co_status-not_ok
      OR <ls_ci_repo>-status         = zif_abapgit_ci_definitions=>co_status-not_ok
      OR ( <ls_ci_repo>-layer IS NOT INITIAL AND
           ( <ls_ci_repo>-check_create_transport = zif_abapgit_ci_definitions=>co_status-not_ok OR
             <ls_ci_repo>-check_delete_transport = zif_abapgit_ci_definitions=>co_status-not_ok ) ).

        <ls_ci_repo>-status = zif_abapgit_ci_definitions=>co_status-not_ok.

      ELSE.

        <ls_ci_repo>-status = zif_abapgit_ci_definitions=>co_status-ok.

      ENDIF.

      GET TIME STAMP FIELD lv_finish_timestamp.

      <ls_ci_repo>-duration = cl_abap_timestamp_util=>get_instance( )->tstmpl_seconds_between(
        iv_timestamp0 = lv_start_timestamp
        iv_timestamp1 = lv_finish_timestamp ).

      IF sy-batch = abap_true.
        MESSAGE |Runtime: { <ls_ci_repo>-duration } seconds| TYPE 'I'.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD syntax_check.

    DATA(lt_list) = zcl_abapgit_factory=>get_code_inspector( iv_package )->run( 'SYNTAX_CHECK' ).

    ASSIGN lt_list[ kind = 'E' ] TO FIELD-SYMBOL(<ls_error>).
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( |Syntax error in repo { iv_package } |
                                 && |object { <ls_error>-objtype } { <ls_error>-objname } { <ls_error>-text } |
                                 && | [ @{ CONV i( <ls_error>-line ) } ]| ).
    ENDIF.

  ENDMETHOD.


  METHOD update_abapgit_ci_repo.

    update_repo( 'CI' ).

  ENDMETHOD.


  METHOD update_abapgit_repo.

    update_repo( 'abapGit' ).

  ENDMETHOD.


  METHOD update_repo.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_online.

    DATA(lt_repo_list) = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repo_list ASSIGNING FIELD-SYMBOL(<repo>).

      IF <repo>->get_name( ) = iv_repo_name.
        lo_repo ?= <repo>.
        EXIT.
      ENDIF.

    ENDLOOP.

    IF lo_repo IS NOT BOUND.
      zcx_abapgit_exception=>raise( |Couldn't find { iv_repo_name } repo| ).
    ENDIF.

    lo_repo->select_branch( iv_branch ).

    TRY.
        DATA(ls_checks) = zcl_abapgit_ci_repo_check=>get( lo_repo ).
      CATCH zcx_abapgit_cancel INTO DATA(lx_cancel).
        zcx_abapgit_exception=>raise(
          iv_text     = lx_cancel->get_text( )
          ix_previous = lx_cancel ).
    ENDTRY.

    lo_repo->deserialize(
      is_checks = ls_checks
      ii_log    = NEW zcl_abapgit_log( ) ).

    syntax_check( lo_repo->get_package( ) ).

  ENDMETHOD.


  METHOD update_repository.

    update_repo( iv_repo_name = iv_repo_name iv_branch = iv_branch ).

  ENDMETHOD.
ENDCLASS.
