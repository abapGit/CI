CLASS zcl_abapgit_ci_repos DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      gc_default_branch TYPE string VALUE 'refs/heads/main'.

    " Open source namespace used for tests
    CONSTANTS:
      gc_namespace TYPE namespace VALUE '/ABAPGIT/'.

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
          zcx_abapgit_exception,

      fail_message
        IMPORTING
          !iv_message TYPE csequence
          !iv_skip    TYPE abap_bool DEFAULT abap_false
        CHANGING
          !cs_ci_repo TYPE zabapgit_ci_result.

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
      repo_start
        IMPORTING
          is_ci_repo TYPE zabapgit_ci_result
          iv_count   TYPE i
          iv_total   TYPE i,

      process_repo
        CHANGING
          cs_ci_repo TYPE zabapgit_ci_result,

      repo_finish
        IMPORTING
          is_ci_repo TYPE zabapgit_ci_result,

      get_repo_list_with_packages
        IMPORTING
          it_repos        TYPE zif_abapgit_ci_definitions=>tty_repo
          is_options      TYPE zif_abapgit_ci_definitions=>ty_repo_check_options
        RETURNING
          VALUE(rt_repos) TYPE zif_abapgit_ci_definitions=>tty_repo_result_list,

      info_message
        IMPORTING
          iv_message TYPE csequence.

ENDCLASS.



CLASS zcl_abapgit_ci_repos IMPLEMENTATION.


  METHOD constructor.
    mv_sync = iv_sync.
  ENDMETHOD.


  METHOD fail_message.

    IF iv_skip = abap_true.
      cs_ci_repo-status = zif_abapgit_ci_definitions=>co_status-skipped.
    ELSE.
      cs_ci_repo-status = zif_abapgit_ci_definitions=>co_status-not_ok.
    ENDIF.
    cs_ci_repo-message = iv_message.

    IF cs_ci_repo-logging = abap_true AND sy-batch = abap_false.
      " For online execution, add timestamp to help cross-reference with other logs
      GET TIME.
      cs_ci_repo-message = cs_ci_repo-message && | ({ sy-uzeit TIME = ISO })|.
    ENDIF.

  ENDMETHOD.


  METHOD get_repo_list_with_packages.
    DATA lv_prefix TYPE namespace.
    FIELD-SYMBOLS: <ls_ci_repo> TYPE zabapgit_ci_result.

    " Copy it_repos into rt_repos, creating an entry for local and transportable packages each

    DATA(lo_skip) = NEW zcl_abapgit_ci_skip( ).
    DATA(lo_repo_cat) = NEW zcl_abapgit_ci_repo_category( ).

    LOOP AT it_repos ASSIGNING FIELD-SYMBOL(<ls_repo>).

      DATA(lv_category) = lo_repo_cat->get_repo_category( <ls_repo>-name ).

      IF lv_category NOT IN is_options-categories.
        CONTINUE.
      ENDIF.

      IF is_options-check_local = abap_true.
        INSERT CORRESPONDING #( <ls_repo> )
               INTO TABLE rt_repos
               ASSIGNING <ls_ci_repo>.
        <ls_ci_repo>-package      = CONV devclass( |$___{ to_upper( <ls_ci_repo>-name ) }| ).
        <ls_ci_repo>-do_not_purge = is_options-no_purge.
        <ls_ci_repo>-logging      = is_options-logging.
        <ls_ci_repo>-category     = lo_repo_cat->get_category_label( lv_category ).

        IF is_options-create_package = abap_false.
          <ls_ci_repo>-create_package = zif_abapgit_ci_definitions=>co_status-skipped.
        ENDIF.

        IF is_options-ignore_skipping IS INITIAL.
          lo_skip->complete_skip_components( CHANGING cs_repo = <ls_ci_repo> ).
        ENDIF.
      ENDIF.

      IF is_options-check_transportable = abap_true.
        IF <ls_ci_repo>-name CP 'NSPC*'.
          lv_prefix = gc_namespace && '_'.
        ELSE.
          lv_prefix = 'Z___'.
        ENDIF.
        INSERT CORRESPONDING #( <ls_repo> )
               INTO TABLE rt_repos
               ASSIGNING <ls_ci_repo>.
        <ls_ci_repo>-package      = CONV devclass( |{ lv_prefix }{ to_upper( <ls_ci_repo>-name ) }| ).
        <ls_ci_repo>-layer        = is_options-layer.
        <ls_ci_repo>-do_not_purge = is_options-no_purge.
        <ls_ci_repo>-logging      = is_options-logging.
        <ls_ci_repo>-category     = lo_repo_cat->get_category_label( lv_category ).

        IF is_options-create_package = abap_false.
          <ls_ci_repo>-create_package = zif_abapgit_ci_definitions=>co_status-skipped.
        ENDIF.

        IF is_options-ignore_skipping IS INITIAL.
          lo_skip->complete_skip_components( CHANGING cs_repo = <ls_ci_repo> ).
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD info_message.

    DATA lv_message TYPE string.

    lv_message = condense( replace(
      val  = iv_message
      sub  = |\n|
      with = ', '
      occ  = 0 ) ).

    MESSAGE lv_message TYPE 'I'.

  ENDMETHOD.


  METHOD process_repo.

    DATA lv_message TYPE c LENGTH 255.
    DATA lv_errid TYPE c LENGTH 30.

    " You should remember that we process the repo in synchronous RFC because of
    " shortdumps there do not crash this main process

    ASSERT cs_ci_repo-package IS NOT INITIAL.

    IF mv_sync = abap_true.
      " Synchronous for debugging
      CALL FUNCTION 'ZABAPGIT_CI_PROCESS_REPO'
        CHANGING
          cs_ci_repo = cs_ci_repo
        EXCEPTIONS
          OTHERS     = 1.
    ELSE.
      " Asynchronous for mass processing
      CALL FUNCTION 'ZABAPGIT_CI_PROCESS_REPO'
        DESTINATION 'NONE'
        CHANGING
          cs_ci_repo            = cs_ci_repo
        EXCEPTIONS
          communication_failure = 1 MESSAGE lv_message
          system_failure        = 2 MESSAGE lv_message
          resource_failure      = 3
          OTHERS                = 4.
    ENDIF.

    IF sy-subrc <> 0.
      " Try to get error code for dump
      " (doesn't work if description contains parameters)
      SELECT SINGLE errid FROM snapt INTO @lv_errid
        WHERE langu = @sy-langu AND ttype = 'K' AND seqno = '0001' AND tline = @lv_message.
      IF sy-subrc <> 0.
        lv_errid = 'DUMP'.
      ENDIF.

      fail_message(
        EXPORTING
          iv_message = |Error processing repo: { lv_errid }\n{ lv_message }|
        CHANGING
          cs_ci_repo = cs_ci_repo ).
    ENDIF.

  ENDMETHOD.


  METHOD process_repos.

    DATA:
      lv_start_timestamp  TYPE timestampl,
      lv_finish_timestamp TYPE timestampl.

    rt_result_list = get_repo_list_with_packages(
      it_repos   = it_repos
      is_options = is_options ).

    LOOP AT rt_result_list ASSIGNING FIELD-SYMBOL(<ls_ci_repo>).

      repo_start(
        is_ci_repo = <ls_ci_repo>
        iv_count   = sy-tabix
        iv_total   = lines( rt_result_list ) ).

      GET TIME STAMP FIELD lv_start_timestamp.

      IF <ls_ci_repo>-skip = abap_true.

        <ls_ci_repo>-status = zif_abapgit_ci_definitions=>co_status-skipped.

      ELSE.

        process_repo( CHANGING cs_ci_repo = <ls_ci_repo> ).

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

      ENDIF.

      GET TIME STAMP FIELD lv_finish_timestamp.

      <ls_ci_repo>-duration = cl_abap_timestamp_util=>get_instance( )->tstmpl_seconds_between(
        iv_timestamp0 = lv_start_timestamp
        iv_timestamp1 = lv_finish_timestamp ).

      repo_finish( <ls_ci_repo> ).

    ENDLOOP.

  ENDMETHOD.


  METHOD repo_finish.

    DATA:
      lv_icon    TYPE string,
      lv_text    TYPE string,
      lv_runtime TYPE string.

    IF sy-batch = abap_false.
      RETURN.
    ENDIF.

    CASE is_ci_repo-status.
      WHEN zif_abapgit_ci_definitions=>co_status-ok.

        lv_icon = icon_led_green.
        lv_text = 'Pass'.

      WHEN zif_abapgit_ci_definitions=>co_status-skipped.

        lv_icon = icon_led_yellow.
        lv_text = 'Skip'.

        info_message( is_ci_repo-message ).

      WHEN zif_abapgit_ci_definitions=>co_status-not_ok.

        lv_icon = icon_led_red.
        lv_text = 'Fail'.

        info_message( is_ci_repo-message ).

      WHEN OTHERS.

        lv_icon = icon_led_yellow.
        lv_text = 'Unknown'.

    ENDCASE.

    IF is_ci_repo-duration > 0.
      lv_runtime = |, Runtime: { is_ci_repo-duration } seconds|.
    ENDIF.

    info_message( |{ lv_icon } Result: { lv_text }{ lv_runtime }| ).

  ENDMETHOD.


  METHOD repo_start.

    IF sy-batch = abap_false.
      RETURN.
    ENDIF.

    DATA(lv_counter) = |{ iv_count } of { iv_total }:|.

    MESSAGE |{ lv_counter } Start processing repo { is_ci_repo-name } { is_ci_repo-package }| TYPE 'I'.

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

    LOOP AT lt_repo_list ASSIGNING FIELD-SYMBOL(<lo_repo>).

      IF <lo_repo>->get_name( ) = iv_repo_name.
        lo_repo ?= <lo_repo>.
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

    update_repo(
      iv_repo_name = iv_repo_name
      iv_branch    = iv_branch ).

  ENDMETHOD.
ENDCLASS.
