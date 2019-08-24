"! Skip logic
CLASS zcl_abapgit_ci_skip DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor,
      complete_skip_components CHANGING cs_repo TYPE zabapgit_ci_result.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_skip,
        repo_name          TYPE zif_abapgit_ci_definitions=>ty_repo-name,
        skip_local         TYPE abap_bool,
        skip_transportable TYPE abap_bool,
        reason             TYPE string,
      END OF gty_skip.
    METHODS:
      do_we_have_an_ads_connection RETURNING VALUE(rv_is_ads_on) TYPE abap_bool.
    DATA:
      mt_skipped TYPE SORTED TABLE OF gty_skip WITH UNIQUE KEY repo_name skip_local skip_transportable.
ENDCLASS.



CLASS zcl_abapgit_ci_skip IMPLEMENTATION.
  METHOD constructor.
    mt_skipped = VALUE #(
      LET no_ads   = xsdbool( do_we_have_an_ads_connection( ) <> abap_true )
          not_hana = xsdbool( cl_db_sys=>is_in_memory_db <> abap_true ) IN
      ( repo_name          = |CUS0|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |skip because UI is called| )
      ( repo_name          = |FDT0|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |https://github.com/larshp/abapGit/pull/2688| )
      ( repo_name          = |*SPRX*|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |https://github.com/larshp/abapGit/issues/87| )
      ( repo_name          = |SFSW|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |https://github.com/larshp/abapGit/issues/2083| )
      ( repo_name          = |DDLX_old|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Skip because it's an old testcase. abapGit indicates diff because migration to new | &&
                             |format| )
      ( repo_name          = |DEVC_component|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |https://github.com/larshp/abapGit/issues/1880| )
      ( repo_name          = |SQSC|
        skip_local         = not_hana
        skip_transportable = not_hana
        reason             = |Runs only on HANA| )
      ( repo_name          = |SFPF|
        skip_local         = no_ads
        skip_transportable = no_ads
        reason             = |Adobe Document Service (ADS) connection neccessary| )
      ( repo_name          = |TTYP_with_CLAS_reference|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |circular dependency https://github.com/larshp/abapGit/issues/2338| )
      ( repo_name          = |SHLP_with_exit|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |circular dependency https://github.com/larshp/abapGit/issues/2338| )
      ( repo_name          = |PROG_ci_variant|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |activation error https://github.com/larshp/abapGit/issues/2338| )
      ( repo_name          = |DOMA_fixed_values_single_translated|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |https://github.com/larshp/abapGit/issues/2385| )
      ( repo_name          = |SFBF|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |https://github.com/larshp/abapGit/issues/2469| )
      ( repo_name          = |SFBS|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |https://github.com/larshp/abapGit/issues/2469| )
      ( repo_name          = |SOTS|
        skip_local         = abap_true
        skip_transportable = abap_false
        reason             = |Cannot be installed in local $-package| ) ).
  ENDMETHOD.

  METHOD do_we_have_an_ads_connection.
    SELECT SINGLE FROM fpconnect
           FIELDS destination
           INTO @DATA(lv_destination).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'RFC_READ_DESTINATION_TYPE'
      EXPORTING
        destination             = lv_destination
        authority_check         = abap_false
        bypass_buf              = abap_false
      EXCEPTIONS
        authority_not_available = 1
        destination_not_exist   = 2
        information_failure     = 3
        internal_failure        = 4
        OTHERS                  = 5.

    IF sy-subrc = 0.
      rv_is_ads_on = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD complete_skip_components.
    FIELD-SYMBOLS: <ls_skip> TYPE gty_skip.

    IF cs_repo-layer IS NOT INITIAL.
      READ TABLE mt_skipped WITH KEY repo_name          = cs_repo-name
                                     skip_transportable = abap_true
                            ASSIGNING <ls_skip>.
    ELSE.
      READ TABLE mt_skipped WITH KEY repo_name  = cs_repo-name
                                     skip_local = abap_true
                            ASSIGNING <ls_skip>.
    ENDIF.

    IF <ls_skip> IS ASSIGNED.
      cs_repo-skip = abap_true.
      cs_repo-message = <ls_skip>-reason.
    ELSE.
      LOOP AT mt_skipped ASSIGNING <ls_skip> WHERE repo_name CA '*'.
        IF cs_repo-name CP <ls_skip>-repo_name AND
           ( ( cs_repo-layer IS NOT INITIAL AND <ls_skip>-skip_transportable = abap_true ) OR
             ( cs_repo-layer IS INITIAL AND <ls_skip>-skip_local = abap_true ) ).
          cs_repo-skip = abap_true.
          cs_repo-message = <ls_skip>-reason.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
