"! Skip logic
CLASS zcl_abapgit_ci_skip DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
    METHODS complete_skip_components
      CHANGING
        !cs_repo TYPE zabapgit_ci_result .
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
      do_we_have_an_ads_connection RETURNING VALUE(rv_is_ads_on) TYPE abap_bool,
      does_system_support_aff RETURNING VALUE(rv_is_aff_on) TYPE abap_bool.
    DATA:
      mt_skipped TYPE SORTED TABLE OF gty_skip WITH UNIQUE KEY repo_name skip_local skip_transportable.
ENDCLASS.



CLASS ZCL_ABAPGIT_CI_SKIP IMPLEMENTATION.


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


  METHOD constructor.

    mt_skipped = VALUE #(
      LET no_ads   = xsdbool( do_we_have_an_ads_connection( ) <> abap_true )
          no_aff   = xsdbool( does_system_support_aff( ) <> abap_true )
          not_diag = xsdbool( sy-batch = abap_true )
          not_hana = xsdbool( cl_db_sys=>is_in_memory_db <> abap_true ) IN
      ( repo_name          = |BranchTest|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Not an object type| )
      ( repo_name          = |CHKC|
        skip_local         = no_aff
        skip_transportable = no_aff
        reason             = |Requires support for ABAP File Format (AFF)| )
      ( repo_name          = |CHKO|
        skip_local         = no_aff
        skip_transportable = no_aff
        reason             = |Requires support for ABAP File Format (AFF)| )
      ( repo_name          = |CHKV|
        skip_local         = no_aff
        skip_transportable = no_aff
        reason             = |Requires support for ABAP File Format (AFF)| )
      ( repo_name          = |DDLX_old|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Old testcase (diff because migration to new format)| )
      ( repo_name          = |DOMA_append|
        skip_local         = not_diag
        skip_transportable = not_diag
        reason             = |Requires user-interaction (not available in batch)| )
      ( repo_name          = |Language_DE|
        skip_local         = xsdbool( sy-langu <> 'D' )
        skip_transportable = xsdbool( sy-langu <> 'D' )
        reason             = |Requires logon in German| )
      ( repo_name          = |SFPF|
        skip_local         = no_ads
        skip_transportable = no_ads
        reason             = |Requires Adobe Document Service (ADS)| )
      ( repo_name          = |SFBF|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Issue https://github.com/abapGit/abapGit/issues/2469| )
      ( repo_name          = |SFBS|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Issue https://github.com/abapGit/abapGit/issues/2469| )
      ( repo_name          = |SFSW|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Issue https://github.com/abapGit/abapGit/issues/2083| )
      ( repo_name          = |SHI5*|
        skip_local         = not_diag
        skip_transportable = not_diag
        reason             = |Requires user-interaction (not available in batch)| )
      ( repo_name          = |SOTS|
        skip_local         = abap_true
        skip_transportable = abap_false
        reason             = |Cannot be installed in local package| )
      ( repo_name          = |SPRX_server_proxy|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Issue https://github.com/abapGit/abapGit/issues/87| )
      ( repo_name          = |SQSC|
        skip_local         = not_hana
        skip_transportable = not_hana
        reason             = |Requires SAP HANA| ) ).

  ENDMETHOD.


  METHOD does_system_support_aff.
    DATA lo_handler_factory TYPE REF TO object.

    TRY.
        CREATE OBJECT lo_handler_factory TYPE ('CL_AFF_OBJECT_HANDLER_FACTORY').
        rv_is_aff_on = abap_true.
      CATCH cx_root.
        rv_is_aff_on = abap_false.
    ENDTRY.
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
ENDCLASS.
