"! Skip logic
CLASS zcl_abapgit_ci_skip DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.

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

    CLASS-DATA:
      go_aff_registry TYPE REF TO zif_abapgit_aff_registry.

    DATA:
      mt_skipped TYPE SORTED TABLE OF gty_skip WITH UNIQUE KEY repo_name skip_local skip_transportable.

    METHODS:
      do_we_have_an_ads_connection RETURNING VALUE(rv_is_ads_on) TYPE abap_bool,
      does_system_support_aff RETURNING VALUE(rv_is_aff_on) TYPE abap_bool.

ENDCLASS.



CLASS zcl_abapgit_ci_skip IMPLEMENTATION.


  METHOD class_constructor.
    go_aff_registry = zcl_abapgit_aff_factory=>get_registry( ).
  ENDMETHOD.


  METHOD complete_skip_components.
    DATA ls_skip TYPE gty_skip.

    FIELD-SYMBOLS <ls_skip> TYPE gty_skip.

    " Skip repos containing AFF if not supported
    IF go_aff_registry->is_supported_object_type( cs_repo-name(4) ) AND NOT does_system_support_aff( ).
      READ TABLE mt_skipped WITH KEY repo_name = cs_repo-name TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        ls_skip = VALUE #(
          repo_name          = cs_repo-name
          skip_local         = abap_true
          skip_transportable = abap_true
          reason             = |Requires support for ABAP File Format (AFF)| ).
        INSERT ls_skip INTO TABLE mt_skipped.
      ENDIF.
    ENDIF.

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
          not_diag = xsdbool( sy-batch = abap_true ) IN
* Repositories with certain restrictions
      ( repo_name          = |AIFC|
        skip_local         = xsdbool( sy-saprl < '755' )
        skip_transportable = xsdbool( sy-saprl < '755' )
        reason             = |Requires release 7.55 (SAP Notes 3106807, 3117150)| )
      ( repo_name          = |APIS|
        skip_local         = xsdbool( sy-saprl < '755' )
        skip_transportable = xsdbool( sy-saprl < '755' )
        reason             = |Requires release 7.55 or higher| )
      ( repo_name          = |AVAR|
        skip_local         = abap_true
        skip_transportable = abap_false
        reason             = |Cannot be installed in local package| )
      ( repo_name          = |BMFR|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |No files found to deserialize| )
      ( repo_name          = |CHKC_namespace|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Requires special namespace| )
      ( repo_name          = |COTA|
        skip_local         = xsdbool( sy-saprl < '816' )
        skip_transportable = xsdbool( sy-saprl < '816' )
        reason             = |Requires release 8.16 or higher| )
      ( repo_name          = |CUS0|
        skip_local         = abap_false
        skip_transportable = not_diag
        reason             = |Requires user-interaction (not available in batch)| )
      ( repo_name          = |DDLX_old|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Obsolete testcase| )
      ( repo_name          = |DESD|
        skip_local         = xsdbool( sy-saprl < '816' )
        skip_transportable = xsdbool( sy-saprl < '816' )
        reason             = |Requires release 8.16 or higher| )
      ( repo_name          = |DEVC_main_package|
        skip_local         = abap_true
        skip_transportable = abap_false
        reason             = |Cannot be installed in local package| )
      ( repo_name          = |DEVC_struct_package|
        skip_local         = abap_true
        skip_transportable = abap_false
        reason             = |Cannot be installed in local package| )
      ( repo_name          = |DIAL|
        skip_local         = not_diag
        skip_transportable = not_diag
        reason             = |Requires user-interaction (not available in batch)| )
      ( repo_name          = |DRAS|
        skip_local         = xsdbool( sy-saprl < '816' )
        skip_transportable = xsdbool( sy-saprl < '816' )
        reason             = |Requires release 8.16 or higher| )
      ( repo_name          = |DSFD|
        skip_local         = xsdbool( sy-saprl < '816' )
        skip_transportable = xsdbool( sy-saprl < '816' )
        reason             = |Requires release 8.16 or higher| )
      ( repo_name          = |DSFI|
        skip_local         = xsdbool( sy-saprl < '816' )
        skip_transportable = xsdbool( sy-saprl < '816' )
        reason             = |Requires release 8.16 or higher| )
      ( repo_name          = |FDT0|
        skip_local         = abap_false
        skip_transportable = abap_true
        reason             = |Cannot be installed in transportable package| )
      ( repo_name          = |Language_DE|
        skip_local         = xsdbool( sy-langu <> 'D' )
        skip_transportable = xsdbool( sy-langu <> 'D' )
        reason             = |Requires logon in German| )
      ( repo_name          = |NSPC|
        skip_local         = abap_true
        skip_transportable = abap_false
        reason             = |Requires package in namespace| )
      ( repo_name          = |NSPC_apack|
        skip_local         = abap_true
        skip_transportable = abap_false
        reason             = |Requires package in namespace| )
      ( repo_name          = |SFPF_SFPI|
        skip_local         = no_ads
        skip_transportable = no_ads
        reason             = |Requires Adobe Document Service (ADS)| )
      ( repo_name          = |SKTD|
        skip_local         = xsdbool( sy-saprl < '755' )
        skip_transportable = xsdbool( sy-saprl < '755' )
        reason             = |Requires release 7.55 or higher| )
      ( repo_name          = |SOD1_SOD2|
        skip_local         = xsdbool( sy-saprl < '755' )
        skip_transportable = xsdbool( sy-saprl < '755' )
        reason             = |Requires release 7.55 or higher| )
      ( repo_name          = |SOTS_old|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Obsolete testcase| )
      ( repo_name          = |SRVD|
        skip_local         = xsdbool( sy-saprl < '755' )
        skip_transportable = xsdbool( sy-saprl < '755' )
        reason             = |Requires release 7.55 or higher| )
      ( repo_name          = |SUSC_and_SUSO|
        skip_local         = abap_false
        skip_transportable = abap_true
        reason             = |Transportable authorization objects cannot be uninstalled| )
      ( repo_name          = |SUSO|
        skip_local         = abap_false
        skip_transportable = abap_true
        reason             = |Transportable authorization objects cannot be uninstalled| )
      ( repo_name          = |SXCI|
        skip_local         = not_diag
        skip_transportable = not_diag
        reason             = |Requires user-interaction (not available in batch)| )
      ( repo_name          = |SWCR|
        skip_local         = xsdbool( sy-saprl < '816' )
        skip_transportable = xsdbool( sy-saprl < '816' )
        reason             = |Requires release 8.16 or higher| )
      ( repo_name          = |UIAD|
        skip_local         = xsdbool( sy-saprl < '816' )
        skip_transportable = xsdbool( sy-saprl < '816' )
        reason             = |Requires release 8.16 or higher| )
      ( repo_name          = |UIPG|
        skip_local         = xsdbool( sy-saprl < '816' )
        skip_transportable = xsdbool( sy-saprl < '816' )
        reason             = |Requires release 8.16 or higher| )
      ( repo_name          = |UIST|
        skip_local         = xsdbool( sy-saprl < '816' )
        skip_transportable = xsdbool( sy-saprl < '816' )
        reason             = |Requires release 8.16 or higher| )
* Repositories with issues that should get fixed
      ( repo_name          = |AOBJ|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Issue https://github.com/abapGit/abapGit/issues/804| )
      ( repo_name          = |CLAS_KEY_USER|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Issue https://github.com/abapGit/abapGit/issues/6154| )
      ( repo_name          = |DDLS_cyclic|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Issue https://github.com/abapGit/abapGit/issues/5524| )
      ( repo_name          = |DDLS_dependency|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Issue https://github.com/abapGit/abapGit/issues/5524| )
      ( repo_name          = |DEVC_struct_pack_hier|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Cannot be installed in local package, https://github.com/abapGit/abapGit/issues/6923| )
      ( repo_name          = |DTEB|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |CDS view missing, https://github.com/abapGit-tests/DTEB/issues/1| )
      ( repo_name          = |EEEC|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |No objects found to deserialize| )
      ( repo_name          = |PINF_exposing_*|
        skip_local         = abap_false
        skip_transportable = abap_true
        reason             = |Requires restart of application| )
      ( repo_name          = |SPRX|
        skip_local         = abap_false
        skip_transportable = abap_true
        reason             = |Issue https://github.com/abapGit/abapGit/issues/5553| )
      ( repo_name          = |SPRX_*|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Issue https://github.com/abapGit/abapGit/issues/4140| )
      ( repo_name          = |SQSC|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Requires SAP ERP on HANA\nIssue https://github.com/abapGit-tests/SQSC/issues/1| )
      ( repo_name          = |SRVB|
        skip_local         = xsdbool( sy-saprl < '755' )
        skip_transportable = xsdbool( sy-saprl < '755' )
        reason             = |Requires release 7.55\nIssue https://github.com/abapGit-tests/SRVB/issues/9| )
      ( repo_name          = |TABU_custom_table|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Not supported yet\nIssue https://github.com/abapGit/CI/issues/226| )
      ( repo_name          = |TABU_sap_table|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Not supported yet\nIssue https://github.com/abapGit/CI/issues/226| )
      ( repo_name          = |UDMO|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Missing test case, https://github.com/abapGit/abapGit/issues/1966| )
      ( repo_name          = |WEBI|
        skip_local         = abap_true
        skip_transportable = abap_true
        reason             = |Issue https://github.com/abapGit/abapGit/issues/4140| ) ).

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
