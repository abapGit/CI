*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_data_receiver DEFINITION.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_ci_view.

    DATA:
      ms_result TYPE zif_abapgit_ci_definitions=>ty_result READ-ONLY.

ENDCLASS.
