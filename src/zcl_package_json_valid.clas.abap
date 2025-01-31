CLASS zcl_package_json_valid DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Package JSON Validator
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS check
      IMPORTING
        !manifest     TYPE zif_types=>ty_manifest
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS is_valid_package_type
      IMPORTING
        !type         TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_sap_package
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_name
      IMPORTING
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_scoped_name
      IMPORTING
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_version
      IMPORTING
        !version      TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_version_range
      IMPORTING
        !range        TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_email
      IMPORTING
        !email        TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_url
      IMPORTING
        !url          TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_engine
      IMPORTING
        !engine       TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_os
      IMPORTING
        !os           TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_cpu
      IMPORTING
        !cpu          TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_db
      IMPORTING
        !db           TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_package_json_valid IMPLEMENTATION.


  METHOD check.

    APPEND LINES OF lcl_validate=>validate_single_values( manifest ) TO result.
    APPEND LINES OF lcl_validate=>validate_arrays( manifest ) TO result.
    APPEND LINES OF lcl_validate=>validate_persons( manifest ) TO result.
    APPEND LINES OF lcl_validate=>validate_engines( manifest ) TO result.
    APPEND LINES OF lcl_validate=>validate_dependencies( manifest ) TO result.

  ENDMETHOD.


  METHOD is_scoped_name.

    result = xsdbool( is_valid_name( name ) AND name(1) = '@' AND name CS '/' ).

  ENDMETHOD.


  METHOD is_valid_cpu.

    DATA(cpu_val) = cpu.

    SHIFT cpu_val LEFT DELETING LEADING '!'.

    result = xsdbool(
      cpu_val IS INITIAL OR
      cpu_val = zif_types=>c_cpu-x86_64 OR
      cpu_val = zif_types=>c_cpu-power_pc OR
      cpu_val = zif_types=>c_cpu-sparc ).

  ENDMETHOD.


  METHOD is_valid_db.

    DATA(db_val) = db.

    SHIFT db_val LEFT DELETING LEADING '!'.

    result = xsdbool(
      db_val IS INITIAL OR
      db_val = zif_types=>c_db-db2 OR
      db_val = zif_types=>c_db-db400 OR
      db_val = zif_types=>c_db-db6 OR
      db_val = zif_types=>c_db-hdb OR
      db_val = zif_types=>c_db-informix OR
      db_val = zif_types=>c_db-mssql OR
      db_val = zif_types=>c_db-oracle OR
      db_val = zif_types=>c_db-sap_db OR
      db_val = zif_types=>c_db-sybase ).

  ENDMETHOD.


  METHOD is_valid_email.

    " Email address validation (RFC 5322)
    CONSTANTS c_email_regex TYPE string VALUE
      '[\w!#$%&*+/=?`{|}~^-]+(?:\.[\w!#$%&*+/=?`{|}~^-]+)*@(?:[A-Za-z0-9-]+\.)+[A-Za-z]{2,6}'.

    IF email IS INITIAL.
      result = abap_true.
    ELSE.
      FIND REGEX c_email_regex IN email.
      result = xsdbool( sy-subrc = 0 ).
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_engine.

    result = xsdbool(
      engine IS INITIAL OR
      engine = zif_types=>c_engine-abap OR
      engine = zif_types=>c_engine-apm ).

  ENDMETHOD.


  METHOD is_valid_name.

    " https://www.npmjs.com/package/validate-npm-package-name
    IF strlen( name )
      BETWEEN zif_types=>c_package_name-min_length
          AND zif_types=>c_package_name-max_length.

      FIND REGEX zif_types=>c_package_name-regex IN name RESPECTING CASE.
      result = xsdbool( sy-subrc = 0 ).
    ELSE.
      result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_os.

    DATA(os_val) = os.

    SHIFT os_val LEFT DELETING LEADING '!'.

    result = xsdbool(
      os_val IS INITIAL OR
      os_val = zif_types=>c_os-aix OR
      os_val = zif_types=>c_os-hp_ux OR
      os_val = zif_types=>c_os-linux OR
      os_val = zif_types=>c_os-ms_windows OR
      os_val = zif_types=>c_os-os_390 OR
      os_val = zif_types=>c_os-os_400 OR
      os_val = zif_types=>c_os-solaris ).

  ENDMETHOD.


  METHOD is_valid_package_type.

    result = xsdbool(
      type IS INITIAL OR
      type = zif_types=>c_package_type-common_abap OR
      type = zif_types=>c_package_type-module ).

  ENDMETHOD.


  METHOD is_valid_sap_package.

    DATA package_type TYPE c LENGTH 1.

    " Limit to local, customer, namespaced, and partner packages (see type-pool TPAK)
    cl_package_helper=>check_package_name(
      EXPORTING
        i_package_name = package
      IMPORTING
        e_package_type = package_type
      EXCEPTIONS
        OTHERS         = 1 ).

    result = xsdbool( sy-subrc = 0 AND package_type CA '$ZNJ' ).

    " Workaround for missing validation of empty namespace
    IF result = abap_true AND package CP '//*'.
      result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_url.

    TRY.
        IF url IS NOT INITIAL.
          zcl_url=>parse( url ).
        ENDIF.
        result = abap_true.
      CATCH zcx_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD is_valid_version.

    " Check if it is a semantic version
    TRY.
        zcl_semver=>create( version ).
        result = abap_true.
      CATCH zcx_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD is_valid_version_range.

    " Check if it is a semantic version range
    TRY.
        zcl_semver_range=>create( range ).
        result = abap_true.
      CATCH zcx_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
