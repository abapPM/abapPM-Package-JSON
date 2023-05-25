INTERFACE zif_package_json PUBLIC.

  TYPES:
    ty_email TYPE string,
    ty_uri   TYPE string,
    BEGIN OF ty_person,
      name  TYPE string,
      url   TYPE ty_uri,
      email TYPE ty_email,
    END OF ty_person,
    BEGIN OF ty_dependency,
      name    TYPE string,
      version TYPE string,
    END OF ty_dependency.

  TYPES:
    BEGIN OF ty_package_json,
      name                  TYPE string,
      version               TYPE string,
      description           TYPE string,
      keywords              TYPE string_table,
      homepage              TYPE string,
      BEGIN OF bugs,
        url   TYPE ty_uri,
        email TYPE ty_email,
      END OF bugs,
      license               TYPE string,
      author                TYPE ty_person,
      contributors          TYPE STANDARD TABLE OF ty_person WITH KEY name,
      maintainers           TYPE STANDARD TABLE OF ty_person WITH KEY name,
      main                  TYPE string,
      type                  TYPE string,
      BEGIN OF repository,
        type      TYPE string,
        url       TYPE ty_uri,
        directory TYPE string,
      END OF repository,
      BEGIN OF funding,
        type TYPE string,
        url  TYPE ty_uri,
      END OF funding,
      dependencies          TYPE STANDARD TABLE OF ty_dependency WITH DEFAULT KEY,
      dev_dependencies      TYPE STANDARD TABLE OF ty_dependency WITH DEFAULT KEY,
      optional_dependencies TYPE STANDARD TABLE OF ty_dependency WITH DEFAULT KEY,
      bundled_dependencies  TYPE string_table,
      package_manager       TYPE string,
      engines               TYPE STANDARD TABLE OF ty_dependency WITH DEFAULT KEY,
      os                    TYPE string_table,
      cpu                   TYPE string_table,
      db                    TYPE string_table,
      private               TYPE abap_bool,
      BEGIN OF dist,
        shasum  TYPE string,
        tarball TYPE string,
      END OF dist,
      readme                TYPE string,
    END OF ty_package_json.

  CONSTANTS:
    c_obj_type TYPE tadir-object VALUE 'ZAPM'.

  CONSTANTS:
    BEGIN OF c_package_name,
      min_length TYPE i VALUE 3,
      max_length TYPE i VALUE 214,
      regex      TYPE string VALUE '^(?:@[a-z0-9\-*~][a-z0-9\-*._~]*/)?[a-z0-9\-~][a-z0-9\-._~]*$',
    END OF c_package_name.

  CONSTANTS:
    BEGIN OF c_package_file,
      obj_name  TYPE c LENGTH 7 VALUE 'package',
      sep1      TYPE c LENGTH 1 VALUE '.',
      obj_type  TYPE c LENGTH 4 VALUE 'zapm',
      sep2      TYPE c LENGTH 1 VALUE '.',
      extension TYPE c LENGTH 4 VALUE 'json',
    END OF c_package_file.

  METHODS get
    RETURNING
      VALUE(result) TYPE ty_package_json.

  METHODS get_json
    IMPORTING
      !iv_complete  TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result) TYPE string.

  METHODS set
    IMPORTING
      !is_json      TYPE ty_package_json
    RETURNING
      VALUE(result) TYPE REF TO zif_package_json
    RAISING
      zcx_package_json.

  METHODS set_json
    IMPORTING
      !iv_json      TYPE string
    RETURNING
      VALUE(result) TYPE REF TO zif_package_json
    RAISING
      zcx_package_json.

  METHODS load
    RETURNING
      VALUE(result) TYPE REF TO zif_package_json
    RAISING
      zcx_package_json.

  METHODS save
    RAISING
      zcx_package_json.

  METHODS delete
    RAISING
      zcx_package_json.

  METHODS is_valid
    RETURNING
      VALUE(result) TYPE abap_bool.

  CLASS-METHODS validate
    IMPORTING
      !is_package_json TYPE ty_package_json
    RAISING
      zcx_package_json.

  CLASS-METHODS is_valid_sap_package
    IMPORTING
      !iv_package   TYPE devclass
    RETURNING
      VALUE(result) TYPE abap_bool.

  CLASS-METHODS is_valid_name
    IMPORTING
      !iv_name      TYPE string
    RETURNING
      VALUE(result) TYPE abap_bool.

  CLASS-METHODS is_valid_version
    IMPORTING
      !iv_version   TYPE string
    RETURNING
      VALUE(result) TYPE abap_bool.

  CLASS-METHODS is_valid_version_range
    IMPORTING
      !iv_range     TYPE string
    RETURNING
      VALUE(result) TYPE abap_bool.

  CLASS-METHODS is_valid_email
    IMPORTING
      !iv_email     TYPE string
    RETURNING
      VALUE(result) TYPE abap_bool.

  CLASS-METHODS is_valid_url
    IMPORTING
      !iv_url       TYPE string
    RETURNING
      VALUE(result) TYPE abap_bool.

ENDINTERFACE.
