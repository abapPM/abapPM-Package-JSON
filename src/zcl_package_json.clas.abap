CLASS zcl_package_json DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_package_json.

    CLASS-METHODS factory
      IMPORTING
        !iv_package   TYPE devclass
        !iv_name      TYPE string OPTIONAL
        !iv_version   TYPE string OPTIONAL
        !iv_private   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zif_package_json
      RAISING
        zcx_package_json.

    CLASS-METHODS injector
      IMPORTING
        !iv_package TYPE devclass
        !ii_mock    TYPE REF TO zif_package_json.

    METHODS constructor
      IMPORTING
        !iv_package TYPE devclass
        !iv_name    TYPE string OPTIONAL
        !iv_version TYPE string OPTIONAL
        !iv_private TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_package_json.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        package  TYPE devclass,
        instance TYPE REF TO zif_package_json,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY package.

    CLASS-DATA:
      gt_instances TYPE ty_instances.

    DATA:
      mv_package      TYPE devclass,
      ms_package_json TYPE zif_package_json=>ty_package_json,
      mo_persist      TYPE REF TO zcl_persistence.

ENDCLASS.



CLASS zcl_package_json IMPLEMENTATION.


  METHOD constructor.

    DATA:
      ls_json  TYPE zif_package_json=>ty_package_json,
      li_json  TYPE REF TO zif_ajson,
      lx_error TYPE REF TO zcx_ajson_error.

    IF zif_package_json~is_valid_sap_package( iv_package ) = abap_false.
      zcx_package_json=>raise( |Invalid package: { iv_package }| ).
    ENDIF.

    mv_package              = iv_package.
    ms_package_json-name    = iv_name.
    ms_package_json-version = iv_version.
    ms_package_json-private = iv_private.

    CREATE OBJECT mo_persist
      EXPORTING
        iv_package = mv_package.

  ENDMETHOD.


  METHOD factory.

    DATA ls_instance TYPE ty_instance.

    FIELD-SYMBOLS <ls_instance> TYPE ty_instance.

    READ TABLE gt_instances ASSIGNING <ls_instance> WITH TABLE KEY package = iv_package.
    IF sy-subrc = 0.
      result = <ls_instance>-instance.
    ELSE.
      CREATE OBJECT result TYPE zcl_package_json
        EXPORTING
          iv_package = iv_package
          iv_name    = iv_name
          iv_version = iv_version
          iv_private = iv_private.

      ls_instance-package  = iv_package.
      ls_instance-instance = result.
      INSERT ls_instance INTO TABLE gt_instances.
    ENDIF.

  ENDMETHOD.


  METHOD injector.

    DATA ls_instance TYPE ty_instance.

    FIELD-SYMBOLS <ls_instance> TYPE ty_instance.

    READ TABLE gt_instances ASSIGNING <ls_instance> WITH TABLE KEY package = iv_package.
    IF sy-subrc = 0.
      <ls_instance>-instance = ii_mock.
    ELSE.
      ls_instance-package  = iv_package.
      ls_instance-instance = ii_mock.
      INSERT ls_instance INTO TABLE gt_instances.
    ENDIF.

  ENDMETHOD.


  METHOD zif_package_json~delete.
    mo_persist->delete( ).
  ENDMETHOD.


  METHOD zif_package_json~get.
    result = ms_package_json.
  ENDMETHOD.


  METHOD zif_package_json~get_json.

    DATA li_json TYPE REF TO zif_ajson.

    TRY.
        li_json = zcl_ajson=>new( )->keep_item_order( )->set(
          iv_path = '/'
          iv_val  = ms_package_json ).

        li_json = li_json->map( zcl_ajson_mapping=>create_to_camel_case( ) ).

        IF iv_complete = abap_false.
          li_json = li_json->filter( zcl_ajson_filter_lib=>create_empty_filter( ) ).
          IF ms_package_json-private = abap_false.
            li_json = li_json->filter( zcl_ajson_filter_lib=>create_path_filter( iv_skip_paths = '/private' ) ).
          ENDIF.
        ENDIF.
        result = li_json->stringify( 2 ).
      CATCH zcx_ajson_error.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_package_json~is_valid.
    TRY.
        zif_package_json~validate( ms_package_json ).
        result = abap_true.
      CATCH zcx_package_json.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_package_json~is_valid_email.

    " Email address validation (RFC 5322)
    CONSTANTS lc_email_regex TYPE string VALUE
      '[\w!#$%&*+/=?`{|}~^-]+(?:\.[\w!#$%&*+/=?`{|}~^-]+)*@(?:[A-Za-z0-9-]+\.)+[A-Za-z]{2,6}'.

    IF iv_email IS INITIAL.
      result = abap_true.
    ELSE.
      FIND REGEX lc_email_regex IN iv_email.
      result = boolc( sy-subrc = 0 ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_package_json~is_valid_name.
    IF strlen( iv_name )
      BETWEEN zif_package_json~c_package_name-min_length
          AND zif_package_json~c_package_name-max_length.

      FIND REGEX zif_package_json~c_package_name-regex IN iv_name.
      result = boolc( sy-subrc =  0 ).
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD zif_package_json~is_valid_sap_package.

    DATA lv_package_type TYPE c LENGTH 1.

    " Limit to local, customer, namespaced, and partner packages (see type-pool TPAK)
    CALL METHOD cl_package_helper=>check_package_name
      EXPORTING
        i_package_name = iv_package
      IMPORTING
        e_package_type = lv_package_type
      EXCEPTIONS
        OTHERS         = 1.
    result = boolc( sy-subrc =  0 AND lv_package_type CA '$ZNJ' ).

    " Workaround for missing validation of empty namespace
    IF result = abap_true AND iv_package CP '//*'.
      result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD zif_package_json~is_valid_url.

    " Basic URL validation
    CONSTANTS lc_url_regex TYPE string VALUE 'https?://.+\..+'.

    IF iv_url IS INITIAL.
      result = abap_true.
    ELSE.
      FIND REGEX lc_url_regex IN iv_url.
      result = boolc( sy-subrc = 0 ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_package_json~is_valid_version.
    " Check if it is a semantic version
    TRY.
        zcl_semver=>create( iv_version ).
        result = abap_true.
      CATCH zcx_semver_error.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_package_json~is_valid_version_range.
    " Check if it is a semantic version range
    TRY.
        zcl_semver_range=>create( iv_range ).
        result = abap_true.
      CATCH zcx_semver_error.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_package_json~load.
    zif_package_json~set_json( mo_persist->load( ) ).
    result = me.
  ENDMETHOD.


  METHOD zif_package_json~save.
    zif_package_json~validate( ms_package_json ).
    mo_persist->save(  zif_package_json~get_json( ) ).
  ENDMETHOD.


  METHOD zif_package_json~set.
    zif_package_json~validate( is_json ).
    ms_package_json = is_json.
    result = me.
  ENDMETHOD.


  METHOD zif_package_json~set_json.

    DATA:
      li_json  TYPE REF TO zif_ajson,
      ls_json  TYPE zif_package_json=>ty_package_json,
      lx_error TYPE REF TO zcx_ajson_error.

    TRY.
        li_json = zcl_ajson=>parse( iv_json ).
        li_json->to_abap( IMPORTING ev_container = ls_json ).

        zif_package_json~validate( ls_json ).

        ms_package_json = ls_json.
      CATCH zcx_ajson_error INTO lx_error.
        zcx_package_json=>raise_with_text( lx_error ).
    ENDTRY.

    result = me.

  ENDMETHOD.


  METHOD zif_package_json~validate.

    DATA:
      ls_dependency TYPE zif_package_json=>ty_dependency,
      ls_person     TYPE zif_package_json=>ty_person,
      lv_value      TYPE string,
      lt_values     TYPE string_table.

    IF zif_package_json~is_valid_name( is_package_json-name ) = abap_false.
      zcx_package_json=>raise( |Invalid name: { is_package_json-name }| ).
    ENDIF.

    IF zif_package_json~is_valid_version( is_package_json-version ) = abap_false.
      zcx_package_json=>raise( |Invalid version: { is_package_json-version }| ).
    ENDIF.

    IF is_package_json-private <> abap_false AND is_package_json-private <> abap_true.
      zcx_package_json=>raise( |Invalid private flag: { is_package_json-private }| ).
    ENDIF.

    IF zif_package_json~is_valid_url( is_package_json-homepage ) = abap_false.
      zcx_package_json=>raise( |Invalid homepage URL: { is_package_json-homepage }| ).
    ENDIF.

    IF zif_package_json~is_valid_url( is_package_json-bugs-url ) = abap_false.
      zcx_package_json=>raise( |Invalid bugs URL: { is_package_json-bugs-url }| ).
    ENDIF.

    IF zif_package_json~is_valid_email( is_package_json-bugs-email ) = abap_false.
      zcx_package_json=>raise( |Invalid bugs email: { is_package_json-bugs-email }| ).
    ENDIF.

    IF zif_package_json~is_valid_email( is_package_json-author-email ) = abap_false.
      zcx_package_json=>raise( |Invalid author email: { is_package_json-author-email }| ).
    ENDIF.

    IF zif_package_json~is_valid_url( is_package_json-author-url ) = abap_false.
      zcx_package_json=>raise( |Invalid author URL: { is_package_json-author-url }| ).
    ENDIF.

    IF is_package_json-type <> '' AND is_package_json-type <> 'app' AND is_package_json-type <> 'module'.
      zcx_package_json=>raise( |Invalid type: { is_package_json-type }| ).
    ENDIF.

    IF zif_package_json~is_valid_url( is_package_json-repository-url ) = abap_false.
      zcx_package_json=>raise( |Invalid repository URL: { is_package_json-repository-url }| ).
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_package_json-contributors INTO ls_person.
      COLLECT ls_person-name INTO lt_values.
      IF zif_package_json~is_valid_email( ls_person-email ) = abap_false.
        zcx_package_json=>raise( |Invalid contributor email: { ls_person-name } { ls_person-email }| ).
      ENDIF.
      IF zif_package_json~is_valid_url( ls_person-url ) = abap_false.
        zcx_package_json=>raise( |Invalid contributor URL: { ls_person-name } { ls_person-url }| ).
      ENDIF.
    ENDLOOP.
    IF lines( is_package_json-contributors ) <> lines( lt_values ).
      zcx_package_json=>raise( |Duplicate contributors| ).
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_package_json-maintainers INTO ls_person.
      COLLECT ls_person-name INTO lt_values.
      IF zif_package_json~is_valid_email( ls_person-email ) = abap_false.
        zcx_package_json=>raise( |Invalid maintainer email: { ls_person-name } { ls_person-email }| ).
      ENDIF.
      IF zif_package_json~is_valid_url( ls_person-url ) = abap_false.
        zcx_package_json=>raise( |Invalid maintainer URL: { ls_person-name } { ls_person-url }| ).
      ENDIF.
    ENDLOOP.
    IF lines( is_package_json-maintainers ) <> lines( lt_values ).
      zcx_package_json=>raise( |Duplicate maintainers| ).
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_package_json-dependencies INTO ls_dependency.
      COLLECT ls_dependency-name INTO lt_values.
      IF zif_package_json~is_valid_name( ls_dependency-name ) = abap_false.
        zcx_package_json=>raise( |Invalid dependency name: { ls_dependency-name }| ).
      ENDIF.
      IF zif_package_json~is_valid_version_range( ls_dependency-version ) = abap_false.
        zcx_package_json=>raise( |Invalid dependency version: { ls_dependency-name } { ls_dependency-version }| ).
      ENDIF.
    ENDLOOP.
    IF lines( is_package_json-dependencies ) <> lines( lt_values ).
      zcx_package_json=>raise( |Duplicate dependencies| ).
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_package_json-dev_dependencies INTO ls_dependency.
      COLLECT ls_dependency-name INTO lt_values.
      IF zif_package_json~is_valid_name( ls_dependency-name ) = abap_false.
        zcx_package_json=>raise( |Invalid dev dependency name: { ls_dependency-name }| ).
      ENDIF.
      IF zif_package_json~is_valid_version_range( ls_dependency-version ) = abap_false.
        zcx_package_json=>raise( |Invalid dev dependency version: { ls_dependency-name } { ls_dependency-version }| ).
      ENDIF.
      READ TABLE is_package_json-dependencies TRANSPORTING NO FIELDS WITH KEY name = ls_dependency-name.
      IF sy-subrc = 0.
        zcx_package_json=>raise( |Dev dependency { ls_dependency-name } already included in dependencies| ).
      ENDIF.
    ENDLOOP.
    IF lines( is_package_json-dev_dependencies ) <> lines( lt_values ).
      zcx_package_json=>raise( |Duplicate dev dependencies| ).
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_package_json-optional_dependencies INTO ls_dependency.
      COLLECT ls_dependency-name INTO lt_values.
      IF zif_package_json~is_valid_name( ls_dependency-name ) = abap_false.
        zcx_package_json=>raise( |Invalid opt dependency name: { ls_dependency-name }| ).
      ENDIF.
      IF zif_package_json~is_valid_version_range( ls_dependency-version ) = abap_false.
        zcx_package_json=>raise( |Invalid opt dependency version: { ls_dependency-name } { ls_dependency-version }| ).
      ENDIF.
      READ TABLE is_package_json-dependencies TRANSPORTING NO FIELDS WITH KEY name = ls_dependency-name.
      IF sy-subrc = 0.
        zcx_package_json=>raise( |Opt dependency { ls_dependency-name } already included in dependencies| ).
      ENDIF.
      READ TABLE is_package_json-dev_dependencies TRANSPORTING NO FIELDS WITH KEY name = ls_dependency-name.
      IF sy-subrc = 0.
        zcx_package_json=>raise( |Opt dependency { ls_dependency-name } already included in dev dependencies| ).
      ENDIF.
    ENDLOOP.
    IF lines( is_package_json-optional_dependencies ) <> lines( lt_values ).
      zcx_package_json=>raise( |Duplicate optional dependencies| ).
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_package_json-bundled_dependencies INTO lv_value.
      COLLECT lv_value INTO lt_values.
      IF zif_package_json~is_valid_name( lv_value ) = abap_false.
        zcx_package_json=>raise( |Invalid bundled dependency name: { ls_dependency-name }| ).
      ENDIF.
      READ TABLE is_package_json-dependencies TRANSPORTING NO FIELDS WITH KEY name = ls_dependency-name.
      IF sy-subrc <> 0.
        zcx_package_json=>raise( |Bundeled dependency { ls_dependency-name } not included in dependencies| ).
      ENDIF.
    ENDLOOP.
    IF lines( is_package_json-bundled_dependencies ) <> lines( lt_values ).
      zcx_package_json=>raise( |Duplicate bundled dependencies| ).
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_package_json-engines INTO ls_dependency.
      COLLECT ls_dependency-name INTO lt_values.
      IF zif_package_json~is_valid_name( ls_dependency-name ) = abap_false.
        zcx_package_json=>raise( |Invalid engine name: { ls_dependency-name }| ).
      ENDIF.
      IF zif_package_json~is_valid_version_range( ls_dependency-version ) = abap_false.
        zcx_package_json=>raise( |Invalid engine version: { ls_dependency-name } { ls_dependency-version }| ).
      ENDIF.
    ENDLOOP.
    IF lines( is_package_json-engines ) <> lines( lt_values ).
      zcx_package_json=>raise( |Duplicate engines| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
