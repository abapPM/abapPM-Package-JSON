CLASS zcl_package_json DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* Package JSON
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_package_json.

    CLASS-METHODS class_constructor.

    CLASS-METHODS factory
      IMPORTING
        !iv_package   TYPE devclass
        !iv_name      TYPE string OPTIONAL
        !iv_version   TYPE string OPTIONAL
        !iv_private   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zif_package_json
      RAISING
        zcx_error.

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
        zcx_error.

    CLASS-METHODS list
      IMPORTING
        !iv_filter      TYPE string OPTIONAL
        !iv_instanciate TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)   TYPE zif_package_json=>ty_packages.

    CLASS-METHODS get_package_key
      IMPORTING
        !iv_package   TYPE devclass
      RETURNING
        VALUE(result) TYPE zif_persist_apm=>ty_key.

    CLASS-METHODS get_package_from_key
      IMPORTING
        !iv_key       TYPE zif_persist_apm=>ty_key
      RETURNING
        VALUE(result) TYPE devclass.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        package  TYPE devclass,
        instance TYPE REF TO zif_package_json,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY package.

    CLASS-DATA:
      gi_persist   TYPE REF TO zif_persist_apm,
      gt_instances TYPE ty_instances.

    DATA:
      mv_key          TYPE zif_persist_apm=>ty_key,
      mv_package      TYPE devclass,
      ms_package_json TYPE zif_package_json_types=>ty_package_json.

    CLASS-METHODS sort_dependencies
      IMPORTING
        !is_package_json TYPE zif_package_json_types=>ty_package_json
      RETURNING
        VALUE(result)    TYPE zif_package_json_types=>ty_package_json.

ENDCLASS.



CLASS zcl_package_json IMPLEMENTATION.


  METHOD class_constructor.
    gi_persist = zcl_persist_apm=>get_instance( ).
  ENDMETHOD.


  METHOD constructor.

    IF zcl_package_json_valid=>is_valid_sap_package( iv_package ) = abap_false.
      zcx_error=>raise( |Invalid package: { iv_package }| ).
    ENDIF.

    mv_package              = iv_package.
    ms_package_json-name    = iv_name.
    ms_package_json-version = iv_version.
    ms_package_json-private = iv_private.

    mv_key = get_package_key( mv_package ).

    TRY.
        zif_package_json~load( ).
      CATCH zcx_error ##NO_HANDLER.
    ENDTRY.

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


  METHOD get_package_from_key.

    DATA:
      lv_prefix TYPE string,
      lv_suffix TYPE string.

    SPLIT iv_key AT ':' INTO lv_prefix result lv_suffix.
    result = to_upper( result ).

  ENDMETHOD.


  METHOD get_package_key.
    result = |{ zif_persist_apm=>c_key_type-package }:{ iv_package }:{ zif_persist_apm=>c_key_extra-package_json }|.
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


  METHOD list.

    DATA:
      lt_list   TYPE zif_persist_apm=>ty_list,
      ls_result LIKE LINE OF result.

    FIELD-SYMBOLS <ls_list> LIKE LINE OF lt_list.

    lt_list = gi_persist->list( zif_persist_apm=>c_key_type-package && |:{ iv_filter }%:|
      && zif_persist_apm=>c_key_extra-package_json ).

    LOOP AT lt_list ASSIGNING <ls_list>.
      CLEAR ls_result.
      ls_result-key            = <ls_list>-keys.
      ls_result-package        = get_package_from_key( <ls_list>-keys ).
      ls_result-changed_by     = <ls_list>-user.
      ls_result-changed_at_raw = <ls_list>-timestamp.
      ls_result-changed_at     = zcl_abapgit_gui_chunk_lib=>render_timestamp( <ls_list>-timestamp ).

      IF iv_instanciate = abap_true.
        TRY.
            ls_result-instance    = factory( ls_result-package )->load( ).
            ls_result-name        = ls_result-instance->get( )-name.
            ls_result-version     = ls_result-instance->get( )-version.
            ls_result-description = ls_result-instance->get( )-description.
            ls_result-type        = ls_result-instance->get( )-type.
            ls_result-private     = ls_result-instance->get( )-private.
          CATCH zcx_error ##NO_HANDLER.
        ENDTRY.
      ENDIF.

      INSERT ls_result INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD sort_dependencies.
    result = is_package_json.
    SORT result-dependencies BY name.
    SORT result-dev_dependencies BY name.
    SORT result-optional_dependencies BY name.
    SORT result-engines BY name.
  ENDMETHOD.


  METHOD zif_package_json~delete.
    gi_persist->delete( mv_key ).
  ENDMETHOD.


  METHOD zif_package_json~exists.
    TRY.
        gi_persist->load( mv_key ).
        result = abap_true.
      CATCH zcx_error.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_package_json~get.
    result = ms_package_json.
  ENDMETHOD.


  METHOD zif_package_json~get_json.

    DATA:
      li_json       TYPE REF TO zif_ajson,
      ls_dependency TYPE zif_package_json_types=>ty_dependency,
      lx_error      TYPE REF TO zcx_ajson_error.

    TRY.
        li_json = zcl_ajson=>new( )->keep_item_order( )->set(
          iv_path = '/'
          iv_val  = ms_package_json ).

        li_json = li_json->map( zcl_ajson_mapping=>create_to_camel_case( ) ).

        " Transpose dependencies
        li_json->setx( '/dependencies:{ }' ).
        LOOP AT ms_package_json-dependencies INTO ls_dependency.
          li_json->set(
            iv_path = '/dependencies/' && ls_dependency-name
            iv_val  = ls_dependency-range ).
        ENDLOOP.
        li_json->setx( '/devDependencies:{ }' ).
        LOOP AT ms_package_json-dev_dependencies INTO ls_dependency.
          li_json->set(
            iv_path = '/devDependencies/' && ls_dependency-name
            iv_val  = ls_dependency-range ).
        ENDLOOP.
        li_json->setx( '/optionalDependencies:{ }' ).
        LOOP AT ms_package_json-optional_dependencies INTO ls_dependency.
          li_json->set(
            iv_path = '/optionalDependencies/' && ls_dependency-name
            iv_val  = ls_dependency-range ).
        ENDLOOP.
        li_json->setx( '/engines:{ }' ).
        LOOP AT ms_package_json-engines INTO ls_dependency.
          li_json->set(
            iv_path = '/engines/' && ls_dependency-name
            iv_val  = ls_dependency-range ).
        ENDLOOP.

        IF iv_complete = abap_false.
          li_json = li_json->filter( lcl_ajson_filters=>create_empty_filter( ) ).
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
        result = boolc( zcl_package_json_valid=>check( ms_package_json ) IS INITIAL ).
      CATCH zcx_error.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_package_json~load.
    zif_package_json~set_json( gi_persist->load( mv_key )-value ).
    result = me.
  ENDMETHOD.


  METHOD zif_package_json~save.
    zcl_package_json_valid=>check( ms_package_json ).
    gi_persist->save(
      iv_key   = mv_key
      iv_value = zif_package_json~get_json( ) ).
  ENDMETHOD.


  METHOD zif_package_json~set.
    zcl_package_json_valid=>check( is_json ).
    ms_package_json = sort_dependencies( is_json ).
    result = me.
  ENDMETHOD.


  METHOD zif_package_json~set_json.

    " TODO: AJSON does not allow for mapping of ABAP to JSON objects like { "user1", "user2", ... }
    " A table would map to an array [ "user1", "user2", ... ]

    TYPES:
      " Copy of schema but without dependencies (instead of array)
      BEGIN OF ty_package_json_wo_deps,
        name                 TYPE string,
        version              TYPE string,
        description          TYPE string,
        keywords             TYPE string_table,
        homepage             TYPE string,
        BEGIN OF bugs,
          url   TYPE zif_package_json_types=>ty_uri,
          email TYPE zif_package_json_types=>ty_email,
        END OF bugs,
        license              TYPE string,
        author               TYPE zif_package_json_types=>ty_person,
        contributors         TYPE STANDARD TABLE OF zif_package_json_types=>ty_person WITH KEY name,
        maintainers          TYPE STANDARD TABLE OF zif_package_json_types=>ty_person WITH KEY name,
        main                 TYPE string,
        man                  TYPE string_table,
        type                 TYPE string,
        BEGIN OF repository,
          type      TYPE string,
          url       TYPE zif_package_json_types=>ty_uri,
          directory TYPE string,
        END OF repository,
        BEGIN OF funding,
          type TYPE string,
          url  TYPE zif_package_json_types=>ty_uri,
        END OF funding,
        bundled_dependencies TYPE string_table,
        os                   TYPE string_table,
        cpu                  TYPE string_table,
        db                   TYPE string_table,
        private              TYPE abap_bool,
        deprecated           TYPE abap_bool,
        BEGIN OF dist,
          file_count    TYPE i,
          integrity     TYPE string,
          shasum        TYPE string,
          signatures    TYPE STANDARD TABLE OF zif_package_json_types=>ty_signature WITH DEFAULT KEY,
          tarball       TYPE string,
          unpacked_size TYPE i,
        END OF dist,
        readme               TYPE string,
      END OF ty_package_json_wo_deps.

    DATA:
      li_json         TYPE REF TO zif_ajson,
      ls_json_wo_deps TYPE ty_package_json_wo_deps,
      ls_dependency   TYPE zif_package_json_types=>ty_dependency,
      ls_json         TYPE zif_package_json_types=>ty_package_json,
      lx_error        TYPE REF TO zcx_ajson_error.

    TRY.
        li_json = zcl_ajson=>parse( iv_json ).
        li_json->to_abap(
          EXPORTING
            iv_corresponding = abap_true
          IMPORTING
            ev_container     = ls_json_wo_deps ).

        MOVE-CORRESPONDING ls_json_wo_deps TO ls_json.

        " Transpose dependencies
        LOOP AT li_json->members( '/dependencies' ) INTO ls_dependency-name.
          ls_dependency-range = li_json->get( '/dependencies/' && ls_dependency-name ).
          INSERT ls_dependency INTO TABLE ls_json-dependencies.
        ENDLOOP.
        LOOP AT li_json->members( '/devDependencies' ) INTO ls_dependency-name.
          ls_dependency-range = li_json->get( '/devDependencies/' && ls_dependency-name ).
          INSERT ls_dependency INTO TABLE ls_json-dev_dependencies.
        ENDLOOP.
        LOOP AT li_json->members( '/optDependencies' ) INTO ls_dependency-name.
          ls_dependency-range = li_json->get( '/optDependencies/' && ls_dependency-name ).
          INSERT ls_dependency INTO TABLE ls_json-optional_dependencies.
        ENDLOOP.
        LOOP AT li_json->members( '/engines' ) INTO ls_dependency-name.
          ls_dependency-range = li_json->get( '/engines/' && ls_dependency-name ).
          INSERT ls_dependency INTO TABLE ls_json-engines.
        ENDLOOP.

        zcl_package_json_valid=>check( ls_json ).

        ms_package_json = sort_dependencies( ls_json ).

      CATCH zcx_ajson_error INTO lx_error.
        zcx_error=>raise_with_text( lx_error ).
    ENDTRY.

    result = me.

  ENDMETHOD.
ENDCLASS.
