CLASS ltcl_package_json DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    DATA mi_package TYPE REF TO zif_package_json.

    METHODS create
      IMPORTING
        iv_args TYPE string
      RAISING
        zcx_package_json.

    METHODS valid
      IMPORTING
        iv_args TYPE string.

    METHODS invalid
      IMPORTING
        iv_args TYPE string.

    METHODS compare
      IMPORTING
        is_json TYPE zif_package_json=>ty_package_json
        iv_json TYPE string.

    METHODS:
      valid_packages FOR TESTING,
      invalid_packages FOR TESTING,
      get_package FOR TESTING RAISING zcx_package_json,
      set_package FOR TESTING RAISING zcx_package_json.

ENDCLASS.

CLASS zcl_package_json DEFINITION LOCAL FRIENDS ltcl_package_json.

CLASS ltcl_package_json IMPLEMENTATION.

  METHOD create.

    DATA:
      lv_package TYPE devclass,
      lv_name    TYPE string,
      lv_version TYPE string,
      lv_private TYPE abap_bool.

    FREE mi_package.

    SPLIT iv_args AT ',' INTO lv_package lv_name lv_version lv_private.

    CREATE OBJECT mi_package TYPE zcl_package_json
      EXPORTING
        iv_package = lv_package
        iv_name    = lv_name
        iv_version = lv_version
        iv_private = lv_private.

  ENDMETHOD.

  METHOD valid.

    DATA lx_error TYPE REF TO zcx_package_json.

    TRY.
        create( iv_args ).
        IF mi_package->is_valid( ) = abap_false.
          cl_abap_unit_assert=>fail( |Invalid but expected valid: { iv_args }| ).
        ENDIF.
      CATCH zcx_package_json INTO lx_error.
        cl_abap_unit_assert=>fail( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD invalid.
    TRY.
        create( iv_args ).
        IF mi_package->is_valid( ) = abap_true.
          cl_abap_unit_assert=>fail( |Valid but expected invalid: { iv_args }| ).
        ENDIF.
      CATCH zcx_package_json.
    ENDTRY.
  ENDMETHOD.

  METHOD compare.

    DATA lv_json TYPE string.

    cl_abap_unit_assert=>assert_equals(
      act = mi_package->get( )
      exp = is_json ).

    " Strip newlines and condense for easier comparison
    lv_json = condense( replace(
      val  = mi_package->get_json( )
      sub  = cl_abap_char_utilities=>newline
      with = ''
      occ  = 0 ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_json
      exp = iv_json ).

  ENDMETHOD.

  METHOD valid_packages.
    " Various SAP packages
    valid( '$TEST,test,1.0.0' ).
    valid( 'YTEST,test,1.0.0' ).
    valid( 'ZTEST,test,1.0.0' ).
    valid( '/NAMESPC/TEST,test,1.0.0' ).
    " Various names
    valid( '$TEST,test_long_name-with-special-characters,1.0.0' ).
    valid( '$TEST,@scope/test,1.0.0' ).
    " Various versions
    valid( '$TEST,test,1.0.0' ).
    valid( '$TEST,test,1.2.3' ).
    valid( '$TEST,test,3.0.99' ).
    valid( '$TEST,test,1.0.0-prerelease.10' ).
  ENDMETHOD.

  METHOD invalid_packages.
    " Missing input
    invalid( ',test,1.0.0' ).
    invalid( '$TEST,,1.0.0' ).
    invalid( '$TEST,test,' ).
    " Various SAP packages
    invalid( 'TEST,test,1.0.0' ).
    invalid( '$TE^ST,test,1.0.0' ).
    invalid( '$test,test,1.0.0' ).
    invalid( ' $TEST,test,1.0.0' ).
    invalid( '$TE ST,test,1.0.0' ).
    invalid( '//TEST,test,1.0.0' ).
    invalid( '/TEST/,test,1.0.0' ).
    invalid( '/NAMESPACE/TEST,test,1.0.0' ).
    invalid( '/0SAP/TEST,test,1.0.0' ).
    " Various names
    invalid( '$TEST, test,1.0.0' ).
    invalid( '$TEST,te st,1.0.0' ).
    invalid( '$TEST,/test,1.0.0' ).
    invalid( '$TEST,test/,1.0.0' ).
    invalid( '$TEST,scope/test,1.0.0' ).
    invalid( '$TEST,t,1.0.0' ).
    invalid( '$TEST,tt,1.0.0' ).
    invalid( |$TEST,{ repeat( val = 't' occ = 215 ) },1.0.0| ).
    " Various versions
    invalid( '$TEST,test,1.0' ).
    invalid( '$TEST,test,1' ).
  ENDMETHOD.

  METHOD get_package.

    DATA ls_json TYPE zif_package_json=>ty_package_json.

    create( '$TEST,test,1.0.0' ).

    CLEAR ls_json.
    ls_json-name    = 'test'.
    ls_json-version = '1.0.0'.

    compare(
      is_json = ls_json
      iv_json = '{ "name": "test", "version": "1.0.0"}' ).

    create( '$TEST,test,1.0.0,X' ).

    CLEAR ls_json.
    ls_json-name    = 'test'.
    ls_json-version = '1.0.0'.
    ls_json-private = abap_true.

    compare(
      is_json = ls_json
      iv_json = '{ "name": "test", "version": "1.0.0", "private": true}' ).

    create( '/TEST/TEST,@test/test,1.2.3' ).

    CLEAR ls_json.
    ls_json-name    = '@test/test'.
    ls_json-version = '1.2.3'.

    compare(
      is_json = ls_json
      iv_json = '{ "name": "@test/test", "version": "1.2.3"}' ).

  ENDMETHOD.

  METHOD set_package.

    DATA ls_json TYPE zif_package_json=>ty_package_json.

    create( '$TEST,test,1.0.0' ).

    CLEAR ls_json.
    ls_json-name    = 'test'.
    ls_json-version = '1.0.0'.
    ls_json-private = abap_true.

    mi_package->set( ls_json ).

    compare(
      is_json = ls_json
      iv_json = '{ "name": "test", "version": "1.0.0", "private": true}' ).

    create( '$TEST,test,1.0.0' ).

    CLEAR ls_json.
    ls_json-name    = 'test'.
    ls_json-version = '1.0.0'.
    ls_json-description = 'My package test'.

    mi_package->set( ls_json ).

    compare(
      is_json = ls_json
      iv_json = '{ "name": "test", "version": "1.0.0", "description": "My package test"}' ).

    CLEAR ls_json.
    ls_json-name    = 'test-2'.
    ls_json-version = '1.2.3'.

    mi_package->set_json( |\{\n "name": "test-2",\n "version": "1.2.3"\n\}\n| ).

    compare(
      is_json = ls_json
      iv_json = '{ "name": "test-2", "version": "1.2.3"}' ).

  ENDMETHOD.

ENDCLASS.
