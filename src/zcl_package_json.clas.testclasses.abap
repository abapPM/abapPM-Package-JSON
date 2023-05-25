CLASS ltcl_package_json DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    DATA mi_package TYPE REF TO zif_package_json.

    METHODS init_test
      IMPORTING
        iv_args TYPE string
      RAISING
        zcx_package_json.

    METHODS test_valid
      IMPORTING
        iv_args TYPE string.

    METHODS test_invalid
      IMPORTING
        iv_args TYPE string.

    METHODS test_compare
      IMPORTING
        is_json TYPE zif_package_json=>ty_package_json
        iv_json TYPE string.

    METHODS:
      valid_packages FOR TESTING,
      invalid_packages FOR TESTING,
      get_complete FOR TESTING RAISING zcx_package_json,
      get_package FOR TESTING RAISING zcx_package_json,
      set_package FOR TESTING RAISING zcx_package_json,
      dependencies FOR TESTING RAISING zcx_package_json.

ENDCLASS.

CLASS zcl_package_json DEFINITION LOCAL FRIENDS ltcl_package_json.

CLASS ltcl_package_json IMPLEMENTATION.

  METHOD init_test.

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

  METHOD test_valid.

    DATA lx_error TYPE REF TO zcx_package_json.

    TRY.
        init_test( iv_args ).
        IF mi_package->is_valid( ) = abap_false.
          cl_abap_unit_assert=>fail( |Invalid but expected valid: { iv_args }| ).
        ENDIF.
      CATCH zcx_package_json INTO lx_error.
        cl_abap_unit_assert=>fail( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_invalid.
    TRY.
        init_test( iv_args ).
        IF mi_package->is_valid( ) = abap_true.
          cl_abap_unit_assert=>fail( |Valid but expected invalid: { iv_args }| ).
        ENDIF.
      CATCH zcx_package_json.
    ENDTRY.
  ENDMETHOD.

  METHOD test_compare.

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
    test_valid( '$TEST,test,1.0.0' ).
    test_valid( 'YTEST,test,1.0.0' ).
    test_valid( 'ZTEST,test,1.0.0' ).
    test_valid( '/NAMESPC/TEST,test,1.0.0' ).
    " Various names
    test_valid( '$TEST,test_long_name-with-special-characters,1.0.0' ).
    test_valid( '$TEST,@scope/test,1.0.0' ).
    " Various versions
    test_valid( '$TEST,test,1.0.0' ).
    test_valid( '$TEST,test,1.2.3' ).
    test_valid( '$TEST,test,3.0.99' ).
    test_valid( '$TEST,test,1.0.0-prerelease.10' ).
  ENDMETHOD.

  METHOD invalid_packages.
    " Missing input
    test_invalid( ',test,1.0.0' ).
    test_invalid( '$TEST,,1.0.0' ).
    test_invalid( '$TEST,test,' ).
    " Various SAP packages
    test_invalid( 'TEST,test,1.0.0' ).
    test_invalid( '$TE^ST,test,1.0.0' ).
    test_invalid( '$test,test,1.0.0' ).
    test_invalid( ' $TEST,test,1.0.0' ).
    test_invalid( '$TE ST,test,1.0.0' ).
    test_invalid( '//TEST,test,1.0.0' ).
    test_invalid( '/TEST/,test,1.0.0' ).
    test_invalid( '/NAMESPACE/TEST,test,1.0.0' ).
    test_invalid( '/0SAP/TEST,test,1.0.0' ).
    " Various names
    test_invalid( '$TEST, test,1.0.0' ).
    test_invalid( '$TEST,te st,1.0.0' ).
    test_invalid( '$TEST,/test,1.0.0' ).
    test_invalid( '$TEST,test/,1.0.0' ).
    test_invalid( '$TEST,scope/test,1.0.0' ).
    test_invalid( '$TEST,t,1.0.0' ).
    test_invalid( '$TEST,tt,1.0.0' ).
    test_invalid( |$TEST,{ repeat( val = 't' occ = 215 ) },1.0.0| ).
    " Various versions
    test_invalid( '$TEST,test,1.0' ).
    test_invalid( '$TEST,test,1' ).
  ENDMETHOD.

  METHOD get_complete.

    DATA lv_json TYPE string.

    init_test( '$TEST,test,1.0.0' ).

    lv_json = |\{\n|
      && |  "name": "test",\n|
      && |  "version": "1.0.0",\n|
      && |  "description": "",\n|
      && |  "keywords": [],\n|
      && |  "homepage": "",\n|
      && |  "bugs": \{\n|
      && |    "url": "",\n|
      && |    "email": ""\n|
      && |  \},\n|
      && |  "license": "",\n|
      && |  "author": \{\n|
      && |    "name": "",\n|
      && |    "url": "",\n|
      && |    "email": ""\n|
      && |  \},\n|
      && |  "contributors": [],\n|
      && |  "maintainers": [],\n|
      && |  "main": "",\n|
      && |  "man": [],\n|
      && |  "type": "",\n|
      && |  "repository": \{\n|
      && |    "type": "",\n|
      && |    "url": "",\n|
      && |    "directory": ""\n|
      && |  \},\n|
      && |  "funding": \{\n|
      && |    "type": "",\n|
      && |    "url": ""\n|
      && |  \},\n|
      && |  "dependencies": [],\n|
      && |  "devDependencies": [],\n|
      && |  "optionalDependencies": [],\n|
      && |  "bundledDependencies": [],\n|
      && |  "packageManager": "",\n|
      && |  "engines": [],\n|
      && |  "os": [],\n|
      && |  "cpu": [],\n|
      && |  "db": [],\n|
      && |  "private": false,\n|
      && |  "dist": \{\n|
      && |    "shasum": "",\n|
      && |    "tarball": ""\n|
      && |  \},\n|
      && |  "readme": ""\n|
      && |\}|.

    cl_abap_unit_assert=>assert_equals(
      act = mi_package->get_json( iv_complete = abap_true )
      exp = lv_json ).

  ENDMETHOD.

  METHOD get_package.

    DATA ls_json TYPE zif_package_json=>ty_package_json.

    init_test( '$TEST,test,1.0.0' ).

    CLEAR ls_json.
    ls_json-name    = 'test'.
    ls_json-version = '1.0.0'.

    test_compare(
      is_json = ls_json
      iv_json = '{ "name": "test", "version": "1.0.0"}' ).

    init_test( '$TEST,test,1.0.0,X' ).

    CLEAR ls_json.
    ls_json-name    = 'test'.
    ls_json-version = '1.0.0'.
    ls_json-private = abap_true.

    test_compare(
      is_json = ls_json
      iv_json = '{ "name": "test", "version": "1.0.0", "private": true}' ).

    init_test( '/TEST/TEST,@test/test,1.2.3' ).

    CLEAR ls_json.
    ls_json-name    = '@test/test'.
    ls_json-version = '1.2.3'.

    test_compare(
      is_json = ls_json
      iv_json = '{ "name": "@test/test", "version": "1.2.3"}' ).

  ENDMETHOD.

  METHOD set_package.

    DATA ls_json TYPE zif_package_json=>ty_package_json.

    init_test( '$TEST' ).

    CLEAR ls_json.
    ls_json-name    = 'test'.
    ls_json-version = '1.0.0'.
    ls_json-private = abap_true.

    mi_package->set( ls_json ).

    test_compare(
      is_json = ls_json
      iv_json = '{ "name": "test", "version": "1.0.0", "private": true}' ).

    init_test( '$TEST' ).

    CLEAR ls_json.
    ls_json-name    = 'test'.
    ls_json-version = '1.0.0'.
    ls_json-description = 'My package test'.

    mi_package->set( ls_json ).

    test_compare(
      is_json = ls_json
      iv_json = '{ "name": "test", "version": "1.0.0", "description": "My package test"}' ).

    init_test( '$TEST' ).

    CLEAR ls_json.
    ls_json-name    = 'test-2'.
    ls_json-version = '1.2.3'.

    mi_package->set_json( |\{\n "name": "test-2",\n "version": "1.2.3"\n\}\n| ).

    test_compare(
      is_json = ls_json
      iv_json = '{ "name": "test-2", "version": "1.2.3"}' ).

  ENDMETHOD.

  METHOD dependencies.

    DATA:
      ls_json TYPE zif_package_json=>ty_package_json,
      ls_dep  TYPE zif_package_json=>ty_dependency.

    init_test( '$TEST' ).

    CLEAR ls_json.
    ls_json-name    = 'test'.
    ls_json-version = '1.0.0'.

    ls_dep-name    = 'dep1'.
    ls_dep-version = '2.0.0'.
    INSERT ls_dep INTO TABLE ls_json-dependencies.
    ls_dep-name    = 'dep2'.
    ls_dep-version = '>3'.
    INSERT ls_dep INTO TABLE ls_json-dependencies.
    ls_dep-name    = 'dep3'.
    ls_dep-version = '^4.1.0'.
    INSERT ls_dep INTO TABLE ls_json-dependencies.

    mi_package->set( ls_json ).

    test_compare(
      is_json = ls_json
      iv_json = '{ "name": "test", "version": "1.0.0", "dependencies": { "dep1": "2.0.0", "dep2": ">3", "dep3": "^4.1.0"}}' ).

  ENDMETHOD.
ENDCLASS.
