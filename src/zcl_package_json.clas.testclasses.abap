CLASS ltcl_package_json DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    DATA cut TYPE REF TO zif_package_json.

    METHODS init_test
      IMPORTING
        args TYPE string
      RAISING
        zcx_error.

    METHODS test_valid
      IMPORTING
        args TYPE string.

    METHODS test_invalid
      IMPORTING
        args TYPE string.

    METHODS test_compare
      IMPORTING
        package_json TYPE zif_types=>ty_package_json
        json_data    TYPE string
      RAISING
        zcx_error.

    METHODS:
      valid_packages FOR TESTING,
      invalid_packages FOR TESTING,
      get_complete FOR TESTING RAISING zcx_error,
      get_package FOR TESTING RAISING zcx_error,
      set_package FOR TESTING RAISING zcx_error,
      get_persons FOR TESTING RAISING zcx_error,
      set_persons FOR TESTING RAISING zcx_error,
      dependencies_json_to_abap FOR TESTING RAISING zcx_error,
      dependencies_abap_to_json FOR TESTING RAISING zcx_error.

ENDCLASS.

CLASS zcl_package_json DEFINITION LOCAL FRIENDS ltcl_package_json.

CLASS ltcl_package_json IMPLEMENTATION.

  METHOD init_test.

    FREE cut.

    SPLIT args AT ',' INTO DATA(package) DATA(name) DATA(version) DATA(private).

    cut = NEW zcl_package_json(
      package = CONV devclass( package )
      name    = name
      version = version
      private = CONV abap_bool( private ) ).

  ENDMETHOD.

  METHOD test_valid.

    TRY.
        init_test( args ).
        IF cut->is_valid( ) = abap_false.
          cl_abap_unit_assert=>fail( |Invalid but expected valid: { args }| ).
        ENDIF.
      CATCH zcx_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_invalid.

    TRY.
        init_test( args ).
        IF cut->is_valid( ) = abap_true.
          cl_abap_unit_assert=>fail( |Valid but expected invalid: { args }| ).
        ENDIF.
      CATCH zcx_error.
    ENDTRY.

  ENDMETHOD.

  METHOD test_compare.

    cl_abap_unit_assert=>assert_equals(
      act = cut->get( )
      exp = package_json ).

    " Strip newlines and condense for easier comparison
    DATA(json) = condense( replace(
      val  = cut->get_json( )
      sub  = cl_abap_char_utilities=>newline
      with = ''
      occ  = 0 ) ).

    cl_abap_unit_assert=>assert_equals(
      act = json
      exp = json_data ).

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

    init_test( '$TEST' ).

    DATA(package_json) = VALUE zif_types=>ty_package_json(
      name         = 'test'
      version      = '1.0.0'
      author-name  = 'Marc'
      author-email = 'marc@test.com'
      private      = abap_true ).

    DATA(dependency) = VALUE zif_types=>ty_dependency(
      name  = 'dep2'
      range = '2.0.0' ).
    INSERT dependency INTO TABLE package_json-dependencies.
    dependency-name  = 'dep3'.
    dependency-range = '>3'.
    INSERT dependency INTO TABLE package_json-dev_dependencies.
    dependency-name  = 'dep4'.
    dependency-range = '^4.1.0'.
    INSERT dependency INTO TABLE package_json-optional_dependencies.
    dependency-name  = 'dep5'.
    dependency-range = '^5.0.1'.
    INSERT dependency INTO TABLE package_json-peer_dependencies.
    INSERT `dep2` INTO TABLE package_json-bundle_dependencies.
    dependency-name  = 'abap'.
    dependency-range = '>=7.50'.
    INSERT dependency INTO TABLE package_json-engines.
    dependency-name  = 'apm'.
    dependency-range = '>=1'.
    INSERT dependency INTO TABLE package_json-engines.

    cut->set( package_json ).

    DATA(json) = |\{\n|
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
      && |    "name": "Marc",\n|
      && |    "url": "",\n|
      && |    "email": "marc@test.com",\n|
      && |    "avatar": ""\n|
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
      && |  "dependencies": \{\n|
      && |    "dep2": "2.0.0"\n|
      && |  \},\n|
      && |  "devDependencies": \{\n|
      && |    "dep3": ">3"\n|
      && |  \},\n|
      && |  "optionalDependencies": \{\n|
      && |    "dep4": "^4.1.0"\n|
      && |  \},\n|
      && |  "peerDependencies": \{\n|
      && |    "dep5": "^5.0.1"\n|
      && |  \},\n|
      && |  "bundleDependencies": [\n|
      && |    "dep2"\n|
      && |  ],\n|
      && |  "engines": \{\n|
      && |    "abap": ">=7.50",\n|
      && |    "apm": ">=1"\n|
      && |  \},\n|
      && |  "os": [],\n|
      && |  "cpu": [],\n|
      && |  "db": [],\n|
      && |  "private": true,\n|
      && |  "readme": ""\n|
      && |\}|.

    cl_abap_unit_assert=>assert_equals(
      act = cut->get_json( abap_true )
      exp = json ).

  ENDMETHOD.

  METHOD get_package.

    init_test( '$TEST,test,1.0.0' ).

    DATA(package_json) = VALUE zif_types=>ty_package_json(
      name    = 'test'
      version = '1.0.0' ).

    test_compare(
      package_json = package_json
      json_data    = '{ "name": "test", "version": "1.0.0"}' ).

    init_test( '$TEST,test,1.0.0,X' ).

    CLEAR package_json.
    package_json-name    = 'test'.
    package_json-version = '1.0.0'.
    package_json-private = abap_true.

    test_compare(
      package_json = package_json
      json_data    = '{ "name": "test", "version": "1.0.0", "private": true}' ).

    init_test( '/TEST/TEST,@test/test,1.2.3' ).

    CLEAR package_json.
    package_json-name    = '@test/test'.
    package_json-version = '1.2.3'.

    test_compare(
      package_json = package_json
      json_data    = '{ "name": "@test/test", "version": "1.2.3"}' ).

  ENDMETHOD.

  METHOD set_package.

    init_test( '$TEST' ).

    DATA(package_json) = VALUE zif_types=>ty_package_json(
      name    = 'test'
      version = '1.0.0'
      private = abap_true ).

    cut->set( package_json ).

    test_compare(
      package_json = package_json
      json_data    = '{ "name": "test", "version": "1.0.0", "private": true}' ).

    init_test( '$TEST' ).

    CLEAR package_json.
    package_json-name    = 'test'.
    package_json-version = '1.0.0'.
    package_json-description = 'My package test'.

    cut->set( package_json ).

    test_compare(
      package_json = package_json
      json_data    = '{ "name": "test", "version": "1.0.0", "description": "My package test"}' ).

    init_test( '$TEST' ).

    CLEAR package_json.
    package_json-name    = 'test-2'.
    package_json-version = '1.2.3'.

    cut->set_json( |\{\n "name": "test-2",\n "version": "1.2.3"\n\}\n| ).

    test_compare(
      package_json = package_json
      json_data    = '{ "name": "test-2", "version": "1.2.3"}' ).

  ENDMETHOD.

  METHOD get_persons.
    ASSERT 0 = 0.
  ENDMETHOD.

  METHOD set_persons.
    ASSERT 0 = 0.
  ENDMETHOD.

  METHOD dependencies_json_to_abap.

    init_test( '$TEST' ).

    DATA(json) = |\{\n|
      && |  "name": "test",\n|
      && |  "version": "1.0.0",\n|
      && |  "devDependencies": \{\n|
      && |    "dep1": "2.0.0",\n|
      && |    "dep2": ">3",\n|
      && |    "dep3": "^4.1.0"\n|
      && |  \}\n|
      && |\}|.

    cut->set_json( json ).

    DATA(package_json) = VALUE zif_types=>ty_package_json(
      name    = 'test'
      version = '1.0.0' ).

    DATA(dependency) = VALUE zif_types=>ty_dependency(
      name  = 'dep1'
      range = '2.0.0' ).
    INSERT dependency INTO TABLE package_json-dev_dependencies.
    dependency-name  = 'dep2'.
    dependency-range = '>3'.
    INSERT dependency INTO TABLE package_json-dev_dependencies.
    dependency-name  = 'dep3'.
    dependency-range = '^4.1.0'.
    INSERT dependency INTO TABLE package_json-dev_dependencies.

    cl_abap_unit_assert=>assert_equals(
      act = cut->get( )
      exp = package_json ).

  ENDMETHOD.

  METHOD dependencies_abap_to_json.

    init_test( '$TEST' ).

    DATA(package_json) = VALUE zif_types=>ty_package_json(
      name    = 'test'
      version = '1.0.0' ).

    DATA(dependency) = VALUE zif_types=>ty_dependency(
      name  = 'dep1'
      range = '2.0.0' ).
    INSERT dependency INTO TABLE package_json-dev_dependencies.
    dependency-name  = 'dep2'.
    dependency-range = '>3'.
    INSERT dependency INTO TABLE package_json-dev_dependencies.
    dependency-name  = 'dep3'.
    dependency-range = '^4.1.0'.
    INSERT dependency INTO TABLE package_json-dev_dependencies.

    cut->set( package_json ).

    DATA(json) = |\{\n|
      && |  "name": "test",\n|
      && |  "version": "1.0.0",\n|
      && |  "devDependencies": \{\n|
      && |    "dep1": "2.0.0",\n|
      && |    "dep2": ">3",\n|
      && |    "dep3": "^4.1.0"\n|
      && |  \}\n|
      && |\}|.

    cl_abap_unit_assert=>assert_equals(
      act = cut->get_json( )
      exp = json ).

  ENDMETHOD.
ENDCLASS.
