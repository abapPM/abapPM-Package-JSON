CLASS ltcl_package_json DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    DATA:
      cut                       TYPE REF TO /apmg/if_package_json,
      test_package_json         TYPE /apmg/if_types=>ty_package_json,
      test_manifest             TYPE /apmg/if_types=>ty_manifest,
      test_manifest_abbreviated TYPE /apmg/if_types=>ty_manifest_abbreviated,
      test_json                 TYPE string,
      test_json_full            TYPE string,
      test_json_abbreviated     TYPE string.

    METHODS setup.

    METHODS prepare_string
      IMPORTING
        input         TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS init_test
      IMPORTING
        args TYPE string
      RAISING
        /apmg/cx_error.

    METHODS test_valid
      IMPORTING
        args TYPE string.

    METHODS test_invalid
      IMPORTING
        args TYPE string.

    METHODS test_compare
      IMPORTING
        package_json TYPE /apmg/if_types=>ty_package_json
        json_data    TYPE string
      RAISING
        /apmg/cx_error.

    METHODS:
      valid_packages FOR TESTING,
      invalid_packages FOR TESTING,
      get_package FOR TESTING RAISING /apmg/cx_error,
      set_package FOR TESTING RAISING /apmg/cx_error,
      get_persons FOR TESTING RAISING /apmg/cx_error,
      set_persons FOR TESTING RAISING /apmg/cx_error,
      dependencies_json_to_abap FOR TESTING RAISING /apmg/cx_error,
      dependencies_abap_to_json FOR TESTING RAISING /apmg/cx_error,
      get_complete FOR TESTING RAISING /apmg/cx_error,
      set_complete FOR TESTING RAISING /apmg/cx_error,
      convert_json_to_manifest FOR TESTING RAISING /apmg/cx_error,
      convert_json_to_manifest_abbr FOR TESTING RAISING /apmg/cx_error,
      convert_manifest_to_json FOR TESTING RAISING /apmg/cx_error,
      convert_manifest_abbr_to_json FOR TESTING RAISING /apmg/cx_error,
      convert_manifest_to_pack_json FOR TESTING RAISING /apmg/cx_error.

ENDCLASS.

CLASS /apmg/cl_package_json DEFINITION LOCAL FRIENDS ltcl_package_json.

CLASS ltcl_package_json IMPLEMENTATION.

  METHOD setup.

    test_json = ``
      && `{`
      && `|  "name": "test",`
      && `|  "version": "1.0.0",`
      && `|  "description": "hello world",`
      && `|  "type": "module",`
      && `|  "keywords": [`
      && `|    "apm"`
      && `|  ],`
      && `|  "homepage": "https://abappm.com",`
      && `|  "icon": "https://abappm.com/apm_logo.svg",`
      && `|  "bugs": {`
      && `|    "url": "https://abappm.com/bugs",`
      && `|    "email": "support@test.com"`
      && `|  },`
      && `|  "license": "MIT",`
      && `|  "author": {`
      && `|    "name": "Marc",`
      && `|    "url": "https://abappm.com",`
      && `|    "email": "marc@test.com",`
      && `|    "avatar": "https://gravatar.com/abapPM"`
      && `|  },`
      && `|  "contributors": [`
      && `|    {`
      && `|      "name": "Marc",`
      && `|      "url": "https://abappm.com",`
      && `|      "email": "marc@test.com",`
      && `|      "avatar": "https://gravatar.com/abapPM"`
      && `|    }`
      && `|  ],`
      && `|  "maintainers": [`
      && `|    {`
      && `|      "name": "Marc",`
      && `|      "url": "https://abappm.com",`
      && `|      "email": "marc@test.com",`
      && `|      "avatar": "https://gravatar.com/abapPM"`
      && `|    }`
      && `|  ],`
      && `|  "main": "test.prog",`
      && `|  "man": [`
      && `|    "manual.md"`
      && `|  ],`
      && `|  "repository": {`
      && `|    "type": "http",`
      && `|    "url": "https://github.com/abapPM/abapPM",`
      && `|    "directory": "subdir"`
      && `|  },`
      && `|  "funding": {`
      && `|    "type": "github",`
      && `|    "url": "https://github.com/abapPM"`
      && `|  },`
      && `|  "dependencies": {`
      && `|    "dep2": "2.0.0"`
      && `|  },`
      && `|  "devDependencies": {`
      && `|    "dep3": ">3"`
      && `|  },`
      && `|  "optionalDependencies": {`
      && `|    "dep4": "^4.1.0"`
      && `|  },`
      && `|  "peerDependencies": {`
      && `|    "dep5": "^5.0.1"`
      && `|  },`
      && `|  "bundleDependencies": [`
      && `|    "dep2"`
      && `|  ],`
      && `|  "engines": {`
      && `|    "abap": ">=7.50",`
      && `|    "apm": ">=1"`
      && `|  },`
      && `|  "os": [`
      && `|    "linux"`
      && `|  ],`
      && `|  "cpu": [`
      && `|    "x86-64"`
      && `|  ],`
      && `|  "db": [`
      && `|    "hdb"`
      && `|  ],`
      && `|  "private": true,`
      && `|  "readme": "# Readme",`
      && `|  "sapPackage": {`
      && `|    "default": "/APMG/TEST",`
      && `|    "softwareComponent": "HOME",`
      && `|    "abapLanguageVersion": "standard"`
      && `|  }`
      && `|}`.

    test_json_full = ``
      && `{`
      && `|  "name": "test",`
      && `|  "version": "1.0.0",`
      && `|  "description": "hello world",`
      && `|  "type": "module",`
      && `|  "keywords": [`
      && `|    "apm"`
      && `|  ],`
      && `|  "homepage": "https://abappm.com",`
      && `|  "icon": "https://abappm.com/apm_logo.svg",`
      && `|  "bugs": {`
      && `|    "url": "https://abappm.com/bugs",`
      && `|    "email": "support@test.com"`
      && `|  },`
      && `|  "license": "MIT",`
      && `|  "author": {`
      && `|    "name": "Marc",`
      && `|    "url": "https://abappm.com",`
      && `|    "email": "marc@test.com",`
      && `|    "avatar": "https://gravatar.com/abapPM"`
      && `|  },`
      && `|  "contributors": [`
      && `|    {`
      && `|      "name": "Marc",`
      && `|      "url": "https://abappm.com",`
      && `|      "email": "marc@test.com",`
      && `|      "avatar": "https://gravatar.com/abapPM"`
      && `|    }`
      && `|  ],`
      && `|  "maintainers": [`
      && `|    {`
      && `|      "name": "Marc",`
      && `|      "url": "https://abappm.com",`
      && `|      "email": "marc@test.com",`
      && `|      "avatar": "https://gravatar.com/abapPM"`
      && `|    }`
      && `|  ],`
      && `|  "main": "test.prog",`
      && `|  "man": [`
      && `|    "manual.md"`
      && `|  ],`
      && `|  "repository": {`
      && `|    "type": "http",`
      && `|    "url": "https://github.com/abapPM/abapPM",`
      && `|    "directory": "subdir"`
      && `|  },`
      && `|  "funding": {`
      && `|    "type": "github",`
      && `|    "url": "https://github.com/abapPM"`
      && `|  },`
      && `|  "dependencies": {`
      && `|    "dep2": "2.0.0"`
      && `|  },`
      && `|  "devDependencies": {`
      && `|    "dep3": ">3"`
      && `|  },`
      && `|  "optionalDependencies": {`
      && `|    "dep4": "^4.1.0"`
      && `|  },`
      && `|  "peerDependencies": {`
      && `|    "dep5": "^5.0.1"`
      && `|  },`
      && `|  "bundleDependencies": [`
      && `|    "dep2"`
      && `|  ],`
      && `|  "engines": {`
      && `|    "abap": ">=7.50",`
      && `|    "apm": ">=1"`
      && `|  },`
      && `|  "os": [`
      && `|    "linux"`
      && `|  ],`
      && `|  "cpu": [`
      && `|    "x86-64"`
      && `|  ],`
      && `|  "db": [`
      && `|    "hdb"`
      && `|  ],`
      && `|  "private": true,`
      && `|  "readme": "# Readme",`
      && `|  "sapPackage": {`
      && `|    "default": "/APMG/TEST",`
      && `|    "softwareComponent": "HOME",`
      && `|    "abapLanguageVersion": "standard"`
      && `|  },`
      && `|  "dist": {`
      && `|    "fileCount": 8,`
      && `|    "shasum": "be",`
      && `|    "tarball": "test-1.0.0.tgz",`
      && `|    "unpackedSize": 2015,`
      && `|    "integrity": "edge",`
      && `|    "signatures": [`
      && `|      {`
      && `|        "keyid": "key",`
      && `|        "sig": "sig"`
      && `|      }`
      && `|    ]`
      && `|  },`
      && `|  "deprecated": true,`
      && `|  "_id": "test@1.0.0",`
      && `|  "_abapVersion": "7.54.0",`
      && `|  "_apmVersion": "1.0.0"`
      && `|}`.

    test_json_abbreviated = ``
      && `{`
      && `|  "name": "test",`
      && `|  "version": "1.0.0",`
      && `|  "dependencies": {`
      && `|    "dep2": "2.0.0"`
      && `|  },`
      && `|  "devDependencies": {`
      && `|    "dep3": ">3"`
      && `|  },`
      && `|  "optionalDependencies": {`
      && `|    "dep4": "^4.1.0"`
      && `|  },`
      && `|  "peerDependencies": {`
      && `|    "dep5": "^5.0.1"`
      && `|  },`
      && `|  "bundleDependencies": [`
      && `|    "dep2"`
      && `|  ],`
      && `|  "engines": {`
      && `|    "abap": ">=7.50",`
      && `|    "apm": ">=1"`
      && `|  },`
      && `|  "os": [`
      && `|    "linux"`
      && `|  ],`
      && `|  "cpu": [`
      && `|    "x86-64"`
      && `|  ],`
      && `|  "db": [`
      && `|    "hdb"`
      && `|  ],`
      && `|  "dist": {`
      && `|    "fileCount": 8,`
      && `|    "shasum": "be",`
      && `|    "tarball": "test-1.0.0.tgz",`
      && `|    "unpackedSize": 2015,`
      && `|    "integrity": "edge",`
      && `|    "signatures": [`
      && `|      {`
      && `|        "keyid": "key",`
      && `|        "sig": "sig"`
      && `|      }`
      && `|    ]`
      && `|  },`
      && `|  "deprecated": true`
      && `|}`.

    test_json             = prepare_string( test_json ).
    test_json_full        = prepare_string( test_json_full ).
    test_json_abbreviated = prepare_string( test_json_abbreviated ).

    DATA(person) = VALUE /apmg/if_types=>ty_person(
      name   = `Marc`
      url    = `https://abappm.com`
      email  = `marc@test.com`
      avatar = `https://gravatar.com/abapPM` ).

    test_package_json = VALUE #(
      name                  = `test`
      version               = `1.0.0`
      description           = `hello world`
      type                  = `module`
      keywords              = VALUE #( ( `apm` ) )
      homepage              = `https://abappm.com`
      icon                  = `https://abappm.com/apm_logo.svg`
      bugs                  = VALUE #(
                                url   = `https://abappm.com/bugs`
                                email = `support@test.com` )
      license               = `MIT`
      author                = person
      contributors          = VALUE #( ( person ) )
      maintainers           = VALUE #( ( person ) )
      main                  = `test.prog`
      man                   = VALUE #( ( `manual.md` ) )
      repository            = VALUE #(
                                type      = `http`
                                url       = `https://github.com/abapPM/abapPM`
                                directory = `subdir` )
      funding               = VALUE #(
                                type = `github`
                                url  = `https://github.com/abapPM` )
      dependencies          = VALUE #( ( key = `dep2` range = `2.0.0` ) )
      dev_dependencies      = VALUE #( ( key = `dep3` range = `>3` ) )
      optional_dependencies = VALUE #( ( key = `dep4` range = `^4.1.0` ) )
      peer_dependencies     = VALUE #( ( key = `dep5` range = `^5.0.1` ) )
      bundle_dependencies   = VALUE #( ( `dep2` ) )
      engines               = VALUE #(
                                ( key = `abap` range = `>=7.50` )
                                ( key = `apm` range = `>=1` ) )
      os                    = VALUE #( ( `linux` ) )
      cpu                   = VALUE #( ( `x86-64` ) )
      db                    = VALUE #( ( `hdb` ) )
      readme                = `# Readme`
      private               = abap_true
      sap_package           = VALUE #(
                                default               = `/APMG/TEST`
                                software_component    = `HOME`
                                abap_language_version = `standard` ) ).

    test_manifest = CORRESPONDING #( test_package_json ).

    test_manifest-deprecated    = abap_true.
    test_manifest-dist          = VALUE #(
                                    file_count    = 8
                                    integrity     = `edge`
                                    shasum        = `be`
                                    unpacked_size = 2015
                                    tarball       = `test-1.0.0.tgz`
                                    signatures    = VALUE #( ( keyid = `key` sig = `sig` ) ) ).
    test_manifest-_id           = `test@1.0.0`.
    test_manifest-_abap_version = `7.54.0`.
    test_manifest-_apm_version  = `1.0.0`.

    test_manifest_abbreviated = CORRESPONDING #( test_manifest ).

  ENDMETHOD.

  METHOD prepare_string.
    result = replace(
      val  = input
      sub  = '|'
      with = |\n|
      occ  = 0 ).
  ENDMETHOD.

  METHOD init_test.

    FREE cut.

    SPLIT args AT ',' INTO DATA(package) DATA(name) DATA(version) DATA(private).

    cut = NEW /apmg/cl_package_json(
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
      CATCH /apmg/cx_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_invalid.

    TRY.
        init_test( args ).
        IF cut->is_valid( ) = abap_true.
          cl_abap_unit_assert=>fail( |Valid but expected invalid: { args }| ).
        ENDIF.
      CATCH /apmg/cx_error.
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

  METHOD get_package.

    init_test( '$TEST,test,1.0.0' ).

    DATA(package_json) = VALUE /apmg/if_types=>ty_package_json(
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

    DATA(package_json) = VALUE /apmg/if_types=>ty_package_json(
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

    cut->set_json( |\{\n "name": "test-2",\n "version": "1.2.3"\n\}| ).

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

    DATA(json) = ``
      && `{`
      && `|  "name": "test",`
      && `|  "version": "1.0.0",`
      && `|  "devDependencies": {`
      && `|    "dep1": "2.0.0",`
      && `|    "dep2": ">3",`
      && `|    "dep3": "^4.1.0"`
      && `|  }`
      && `|}`.

    cut->set_json( prepare_string( json ) ).

    DATA(package_json) = VALUE /apmg/if_types=>ty_package_json(
      name    = 'test'
      version = '1.0.0'
      dev_dependencies = VALUE #(
                           ( key = `dep1` range = `2.0.0` )
                           ( key = `dep2` range = `>3` )
                           ( key = `dep3` range = `^4.1.0` ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->get( )
      exp = package_json ).

  ENDMETHOD.

  METHOD dependencies_abap_to_json.

    init_test( '$TEST' ).

    DATA(package_json) = VALUE /apmg/if_types=>ty_package_json(
      name    = 'test'
      version = '1.0.0'
      dev_dependencies = VALUE #(
                           ( key = `dep1` range = `2.0.0` )
                           ( key = `dep2` range = `>3` )
                           ( key = `dep3` range = `^4.1.0` ) ) ).

    cut->set( package_json ).

    DATA(json) = ``
      && `{`
      && `|  "name": "test",`
      && `|  "version": "1.0.0",`
      && `|  "devDependencies": {`
      && `|    "dep1": "2.0.0",`
      && `|    "dep2": ">3",`
      && `|    "dep3": "^4.1.0"`
      && `|  }`
      && `|}`.

    cl_abap_unit_assert=>assert_equals(
      act = cut->get_json( )
      exp = prepare_string( json ) ).

  ENDMETHOD.

  METHOD get_complete.

    init_test( '$TEST' ).

    cut->set( test_package_json ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->get_json( abap_true )
      exp = test_json ).

  ENDMETHOD.

  METHOD set_complete.

    init_test( '$TEST' ).

    cut->set_json( test_json ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->get( )
      exp = test_package_json ).

  ENDMETHOD.

  METHOD convert_json_to_manifest.

    DATA(manifest) = /apmg/cl_package_json=>convert_json_to_manifest( test_json_full ).

    cl_abap_unit_assert=>assert_equals(
      act = manifest
      exp = test_manifest ).

  ENDMETHOD.

  METHOD convert_json_to_manifest_abbr.

    DATA(abbreviated) = CORRESPONDING /apmg/if_types=>ty_manifest_abbreviated(
      /apmg/cl_package_json=>convert_json_to_manifest( test_json_abbreviated ) ).

    cl_abap_unit_assert=>assert_equals(
      act = abbreviated
      exp = test_manifest_abbreviated ).

  ENDMETHOD.

  METHOD convert_manifest_to_json.

    DATA(json) = /apmg/cl_package_json=>convert_manifest_to_json( manifest = test_manifest is_complete = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = json
      exp = test_json_full ).

  ENDMETHOD.

  METHOD convert_manifest_abbr_to_json.

    DATA(manifest) = CORRESPONDING /apmg/if_types=>ty_manifest( test_manifest_abbreviated ).

    DATA(json) = /apmg/cl_package_json=>convert_manifest_to_json( manifest = manifest is_complete = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = json
      exp = test_json_abbreviated ).

  ENDMETHOD.

  METHOD convert_manifest_to_pack_json.

    DATA(json) = /apmg/cl_package_json=>convert_manifest_to_json(
      manifest        = test_manifest
      is_package_json = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = json
      exp = test_json ).

  ENDMETHOD.

ENDCLASS.
