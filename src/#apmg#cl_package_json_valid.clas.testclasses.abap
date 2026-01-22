CLASS ltcl_package_json_valid DEFINITION FINAL FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA tap TYPE REF TO /apmg/cl_tap.

    METHODS:
      setup,
      is_scoped FOR TESTING,
      is_valid_scope FOR TESTING,
      is_valid_name FOR TESTING,
      is_valid_email FOR TESTING,
      is_valid_url FOR TESTING.

    " TODO: More tests

ENDCLASS.

CLASS ltcl_package_json_valid IMPLEMENTATION.

  METHOD setup.
    tap = NEW #( ).
  ENDMETHOD.

  METHOD is_valid_name.
    tap->_( /apmg/cl_package_json_valid=>is_valid_name( '' ) )->false( ).
    tap->_( /apmg/cl_package_json_valid=>is_valid_name( 'ab' ) )->false( ).
    tap->_( /apmg/cl_package_json_valid=>is_valid_name( 'valid' ) )->true( ).
    tap->_( /apmg/cl_package_json_valid=>is_valid_name( 'too-short/tes' ) )->false( ).
    tap->_( /apmg/cl_package_json_valid=>is_valid_name( 'test/invalid' ) )->false( ).
    tap->_( /apmg/cl_package_json_valid=>is_valid_name( '@test/valid' ) )->true( ).
    tap->_( /apmg/cl_package_json_valid=>is_valid_name( '@test/@invalid' ) )->false( ).
  ENDMETHOD.

  METHOD is_scoped.
    tap->_( /apmg/cl_package_json_valid=>is_scoped( '' ) )->false( ).
    tap->_( /apmg/cl_package_json_valid=>is_scoped( 'test' ) )->false( ).
    tap->_( /apmg/cl_package_json_valid=>is_scoped( '@test' ) )->false( ).
    tap->_( /apmg/cl_package_json_valid=>is_scoped( 'test/invalid' ) )->false( ).
    tap->_( /apmg/cl_package_json_valid=>is_scoped( '@test/valid' ) )->true( ).
    tap->_( /apmg/cl_package_json_valid=>is_scoped( '@ab/too-short' ) )->true( ). " but too short scope
    tap->_( /apmg/cl_package_json_valid=>is_scoped( '@test/@invalid' ) )->true( ). "but bad package
  ENDMETHOD.

  METHOD is_valid_scope.
    tap->_( /apmg/cl_package_json_valid=>is_valid_scope( '' ) )->false( ).
    tap->_( /apmg/cl_package_json_valid=>is_valid_scope( 'test' ) )->false( ).
    tap->_( /apmg/cl_package_json_valid=>is_valid_scope( '@test' ) )->true( ).
    tap->_( /apmg/cl_package_json_valid=>is_valid_scope( '@te' ) )->false( ).
    tap->_( /apmg/cl_package_json_valid=>is_valid_scope( '@0123-test' ) )->true( ).
    tap->_( /apmg/cl_package_json_valid=>is_valid_scope( '@test_more' ) )->false( ).
  ENDMETHOD.

  METHOD is_valid_email.
    tap->_( /apmg/cl_package_json_valid=>is_valid_email( '' ) )->true( ).
    tap->_( /apmg/cl_package_json_valid=>is_valid_email( 'test@example.com' ) )->true( ).
    tap->_( /apmg/cl_package_json_valid=>is_valid_email( 'test@invalid' ) )->false( ).
  ENDMETHOD.

  METHOD is_valid_url.
    tap->_( /apmg/cl_package_json_valid=>is_valid_url( '' ) )->true( ).
    tap->_( /apmg/cl_package_json_valid=>is_valid_url( 'https://example.com' ) )->true( ).
    tap->_( /apmg/cl_package_json_valid=>is_valid_url( '\\git://invalid.com' ) )->false( ).
  ENDMETHOD.

ENDCLASS.
