CLASS tcl_string DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      get_left FOR TESTING,
      get_left_too_long FOR TESTING,
      get_right FOR TESTING,
      get_right_too_long FOR TESTING,
      get_sub FOR TESTING,
      get_sub_too_long FOR TESTING,
      get_sub_too_far FOR TESTING.

    DATA:
        test TYPE string VALUE 'This is test.'.
ENDCLASS.

CLASS tcl_string IMPLEMENTATION.
  METHOD get_left.
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_string=>left( val = test len = 4 )
        exp  = 'This' ).
  ENDMETHOD.

  METHOD get_left_too_long.
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_string=>left( val = test len = 1024 )
        exp  = test ).
  ENDMETHOD.

  METHOD get_right.
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_string=>right( val = test len = 5 )
        exp  = 'test.' ).
  ENDMETHOD.

  METHOD get_right_too_long.
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_string=>right( val = test len = 1024 )
        exp  = test ).
  ENDMETHOD.

  METHOD get_sub.
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_string=>sub( val = test off = 2 len = 2 )
        exp  = 'is' ).
  ENDMETHOD.

  METHOD get_sub_too_far.
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_string=>sub( val = test off = 1024 len = 2 )
        exp  = space ).
  ENDMETHOD.

  METHOD get_sub_too_long.
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_string=>sub( val = test off = 2 len = 1024 )
        exp  = 'is is test.' ).
  ENDMETHOD.
ENDCLASS.
