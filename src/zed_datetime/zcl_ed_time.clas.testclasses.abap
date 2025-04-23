

CLASS ltcl_time_simple_calculactions DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_test_data,
        t                     TYPE t,
        seconds               TYPE i,
        total_seconds         TYPE i,
        seconds_till_midnight TYPE i,
        minutes               TYPE i,
        total_minutes         TYPE i,
        hours                 TYPE i,
      END OF t_test_data,
      tt_test_data TYPE STANDARD TABLE OF t_test_data WITH EMPTY KEY.

    METHODS:
      test_calculations FOR TESTING,
      test_validity FOR TESTING,
      test_time_creation FOR TESTING.
ENDCLASS.

CLASS ltcl_time_simple_calculactions IMPLEMENTATION.
  METHOD test_calculations.
    DATA(test_data) = VALUE tt_test_data(
      ( t = '140101' seconds = 1 total_seconds = 50461  seconds_till_midnight = 35939 minutes = 1 total_minutes = 841 hours = 14 )
      ( t = '240000' seconds = 0 total_seconds = 86400  seconds_till_midnight = 0 minutes = 0 total_minutes = 1440 hours = 24 )
      ( t = '000000' seconds = 0 total_seconds = 0 seconds_till_midnight = 86400 minutes = 0 total_minutes = 0 hours = 0 )
      ( t = '000001' seconds = 1 total_seconds = 1 seconds_till_midnight = 86399 minutes = 0 total_minutes = 0 hours = 0 ) ).
    "Validate
    LOOP AT test_data REFERENCE INTO DATA(test).
      DATA(msg) = |line={ sy-tabix }, t={ test->t }|.
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_time=>seconds( test->t ) exp = test->seconds msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_time=>total_seconds( test->t ) exp = test->total_seconds msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_time=>seconds_till_midnight( test->t ) exp = test->seconds_till_midnight msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_time=>minutes( test->t ) exp = test->minutes msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_time=>total_minutes( test->t ) exp = test->total_minutes msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_time=>hours( test->t ) exp = test->hours msg = msg ).
    ENDLOOP.
  ENDMETHOD.

  METHOD test_validity.
    cl_abap_unit_assert=>assert_true( zcl_ed_time=>is_valid( '240000' ) ).
    cl_abap_unit_assert=>assert_true( zcl_ed_time=>is_valid( '000000' ) ).
    cl_abap_unit_assert=>assert_true( zcl_ed_time=>is_valid( '140101' ) ).

    cl_abap_unit_assert=>assert_false( zcl_ed_time=>is_valid( '250000' ) ).
    cl_abap_unit_assert=>assert_false( zcl_ed_time=>is_valid( '      ' ) ).
    cl_abap_unit_assert=>assert_false( zcl_ed_time=>is_valid( '-10000' ) ).
  ENDMETHOD.

  METHOD test_time_creation.
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_time=>create( hours = 2 minutes = 3 seconds = 4 ) exp = '020304' ).
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_time=>create( hours = 2 minutes = 3 seconds = 61 ) exp = '020401' ).
  ENDMETHOD.
ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
CLASS ltcl_differences DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_test_data,
        t        TYPE t,
        interval TYPE c LENGTH 1,
        value    TYPE i,
        expected TYPE t,
      END OF t_test_data,
      tt_test_data TYPE STANDARD TABLE OF t_test_data WITH EMPTY KEY.

    METHODS:
      test FOR TESTING.
ENDCLASS.

CLASS ltcl_differences IMPLEMENTATION.
  METHOD test.
    DATA(test_data) = VALUE tt_test_data(
     "Simple addition
     ( t = '220000' interval = 'S' value = 1 expected = '220001' )
     ( t = '220000' interval = 'S' value = 0 expected = '220000' )
     ( t = '220000' interval = 'S' value = 24 * 60 * 60 - 1 expected = '215959' )
     ( t = '220000' interval = 'M' value = 1 expected = '220100' )
     ( t = '220000' interval = 'M' value = 0 expected = '220000' )
     ( t = '220000' interval = 'M' value = 24 * 60 - 1 expected = '215900' )
     ( t = '220000' interval = 'H' value = 1 expected = '230000' )
     ( t = '220000' interval = 'H' value = 0 expected = '220000' )
     ( t = '220000' interval = 'H' value = 24 - 1 expected = '210000' )
     "Month - skip non-existing days
     ( t = '122022' interval = 'H' value = 18 expected = '062022' ) ).

    "Validate
    LOOP AT test_data REFERENCE INTO DATA(test).
      DATA(msg) = |line={ sy-tabix }, t={ test->t }|.
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_time=>add_to_time( time = test->t interval = test->interval value = test->value )
          exp = test->expected msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_time=>difference( time_from = test->t interval = test->interval time_to = test->expected )
        exp = test->value msg = msg ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
