

CLASS ltcl_date_simple_calculactions DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_test_data,
        d                  TYPE d,
        day                TYPE i,
        week               TYPE kweek,
        weekday            TYPE i,
        month              TYPE i,
        first_day_in_month TYPE d,
        last_day_in_month  TYPE d,
        days_in_month      TYPE i,
        year               TYPE i,
        day_in_year       TYPE i,
        days_in_year       TYPE i,
      END OF t_test_data,
      tt_test_data TYPE STANDARD TABLE OF t_test_data WITH EMPTY KEY.

    METHODS:
      test_calculations FOR TESTING,
      test_validity FOR TESTING,
      test_date_creation FOR TESTING.
ENDCLASS.

CLASS ltcl_date_simple_calculactions IMPLEMENTATION.
  METHOD test_calculations.
    DATA(test_data) = VALUE tt_test_data(
      ( d = '20250101' day = '01' week = '202501' weekday = 3 month = 1 first_day_in_month = '20250101'
          last_day_in_month = '20250131' days_in_month = 31 year = 2025 day_in_year = 1 days_in_year = 365 )
      ( d = '20250108' day = '08' week = '202502' weekday = 3 month = 1 first_day_in_month = '20250101'
          last_day_in_month = '20250131' days_in_month = 31 year = 2025 day_in_year = 8 days_in_year = 365 )
      ( d = '20250131' day = '31' week = '202505' weekday = 5 month = 1 first_day_in_month = '20250101'
          last_day_in_month = '20250131' days_in_month = 31 year = 2025 day_in_year = 31 days_in_year = 365 )
      ( d = '20210101' day = '1' week = '202053' weekday = 5 month = 1 first_day_in_month = '20210101'
          last_day_in_month = '20210131' days_in_month = 31 year = 2021 day_in_year = 1 days_in_year = 365 )
      "February
      ( d = '20240222' day = '22' week = '202408' weekday = 4 month = 2 first_day_in_month = '20240201'
          last_day_in_month = '20240229' days_in_month = 29 year = 2024 day_in_year = 53 days_in_year = 366 )
      ( d = '20250222' day = '22' week = '202508' weekday = 6 month = 2 first_day_in_month = '20250201'
          last_day_in_month = '20250228' days_in_month = 28 year = 2025 day_in_year = 53 days_in_year = 365 ) ).

    "Validate
    LOOP AT test_data REFERENCE INTO DATA(test).
      DATA(msg) = |line={ sy-tabix }, d={ test->d }|.
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>day( test->d ) exp = test->day msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>week( test->d ) exp = test->week msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>weekday( test->d ) exp = test->weekday msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>month( test->d ) exp = test->month msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>first_day_in_month( test->d ) exp = test->first_day_in_month msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>last_day_in_month( test->d ) exp = test->last_day_in_month msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>days_in_month( test->d ) exp = test->days_in_month msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>year( test->d ) exp = test->year msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>day_in_year( test->d ) exp = test->day_in_year msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>days_in_year( test->d ) exp = test->days_in_year msg = msg ).
    ENDLOOP.
  ENDMETHOD.
  METHOD test_validity.
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>is_valid( '20250101' ) exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>is_valid( '00010101' ) exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>is_valid( '00250101' ) exp = abap_true ).
    "Year 0 shouldn't exists in Gregorian calendar. Although fm is named date_check_PLAUSIBILITY
    "cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>is_valid( '00000101' ) exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>is_valid( '20250229' ) exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>is_valid( '20251301' ) exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>is_valid( '00000000' ) exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>is_valid( '99999999' ) exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>is_valid( '        ' ) exp = abap_false ).
  ENDMETHOD.

  METHOD test_date_creation.
cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>create( year = 25 month = 1 day = 1 ) exp = '00250101' ).
  ENDMETHOD.

ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
CLASS ltcl_differences DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_test_data,
        d        TYPE d,
        interval TYPE c LENGTH 1,
        value    TYPE i,
        expected TYPE d,
      END OF t_test_data,
      tt_test_data TYPE STANDARD TABLE OF t_test_data WITH EMPTY KEY.

    METHODS:
      test FOR TESTING.
ENDCLASS.

CLASS ltcl_differences IMPLEMENTATION.
  METHOD test.
    DATA(test_data) = VALUE tt_test_data(
     "Simple addition
     ( d = '20250101' interval = 'D' value = 1 expected = '20250102' )
     ( d = '20250101' interval = 'D' value = 0 expected = '20250101' )
     ( d = '20250101' interval = 'D' value = -1 expected = '20241231' )
     ( d = '20250101' interval = 'W' value = 1 expected = '20250108' )
     ( d = '20250101' interval = 'W' value = 0 expected = '20250101' )
     ( d = '20250101' interval = 'W' value = -1 expected = '20241225' )
     ( d = '20250101' interval = 'M' value = 1 expected = '20250201' )
     ( d = '20250101' interval = 'M' value = 0 expected = '20250101' )
     ( d = '20250101' interval = 'M' value = -1 expected = '20241201' )
     ( d = '20250101' interval = 'Y' value = 1 expected = '20260101' )
     ( d = '20250101' interval = 'Y' value = 0 expected = '20250101' )
     ( d = '20250101' interval = 'Y' value = -1 expected = '20240101' )
     "Month - skip non-existing days
     ( d = '20250131' interval = 'M' value = 1 expected = '20250228' )
     ( d = '20250228' interval = 'M' value = 1 expected = '20250328' )
     ( d = '20250731' interval = 'M' value = -1 expected = '20250630' )
     ( d = '20240229' interval = 'M' value = 12 expected = '20250228' )
     "Year - skip non-existing days
     ( d = '20240229' interval = 'Y' value = 1 expected = '20250228' ) ).

    "Validate
    LOOP AT test_data REFERENCE INTO DATA(test).
      DATA(msg) = |line={ sy-tabix }, d={ test->d }|.
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>add_to_date( date = test->d interval = test->interval value = test->value )
          exp = test->expected msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_date=>difference( date_from = test->d interval = test->interval date_to = test->expected )
        exp = test->value msg = msg ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
