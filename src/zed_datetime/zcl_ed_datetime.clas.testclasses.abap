CLASS ltcl_parsing DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_test_data,
        string   TYPE string,
        mask     TYPE string,
        exp_date TYPE d,
        exp_time TYPE t,
      END OF t_test_data,
      tt_test_data TYPE STANDARD TABLE OF t_test_data WITH EMPTY KEY.
    METHODS:
      test FOR TESTING.
ENDCLASS.


CLASS ltcl_parsing IMPLEMENTATION.

  METHOD test.
    DATA(test_data) = VALUE tt_test_data(
        ( string = '  2025-01-01T14:59:59 some text' mask = 'yyyy-MM-ddThh:mm:ss' exp_date = '20250101' exp_time = '145959' )
        ( string = '  2025-01-01T14:59:59 some text' mask = 'y-M-dTh:m:s' exp_date = '20250101' exp_time = '145959' )
        "Dot is escaped
        ( string = '  2025-01-01T.14:59:59. some text' mask = 'y-M-dT\.h:m:s\.' exp_date = '20250101' exp_time = '145959' )
        ( string = '2025-01-01T14:59:59' mask = 'yyyy-MM-dd' exp_date = '20250101' exp_time = '000000' )
        ( string = '2025-01-01T14:59:59' mask = 'Thh:mm:ss' exp_date = '00000000' exp_time = '145959' )
        ( string = 'no date or time here' mask = 'yyyy-MM-ddThh:mm:ss' exp_date = '00000000' exp_time = '000000' )
        ( string = '2025-1-1T14:9:59 some text' mask = 'y-M-dTh:m:s' exp_date = '20250101' exp_time = '140959' )
    ).

    LOOP AT test_data REFERENCE INTO DATA(test).
      DATA(msg) = |line={ sy-tabix }|.
      "Declare date/time here so they don't carry data from previous test runs
      DATA(date) = VALUE d( ).
      DATA(time) = VALUE t( ).
      DATA(regex) = zcl_ed_datetime=>prepare_regex( test->mask ).
      zcl_ed_datetime=>parse_string( EXPORTING string = test->string regex_info = regex IMPORTING date = date time = time ).

      cl_abap_unit_assert=>assert_equals( act = date exp = test->exp_date msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = time exp = test->exp_time msg = msg ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
CLASS ltcl_formatting DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_test_data,
        mask       TYPE string,
        date       TYPE d,
        time       TYPE t,
        exp_format TYPE string,
      END OF t_test_data,
      tt_test_data TYPE STANDARD TABLE OF t_test_data WITH EMPTY KEY.
    METHODS:
      test FOR TESTING.
ENDCLASS.


CLASS ltcl_formatting IMPLEMENTATION.

  METHOD test.
    DATA(test_data) = VALUE tt_test_data(
        ( mask = '   yyyy-MM-ddThh:mm:ss XX' date = '20250101' time = '145959' exp_format = '   2025-01-01T14:59:59 XX')
        ( mask = 'y-M-dTh:m:s' date = '20250101' time = '140959' exp_format = '2025-1-1T14:9:59')
    ).

    LOOP AT test_data REFERENCE INTO DATA(test).
      DATA(msg) = |line={ sy-tabix }|.
      cl_abap_unit_assert=>assert_equals( act = zcl_ed_datetime=>format_date_time( mask = test->mask date = test->date time = test->time )
         exp = test->exp_format msg = msg ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
