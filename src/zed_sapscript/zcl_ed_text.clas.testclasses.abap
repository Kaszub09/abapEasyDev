CLASS ltcl_conversions DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      back_and_forth_from_string FOR TESTING,
      back_and_forth_from_lines FOR TESTING.
ENDCLASS.


CLASS ltcl_conversions IMPLEMENTATION.


  METHOD back_and_forth_from_string.
    DATA(string) = |This is multiline string.{ cl_abap_char_utilities=>newline }This is new line. 1234567890|
        && |This is superlong line that exceeds 132 characters which is the default size of line when calling strign to lines. We will see what happens when we use this line.|
        && |{ cl_abap_char_utilities=>newline }Duplicate above - This is superlong line that exceeds 132 characters which is the default size of line when calling strign to lines. We will see what happens when we use this line.|.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_ed_text=>lines_to_string( zcl_ed_text=>string_to_lines( string ) )
        exp = string ).
  ENDMETHOD.

  METHOD back_and_forth_from_lines.
    DATA(lines) = VALUE zcl_ed_text=>tt_line( ( tdformat = '*' tdline = 'Line 1 up to 20 char' )
        ( tdformat = '' tdline = 'Line 1 continue' )
        ( tdformat = '*' tdline = 'Line 2' )
     ).
    "iv_fw must be correct so the string is splitted exactly as above
    cl_abap_unit_assert=>assert_equals(
        act = zcl_ed_text=>string_to_lines( string = zcl_ed_text=>lines_to_string( lines ) iv_fw = 20 )
        exp = lines ).

  ENDMETHOD.

ENDCLASS.
