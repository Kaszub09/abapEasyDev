"! <p class="shorttext synchronized">Encoding. Also check <em>cl_abap_codepage</em></p>
"! <br/>TAGS: encoding; decoding
CLASS ltcl_ed_coding DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      no_conversion_between_same_cp FOR TESTING,
      convert_system_to_utf8 FOR TESTING,
      convert_utf8_to_system FOR TESTING,
      convert_utf8_to_utf16le FOR TESTING.
    DATA:
      test_string          TYPE string VALUE 'AĄŻ1+-',
      "! via https://dencode.com/string/hex
      test_xstring_utf16le TYPE xstring VALUE '410004017B0131002B002D00',
      "! via https://dencode.com/string/hex
      test_xstring_utf8    TYPE xstring VALUE '41C484C5BB312B2D'.
ENDCLASS.


CLASS ltcl_ed_coding IMPLEMENTATION.
  METHOD convert_system_to_utf8.
    DATA(result) = zcl_ed_encoding=>convert( input = test_string in_codepage = zcl_ed_encoding=>as_codepage out_codepage = zcl_ed_encoding=>c_utf8 ).
    DATA(xresult) = cl_bcs_convert=>string_to_xstring( iv_string = result iv_convert_cp = ' ' ).

    cl_abap_unit_assert=>assert_equals( act = xresult exp = test_xstring_utf8 ).
  ENDMETHOD.

  METHOD convert_utf8_to_system.
    "It converts straight to string encoded in application server encoding, so no point in manual conversion
    DATA(result) = cl_bcs_convert=>xstring_to_string( iv_xstr = test_xstring_utf8 iv_cp = zcl_ed_encoding=>c_utf8 ).
    cl_abap_unit_assert=>assert_equals( act = result exp = test_string ).
  ENDMETHOD.

  METHOD no_conversion_between_same_cp.
    DATA(result) = zcl_ed_encoding=>convert( input = test_string in_codepage = zcl_ed_encoding=>c_utf8 out_codepage = zcl_ed_encoding=>c_utf8 ).

    cl_abap_unit_assert=>assert_equals( act = result exp = test_string ).
  ENDMETHOD.

  METHOD convert_utf8_to_utf16le.
    DATA(test_xstring_utf8) = cl_bcs_convert=>string_to_xstring( iv_string = test_string iv_convert_cp = abap_true
                                                                 iv_codepage = CONV #( zcl_ed_encoding=>c_utf8 ) ).
    DATA(text_string_utf8) = cl_bcs_convert=>xstring_to_string( iv_xstr = test_xstring_utf8 iv_cp = zcl_ed_encoding=>c_utf16le ).
    DATA(text_string_utf16le) = zcl_ed_encoding=>convert( input = text_string_utf8 in_codepage = zcl_ed_encoding=>c_utf8
                                                        out_codepage = zcl_ed_encoding=>c_utf16le ).
    DATA(text_xstring_utf16le) = cl_bcs_convert=>string_to_xstring( iv_string = text_string_utf16le iv_convert_cp = abap_true
                                                                 iv_codepage = CONV #( zcl_ed_encoding=>c_utf16le ) ).
    cl_abap_unit_assert=>assert_equals( act = text_xstring_utf16le exp = test_xstring_utf16le ).
  ENDMETHOD.

ENDCLASS.
