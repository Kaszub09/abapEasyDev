
CLASS ltcl_ed_base64 DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      encode_utf8 FOR TESTING,
      encode_utf16le FOR TESTING,
      dencode_utf8 FOR TESTING,
      decode_utf16le FOR TESTING.

    DATA:
      decoded         TYPE string VALUE 'AĄŻ1+-',
      "! via https://www.base64encode.org/
      encoded_utf8    TYPE string VALUE 'QcSExbsxKy0=',
      "! via https://www.base64encode.org/
      encoded_utf16le TYPE string VALUE 'QQAEAXsBMQArAC0A'.
ENDCLASS.

CLASS ltcl_ed_base64 IMPLEMENTATION.
  METHOD decode_utf16le.
    DATA(result) = zcl_ed_base64=>decode( encoded = encoded_utf16le in_codepage = zcl_ed_coding=>c_utf16le ).
    cl_abap_unit_assert=>assert_equals( act = result exp = decoded ).
  ENDMETHOD.

  METHOD dencode_utf8.
    DATA(result) = zcl_ed_base64=>decode( encoded = encoded_utf8 in_codepage = zcl_ed_coding=>c_utf8 ).
    cl_abap_unit_assert=>assert_equals( act = result exp = decoded ).
  ENDMETHOD.

  METHOD encode_utf16le.
    DATA(result) = zcl_ed_base64=>encode( text = decoded out_codepage = zcl_ed_coding=>c_utf16le ).
    cl_abap_unit_assert=>assert_equals( act = result exp = encoded_utf16le ).
  ENDMETHOD.

  METHOD encode_utf8.
    DATA(result) = zcl_ed_base64=>encode( text = decoded out_codepage = zcl_ed_coding=>c_utf8 ).
    cl_abap_unit_assert=>assert_equals( act = result exp = encoded_utf8 ).
  ENDMETHOD.
ENDCLASS.
