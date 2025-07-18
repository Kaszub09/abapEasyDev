"! <p class="shorttext synchronized">Base64 encoding and decoding</p>
"! <br/>TAGS: base64; encoding; decoding
CLASS zcl_ed_base64 DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Try different encodings with <em>zcl_ed_coding</em> in case of garbage data</p>
      "! @parameter out_codepage | <p class="shorttext synchronized" lang="en">Convert to codepage before encoding</p>
      encode IMPORTING text TYPE string out_codepage TYPE cpcodepage OPTIONAL RETURNING VALUE(encoded) TYPE string RAISING cx_bcs,
      "! <p class="shorttext synchronized" lang="en">Try different encodings with <em>zcl_ed_coding</em> in case of garbage data</p>
      "! @parameter in_codepage | <p class="shorttext synchronized" lang="en">Original codepage before decoding</p>
      decode IMPORTING encoded TYPE string in_codepage TYPE cpcodepage OPTIONAL RETURNING VALUE(decoded) TYPE string.
ENDCLASS.

CLASS zcl_ed_base64 IMPLEMENTATION.
  METHOD decode.
    DATA(xstring) = cl_http_utility=>decode_x_base64( encoded ).
    DATA(codepage) = COND #( WHEN in_codepage IS NOT INITIAL THEN in_codepage ELSE zcl_ed_encoding=>as_codepage ).
    decoded = cl_bcs_convert=>xstring_to_string( iv_xstr = xstring iv_cp = codepage ).
  ENDMETHOD.

  METHOD encode.
    DATA(xstring) = cl_bcs_convert=>string_to_xstring( iv_string = text
        iv_convert_cp = xsdbool( out_codepage IS NOT INITIAL ) iv_codepage = CONV #( out_codepage ) ).
    encoded = cl_http_utility=>encode_x_base64( xstring ).
  ENDMETHOD.
ENDCLASS.
