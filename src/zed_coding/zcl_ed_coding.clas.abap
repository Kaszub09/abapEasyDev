"! <p class="shorttext synchronized">Coding. Also check <em>cl_abap_codepage</em></p>
"! <br/>TAGS: coding; encoding; decoding
CLASS zcl_ed_coding DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      "! UTF-16LE Unicode / ISO/IEC 10646
      c_utf16le       TYPE cpcodepage VALUE '4103',
      "! Unicode UTF-8
      c_utf8          TYPE cpcodepage VALUE '4110',
      "! Microsoft 1252, Superset of ISO 8859-1
      c_microsoft1252 TYPE cpcodepage VALUE '1160',
      "! Microsoft Windows 1250 for Central Europe
      c_microsoft1250 TYPE abap_encoding  VALUE '1404'.

    CLASS-METHODS:
      class_constructor,
      "! <p class="shorttext synchronized">Table TCP00 has all codepages.</p>
      convert IMPORTING input TYPE string in_codepage TYPE cpcodepage out_codepage TYPE cpcodepage
              RETURNING VALUE(output) TYPE string.

    CLASS-DATA:
      as_codepage  TYPE cpcodepage READ-ONLY,
      gui_codepage TYPE cpcodepage READ-ONLY.
ENDCLASS.

CLASS zcl_ed_coding IMPLEMENTATION.
  METHOD class_constructor.
    CALL FUNCTION 'SCP_GET_CODEPAGE_NUMBER'
      EXPORTING
        database_also  = space
      IMPORTING
        appl_codepage  = as_codepage
        gui_codepage   = gui_codepage
      EXCEPTIONS
        internal_error = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |SCP_GET_CODEPAGE_NUMBER rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD convert.
    DATA(codepage_converter) = NEW cl_abap_conv_obj( incode = in_codepage outcode = out_codepage ).
    codepage_converter->convert( EXPORTING inbuff = input outbufflg = 0 IMPORTING outbuff = output ).
  ENDMETHOD.
ENDCLASS.
