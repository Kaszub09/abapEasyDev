CLASS tcl_ed_logger_hex DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      setup,
      can_encode_and_decode_messages FOR TESTING.

    DATA:
      cut TYPE REF TO zcl_ed_logger_hex.
ENDCLASS.


CLASS tcl_ed_logger_hex IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD can_encode_and_decode_messages.
    DATA(msgs) = VALUE zif_ed_logger=>tt_log_message( ( msg_type = 'E' sap_msg = VALUE #( msgid = 'ID' ) ) ( msg_type = 'I' ) ).
    DATA(encoded) = cut->messages_to_hex( msgs ).
    DATA(decoded) = cut->hex_to_messages( encoded ).
    cl_abap_unit_assert=>assert_equals( act = decoded exp = msgs ).
  ENDMETHOD.

ENDCLASS.
