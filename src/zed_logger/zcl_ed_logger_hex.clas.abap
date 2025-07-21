CLASS zcl_ed_logger_hex DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      messages_to_hex IMPORTING messages TYPE zif_ed_logger=>tt_log_message RETURNING VALUE(binary) TYPE xstring,
      hex_to_messages IMPORTING binary TYPE xstring RETURNING VALUE(messages) TYPE zif_ed_logger=>tt_log_message.
ENDCLASS.

CLASS zcl_ed_logger_hex IMPLEMENTATION.
  METHOD hex_to_messages.
    IMPORT messages = messages FROM DATA BUFFER binary.
  ENDMETHOD.

  METHOD messages_to_hex.
    EXPORT messages = messages TO DATA BUFFER binary COMPRESSION ON.
  ENDMETHOD.
ENDCLASS.
