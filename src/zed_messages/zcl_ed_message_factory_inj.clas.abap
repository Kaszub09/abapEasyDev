CLASS zcl_ed_message_factory_inj  DEFINITION PUBLIC FINAL CREATE PRIVATE FOR TESTING.

  PUBLIC SECTION.
    CLASS-METHODS:
      inject IMPORTING message TYPE REF TO zif_ed_message,
      clear.
ENDCLASS.

CLASS zcl_ed_message_factory_inj IMPLEMENTATION.
  METHOD clear.
    CLEAR: zcl_ed_message_factory=>message_obj.
  ENDMETHOD.

  METHOD inject.
    zcl_ed_message_factory=>message_obj = message.
  ENDMETHOD.
ENDCLASS.
