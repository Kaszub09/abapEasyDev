CLASS zcl_ed_message_factory_inj DEFINITION PUBLIC FINAL CREATE PRIVATE FOR TESTING.

  PUBLIC SECTION.
    CLASS-METHODS:
      inject IMPORTING message TYPE REF TO zif_ed_message OPTIONAL.
ENDCLASS.

CLASS zcl_ed_message_factory_inj IMPLEMENTATION.
  METHOD inject.
    zcl_ed_message_factory=>message_mock = message.
  ENDMETHOD.
ENDCLASS.
