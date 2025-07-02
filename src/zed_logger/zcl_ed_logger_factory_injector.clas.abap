CLASS zcl_ed_logger_factory_injector DEFINITION PUBLIC CREATE PRIVATE FOR TESTING.

  PUBLIC SECTION.
    CLASS-METHODS:
      inject_logger IMPORTING logger TYPE REF TO zif_ed_logger OPTIONAL,
      inject_logger_display IMPORTING display TYPE REF TO zif_ed_logger_display OPTIONAL.
ENDCLASS.

CLASS zcl_ed_logger_factory_injector IMPLEMENTATION.
  METHOD inject_logger.
    zcl_ed_logger_factory=>logger_mock = logger.
  ENDMETHOD.

  METHOD inject_logger_display.
    zcl_ed_logger_factory=>logger_display_mock = display.
  ENDMETHOD.
ENDCLASS.
