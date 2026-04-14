CLASS zcl_ed_logger_factory_injector DEFINITION PUBLIC CREATE PRIVATE FOR TESTING.

  PUBLIC SECTION.
    CLASS-METHODS:
      inject_logger IMPORTING logger TYPE REF TO zif_ed_logger OPTIONAL.
ENDCLASS.

CLASS zcl_ed_logger_factory_injector IMPLEMENTATION.
  METHOD inject_logger.
    zcl_ed_logger_factory=>logger_mock = logger.
  ENDMETHOD.
ENDCLASS.
