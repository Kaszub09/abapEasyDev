CLASS zcl_ed_logger_display_factory DEFINITION PUBLIC CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      create_container RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_cont,
      create_modal RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_modal,
      create_nonmodal RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_nonmodal.
ENDCLASS.

CLASS zcl_ed_logger_display_factory IMPLEMENTATION.
  METHOD create_container.
    display = NEW #( ).
  ENDMETHOD.

  METHOD create_modal.
    display = NEW #( ).
  ENDMETHOD.

  METHOD create_nonmodal.
    display = NEW #( ).
  ENDMETHOD.
ENDCLASS.
