CLASS zcl_ed_logger_ext_display DEFINITION PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS zcl_ed_logger_ext_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_logger_ext_display.

    METHODS:
      constructor IMPORTING logger TYPE REF TO zif_ed_logger.

  PRIVATE SECTION.
    DATA:
        logger TYPE REF TO zif_ed_logger.
ENDCLASS.

CLASS zcl_ed_logger_ext_display IMPLEMENTATION.
  METHOD constructor.
    me->logger = logger.
  ENDMETHOD.

  METHOD zif_ed_logger_ext_display~get_display_cont.
    display = zcl_ed_logger_display_factory=>create_container( )->set_settings( logger = logger messages_only = messages_only ).
  ENDMETHOD.

  METHOD zif_ed_logger_ext_display~get_display_modal.
    display = zcl_ed_logger_display_factory=>create_modal( )->set_settings( logger = logger messages_only = messages_only ).
  ENDMETHOD.

  METHOD zif_ed_logger_ext_display~get_display_nonmodal.
    display = zcl_ed_logger_display_factory=>create_nonmodal( )->set_settings( logger = logger messages_only = messages_only ).
  ENDMETHOD.
ENDCLASS.
