CLASS zcl_ed_screens_default_handler DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_screens_handler.
ENDCLASS.

CLASS zcl_ed_screens_default_handler IMPLEMENTATION.
  METHOD zif_ed_screens_handler~pai.
    CASE command.
      WHEN zif_ed_screens_handler=>c_status_commands-back OR zif_ed_screens_handler=>c_status_commands-exit OR zif_ed_screens_handler=>c_status_commands-cancel.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pbo_get_status_commands_to_ex.
    APPEND zif_ed_screens_handler=>c_status_commands-save TO commands_to_exclude.
  ENDMETHOD.
ENDCLASS.
