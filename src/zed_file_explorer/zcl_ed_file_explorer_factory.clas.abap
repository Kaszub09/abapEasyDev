CLASS zcl_ed_file_explorer_factory DEFINITION PUBLIC FINAL CREATE PRIVATE
GLOBAL FRIENDS zcl_ed_file_explorer_factory_i.

  PUBLIC SECTION.
    CLASS-METHODS:
      create_gui RETURNING VALUE(fe) TYPE REF TO zif_ed_file_explorer,
      create_as RETURNING VALUE(fe) TYPE REF TO zif_ed_file_explorer,
      create_file_dialogue RETURNING VALUE(file_dialogue) TYPE REF TO zif_ed_file_dialogue.

  PRIVATE SECTION.
    CLASS-DATA:
      gui_fe TYPE REF TO zif_ed_file_explorer,
      as_fe  TYPE REF TO zif_ed_file_explorer,
      fd     TYPE REF TO zif_ed_file_dialogue.
ENDCLASS.

CLASS zcl_ed_file_explorer_factory IMPLEMENTATION.
  METHOD create_as.
    IF NOT as_fe IS BOUND.
      as_fe = NEW zcl_ed_file_explorer_as( ).
    ENDIF.
    fe = as_fe.
  ENDMETHOD.

  METHOD create_gui.
    IF NOT gui_fe IS BOUND.
      gui_fe = NEW zcl_ed_file_explorer_gui( ).
    ENDIF.
    fe = gui_fe.
  ENDMETHOD.

  METHOD create_file_dialogue.
    IF NOT fd IS BOUND.
      fd = NEW zcl_ed_file_explorer_dialogue( ).
    ENDIF.
    file_dialogue = fd.
  ENDMETHOD.

ENDCLASS.
