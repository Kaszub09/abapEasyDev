CLASS zcl_ed_file_explorer_factory_i DEFINITION PUBLIC FINAL CREATE PRIVATE FOR TESTING.

  PUBLIC SECTION.
    CLASS-METHODS:
      inject_gui IMPORTING file_explorer TYPE REF TO zif_ed_file_explorer,
      inject_as IMPORTING file_explorer TYPE REF TO zif_ed_file_explorer,
      inject_fd IMPORTING file_dialogue TYPE REF TO zif_ed_file_dialogue,
      clear_gui,
      clear_as,
      clear_fd,
      clear_all.
ENDCLASS.

CLASS zcl_ed_file_explorer_factory_i IMPLEMENTATION.
  METHOD clear_all.
    CLEAR:
        zcl_ed_file_explorer_factory=>file_explorer_as_obj,
        zcl_ed_file_explorer_factory=>file_explorer_gui_obj,
        zcl_ed_file_explorer_factory=>file_dialogue_obj.
  ENDMETHOD.

  METHOD clear_as.
    CLEAR: zcl_ed_file_explorer_factory=>file_explorer_as_obj.
  ENDMETHOD.

  METHOD clear_gui.
    CLEAR: zcl_ed_file_explorer_factory=>file_explorer_gui_obj.
  ENDMETHOD.

  METHOD clear_fd.
    CLEAR: zcl_ed_file_explorer_factory=>file_dialogue_obj.
  ENDMETHOD.

  METHOD inject_as.
    zcl_ed_file_explorer_factory=>file_explorer_as_obj = file_explorer.
  ENDMETHOD.

  METHOD inject_gui.
    zcl_ed_file_explorer_factory=>file_explorer_gui_obj = file_explorer.
  ENDMETHOD.

  METHOD inject_fd.
    zcl_ed_file_explorer_factory=>file_dialogue_obj = file_dialogue.
  ENDMETHOD.
ENDCLASS.
