CLASS zcl_ed_file_explorer_factory_i DEFINITION PUBLIC FINAL CREATE PRIVATE FOR TESTING.

  PUBLIC SECTION.
    CLASS-METHODS:
      inject_gui IMPORTING fe TYPE REF TO zif_ed_file_explorer,
      inject_as IMPORTING fe TYPE REF TO zif_ed_file_explorer,
      inject_fd IMPORTING fd TYPE REF TO zif_ed_file_dialogue,
      clear_gui,
      clear_as,
      clear_fd,
      clear_all.
ENDCLASS.

CLASS zcl_ed_file_explorer_factory_i IMPLEMENTATION.
  METHOD clear_all.
    CLEAR:
        zcl_ed_file_explorer_factory=>as_fe, zcl_ed_file_explorer_factory=>gui_fe.
  ENDMETHOD.

  METHOD clear_as.
    CLEAR: zcl_ed_file_explorer_factory=>as_fe.
  ENDMETHOD.

  METHOD clear_gui.
    CLEAR: zcl_ed_file_explorer_factory=>gui_fe.
  ENDMETHOD.

  METHOD clear_fd.
    CLEAR: zcl_ed_file_explorer_factory=>fd.
  ENDMETHOD.

  METHOD inject_as.
    zcl_ed_file_explorer_factory=>as_fe = fe.
  ENDMETHOD.

  METHOD inject_gui.
    zcl_ed_file_explorer_factory=>gui_fe = fe.
  ENDMETHOD.

  METHOD inject_fd.
    zcl_ed_file_explorer_factory=>fd = fd.
  ENDMETHOD.
ENDCLASS.
