"! <p class="shorttext synchronized">File explorer factory</p>
"! <br/>TAGS: file; directory; file explorer; factory
CLASS zcl_ed_file_explorer_factory DEFINITION PUBLIC FINAL CREATE PRIVATE
GLOBAL FRIENDS zcl_ed_file_explorer_factory_i.

  PUBLIC SECTION.
    CLASS-METHODS:
      create_gui RETURNING VALUE(file_explorer) TYPE REF TO zif_ed_file_explorer,
      create_as RETURNING VALUE(file_explorer) TYPE REF TO zif_ed_file_explorer,
      create_file_dialogue RETURNING VALUE(file_dialogue) TYPE REF TO zif_ed_file_dialogue.

  PRIVATE SECTION.
    CLASS-DATA:
      file_explorer_gui_obj TYPE REF TO zif_ed_file_explorer,
      file_explorer_as_obj  TYPE REF TO zif_ed_file_explorer,
      file_dialogue_obj     TYPE REF TO zif_ed_file_dialogue.
ENDCLASS.

CLASS zcl_ed_file_explorer_factory IMPLEMENTATION.
  METHOD create_as.
    IF NOT file_explorer_as_obj IS BOUND.
      file_explorer_as_obj = NEW zcl_ed_file_explorer_as( ).
    ENDIF.
    file_explorer = file_explorer_as_obj.
  ENDMETHOD.

  METHOD create_gui.
    IF NOT file_explorer_gui_obj IS BOUND.
      file_explorer_gui_obj = NEW zcl_ed_file_explorer_gui( ).
    ENDIF.
    file_explorer = file_explorer_gui_obj.
  ENDMETHOD.

  METHOD create_file_dialogue.
    IF NOT file_dialogue_obj IS BOUND.
      file_dialogue_obj = NEW zcl_ed_file_explorer_dialogue( ).
    ENDIF.
    file_dialogue = file_dialogue_obj.
  ENDMETHOD.

ENDCLASS.
