CLASS zcl_ed_change_doc_factory_inj DEFINITION PUBLIC FINAL CREATE PRIVATE FOR TESTING.
  PUBLIC SECTION.
    CLASS-METHODS:
      inject_cd IMPORTING cd TYPE REF TO zif_ed_change_doc,
      clear_cd.
ENDCLASS.

CLASS zcl_ed_change_doc_factory_inj IMPLEMENTATION.
  METHOD clear_cd.
    CLEAR zcl_ed_change_doc_factory=>cd_obj.
  ENDMETHOD.

  METHOD inject_cd.
    zcl_ed_change_doc_factory=>cd_obj = cd.
  ENDMETHOD.
ENDCLASS.
