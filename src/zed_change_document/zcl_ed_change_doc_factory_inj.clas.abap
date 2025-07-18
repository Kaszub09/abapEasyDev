CLASS zcl_ed_change_doc_factory_inj DEFINITION PUBLIC CREATE PRIVATE FOR TESTING.
  PUBLIC SECTION.
    CLASS-METHODS:
      inject_cd IMPORTING cd TYPE REF TO zif_ed_change_doc optional.
ENDCLASS.

CLASS zcl_ed_change_doc_factory_inj IMPLEMENTATION.
  METHOD inject_cd.
    zcl_ed_change_doc_factory=>cd_mock = cd.
  ENDMETHOD.
ENDCLASS.
