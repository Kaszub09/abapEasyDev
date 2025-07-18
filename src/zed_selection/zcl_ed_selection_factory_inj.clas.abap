"! <p class="shorttext synchronized" lang="en">Selection factory injector</p>
CLASS zcl_ed_selection_factory_inj DEFINITION PUBLIC FINAL CREATE PRIVATE FOR TESTING.

  PUBLIC SECTION.
    CLASS-METHODS:
      inject_sel IMPORTING mock TYPE REF TO zif_ed_selection OPTIONAL.

    TYPES:
      tt_table TYPE STANDARD TABLE OF tabname WITH EMPTY KEY.

    CLASS-METHODS:
      create_from_tables IMPORTING tables TYPE tt_table RETURNING VALUE(sel_obj) TYPE REF TO zif_ed_selection,
      create_from_selection IMPORTING selection TYPE zif_ed_selection=>tt_table_selection RETURNING VALUE(sel_obj) TYPE REF TO zif_ed_selection.
ENDCLASS.

CLASS zcl_ed_selection_factory_inj IMPLEMENTATION.
  METHOD inject_sel.
    zcl_ed_selection_factory=>selection_mock = mock.
  ENDMETHOD.

  METHOD create_from_tables.
    DATA(obj) = NEW zcl_ed_selection( ).
    sel_obj = obj.
    LOOP AT tables REFERENCE INTO DATA(table).
      SELECT fieldname, position FROM dd03l
      WHERE tabname = @table->* AND ( NOT fieldname LIKE '.%' ) AND as4local = 'A'
      ORDER BY position DESCENDING
      INTO TABLE @DATA(fields).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND VALUE #( table = table->* ) TO obj->zif_ed_selection~selection REFERENCE INTO DATA(selection_table).
      LOOP AT fields REFERENCE INTO DATA(field).
        APPEND VALUE #( field = field->fieldname ) TO selection_table->fields.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_from_selection.
    DATA(obj) = NEW zcl_ed_selection( ).
    sel_obj = obj.
    obj->zif_ed_selection~selection = selection.
  ENDMETHOD.
ENDCLASS.
