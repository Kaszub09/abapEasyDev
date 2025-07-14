CLASS lcl_select_options DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
        c_max_id TYPE i VALUE 150.

    METHODS:
      read_from_fields IMPORTING fields TYPE tt_field_selection,
      import_to_fields CHANGING fields TYPE tt_field_selection,
      refresh_visibility,
      cleanup.

    DATA:
      last_used_id TYPE i READ-ONLY VALUE 0.

  PRIVATE SECTION.
    METHODS:
      read_sel_from_field IMPORTING id TYPE i field TYPE REF TO t_field_selection,
      import_sel_to_field IMPORTING id TYPE i field TYPE REF TO t_field_selection,
      clear_sel IMPORTING id TYPE i.
ENDCLASS.

CLASS lcl_select_options IMPLEMENTATION.
  METHOD read_from_fields.
    DATA(id) = 1.
    last_used_id = 0.

    LOOP AT fields REFERENCE INTO DATA(field) WHERE is_selected = abap_true. "2nd key won't preserve order
      IF id > c_max_id.
        EXIT.
      ENDIF.

      read_sel_from_field( id = id field = field ).
      last_used_id = id.
      id = id + 1.
    ENDLOOP.

    WHILE id <= c_max_id.
      clear_sel( id ).
      id = id + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD import_to_fields.
    DATA(id) = 1.

    LOOP AT fields REFERENCE INTO DATA(field) WHERE is_selected = abap_true. "2nd key won't preserve order
      import_sel_to_field( id = id field = field ).
      id = id + 1.
      IF id > last_used_id.
        "^There should be no more selected fields, so just skip rest
        EXIT.
      ENDIF.
    ENDLOOP.

    cleanup( ).
  ENDMETHOD.


  METHOD read_sel_from_field.
    DATA(so_tab) = |S_{ id WIDTH = 3 PAD = '0' ALIGN = RIGHT }[]|.
    DATA(so_struct) = |S_{ id WIDTH = 3 PAD = '0' ALIGN = RIGHT }|.
    DATA(so_type) = |SEL-S_{ id WIDTH = 3 PAD = '0' ALIGN = RIGHT }|.

    FIELD-SYMBOLS: <so_tab> TYPE table.
    ASSIGN (so_tab) TO <so_tab>.
    ASSIGN (so_struct) TO FIELD-SYMBOL(<so_struct>).
    ASSIGN (so_type) TO FIELD-SYMBOL(<so_type>).

    CLEAR: <so_struct>.
    <so_type> = |{ field->table }-{ field->field }|.
    <so_tab> = field->range.
  ENDMETHOD.

  METHOD import_sel_to_field.
    DATA(so_tab) = |S_{ id WIDTH = 3 PAD = '0' ALIGN = RIGHT }[]|.
    FIELD-SYMBOLS: <so_tab> TYPE table.
    ASSIGN (so_tab) TO <so_tab>.
    field->range = <so_tab>.
  ENDMETHOD.

  METHOD clear_sel.
    DATA(so_tab) = |S_{ id WIDTH = 3 PAD = '0' ALIGN = RIGHT }[]|.
    DATA(so_struct) = |S_{ id WIDTH = 3 PAD = '0' ALIGN = RIGHT }|.
    DATA(so_type) = |SEL-S_{ id WIDTH = 3 PAD = '0' ALIGN = RIGHT }|.

    FIELD-SYMBOLS: <so_tab> TYPE table.
    ASSIGN (so_tab) TO <so_tab>.
    ASSIGN (so_struct) TO FIELD-SYMBOL(<so_struct>).
    ASSIGN (so_type) TO FIELD-SYMBOL(<so_type>).

    CLEAR: <so_struct>, <so_type>, <so_tab>.
  ENDMETHOD.

  METHOD refresh_visibility.
    LOOP AT SCREEN.
      screen-active = COND #( WHEN screen-group1 <= last_used_id THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

  METHOD cleanup.
    last_used_id = 0.
    DATA(id) = 1.
    WHILE id <= c_max_id.
      clear_sel( id ).
      id = id + 1.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
