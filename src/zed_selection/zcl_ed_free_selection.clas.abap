"! <p class="shorttext synchronized">Based on free selections.</p>
CLASS zcl_ed_free_selection DEFINITION PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS zcl_ed_selection_factory.
  PUBLIC SECTION.
    INTERFACES:
      zif_ed_selection.
  PRIVATE SECTION.
    DATA:
      sel_id     TYPE  dynselid,
      fields_tab TYPE STANDARD TABLE OF rsdsfields WITH EMPTY KEY.
ENDCLASS.

CLASS zcl_ed_free_selection IMPLEMENTATION.
  METHOD zif_ed_selection~display.

    DATA:field_ranges TYPE rsds_trange.
    DATA(as_window) = xsdbool( start_column <> 0 AND start_line <> 0 ).
    CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
      EXPORTING
        selection_id    = sel_id
        as_window       = as_window            " X: Selection screen as dialog box
        start_row       = start_line                " Dialog box: Initial row
        start_col       = start_column                " Dialog box: Initial column
      IMPORTING
        field_ranges    = field_ranges                " Selections in form of RANGES tables
      TABLES
        fields_tab      = fields_tab                 " Returns selected fields
      EXCEPTIONS
        internal_error  = 1                " Internal error
        no_action       = 2                " Canceled by user
        selid_not_found = 3                " Transfer non-existent selection ID
        illegal_status  = 4                " Invalid status number
        OTHERS          = 5.

    IF sy-subrc = 0.
      user_confirmed = abap_true.

    ELSEIF sy-subrc = 2.
      user_confirmed = abap_false.
      RETURN. "Skip field reading

    ELSE.
      zcx_ed_exception=>throw( |FREE_SELECTIONS_DIALOG sy-subrc={ sy-subrc }| ).
    ENDIF.


    LOOP AT field_ranges REFERENCE INTO DATA(table_range).
      LOOP AT zif_ed_selection~selection REFERENCE INTO DATA(table_sel) USING KEY table WHERE table = table_range->tablename.
        "Clear unselected
        LOOP AT table_sel->fields REFERENCE INTO DATA(field).
          field->is_selected = abap_false.
          FREE: field->range.
        ENDLOOP.

        "Get ranges
        LOOP AT table_range->frange_t REFERENCE INTO DATA(field_range).
          LOOP AT table_sel->fields REFERENCE INTO field USING KEY field WHERE field = field_range->fieldname.
            field->is_selected = abap_true.
            field->range = field_range->selopt_t.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_ed_selection~get_as_where_clause.
    DATA: selopt_tab TYPE STANDARD TABLE OF ddshselopt WITH EMPTY KEY.

    LOOP AT zif_ed_selection~selection REFERENCE INTO DATA(selection).
      LOOP AT selection->fields REFERENCE INTO DATA(field) WHERE is_selected = abap_true.
        LOOP AT field->range REFERENCE INTO DATA(field_range).
          DATA(field_sel) = CORRESPONDING ddshselopt( field_range->* ).
          field_sel-shlpname = VALUE #( zif_ed_selection~aliases[ table = selection->table ]-alias DEFAULT selection->table ).
          field_sel-shlpfield = field->field.
          APPEND field_sel TO selopt_tab.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    CALL FUNCTION 'F4_CONV_SELOPT_TO_WHERECLAUSE'
      EXPORTING
        gen_alias_names = add_aliases
      IMPORTING
        where_clause    = where
      TABLES
        selopt_tab      = selopt_tab.
  ENDMETHOD.
ENDCLASS.
