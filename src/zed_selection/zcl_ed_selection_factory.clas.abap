"! <p class="shorttext synchronized" lang="en">Selection factory</p>
"! <br/>TAGS: factory; selection; free selections
CLASS zcl_ed_selection_factory DEFINITION PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS zcl_ed_selection_factory_inj.

  PUBLIC SECTION.
    TYPES:
        tt_table TYPE STANDARD TABLE OF tablenam WITH EMPTY KEY.

    CLASS-METHODS:
      create_from_tables IMPORTING tables         TYPE tt_table
                         RETURNING VALUE(sel_obj) TYPE REF TO zif_ed_selection
                         RAISING   zcx_ed_exception,
      create_from_selection IMPORTING selection      TYPE zif_ed_selection=>tt_table_selection
                            RETURNING VALUE(sel_obj) TYPE REF TO zif_ed_selection.

  PRIVATE SECTION.
    CLASS-DATA:
        selection_mock TYPE REF TO zif_ed_selection.
ENDCLASS.

CLASS zcl_ed_selection_factory IMPLEMENTATION.
  METHOD create_from_tables.
    IF selection_mock IS BOUND.
      sel_obj = selection_mock.
      RETURN.
    ENDIF.

    DATA(free_sel) = NEW zcl_ed_free_selection( ).
    sel_obj = free_sel.

    DATA: tables_copy TYPE STANDARD TABLE OF rsdstabs WITH EMPTY KEY.
    tables_copy = VALUE #( FOR tab IN tables ( prim_tab = tab ) ).

    CALL FUNCTION 'FREE_SELECTIONS_INIT'
      EXPORTING
        kind                     = 'T'
      IMPORTING
        selection_id             = free_sel->sel_id
      TABLES
        tables_tab               = tables_copy
      EXCEPTIONS
        fields_incomplete        = 1                " Only PRIM_FNAME filled
        fields_no_join           = 2                " Field assignment without join
        field_not_found          = 3                " Dictionary field not found
        no_tables                = 4                " Table P_TABLES is empty
        table_not_found          = 5                " A table from P_TABLES was not found
        expression_not_supported = 6                " Expression not (yet) supported
        incorrect_expression     = 7                " Incorrect logical expression
        illegal_kind             = 8                " KIND not equal to T,G,F or: G without P_RSDSQCAT
        area_not_found           = 9                " Invalid key FIELD_GROUPS_KEY
        inconsistent_area        = 10               " Inconsistent selection view
        kind_f_no_fields_left    = 11               " KIND = F, but no field left after cleanup
        kind_f_no_fields         = 12               " KIND = F, but no field passed
        too_many_fields          = 13               " Too many entries in FIELDS_TAB
        dup_field                = 14               " Field doubles in FIELDS_TAB
        field_no_type            = 15               " No field type in the field description
        field_ill_type           = 16               " Non-allowed field type
        dup_event_field          = 17               " Field doubled in EVENT_FIELDS
        node_not_in_ldb          = 18               " Node not part of logical database
        area_no_field            = 19               " Selection view has no fields
        OTHERS                   = 20.
    IF sy-subrc <> 0.
      zcx_ed_exception=>throw( |FREE_SELECTIONS_INIT sy-subrc={ sy-subrc }| ).
    ENDIF.

    LOOP AT tables REFERENCE INTO DATA(table).
      SELECT fieldname, position FROM dd03l
      WHERE tabname = @table->* AND ( NOT fieldname LIKE '.%' ) AND as4local = 'A'
      ORDER BY position DESCENDING
      INTO TABLE @DATA(fields).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND VALUE #( table = table->* ) TO sel_obj->selection REFERENCE INTO DATA(selection_table).
      LOOP AT fields REFERENCE INTO DATA(field).
        APPEND VALUE #( field = field->fieldname ) TO selection_table->fields.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_from_selection.
    IF selection_mock IS BOUND.
      sel_obj = selection_mock.
      RETURN.
    ENDIF.


    DATA(free_sel) = NEW zcl_ed_free_selection( ).
    sel_obj = free_sel.
    sel_obj->selection = selection.

    LOOP AT selection REFERENCE INTO DATA(tab_selection).
      LOOP AT tab_selection->fields REFERENCE INTO DATA(field).
        APPEND VALUE #( tablename = tab_selection->table fieldname = field->field ) TO free_sel->fields_tab.
      ENDLOOP.
    ENDLOOP.

    CALL FUNCTION 'FREE_SELECTIONS_INIT'
      EXPORTING
        kind                     = 'F'
      IMPORTING
        selection_id             = free_sel->sel_id
      TABLES
        fields_tab               = free_sel->fields_tab
      EXCEPTIONS
        fields_incomplete        = 1                " Only PRIM_FNAME filled
        fields_no_join           = 2                " Field assignment without join
        field_not_found          = 3                " Dictionary field not found
        no_tables                = 4                " Table P_TABLES is empty
        table_not_found          = 5                " A table from P_TABLES was not found
        expression_not_supported = 6                " Expression not (yet) supported
        incorrect_expression     = 7                " Incorrect logical expression
        illegal_kind             = 8                " KIND not equal to T,G,F or: G without P_RSDSQCAT
        area_not_found           = 9                " Invalid key FIELD_GROUPS_KEY
        inconsistent_area        = 10               " Inconsistent selection view
        kind_f_no_fields_left    = 11               " KIND = F, but no field left after cleanup
        kind_f_no_fields         = 12               " KIND = F, but no field passed
        too_many_fields          = 13               " Too many entries in FIELDS_TAB
        dup_field                = 14               " Field doubles in FIELDS_TAB
        field_no_type            = 15               " No field type in the field description
        field_ill_type           = 16               " Non-allowed field type
        dup_event_field          = 17               " Field doubled in EVENT_FIELDS
        node_not_in_ldb          = 18               " Node not part of logical database
        area_no_field            = 19               " Selection view has no fields
        OTHERS                   = 20.
    IF sy-subrc <> 0.
      zcx_ed_exception=>throw( |FREE_SELECTIONS_INIT sy-subrc={ sy-subrc }| ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
