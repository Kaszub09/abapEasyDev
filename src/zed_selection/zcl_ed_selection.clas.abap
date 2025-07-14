"! <p class="shorttext synchronized">"Failed attempt.
"! Check comment inside include LZED_SELECTIONL03, method AT_SELECTION_SCREEN_OUTPUT.
"! You may play with creators in zcl_ed_selection_factory_inj</p>
CLASS zcl_ed_selection DEFINITION PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS zcl_ed_selection_factory zcl_ed_selection_factory_inj.
  PUBLIC SECTION.
    INTERFACES:
      zif_ed_selection.
ENDCLASS.

CLASS zcl_ed_selection IMPLEMENTATION.
  METHOD zif_ed_selection~display.
    DATA user_selection TYPE zif_ed_selection=>tt_table_selection.

    CALL FUNCTION 'ZED_SELECTION_DISPLAY'
      EXPORTING
        start_column      = start_column
        end_column        = end_column
        start_line        = start_line
        end_line          = end_line
        initial_selection = zif_ed_selection~selection
      IMPORTING
        user_confirmed    = user_confirmed
        user_selection    = user_selection.
    IF user_confirmed = abap_true.
      zif_ed_selection~selection = user_selection.
    ENDIF.
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
        gen_alias_names = 'X'
      IMPORTING
        where_clause    = where
      TABLES
        selopt_tab      = selopt_tab.
  ENDMETHOD.
ENDCLASS.
