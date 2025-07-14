FUNCTION zed_selection_display.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(START_COLUMN) TYPE  I DEFAULT 0
*"     REFERENCE(END_COLUMN) TYPE  I DEFAULT 128
*"     REFERENCE(START_LINE) TYPE  I DEFAULT 0
*"     REFERENCE(END_LINE) TYPE  I DEFAULT 24
*"     REFERENCE(INITIAL_SELECTION) TYPE
*"        ZIF_ED_SELECTION=>TT_TABLE_SELECTION
*"     REFERENCE(HANDLER) TYPE REF TO  ZIF_ED_SELECTION_HANDLER
*"       OPTIONAL
*"  EXPORTING
*"     REFERENCE(USER_CONFIRMED) TYPE  ABAP_BOOL
*"     REFERENCE(USER_SELECTION) TYPE
*"        ZIF_ED_SELECTION=>TT_TABLE_SELECTION
*"----------------------------------------------------------------------
  selection->prepare_for_screen_display( initial_selection = initial_selection handler = handler ).

  CALL SELECTION-SCREEN 1 STARTING AT start_column start_line ENDING AT end_column end_line.

  selection->get_data_after_display( IMPORTING user_confirmed = user_confirmed user_selection = user_selection ).
  selection->cleanup( ).
ENDFUNCTION.
