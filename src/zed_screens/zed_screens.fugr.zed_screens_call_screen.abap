FUNCTION zed_screens_call_screen.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(DYNNR) TYPE  DYNNR
*"     REFERENCE(START_COLUMN) TYPE  I
*"     REFERENCE(END_COLUMN) TYPE  I
*"     REFERENCE(START_LINE) TYPE  I
*"     REFERENCE(END_LINE) TYPE  I
*"----------------------------------------------------------------------
  CALL SCREEN dynnr STARTING AT start_column start_line ENDING AT end_column end_line.
ENDFUNCTION.
