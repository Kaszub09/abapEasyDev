FUNCTION zed_screens_set_commands.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(DYNAMIC_COMMANDS) TYPE
*"        ZIF_ED_SCREENS_HANDLER=>TT_COMMAND
*"----------------------------------------------------------------------
  commands = dynamic_commands.

  CLEAR dynamic.
  LOOP AT commands REFERENCE INTO DATA(command).
    ASSIGN COMPONENT |DYNAMIC_{ sy-tabix }| OF STRUCTURE dynamic TO FIELD-SYMBOL(<dynamic>).
    <dynamic> = command->description.
  ENDLOOP.
ENDFUNCTION.
