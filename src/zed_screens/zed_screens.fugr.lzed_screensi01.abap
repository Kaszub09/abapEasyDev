*----------------------------------------------------------------------*
***INCLUDE LZMK_SCREEN_WITH_CONTAINERI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command INPUT.
  DATA(command_converted) = sy-ucomm.
  IF command_converted CP 'DYNAMIC_*'.
    command_converted = commands[ CONV i( replace( val = command_converted sub = 'DYNAMIC_' with = ||  ) ) ]-command.
  ENDIF.
  zcl_ed_screens=>zif_ed_screens_function_group~raise_pai( dynnr = sy-dynnr command = command_converted ).
ENDMODULE.
