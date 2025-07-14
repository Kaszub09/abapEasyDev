LOAD-OF-PROGRAM.
  selection = NEW #( ).

AT SELECTION-SCREEN OUTPUT.
  selection->at_selection_screen_output( ).

AT SELECTION-SCREEN.
  selection->at_selection_screen_input( ).

MODULE status_0001 OUTPUT.
  selection->at_screen_output( ).
ENDMODULE.

MODULE user_command_0001 INPUT.
  selection->at_screen_input( ).
ENDMODULE.
