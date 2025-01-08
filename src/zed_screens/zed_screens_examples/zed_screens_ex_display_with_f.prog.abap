*&---------------------------------------------------------------------*
*& Report zed_screens_example_call
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_screens_ex_display_with_f.

"Table with data to display something
TYPES:
  BEGIN OF t_table,
    screen TYPE i,
  END OF t_table,
  tt_table TYPE STANDARD TABLE OF t_table WITH EMPTY KEY.
DATA(table) = VALUE tt_table( (  screen = 1 ) (  screen = 2 ) ).

"Handler for commands
CLASS lcl_handler DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES:
      zif_ed_screens_handler.
ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD zif_ed_screens_handler~pbo_get_toolbar_commands.
    "Add commands to toolbar - screen with toolbar should be called
    commands_to_set = VALUE #( ( command = 'COMMAND_1' description = VALUE #( icon_id = icon_okay text = 'Command 1' icon_text = 'Command 1' ) )
      ( command = 'COMMAND_2' description = VALUE #( icon_id = icon_calculation text = 'Command 2' icon_text = 'Command 2' ) ) ).
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pai.
    "Handle added commands and not-excluded status commands
    MESSAGE |'{ command }' was clicked| TYPE 'I'.
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pbo.
    "You could set different status/title here, or e.g. create docking container manually etc.
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pbo_change_header.
    "Set new header
    header = 'New header'.
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pbo_get_status_commands_to_ex.
    "Don't display save button
    APPEND zif_ed_screens_handler=>c_status_commands-save TO commands_to_exclude.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  "Get container, use it to create SALV_TABLE, display it
  DATA(container) = zcl_ed_screens=>prepare_next_screen( handler = NEW lcl_handler( ) ).
  cl_salv_table=>factory( EXPORTING r_container = container IMPORTING r_salv_table = DATA(salv_table) CHANGING t_table = table ).
  salv_table->display( ).
  zcl_ed_screens=>call_next_screen( ).
