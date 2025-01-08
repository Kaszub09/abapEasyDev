*&---------------------------------------------------------------------*
*& Report zed_screens_example_call
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_screens_ex_simple_display.

"Table with data to display something
TYPES:
  BEGIN OF t_table,
    screen TYPE i,
  END OF t_table,
  tt_table TYPE STANDARD TABLE OF t_table WITH EMPTY KEY.
DATA(table) = VALUE tt_table( (  screen = 1 ) (  screen = 2 ) ).

"Get container, use it to create SALV_TABLE, display it
DATA(container) = zcl_ed_screens=>prepare_next_screen( ).
cl_salv_table=>factory( EXPORTING r_container = container IMPORTING r_salv_table = DATA(salv_table) CHANGING t_table = table ).
salv_table->display( ).
zcl_ed_screens=>call_next_screen( ).
