*&---------------------------------------------------------------------*
*& Report zed_selection_ex_simple_usage
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_selection_ex_simple_table.

DATA(selection) = zcl_ed_selection_factory=>create_from_tables( VALUE #( ( 'TADIR' ) ) ).
DATA(result) = selection->display( start_column = 1 start_line = 1 ).

MESSAGE |Did user confirm? '{ result }'| TYPE 'I'.
cl_demo_output=>display( selection->get_as_where_clause( ) ).
