REPORT zed_selection_ex_fields.

DATA(selection) = zcl_ed_selection_factory=>create_from_selection(
    VALUE #( (  table = 'TADIR' fields = VALUE #( ( field = 'OBJ_NAME' ) ( field = 'AUTHOR' ) ) )
             (  table = 'SFLIGHT' fields = VALUE #( ( field = 'CARRID' ) ( field = 'CONNID' ) ) )
           ) ).
selection->aliases = VALUE #( ( table = 'TADIR' alias = 'SUPER_TADIR' ) ).

DATA(result) = selection->display( start_column = 1 start_line = 1 ).
MESSAGE |Did user confirm? '{ result }'| TYPE 'I'.

DATA(where) = selection->get_as_where_clause( ).
"Could be used like this:
"SELECT * FROM tadir WHERE (where) INTO TABLE @DATA(tab).
cl_demo_output=>display( where ).



MESSAGE |Calling 2nd time...| TYPE 'I'.
MESSAGE |Did user confirm? '{ selection->display( start_column = 1 start_line = 1 ) }'| TYPE 'I'.
cl_demo_output=>display( selection->get_as_where_clause( ) ).
