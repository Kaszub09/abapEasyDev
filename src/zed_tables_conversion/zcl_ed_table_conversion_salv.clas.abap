CLASS zcl_ed_table_conversion_salv DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Applies filters, sorts, hide columns etc. according to layout in salv.
      "! <br/>Totals/Subtotals not supported (yet?)</p>
      get_from_salv IMPORTING original_table TYPE REF TO data salv TYPE REF TO cl_salv_table RETURNING VALUE(display_table) TYPE REF TO data.

  PRIVATE SECTION.
    CLASS-METHODS:
      get_display_ref IMPORTING original_table TYPE REF TO data salv TYPE REF TO cl_salv_table RETURNING VALUE(display_table) TYPE REF TO data,
      filter_ref IMPORTING display_table TYPE REF TO data salv TYPE REF TO cl_salv_table,
      sort_ref IMPORTING display_table TYPE REF TO data salv TYPE REF TO cl_salv_table,
      expand_components CHANGING components TYPE cl_abap_structdescr=>component_table.
ENDCLASS.

CLASS zcl_ed_table_conversion_salv IMPLEMENTATION.
  METHOD get_from_salv.
    display_table = get_display_ref( original_table = original_table salv = salv ).

    ASSIGN original_table->* TO FIELD-SYMBOL(<original_table>).
    ASSIGN display_table->* TO FIELD-SYMBOL(<display_table>).

    <display_table> = CORRESPONDING #( <original_table> ).

    filter_ref( display_table = display_table salv = salv ).
    sort_ref( display_table = display_table salv = salv ).
  ENDMETHOD.

  METHOD get_display_ref.
    DATA(table_descr) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( original_table ) ).
    DATA(struct_descr) = CAST cl_abap_structdescr( table_descr->get_table_line_type( ) ).

    DATA(components) = struct_descr->get_components( ).
    expand_components( CHANGING components = components ).


    LOOP AT salv->get_columns( )->get( ) REFERENCE INTO DATA(col).
      IF col->r_column->is_technical( ) OR NOT col->r_column->is_visible( ).
        DELETE components WHERE name = col->columnname.
      ENDIF.
    ENDLOOP.

    struct_descr = cl_abap_structdescr=>create( components ).
    table_descr = cl_abap_tabledescr=>create( p_line_type = struct_descr p_key_kind = cl_abap_tabledescr=>keydefkind_empty ).
    CREATE DATA display_table TYPE HANDLE table_descr.
  ENDMETHOD.

  METHOD filter_ref.
    FIELD-SYMBOLS: <display_table> TYPE table.

    ASSIGN display_table->* TO <display_table>.

    LOOP AT salv->get_filters( )->get( ) REFERENCE INTO DATA(filter).
      DATA(selection_tab) = VALUE rsdsselopt_t( ).
      LOOP AT filter->r_filter->get( ) INTO DATA(selection_row).
        APPEND VALUE #( sign = selection_row->get_sign( ) option = selection_row->get_option( )
            low = selection_row->get_low( ) high = selection_row->get_high( ) ) TO selection_tab.
      ENDLOOP.
      DATA(delete_where) = |NOT { filter->columnname } IN selection_tab|.
      DELETE <display_table> WHERE (delete_where).
    ENDLOOP.
  ENDMETHOD.

  METHOD sort_ref.
    FIELD-SYMBOLS: <display_table> TYPE table.

    ASSIGN display_table->* TO <display_table>.

    DATA(sort_tab) = VALUE abap_sortorder_tab( ).

    LOOP AT salv->get_sorts( )->get( ) REFERENCE INTO DATA(sort).
      DATA(is_descending) = COND #( WHEN sort->r_sort->get_sequence( ) = if_salv_c_sort=>sort_down THEN abap_true ELSE abap_false ).
      APPEND VALUE #( name = sort->columnname descending = is_descending ) TO sort_tab.
    ENDLOOP.

    SORT <display_table> BY (sort_tab).
  ENDMETHOD.

  METHOD expand_components.
    DATA:
      index        TYPE i,
      struct_descr TYPE REF TO cl_abap_structdescr.

    LOOP AT components REFERENCE INTO DATA(comp) WHERE as_include = abap_true.
      index = sy-tabix.
      struct_descr = CAST cl_abap_structdescr( comp->type ).
    ENDLOOP.

    IF struct_descr IS NOT BOUND.
      RETURN.
    ENDIF.

    DELETE components INDEX index.
    INSERT LINES OF struct_descr->get_components( ) INTO components INDEX index .
    expand_components( CHANGING components = components ).
  ENDMETHOD.

ENDCLASS.
