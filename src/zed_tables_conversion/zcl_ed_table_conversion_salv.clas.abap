CLASS zcl_ed_table_conversion_salv DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Applies filters, sorts, hide columns etc. according to layout in salv.
      get_from_salv IMPORTING original_table TYPE REF TO data salv TYPE REF TO cl_salv_table RETURNING VALUE(display_table) TYPE REF TO data.

  PRIVATE SECTION.
    CLASS-METHODS:
      get_display_ref IMPORTING original_table TYPE REF TO data salv TYPE REF TO cl_salv_table RETURNING VALUE(display_table) TYPE REF TO data,
      filter_ref IMPORTING display_table TYPE REF TO data salv TYPE REF TO cl_salv_table,
      sort_ref IMPORTING display_table TYPE REF TO data salv TYPE REF TO cl_salv_table,
      subtotals_ref IMPORTING display_table TYPE REF TO data salv TYPE REF TO cl_salv_table,
      expand_components CHANGING components TYPE cl_abap_structdescr=>component_table.
ENDCLASS.

CLASS zcl_ed_table_conversion_salv IMPLEMENTATION.
  METHOD get_from_salv.
    ASSIGN original_table->* TO FIELD-SYMBOL(<original_table>).

    DATA copy TYPE REF TO data.
    CREATE DATA copy LIKE <original_table>.
    ASSIGN copy->* TO FIELD-SYMBOL(<copy>).
    <copy> = CORRESPONDING #( <original_table> ).

    filter_ref( display_table = copy salv = salv ).
    sort_ref( display_table = copy salv = salv ).
    subtotals_ref( display_table = copy salv = salv ).

    display_table = get_display_ref( original_table = original_table salv = salv ).
    ASSIGN display_table->* TO FIELD-SYMBOL(<display_table>).
    <display_table> = CORRESPONDING #( <copy> ).
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
      DATA(is_descending) = xsdbool( sort->r_sort->get_sequence( ) = if_salv_c_sort=>sort_down ).
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

  METHOD subtotals_ref.
    "Don't even try if no need.
    IF lines( salv->get_aggregations( )->get( ) ) = 0.
      RETURN.
    ENDIF.

    "Need to also take care of currency and quantity field, maybe one day...
    "On the other hand, you could just do it manually by specifying column as sort so...
    FIELD-SYMBOLS: <display_table> TYPE table.

    ASSIGN display_table->* TO <display_table>.
    "-----------------------------------------------------------------
    "Create table for subtotals
    TYPES:
      BEGIN OF t_subtotals,
        row_count     TYPE i, "needed for averages
        is_compressed TYPE abap_bool,
        data          TYPE REF TO data,
      END OF t_subtotals,
      tt_subtotals TYPE STANDARD TABLE OF t_subtotals WITH EMPTY KEY.
    DATA:
      subtotal  TYPE REF TO data,
      subtotals TYPE tt_subtotals.

    "Just to catch row structure
    APPEND INITIAL LINE TO <display_table> ASSIGNING FIELD-SYMBOL(<display_table_line>).
    "Add subtotal level for each sort + one for total.
    DATA(subtotals_count) = lines( salv->get_sorts( )->get( ) ) + 1.
    DO subtotals_count TIMES.
      CREATE DATA subtotal LIKE <display_table_line>.
      APPEND VALUE #( row_count = 0 data = subtotal ) TO subtotals.
    ENDDO.
    "Remove dummy row
    DELETE <display_table> INDEX lines( <display_table> ).

    "-----------------------------------------------------------------
    DATA(lines_to_skip) = 0.
    LOOP AT <display_table> ASSIGNING FIELD-SYMBOL(<row>).
      DATA(index) = sy-tabix.

      "Skip lines that are subtotals
      IF lines_to_skip > 0.
        lines_to_skip = lines_to_skip - 1.
        CONTINUE.
      ENDIF.

      "Add all aggregations to all subtotals.
      LOOP AT subtotals REFERENCE INTO DATA(subtotal_ref).
        subtotal_ref->row_count = subtotal_ref->row_count + 1.
        ASSIGN subtotal_ref->data->* TO FIELD-SYMBOL(<subtotal_row>).
        LOOP AT salv->get_aggregations( )->get( ) REFERENCE INTO DATA(agg).
          ASSIGN COMPONENT agg->columnname OF STRUCTURE <subtotal_row> TO FIELD-SYMBOL(<agg_field>).
          ASSIGN COMPONENT agg->columnname OF STRUCTURE <row> TO FIELD-SYMBOL(<row_value>).
          "Add to each subtotal
          CASE agg->r_aggregation->get( ).
            WHEN 1."TOTAL
              <agg_field> = <agg_field> + <row_value>.
            WHEN 2."MAX
              IF subtotal_ref->row_count = 1 OR <agg_field> < <row_value>.
                <agg_field> = <row_value>.
              ENDIF.
            WHEN 3."MIN
              IF subtotal_ref->row_count = 1 OR <agg_field> > <row_value>.
                <agg_field> = <row_value>.
              ENDIF.
            WHEN 4."AVERAGE
              <agg_field> = <agg_field> * ( subtotal_ref->row_count - 1 ) DIV subtotal_ref->row_count + <row_value>.
          ENDCASE.
        ENDLOOP.
      ENDLOOP.
      DATA(asd1232) = salv->get_sorts( )->get_compressed_subtotal( ).
      DATA(asd123) = salv->get_sorts( )->get_compressed_subtotal( ).
      "Check if new sort - order is as for subtotals levels
      LOOP AT salv->get_sorts( )->get( ) REFERENCE INTO DATA(sort).
        DATA(sort_index) = sy-tabix.
        IF NOT sort->r_sort->is_subtotalled( ).
          CONTINUE.
        ENDIF.

        FIELD-SYMBOLS:
          <next>    TYPE any,
          <present> TYPE any.

        IF index = lines( <display_table> ).
          UNASSIGN <next>.
        ELSE.
          ASSIGN COMPONENT sort->columnname OF STRUCTURE <display_table>[ index + 1 ] TO <next>.
        ENDIF.
        ASSIGN COMPONENT sort->columnname OF STRUCTURE <display_table>[ index ] TO <present>.

        "Fill subtotal col value, previous levels columns also.
        LOOP AT subtotals REFERENCE INTO subtotal_ref TO lines( subtotals ) - sort_index.
          subtotal_ref->is_compressed = sort->r_sort->is_subtotal_compressed( ).
          ASSIGN subtotal_ref->data->* TO <subtotal_row>.
          ASSIGN COMPONENT sort->columnname OF STRUCTURE <subtotal_row> TO FIELD-SYMBOL(<subtotal_level_value>).
          <subtotal_level_value> = <present>.
        ENDLOOP.

        IF <next> IS NOT ASSIGNED OR <present> <> <next>.
          "New subtotal to add
          lines_to_skip = lines_to_skip + 1.
        ENDIF.
      ENDLOOP.

      "Add all requried subtotals
      IF lines_to_skip > 0.
        DATA(loop_to) = lines_to_skip.
        DATA(insert_at) = index + 1.
        LOOP AT subtotals REFERENCE INTO subtotal_ref TO loop_to.
          DATA(subtotal_index) = sy-tabix.
          "Skip compressed subtotals
          DATA(next_is_compressed) = VALUE #( subtotals[ subtotal_index + 1 ]-is_compressed DEFAULT abap_false ).
          IF next_is_compressed = abap_true.
            lines_to_skip = lines_to_skip - 1.
            CONTINUE.
          ENDIF.

          ASSIGN subtotal_ref->data->* TO <subtotal_row>.
          INSERT <subtotal_row> INTO <display_table> INDEX insert_at.
          insert_at = insert_at + 1.
          CLEAR <subtotal_row>.
        ENDLOOP.
      ENDIF.

      "Delete normal row if any is compressed
      LOOP AT subtotals REFERENCE INTO subtotal_ref WHERE is_compressed = abap_true.
        DELETE <display_table> INDEX index.
        EXIT.
      ENDLOOP.
    ENDLOOP.

    "Add totals
    DATA(total_ref) = subtotals[ lines( subtotals ) ]-data.
    ASSIGN total_ref->* TO <subtotal_row>.
    APPEND <subtotal_row> TO <display_table>.
  ENDMETHOD.

ENDCLASS.
