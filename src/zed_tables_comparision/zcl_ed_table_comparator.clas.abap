"! <p class="shorttext synchronized">Table comparator TODO</p>
"! <br/>TAGS: rtts; keys; struct; table
CLASS zcl_ed_table_comparator DEFINITION PUBLIC CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_comp_result_indexes,
        "! Original table used in comparision
        initial_data    TYPE REF TO data,
        "! Original table used in comparision
        modified_data   TYPE REF TO data,
        "! Table with indexes after comparision
        duplicates      TYPE STANDARD TABLE OF i WITH EMPTY KEY,
        "! Table with indexes after comparision
        deleted         TYPE STANDARD TABLE OF i WITH EMPTY KEY,
        "! Table with indexes after comparision
        inserted        TYPE STANDARD TABLE OF i WITH EMPTY KEY,
        "! Table with indexes after comparision
        before_modified TYPE STANDARD TABLE OF i WITH EMPTY KEY,
        "! Table with indexes after comparision
        modified        TYPE STANDARD TABLE OF i WITH EMPTY KEY,
      END OF t_comp_result_indexes,
      BEGIN OF t_comp_result_data,
        "! Original table used in comparision
        initial_data    TYPE REF TO data,
        "! Original table used in comparision
        modified_data   TYPE REF TO data,
        "! Table with records after comparision
        duplicates      TYPE REF TO data,
        "! Table with records after comparision
        deleted         TYPE REF TO data,
        "! Table with records after comparision
        inserted        TYPE REF TO data,
        "! Table with records after comparision
        before_modified TYPE REF TO data,
        "! Table with records after comparision
        modified        TYPE REF TO data,
      END OF t_comp_result_data.
    CLASS-METHODS:
      "! <p class="shorttext synchronized">Import tables must be of the same type as used in <em>rtts_keys</em>.
      "! Export table will also be of the same type</p>
      "! @parameter result_indexes | <p class="shorttext synchronized">Duplicate record is added per detected duplicate with previous.
      "! [number of duplicates] = [number of same records] - 1.</p>
      compare_tables IMPORTING initial_data TYPE REF TO data modified_data TYPE REF TO data rtts_keys TYPE REF TO zcl_ed_rtts_keys
                     RETURNING VALUE(result_indexes) TYPE t_comp_result_indexes,
      get_comparision_data IMPORTING result_indexes TYPE t_comp_result_indexes RETURNING VALUE(result_data) TYPE t_comp_result_data,
      "! <p class="shorttext synchronized">Updates mandant field</p>
      update_mandant IMPORTING table TYPE REF TO data.

  PRIVATE SECTION.
    CLASS-METHODS:
      prepare_key_tables IMPORTING index_field TYPE fieldname initial_data TYPE table modified_data TYPE table rtts_keys TYPE REF TO zcl_ed_rtts_keys
                        CHANGING initial_keys TYPE table modified_keys TYPE table.

    CONSTANTS:
        c_index_field TYPE fieldname VALUE 'ZED_TABLE_COMPARATOR_INDEX_F'.
ENDCLASS.

CLASS zcl_ed_table_comparator IMPLEMENTATION.
  METHOD compare_tables.
    "=================================================================
    " Data ref, field-symbols declaration/creation/assignment
    "-----------------------------------------------------------------
    "All records tables.
    result_indexes-initial_data = initial_data.
    result_indexes-modified_data = modified_data.
    assign_to_table_fs modified_data->* <modified_data>.
    assign_to_table_fs initial_data->* <initial_data>.
    "--------------------------------------------------
    "Create key tables.
    rtts_keys->get_descr( EXPORTING additional_fields = VALUE #( ( name = c_index_field type = CAST #( cl_abap_elemdescr=>describe_by_data( 0 ) ) ) )
                                    include_mandt = abap_false keys_only = abap_true
                          IMPORTING table = DATA(table) ).

    DATA initial_keys TYPE REF TO data.
    DATA modified_keys TYPE REF TO data.
    CREATE DATA initial_keys TYPE HANDLE table.
    CREATE DATA modified_keys TYPE HANDLE table.
    assign_to_table_fs initial_keys->* <initial_keys>.
    assign_to_table_fs modified_keys->* <modified_keys>.
    "--------------------------------------------------
    prepare_key_tables( EXPORTING index_field = c_index_field modified_data = <modified_data> initial_data = <initial_data> rtts_keys = rtts_keys
                        CHANGING modified_keys = <modified_keys> initial_keys = <initial_keys> ).

    "Declare indexes for traversing
    DATA(i_max_initial) = lines( <initial_data> ).
    DATA(i_max_modified) = lines( <modified_data> ).
    DATA(i_initial) = 1.
    DATA(i_modified) = 1.
    "Declare indexes of data tables
    FIELD-SYMBOLS <modified_data_index> TYPE i.
    FIELD-SYMBOLS <initial_data_index> TYPE i.
    DATA modified_data_index TYPE i.
    DATA initial_data_index TYPE i.

    "=================================================================
    " Actual comparision
    "-----------------------------------------------------------------
    WHILE i_initial <= i_max_initial AND i_modified <= i_max_modified.
      "Remember indexes and remove them from key tabs, because they will interfere with comparison
      ASSIGN COMPONENT c_index_field OF STRUCTURE <modified_keys>[ i_modified ] TO <modified_data_index>.
      ASSIGN COMPONENT c_index_field OF STRUCTURE <initial_keys>[ i_initial ] TO <initial_data_index>.
      modified_data_index = COND #( WHEN <modified_data_index> = 0 THEN modified_data_index ELSE <modified_data_index> ).
      initial_data_index = COND #( WHEN <initial_data_index> = 0 THEN initial_data_index ELSE <initial_data_index> ).
      CLEAR: <modified_data_index>, <initial_data_index>.

      "Check for key duplicates with previous record (not next since index is not yet removed)
      IF i_modified > 1.
        IF <modified_keys>[ i_modified ] = <modified_keys>[ i_modified - 1 ].
          APPEND modified_data_index TO result_indexes-duplicates.
        ENDIF.
      ENDIF.

      "Compare with original based on key fields
      IF <initial_keys>[ i_initial ] = <modified_keys>[ i_modified ].
        "^Same key record
        IF <initial_data>[ initial_data_index ] <> <modified_data>[ modified_data_index ].
          "^Same whole records, copy after and before modified
          APPEND initial_data_index TO result_indexes-before_modified.
          APPEND modified_data_index TO result_indexes-modified.
        ENDIF.
        i_modified = i_modified + 1.
        i_initial = i_initial + 1.

      ELSEIF <initial_keys>[ i_initial ] < <modified_keys>[ i_modified  ].
        "^Record deleted from original
        APPEND initial_data_index TO result_indexes-deleted.

        i_initial = i_initial + 1.

      ELSE.
        "^Record added to modified
        APPEND modified_data_index TO result_indexes-inserted.
        i_modified = i_modified + 1.

      ENDIF.
    ENDWHILE.

    "Add skipped records
    WHILE i_initial <= i_max_initial.
      ASSIGN COMPONENT c_index_field OF STRUCTURE <initial_keys>[ i_initial ] TO <initial_data_index>.
      initial_data_index = COND #( WHEN <initial_data_index> = 0 THEN initial_data_index ELSE <initial_data_index> ).
      APPEND initial_data_index TO result_indexes-deleted.
      i_initial = i_initial + 1.
    ENDWHILE.

    WHILE i_modified <= i_max_modified.
      ASSIGN COMPONENT c_index_field OF STRUCTURE <modified_keys>[ i_modified ] TO <modified_data_index>.
      modified_data_index = COND #( WHEN <modified_data_index> = 0 THEN modified_data_index ELSE <modified_data_index> ).
      CLEAR: <modified_data_index>.

      "Check for key duplicates with previous record (not next since index is not yet removed)
      IF i_modified > 1.
        IF <modified_keys>[ i_modified ] = <modified_keys>[ i_modified - 1 ].
          APPEND modified_data_index TO result_indexes-duplicates.
        ENDIF.
      ENDIF.
      APPEND modified_data_index TO result_indexes-inserted.
      i_modified = i_modified + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_comparision_data.
    "All records tables.
    result_data-modified_data = result_indexes-modified_data.
    result_data-initial_data = result_indexes-initial_data.
    assign_to_table_fs result_indexes-initial_data->* <initial_data>.
    assign_to_table_fs result_indexes-modified_data->* <modified_data>.
    "--------------------------------------------------
    "Tables for records. Amount of unnecessary typing is ridiculous -> got some help from macro
    CREATE DATA result_data-before_modified LIKE <initial_data>.
    CREATE DATA result_data-modified LIKE <initial_data>.
    CREATE DATA result_data-inserted LIKE <initial_data>.
    CREATE DATA result_data-deleted LIKE <initial_data>.
    CREATE DATA result_data-duplicates LIKE <initial_data>.
    assign_to_table_fs result_data-before_modified->* <before_modified>.
    assign_to_table_fs result_data-modified->* <modified>.
    assign_to_table_fs result_data-inserted->* <inserted>.
    assign_to_table_fs result_data-deleted->* <deleted>.
    assign_to_table_fs result_data-duplicates->* <duplicates>.
    "--------------------------------------------------
    DATA index TYPE i.
    LOOP AT result_indexes-before_modified INTO index.
      APPEND <initial_data>[ index ] TO <before_modified>.
    ENDLOOP.
    LOOP AT result_indexes-modified INTO index.
      APPEND <modified_data>[ index ] TO <modified>.
    ENDLOOP.
    LOOP AT result_indexes-inserted INTO index.
      APPEND <modified_data>[ index ] TO <inserted>.
    ENDLOOP.
    LOOP AT result_indexes-deleted INTO index.
      APPEND <initial_data>[ index ] TO <deleted>.
    ENDLOOP.
    LOOP AT result_indexes-duplicates INTO index.
      APPEND <modified_data>[ index ] TO <duplicates>.
    ENDLOOP.
  ENDMETHOD.

  METHOD update_mandant.
    assign_to_table_fs table->* <table>.
    LOOP AT <table> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT 'MANDT' OF STRUCTURE <row> TO FIELD-SYMBOL(<mandt>).
      <mandt> = sy-mandt.
    ENDLOOP.
  ENDMETHOD.

  METHOD prepare_key_tables.
    "Fill key tables
    initial_keys = CORRESPONDING #( initial_data ).
    modified_keys = CORRESPONDING #( modified_data ).

    "Assign index of entries in original tables - because key tables will be sorted and we don't want to change order of what's displayed
    FIELD-SYMBOLS <index_field> TYPE any.
    DATA(index) = 1.
    LOOP AT modified_keys ASSIGNING FIELD-SYMBOL(<modified_keys_entry>).
      ASSIGN COMPONENT index_field OF STRUCTURE <modified_keys_entry> TO <index_field>.
      <index_field> = index.
      index = index + 1.
    ENDLOOP.

    index = 1.
    LOOP AT initial_keys ASSIGNING FIELD-SYMBOL(<initial_keys_entry>).
      ASSIGN COMPONENT index_field OF STRUCTURE <initial_keys_entry> TO <index_field>.
      <index_field> = index.
      index = index + 1.
    ENDLOOP.

    "Create sort condition
    DATA sort_order TYPE abap_sortorder_tab.
    LOOP AT rtts_keys->get_components( include_mandt = abap_false keys_only = abap_true ) REFERENCE INTO DATA(component).
      APPEND VALUE #( name = component->name descending = abap_false ) TO sort_order.
    ENDLOOP.

    "Sort key tables
    SORT modified_keys BY (sort_order).
    SORT initial_keys BY (sort_order).
  ENDMETHOD.


ENDCLASS.
