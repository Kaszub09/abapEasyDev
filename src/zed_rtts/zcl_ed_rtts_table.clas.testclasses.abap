TYPES:
  BEGIN OF t_base,
    field1 TYPE c LENGTH 20,
    field2 TYPE i,
    field3 TYPE c LENGTH 10,
    field4 TYPE decfloat34,
  END OF t_base,
  tt_base TYPE STANDARD TABLE OF t_base WITH EMPTY KEY.
"=================================================================
"-----------------------------------------------------------------
CLASS tcl_fields DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_base_ext.
        INCLUDE TYPE t_base.
      TYPES:
        new_f1 TYPE tadir-object,
        new_f2 TYPE tadir-obj_name,
      END OF t_base_ext,
      tt_base_ext TYPE STANDARD TABLE OF t_base_ext WITH EMPTY KEY,
      BEGIN OF t_base_removed,
        field1 TYPE c LENGTH 20,
      END OF t_base_removed,
      tt_base_removed TYPE STANDARD TABLE OF t_base_removed WITH EMPTY KEY.

    CONSTANTS:
      c_new_f1 TYPE fieldname VALUE 'NEW_F1',
      c_new_f2 TYPE fieldname VALUE 'NEW_F2'.

    METHODS:
      setup,
      check_expected_tab IMPORTING expected_tab TYPE any,
      check_expected_row IMPORTING expected_row TYPE any,
      add_no_fields FOR TESTING,
      add_2_field_by_name FOR TESTING,
      add_2_field_by_name_alt FOR TESTING,
      add_2_field_by_descr FOR TESTING,
      add_2_field_by_descr_alt FOR TESTING,
      remove_field FOR TESTING.

    DATA:
      base_tab  TYPE tt_base,
      base_row1 TYPE t_base,
      base_row2 TYPE t_base,
      cut       TYPE REF TO zcl_ed_rtts_table.
ENDCLASS.

CLASS tcl_fields IMPLEMENTATION.
  METHOD setup.
    base_row1 = VALUE #( field1 = 'BASE1 F1' field2 = 1 field3 = 'F3' field4 = '123456.789' ).
    base_row2 = VALUE #( field1 = 'BASE2 F2' field2 = 2 field3 = 'F3' field4 = '123456.789' ).
    base_tab = VALUE #( ( base_row1 ) ( base_row2 ) ).
    cut = zcl_ed_rtts_table=>create_from_table( base_tab ).
  ENDMETHOD.

  METHOD check_expected_tab.
    DATA(act_tab) = cut->create_table( base_tab ).
    ASSIGN act_tab->* TO FIELD-SYMBOL(<act_tab>).

    cl_abap_unit_assert=>assert_equals( act = <act_tab> exp = expected_tab ).
  ENDMETHOD.

  METHOD check_expected_row.
    DATA(act_row) = cut->create_line( expected_row ).
    ASSIGN act_row->* TO FIELD-SYMBOL(<act_row>).

    cl_abap_unit_assert=>assert_equals( act = <act_row> exp = expected_row ).

    "Check if created row has new expected fields, but only if expecting them
    ASSIGN COMPONENT c_new_f1 OF STRUCTURE expected_row TO FIELD-SYMBOL(<new_f1>).
    IF sy-subrc = 0.
      ASSIGN COMPONENT c_new_f1 OF STRUCTURE <act_row> TO <new_f1>.
      IF sy-subrc <> 0.
        cl_abap_unit_assert=>fail( |Field '{ c_new_f1 }' not found | ).
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT c_new_f2 OF STRUCTURE expected_row TO FIELD-SYMBOL(<new_f2>).
    IF sy-subrc = 0.
      ASSIGN COMPONENT c_new_f2 OF STRUCTURE <act_row> TO <new_f2>.
      IF sy-subrc <> 0.
        cl_abap_unit_assert=>fail( |Field '{ c_new_f2 }' not found | ).
      ENDIF.
    ENDIF.

    IF 1 = 2.
      CLEAR: <new_f1>, <new_f2>. "To silence ABAPcleaner unused variable.
    ENDIF.
  ENDMETHOD.

  METHOD add_no_fields.
    check_expected_tab( base_tab ).
    check_expected_row( base_row1 ).
    check_expected_row( base_row2 ).
  ENDMETHOD.

  METHOD add_2_field_by_name.
    cut->add_field( VALUE #( name = c_new_f1 data_element = 'TADIR-OBJECT' ) ).
    cut->add_field( VALUE #( name = c_new_f2 data_element = 'TADIR-OBJ_NAME' ) ).

    check_expected_tab( CORRESPONDING tt_base_ext( base_tab ) ).
    check_expected_row( CORRESPONDING t_base_ext( base_row1 ) ).
    check_expected_row( CORRESPONDING t_base_ext( base_row2 ) ).
  ENDMETHOD.

  METHOD add_2_field_by_name_alt.
    cut->add_fields( VALUE #( ( name = c_new_f1 data_element = 'TADIR-OBJECT' ) ( name = c_new_f2 data_element = 'TADIR-OBJ_NAME' ) ) ).

    check_expected_tab( CORRESPONDING tt_base_ext( base_tab ) ).
    check_expected_row( CORRESPONDING t_base_ext( base_row1 ) ).
    check_expected_row( CORRESPONDING t_base_ext( base_row2 ) ).
  ENDMETHOD.

  METHOD add_2_field_by_descr.
    cut->add_field_by_type( VALUE #( name = c_new_f1 type = CAST #( cl_abap_datadescr=>describe_by_name( 'TADIR-OBJECT' ) ) ) ).
    cut->add_field_by_type( VALUE #( name = c_new_f2 type = CAST #( cl_abap_datadescr=>describe_by_name( 'TADIR-OBJ_NAME' ) ) ) ).

    check_expected_tab( CORRESPONDING tt_base_ext( base_tab ) ).
    check_expected_row( CORRESPONDING t_base_ext( base_row1 ) ).
    check_expected_row( CORRESPONDING t_base_ext( base_row2 ) ).
  ENDMETHOD.

  METHOD add_2_field_by_descr_alt.
    cut->add_fields_by_type( VALUE #(
      ( name = c_new_f1 type = CAST #( cl_abap_datadescr=>describe_by_name( 'TADIR-OBJECT' ) ) )
      ( name = c_new_f2 type = CAST #( cl_abap_datadescr=>describe_by_name( 'TADIR-OBJ_NAME' ) ) ) ) ).
    check_expected_tab( CORRESPONDING tt_base_ext( base_tab ) ).
    check_expected_row( CORRESPONDING t_base_ext( base_row1 ) ).
    check_expected_row( CORRESPONDING t_base_ext( base_row2 ) ).
  ENDMETHOD.

  METHOD remove_field.
    cut->remove_field( 'FIELD2' ).
    cut->remove_field( 'FIELD3' ).
    cut->remove_field( 'FIELD4' ).

    check_expected_tab( CORRESPONDING tt_base_removed( base_tab ) ).
    check_expected_row( CORRESPONDING t_base_removed( base_row1 ) ).
    check_expected_row( CORRESPONDING t_base_removed( base_row2 ) ).
  ENDMETHOD.
ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
CLASS tcl_correct_keys DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      setup,
      set_uniq_sorted_key_2_fields FOR TESTING,
      set_hashed_key_1_field FOR TESTING.

    DATA:
      base_tab  TYPE tt_base,
      base_row1 TYPE t_base,
      base_row2 TYPE t_base,
      cut       TYPE REF TO zcl_ed_rtts_table.
ENDCLASS.

CLASS tcl_correct_keys IMPLEMENTATION.
  METHOD setup.
    base_row1 = VALUE #( field1 = 'BASE1 F1' field2 = 1 field3 = 'F3' field4 = '123456.789' ).
    base_row2 = VALUE #( field1 = 'BASE2 F2' field2 = 2 field3 = 'F3' field4 = '123456.789' ).
    base_tab = VALUE #( ( base_row1 ) ( base_row2 ) ).
    cut = zcl_ed_rtts_table=>create_from_table( base_tab ).
  ENDMETHOD.

  METHOD set_uniq_sorted_key_2_fields.
    cut->set_table_type( table_kind = cl_abap_tabledescr=>tablekind_sorted
        key = VALUE #( ( name = 'FIELD1' ) ( name = 'FIELD2' ) )
        key_kind = cl_abap_tabledescr=>keydefkind_user
        key_is_unique = abap_true ).

    DATA(act_tab) = cut->create_table( base_tab ).
    FIELD-SYMBOLS: <act_tab> TYPE ANY TABLE.
    ASSIGN act_tab->* TO <act_tab>.

    "Expect error when trying to insert
    INSERT base_row1 INTO TABLE <act_tab>.
    cl_abap_unit_assert=>assert_equals( act = CONV sy-subrc( sy-subrc ) exp = 4 ). "Trick to make copy of sy-subrc
  ENDMETHOD.

  METHOD set_hashed_key_1_field.
    cut->set_table_type( table_kind = cl_abap_tabledescr=>tablekind_hashed
        key = VALUE #( ( name = 'FIELD1' ) )
        key_kind = cl_abap_tabledescr=>keydefkind_user
        key_is_unique = abap_true ).

    DATA(act_tab) = cut->create_table( base_tab ).
    FIELD-SYMBOLS: <act_tab> TYPE ANY TABLE.
    ASSIGN act_tab->* TO <act_tab>.

    "Expect error when trying to insert
    INSERT base_row1 INTO TABLE <act_tab>.
    cl_abap_unit_assert=>assert_equals( act = CONV sy-subrc( sy-subrc ) exp = 4 ).
  ENDMETHOD.
ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
CLASS tcl_creation DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
      tt_base_sorted TYPE SORTED TABLE OF t_base WITH UNIQUE KEY field1 field2.

    METHODS:
      setup,
      from_field FOR TESTING,
      from_struct FOR TESTING.

    DATA:
      base_tab  TYPE tt_base,
      base_row1 TYPE t_base,
      base_row2 TYPE t_base,
      cut       TYPE REF TO zcl_ed_rtts_table.
ENDCLASS.

CLASS tcl_creation IMPLEMENTATION.
  METHOD setup.
    base_row1 = VALUE #( field1 = 'BASE1 F1' field2 = 1 field3 = 'F3' field4 = '123456.789' ).
    base_row2 = VALUE #( field1 = 'BASE2 F2' field2 = 2 field3 = 'F3' field4 = '123456.789' ).
    base_tab = VALUE #( ( base_row1 ) ( base_row2 ) ).
  ENDMETHOD.

  METHOD from_field.
    cut = zcl_ed_rtts_table=>create_from_field( VALUE #( name = 'FIELD1' data_element = 'TADIR-OBJECT' ) ).
    DATA(line_ref) = cut->create_line( ).
    ASSIGN line_ref->* TO FIELD-SYMBOL(<line>).

    ASSIGN COMPONENT 'FIELD1' OF STRUCTURE <line> TO FIELD-SYMBOL(<field1>).
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( |Expected FIELD1 not found| ).
    ENDIF.

    ASSIGN COMPONENT 2 OF STRUCTURE <line> TO FIELD-SYMBOL(<field2>).
    IF sy-subrc = 0.
      cl_abap_unit_assert=>fail( |Expected only one field| ).
    ENDIF.

    IF 1 = 2.
      CLEAR: <field1>, <field2>. "To silence ABAPcleaner unused variable.
    ENDIF.
  ENDMETHOD.

  METHOD from_struct.
    cut = zcl_ed_rtts_table=>create_from_row( base_row1 ).

    DATA(act_tab) = cut->create_table( base_tab ).
    ASSIGN act_tab->* TO FIELD-SYMBOL(<act_tab>).

    cl_abap_unit_assert=>assert_equals( act = <act_tab> exp = base_tab ).
  ENDMETHOD.
ENDCLASS.
