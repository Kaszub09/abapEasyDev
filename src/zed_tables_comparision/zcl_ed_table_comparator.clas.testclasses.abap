
CLASS ltcl_ed_table_comparator DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_table,
        mandt  TYPE mandt,
        key1   TYPE c LENGTH 10,
        key2   TYPE decfloat34,
        key3   TYPE c LENGTH 3,
        field1 TYPE c LENGTH 20,
        field2 TYPE i,
        field3 TYPE c LENGTH 4,
      END OF t_table,
      tt_table TYPE STANDARD TABLE OF t_table WITH EMPTY KEY.

    METHODS:
      compare_two_tables FOR TESTING,
      compare_leftover_code FOR TESTING,
      compare_leftover_code2 FOR TESTING,
      update_mandt FOR TESTING.
ENDCLASS.
"zcl_ed_table_comparator

CLASS ltcl_ed_table_comparator IMPLEMENTATION.

  METHOD compare_two_tables.
    "--------------------------------------------------
    DATA(before) = VALUE tt_table(
        ( key1 = 'DEL' )
        ( key1 = 'MOD1'  key3 = 'X' field2 = 12 )
        ( key1 = 'DEL2' )
        ( key1 = 'NO_CHANGE' )
        ( key1 = 'DUPL' key2 = '22' )
        ( mandt = '001' key1 = 'MOD2' key2 = '1.1' key3 = 'X' field2 = 12 )
        ( key1 = 'ZZDEL' )
        ( key1 = 'ZZDEL2' )
    ).
    "--------------------------------------------------
    DATA(after) = VALUE tt_table(
        ( key1 = 'INS' )
        ( key1 = 'MOD2' key2 = '1.1' key3 = 'X' field2 = 0 field3 = 'A' )
        ( key1 = 'DUPL' key2 = '22' )
        ( key1 = 'MOD1' key3 = 'X' field2 = 1 )
        ( key1 = 'INS' key2 = '1.2' )
        ( key1 = 'NO_CHANGE' )
        ( key1 = 'DUPL' key2 = '22' )
    ).
    "--------------------------------------------------
    DATA(before_modified) = VALUE tt_table(
        ( key1 = 'MOD1'  key3 = 'X' field2 = 12 )
        ( mandt = '001' key1 = 'MOD2' key2 = '1.1' key3 = 'X' field2 = 12 )
    ).
    "--------------------------------------------------
    DATA(modified) = VALUE tt_table(
        ( key1 = 'MOD1' key3 = 'X' field2 = 1 )
        ( key1 = 'MOD2' key2 = '1.1' key3 = 'X' field2 = 0 field3 = 'A' )
    ).
    "--------------------------------------------------
    DATA(deleted) = VALUE tt_table(
        ( key1 = 'DEL' )
        ( key1 = 'DEL2' )
        ( key1 = 'ZZDEL' )
        ( key1 = 'ZZDEL2' )
    ).
    "--------------------------------------------------
    DATA(inserted) = VALUE tt_table(
        ( key1 = 'DUPL' key2 = '22' )
        ( key1 = 'INS' )
        ( key1 = 'INS' key2 = '1.2' )
    ).
    "--------------------------------------------------
    DATA(duplicates) = VALUE tt_table(
        ( key1 = 'DUPL' key2 = '22' )
    ).
    "=================================================================
    DATA(result) = zcl_ed_table_comparator=>get_comparision_data( zcl_ed_table_comparator=>compare_tables(
      EXPORTING
        initial_data    = REF #( before )
        modified_data   = REF #( after )
        rtts_keys       = zcl_ed_rtts_keys=>create_from_table( table = REF #( before )  key_fields = VALUE #( ( 'KEY1' ) ( 'KEY2' ) ( 'KEY3' ) ) )
     ) ).

    ASSIGN result-duplicates->* TO FIELD-SYMBOL(<duplicates>).
    ASSIGN result-inserted->* TO FIELD-SYMBOL(<inserted>).
    ASSIGN result-deleted->* TO FIELD-SYMBOL(<deleted>).
    ASSIGN result-before_modified->* TO FIELD-SYMBOL(<before_modified>).
    ASSIGN result-modified->* TO FIELD-SYMBOL(<modified>).
    "=================================================================
    cl_abap_unit_assert=>assert_equals( act = <duplicates> exp = duplicates msg = |Wrong duplicates table| ).
    cl_abap_unit_assert=>assert_equals( act = <inserted> exp = inserted msg = |Wrong inserted table| ).
    cl_abap_unit_assert=>assert_equals( act = <deleted> exp = deleted msg = |Wrong deleted table| ).
    cl_abap_unit_assert=>assert_equals( act = <before_modified> exp = before_modified msg = |Wrong before_modified table| ).
    cl_abap_unit_assert=>assert_equals( act = <modified> exp = modified msg = |Wrong modified table| ).
  ENDMETHOD.

  METHOD update_mandt.
    DATA(table) = VALUE tt_table(
        ( mandt = '123' key1 = 'DEL' )
        ( mandt = '999' key1 = 'DEL2' )
        ( key1 = 'NO_CHANGE' )
    ).
    "--------------------------------------------------
    DATA(after) = VALUE tt_table(
        ( mandt = sy-mandt key1 = 'DEL' )
        ( mandt = sy-mandt key1 = 'DEL2' )
        ( mandt = sy-mandt key1 = 'NO_CHANGE' )
    ).
    "=================================================================
    zcl_ed_table_comparator=>update_mandant( REF #( table ) ).
    "=================================================================
    cl_abap_unit_assert=>assert_equals( act = table exp = after ).
  ENDMETHOD.

  METHOD compare_leftover_code.
    "--------------------------------------------------
    DATA(before) = VALUE tt_table(
        ( key1 = 'DEL' )
        ( key1 = 'DEL2' )
        ( key1 = 'DEL3' )
    ).
    "--------------------------------------------------
    DATA(after) = VALUE tt_table(
        ( key1 = 'INS' )
        ( key1 = 'INS2' )
        ( key1 = 'INS2' )
    ).
    "--------------------------------------------------
    DATA(before_modified) = VALUE tt_table(
    ).
    "--------------------------------------------------
    DATA(modified) = VALUE tt_table(
    ).
    "--------------------------------------------------
    DATA(deleted) = VALUE tt_table(
        ( key1 = 'DEL' )
        ( key1 = 'DEL2' )
        ( key1 = 'DEL3' )
    ).
    "--------------------------------------------------
    DATA(inserted) = VALUE tt_table(
        ( key1 = 'INS' )
        ( key1 = 'INS2' )
        ( key1 = 'INS2' )
    ).
    "--------------------------------------------------
    DATA(duplicates) = VALUE tt_table(
        ( key1 = 'INS2' )
    ).
    "=================================================================
    DATA(result) = zcl_ed_table_comparator=>get_comparision_data( zcl_ed_table_comparator=>compare_tables(
      EXPORTING
        initial_data    = REF #( before )
        modified_data   = REF #( after )
        rtts_keys       = zcl_ed_rtts_keys=>create_from_table( table = REF #( before )  key_fields = VALUE #( ( 'KEY1' ) ( 'KEY2' ) ( 'KEY3' ) ) )
     ) ).

    ASSIGN result-duplicates->* TO FIELD-SYMBOL(<duplicates>).
    ASSIGN result-inserted->* TO FIELD-SYMBOL(<inserted>).
    ASSIGN result-deleted->* TO FIELD-SYMBOL(<deleted>).
    ASSIGN result-before_modified->* TO FIELD-SYMBOL(<before_modified>).
    ASSIGN result-modified->* TO FIELD-SYMBOL(<modified>).
    "=================================================================
    cl_abap_unit_assert=>assert_equals( act = <duplicates> exp = duplicates msg = |Wrong duplicates table| ).
    cl_abap_unit_assert=>assert_equals( act = <inserted> exp = inserted msg = |Wrong inserted table| ).
    cl_abap_unit_assert=>assert_equals( act = <deleted> exp = deleted msg = |Wrong deleted table| ).
    cl_abap_unit_assert=>assert_equals( act = <before_modified> exp = before_modified msg = |Wrong before_modified table| ).
    cl_abap_unit_assert=>assert_equals( act = <modified> exp = modified msg = |Wrong modified table| ).
  ENDMETHOD.

  METHOD compare_leftover_code2.
    "--------------------------------------------------
    DATA(before) = VALUE tt_table(
        ( key1 = 'ZZ' )
    ).
    "--------------------------------------------------
    DATA(after) = VALUE tt_table(
        ( key1 = 'AA' )
    ).
    "--------------------------------------------------
    DATA(before_modified) = VALUE tt_table(
    ).
    "--------------------------------------------------
    DATA(modified) = VALUE tt_table(
    ).
    "--------------------------------------------------
    DATA(deleted) = VALUE tt_table(
        ( key1 = 'ZZ' )
    ).
    "--------------------------------------------------
    DATA(inserted) = VALUE tt_table(
        ( key1 = 'AA' )
    ).
    "--------------------------------------------------
    DATA(duplicates) = VALUE tt_table(
    ).
    "=================================================================
    DATA(result) = zcl_ed_table_comparator=>get_comparision_data( zcl_ed_table_comparator=>compare_tables(
      EXPORTING
        initial_data    = REF #( before )
        modified_data   = REF #( after )
        rtts_keys       = zcl_ed_rtts_keys=>create_from_table( table = REF #( before )  key_fields = VALUE #( ( 'KEY1' ) ( 'KEY2' ) ( 'KEY3' ) ) )
     ) ).

    ASSIGN result-duplicates->* TO FIELD-SYMBOL(<duplicates>).
    ASSIGN result-inserted->* TO FIELD-SYMBOL(<inserted>).
    ASSIGN result-deleted->* TO FIELD-SYMBOL(<deleted>).
    ASSIGN result-before_modified->* TO FIELD-SYMBOL(<before_modified>).
    ASSIGN result-modified->* TO FIELD-SYMBOL(<modified>).
    "=================================================================
    cl_abap_unit_assert=>assert_equals( act = <duplicates> exp = duplicates msg = |Wrong duplicates table| ).
    cl_abap_unit_assert=>assert_equals( act = <inserted> exp = inserted msg = |Wrong inserted table| ).
    cl_abap_unit_assert=>assert_equals( act = <deleted> exp = deleted msg = |Wrong deleted table| ).
    cl_abap_unit_assert=>assert_equals( act = <before_modified> exp = before_modified msg = |Wrong before_modified table| ).
    cl_abap_unit_assert=>assert_equals( act = <modified> exp = modified msg = |Wrong modified table| ).
  ENDMETHOD.

ENDCLASS.
