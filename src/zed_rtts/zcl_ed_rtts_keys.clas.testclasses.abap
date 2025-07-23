TYPES:
  BEGIN OF t_test_structure,
    field1 TYPE i,
    field2 TYPE string,
    field3 TYPE d,
  END OF t_test_structure,
  BEGIN OF t_expected_fields,
    name   TYPE fieldname,
    is_key TYPE abap_bool,
  END OF t_expected_fields,
  tt_expected_fields TYPE STANDARD TABLE OF t_expected_fields WITH EMPTY KEY WITH NON-UNIQUE SORTED KEY is_key COMPONENTS is_key.
"=================================================================
"-----------------------------------------------------------------
CLASS ltcl_detect_key_fields DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      detect_from_ddic_table_tadir FOR TESTING,
      detect_from_structure FOR TESTING,
      test_detection IMPORTING fields TYPE tt_expected_fields rtts TYPE REF TO zcl_ed_rtts_keys mandt_param TYPE abap_bool.
ENDCLASS.

CLASS ltcl_detect_key_fields IMPLEMENTATION.
  METHOD detect_from_ddic_table_tadir.
    DATA(expected_fields) = CORRESPONDING tt_expected_fields( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( 'TADIR' ) )->get_components( ) ).
    LOOP AT expected_fields REFERENCE INTO DATA(field) WHERE name = 'PGMID' OR name = 'OBJECT' OR name = 'OBJ_NAME'.
      field->is_key = abap_true.
    ENDLOOP.

    test_detection( fields = expected_fields rtts = zcl_ed_rtts_keys=>create_from_ddic_table( 'TADIR' ) mandt_param = abap_true ).
    test_detection( fields = expected_fields rtts = zcl_ed_rtts_keys=>create_from_ddic_table( 'TADIR' ) mandt_param = abap_false ).
  ENDMETHOD.

  METHOD detect_from_structure.
    DATA(expected_fields) = VALUE tt_expected_fields( ( name = 'FIELD1' is_key = abap_true ) ( name = 'FIELD2'  )  ( name = 'FIELD3'  ) ).
    DATA test_struct TYPE t_test_structure.
    test_detection( fields = expected_fields rtts = zcl_ed_rtts_keys=>create_from_struct( struct = REF #( test_struct )
        key_fields = VALUE #( FOR key_field IN expected_fields USING KEY is_key WHERE ( is_key = abap_true ) ( key_field-name ) ) )
        mandt_param = abap_true ).
    test_detection( fields = expected_fields rtts = zcl_ed_rtts_keys=>create_from_struct( struct = REF #( test_struct )
        key_fields = VALUE #( FOR key_field IN expected_fields USING KEY is_key WHERE ( is_key = abap_true ) ( key_field-name ) ) )
        mandt_param = abap_false ).
  ENDMETHOD.

  METHOD test_detection.
    "Key fields
    DATA(keys) = rtts->get_components( include_mandt = mandt_param keys_only = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = lines( keys ) exp = lines( FILTER #( fields USING KEY is_key WHERE is_key = abap_true ) )
        msg = |Key fields number doesn't match| ).
    LOOP AT fields REFERENCE INTO DATA(field) USING KEY is_key WHERE is_key = abap_true.
      cl_abap_unit_assert=>assert_true( act = xsdbool( line_exists( keys[ name = field->name ] ) )
        msg = |Expected { field->name } key field in TADIR| ).
    ENDLOOP.

    "Non key fields
    DATA(all_fields) = rtts->get_components( include_mandt = mandt_param keys_only = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lines( all_fields ) exp = lines( fields )
        msg = |Key fields number doesn't match| ).
    LOOP AT fields REFERENCE INTO field.
      cl_abap_unit_assert=>assert_true( act = xsdbool( line_exists( all_fields[ name = field->name ] ) )
        msg = |Expected { field->name } field in TADIR| ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

"=================================================================
"-----------------------------------------------------------------

CLASS ltcl_correct_struct_created DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      create_key_table FOR TESTING,
      create_table_with_add_fields FOR TESTING.
ENDCLASS.

CLASS ltcl_correct_struct_created IMPLEMENTATION.
  METHOD create_key_table.
    TYPES:
      BEGIN OF t_test_table,
        field1 TYPE i,
      END OF t_test_table.
    DATA test_table TYPE STANDARD TABLE OF t_test_table WITH EMPTY KEY.

    APPEND VALUE #( ) TO test_table.

    "Create key table from RTTS
    DATA test_table_for_creation TYPE STANDARD TABLE OF t_test_structure WITH EMPTY KEY.
    DATA(rtts) = zcl_ed_rtts_keys=>create_from_table( table = REF #( test_table_for_creation ) key_fields = VALUE #( ( 'FIELD1' ) ) ).
    rtts->get_descr( EXPORTING keys_only = abap_true IMPORTING struct = DATA(struct) table = DATA(table) ).

    DATA key_table TYPE REF TO data.
    CREATE DATA key_table TYPE HANDLE table.
    FIELD-SYMBOLS <key_table> TYPE INDEX TABLE.
    ASSIGN key_table->* TO <key_table>.

    "This line would fail if <key_table> had more fields than test_table
    APPEND LINES OF test_table TO <key_table>.
  ENDMETHOD.

  METHOD create_table_with_add_fields.
    TYPES:
      BEGIN OF t_test_table,
        field1 TYPE i,
        field2 TYPE string,
        field3 TYPE d,
        field4 TYPE abap_bool,
      END OF t_test_table.
    DATA test_table TYPE STANDARD TABLE OF t_test_table WITH EMPTY KEY.

    APPEND VALUE #( ) TO test_table.

    "Create key table from RTTS
    DATA test_struct TYPE t_test_structure.
    DATA(rtts) = zcl_ed_rtts_keys=>create_from_struct(  struct = REF #( test_struct ) key_fields = VALUE #( ( 'FIELD1' ) ) ).
    rtts->get_descr( EXPORTING keys_only = abap_false additional_fields = VALUE #( ( name = 'FIELD4'
            type = CAST #( cl_abap_datadescr=>describe_by_data( abap_true ) ) ) )
        IMPORTING struct = DATA(struct) table = DATA(table) ).

    DATA add_fields_table TYPE REF TO data.
    CREATE DATA add_fields_table TYPE HANDLE table.
    FIELD-SYMBOLS <add_fields_table> TYPE INDEX TABLE.
    ASSIGN add_fields_table->* TO <add_fields_table>.

    "This line would fail if <add_fields_table> had less fields than test_table
    APPEND LINES OF test_table TO <add_fields_table>.
  ENDMETHOD.
ENDCLASS.
"=================================================================
"-----------------------------------------------------------------

CLASS ltcl_detect_mandt_field DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      componenets_with_mandt FOR TESTING,
      componenets_without_mandt FOR TESTING.
ENDCLASS.

CLASS ltcl_detect_mandt_field IMPLEMENTATION.
  METHOD componenets_with_mandt.
    DATA(rtts) = zcl_ed_rtts_keys=>create_from_ddic_table( 'T000' ).
    DATA(components) = rtts->get_components( include_mandt = abap_true keys_only = abap_true ).

    cl_abap_unit_assert=>assert_true( act = xsdbool( line_exists( components[ name = 'MANDT' ] ) )
      msg = |Expected MANDT field in T000| ).
    cl_abap_unit_assert=>assert_equals( act = lines( components ) exp = 1 msg = |Expected 1 field in components| ).
  ENDMETHOD.

  METHOD componenets_without_mandt.
    DATA(rtts) = zcl_ed_rtts_keys=>create_from_ddic_table( 'T000' ).
    DATA(components) = rtts->get_components( include_mandt = abap_false keys_only = abap_true ).

    cl_abap_unit_assert=>assert_true( act = xsdbool( NOT line_exists( components[ name = 'MANDT' ] ) )
      msg = |Expected no MANDT field in T000| ).
    cl_abap_unit_assert=>assert_equals( act = lines( components ) exp = 0 msg = |Expected 0 fields in components| ).
  ENDMETHOD.
ENDCLASS.
