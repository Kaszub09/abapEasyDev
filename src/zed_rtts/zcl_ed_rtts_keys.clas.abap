"! <p class="shorttext synchronized" lang="en">RTTS with key differentiation</p>
"! <br/>TAGS: rtts; keys; struct; table
CLASS zcl_ed_rtts_keys DEFINITION PUBLIC FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      tt_fields     TYPE STANDARD TABLE OF fieldname WITH EMPTY KEY,
      tt_key_fields TYPE STANDARD TABLE OF fieldname WITH EMPTY KEY.

    CLASS-METHODS:
      create_from_ddic_table IMPORTING table TYPE tabname RETURNING VALUE(rtts_key) TYPE REF TO zcl_ed_rtts_keys,
      create_from_table IMPORTING table TYPE REF TO data key_fields TYPE tt_key_fields
                        RETURNING VALUE(rtts_key) TYPE REF TO zcl_ed_rtts_keys,
      create_from_struct IMPORTING struct TYPE REF TO data key_fields TYPE tt_key_fields
                        RETURNING VALUE(rtts_key) TYPE REF TO zcl_ed_rtts_keys.

    METHODS:
      get_descr IMPORTING additional_fields TYPE cl_abap_structdescr=>component_table OPTIONAL
                          include_mandt TYPE abap_bool DEFAULT abap_true
                          keys_only TYPE abap_bool DEFAULT abap_true
                EXPORTING struct TYPE REF TO cl_abap_structdescr table TYPE REF TO cl_abap_tabledescr,
      get_components IMPORTING include_mandt TYPE abap_bool DEFAULT abap_true
                               keys_only TYPE abap_bool DEFAULT abap_true
                     RETURNING VALUE(components) TYPE cl_abap_structdescr=>component_table.

    DATA:
        has_mandt TYPE abap_bool READ-ONLY.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_field_info,
        name   TYPE fieldname,
        type   TYPE REF TO cl_abap_datadescr,
        is_key TYPE abap_bool,
      END OF t_field_info,
      tt_field_info TYPE STANDARD TABLE OF t_field_info WITH EMPTY KEY
      WITH UNIQUE SORTED KEY name COMPONENTS name.

    CLASS-METHODS:
        create_from_structdescr IMPORTING struct TYPE REF TO cl_abap_structdescr key_fields TYPE tt_key_fields
                                  RETURNING VALUE(rtts_key) TYPE REF TO zcl_ed_rtts_keys.

    DATA:
        fields TYPE tt_field_info.
ENDCLASS.

CLASS zcl_ed_rtts_keys IMPLEMENTATION.
  METHOD create_from_ddic_table.
    DATA(struct) = CAST cl_abap_structdescr( cl_abap_tabledescr=>describe_by_name( table ) ).
    rtts_key = NEW #( ).
    rtts_key->fields = CORRESPONDING #( struct->get_components( ) ).

    DATA(ddic_fields) = CAST cl_abap_structdescr( cl_abap_tabledescr=>describe_by_name( table ) )->get_ddic_field_list( ).
    LOOP AT ddic_fields REFERENCE INTO DATA(field) WHERE keyflag = abap_true.
      rtts_key->fields[ KEY name name = field->fieldname ]-is_key = abap_true.
    ENDLOOP.

    rtts_key->has_mandt = xsdbool( rtts_key->fields[ 1 ]-name = 'MANDT' ).
  ENDMETHOD.

  METHOD create_from_table.
    rtts_key = create_from_structdescr(
        struct = CAST cl_abap_structdescr( CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( table ) )->get_table_line_type( ) )
        key_fields = key_fields ).
  ENDMETHOD.

  METHOD create_from_struct.
    rtts_key = create_from_structdescr( struct = CAST cl_abap_structdescr( cl_abap_tabledescr=>describe_by_data_ref( struct ) ) key_fields = key_fields ).
  ENDMETHOD.

  METHOD create_from_structdescr.
    rtts_key = NEW #( ).
    rtts_key->fields = CORRESPONDING #( struct->get_components( ) ).

    LOOP AT key_fields REFERENCE INTO DATA(field).
      rtts_key->fields[ KEY name name = field->* ]-is_key = abap_true.
    ENDLOOP.

    rtts_key->has_mandt = xsdbool( rtts_key->fields[ 1 ]-name = 'MANDT' ).
  ENDMETHOD.

  METHOD get_descr.
    DATA(components) = get_components( include_mandt = include_mandt keys_only = keys_only ).

    APPEND LINES OF additional_fields TO components.

    struct = cl_abap_structdescr=>get( p_components = components ).
    table = cl_abap_tabledescr=>get( p_line_type = struct p_key_kind = cl_abap_tabledescr=>keydefkind_default ).
  ENDMETHOD.

  METHOD get_components.
    DATA(start_index) = COND i( WHEN has_mandt = abap_true OR include_mandt = abap_false THEN 2 ELSE 1 ).
    LOOP AT fields REFERENCE INTO DATA(field) FROM start_index.
      IF keys_only = abap_true AND field->is_key = abap_false.
        CONTINUE.
      ENDIF.
      APPEND CORRESPONDING #( field->* ) TO components.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
