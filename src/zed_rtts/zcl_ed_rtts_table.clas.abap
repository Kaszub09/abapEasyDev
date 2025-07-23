"! <p class="shorttext synchronized">Helper for table creation</p>
"! <br/>TAGS: rtts; struct; table; dynamic; descr
CLASS zcl_ed_rtts_table DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_field_de,
        name         TYPE fieldname,
        data_element TYPE rollname,
      END OF t_field_de,
      tt_field_de TYPE STANDARD TABLE OF t_field_de WITH EMPTY KEY,
      BEGIN OF t_field_descr,
        name TYPE fieldname,
        type TYPE REF TO cl_abap_datadescr,
      END OF t_field_descr,
      tt_field_descr TYPE STANDARD TABLE OF t_field_descr WITH EMPTY KEY.

    CLASS-METHODS:
      create_from_field IMPORTING field TYPE t_field_de RETURNING VALUE(rtts_table) TYPE REF TO zcl_ed_rtts_table,
      "! <p class="shorttext synchronized" lang="en">Primary key is taken from table if exists.</p>
      create_from_table IMPORTING table TYPE any RETURNING VALUE(rtts_table) TYPE REF TO zcl_ed_rtts_table,
      create_from_row IMPORTING row TYPE any RETURNING VALUE(rtts_table) TYPE REF TO zcl_ed_rtts_table.

    METHODS:
      set_table_type IMPORTING table_kind    TYPE abap_tablekind DEFAULT cl_abap_tabledescr=>tablekind_std
                               key           TYPE abap_keydescr_tab OPTIONAL
                               key_kind      TYPE abap_keydefkind DEFAULT cl_abap_tabledescr=>keydefkind_default
                               key_is_unique TYPE abap_bool DEFAULT abap_false,
      add_field IMPORTING field TYPE t_field_de,
      add_fields IMPORTING fields TYPE tt_field_de,
      add_field_by_type IMPORTING field TYPE t_field_descr,
      add_fields_by_type IMPORTING fields TYPE tt_field_descr,
      remove_field IMPORTING name TYPE fieldname,
      create_table IMPORTING base TYPE any OPTIONAL RETURNING VALUE(table_ref) TYPE REF TO data,
      create_line IMPORTING base TYPE any OPTIONAL RETURNING VALUE(line_ref) TYPE REF TO data.

  PRIVATE SECTION.
    METHODS:
      refresh_from_components IMPORTING components TYPE cl_abap_structdescr=>component_table.

    DATA:
      table_descr   TYPE REF TO cl_abap_tabledescr,
      line_descr    TYPE REF TO cl_abap_structdescr,
      table_kind    TYPE abap_tablekind VALUE cl_abap_tabledescr=>tablekind_std,
      key           TYPE abap_keydescr_tab,
      key_kind      TYPE abap_keydefkind VALUE cl_abap_tabledescr=>keydefkind_default,
      key_is_unique TYPE abap_bool VALUE abap_false.
ENDCLASS.

CLASS zcl_ed_rtts_table IMPLEMENTATION.
  METHOD create_from_field.
    rtts_table = NEW #( ).
    rtts_table->line_descr = CAST #( cl_abap_structdescr=>get( VALUE #(
        ( name = field-name type = CAST #( cl_abap_datadescr=>describe_by_name( field-data_element ) ) ) ) ) ).
    rtts_table->table_descr = cl_abap_tabledescr=>get( rtts_table->line_descr ).
  ENDMETHOD.

  METHOD create_from_table.
    rtts_table = NEW #( ).
    rtts_table->table_descr = CAST #( cl_abap_tabledescr=>describe_by_data( table ) ).
    LOOP AT rtts_table->table_descr->get_keys( ) REFERENCE INTO DATA(key) WHERE is_primary = abap_true.
      rtts_table->key = key->components.
      rtts_table->key_kind = key->key_kind.
      rtts_table->key_is_unique = key->is_unique.
    ENDLOOP.
    rtts_table->table_kind = rtts_table->table_descr->table_kind.
    rtts_table->line_descr = CAST #( rtts_table->table_descr->get_table_line_type( ) ).
  ENDMETHOD.

  METHOD create_from_row.
    rtts_table = NEW #( ).
    rtts_table->line_descr = CAST #( cl_abap_tabledescr=>describe_by_data( row ) ).
    rtts_table->table_descr = cl_abap_tabledescr=>get( rtts_table->line_descr ).
  ENDMETHOD.

  METHOD set_table_type.
    me->table_kind = table_kind.
    me->key_kind = key_kind.
    me->key = key.
    me->key_is_unique = key_is_unique.
    refresh_from_components( line_descr->get_components( ) ).
  ENDMETHOD.

  METHOD add_field.
    DATA(components) = line_descr->get_components( ).
    APPEND VALUE #( name = field-name type = CAST #( cl_abap_datadescr=>describe_by_name( field-data_element ) ) ) TO components.
    refresh_from_components( components ).
  ENDMETHOD.

  METHOD add_fields.
    DATA(components) = line_descr->get_components( ).
    LOOP AT fields REFERENCE INTO DATA(field).
      APPEND VALUE #( name = field->name type = CAST #( cl_abap_datadescr=>describe_by_name( field->data_element ) ) ) TO components.
    ENDLOOP.
    refresh_from_components( components ).
  ENDMETHOD.

  METHOD add_field_by_type.
    DATA(components) = line_descr->get_components( ).
    APPEND VALUE #( name = field-name type = field-type ) TO components.
    refresh_from_components( components ).
  ENDMETHOD.

  METHOD add_fields_by_type.
    DATA(components) = line_descr->get_components( ).
    LOOP AT fields REFERENCE INTO DATA(field).
      APPEND VALUE #( name = field->name type = field->type ) TO components.
    ENDLOOP.
    refresh_from_components( components ).
  ENDMETHOD.

  METHOD remove_field.
    DATA(components) = line_descr->get_components( ).
    DELETE components WHERE name = name.
    refresh_from_components( components ).
  ENDMETHOD.

  METHOD create_table.
    CREATE DATA table_ref TYPE HANDLE table_descr.
    IF base IS NOT INITIAL.
      ASSIGN table_ref->* TO FIELD-SYMBOL(<table>).
      <table> = CORRESPONDING #( base ).
    ENDIF.
  ENDMETHOD.

  METHOD create_line.
    CREATE DATA line_ref TYPE HANDLE line_descr.
    IF base IS NOT INITIAL.
      ASSIGN line_ref->* TO FIELD-SYMBOL(<line>).
      <line> = CORRESPONDING #( base ).
    ENDIF.
  ENDMETHOD.

  METHOD refresh_from_components.
    line_descr = cl_abap_structdescr=>get( components ).
    IF lines( key ) = 0.
      table_descr = cl_abap_tabledescr=>get( p_line_type = line_descr ).
    ELSE.
      table_descr = cl_abap_tabledescr=>get( p_line_type = line_descr
        p_table_kind = table_kind p_unique = key_is_unique p_key = key p_key_kind = key_kind ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
