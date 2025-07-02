CLASS zcl_ed_logger_context DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ed_logger_factory.

  PUBLIC SECTION.
    METHODS:
      exists RETURNING VALUE(exists) TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Can't be called if context doesn't exists</p>
      convert_data_to_string IMPORTING context_data TYPE REF TO data RETURNING VALUE(context_string) TYPE string,
      "! <p class="shorttext synchronized" lang="en">Can't be called if context doesn't exists</p>
      convert_string_to_data IMPORTING context_string TYPE string RETURNING VALUE(context_data) TYPE REF TO data,
      "! <p class="shorttext synchronized" lang="en">Can't be called if context doesn't exists</p>
      get_struct_info RETURNING VALUE(struct_info) TYPE REF TO cl_abap_structdescr,
      "! <p class="shorttext synchronized" lang="en">Can be always safely called</p>
      get_struct_components RETURNING VALUE(components) TYPE cl_abap_structdescr=>component_table,
      "! <p class="shorttext synchronized" lang="en">Can be always safely called</p>
      get_context_col_info RETURNING VALUE(col_info) TYPE zted_log_context_col_info.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      context_exists TYPE abap_bool,
      struct         TYPE REF TO cl_abap_structdescr.
ENDCLASS.

CLASS zcl_ed_logger_context IMPLEMENTATION.
  METHOD exists.
    exists = context_exists.
  ENDMETHOD.

  METHOD convert_data_to_string.
    ASSIGN context_data->* TO FIELD-SYMBOL(<context_data>).
    LOOP AT struct->components REFERENCE INTO DATA(comp).
      ASSIGN COMPONENT comp->name OF STRUCTURE <context_data> TO FIELD-SYMBOL(<field>).
      context_string = |{ context_string }{ <field> }\||.
    ENDLOOP.

    IF strlen( context_string ) > 0.
      context_string = substring( val = context_string off = 0 len = strlen( context_string ) - 1 ).
    ENDIF.
  ENDMETHOD.

  METHOD convert_string_to_data.
    CREATE DATA context_data TYPE HANDLE struct.
    ASSIGN context_data->* TO FIELD-SYMBOL(<context_data>).

    SPLIT context_string AT |\|| INTO TABLE DATA(fields).
    LOOP AT fields REFERENCE INTO DATA(field).
      ASSIGN COMPONENT sy-tabix OF STRUCTURE <context_data> TO FIELD-SYMBOL(<field>).
      <field> = field->*.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_struct_info.
    struct_info = struct.
  ENDMETHOD.

  METHOD get_struct_components.
    IF struct IS NOT BOUND.
      RETURN.
    ENDIF.
    components = struct->get_components( ).
  ENDMETHOD.

  METHOD get_context_col_info.
    IF struct IS NOT BOUND.
      RETURN.
    ENDIF.
    LOOP AT struct->get_components( ) REFERENCE INTO DATA(comp).
      col_info = |{ col_info }{ comp->name }-{ CAST cl_abap_typedescr( comp->type )->get_relative_name( ) }\||.
    ENDLOOP.

    IF strlen( col_info ) > 0.
      col_info = substring( val = col_info off = 0 len = strlen( col_info ) - 1 ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
