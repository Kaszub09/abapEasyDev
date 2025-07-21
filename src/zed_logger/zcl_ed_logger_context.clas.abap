CLASS zcl_ed_logger_context DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ed_logger_factory.

  PUBLIC SECTION.
    CONSTANTS:
      c_value_delimiter TYPE c LENGTH 1 VALUE '|',
      BEGIN OF c_col_info_delimiter,
        component TYPE c LENGTH 1 VALUE '|',
        name_type TYPE c LENGTH 1 VALUE '&',
      END OF c_col_info_delimiter.

    CLASS-METHODS:
      "! @parameter sample_data | <p class="shorttext synchronized" lang="en">Must be simple structure with dictionary data elements.</p>
      create_from_ref IMPORTING sample_data    TYPE REF TO data OPTIONAL
                      RETURNING VALUE(context) TYPE REF TO zcl_ed_logger_context
                      RAISING   zcx_ed_exception,
      create_from_col_info IMPORTING col_info       TYPE zted_log_context_col_info
                           RETURNING VALUE(context) TYPE REF TO zcl_ed_logger_context
                           RAISING   zcx_ed_exception,
      create_empty   RETURNING VALUE(context) TYPE REF TO zcl_ed_logger_context.

    METHODS:
      exists RETURNING VALUE(exists) TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Data is delimited by <em>c_value_delimiter</em>.
      "! <br/>Structure can have only some of fields, and rest are assumed to be empty.</p>
      convert_data_to_string IMPORTING context_data TYPE REF TO data RETURNING VALUE(context_string) TYPE string RAISING zcx_ed_logger_no_context,
      "! <p class="shorttext synchronized" lang="en">Input data should be delimited by <em>c_value_delimiter</em>.</p>
      convert_string_to_data IMPORTING context_string TYPE string RETURNING VALUE(context_data) TYPE REF TO data RAISING zcx_ed_logger_no_context,
      get_struct_info RETURNING VALUE(struct_info) TYPE REF TO cl_abap_structdescr RAISING zcx_ed_logger_no_context,
      "! <p class="shorttext synchronized" lang="en">Can be always safely called.</p>
      get_struct_components RETURNING VALUE(components) TYPE cl_abap_structdescr=>component_table,
      "! <p class="shorttext synchronized" lang="en">Fields are delimited by <em>c_col_info_delimiter-component</em>,
      "! names and types by <em>c_col_info_delimiter-name_type</em>.</p>
      get_context_col_info RETURNING VALUE(col_info) TYPE zted_log_context_col_info.

  PRIVATE SECTION.
    METHODS:
      raise_err_if_no_context RAISING zcx_ed_logger_no_context.

    DATA:
      context_exists TYPE abap_bool,
      struct         TYPE REF TO cl_abap_structdescr.
ENDCLASS.

CLASS zcl_ed_logger_context IMPLEMENTATION.
  METHOD exists.
    exists = context_exists.
  ENDMETHOD.

  METHOD convert_data_to_string.
    raise_err_if_no_context( ).

    ASSIGN context_data->* TO FIELD-SYMBOL(<context_data>).
    LOOP AT struct->components REFERENCE INTO DATA(comp).
      ASSIGN COMPONENT comp->name OF STRUCTURE <context_data> TO FIELD-SYMBOL(<field>).
      IF sy-subrc <> 0.
        "^Assume non-existing fields are empty instead of throwing unassigned error.
        ASSIGN space TO <field>.
      ENDIF.
      context_string = |{ context_string }{ <field> }{ c_value_delimiter }|.
    ENDLOOP.

    IF strlen( context_string ) > 0. "Remove trailing delimiter
      context_string = substring( val = context_string off = 0 len = strlen( context_string ) - 1 ).
    ENDIF.
  ENDMETHOD.

  METHOD convert_string_to_data.
    raise_err_if_no_context( ).

    CREATE DATA context_data TYPE HANDLE struct.
    ASSIGN context_data->* TO FIELD-SYMBOL(<context_data>).

    SPLIT context_string AT c_value_delimiter INTO TABLE DATA(fields).
    LOOP AT fields REFERENCE INTO DATA(field).
      ASSIGN COMPONENT sy-tabix OF STRUCTURE <context_data> TO FIELD-SYMBOL(<field>).
      <field> = field->*.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_struct_info.
    raise_err_if_no_context( ).
    struct_info = struct.
  ENDMETHOD.

  METHOD get_struct_components.
    raise_err_if_no_context( ).
    components = struct->get_components( ).
  ENDMETHOD.

  METHOD get_context_col_info.
    IF NOT exists( ).
      RETURN.
    ENDIF.

    LOOP AT struct->get_components( ) REFERENCE INTO DATA(comp).
      col_info = |{ col_info }{ comp->name }{ c_col_info_delimiter-name_type }{
        CAST cl_abap_typedescr( comp->type )->absolute_name }{ c_col_info_delimiter-component }|.
    ENDLOOP.

    IF strlen( col_info ) > 0. "Remove trailing delimiter
      col_info = substring( val = col_info off = 0 len = strlen( col_info ) - 1 ).
    ENDIF.
  ENDMETHOD.

  METHOD raise_err_if_no_context.
    IF NOT exists( ).
      RAISE EXCEPTION TYPE zcx_ed_logger_no_context.
    ENDIF.
  ENDMETHOD.

  METHOD create_from_col_info.
    context = NEW #( ).
    context->context_exists = abap_false.

    IF strlen( col_info ) = 0.
      RETURN.
    ENDIF.

    context->context_exists = abap_true.
    DATA(components) = VALUE cl_abap_structdescr=>component_table( ).

    SPLIT col_info AT c_col_info_delimiter-component INTO TABLE DATA(columns_info).
    LOOP AT columns_info REFERENCE INTO DATA(info).
      SPLIT info->* AT c_col_info_delimiter-name_type INTO TABLE DATA(col).
      IF lines( col ) <> 2.
        zcl_ed_msg=>throw( text = TEXT-e01 v1 = c_col_info_delimiter-name_type v2 = info->* ).
      ENDIF.

      cl_abap_elemdescr=>describe_by_name( EXPORTING p_name = col[ 2 ] RECEIVING p_descr_ref = DATA(descr) EXCEPTIONS type_not_found = 1 ).
      IF sy-subrc = 1.
        zcl_ed_msg=>throw( text = TEXT-e02 v1 = col[ 2 ] ).
      ENDIF.

      APPEND VALUE #( name = col[ 1 ] type = CAST #( descr ) ) TO components.
    ENDLOOP.
    context->struct = cl_abap_structdescr=>get( components ).
  ENDMETHOD.

  METHOD create_from_ref.
    DATA(descr) = cl_abap_structdescr=>describe_by_data_ref( sample_data ).
    IF descr IS NOT INSTANCE OF cl_abap_structdescr.
      zcl_ed_msg=>throw( text = TEXT-e03 ).
    ENDIF.

    context = NEW #( ).
    context->context_exists = abap_true.
    context->struct = CAST #( descr ).
  ENDMETHOD.

  METHOD create_empty.
    context = NEW #( ).
    context->context_exists = abap_false.
  ENDMETHOD.

ENDCLASS.
