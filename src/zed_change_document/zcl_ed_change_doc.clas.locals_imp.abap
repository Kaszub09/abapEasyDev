"=================================================================
"-----------------------------------------------------------------
CLASS lcl_force_cd_marker IMPLEMENTATION.
  METHOD force_cd_if_needed.
    DATA(force_logging) = abap_false.
    IMPORT force_logging = force_logging FROM MEMORY ID c_memory_id.
    IF sy-subrc <> 0 OR force_logging = abap_false.
      RETURN.
    ENDIF.

    DATA table_name TYPE tabname.
    IMPORT table_name = table_name FROM MEMORY ID c_memory_id.
    LOOP AT tabinfo ASSIGNING FIELD-SYMBOL(<row>) WHERE ('tabname = table_name AND logflag = abap_false AND keyflag = abap_false').
      ASSIGN COMPONENT 'LOGFLAG' OF STRUCTURE <row> TO FIELD-SYMBOL(<logflag>).
      <logflag> = 'F'.
    ENDLOOP.
  ENDMETHOD.

  METHOD clear_force_cd.
    IF force_marker = abap_false.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS: <tabinfo> TYPE table.
    ASSIGN ('(SAPLSCD0)TABINFO') TO <tabinfo>.
    IF sy-subrc = 0.
      LOOP AT <tabinfo> ASSIGNING FIELD-SYMBOL(<tabinfo_row>) WHERE ('tabname = table_name AND logflag = ''F''').
        ASSIGN COMPONENT 'LOGFLAG' OF STRUCTURE <tabinfo_row> TO FIELD-SYMBOL(<logflag>).
        CLEAR: <logflag>.
      ENDLOOP.
    ENDIF.

    EXPORT force_logging = abap_false table_name = table_name TO MEMORY ID c_memory_id.
  ENDMETHOD.

  METHOD set_force_cd.
    IF force_marker = abap_false.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS: <tabinfo> TYPE table.
    ASSIGN ('(SAPLSCD0)TABINFO') TO <tabinfo>.
    IF sy-subrc = 0.
      LOOP AT <tabinfo> ASSIGNING FIELD-SYMBOL(<tabinfo_row>) WHERE ('tabname = table_name AND logflag = abap_false').
        ASSIGN COMPONENT 'LOGFLAG' OF STRUCTURE <tabinfo_row> TO FIELD-SYMBOL(<logflag>).
        <logflag> = 'F'.
      ENDLOOP.
    ENDIF.

    EXPORT force_logging = abap_true table_name = table_name TO MEMORY ID c_memory_id.
  ENDMETHOD.
ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
CLASS lcl_table_descr_manager IMPLEMENTATION.
  METHOD create_empty_table.
    CREATE DATA empty_table TYPE TABLE OF (table_name).
  ENDMETHOD.

  METHOD create_table_with_indicator.
    DATA(handle) = get_table_handle( table_name ).

    CREATE DATA table_with_indicator TYPE HANDLE handle.

    FIELD-SYMBOLS:
      <table_with_indicator> TYPE table,
      <original_table>       TYPE table.
    ASSIGN table_with_indicator->* TO <table_with_indicator>.
    ASSIGN original_table->* TO <original_table>.

    <table_with_indicator> = CORRESPONDING #( <original_table> ).

    LOOP AT <table_with_indicator> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT c_cd_field OF STRUCTURE <row> TO FIELD-SYMBOL(<change_indicator>).
      <change_indicator> = indicator.
    ENDLOOP.

    IF sort = abap_true.
      "Create sort condition
      DATA sort_order TYPE abap_sortorder_tab.
      LOOP AT handle->get_keys( ) REFERENCE INTO DATA(primary_key) WHERE is_primary = abap_true.
        LOOP AT primary_key->components REFERENCE INTO DATA(primary_key_field).
          APPEND VALUE #( name = primary_key_field->name descending = abap_false ) TO sort_order.
        ENDLOOP.
        EXIT.
      ENDLOOP.

      SORT <table_with_indicator> BY (sort_order).
    ENDIF.
  ENDMETHOD.

  METHOD get_table_handle.
    handle = VALUE #( tables_info[ name = table_name ]-handle OPTIONAL ).
    IF handle IS BOUND.
      RETURN.
    ENDIF.

    "Get components
    DATA(struct_descr) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( table_name ) ).
    DATA(components) = struct_descr->get_components( ).
    APPEND VALUE #( name = c_cd_field type = CAST #( cl_abap_structdescr=>describe_by_name( 'CDCHNGINDH' ) ) ) TO components.

    "Get keys
    DATA(keys) = VALUE abap_keydescr_tab( ).
    LOOP AT struct_descr->get_ddic_field_list( ) REFERENCE INTO DATA(field) WHERE keyflag = abap_true.
      APPEND VALUE #( name = field->fieldname ) TO keys.
    ENDLOOP.

    "Build handle back
    handle = cl_abap_tabledescr=>get( p_line_type =  cl_abap_structdescr=>get( components )
      p_key_kind = cl_abap_tabledescr=>keydefkind_user p_key = keys ).
    INSERT VALUE #( name = table_name handle = handle ) INTO TABLE tables_info.
  ENDMETHOD.
ENDCLASS.
