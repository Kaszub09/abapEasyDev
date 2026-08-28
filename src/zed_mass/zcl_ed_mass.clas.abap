CLASS zcl_ed_mass DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_screens_handler,
      zif_ed_mass_callback.

    METHODS:
      "! @parameter table_ref | <p class="shorttext synchronized" lang="en">Can be used to initialize with data,
      "! should be same type as returned by ZIF_ED_MASS~CREATE_TABLE.</p>
      run IMPORTING mass TYPE REF TO zif_ed_mass table_ref TYPE REF TO data OPTIONAL.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_fields,
        result    TYPE fieldname VALUE 'RESULT',
        mass_int  TYPE fieldname VALUE 'MASS_INT',
        success   TYPE fieldname VALUE 'RESULT-SUCCESS',
        msg       TYPE fieldname VALUE 'RESULT-MSG',
        logger    TYPE fieldname VALUE 'RESULT-LOGGER',
        color     TYPE fieldname VALUE 'MASS_INT-COLOR',
        style     TYPE fieldname VALUE 'MASS_INT-STYLE',
        exception TYPE fieldname VALUE 'MASS_INT-EXCEPTION',
      END OF c_fields.

    METHODS:
      on_added_function FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no,
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid.

    METHODS:
      prepare_alv,
      read_dtti IMPORTING source TYPE REF TO zif_dtti_source,
      confirm_data_loss RETURNING VALUE(confirmed) TYPE abap_bool.

    DATA:
      ask_data_loss TYPE abap_bool,
      alv           TYPE REF TO zcl_ea_alv_table,
      mass          TYPE REF TO zif_ed_mass,
      table         TYPE REF TO data,
      config        TYPE zif_ed_mass=>t_config.
ENDCLASS.

CLASS zcl_ed_mass IMPLEMENTATION.
  METHOD zif_ed_screens_handler~pbo_change_header.
    header = config-header_text.
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pai.
    CASE command.
      WHEN 'SAVE'.
        on_added_function( zif_ed_mass=>c_functions-save ).

      WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
        IF NOT confirm_data_loss( ).
          RETURN.
        ENDIF.
        LEAVE TO SCREEN 0.

    ENDCASE.
  ENDMETHOD.

  METHOD run.
    me->mass = mass.
    ask_data_loss = abap_false.

    APPEND VALUE #( function = zif_ed_mass=>c_functions-documentation icon = '@0S@' quickinfo = TEXT-f01 text = TEXT-f01 ) TO config-functions.
    APPEND VALUE #( function = zif_ed_mass=>c_functions-read_from_clipboard icon = '@2V@' quickinfo = TEXT-f02 text = TEXT-f02 ) TO config-functions.
    APPEND VALUE #( function = zif_ed_mass=>c_functions-read_from_file icon = '@XE@' quickinfo = TEXT-f03 text = TEXT-f03 ) TO config-functions.
    APPEND VALUE #( function = zif_ed_mass=>c_functions-read_from_db icon = '@3W@' quickinfo = TEXT-f04 text = TEXT-f04 ) TO config-functions.
    APPEND VALUE #( function = zif_ed_mass=>c_functions-save icon = '@2L@' quickinfo = TEXT-f05 text = TEXT-f05 ) TO config-functions.

    mass->setup( EXPORTING callback = me CHANGING config = config ).
    IF config-docu IS INITIAL.
      DELETE config-functions WHERE function = zif_ed_mass=>c_functions-documentation.
    ENDIF.

    table = COND #( WHEN table_ref IS BOUND THEN table_ref ELSE mass->create_table( ) ).

    prepare_alv( ).
    alv->display_data( it_toolbar_excluding = VALUE #( ( cl_gui_alv_grid=>mc_fc_info ) ( cl_gui_alv_grid=>mc_fc_graph )
        ( cl_gui_alv_grid=>mc_fc_subtot ) ( cl_gui_alv_grid=>mc_fc_refresh ) ) ).
    zcl_ed_screens=>call_next_screen( ).
  ENDMETHOD.

  METHOD prepare_alv.
    DATA(container) = zcl_ed_screens=>prepare_next_screen( handler = me with_toolbar = abap_false ).
    alv = NEW zcl_ea_alv_table( layout_key = config-layout_key container = container ).

    alv->set_data( table ).

    SET HANDLER: on_added_function on_hotspot_click on_data_changed FOR alv->alv_grid.

    alv->grid_layout-sel_mode = 'D'.

    alv->columns->set_all_as_editable( ).
    alv->columns->set_as_editable( column = c_fields-success is_editable = abap_false ).
    alv->columns->set_as_editable( column = c_fields-msg is_editable = abap_false ).
    alv->columns->set_as_color( c_fields-color ).
    alv->columns->set_as_cell_style( c_fields-style ).
    alv->columns->set_as_exception( column = c_fields-exception ).

    "Functions
    LOOP AT config-functions REFERENCE INTO DATA(function).
      alv->functions->add_function( function->* ).
    ENDLOOP.

    mass->modify_alv( alv ).
  ENDMETHOD.

  METHOD on_added_function.
    CASE e_ucomm.
      WHEN zif_ed_mass=>c_functions-documentation.
        zcl_ed_docu=>show_alt( id = config-docu-id object = config-docu-object ).

      WHEN zif_ed_mass=>c_functions-read_from_db.
        mass->fill_table_with_db_data( table ).
        alv->refresh( ).

      WHEN zif_ed_mass=>c_functions-read_from_clipboard.
        read_dtti( zcl_dtti_source_factory=>create_from_clipboard( ) ).

      WHEN zif_ed_mass=>c_functions-read_from_file.
        read_dtti( zcl_dtti_source_factory=>create_from_file( ) ).

      WHEN OTHERS.
        mass->function( function = e_ucomm callback = me ).

    ENDCASE.
  ENDMETHOD.

  METHOD on_hotspot_click.
    IF e_row_id-index = 0.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS:
      <table>  TYPE table,
      <logger> TYPE REF TO zif_ed_logger.
    ASSIGN table->* TO <table>.

    ASSIGN COMPONENT c_fields-logger OF STRUCTURE <table>[ e_row_id-index ] TO <logger>.
    IF <logger> IS BOUND.
      <logger>->ext->display->get_display_modal( abap_false )->display( ).
    ENDIF.
  ENDMETHOD.

  METHOD on_data_changed.
    ask_data_loss = abap_true.
  ENDMETHOD.

  METHOD confirm_data_loss.
    IF ask_data_loss = abap_true.
      confirmed = zcl_ed_popup=>yes_no( question = TEXT-003 ).
      RETURN.
    ENDIF.
    confirmed = abap_true.
  ENDMETHOD.

  METHOD read_dtti.
    IF source IS NOT BOUND. "user cancelled
      RETURN.
    ENDIF.

    DATA(target) = zcl_dtti_target_factory=>create_from_ref( table ).
    target->remove_field( c_fields-success ).
    target->remove_field( c_fields-msg ).
    target->remove_field( c_fields-logger ).
    target->remove_field( c_fields-color ).
    target->remove_field( c_fields-style ).
    target->remove_field( c_fields-exception ).
    mass->modify_dtti_target( target ).

    DATA(dtti) = NEW zcl_data_to_table_import( ).
    IF NOT dtti->run_mapping( source = source target = target config = VALUE #( convert_currrency_to_internal = abap_true ) ).
      RETURN.
    ENDIF.

    DATA(target_table) = target->get_target_table( ).
    ASSIGN table->* TO FIELD-SYMBOL(<table>).
    ASSIGN target_table->* TO FIELD-SYMBOL(<target_table>).
    <table> = CORRESPONDING #( <target_table> ).

    alv->refresh( ).
  ENDMETHOD.

  METHOD zif_ed_mass_callback~get_hidden_columns.
    alv->alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(fc) ).
    DELETE fc WHERE no_out = abap_false OR tech = abap_true.
    hidden_columns = VALUE zif_ed_mass_callback=>tt_hidden_columns( FOR <fc_row> IN fc ( <fc_row>-fieldname ) ).
  ENDMETHOD.

  METHOD zif_ed_mass_callback~get_selected_rows.
    alv->alv_grid->get_selected_rows( IMPORTING et_index_rows = rows ).
    DELETE rows WHERE index = '0000000000'.
  ENDMETHOD.

  METHOD zif_ed_mass_callback~refresh_alv.
    IF refresh_mass_int_info = abap_true.
      FIELD-SYMBOLS: <table>    TYPE table.
      ASSIGN table->* TO <table>.

      DATA(selected_rows) = zif_ed_mass_callback~get_selected_rows( ).
      IF only_selected_rows = abap_false.
        CLEAR: selected_rows.
        DATA(index) = 1.
        WHILE index < lines( <table> ).
          APPEND VALUE #( index = index ) TO selected_rows.
          index = index + 1.
        ENDWHILE.
      ENDIF.

      FIELD-SYMBOLS:
        <result>   TYPE zif_ed_mass=>t_table_include-result,
        <mass_int> TYPE zif_ed_mass=>t_table_include-mass_int.

      LOOP AT selected_rows REFERENCE INTO DATA(selected_row).
        ASSIGN <table>[ selected_row->index ] TO FIELD-SYMBOL(<row>).
        ASSIGN COMPONENT c_fields-result OF STRUCTURE <row> TO <result>.
        ASSIGN COMPONENT c_fields-mass_int OF STRUCTURE <row> TO <mass_int>.

        IF <result>-success = abap_true.
          <mass_int>-exception = '3'.
          <mass_int>-color = VALUE #( color = VALUE #( col = 5 ) ( fname = c_fields-success ) ( fname = c_fields-msg ) ).
        ELSE.
          <mass_int>-exception = '1'.
          <mass_int>-color = VALUE #( color = VALUE #( col = 6 ) ( fname = c_fields-success ) ( fname = c_fields-msg ) ).
        ENDIF.

        CLEAR: <mass_int>-style.

        IF <result>-logger IS BOUND.
          <mass_int>-style = VALUE #( ( fieldname = c_fields-msg style = cl_gui_alv_grid=>mc_style_hotspot ) ).
          IF <result>-logger->log-has_warnings = abap_true AND <result>-logger->log-has_errors = abap_false.
            <mass_int>-exception = '2'.
            <mass_int>-color = VALUE #( color = VALUE #( col = 7 ) ( fname = c_fields-success ) ( fname = c_fields-msg ) ).
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    alv->alv_grid->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
  ENDMETHOD.

  METHOD zif_ed_mass_callback~get_table_ref.
    table_ref = me->table.
  ENDMETHOD.

  METHOD zif_ed_mass_callback~are_rows_selected.
    are_selected = xsdbool( lines( zif_ed_mass_callback~get_selected_rows( ) ) <> 0 ).
    IF ask_for_sel_if_none_sel = abap_true AND are_selected = abap_false.
      MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_mass_callback~ask_confirm_save.
    is_confirmed = zcl_ed_popup=>yes_no( TEXT-002 ).
  ENDMETHOD.
ENDCLASS.
