CLASS zcl_ed_mass DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_screens_handler.

    METHODS:
      run IMPORTING mass TYPE REF TO zif_ed_mass.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_functions,
        documentation       TYPE ui_func VALUE 'DOCUMENTATION',
        read_from_clipboard TYPE ui_func VALUE 'READ_FROM_CLIPBOARD',
        read_from_file      TYPE ui_func VALUE 'READ_FROM_FILE',
        read_from_db        TYPE ui_func VALUE 'READ_FROM_DB',
        save                TYPE ui_func VALUE 'SAVE',
      END OF c_functions,
      BEGIN OF c_fields,
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
      confirm_data_loss RETURNING VALUE(confirmed) TYPE abap_bool,
      confirm_save RETURNING VALUE(confirmed) TYPE abap_bool,
      command_save,
      update_rows_int_info IMPORTING table TYPE REF TO data.

    DATA:
      ask_data_loss TYPE abap_bool,
      alv           TYPE REF TO zcl_ea_alv_table,
      mass          TYPE REF TO zif_ed_mass,
      table         TYPE REF TO data.
ENDCLASS.

CLASS zcl_ed_mass IMPLEMENTATION.
  METHOD zif_ed_screens_handler~pai.
    CASE command.
      WHEN 'SAVE'.
        command_save( ).

      WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
        IF NOT confirm_data_loss( ).
          RETURN.
        ENDIF.
        LEAVE TO SCREEN 0.

    ENDCASE.
  ENDMETHOD.

  METHOD run.
    ask_data_loss = abap_false.
    me->mass = mass.
    table = mass->create_table( ).

    prepare_alv( ).
    alv->display_data( it_toolbar_excluding = VALUE #( ( cl_gui_alv_grid=>mc_fc_info ) ( cl_gui_alv_grid=>mc_fc_graph )
        ( cl_gui_alv_grid=>mc_fc_subtot ) ( cl_gui_alv_grid=>mc_fc_refresh ) ) ).
    zcl_ed_screens=>call_next_screen( ).
  ENDMETHOD.

  METHOD prepare_alv.
    DATA(container) = zcl_ed_screens=>prepare_next_screen( handler = me with_toolbar = abap_false ).
    alv = NEW zcl_ea_alv_table( layout_key = mass->config-layout_key container = container ).

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
    IF mass->config-docu IS NOT INITIAL.
      alv->functions->add_function( function = VALUE #( function = c_functions-documentation icon = '@0S@'
          quickinfo = TEXT-f01 text = TEXT-f01 ) ).
    ENDIF.
    alv->functions->add_function( function = VALUE #( function = c_functions-read_from_clipboard icon = '@2V@'
        quickinfo = TEXT-f02 text = TEXT-f02 ) ).
    alv->functions->add_function( function = VALUE #( function = c_functions-read_from_file icon = '@XE@'
        quickinfo = TEXT-f03 text = TEXT-f03 ) ).
    IF mass->config-disable_read_from_db = abap_false.
      alv->functions->add_function( function = VALUE #( function = c_functions-read_from_db icon = '@3W@'
          quickinfo = TEXT-f04 text = TEXT-f04 ) ).
    ENDIF.
    alv->functions->add_function( function = VALUE #( function = c_functions-save icon = '@2L@'
        quickinfo = TEXT-f05 text = TEXT-f05 ) ).

    mass->modify_alv( alv ).
  ENDMETHOD.

  METHOD on_added_function.
    CASE e_ucomm.
      WHEN c_functions-documentation.
        zcl_ed_docu=>show_alt( id = mass->config-docu-id object = mass->config-docu-object ).

      WHEN c_functions-read_from_db.
        mass->fill_table_with_db_data( table ).
        alv->refresh( ).

      WHEN c_functions-read_from_clipboard.
        read_dtti( zcl_dtti_source_factory=>create_from_clipboard( ) ).

      WHEN c_functions-read_from_file.
        read_dtti( zcl_dtti_source_factory=>create_from_file( ) ).

      WHEN c_functions-save.
        command_save( ).
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
      zcl_ed_logger_factory=>create_display( )->display_log( logger = <logger> start_column = 1 start_line = 1 ).
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
    "target->remove_field( c_fields-success ).
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
  ENDMETHOD.

  METHOD command_save.
    IF NOT confirm_save( ).
      RETURN.
    ENDIF.

    FIELD-SYMBOLS: <table> TYPE table.
    ASSIGN table->* TO <table>.

    alv->alv_grid->get_selected_rows( IMPORTING et_index_rows = DATA(et_index_rows) ).
    DELETE et_index_rows WHERE index = '0000000000'.

    alv->alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(fc) ).
    DELETE fc WHERE no_out = abap_false OR tech = abap_true.
    DATA(hidden_columns) = VALUE zif_ed_mass=>tt_hidden_columns( FOR <fc_row> IN fc ( <fc_row>-fieldname ) ).

    IF lines( et_index_rows ) = lines( <table> ).
      "All rows selected
      mass->save( table_ref = table hidden_columns = hidden_columns ).
      update_rows_int_info( table ).

    ELSE.
      "Need to filter rows and pass only selected
      DATA: selected_rows TYPE REF TO data.
      CREATE DATA selected_rows LIKE <table>.
      FIELD-SYMBOLS: <selected_rows> TYPE table.
      ASSIGN selected_rows->* TO <selected_rows>.

      LOOP AT et_index_rows REFERENCE INTO DATA(selected_index).
        APPEND <table>[ selected_index->index ] TO <selected_rows>.
      ENDLOOP.

      mass->save( table_ref = selected_rows hidden_columns = hidden_columns ).
      update_rows_int_info( selected_rows ).

      "Assume row order was not changed
      LOOP AT et_index_rows REFERENCE INTO selected_index.
        <table>[ selected_index->index ] = <selected_rows>[ sy-tabix ].
      ENDLOOP.
    ENDIF.

    ask_data_loss = abap_false.
    alv->refresh( ).
  ENDMETHOD.

  METHOD confirm_save.
    alv->alv_grid->get_selected_rows( IMPORTING et_index_rows = DATA(et_index_rows) ).
    DELETE et_index_rows WHERE index = '0000000000'.
    IF lines( et_index_rows ) = 0.
      MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF NOT zcl_ed_popup=>yes_no( question = TEXT-002 ).
      RETURN.
    ENDIF.

    confirmed = abap_true.
  ENDMETHOD.

  METHOD update_rows_int_info.
    FIELD-SYMBOLS:
      <table>     TYPE table,
      <success>   TYPE zted_success,
      <logger>    TYPE REF TO zif_ed_logger,
      <color>     TYPE lvc_t_scol,
      <style>     TYPE lvc_t_styl,
      <exception> TYPE c.

    ASSIGN table->* TO <table>.

    LOOP AT <table> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT c_fields-success OF STRUCTURE <row> TO <success>.
      ASSIGN COMPONENT c_fields-logger OF STRUCTURE <row> TO <logger>.
      ASSIGN COMPONENT c_fields-color OF STRUCTURE <row> TO <color>.
      ASSIGN COMPONENT c_fields-style OF STRUCTURE <row> TO <style>.
      ASSIGN COMPONENT c_fields-exception OF STRUCTURE <row> TO <exception>.

      IF <success> = abap_true.
        <exception> = '3'.
        <color> = VALUE #( color = VALUE #( col = 5 ) ( fname = c_fields-success ) ( fname = c_fields-msg ) ).
      ELSE.
        <exception> = '1'.
        <color> = VALUE #( color = VALUE #( col = 6 ) ( fname = c_fields-success ) ( fname = c_fields-msg ) ).
      ENDIF.

      CLEAR: <style>.

      IF <logger> IS BOUND.
        <style> = VALUE #( ( fieldname = c_fields-msg style = cl_gui_alv_grid=>mc_style_hotspot ) ).
        IF <logger>->log-has_warnings = abap_true AND <logger>->log-has_errors = abap_false.
          <exception> = '2'.
          <color> = VALUE #( color = VALUE #( col = 7 ) ( fname = c_fields-success ) ( fname = c_fields-msg ) ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    alv->refresh( ).
  ENDMETHOD.
ENDCLASS.
