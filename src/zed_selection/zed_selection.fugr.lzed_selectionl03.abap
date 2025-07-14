
CLASS lcl_selection DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor,
      prepare_for_screen_display IMPORTING initial_selection TYPE zif_ed_selection=>tt_table_selection handler TYPE REF TO zif_ed_selection_handler,
      clear_descriptions_cache,
      at_screen_output,
      at_screen_input,
      at_selection_screen_output,
      at_selection_screen_input,
      get_data_after_display EXPORTING user_confirmed TYPE abap_bool user_selection TYPE zif_ed_selection=>tt_table_selection,
      cleanup.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_command,
        confirm         TYPE syst_ucomm VALUE 'CONFIRM',
        cancel          TYPE syst_ucomm VALUE 'CANCEL',
        add_selected    TYPE syst_ucomm VALUE 'ADD_SEL',
        remove_selected TYPE syst_ucomm VALUE 'REM_SEL',
      END OF c_command,
      BEGIN OF c_change_node,
        select TYPE i VALUE 0,
        remove TYPE i VALUE 1,
        switch TYPE i VALUE 2,
      END OF c_change_node.

    METHODS:
      recreate_screen,
      on_double_click FOR EVENT double_click OF cl_salv_events_tree IMPORTING node_key,
      "! @parameter change | Like c_change_node
      change_selected_nodes IMPORTING change TYPE i,
      "! @parameter change | Like c_change_node
      change_nodes IMPORTING nodes TYPE salv_t_nodes change TYPE i,
      extension_before_exit RETURNING VALUE(stop_exit) TYPE abap_bool,
      handle_too_big_selection.

    DATA:
      fields       TYPE tt_field_selection,
      descriptions TYPE REF TO lcl_descriptions,
      so           TYPE REF TO lcl_select_options.

    DATA:
      first_time_pbo               TYPE abap_bool,
      skip_input_after_node_change TYPE abap_bool,
      tree_container               TYPE REF TO cl_gui_docking_container,
      alv_tree                     TYPE REF TO cl_salv_tree,
      tree_tab                     TYPE tt_tree_display.

    DATA:
      handler        TYPE REF TO zif_ed_selection_handler,
      user_confirmed TYPE abap_bool.
ENDCLASS.

CLASS lcl_selection IMPLEMENTATION.
  METHOD constructor.
    descriptions = NEW #( ).
    so = NEW #( ).
    cleanup( ).
  ENDMETHOD.

  METHOD prepare_for_screen_display.
    me->handler = handler.

    LOOP AT initial_selection REFERENCE INTO DATA(init_sel_tab).
      LOOP AT init_sel_tab->fields REFERENCE INTO DATA(init_sel_field).
        APPEND VALUE #( BASE CORRESPONDING #( init_sel_field->* ) table = init_sel_tab->table ) TO fields.
      ENDLOOP.
    ENDLOOP.

    handle_too_big_selection( ).

    so->read_from_fields( fields ).
  ENDMETHOD.

  METHOD clear_descriptions_cache.
    descriptions->clear_descriptions_cache( ).
  ENDMETHOD.

  METHOD at_screen_output.
    IF first_time_pbo = abap_true.
      first_time_pbo = abap_false.
      recreate_screen( ). "Must be called inside pbo, otherwise container will be empty if screen called as popup
    ENDIF.

    SET PF-STATUS 'MAIN'.
  ENDMETHOD.

  METHOD at_screen_input.
    CASE sy-ucomm.
      WHEN c_command-confirm.
        user_confirmed = abap_true.
        so->import_to_fields( CHANGING fields = fields ).
        IF ( NOT zcl_ed_popup=>yes_no( TEXT-q01 ) ) OR extension_before_exit( ).
          RETURN.
        ENDIF.
        LEAVE TO SCREEN 0.

      WHEN c_command-cancel.
        user_confirmed = abap_false.
        so->import_to_fields( CHANGING fields = fields ).
        IF ( NOT zcl_ed_popup=>yes_no( TEXT-q02 ) ) OR extension_before_exit( ).
          RETURN.
        ENDIF.
        PERFORM set_query_active IN PROGRAM rsdbrunt USING ' '.
        LEAVE TO SCREEN 0.

      WHEN c_command-add_selected.
        change_selected_nodes( c_change_node-select ).

      WHEN c_command-remove_selected.
        change_selected_nodes( c_change_node-remove ).

    ENDCASE.
  ENDMETHOD.

  METHOD at_selection_screen_output.
    so->refresh_visibility( ).
    "Special call to make RSDBRUNT refresh select-options definition, labels etc. after every change
    "
    "Apparently that is too much for SAP. After exiting, it tries to execute
    "   system-exit.
    "       perform (SY-XFORM) in program (SY-XPROG).
    "in program <sysini> with SY-XFORM='%_ROOT' and SY-XPROG, which causes dump.
    "Restoring it to space before doesn't work :(
    PERFORM set_query_active IN PROGRAM rsdbrunt USING 'A'.
  ENDMETHOD.

  METHOD at_selection_screen_input.
    IF skip_input_after_node_change = abap_true.
      "^Special case with double click event of salv_tree.
      "Raising command to refresh data also raises selection screen input with old data, so we must clear fields once again.
      so->read_from_fields( fields ).
      skip_input_after_node_change = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD get_data_after_display.
    user_confirmed = me->user_confirmed.
    DATA(prev_tab) = VALUE tabnam( ).
    LOOP AT fields REFERENCE INTO DATA(field).
      IF prev_tab <> field->table.
        prev_tab = field->table.
        APPEND VALUE #( table = field->table ) TO user_selection REFERENCE INTO DATA(sel_tab).
      ENDIF.

      APPEND CORRESPONDING #( field->* ) TO sel_tab->fields.
    ENDLOOP.
  ENDMETHOD.

  METHOD recreate_screen.
    tree_container = NEW #( repid = 'SAPLZED_SELECTION' dynnr = '0001' side = cl_gui_docking_container=>dock_at_left ratio = 30 ).

    cl_salv_tree=>factory( EXPORTING r_container = tree_container IMPORTING r_salv_tree = alv_tree CHANGING t_table = tree_tab ).

    DATA(prev_tab) = VALUE tabnam( ).
    LOOP AT fields REFERENCE INTO DATA(field).
      IF prev_tab <> field->table.
        prev_tab = field->table.
        DATA(table_node) = alv_tree->get_nodes( )->add_node( related_node = space relationship = if_salv_c_node_relation=>last_child
          text = descriptions->get_description( field->table ) ).
      ENDIF.

      DATA(node) = alv_tree->get_nodes( )->add_node( related_node = table_node->get_key( )
         relationship = if_salv_c_node_relation=>last_child
         data_row = CORRESPONDING t_tree_display( field->* )
         text = descriptions->get_description( table = field->table field = field->field ) ).
      field->node_key = node->get_key( ).
      node->set_row_style( COND #( WHEN field->is_selected = abap_true THEN cl_gui_column_tree=>style_emphasized_positive
                                                                       ELSE cl_gui_column_tree=>style_default ) ).
    ENDLOOP.

    alv_tree->get_columns( )->set_optimize( abap_true ).
    alv_tree->get_functions( )->set_find( abap_true ).
    alv_tree->get_functions( )->set_layout_change( abap_true ).
    alv_tree->display( ).

    SET HANDLER on_double_click FOR alv_tree->get_event( ).
  ENDMETHOD.

  METHOD on_double_click.
    change_nodes( nodes = VALUE #( ( key = node_key node = alv_tree->get_nodes( )->get_node( node_key ) ) ) change = c_change_node-switch ).
    skip_input_after_node_change = abap_true.

    "Force screen update
    cl_gui_cfw=>set_new_ok_code( 'DUMMY' ).
    cl_gui_cfw=>flush( ).
  ENDMETHOD.

  METHOD cleanup.
    IF tree_container IS BOUND.
      tree_container->free( ).
    ENDIF.
    FREE: tree_container, alv_tree, tree_tab, fields.
    so->cleanup( ).
    first_time_pbo = abap_true.
    user_confirmed = abap_false.
    skip_input_after_node_change = abap_false.
  ENDMETHOD.

  METHOD change_selected_nodes.
    alv_tree->get_metadata( ). "Required so get_selected_nodes returns refreshed data
    change_nodes( nodes = alv_tree->get_selections( )->get_selected_nodes( ) change = change ).
    so->read_from_fields( fields ).
  ENDMETHOD.

  METHOD change_nodes.
    so->import_to_fields( CHANGING fields = fields ).

    LOOP AT nodes REFERENCE INTO DATA(selected_node).
      DATA(field) = REF #( fields[ KEY node node_key = selected_node->key ] OPTIONAL ).
      IF field IS NOT BOUND.
        "^table node, skip
        CONTINUE.
      ENDIF.

      CASE change.
        WHEN c_change_node-select. field->is_selected = abap_true.
        WHEN c_change_node-remove. field->is_selected = abap_false.
        WHEN c_change_node-switch. field->is_selected = xsdbool( field->is_selected = abap_false ).
      ENDCASE.

      IF field->is_selected = abap_true.
        selected_node->node->set_row_style( cl_gui_column_tree=>style_emphasized_positive ).
      ELSE.
        selected_node->node->set_row_style( cl_gui_column_tree=>style_default ).
        CLEAR: field->range.
      ENDIF.
    ENDLOOP.

    handle_too_big_selection( ).

    alv_tree->display( ).
  ENDMETHOD.

  METHOD extension_before_exit.
    stop_exit = abap_false.
    IF handler IS NOT BOUND.
      RETURN.
    ENDIF.

    get_data_after_display( IMPORTING user_confirmed = DATA(user_confirmed) user_selection = DATA(user_selection) ).
    handler->before_exit( EXPORTING user_confirmed = user_confirmed selection = user_selection CHANGING stop_exit = stop_exit ).
  ENDMETHOD.

  METHOD handle_too_big_selection.
    DATA(id) = 1.
    DATA(limit_hit) = abap_false.

    LOOP AT fields REFERENCE INTO DATA(field).
      IF id > so->c_max_id.
        field->is_selected = abap_false.
        FREE: field->range.
        limit_hit = abap_true.

        IF field->node_key IS NOT INITIAL.
          alv_tree->get_nodes( )->get_node( field->node_key )->set_row_style( cl_gui_column_tree=>style_default ).
        ENDIF.
      ENDIF.
      id = id + 1.
    ENDLOOP.

    IF limit_hit = abap_true.
      MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
