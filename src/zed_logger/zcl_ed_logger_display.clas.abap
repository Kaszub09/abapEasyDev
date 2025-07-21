CLASS zcl_ed_logger_display DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ed_logger_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_logger_display,
      zif_ed_screens_handler.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_message_display,
        level           TYPE zted_log_detail_level,
        msg_type_icon   TYPE c LENGTH 1,
        msg_type        TYPE msgty,
        details_icon    TYPE zted_log_details_icon,
        created_at_date TYPE zted_log_created_at_date,
        created_at_time TYPE zted_log_created_at_time,
        msg             TYPE zted_log_msg,
        color           TYPE lvc_t_scol,
      END OF t_message_display,
      tt_message_display TYPE STANDARD TABLE OF t_message_display WITH EMPTY KEY,
      BEGIN OF t_header.
        INCLUDE TYPE zed_logs.
      TYPES:
        color TYPE lvc_t_scol,
      END OF t_header,
      tt_header TYPE STANDARD TABLE OF t_header WITH EMPTY KEY.

    METHODS:
      reset_state,
      select_headers IMPORTING sel TYPE zif_ed_logger_display~t_selection,
      create_headers_alv IMPORTING container TYPE REF TO cl_gui_container,
      create_messages_alv IMPORTING container TYPE REF TO cl_gui_container,

      display_messages IMPORTING logger TYPE REF TO zif_ed_logger,
      init_messages_display_tab IMPORTING logger TYPE REF TO zif_ed_logger RETURNING VALUE(display_tab) TYPE REF TO data,
      fill_messages_display_tab IMPORTING context TYPE REF TO zcl_ed_logger_context tab TYPE zif_ed_logger=>tt_log_message display_tab TYPE REF TO data,
      set_messages_filter IMPORTING msg_type TYPE msgty,
      refresh_single_log_display.

    METHODS:
      on_header_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING row column,
      on_header_link_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column,
      on_msg_link_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column,
      on_close FOR EVENT close OF cl_gui_dialogbox_container.

    DATA:
      BEGIN OF headers,
        tab TYPE tt_header,
        alv TYPE REF TO zcl_ea_salv_table,
      END OF headers,
      BEGIN OF messages,
        tab         TYPE zif_ed_logger=>tt_log_message,
        alv         TYPE REF TO zcl_ea_salv_table,
        display_tab TYPE REF TO data,
      END OF messages,
      BEGIN OF single_log,
        is                      TYPE abap_bool,
        logger                  TYPE REF TO zif_ed_logger,
        is_modeless_window_open TYPE abap_bool,
      END OF single_log,
      container      TYPE REF TO cl_gui_container,
      first_time_pbo TYPE abap_bool.
ENDCLASS.

CLASS zcl_ed_logger_display IMPLEMENTATION.

  METHOD zif_ed_logger_display~display_log.
    reset_state( ). "In case there are leftovers from modeless call.
    single_log = VALUE #( is = abap_true logger = logger is_modeless_window_open = abap_false ).
    APPEND CORRESPONDING #( single_log-logger->log ) TO headers-tab.

    zcl_ed_screens=>prepare_next_screen( handler = me create_container = abap_false ).
    zcl_ed_screens=>call_next_screen( start_column = start_column end_column = end_column start_line = start_line end_line = end_line ).
    reset_state( ). "Free memory etc.
  ENDMETHOD.

  METHOD zif_ed_logger_display~display_log_in_modeless.
    "Modeless window for this logger is already opened, so just refresh. Otherwise, create from scratch.
    IF single_log-is = abap_true AND single_log-is_modeless_window_open = abap_true AND single_log-logger = logger.
      refresh_single_log_display( ).
      RETURN.
    ENDIF.

    reset_state( ).
    single_log = VALUE #( is = abap_true logger = logger is_modeless_window_open = abap_true ).
    APPEND CORRESPONDING #( single_log-logger->log ) TO headers-tab.

    container = NEW cl_gui_dialogbox_container( width = width height = height ).
    SET HANDLER on_close FOR CAST cl_gui_dialogbox_container( container ).

    zif_ed_screens_handler~pbo( sy-dynnr ).
  ENDMETHOD.

  METHOD zif_ed_logger_display~display_logs.
    reset_state( ).
    single_log = VALUE #( is = abap_false is_modeless_window_open = abap_false ).
    select_headers( sel ).

    zcl_ed_screens=>prepare_next_screen( handler = me create_container = abap_false ).
    zcl_ed_screens=>call_next_screen( start_column = start_column end_column = end_column start_line = start_line end_line = end_line ).
    reset_state( ).
  ENDMETHOD.

  METHOD zif_ed_logger_display~is_modeless_window_open.
    is_open = single_log-is_modeless_window_open.
  ENDMETHOD.


  METHOD zif_ed_screens_handler~pai.
    CASE command.
      WHEN zif_ed_screens_handler~c_status_commands-back OR zif_ed_screens_handler~c_status_commands-cancel
      OR zif_ed_screens_handler~c_status_commands-exit.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pbo.
    IF first_time_pbo = abap_false.
      RETURN.
    ENDIF.
    first_time_pbo = abap_false.

    IF single_log-is = abap_false OR single_log-is_modeless_window_open = abap_false.
      "Must create there, otherwise modal popup will be empty
      container = zcl_ed_screens=>get_screen_container( ).
    ENDIF.
    DATA(splitter) = NEW cl_gui_splitter_container( parent = container rows = 2 columns = 1 ).

    create_headers_alv( splitter->get_container( row = 1 column = 1 ) ).
    create_messages_alv( splitter->get_container( row = 2 column = 1 ) ).

    IF single_log-is = abap_true.
      "Display messages immediately
      splitter->set_row_height( id = 1 height = 16 ).
      display_messages( single_log-logger ).
    ENDIF.
  ENDMETHOD.

  METHOD reset_state.
    IF container IS BOUND.
      container->free( ).
    ENDIF.
    CLEAR: container, headers, single_log.
    first_time_pbo = abap_true.
    single_log-is = abap_false.
  ENDMETHOD.

  METHOD select_headers.
    SELECT * FROM zed_logs
     WHERE uuid IN @sel-uuid AND category IN @sel-category AND external_identifier IN @sel-external_identifier
         AND created_by IN @sel-created_by AND created_at_date IN @sel-created_at_date
         AND created_at_time IN @sel-created_at_time AND tcode IN @sel-transaction AND expiry_date IN @sel-expiry_date
         AND is_batch IN @sel-is_batch AND last_change_by IN @sel-last_change_by
         AND last_change_date IN @sel-last_change_date AND last_change_time IN @sel-last_change_time
         AND messsages_count IN @sel-messsages_count AND has_warnings IN @sel-has_warnings AND has_errors IN @sel-has_errors
    INTO CORRESPONDING FIELDS OF TABLE @headers-tab.
  ENDMETHOD.

  METHOD create_headers_alv.
    headers-alv = NEW #( container = container layout_key = VALUE #( report = sy-repid handle = 'HDR' )  ).
    headers-alv->set_data( REF #( headers-tab ) ).

    headers-alv->columns->set_as_hotspot( 'UUID' ).
    headers-alv->columns->set_as_hotspot( 'HAS_ERRORS' ).
    headers-alv->columns->set_as_hotspot( 'HAS_WARNINGS' ).
    headers-alv->columns->set_as_hidden( 'MANDT' ).
    headers-alv->columns->set_as_hidden( 'CONTEXT_COL_INFO' ).
    headers-alv->columns->set_as_color( 'COLOR' ).

    SET HANDLER on_header_double_click FOR headers-alv->alv_table->get_event( ).
    SET HANDLER on_header_link_click FOR headers-alv->alv_table->get_event( ).

    LOOP AT headers-tab REFERENCE INTO DATA(header).
      IF header->has_errors = abap_true.
        APPEND VALUE #( fname = 'HAS_ERRORS' color = VALUE #( col = 6 ) ) TO header->color.
      ENDIF.
      IF header->has_warnings = abap_true.
        APPEND VALUE #( fname = 'HAS_WARNINGS' color = VALUE #( col = 7 ) ) TO header->color.
      ENDIF.
    ENDLOOP.

    headers-alv->columns->set_optimize( ).
    headers-alv->display_data( ).
  ENDMETHOD.

  METHOD create_messages_alv.
    messages-alv = NEW #( container = container layout_key = VALUE #( report = sy-repid handle = 'MSG' )  ).
    SET HANDLER on_msg_link_click FOR messages-alv->alv_table->get_event( ).
  ENDMETHOD.

  METHOD display_messages.
    messages-tab = CORRESPONDING #( logger->log-messages ).
    messages-display_tab = init_messages_display_tab( logger ).
    fill_messages_display_tab( context = logger->context tab = messages-tab display_tab = messages-display_tab ).

    messages-alv->set_data( messages-display_tab ).

    messages-alv->columns->set_as_exception( 'MSG_TYPE_ICON' ).
    messages-alv->columns->set_as_hotspot( 'DETAILS_ICON' ).
    messages-alv->columns->set_as_color( 'COLOR' ).

    messages-alv->alv_table->get_filters( )->clear( ).
    messages-alv->display_data( ).
    messages-alv->columns->set_optimize( ).
    messages-alv->alv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDMETHOD.

  METHOD init_messages_display_tab.
    DATA(components) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( VALUE t_message_display( ) ) )->get_components( ).
    IF logger->context->exists( ).
      APPEND LINES OF logger->context->get_struct_components( ) TO components.
    ENDIF.

    DATA(table_descr) = cl_abap_tabledescr=>get( cl_abap_structdescr=>get( components ) ).
    CREATE DATA display_tab TYPE HANDLE table_descr.
  ENDMETHOD.

  METHOD fill_messages_display_tab.
    FIELD-SYMBOLS: <display_tab> TYPE table.

    ASSIGN display_tab->* TO <display_tab>.

    LOOP AT tab REFERENCE INTO DATA(msg).
      APPEND INITIAL LINE TO <display_tab> ASSIGNING FIELD-SYMBOL(<display_row>).
      DATA(msg_display) = CORRESPONDING t_message_display( msg->* ).
      CASE msg_display-msg_type.
        WHEN 'I'.
          msg_display-msg_type_icon = '0'.
        WHEN 'E'.
          APPEND VALUE #( fname = 'MSG_TYPE' color = VALUE #( col = 6 ) ) TO msg_display-color.
          msg_display-msg_type_icon = '1'.
        WHEN 'W'.
          APPEND VALUE #( fname = 'MSG_TYPE' color = VALUE #( col = 7 ) ) TO msg_display-color.
          msg_display-msg_type_icon = '2'.
        WHEN 'S'.
          APPEND VALUE #( fname = 'MSG_TYPE' color = VALUE #( col = 5 ) ) TO msg_display-color.
          msg_display-msg_type_icon = '3'.
      ENDCASE.

      msg_display-details_icon = COND #( WHEN msg->is_sap_msg = abap_true THEN '@35@' ELSE '@16@' ).

      <display_row> = CORRESPONDING #( msg_display ).
      IF context->exists( ).
        DATA(context_data) = context->convert_string_to_data( msg->context_values ).
        ASSIGN context_data->* TO FIELD-SYMBOL(<context_data>).
        MOVE-CORRESPONDING <context_data> TO <display_row>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_messages_filter.
    DATA(filters) = messages-alv->alv_table->get_filters( ).
    filters->clear( ).
    filters->add_filter( columnname = 'MSG_TYPE' sign = 'I' option = 'EQ' low = CONV #( msg_type ) ).
    messages-alv->alv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDMETHOD.

  METHOD refresh_single_log_display.
    CLEAR: headers.
    APPEND CORRESPONDING #( single_log-logger->log ) TO headers-tab.
    IF headers-alv IS BOUND.
      headers-alv->alv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ENDIF.
    display_messages( single_log-logger ).
  ENDMETHOD.

  METHOD on_header_double_click.
    IF row = 0.
      RETURN.
    ENDIF.

    display_messages( COND #( WHEN single_log-is = abap_true THEN single_log-logger
                                  ELSE zcl_ed_logger_factory=>open_logger( uuid = headers-tab[ row ]-uuid ) ) ).
  ENDMETHOD.

  METHOD on_header_link_click.
    IF row = 0.
      RETURN.
    ENDIF.

    CASE column.
      WHEN 'UUID' OR 'HAS_ERRORS' OR 'HAS_WARNINGS'.
        display_messages( COND #( WHEN single_log-is = abap_true THEN single_log-logger
                                      ELSE zcl_ed_logger_factory=>open_logger( uuid = headers-tab[ row ]-uuid ) ) ).
        CASE column.
          WHEN 'HAS_ERRORS'. set_messages_filter( 'E' ).
          WHEN 'HAS_WARNINGS'. set_messages_filter( 'W' ).
        ENDCASE.
    ENDCASE.
  ENDMETHOD.

  METHOD on_msg_link_click.
    IF row = 0.
      RETURN.
    ENDIF.
    DATA(selected) = REF #( messages-tab[ row ] ).

    CASE column.
      WHEN 'DETAILS_ICON'.
        IF selected->is_sap_msg = abap_true.
          zcl_ed_docu=>show( dokclass = 'NA' dokname = |{ selected->sap_msg-msgid }{ selected->sap_msg-msgno }|
              msg_var_1 = selected->sap_msg-msgv1 msg_var_2 = selected->sap_msg-msgv2
              msg_var_3 = selected->sap_msg-msgv3  msg_var_4 = selected->sap_msg-msgv4 ).

        ELSEIF selected->is_json = abap_true.
          cl_demo_output=>display_json( messages-tab[ row ]-msg  ).

        ELSE.
          cl_demo_output=>display( messages-tab[ row ]-msg  ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD on_close.
    reset_state( ).
  ENDMETHOD.

ENDCLASS.
