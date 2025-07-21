CLASS zcl_ed_logger_display DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ed_logger_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_screens_handler,
      zif_ed_logger_display.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_messages_ext,
        level           TYPE zted_log_detail_level,
        msg_type_icon   TYPE c LENGTH 1,
        msg_type        TYPE msgty,
        details_icon    TYPE zted_log_details_icon,
        created_at_date TYPE zted_log_created_at_date,
        created_at_time TYPE zted_log_created_at_time,
        msg             TYPE zted_log_msg,
        color           TYPE lvc_t_scol,
      END OF t_messages_ext,
      tt_messages_ext TYPE STANDARD TABLE OF t_messages_ext WITH EMPTY KEY,
      BEGIN OF t_header.
        INCLUDE TYPE zed_logs.
      TYPES:
        color TYPE lvc_t_scol,
      END OF t_header,
      tt_header TYPE STANDARD TABLE OF t_header WITH EMPTY KEY.

    METHODS:
      read_headers IMPORTING sel TYPE zif_ed_logger_display~t_selection,
      init_headers_alv IMPORTING container TYPE REF TO cl_gui_container,
      init_messages_alv IMPORTING container TYPE REF TO cl_gui_container,
      cleanup,
      prepare_messages_alv IMPORTING logger TYPE REF TO zif_ed_logger,
      create_messages_ext_data IMPORTING logger TYPE REF TO zif_ed_logger,
      copy_messages_int_to_ext IMPORTING logger TYPE REF TO zif_ed_logger,
      set_messages_filter IMPORTING msg_type TYPE msgty,
      refresh_single_log_display.

    METHODS:
      on_header_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING row column,
      on_header_link_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column,
      on_msg_link_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column,
      on_close FOR EVENT close OF cl_gui_dialogbox_container.

    DATA:
      headers      TYPE tt_header,
      messages_int TYPE zif_ed_logger=>tt_log_message,
      messages_ext TYPE REF TO data.

    DATA:
      "! If it's bound, we are in single log display mode
      logger_to_display TYPE REF TO zif_ed_logger,
      modeless_window   TYPE abap_bool,
      first_pbo         TYPE abap_bool VALUE abap_true,
      headers_alv       TYPE REF TO zcl_ea_salv_table,
      messages_alv      TYPE REF TO zcl_ea_salv_table,
      main_container    TYPE REF TO cl_gui_container.
ENDCLASS.

CLASS zcl_ed_logger_display IMPLEMENTATION.

  METHOD zif_ed_logger_display~display_log.
    cleanup( ). "In case there are leftovers from modeless call.
    logger_to_display = logger.
    APPEND CORRESPONDING #( logger_to_display->log ) TO headers.

    zcl_ed_screens=>prepare_next_screen( handler = me create_container = abap_false ).
    zcl_ed_screens=>call_next_screen( start_column = start_column end_column = end_column start_line = start_line end_line = end_line ).
    cleanup( ).
  ENDMETHOD.

  METHOD zif_ed_logger_display~display_log_in_modeless.
    "Modeless window for this logger is already opened, so just refresh. Otherwise, create from scratch
    IF modeless_window = abap_true AND logger_to_display = logger.
      refresh_single_log_display( ).
      RETURN.
    ENDIF.

    cleanup( ). "In case there are leftovers from modeless call.
    logger_to_display = logger.
    modeless_window = abap_true.
    APPEND CORRESPONDING #( logger_to_display->log ) TO headers.

    main_container = NEW cl_gui_dialogbox_container( width = width height = height ).
    SET HANDLER on_close FOR CAST cl_gui_dialogbox_container( main_container ).

    "Call to recreate containers etc.
    zif_ed_screens_handler~pbo( sy-dynnr ).
  ENDMETHOD.

  METHOD zif_ed_logger_display~display_logs.
    cleanup( ). "In case there are leftovers from modeless call.
    read_headers( sel ).

    zcl_ed_screens=>prepare_next_screen( handler = me create_container = abap_false ).
    zcl_ed_screens=>call_next_screen( start_column = start_column end_column = end_column start_line = start_line end_line = end_line ).
    cleanup( ).
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pai.
    CASE command.
      WHEN zif_ed_screens_handler~c_status_commands-back OR zif_ed_screens_handler~c_status_commands-cancel
      OR zif_ed_screens_handler~c_status_commands-exit.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pbo.
    IF first_pbo = abap_false.
      RETURN.
    ENDIF.
    first_pbo = abap_false.

    IF modeless_window = abap_false.
      main_container = zcl_ed_screens=>get_screen_container( ).
    ENDIF.
    DATA(splitter) = NEW cl_gui_splitter_container( parent = main_container rows = 2 columns = 1 ).

    init_headers_alv( splitter->get_container( row = 1 column = 1 ) ).
    init_messages_alv( splitter->get_container( row = 2 column = 1 ) ).

    IF logger_to_display IS BOUND.
      "Special case, since in single log mode we want to display messages immediately
      splitter->set_row_height( id = 1 height = 16 ).
      prepare_messages_alv( logger_to_display ).
    ENDIF.
  ENDMETHOD.

  METHOD read_headers.
    SELECT * FROM zed_logs
     WHERE uuid IN @sel-uuid AND category IN @sel-category AND external_identifier IN @sel-external_identifier
         AND created_by IN @sel-created_by AND created_at_date IN @sel-created_at_date
         AND created_at_time IN @sel-created_at_time AND tcode IN @sel-transaction AND expiry_date IN @sel-expiry_date
         AND is_batch IN @sel-is_batch AND last_change_by IN @sel-last_change_by
         AND last_change_date IN @sel-last_change_date AND last_change_time IN @sel-last_change_time
         AND messsages_count IN @sel-messsages_count AND has_warnings IN @sel-has_warnings AND has_errors IN @sel-has_errors
    INTO CORRESPONDING FIELDS OF TABLE @headers.
  ENDMETHOD.

  METHOD init_headers_alv.
    headers_alv = NEW #( container = container layout_key = VALUE #( report = sy-repid handle = 'HDR' )  ).
    headers_alv->set_data( REF #( headers ) ).

    headers_alv->columns->set_as_hotspot( 'UUID' ).
    headers_alv->columns->set_as_hotspot( 'HAS_ERRORS' ).
    headers_alv->columns->set_as_hotspot( 'HAS_WARNINGS' ).
    headers_alv->columns->set_as_hidden( 'MANDT' ).
    headers_alv->columns->set_as_hidden( 'CONTEXT_COL_INFO' ).
    headers_alv->columns->set_as_color( 'COLOR' ).

    SET HANDLER on_header_double_click FOR headers_alv->alv_table->get_event( ).
    SET HANDLER on_header_link_click FOR headers_alv->alv_table->get_event( ).

    LOOP AT headers REFERENCE INTO DATA(header).
      IF header->has_errors = abap_true.
        APPEND VALUE #( fname = 'HAS_ERRORS' color = VALUE #( col = 6 ) ) TO header->color.
      ENDIF.
      IF header->has_warnings = abap_true.
        APPEND VALUE #( fname = 'HAS_WARNINGS' color = VALUE #( col = 7 ) ) TO header->color.
      ENDIF.
    ENDLOOP.

    headers_alv->columns->set_optimize( ).
    headers_alv->display_data( ).
  ENDMETHOD.

  METHOD init_messages_alv.
    messages_alv = NEW #( container = container layout_key = VALUE #( report = sy-repid handle = 'MSG' )  ).
    SET HANDLER on_msg_link_click FOR messages_alv->alv_table->get_event( ).
  ENDMETHOD.

  METHOD cleanup.
    first_pbo = abap_true.
    IF main_container IS BOUND.
      main_container->free( ).
    ENDIF.
    CLEAR: modeless_window, headers, messages_int, messages_ext, logger_to_display, headers_alv, messages_alv, main_container.
  ENDMETHOD.

  METHOD prepare_messages_alv.
    messages_int = CORRESPONDING #( logger->log-messages ).
    create_messages_ext_data( logger ).
    copy_messages_int_to_ext( logger ).

    messages_alv->set_data( messages_ext ).

    messages_alv->columns->set_as_exception( 'MSG_TYPE_ICON' ).
    messages_alv->columns->set_as_hotspot( 'DETAILS_ICON' ).
    messages_alv->columns->set_as_color( 'COLOR' ).

    messages_alv->alv_table->get_filters( )->clear( ).
    messages_alv->display_data( ).
    messages_alv->columns->set_optimize( ).
    messages_alv->alv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDMETHOD.

  METHOD create_messages_ext_data.
    DATA(components) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( VALUE t_messages_ext( ) ) )->get_components( ).
    IF logger->context->exists( ).
      APPEND LINES OF logger->context->get_struct_components( ) TO components.
    ENDIF.

    DATA(table_descr) = cl_abap_tabledescr=>get( cl_abap_structdescr=>get( components ) ).
    CREATE DATA messages_ext TYPE HANDLE table_descr.
  ENDMETHOD.

  METHOD copy_messages_int_to_ext.
    FIELD-SYMBOLS: <messages_ext> TYPE table.

    ASSIGN messages_ext->* TO <messages_ext>.

    LOOP AT messages_int REFERENCE INTO DATA(msg_int).
      APPEND INITIAL LINE TO <messages_ext> ASSIGNING FIELD-SYMBOL(<row_ext>).
      DATA(msg_ext) = CORRESPONDING t_messages_ext( msg_int->* ).
      CASE msg_ext-msg_type.
        WHEN 'I'.
          msg_ext-msg_type_icon = '0'.
        WHEN 'E'.
          APPEND VALUE #( fname = 'MSG_TYPE' color = VALUE #( col = 6 ) ) TO msg_ext-color.
          msg_ext-msg_type_icon = '1'.
        WHEN 'W'.
          APPEND VALUE #( fname = 'MSG_TYPE' color = VALUE #( col = 7 ) ) TO msg_ext-color.
          msg_ext-msg_type_icon = '2'.
        WHEN 'S'.
          APPEND VALUE #( fname = 'MSG_TYPE' color = VALUE #( col = 5 ) ) TO msg_ext-color.
          msg_ext-msg_type_icon = '3'.
      ENDCASE.

      msg_ext-details_icon = COND #( WHEN msg_int->is_sap_msg = abap_true THEN '@35@' ELSE '@16@' ).

      <row_ext> = CORRESPONDING #( msg_ext ).
      IF logger->context->exists( ).
        DATA(context_data) = logger->context->convert_string_to_data( msg_int->context_values ).
        ASSIGN context_data->* TO FIELD-SYMBOL(<context_data>).
        MOVE-CORRESPONDING <context_data> TO <row_ext>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_messages_filter.
    DATA(filters) = messages_alv->alv_table->get_filters( ).
    filters->clear( ).
    filters->add_filter( columnname = 'MSG_TYPE' sign = 'I' option = 'EQ' low = CONV #( msg_type ) ).
    messages_alv->alv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDMETHOD.

  METHOD on_header_double_click.
    IF row = 0.
      RETURN.
    ENDIF.

    prepare_messages_alv( COND #( WHEN logger_to_display IS BOUND THEN logger_to_display
                                  ELSE zcl_ed_logger_factory=>open_logger( uuid = headers[ row ]-uuid ) ) ).
  ENDMETHOD.

  METHOD on_header_link_click.
    IF row = 0.
      RETURN.
    ENDIF.
    CASE column.
      WHEN 'UUID' OR 'HAS_ERRORS' OR 'HAS_WARNINGS'.
        prepare_messages_alv( COND #( WHEN logger_to_display IS BOUND THEN logger_to_display
                                      ELSE zcl_ed_logger_factory=>open_logger( uuid = headers[ row ]-uuid ) ) ).
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
    DATA(selected) = REF #( messages_int[ row ] ).

    CASE column.
      WHEN 'DETAILS_ICON'.
        IF selected->is_sap_msg = abap_true.
          zcl_ed_docu=>show( dokclass = 'NA' dokname = |{ selected->sap_msg-msgid }{ selected->sap_msg-msgno }|
              msg_var_1 = selected->sap_msg-msgv1 msg_var_2 = selected->sap_msg-msgv2
              msg_var_3 = selected->sap_msg-msgv3  msg_var_4 = selected->sap_msg-msgv4 ).

        ELSEIF selected->is_json = abap_true.
          cl_demo_output=>display_json( messages_int[ row ]-msg  ).

        ELSE.
          cl_demo_output=>display( messages_int[ row ]-msg  ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD on_close.
    cleanup( ).
  ENDMETHOD.

  METHOD zif_ed_logger_display~modeless_window_exists.
    exists = modeless_window.
  ENDMETHOD.

  METHOD refresh_single_log_display.
    CLEAR:headers.
    APPEND CORRESPONDING #( logger_to_display->log ) TO headers.
    IF headers_alv IS BOUND.
      headers_alv->alv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ENDIF.
    prepare_messages_alv( logger_to_display ).
  ENDMETHOD.

ENDCLASS.
