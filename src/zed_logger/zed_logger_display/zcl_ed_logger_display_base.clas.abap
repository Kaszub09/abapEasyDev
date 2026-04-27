CLASS zcl_ed_logger_display_base DEFINITION PUBLIC CREATE PRIVATE
  GLOBAL FRIENDS zcl_ed_logger_display_cont zcl_ed_logger_display_nonmodal zcl_ed_logger_display_modal.

  PROTECTED SECTION.
    METHODS:
      reset_state,
      create_alv IMPORTING container TYPE REF TO cl_gui_container,
      "! <p class="shorttext synchronized" lang="en">Loggers from all parameters are combined. Empty parameters are skipped.
      "! <br/> So e.g. using only logger displays only it, using logger+selection display logger+all logs found by selection.</p>
      "! @parameter messages_only | <p class="shorttext synchronized" lang="en">Can be true only if one logger is found!</p>
      set_settings_base IMPORTING logger        TYPE REF TO zif_ed_logger OPTIONAL
                                  logs          TYPE zif_ed_logger_display=>tt_log OPTIONAL
                                  selection     TYPE REF TO zif_ed_logger_display=>t_selection OPTIONAL
                                  messages_only TYPE abap_bool DEFAULT abap_true,
      refresh_base.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_message_display,
        msg_index       TYPE zted_messages_index,
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
      select_headers IMPORTING sel TYPE zif_ed_logger_display=>t_selection,
      create_headers_alv IMPORTING container TYPE REF TO cl_gui_container,
      create_messages_alv IMPORTING container TYPE REF TO cl_gui_container,

      display_messages IMPORTING logger TYPE REF TO zif_ed_logger,
      init_messages_display_tab IMPORTING logger TYPE REF TO zif_ed_logger RETURNING VALUE(display_tab) TYPE REF TO data,
      fill_messages_display_tab IMPORTING context TYPE REF TO zcl_ed_logger_context tab TYPE zif_ed_logger=>tt_log_message display_tab TYPE REF TO data,
      set_messages_filter IMPORTING msg_type TYPE msgty.

    METHODS:
      on_header_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING row column,
      on_header_link_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column,
      on_msg_link_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column.

    DATA:
      BEGIN OF headers,
        tab TYPE tt_header,
        alv TYPE REF TO zcl_ea_salv_table,
      END OF headers,
      BEGIN OF messages,
        tab         TYPE zif_ed_logger=>tt_log_message,
        alv         TYPE REF TO zcl_ea_salv_table,
        display_tab TYPE REF TO data,
        logger      TYPE REF TO zif_ed_logger,
      END OF messages.

    "--------------------------------------------------
    TYPES:
      BEGIN OF t_log_obj,
        uuid   TYPE zted_log_uuid,
        logger TYPE REF TO zif_ed_logger,
      END OF t_log_obj,
      tt_log_obj TYPE SORTED TABLE OF t_log_obj WITH UNIQUE KEY uuid.

    METHODS:
      add_logger IMPORTING logger TYPE REF TO zif_ed_logger,
      get_logger IMPORTING uuid TYPE zted_log_uuid RETURNING VALUE(logger) TYPE REF TO zif_ed_logger.

    DATA:
      is_log_single TYPE abap_bool,
      logs_obj      TYPE tt_log_obj,
      messages_only TYPE abap_bool.
ENDCLASS.

CLASS zcl_ed_logger_display_base IMPLEMENTATION.
  METHOD set_settings_base.
    reset_state( ).

    IF logger IS BOUND.
      add_logger( logger ).
    ENDIF.

    LOOP AT logs INTO DATA(log).
      add_logger( log ).
    ENDLOOP.

    IF selection IS BOUND.
      select_headers( selection->* ).
    ENDIF.

    me->messages_only = messages_only.
    IF lines( headers-tab ) <> 1.
      me->messages_only = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD reset_state.
    FREE:logs_obj, headers, messages.
  ENDMETHOD.

  METHOD add_logger.
    INSERT VALUE #( uuid = logger->log-uuid logger = logger ) INTO TABLE logs_obj.
    APPEND CORRESPONDING #( logger->log ) TO headers-tab.
  ENDMETHOD.

  METHOD select_headers.
    SELECT * FROM zed_logs
     WHERE uuid IN @sel-uuid AND category IN @sel-category AND external_identifier IN @sel-external_identifier
         AND created_by IN @sel-created_by AND created_at_date IN @sel-created_at_date
         AND created_at_time IN @sel-created_at_time AND tcode IN @sel-transaction AND expiry_date IN @sel-expiry_date
         AND is_batch IN @sel-is_batch AND last_change_by IN @sel-last_change_by
         AND last_change_date IN @sel-last_change_date AND last_change_time IN @sel-last_change_time
         AND messsages_count IN @sel-messsages_count AND has_warnings IN @sel-has_warnings AND has_errors IN @sel-has_errors
    APPENDING CORRESPONDING FIELDS OF TABLE @headers-tab.
  ENDMETHOD.

  METHOD create_alv.
    IF messages_only = abap_true.
      create_messages_alv( container ).
      display_messages( get_logger( headers-tab[ 1 ]-uuid ) ).

    ELSE.
      DATA(splitter) = NEW cl_gui_splitter_container( parent = container rows = 2 columns = 1 ).
      create_headers_alv( splitter->get_container( row = 1 column = 1 ) ).
      create_messages_alv( splitter->get_container( row = 2 column = 1 ) ).
    ENDIF.
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
    messages-logger = logger.
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
      DATA(index) = sy-tabix.
      APPEND INITIAL LINE TO <display_tab> ASSIGNING FIELD-SYMBOL(<display_row>).
      DATA(msg_display) = CORRESPONDING t_message_display( msg->* ).
      msg_display-msg_index = index.
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

  METHOD on_header_double_click.
    IF row = 0.
      RETURN.
    ENDIF.

    display_messages( get_logger( headers-tab[ row ]-uuid ) ).
  ENDMETHOD.

  METHOD on_header_link_click.
    IF row = 0.
      RETURN.
    ENDIF.

    CASE column.
      WHEN 'UUID' OR 'HAS_ERRORS' OR 'HAS_WARNINGS'.
        display_messages( get_logger( headers-tab[ row ]-uuid ) ).

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
    "After sorting, messages-display_tab can be in different order than messages-tab, so get tab index from display_tab
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.
    ASSIGN messages-display_tab->* TO <table>.
    ASSIGN COMPONENT 'MSG_INDEX' OF STRUCTURE <table>[ row ] TO FIELD-SYMBOL(<tab_index>).
    DATA(selected) = REF #( messages-tab[ <tab_index> ] ).

    CASE column.
      WHEN 'DETAILS_ICON'.
        IF selected->is_sap_msg = abap_true.
          zcl_ed_docu=>show( dokclass = 'NA' dokname = |{ selected->sap_msg-msgid }{ selected->sap_msg-msgno }|
              msg_var_1 = selected->sap_msg-msgv1 msg_var_2 = selected->sap_msg-msgv2
              msg_var_3 = selected->sap_msg-msgv3 msg_var_4 = selected->sap_msg-msgv4 ).

        ELSEIF selected->is_json = abap_true.
          cl_demo_output=>display_json( selected->msg ).

        ELSE.
          cl_demo_output=>display( selected->msg ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD get_logger.
    logger = VALUE #( logs_obj[ uuid = uuid ]-logger OPTIONAL ).
    IF logger IS NOT BOUND.
      logger = zcl_ed_logger_factory=>open_logger( uuid ).
      add_logger( logger ).
    ENDIF.
  ENDMETHOD.

  METHOD refresh_base.
    IF headers-alv IS BOUND.
      headers-alv->alv_table->refresh( ).
    ENDIF.

    IF messages-alv IS BOUND AND messages-logger IS BOUND.
      display_messages( messages-logger ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
