CLASS zcl_ed_logger DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ed_logger_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_logger.

    ALIASES:
      add FOR zif_ed_logger~add,
      i FOR zif_ed_logger~i,
      w FOR zif_ed_logger~w,
      e FOR zif_ed_logger~e,
      s FOR zif_ed_logger~s,
      save FOR zif_ed_logger~save,
      settings FOR zif_ed_logger~settings,
      log FOR zif_ed_logger~log,
      context FOR zif_ed_logger~context,
      hex FOR zif_ed_logger~hex.

  PRIVATE SECTION.
    METHODS:
      update_log_metainfo.

    DATA:
      msg_creator TYPE REF TO zcl_ed_logger_msg_creator.
ENDCLASS.

CLASS zcl_ed_logger IMPLEMENTATION.
  METHOD zif_ed_logger~add.
    IF settings->logging_level < level.
      RETURN.
    ENDIF.

    DATA context_string TYPE string.
    IF context_data IS BOUND.
      IF NOT context->exists( ).
        zcl_ed_msg=>throw( text = TEXT-e01 ).
      ENDIF.
      context_string = context->convert_data_to_string( context_data ).
    ENDIF.

    LOOP AT msg_creator->get_msgs( settings = settings obj = obj msg_type = msg_type ) REFERENCE INTO DATA(msg).
      msg->context_values = context_string.
      msg->level = level.
      GET TIME.
      msg->created_at_date = sy-datum.
      msg->created_at_time = sy-uzeit.
      APPEND msg->* TO log-messages.
    ENDLOOP.

    update_log_metainfo( ).
    IF settings->autosave = abap_true.
      save( ).
    ENDIF.

    logger = me.
  ENDMETHOD.

  METHOD zif_ed_logger~i.
    logger = zif_ed_logger~add( obj = obj msg_type = 'I' level = level context_data = context_data ).
  ENDMETHOD.

  METHOD zif_ed_logger~s.
    logger = zif_ed_logger~add( obj = obj msg_type = 'S' level = level context_data = context_data ).
  ENDMETHOD.

  METHOD zif_ed_logger~w.
    logger = zif_ed_logger~add( obj = obj msg_type = 'W' level = level context_data = context_data ).
  ENDMETHOD.

  METHOD zif_ed_logger~e.
    logger = zif_ed_logger~add( obj = obj msg_type = 'E' level = level context_data = context_data ).
  ENDMETHOD.

  METHOD zif_ed_logger~save.
    DATA(log_db) = CORRESPONDING zed_logs( log ).
    DATA(log_msg_db) = VALUE zed_logs_msg( uuid = log-uuid messages_hex = hex->messages_to_hex( log-messages ) ).
    log_msg_db-log_size = xstrlen( log_msg_db-messages_hex ).

    "TODO - check update task? save lock?
    IF settings->second_con-use = abap_true.
      MODIFY zed_logs CONNECTION (settings->second_con-name) FROM @log_db.
      MODIFY zed_logs_msg CONNECTION (settings->second_con-name) FROM @log_msg_db.
      IF settings->second_con-commit = abap_true.
        COMMIT CONNECTION (settings->second_con-name).
      ENDIF.

    ELSE.
      MODIFY zed_logs FROM @log_db.
      MODIFY zed_logs_msg FROM @log_msg_db.
    ENDIF.
    logger = me.
  ENDMETHOD.

  METHOD update_log_metainfo.
    log-messsages_count = lines( log-messages ).
    log-has_errors = xsdbool( line_exists( log-messages[ msg_type = 'E' ] ) ).
    log-has_warnings = xsdbool( line_exists( log-messages[ msg_type = 'W' ] ) ).

    log-last_change_by = sy-uname.
    GET TIME.
    log-last_change_date = sy-datum.
    log-last_change_time = sy-uzeit.
    log-context_col_info = context->get_context_col_info( ).
  ENDMETHOD.

  METHOD zif_ed_logger~clear_all_logs.
    FREE: log-messages.
    update_log_metainfo( ).
  ENDMETHOD.
ENDCLASS.
