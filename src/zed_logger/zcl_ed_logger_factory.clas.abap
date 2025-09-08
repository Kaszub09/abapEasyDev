CLASS zcl_ed_logger_factory DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ed_logger_factory_injector.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! @parameter settings | <p class="shorttext synchronized">Uses default from <em>create_settings</em> if not supplied</p>
      "! @parameter context | <p class="shorttext synchronized" lang="en">Use <em>create_context_from_ref</em> if needed, otherwise leave empty.
      "! <br/>Should be simple structure with dictionary data elements (stuff like decfloat34/int is also permitted).
      "! <br/>Data is stored as [name][zcl_ed_logger_context=&gt;c_col_info_delimiter-name_type][data element] combination,
      "! separated by zcl_ed_logger_context=&gt;c_col_info_delimiter-component, in field <em>context_col_info</em>.
      create_logger IMPORTING settings      TYPE REF TO zif_ed_logger=>t_settings OPTIONAL
                              category      TYPE zted_log_category OPTIONAL
                              ext_id        TYPE zted_log_external_identifier OPTIONAL
                              expiry_date   TYPE zted_log_expiry_date DEFAULT '99991231'
                              context       TYPE REF TO zcl_ed_logger_context OPTIONAL
                    RETURNING VALUE(logger) TYPE REF TO zif_ed_logger
                    RAISING   zcx_ed_exception,
      "! <p class="shorttext synchronized" lang="en">Shortcut for creating logger
      "! <br/> which is not meant to be stored in DB but just displayed in transaction.</p>
      create_temporary_logger IMPORTING ext_id        TYPE zted_log_external_identifier OPTIONAL
                                        context       TYPE REF TO zcl_ed_logger_context OPTIONAL
                              RETURNING VALUE(logger) TYPE REF TO zif_ed_logger
                              RAISING   zcx_ed_exception,
      "! @parameter settings | <p class="shorttext synchronized">Uses default from <em>create_settings</em> if not supplied</p>
      open_logger IMPORTING uuid          TYPE zted_log_uuid
                            settings      TYPE REF TO zif_ed_logger=>t_settings OPTIONAL
                  RETURNING VALUE(logger) TYPE REF TO zif_ed_logger
                  RAISING   zcx_ed_exception,
      create_settings IMPORTING autosave_use              TYPE abap_bool DEFAULT abap_true
                                autosave_only_if_errors   TYPE abap_bool DEFAULT abap_false
                                logging_level             TYPE zted_log_detail_level DEFAULT zif_ed_logger=>c_log_level-standard
                                exception_drilldown_level TYPE i DEFAULT 5
                                compress_json             TYPE abap_bool DEFAULT abap_true
                                second_conn_name          TYPE string DEFAULT zif_ed_logger=>c_2nd_con_name
                                second_conn_use           TYPE abap_bool DEFAULT abap_true
                                second_conn_commit        TYPE abap_bool DEFAULT abap_true
                      RETURNING VALUE(settings)           TYPE REF TO zif_ed_logger=>t_settings,
      "! @parameter sample_data | <p class="shorttext synchronized" lang="en">Must be simple structure with dictionary data elements.</p>
      create_context_from_ref IMPORTING sample_data    TYPE REF TO data
                              RETURNING VALUE(context) TYPE REF TO zcl_ed_logger_context,
      create_display RETURNING VALUE(display) TYPE REF TO zif_ed_logger_display.

  PRIVATE SECTION.
    CLASS-METHODS:
      fill_logger_extensions IMPORTING logger_base TYPE REF TO zcl_ed_logger.

    CLASS-DATA:
      logger_mock         TYPE REF TO zif_ed_logger,
      logger_display_mock TYPE REF TO zif_ed_logger_display.
ENDCLASS.

CLASS zcl_ed_logger_factory IMPLEMENTATION.
  METHOD create_logger.
    IF logger_mock IS BOUND.
      logger = logger_mock.
      RETURN.
    ENDIF.

    DATA(logger_base) = NEW zcl_ed_logger( ).
    GET TIME.
    TRY.
        logger_base->log = VALUE #( uuid = cl_system_uuid=>create_uuid_x16_static( )
            mandt = sy-mandt category = category external_identifier = ext_id expiry_date = expiry_date
            created_by = sy-uname created_at_date = sy-datum created_at_time = sy-uzeit tcode = sy-tcode is_batch = sy-batch ).
      CATCH cx_uuid_error INTO DATA(cx).
        zcx_ed_exception=>throw( |{ cx->get_text( ) }| ).
    ENDTRY.
    logger_base->hex = NEW #( ).
    logger_base->settings = COND #( WHEN settings IS BOUND THEN settings ELSE create_settings( ) ).
    logger_base->context = COND #( WHEN context IS BOUND THEN context ELSE zcl_ed_logger_context=>create_empty( ) ).
    logger_base->msg_creator = NEW #( ).
    logger_base->update_log_metainfo( ).

    fill_logger_extensions( logger_base ).

    logger = logger_base.
  ENDMETHOD.

  METHOD create_temporary_logger.
    logger = create_logger( ext_id = ext_id context = context
        settings = zcl_ed_logger_factory=>create_settings( autosave_use = abap_false ) ).
  ENDMETHOD.

  METHOD open_logger.
    IF logger_mock IS BOUND.
      logger = logger_mock.
      RETURN.
    ENDIF.

    DATA(logger_base) = NEW zcl_ed_logger( ).
    SELECT SINGLE * FROM zed_logs
            LEFT JOIN zed_logs_msg ON zed_logs_msg~uuid = zed_logs~uuid
        WHERE zed_logs~uuid = @uuid
        INTO @DATA(log_db).
    IF sy-subrc <> 0.
      zcl_ed_msg=>throw( text = TEXT-e01 v1 = uuid ).
    ENDIF.

    logger_base->log = CORRESPONDING #( log_db-zed_logs ).
    logger_base->hex = NEW #( ).
    logger_base->settings = COND #( WHEN settings IS BOUND THEN settings ELSE create_settings( ) ).
    logger_base->context = zcl_ed_logger_context=>create_from_col_info( logger_base->log-context_col_info ).
    logger_base->msg_creator = NEW #( ).
    logger_base->log-messages = logger_base->hex->hex_to_messages( log_db-zed_logs_msg-messages_hex ).

    logger = logger_base.
  ENDMETHOD.

  METHOD create_settings.
    settings = NEW #( autosave = VALUE #( use = autosave_use only_if_errors = autosave_only_if_errors )
        exception_drilldown_level = exception_drilldown_level logging_level = logging_level compress_json = compress_json
        second_con = VALUE #( name = second_conn_name use = second_conn_use commit = second_conn_commit  ) ).
  ENDMETHOD.

  METHOD create_context_from_ref.
    context = zcl_ed_logger_context=>create_from_ref( sample_data ).
  ENDMETHOD.

  METHOD create_display.
    IF logger_display_mock IS BOUND.
      display = logger_display_mock.
      RETURN.
    ENDIF.
    display = NEW zcl_ed_logger_display( ).
  ENDMETHOD.

  METHOD fill_logger_extensions.
    DATA(ext_msg) = NEW zcl_ed_logger_ext_msg( ).
    ext_msg->logger = logger_base.

    logger_base->zif_ed_logger~ext-msg = ext_msg.
  ENDMETHOD.
ENDCLASS.
