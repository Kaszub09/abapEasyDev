CLASS zcl_ed_logger_factory DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ed_logger_factory_injector.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! @parameter settings | <p class="shorttext synchronized">Uses default from <em>create_settings</em> if not supplied</p>
      "! @parameter context | <p class="shorttext synchronized" lang="en">Use <em>create_context_from_ref</em> if needed, otherwise leave empty.
      "! <br/>Must be from simple structure with dictionary data elements.
      "! <br/>Data is stored as [name]-[data element] combination, separated by '&#124', in field <em>context_clo_info</em>.
      create_logger IMPORTING settings      TYPE REF TO zif_ed_logger=>t_settings OPTIONAL
                              category      TYPE zted_log_category OPTIONAL
                              ext_id        TYPE zted_log_external_identifier OPTIONAL
                              expiry_date   TYPE zted_log_expiry_date DEFAULT '99991231'
                              context       TYPE REF TO zcl_ed_logger_context OPTIONAL
                    RETURNING VALUE(logger) TYPE REF TO zif_ed_logger
                    RAISING   zcx_ed_exception,
      "! @parameter settings | <p class="shorttext synchronized">Uses default from <em>create_settings</em> if not supplied</p>
      open_logger IMPORTING uuid          TYPE zted_log_uuid
                            settings      TYPE REF TO zif_ed_logger=>t_settings OPTIONAL
                  RETURNING VALUE(logger) TYPE REF TO zif_ed_logger
                  RAISING   zcx_ed_exception,
      create_settings IMPORTING autosave                  TYPE abap_bool DEFAULT abap_true
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
      create_selection IMPORTING uuid                TYPE zcl_ed_logger_selection=>ttr_uuid OPTIONAL
                                 category            TYPE zcl_ed_logger_selection=>ttr_category OPTIONAL
                                 external_identifier TYPE zcl_ed_logger_selection=>ttr_external_identifier OPTIONAL
                                 created_by          TYPE zcl_ed_logger_selection=>ttr_created_by OPTIONAL
                                 created_at_date     TYPE zcl_ed_logger_selection=>ttr_created_at_date OPTIONAL
                                 created_at_time     TYPE zcl_ed_logger_selection=>ttr_created_at_time OPTIONAL
                                 transaction         TYPE zcl_ed_logger_selection=>ttr_transaction OPTIONAL
                                 expiry_date         TYPE zcl_ed_logger_selection=>ttr_expiry_date OPTIONAL
                                 is_batch            TYPE zcl_ed_logger_selection=>ttr_is_batch OPTIONAL
                                 last_change_by      TYPE zcl_ed_logger_selection=>ttr_last_change_by OPTIONAL
                                 last_change_date    TYPE zcl_ed_logger_selection=>ttr_last_change_date OPTIONAL
                                 last_change_time    TYPE zcl_ed_logger_selection=>ttr_last_change_time OPTIONAL
                                 messsages_count     TYPE zcl_ed_logger_selection=>ttr_messsages_count OPTIONAL
                                 has_warnings        TYPE zcl_ed_logger_selection=>ttr_has_warnings OPTIONAL
                                 has_errors          TYPE zcl_ed_logger_selection=>ttr_has_errors OPTIONAL
                       RETURNING VALUE(sel)          TYPE REF TO zcl_ed_logger_selection,
      create_display RETURNING VALUE(display) TYPE REF TO zif_ed_logger_display.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Empty if not used</p>
      create_context_from_col_info IMPORTING col_info       TYPE zted_log_context_col_info
                                   RETURNING VALUE(context) TYPE REF TO zcl_ed_logger_context.

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
    logger_base->context =  COND #( WHEN context IS BOUND THEN context ELSE create_context_from_col_info( || ) ).
    logger_base->msg_creator = NEW #( ).
    logger_base->update_log_metainfo( ).

    logger = logger_base.
  ENDMETHOD.

  METHOD open_logger.
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
    logger_base->context = create_context_from_col_info( logger_base->log-context_col_info ).
    logger_base->msg_creator = NEW #(  ).
    logger_base->log-messages = logger_base->hex->hex_to_messages( log_db-zed_logs_msg-messages_hex ).

    logger = logger_base.
  ENDMETHOD.

  METHOD create_settings.
    settings = NEW #( autosave = autosave exception_drilldown_level = exception_drilldown_level logging_level = logging_level
        compress_json = compress_json
        second_con = VALUE #( name = second_conn_name use = second_conn_use commit = second_conn_commit  ) ).
  ENDMETHOD.

  METHOD create_context_from_ref.
    context = NEW #( ).
    context->context_exists = abap_true.
    context->struct = CAST #( cl_abap_structdescr=>describe_by_data_ref( sample_data ) ).
  ENDMETHOD.

  METHOD create_context_from_col_info.
    context = NEW #( ).
    context->context_exists = abap_false.

    IF strlen( col_info ) = 0.
      RETURN.
    ENDIF.

    context->context_exists = abap_true.
    DATA(components) = VALUE cl_abap_structdescr=>component_table( ).

    SPLIT col_info AT |\|| INTO TABLE DATA(columns_info).
    LOOP AT columns_info REFERENCE INTO DATA(info).
      SPLIT info->* AT '-' INTO TABLE DATA(col).
      APPEND VALUE #(    name = col[ 1 ] type = CAST #( cl_abap_elemdescr=>describe_by_name( col[ 2 ] ) )   ) TO components.
    ENDLOOP.
    context->struct = cl_abap_structdescr=>get( components ).
  ENDMETHOD.

  METHOD create_selection.
    sel = NEW #( ).
    sel->uuid = uuid.
    sel->category = category.
    sel->external_identifier = external_identifier.
    sel->created_by = created_by.
    sel->created_at_date = created_at_date.
    sel->created_at_time = created_at_time.
    sel->transaction = transaction.
    sel->expiry_date = expiry_date.
    sel->is_batch = is_batch.
    sel->last_change_by = last_change_by.
    sel->last_change_date = last_change_date.
    sel->last_change_time = last_change_time.
    sel->messsages_count = messsages_count.
    sel->has_warnings = has_warnings.
    sel->has_errors = has_errors.
  ENDMETHOD.


  METHOD create_display.
    IF logger_display_mock IS BOUND.
      display = logger_display_mock.
      RETURN.
    ENDIF.
    display = NEW zcl_ed_logger_display( ).
  ENDMETHOD.
ENDCLASS.
