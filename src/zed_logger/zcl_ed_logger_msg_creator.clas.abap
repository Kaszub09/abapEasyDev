CLASS zcl_ed_logger_msg_creator DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ed_logger_factory.

  PUBLIC SECTION.
    METHODS:
      get_msgs IMPORTING settings    TYPE REF TO zif_ed_logger=>t_settings
                         obj         TYPE any
                         msg_type    TYPE msgty
               RETURNING VALUE(msgs) TYPE zif_ed_logger=>tt_log_message
               RAISING   zcx_ed_exception.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS:
      get_msgs_from_exception IMPORTING exception   TYPE REF TO cx_root
                              RETURNING VALUE(msgs) TYPE zif_ed_logger=>tt_log_message,
      get_msgs_from_element IMPORTING obj         TYPE any
                            RETURNING VALUE(msgs) TYPE zif_ed_logger=>tt_log_message,
      get_msgs_from_struct IMPORTING obj          TYPE any
                                     struct_descr TYPE REF TO cl_abap_structdescr
                           RETURNING VALUE(msgs)  TYPE zif_ed_logger=>tt_log_message,
      get_msgs_from_table IMPORTING obj         TYPE any
                                    table_descr TYPE REF TO cl_abap_tabledescr
                          RETURNING VALUE(msgs) TYPE zif_ed_logger=>tt_log_message.

    DATA:
      settings TYPE REF TO zif_ed_logger=>t_settings.
ENDCLASS.

CLASS zcl_ed_logger_msg_creator IMPLEMENTATION.

  METHOD get_msgs.
    me->settings = settings.

    DATA(descr) = cl_abap_structdescr=>describe_by_data( obj ).
    IF descr IS INSTANCE OF cl_abap_classdescr AND obj IS INSTANCE OF cx_root.
      msgs = get_msgs_from_exception( CAST #( obj ) ).

    ELSE.
      IF descr IS INSTANCE OF cl_abap_elemdescr.
        msgs = get_msgs_from_element( obj ).
      ELSEIF descr IS INSTANCE OF cl_abap_structdescr.
        msgs = get_msgs_from_struct( obj = obj struct_descr = CAST #( descr ) ).
      ELSEIF descr IS INSTANCE OF cl_abap_tabledescr.
        msgs = get_msgs_from_table( obj = obj table_descr = CAST #( descr ) ).
      ELSE.
        zcl_ed_msg=>throw( text = TEXT-c01 v1 = descr->get_relative_name( ) ).
      ENDIF.
    ENDIF.

    IF msg_type <> space.
      LOOP AT msgs REFERENCE INTO DATA(msg).
        msg->msg_type = msg_type.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_msgs_from_element.
    APPEND VALUE #( msg = |{ obj }| is_sap_msg = abap_false ) TO msgs.
  ENDMETHOD.

  METHOD get_msgs_from_exception.
    DATA(ex) = exception.
    APPEND VALUE #( msg = ex->get_text( ) is_sap_msg = abap_false ) TO msgs.

    DATA(drilldown_level) = 1.
    WHILE ex->previous IS BOUND AND drilldown_level <= settings->exception_drilldown_level.
      ex = ex->previous.
      APPEND VALUE #( msg = ex->get_text( ) ) TO msgs.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_msgs_from_struct.
    DATA msg TYPE zif_ed_logger=>t_log_message.

    DATA(name) = struct_descr->get_relative_name( ).

    CASE name.
      WHEN 'SYST'.
        msg-is_sap_msg = abap_true.
        msg-sap_msg = CORRESPONDING #( obj ).

      WHEN 'BAPIRET2'.
        msg-is_sap_msg = abap_true.
        msg-sap_msg = CORRESPONDING #( CORRESPONDING bapiret2( obj ) MAPPING msgid = id msgty = type msgno = number
            msgv1 = message_v1 msgv2 = message_v2 msgv3 = message_v3 msgv4 = message_v4 ).

      WHEN 'BAPI_CORU_RETURN'.
        msg-is_sap_msg = abap_true.
        msg-sap_msg = CORRESPONDING #( CORRESPONDING bapi_coru_return( obj ) MAPPING msgid = id msgty = type msgno = number
            msgv1 = message_v1 msgv2 = message_v2 msgv3 = message_v3 msgv4 = message_v4 ).

      WHEN 'BDCMSGCOLL'.
        msg-is_sap_msg = abap_true.
        msg-sap_msg = CORRESPONDING #( CORRESPONDING bdcmsgcoll( obj ) MAPPING msgty = msgtyp msgno = msgnr ).

      WHEN OTHERS.
        msg-is_json = abap_true.
        msg-msg = /ui2/cl_json=>serialize( data = obj compress = settings->compress_json ).
    ENDCASE.

    IF msg-is_sap_msg = abap_true AND msg-sap_msg-msgid IS NOT INITIAL.
      MESSAGE ID msg-sap_msg-msgid TYPE msg-sap_msg-msgty NUMBER msg-sap_msg-msgno
          WITH msg-sap_msg-msgv1 msg-sap_msg-msgv1 msg-sap_msg-msgv1 msg-sap_msg-msgv1 INTO msg-msg.
      msg-msg_type = msg-sap_msg-msgty.
    ENDIF.

    APPEND msg TO msgs.
  ENDMETHOD.

  METHOD get_msgs_from_table.
    DATA(line_descr) = table_descr->get_table_line_type( ).
    CASE line_descr->get_relative_name( ).
      WHEN 'SYST' OR 'BAPIRET2' OR 'BAPI_CORU_RETURN' OR 'BDCMSGCOLL'.
        "Table of standard messages, add them one by one
        FIELD-SYMBOLS <table> TYPE table.
        ASSIGN obj TO <table>.

        LOOP AT <table> ASSIGNING FIELD-SYMBOL(<row>).
          APPEND LINES OF get_msgs_from_struct( obj = <row> struct_descr = CAST #( line_descr ) ) TO msgs.
        ENDLOOP.

      WHEN OTHERS.
        "Some other table, so just export it to JSON
        APPEND VALUE #( is_json = abap_true is_sap_msg = abap_false msg = /ui2/cl_json=>serialize( data = obj compress = settings->compress_json ) ) TO msgs.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
