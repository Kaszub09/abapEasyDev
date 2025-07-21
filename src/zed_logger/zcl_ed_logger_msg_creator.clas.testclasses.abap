"=================================================================
"-----------------------------------------------------------------
CLASS tcl_exceptions DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      setup,
      log_exception FOR TESTING,
      drilldown_exception_1lvl FOR TESTING,
      dont_drilldown_ex_above_1lvl FOR TESTING,
      raise_ex IMPORTING count TYPE i RAISING zcx_ed_exception,
      check_exception IMPORTING exception_id TYPE i msg TYPE zif_ed_logger=>t_log_message.

    DATA:
      settings TYPE REF TO zif_ed_logger=>t_settings,
      msg_type TYPE msgty,
      cut      TYPE REF TO zcl_ed_logger_msg_creator.
ENDCLASS.

CLASS tcl_exceptions IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
    settings = NEW #( exception_drilldown_level = 2 ). "Means get at max 2 previous ex
    msg_type = 'E'.
  ENDMETHOD.

  METHOD log_exception.
    TRY.
        raise_ex( 1 ).
      CATCH zcx_ed_exception INTO DATA(zcx).
        DATA(msgs) = cut->get_msgs( settings = settings obj = zcx msg_type = msg_type ).
    ENDTRY.
    cl_abap_unit_assert=>assert_true( xsdbool( lines( msgs ) = 1 ) ).
    check_exception( exception_id = 1 msg = msgs[ 1 ] ).
  ENDMETHOD.

  METHOD drilldown_exception_1lvl.
    TRY.
        raise_ex( 2 ).

      CATCH zcx_ed_exception INTO DATA(zcx).
        DATA(msgs) = cut->get_msgs( settings = settings obj = zcx msg_type = msg_type ).
    ENDTRY.
    cl_abap_unit_assert=>assert_true( xsdbool( lines( msgs ) = 2 ) ).
    check_exception( exception_id = 2 msg = msgs[ 1 ] ).
    check_exception( exception_id = 1 msg = msgs[ 2 ] ).
  ENDMETHOD.

  METHOD dont_drilldown_ex_above_1lvl.
    TRY.
        raise_ex( 5 ).

      CATCH zcx_ed_exception INTO DATA(zcx).
        DATA(msgs) = cut->get_msgs( settings = settings obj = zcx msg_type = msg_type ).
    ENDTRY.
    cl_abap_unit_assert=>assert_true( xsdbool( lines( msgs ) = 3 ) ).
    check_exception( exception_id = 5 msg = msgs[ 1 ] ).
    check_exception( exception_id = 4 msg = msgs[ 2 ] ).
    check_exception( exception_id = 3 msg = msgs[ 3 ] ).
  ENDMETHOD.

  METHOD raise_ex.
    DATA ex TYPE REF TO zcx_ed_exception.

    DATA(index) = 1.
    WHILE index <= count.
      ex = NEW zcx_ed_exception( custom_message = |Exception { index }| previous = ex ).
      index = index + 1.
    ENDWHILE.

    RAISE EXCEPTION ex.
  ENDMETHOD.

  METHOD check_exception.
    cl_abap_unit_assert=>assert_equals( act = msg-msg_type exp = msg_type ).
    cl_abap_unit_assert=>assert_equals( act = msg-msg exp = |Exception { exception_id }| ).
    cl_abap_unit_assert=>assert_false( msg-is_json ).
    cl_abap_unit_assert=>assert_false( msg-is_sap_msg ).
    cl_abap_unit_assert=>assert_equals( act = msg-sap_msg exp = VALUE zif_ed_logger=>t_sap_msg( ) ).
    cl_abap_unit_assert=>assert_equals( act = msg-context_values exp = space ).
  ENDMETHOD.
ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
CLASS tcl_unsupported DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      setup,
      raise_err_on_class FOR TESTING.

    DATA:
      settings TYPE REF TO zif_ed_logger=>t_settings,
      msg_type TYPE msgty,
      cut      TYPE REF TO zcl_ed_logger_msg_creator.
ENDCLASS.

CLASS tcl_unsupported IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
    settings = NEW #( ). "Means get at max 2 previous ex
    msg_type = 'E'.
  ENDMETHOD.

  METHOD raise_err_on_class.
    TRY.
        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(msgs) = cut->get_msgs( settings = settings obj = cut msg_type = msg_type ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_ed_exception INTO DATA(zcx). " TODO: variable is assigned but never used (ABAP cleaner)
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
CLASS tcl_data_element DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      setup,
      log_char FOR TESTING,
      log_dec FOR TESTING,
      log_ref_to_char FOR TESTING,
      check_data_element IMPORTING text TYPE string msg TYPE zif_ed_logger=>t_log_message.

    DATA:
      char     TYPE c LENGTH 20 VALUE 'CHAR TO LOG',
      dec      TYPE decfloat34 VALUE '123456.789',
      settings TYPE REF TO zif_ed_logger=>t_settings,
      msg_type TYPE msgty,
      cut      TYPE REF TO zcl_ed_logger_msg_creator.
ENDCLASS.

CLASS tcl_data_element IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
    settings = NEW #( ).
    msg_type = 'S'.
  ENDMETHOD.

  METHOD log_char.
    DATA(msgs) = cut->get_msgs( settings = settings obj = char msg_type = msg_type ).
    cl_abap_unit_assert=>assert_true( xsdbool( lines( msgs ) = 1 ) ).
    check_data_element( msg = msgs[ 1 ] text = CONV #( char ) ).
  ENDMETHOD.

  METHOD log_dec.
    DATA(msgs) = cut->get_msgs( settings = settings obj = dec msg_type = msg_type ).
    cl_abap_unit_assert=>assert_true( xsdbool( lines( msgs ) = 1 ) ).
    check_data_element( msg = msgs[ 1 ] text = |{ dec }| ).
  ENDMETHOD.

  METHOD log_ref_to_char.
    DATA(msgs) = cut->get_msgs( settings = settings obj = REF #( char ) msg_type = msg_type ).
    cl_abap_unit_assert=>assert_true( xsdbool( lines( msgs ) = 1 ) ).
    check_data_element( msg = msgs[ 1 ] text = CONV #( char ) ).
  ENDMETHOD.

  METHOD check_data_element.
    cl_abap_unit_assert=>assert_equals( act = msg-msg_type exp = msg_type ).
    cl_abap_unit_assert=>assert_equals( act = msg-msg exp = text ).
    cl_abap_unit_assert=>assert_false( msg-is_json ).
    cl_abap_unit_assert=>assert_false( msg-is_sap_msg ).
    cl_abap_unit_assert=>assert_equals( act = msg-sap_msg exp = VALUE zif_ed_logger=>t_sap_msg( ) ).
    cl_abap_unit_assert=>assert_equals( act = msg-context_values exp = space ).
  ENDMETHOD.
ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
CLASS tcl_non_sap_msg_struct_tab DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_struct,
        object   TYPE trobjtype,
        obj_name TYPE sobj_name,
        object2  TYPE trobjtype,
        dec      TYPE decfloat34,
        int      TYPE i,
      END OF t_struct,
      tt_struct TYPE STANDARD TABLE OF t_struct WITH EMPTY KEY.

    METHODS:
      setup,
      log_struct_compressed FOR TESTING,
      log_struct_uncompressed FOR TESTING,
      log_table FOR TESTING,
      check_json IMPORTING json TYPE string msg TYPE zif_ed_logger=>t_log_message.

    DATA:
      settings TYPE REF TO zif_ed_logger=>t_settings,
      msg_type TYPE msgty,
      cut      TYPE REF TO zcl_ed_logger_msg_creator.
ENDCLASS.

CLASS tcl_non_sap_msg_struct_tab IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
    settings = NEW #( compress_json = abap_true ).
    msg_type = 'I'.
  ENDMETHOD.

  METHOD log_struct_compressed.
    DATA(obj) = VALUE t_struct( object = 'OBJ' dec = '123456.789' int = 1 ).
    DATA(msgs) = cut->get_msgs( settings = settings obj = obj msg_type = msg_type ).

    cl_abap_unit_assert=>assert_true( xsdbool( lines( msgs ) = 1 ) ).
    check_json( msg = msgs[ 1 ] json = /ui2/cl_json=>serialize( data = obj compress = abap_true ) ).
  ENDMETHOD.

  METHOD log_struct_uncompressed.
    settings = NEW #( compress_json = abap_false ).
    DATA(obj) = VALUE t_struct( object = 'OBJ' dec = '123456.789' int = 1 ).
    DATA(msgs) = cut->get_msgs( settings = settings obj = obj msg_type = msg_type ).

    cl_abap_unit_assert=>assert_true( xsdbool( lines( msgs ) = 1 ) ).
    check_json( msg = msgs[ 1 ] json = /ui2/cl_json=>serialize( data = obj compress = abap_false ) ).
  ENDMETHOD.

  METHOD log_table.
    DATA(obj) = VALUE tt_struct( ( object = 'OBJ' dec = '123456.789' int = 1 ) ( object = 'OBJ2' dec = '123' int = 123 ) ).
    DATA(msgs) = cut->get_msgs( settings = settings obj = obj msg_type = msg_type ).

    cl_abap_unit_assert=>assert_true( xsdbool( lines( msgs ) = 1 ) ).
    check_json( msg = msgs[ 1 ] json = /ui2/cl_json=>serialize( data = obj compress = abap_true ) ).
  ENDMETHOD.

  METHOD check_json.
    cl_abap_unit_assert=>assert_equals( act = msg-msg_type exp = msg_type ).
    cl_abap_unit_assert=>assert_equals( act = msg-msg exp = json ).
    cl_abap_unit_assert=>assert_true( msg-is_json ).
    cl_abap_unit_assert=>assert_false( msg-is_sap_msg ).
    cl_abap_unit_assert=>assert_equals( act = msg-sap_msg exp = VALUE zif_ed_logger=>t_sap_msg( ) ).
    cl_abap_unit_assert=>assert_equals( act = msg-context_values exp = space ).
  ENDMETHOD.
ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
CLASS tcl_sap_msg DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_struct,
        object   TYPE trobjtype,
        obj_name TYPE sobj_name,
        object2  TYPE trobjtype,
        dec      TYPE decfloat34,
        int      TYPE i,
      END OF t_struct.

    CONSTANTS:
      c_msgnr TYPE msgnr VALUE '001',
      c_msgid TYPE msgid VALUE 'ZED_LOGGER_TEST',
      c_msgty TYPE msgty VALUE 'W',
      c_p1    TYPE string VALUE 'PARAMETER_1',
      c_p2    TYPE string VALUE 'PARAMETER_2',
      c_p3    TYPE string VALUE 'PARAMETER_3',
      c_p4    TYPE string VALUE 'PARAMETER_4'.

    METHODS:
      setup,
      log_syst FOR TESTING,
      log_bapiret2 FOR TESTING,
      log_bapi_coru_return FOR TESTING,
      log_bdcmsgcoll FOR TESTING,
      log_bdcmsgcoll_tab FOR TESTING,
      expect_sap_msg IMPORTING msg TYPE zif_ed_logger=>t_log_message.

    DATA:
      msg001_text TYPE string,
      settings    TYPE REF TO zif_ed_logger=>t_settings,
      cut         TYPE REF TO zcl_ed_logger_msg_creator.
ENDCLASS.

CLASS tcl_sap_msg IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
    settings = NEW #( compress_json = abap_true ).
    MESSAGE ID c_msgid TYPE c_msgty NUMBER c_msgnr WITH c_p1 c_p2 c_p3 c_p4 INTO msg001_text.
  ENDMETHOD.

  METHOD log_syst.
    MESSAGE ID c_msgid TYPE c_msgty NUMBER c_msgnr WITH c_p1 c_p2 c_p3 c_p4 INTO DATA(dummy) ##NEEDED.

    DATA(msgs) = cut->get_msgs( settings = settings obj = sy msg_type = space ).

    cl_abap_unit_assert=>assert_true( xsdbool( lines( msgs ) = 1 ) ).
    expect_sap_msg( msgs[ 1 ] ).
  ENDMETHOD.

  METHOD log_bapiret2.
    DATA(msgs) = cut->get_msgs( settings = settings obj = VALUE bapiret2( type = c_msgty id = c_msgid number = c_msgnr message = msg001_text
      message_v1 = c_p1 message_v2 = c_p2 message_v3 = c_p3 message_v4 = c_p4 ) msg_type = space ).

    cl_abap_unit_assert=>assert_true( xsdbool( lines( msgs ) = 1 ) ).
    expect_sap_msg( msgs[ 1 ] ).
  ENDMETHOD.

  METHOD log_bapi_coru_return.
    DATA(msgs) = cut->get_msgs( settings = settings obj = VALUE bapi_coru_return( type = c_msgty id = c_msgid number = c_msgnr message = msg001_text
      message_v1 = c_p1 message_v2 = c_p2 message_v3 = c_p3 message_v4 = c_p4 ) msg_type = space ).

    cl_abap_unit_assert=>assert_true( xsdbool( lines( msgs ) = 1 ) ).
    expect_sap_msg( msgs[ 1 ] ).
  ENDMETHOD.

  METHOD log_bdcmsgcoll.
    DATA(msgs) = cut->get_msgs( settings = settings obj = VALUE bdcmsgcoll( msgtyp = c_msgty msgid = c_msgid msgnr = '001'
      msgv1 = c_p1 msgv2 = c_p2 msgv3 = c_p3 msgv4 = c_p4 ) msg_type = space ).

    cl_abap_unit_assert=>assert_true( xsdbool( lines( msgs ) = 1 ) ).
    expect_sap_msg( msgs[ 1 ] ).
  ENDMETHOD.

  METHOD log_bdcmsgcoll_tab.
    DATA: bdcmsgcoll_tab TYPE STANDARD TABLE OF bdcmsgcoll WITH EMPTY KEY.

    bdcmsgcoll_tab = VALUE #( ( msgtyp = c_msgty msgid = c_msgid msgnr = c_msgnr msgv1 = c_p1 msgv2 = c_p2 msgv3 = c_p3 msgv4 = c_p4  )
        ( msgtyp = 'E' msgid = c_msgid msgnr = '002' ) ).
    "Type S shouldn't matter and should be taken from sap_msg anyway
    DATA(msgs) = cut->get_msgs( settings = settings obj = bdcmsgcoll_tab msg_type = 'S' ).

    cl_abap_unit_assert=>assert_true( xsdbool( lines( msgs ) = 2 ) ).

    expect_sap_msg( msgs[ 1 ] ).
    "Second msg
    MESSAGE ID c_msgid TYPE c_msgty NUMBER '002' INTO DATA(msg002_text).
    cl_abap_unit_assert=>assert_equals( act = msgs[ 2 ]-msg_type exp = 'E' ).
    cl_abap_unit_assert=>assert_equals( act = msgs[ 2 ]-msg exp = msg002_text ).
    cl_abap_unit_assert=>assert_false( msgs[ 2 ]-is_json ).
    cl_abap_unit_assert=>assert_true( msgs[ 2 ]-is_sap_msg ).
    cl_abap_unit_assert=>assert_equals( act = msgs[ 2 ]-sap_msg
        exp = VALUE zif_ed_logger=>t_sap_msg( msgid = c_msgid msgno = '002' msgty = 'E' ) ).
    cl_abap_unit_assert=>assert_equals( act = msgs[ 2 ]-context_values exp = space ).
  ENDMETHOD.

  METHOD expect_sap_msg.
    MESSAGE ID c_msgid TYPE c_msgty NUMBER c_msgnr WITH c_p1 c_p2 c_p3 c_p4 INTO DATA(dummy).

    cl_abap_unit_assert=>assert_equals( act = msg-msg_type exp = c_msgty ).
    cl_abap_unit_assert=>assert_equals( act = msg-msg exp = dummy ).
    cl_abap_unit_assert=>assert_false( msg-is_json ).
    cl_abap_unit_assert=>assert_true( msg-is_sap_msg ).
    cl_abap_unit_assert=>assert_equals( act = msg-sap_msg exp = VALUE zif_ed_logger=>t_sap_msg(
       msgid = c_msgid msgno = c_msgnr msgty = c_msgty msgv1 = c_p1 msgv2 = c_p2 msgv3 = c_p3 msgv4 = c_p4 ) ).
    cl_abap_unit_assert=>assert_equals( act = msg-context_values exp = space ).
  ENDMETHOD.
ENDCLASS.
