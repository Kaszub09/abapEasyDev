CLASS tcl_ed_msg DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS:
      c_msgnr TYPE msgnr VALUE '001',
      c_msgid TYPE msgid VALUE 'ZED_MSG_TEST',
      c_msgty TYPE msgty VALUE 'W',
      c_p1    TYPE string VALUE 'PARAMETER_1',
      c_p2    TYPE string VALUE 'PARAMETER_2',
      c_p3    TYPE string VALUE 'PARAMETER_3',
      c_p4    TYPE string VALUE 'PARAMETER_4'.

    CLASS-METHODS:
      class_setup.

    CLASS-DATA:
        expected_msg TYPE string.

    METHODS:
      get FOR TESTING,
      get_from_bapiret2 FOR TESTING,
      get_from_bapi_coru_return FOR TESTING,
      get_from_bdcmsgcoll FOR TESTING,
      get_from_sy FOR TESTING,
      throw FOR TESTING.

ENDCLASS.


CLASS tcl_ed_msg IMPLEMENTATION.
  METHOD class_setup.
    MESSAGE ID c_msgid TYPE c_msgty NUMBER c_msgnr WITH c_p1 c_p2 c_p3 c_p4 INTO expected_msg.
  ENDMETHOD.

  METHOD get.
    SELECT SINGLE text FROM t100 WHERE arbgb = @c_msgid AND msgnr = @c_msgnr INTO @DATA(text).
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_msg=>get( text = text v1 = c_p1 v2 = c_p2 v3 = c_p3 v4 = c_p4 )
        exp = expected_msg ).
  ENDMETHOD.

  METHOD get_from_bapiret2.
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_msg=>get_from_bapiret2( VALUE bapiret2(
            type = c_msgty id = c_msgid number = c_msgnr message = expected_msg
            message_v1 = c_p1 message_v2 = c_p2 message_v3 = c_p3 message_v4 = c_p4 ) )
        exp = expected_msg ).
  ENDMETHOD.

  METHOD get_from_bapi_coru_return.
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_msg=>get_from_bapi_coru_return( VALUE bapi_coru_return(
            type = c_msgty id = c_msgid number = c_msgnr message = expected_msg
            message_v1 = c_p1 message_v2 = c_p2 message_v3 = c_p3 message_v4 = c_p4 ) )
        exp = expected_msg ).
  ENDMETHOD.

  METHOD get_from_bdcmsgcoll.
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_msg=>get_from_bdcmsgcoll( VALUE bdcmsgcoll(
            msgtyp = c_msgty msgid = c_msgid msgnr = c_msgnr
            msgv1 = c_p1 msgv2 = c_p2 msgv3 = c_p3 msgv4 = c_p4 ) )
        exp = expected_msg ).
  ENDMETHOD.

  METHOD get_from_sy.
    MESSAGE ID c_msgid TYPE c_msgty NUMBER c_msgnr WITH c_p1 c_p2 c_p3 c_p4 INTO DATA(dummy).
    cl_abap_unit_assert=>assert_equals( act = zcl_ed_msg=>get_from_sy( )
        exp = expected_msg ).
  ENDMETHOD.

  METHOD throw.
    SELECT SINGLE text FROM t100 WHERE arbgb = @c_msgid AND msgnr = @c_msgnr INTO @DATA(text).

    TRY.
        zcl_ed_msg=>throw( text = text v1 = c_p1 v2 = c_p2 v3 = c_p3 v4 = c_p4 ).
        cl_abap_unit_assert=>fail( ).

      CATCH zcx_ed_exception INTO DATA(zcx).
        cl_abap_unit_assert=>assert_equals( act = zcx->get_text( ) exp = expected_msg ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
