"=================================================================
"-----------------------------------------------------------------
CLASS tcl_ext_msg DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_msg,
        msg1 TYPE string VALUE 'This is message 1. It''s quite long, so it can be tested against the 128 character limit. This is message 1. It''s quite long, so it can be tested against the 128 character limit. ',
        msg2 TYPE string VALUE 'Msg2',
      END OF c_msg.

    METHODS:
      setup,
      has_warnings FOR TESTING,
      has_errors FOR TESTING,
      has_warnings_no_errors FOR TESTING,
      get_as_string FOR TESTING,
      get_errors_as_string FOR TESTING.

    DATA:
      log      TYPE REF TO zif_ed_logger.
ENDCLASS.

CLASS tcl_ext_msg IMPLEMENTATION.
  METHOD setup.
    log = zcl_ed_logger_factory=>create_temporary_logger( ).
  ENDMETHOD.

  METHOD has_warnings.
    cl_abap_unit_assert=>assert_false( log->ext-msg->has_warnings( ) ).

    log->e( c_msg-msg2 ).
    cl_abap_unit_assert=>assert_false( log->ext-msg->has_warnings( ) ).

    log->w( c_msg-msg2 ).
    cl_abap_unit_assert=>assert_true( log->ext-msg->has_warnings( ) ).

    log->e( c_msg-msg2 ).
    cl_abap_unit_assert=>assert_true( log->ext-msg->has_warnings( ) ).
  ENDMETHOD.

  METHOD has_errors.
    cl_abap_unit_assert=>assert_false( log->ext-msg->has_errors( ) ).

    log->w( c_msg-msg2 ).
    cl_abap_unit_assert=>assert_false( log->ext-msg->has_errors( ) ).

    log->e( c_msg-msg2 ).
    cl_abap_unit_assert=>assert_true( log->ext-msg->has_errors( ) ).

    log->s( c_msg-msg2 ).
    cl_abap_unit_assert=>assert_true( log->ext-msg->has_errors( ) ).
  ENDMETHOD.

  METHOD has_warnings_no_errors.
    cl_abap_unit_assert=>assert_false( log->ext-msg->has_warnings_no_errors( ) ).

    log->s( c_msg-msg2 ).
    cl_abap_unit_assert=>assert_false( log->ext-msg->has_warnings_no_errors( ) ).

    log->w( c_msg-msg2 ).
    cl_abap_unit_assert=>assert_true( log->ext-msg->has_warnings_no_errors( ) ).

    log->e( c_msg-msg2 ).
    cl_abap_unit_assert=>assert_false( log->ext-msg->has_warnings_no_errors( ) ).
  ENDMETHOD.

  METHOD get_as_string.
    log->s( c_msg-msg1 )->s( c_msg-msg2 ).
    DATA(exp) = |{ c_msg-msg1 } { c_msg-msg2 }|.

    cl_abap_unit_assert=>assert_equals( act = log->ext-msg->get_as_string( ) exp = exp ).
    cl_abap_unit_assert=>assert_equals( act = log->ext-msg->get_as_string( length_restriction = log->ext-msg->c_max_alv_line_length )
        exp = substring( val = exp len = log->ext-msg->c_max_alv_line_length ) ).
  ENDMETHOD.

  METHOD get_errors_as_string.
    log->e( c_msg-msg1 )->w( c_msg-msg2 )->s( c_msg-msg1 ).
    DATA(exp) = |{ c_msg-msg1 }|.

    cl_abap_unit_assert=>assert_equals( act = log->ext-msg->get_errors_as_string( ) exp = exp ).
    cl_abap_unit_assert=>assert_equals( act = log->ext-msg->get_errors_as_string( length_restriction = log->ext-msg->c_max_alv_line_length )
        exp = substring( val = exp len = log->ext-msg->c_max_alv_line_length ) ).
  ENDMETHOD.
ENDCLASS.
