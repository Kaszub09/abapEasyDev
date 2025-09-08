"=================================================================
"-----------------------------------------------------------------
CLASS tcl_saving DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      log_is_saved_rollback_2nd_conn FOR TESTING,
      no_commit_on_normal_conn FOR TESTING,
      autosave_called FOR TESTING,
      messages_saved FOR TESTING,
      autosave_errors_only FOR TESTING.

    DATA:
      subrc TYPE sy-subrc,
      cut   TYPE REF TO zif_ed_logger.
ENDCLASS.

CLASS tcl_saving IMPLEMENTATION.
  METHOD log_is_saved_rollback_2nd_conn.
    cut = zcl_ed_logger_factory=>create_logger( settings = zcl_ed_logger_factory=>create_settings(
        second_conn_name = zif_ed_logger=>c_2nd_con_name second_conn_commit = abap_true )
        expiry_date = sy-datum ).

    cut->add( 'MESSAGE' ).
    cut->save( ).
    ROLLBACK WORK.

    " TODO: variable is assigned but never used (ABAP cleaner)
    SELECT SINGLE * FROM zed_logs WHERE uuid = @cut->log-uuid INTO @DATA(log).
    subrc = sy-subrc.
    cl_abap_unit_assert=>assert_equals( act = subrc exp = 0 ).
  ENDMETHOD.

  METHOD no_commit_on_normal_conn.
    cut = zcl_ed_logger_factory=>create_logger( settings = zcl_ed_logger_factory=>create_settings(
        second_conn_use = abap_false )
        expiry_date = sy-datum ).

    cut->add( 'MESSAGE' ).
    cut->save( ).
    ROLLBACK WORK.

    " TODO: variable is assigned but never used (ABAP cleaner)
    SELECT SINGLE * FROM zed_logs WHERE uuid = @cut->log-uuid INTO @DATA(log).
    subrc = sy-subrc.
    cl_abap_unit_assert=>assert_equals( act = subrc exp = 4 ).
  ENDMETHOD.

  METHOD autosave_called.
    cut = zcl_ed_logger_factory=>create_logger( settings = zcl_ed_logger_factory=>create_settings( autosave_use = abap_true
        second_conn_name = zif_ed_logger=>c_2nd_con_name second_conn_commit = abap_true )
        expiry_date = sy-datum ).

    cut->add( 'MESSAGE' ).
    ROLLBACK WORK.

    " TODO: variable is assigned but never used (ABAP cleaner)
    SELECT SINGLE * FROM zed_logs WHERE uuid = @cut->log-uuid INTO @DATA(log).
    subrc = sy-subrc.
    cl_abap_unit_assert=>assert_equals( act = subrc exp = 0 ).
  ENDMETHOD.

  METHOD messages_saved.
    cut = zcl_ed_logger_factory=>create_logger( settings = zcl_ed_logger_factory=>create_settings(
        second_conn_use = abap_false )
        expiry_date = sy-datum ).

    cut->add( 'MESSAGE' ).
    cut->save( ).

    SELECT SINGLE * FROM zed_logs_msg WHERE uuid = @cut->log-uuid INTO @DATA(log_msg).
    subrc = sy-subrc.
    cl_abap_unit_assert=>assert_equals( act = subrc exp = 0 msg = |Expected to find msgs| ).

    ROLLBACK WORK.
    cl_abap_unit_assert=>assert_equals( act = cut->hex->hex_to_messages( log_msg-messages_hex ) exp = cut->log-messages ).
  ENDMETHOD.

  METHOD autosave_errors_only.
    cut = zcl_ed_logger_factory=>create_logger( settings = zcl_ed_logger_factory=>create_settings( autosave_use = abap_true
        autosave_only_if_errors = abap_true
        second_conn_name = zif_ed_logger=>c_2nd_con_name second_conn_commit = abap_true )
        expiry_date = sy-datum ).

    cut->add( 'MESSAGE' ).
    ROLLBACK WORK.

    " TODO: variable is assigned but never used (ABAP cleaner)
    SELECT SINGLE * FROM zed_logs WHERE uuid = @cut->log-uuid INTO @DATA(log).
    subrc = sy-subrc.
    cl_abap_unit_assert=>assert_equals( act = subrc exp = 4 ).

    cut->e( 'MESSAGE' ).
    ROLLBACK WORK.

    " TODO: variable is assigned but never used (ABAP cleaner)
    SELECT SINGLE * FROM zed_logs WHERE uuid = @cut->log-uuid INTO @DATA(log2).
    subrc = sy-subrc.
    cl_abap_unit_assert=>assert_equals( act = subrc exp = 0 ).
  ENDMETHOD.
ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
CLASS tcl_log_level DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      dont_log_greater_than_level FOR TESTING,
      long_eq_or_lt_than_level FOR TESTING.

    DATA:
      cut   TYPE REF TO zif_ed_logger.
ENDCLASS.

CLASS tcl_log_level IMPLEMENTATION.
  METHOD dont_log_greater_than_level.
    cut = zcl_ed_logger_factory=>create_logger( settings = zcl_ed_logger_factory=>create_settings( autosave_use = abap_false
        logging_level = 3 ) ).
    cut->add( obj = 'MESSAGE' level = 5 ).

    cl_abap_unit_assert=>assert_equals( act = lines( cut->log-messages ) exp = 0 ).
  ENDMETHOD.

  METHOD long_eq_or_lt_than_level.
    cut = zcl_ed_logger_factory=>create_logger( settings = zcl_ed_logger_factory=>create_settings( autosave_use = abap_false
        logging_level = 3 ) ).
    cut->add( obj = 'MESSAGE' level = 2 ).
    cut->add( obj = 'MESSAGE' level = 3 ).

    cl_abap_unit_assert=>assert_equals( act = lines( cut->log-messages ) exp = 2 ).
  ENDMETHOD.
ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
CLASS tcl_log_msg_type DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      setup,
      log_e FOR TESTING,
      log_w FOR TESTING,
      log_s FOR TESTING,
      log_i FOR TESTING,
      log_space FOR TESTING.

    DATA:
      cut   TYPE REF TO zif_ed_logger.
ENDCLASS.

CLASS tcl_log_msg_type IMPLEMENTATION.
  METHOD setup.
    cut = zcl_ed_logger_factory=>create_logger( settings = zcl_ed_logger_factory=>create_settings( autosave_use = abap_false ) ).
  ENDMETHOD.

  METHOD log_e.
    cut->e( 'MESSAGE' ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cut->log-messages[ msg_type = 'E' ] ) ) ).
  ENDMETHOD.

  METHOD log_w.
    cut->w( 'MESSAGE' ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cut->log-messages[ msg_type = 'W' ] ) ) ).
  ENDMETHOD.

  METHOD log_s.
    cut->s( 'MESSAGE' ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cut->log-messages[ msg_type = 'S' ] ) ) ).
  ENDMETHOD.

  METHOD log_i.
    cut->i( 'MESSAGE' ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cut->log-messages[ msg_type = 'I' ] ) ) ).
  ENDMETHOD.

  METHOD log_space.
    cut->add(  'MESSAGE' ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cut->log-messages[ msg_type = space ] ) ) ).
  ENDMETHOD.
ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
CLASS tcl_header_stats DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      setup,
      check_msgs_stats FOR TESTING,
      check_metainfo_after_creation FOR TESTING.

    DATA:
      cut   TYPE REF TO zif_ed_logger.
ENDCLASS.

CLASS tcl_header_stats IMPLEMENTATION.
  METHOD setup.
    cut = zcl_ed_logger_factory=>create_logger( settings = zcl_ed_logger_factory=>create_settings( autosave_use = abap_false ) ).
  ENDMETHOD.

  METHOD check_msgs_stats.
    cut->s( 'MSG' )->s( 'MSG' )->s( 'MSG' ).
    cut->e( 'MSG' )->e( 'MSG' ).
    cut->i( 'MSG' )->i( 'MSG' ).
    cut->w( 'MSG' ).

    cl_abap_unit_assert=>assert_true( cut->log-has_errors ).
    cl_abap_unit_assert=>assert_true( cut->log-has_warnings ).
    cl_abap_unit_assert=>assert_equals( act = cut->log-messsages_count exp = 8 ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut->log-messages ) exp = 8 ).

    GET TIME.
    cl_abap_unit_assert=>assert_equals( act = cut->log-last_change_by exp = sy-uname  ).
    cl_abap_unit_assert=>assert_equals( act = cut->log-last_change_date exp = sy-datum ).
    cl_abap_unit_assert=>assert_equals( act = CONV f( cut->log-last_change_time ) exp = CONV f( sy-uzeit ) tol = '10' ).
  ENDMETHOD.

  METHOD check_metainfo_after_creation.
    cut = zcl_ed_logger_factory=>create_logger( category = 'CATEGORY' ext_id = 'EXT_ID' expiry_date = '99991231'
      settings = zcl_ed_logger_factory=>create_settings( autosave_use = abap_false ) ).

    cl_abap_unit_assert=>assert_not_initial( act = cut->log-uuid ).
    cl_abap_unit_assert=>assert_equals( act = cut->log-category exp = 'CATEGORY' ).
    cl_abap_unit_assert=>assert_equals( act = cut->log-external_identifier exp = 'EXT_ID' ).
    cl_abap_unit_assert=>assert_equals( act = cut->log-expiry_date exp = '99991231' ).
    cl_abap_unit_assert=>assert_equals( act = cut->log-tcode exp = sy-tcode ).

    GET TIME.
    cl_abap_unit_assert=>assert_equals( act = cut->log-created_by exp = sy-uname  ).
    cl_abap_unit_assert=>assert_equals( act = cut->log-created_at_date exp = sy-datum ).
    cl_abap_unit_assert=>assert_equals( act = CONV f( cut->log-created_at_time ) exp = CONV f( sy-uzeit ) tol = '10' ).
  ENDMETHOD.
ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
CLASS tcl_context_log DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_context_struct,
        object TYPE trobjtype,
      END OF t_context_struct.

    METHODS:
      setup,
      expect_empty_context FOR TESTING,
      log_context FOR TESTING.

    DATA:
      cut   TYPE REF TO zif_ed_logger.
ENDCLASS.

CLASS tcl_context_log IMPLEMENTATION.
  METHOD setup.
    cut = zcl_ed_logger_factory=>create_logger( settings = zcl_ed_logger_factory=>create_settings( autosave_use = abap_false ) ).
  ENDMETHOD.

  METHOD expect_empty_context.
    cut = zcl_ed_logger_factory=>create_logger( settings = zcl_ed_logger_factory=>create_settings( autosave_use = abap_false ) ).
    cl_abap_unit_assert=>assert_false( cut->context->exists( ) ).

    TRY.
        DATA(context_data) = VALUE t_context_struct( object = 'TEST' ).
        cut->add( obj = 'MESSAGE' context_data = REF #( context_data ) ).
        cl_abap_unit_assert=>fail( 'Exception should be raised' ).
      CATCH zcx_ed_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD log_context.
    DATA(context_data) = VALUE t_context_struct( object = 'TEST' ).

    cut = zcl_ed_logger_factory=>create_logger( settings = zcl_ed_logger_factory=>create_settings( autosave_use = abap_false )
        context = zcl_ed_logger_factory=>create_context_from_ref( REF #( context_data ) ) ).
    cl_abap_unit_assert=>assert_true( cut->context->exists( ) ).

    cut->add( obj = 'MESSAGE' context_data = REF #( context_data ) ).
    cl_abap_unit_assert=>assert_equals( act = cut->log-messages[ 1 ]-context_values exp = 'TEST' ).
  ENDMETHOD.
ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
CLASS tcl_read DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      save_then_read_log FOR TESTING.

    DATA:
      cut   TYPE REF TO zif_ed_logger.
ENDCLASS.

CLASS tcl_read IMPLEMENTATION.
  METHOD save_then_read_log.
    cut = zcl_ed_logger_factory=>create_logger( settings = zcl_ed_logger_factory=>create_settings(
       second_conn_use = abap_false )
       expiry_date = sy-datum ).
    cut->add( 'MESSAGE' ).
    cut->save( ).

    DATA(logger_from_db) = zcl_ed_logger_factory=>open_logger( uuid = cut->log-uuid ).
    ROLLBACK WORK.

    cl_abap_unit_assert=>assert_equals( act = logger_from_db->log exp = cut->log ).
  ENDMETHOD.
ENDCLASS.
