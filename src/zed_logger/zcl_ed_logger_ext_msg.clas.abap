CLASS zcl_ed_logger_ext_msg DEFINITION PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS zcl_ed_logger_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_logger_ext_msg.

  PRIVATE SECTION.
    DATA:
        logger TYPE REF TO zif_ed_logger.
ENDCLASS.

CLASS zcl_ed_logger_ext_msg IMPLEMENTATION.
  METHOD zif_ed_logger_ext_msg~has_warnings.
    has_warnings = xsdbool( logger->log-has_warnings = abap_true ).
  ENDMETHOD.

  METHOD zif_ed_logger_ext_msg~has_errors.
    has_errors = xsdbool( logger->log-has_errors = abap_true ).
  ENDMETHOD.

  METHOD zif_ed_logger_ext_msg~has_warnings_no_errors.
    has_warnings_no_errors = xsdbool( logger->log-has_warnings = abap_true AND logger->log-has_errors = abap_false ).
  ENDMETHOD.

  METHOD zif_ed_logger_ext_msg~get_as_string.
    DATA(first_msg) = abap_true.
    LOOP AT logger->log-messages REFERENCE INTO DATA(msg).
      IF msgty_filter <> space AND msg->msg_type <> msgty_filter.
        CONTINUE.
      ENDIF.

      msgs_string = |{ msgs_string } { msg->msg }|.

      IF first_msg = abap_true. "Remove initial extra space.
        first_msg = abap_false.
        msgs_string = substring( val = msgs_string off = 1 len = strlen( msgs_string ) - 1 ).
      ENDIF.

      IF length_restriction > 0 AND strlen( msgs_string ) > length_restriction.
        msgs_string = substring( val = msgs_string off = 0 len = length_restriction ).
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_ed_logger_ext_msg~get_errors_as_string.
    errors_string = zif_ed_logger_ext_msg~get_as_string( length_restriction = length_restriction msgty_filter = 'E' ).
  ENDMETHOD.


ENDCLASS.
