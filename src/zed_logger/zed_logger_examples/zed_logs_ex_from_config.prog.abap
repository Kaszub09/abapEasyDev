*&---------------------------------------------------------------------*
*& Report zed_logs_ex_simple_use
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_logs_ex_from_config.

"Add entry in transaction ZED_LOGGER_CONFIG. Without entry code still works but does nothing, just like deactivated entry.
DATA(logger) = zcl_ed_logger_factory=>create_logger_from_config( category = 'LOG_FROM_CONFIG' ext_id = 'TEST' ).

logger->i( 'INFO' ).
logger->i( 'INFO' ).
logger->w( 'WARNING' ).
