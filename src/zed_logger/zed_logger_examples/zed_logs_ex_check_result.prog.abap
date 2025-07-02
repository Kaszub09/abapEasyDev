*&---------------------------------------------------------------------*
*& Report zed_logs_ex_check_result
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_logs_ex_check_result.

"Our context will be delivery number and position - it will be displayed next to log
TYPES:
  BEGIN OF t_context,
    posnr TYPE posnr_vl,
  END OF t_context.

START-OF-SELECTION.
  "Since we don't intend to save log, we don't care about other settings, or category, expiry date etc.
  DATA(logger) = zcl_ed_logger_factory=>create_logger( settings = zcl_ed_logger_factory=>create_settings( autosave = abap_false )
      context = zcl_ed_logger_factory=>create_context_from_ref( NEW t_context(  ) ) ).

  DATA(index) = 1.
  WHILE index < 3.
    "Clear leftovers from previous loop
    logger->clear_all_logs( ).

    "Simulate first pass errors, then user corrected
    IF index = 1.
      logger->s( obj = 'Item ok' context_data = NEW t_context( posnr = '000010' ) ).
      logger->e( obj = 'Item wrong' context_data = NEW t_context( posnr = '000020' ) ).
      logger->s( obj = 'Item ok' context_data = NEW t_context( posnr = '000030' ) ).
    ELSE.
      logger->s( obj = 'Item ok' context_data = NEW t_context( posnr = '000010' ) ).
      logger->s( obj = 'Item ok' context_data = NEW t_context( posnr = '000020' ) ).
      logger->s( obj = 'Item ok' context_data = NEW t_context( posnr = '000030' ) ).
    ENDIF.

    IF logger->log-has_errors = abap_true.
      MESSAGE 'Correct errors' TYPE 'I'.
      "Preferably in popup so user can move and see delivery data
      zcl_ed_logger_factory=>create_display( )->display_log( logger = logger start_column = 1 start_line = 1  ).
    ELSE.
      MESSAGE 'All good' TYPE 'I'.
    ENDIF.

    index = index + 1.
  ENDWHILE.
