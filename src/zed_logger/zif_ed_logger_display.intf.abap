INTERFACE zif_ed_logger_display PUBLIC.
  METHODS:
    display_log IMPORTING logger       TYPE REF TO zif_ed_logger
                          start_column TYPE i DEFAULT 0
                          end_column   TYPE i DEFAULT 208
                          start_line   TYPE i DEFAULT 0
                          end_line     TYPE i DEFAULT 32,
    "! <p class="shorttext synchronized" lang="en">If modeless window exists and logger is the same as previous,
    "! data is refreshed instead of being recreated.</p>
    display_log_in_modeless IMPORTING logger TYPE REF TO zif_ed_logger
                                      width  TYPE i DEFAULT 1600
                                      height TYPE i DEFAULT 480,
    display_logs IMPORTING sel          TYPE REF TO zcl_ed_logger_selection
                           start_column TYPE i DEFAULT 0
                           end_column   TYPE i DEFAULT 208
                           start_line   TYPE i DEFAULT 0
                           end_line     TYPE i DEFAULT 32,
    modeless_window_exists RETURNING VALUE(exists) TYPE abap_bool.
ENDINTERFACE.
