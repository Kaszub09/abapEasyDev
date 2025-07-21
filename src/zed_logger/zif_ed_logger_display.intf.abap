INTERFACE zif_ed_logger_display PUBLIC.
  TYPES:
    BEGIN OF t_selection,
      uuid                TYPE RANGE OF zted_log_uuid,
      category            TYPE RANGE OF zted_log_category,
      external_identifier TYPE RANGE OF zted_log_external_identifier,
      created_by          TYPE RANGE OF zted_log_created_by,
      created_at_date     TYPE RANGE OF zted_log_created_at_date,
      created_at_time     TYPE RANGE OF zted_log_created_at_time,
      transaction         TYPE RANGE OF zted_log_transaction,
      expiry_date         TYPE RANGE OF zted_log_expiry_date,
      is_batch            TYPE RANGE OF zted_log_is_batch,
      last_change_by      TYPE RANGE OF zted_log_last_change_by,
      last_change_date    TYPE RANGE OF zted_log_last_change_date,
      last_change_time    TYPE RANGE OF zted_log_last_change_time,
      messsages_count     TYPE RANGE OF zted_log_messsages_count,
      has_warnings        TYPE RANGE OF zted_log_has_warnings,
      has_errors          TYPE RANGE OF zted_log_has_errors,
    END OF t_selection.

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
    display_logs IMPORTING sel          TYPE t_selection
                           start_column TYPE i DEFAULT 0
                           end_column   TYPE i DEFAULT 208
                           start_line   TYPE i DEFAULT 0
                           end_line     TYPE i DEFAULT 32,
    modeless_window_exists RETURNING VALUE(exists) TYPE abap_bool.
ENDINTERFACE.
