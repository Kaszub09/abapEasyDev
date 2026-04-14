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
    END OF t_selection,
    tt_log TYPE STANDARD TABLE OF REF TO  zif_ed_logger WITH EMPTY KEY.
ENDINTERFACE.
