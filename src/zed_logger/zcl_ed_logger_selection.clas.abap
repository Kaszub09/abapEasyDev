CLASS zcl_ed_logger_selection DEFINITION PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS zcl_ed_logger_factory.

  PUBLIC SECTION.
    TYPES:
      ttr_uuid                TYPE RANGE OF zted_log_uuid,
      ttr_category            TYPE RANGE OF zted_log_category,
      ttr_external_identifier TYPE RANGE OF zted_log_external_identifier,
      ttr_created_by          TYPE RANGE OF zted_log_created_by,
      ttr_created_at_date     TYPE RANGE OF zted_log_created_at_date,
      ttr_created_at_time     TYPE RANGE OF zted_log_created_at_time,
      ttr_transaction         TYPE RANGE OF zted_log_transaction,
      ttr_expiry_date         TYPE RANGE OF zted_log_expiry_date,
      ttr_is_batch            TYPE RANGE OF zted_log_is_batch,
      ttr_last_change_by      TYPE RANGE OF zted_log_last_change_by,
      ttr_last_change_date    TYPE RANGE OF zted_log_last_change_date,
      ttr_last_change_time    TYPE RANGE OF zted_log_last_change_time,
      ttr_messsages_count     TYPE RANGE OF zted_log_messsages_count,
      ttr_has_warnings        TYPE RANGE OF zted_log_has_warnings,
      ttr_has_errors          TYPE RANGE OF zted_log_has_errors.

    DATA:
      uuid                TYPE ttr_uuid,
      category            TYPE ttr_category,
      external_identifier TYPE ttr_external_identifier,
      created_by          TYPE ttr_created_by,
      created_at_date     TYPE ttr_created_at_date,
      created_at_time     TYPE ttr_created_at_time,
      transaction         TYPE ttr_transaction,
      expiry_date         TYPE ttr_expiry_date,
      is_batch            TYPE ttr_is_batch,
      last_change_by      TYPE ttr_last_change_by,
      last_change_date    TYPE ttr_last_change_date,
      last_change_time    TYPE ttr_last_change_time,
      messsages_count     TYPE ttr_messsages_count,
      has_warnings        TYPE ttr_has_warnings,
      has_errors          TYPE ttr_has_errors.
ENDCLASS.

CLASS zcl_ed_logger_selection IMPLEMENTATION.
ENDCLASS.
