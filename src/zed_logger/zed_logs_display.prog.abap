*&---------------------------------------------------------------------*
*& Report zed_logs_display
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_logs_display.

DATA zed_logs TYPE zed_logs.
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
SELECT-OPTIONS:
    s_uuid FOR zed_logs-uuid,
    s_cat FOR zed_logs-category,
    s_ext_id FOR zed_logs-external_identifier,
    s_cr_by FOR zed_logs-created_by,
    s_cr_dat FOR zed_logs-created_at_date DEFAULT sy-datum SIGN I OPTION EQ,
    s_cr_tim FOR zed_logs-created_at_time,
    s_tcode FOR zed_logs-tcode,
    s_expdat FOR zed_logs-expiry_date,
    s_is_bat FOR zed_logs-is_batch,
    s_lc_by FOR zed_logs-last_change_by,
    s_lc_dat FOR zed_logs-last_change_date,
    s_lc_tim FOR zed_logs-last_change_time,
    s_msg_c FOR zed_logs-messsages_count,
    s_has_w FOR zed_logs-has_warnings,
    s_has_e FOR zed_logs-has_errors.
SELECTION-SCREEN END OF BLOCK b01.

START-OF-SELECTION.
  DATA(display) = zcl_ed_logger_factory=>create_display( ).
  display->display_logs( VALUE #(
    uuid = s_uuid[]
    category = s_cat[]
    external_identifier = s_ext_id[]
    created_by = s_cr_by[]
    created_at_date = s_cr_dat[]
    created_at_time = s_cr_tim[]
    transaction = s_tcode[]
    expiry_date = s_expdat[]
    is_batch = s_is_bat[]
    last_change_by = s_lc_by[]
    last_change_date = s_lc_dat[]
    last_change_time = s_lc_tim[]
    messsages_count = s_msg_c[]
    has_warnings = s_has_w[]
    has_errors = s_has_e[] ) ).
