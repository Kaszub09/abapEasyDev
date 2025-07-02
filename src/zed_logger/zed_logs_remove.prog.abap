*&---------------------------------------------------------------------*
*& Report zed_logs_display
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_logs_remove.

"It is recommended to set this report in background job and delete expired logs, so that the table won't get too big
DATA zed_logs TYPE zed_logs.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
SELECT-OPTIONS:
    s_cat FOR zed_logs-category,
    s_ext_id FOR zed_logs-external_identifier,
    s_cr_by FOR zed_logs-created_by,
    s_cr_dat FOR zed_logs-created_at_date,
    s_cr_tim FOR zed_logs-created_at_time,
    s_expdat FOR zed_logs-expiry_date DEFAULT sy-datum OPTION LT SIGN I ,
    s_is_bat FOR zed_logs-is_batch,
    s_msg_c FOR zed_logs-messsages_count,
    s_has_w FOR zed_logs-has_warnings,
    s_has_e FOR zed_logs-has_errors.
SELECTION-SCREEN END OF BLOCK b01.

INITIALIZATION.
  DATA(container) = NEW cl_gui_docking_container( repid = sy-repid dynnr = '1000' ratio = 20 side = cl_gui_docking_container=>dock_at_top   ).
  SELECT COUNT( * ) AS count FROM zed_logs INTO @DATA(log_count).
  SELECT SUM( log_size ) FROM zed_logs_msg INTO @DATA(log_size_b).
  DATA(log_size_kb) = round( val = log_size_b / 1024 dec = 3 ).

AT SELECTION-SCREEN OUTPUT.
  cl_abap_browser=>show_html( container = container html_string = |<body><h4>Log count: { log_count
    }</h4><h4>Estimated log size (KB): { log_size_kb }</h4></body>|  ).

START-OF-SELECTION.
  SELECT uuid FROM zed_logs
  WHERE category IN @s_cat AND external_identifier IN @s_ext_id
      AND created_by IN @s_cr_by AND created_at_date IN @s_cr_dat AND created_at_time IN @s_cr_tim
      AND expiry_date IN @s_expdat AND is_batch IN @s_is_bat
      AND messsages_count IN @s_msg_c AND has_warnings IN @s_has_w AND has_errors IN @s_has_e
  INTO TABLE @DATA(uuid_tab).

  "Remove in packages of 1000 in case range table is too large
  DATA uuid_range TYPE RANGE OF zed_logs-uuid.
  LOOP AT uuid_tab REFERENCE INTO DATA(uuid).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = uuid->uuid ) TO uuid_range.
    IF lines( uuid_range ) > 1000.
      DELETE FROM zed_logs_msg WHERE uuid IN @uuid_range.
      DELETE FROM zed_logs WHERE uuid IN @uuid_range.
      CLEAR: uuid_range.
    ENDIF.
  ENDLOOP.

  "Need to check otherwise could end up removing all
  IF lines( uuid_range ) > 0.
    DELETE FROM zed_logs_msg WHERE uuid IN @uuid_range.
    DELETE FROM zed_logs WHERE uuid IN @uuid_range.
  ENDIF.
  COMMIT WORK AND WAIT.
  MESSAGE zcl_ed_msg=>get( text = TEXT-001 v1 = lines( uuid_tab ) ) TYPE 'S'.
