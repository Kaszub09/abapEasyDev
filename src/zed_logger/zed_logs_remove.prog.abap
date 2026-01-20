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

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-s02.
PARAMETERS:
    p_pack_s TYPE i DEFAULT 1000.
SELECTION-SCREEN END OF BLOCK b02.

INITIALIZATION.
  IF sy-batch = abap_false.
    DATA(container) = NEW cl_gui_docking_container( repid = sy-repid dynnr = '1000' ratio = 25 side = cl_gui_docking_container=>dock_at_top   ).
    SELECT COUNT( * ) AS count FROM zed_logs INTO @DATA(log_count).
    "Add about 492 from header row entry + about 28 from msg row info for better estimate
    SELECT SUM( CAST( ( log_size + 520 ) AS FLTP )  ) FROM zed_logs_msg INTO @DATA(log_size_b).
    DATA(log_size_kb) = round( val = CONV decfloat34( log_size_b ) / 1024 dec = 3 ).
    DATA(avg_log_size_kb) = COND decfloat34( WHEN log_count = 0 THEN 0 ELSE round( val = log_size_kb / log_count dec = 3 ) ).
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  IF sy-batch = abap_false.
    cl_abap_browser=>show_html( container = container html_string = |<body><h4>Log count: { log_count
      }</h4><h4 style="margin:0px";>Estimated log size (KB): { log_size_kb NUMBER = USER } </h4><h5>Average ( KB ): { avg_log_size_kb }</h5></body>|  ).
  ENDIF.

START-OF-SELECTION.
  SELECT uuid FROM zed_logs
  WHERE category IN @s_cat AND external_identifier IN @s_ext_id
      AND created_by IN @s_cr_by AND created_at_date IN @s_cr_dat AND created_at_time IN @s_cr_tim
      AND expiry_date IN @s_expdat AND is_batch IN @s_is_bat
      AND messsages_count IN @s_msg_c AND has_warnings IN @s_has_w AND has_errors IN @s_has_e
  INTO TABLE @DATA(uuid_tab).

  "Remove in packages in case range table is too large
  DATA uuid_range TYPE RANGE OF zed_logs-uuid.
  LOOP AT uuid_tab REFERENCE INTO DATA(uuid).
    DATA(index) = sy-tabix.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = uuid->uuid ) TO uuid_range.
    IF lines( uuid_range ) >= p_pack_s OR index = lines( uuid_tab ).
      cl_progress_indicator=>progress_indicate( i_text = TEXT-002 i_msgv1 = index i_msgv2 = lines( uuid_tab )
        i_output_immediately = abap_true i_processed = index i_total = lines( uuid_tab ) ).

      DELETE FROM zed_logs_msg WHERE uuid IN @uuid_range.
      DELETE FROM zed_logs WHERE uuid IN @uuid_range.
      COMMIT WORK.
      CLEAR: uuid_range.
    ENDIF.
  ENDLOOP.

  MESSAGE zcl_ed_msg=>get( text = TEXT-001 v1 = lines( uuid_tab ) ) TYPE 'S'.
