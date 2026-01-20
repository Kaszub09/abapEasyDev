*&---------------------------------------------------------------------*
*& Report zed_sap_query_info
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_sap_query_info.

"---------------------------------------------------------------------
" SELECTION SCREEN
"---------------------------------------------------------------------
TABLES:
  tstcp, aqgqcat,aqgscat.
DATA: global TYPE abap_bool.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
SELECT-OPTIONS:
  s_tcode FOR tstcp-tcode.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-s02.
SELECT-OPTIONS:
  s_num FOR aqgqcat-num,
  s_qnum FOR aqgqcat-qnum,
  s_clas FOR aqgscat-clas,
  s_global FOR global.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-s03.
PARAMETERS:
  p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b03.

"---------------------------------------------------------------------
" REPORT
"---------------------------------------------------------------------
INCLUDE zed_sap_query_info_cls.

INITIALIZATION.
  DATA(report) = NEW lcl_report( sy-repid ).

START-OF-SELECTION.
  report->prepare_report( ).
  report->display_data( p_layout ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  p_layout = report->get_layout_from_f4_selection( ).
