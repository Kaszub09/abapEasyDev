*&---------------------------------------------------------------------*
*& Report zed_idocs
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_idocs.


"---------------------------------------------------------------------
" SELECTION SCREEN
"---------------------------------------------------------------------
TABLES:
  edidc.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
SELECT-OPTIONS:
  s_credat FOR edidc-credat,
  s_docnum FOR edidc-docnum,
  s_serial FOR edidc-serial,
  s_direct FOR edidc-direct,
  s_idoctp FOR edidc-idoctp,
  s_cimtyp FOR edidc-cimtyp,
  s_mestyp FOR edidc-mestyp.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-s02.
PARAMETERS:
  p_rownum TYPE i DEFAULT 1000,
  p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b02.

"---------------------------------------------------------------------
" REPORT
"---------------------------------------------------------------------
INCLUDE zed_idocs_rep_cls.

INITIALIZATION.
  DATA(report) = NEW lcl_report( sy-repid ).
  lcl_report=>init_user_variant( ).

START-OF-SELECTION.
  report->prepare_report( ).
  report->display_data( p_layout  ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  p_layout = report->get_layout_from_f4_selection( ).
