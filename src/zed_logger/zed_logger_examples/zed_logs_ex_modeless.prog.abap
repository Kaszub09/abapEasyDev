*&---------------------------------------------------------------------*
*& Report zed_logs_ex_modeless
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_logs_ex_modeless.

"In this example, whatever connection you click will be added to log, which will be displayed in modeless window (non-blocking call).

CLASS lcl_report DEFINITION INHERITING FROM zcl_ea_salv_table CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      run_report.

  PROTECTED SECTION.
    METHODS:
      on_link_click REDEFINITION.

  PRIVATE SECTION.
    DATA:
      logger  TYPE REF TO zif_ed_logger,
      display TYPE REF TO zif_ed_logger_display,
      flights TYPE STANDARD TABLE OF sflight WITH EMPTY KEY.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD run_report.
    SELECT * UP TO 20 ROWS FROM sflight INTO CORRESPONDING FIELDS OF TABLE @flights.
    APPEND VALUE #( carrid = 'ABC' connid = '1234' ) TO flights. "Dummy entry in case sflight is empty
    APPEND VALUE #( carrid = 'DEF' connid = '0007' ) TO flights. "Dummy entry in case sflight is empty

    logger = zcl_ed_logger_factory=>create_logger( settings = zcl_ed_logger_factory=>create_settings( autosave = abap_false ) ).
    display = zcl_ed_logger_factory=>create_display( ).

    set_data( REF #( flights ) ).
    columns->set_as_hotspot( 'CONNID' ).
    display_data( ).
  ENDMETHOD.

  METHOD on_link_click.
    IF row = 0.
      RETURN.
    ENDIF.
    logger->s( |Added connection id '{ flights[ row ]-connid }'| ).

    "Display log. If modelees windows doesn't exists, it is recreated.
    display->display_log_in_modeless( logger ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_report( )->run_report( ).
