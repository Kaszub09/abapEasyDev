*&---------------------------------------------------------------------*
*& Report zed_logs_ex_modeless
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_logs_ex_modeless.

"In this example, whatever connection you click will be added to log
"You can then display logs in nonmodal window (non-blocking call).
"You can reuse window or display new detached every time.
CLASS lcl_report DEFINITION INHERITING FROM zcl_ea_salv_table CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      run_report.

  PROTECTED SECTION.
    METHODS:
      on_added_function REDEFINITION,
      on_link_click REDEFINITION.

  PRIVATE SECTION.
    DATA:
      logger  TYPE REF TO zif_ed_logger,
      display TYPE REF TO zcl_ed_logger_display_nonmodal,
      flights TYPE STANDARD TABLE OF sflight WITH EMPTY KEY.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD run_report.
    SELECT * UP TO 20 ROWS FROM sflight INTO CORRESPONDING FIELDS OF TABLE @flights.
    APPEND VALUE #( carrid = 'ABC' connid = '1234' ) TO flights. "Dummy entry in case sflight is empty
    APPEND VALUE #( carrid = 'DEF' connid = '0007' ) TO flights. "Dummy entry in case sflight is empty

    logger = zcl_ed_logger_factory=>create_logger( settings = zcl_ed_logger_factory=>create_settings( autosave_use = abap_false ) ).
    display = zcl_ed_logger_display_factory=>create_nonmodal( ).
    display->set_settings( logger = logger ).

    functions->add_function( function = 'NEW_WINDOW' description = VALUE #( text = 'New detached window' ) ).

    set_data( REF #( flights ) ).
    columns->set_as_hotspot( 'CONNID' ).
    display_data( ).
  ENDMETHOD.

  METHOD on_added_function.
    CASE function.
      WHEN 'NEW_WINDOW'. zcl_ed_logger_display_factory=>create_nonmodal( )->set_settings( logger = logger )->display( caption = 'Detached window' ).
    ENDCASE.
  ENDMETHOD.

  METHOD on_link_click.
    IF row = 0.
      RETURN.
    ENDIF.
    logger->s( |Added connection id '{ flights[ row ]-connid }'| ).

    "Only persistent window will be refreshed. All other detached will not.
    "You could also recreate display every time, but position would be reset.
    IF display->is_closed( ).
      display->display( caption = 'This window is automatically refreshed' ).
    ENDIF.
    display->refresh( ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_report( )->run_report( ).
