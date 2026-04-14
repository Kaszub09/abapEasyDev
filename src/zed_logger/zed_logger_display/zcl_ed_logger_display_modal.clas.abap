CLASS zcl_ed_logger_display_modal DEFINITION PUBLIC INHERITING FROM zcl_ed_logger_display_base FINAL CREATE PRIVATE
  GLOBAL FRIENDS zcl_ed_logger_display_factory.

  PUBLIC SECTION.
    INTERFACES:
      "! Internal use only, ignore.
      zif_ed_screens_handler.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Loggers from all parameters are combined. Empty parameters are skipped.
      "! <br/> So e.g. using only logger displays only it, using logger+selection display logger+all logs found by selection.</p>
      "! @parameter messages_only | <p class="shorttext synchronized" lang="en">Can be true only if one logger is found!</p>
      set_settings IMPORTING logger         TYPE REF TO zif_ed_logger OPTIONAL
                             logs           TYPE zif_ed_logger_display=>tt_log OPTIONAL
                             selection      TYPE zif_ed_logger_display=>t_selection OPTIONAL
                             messages_only  TYPE abap_bool DEFAULT abap_true
                   RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_modal,
      refresh RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_modal.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Set <em>start_column</em> or <em>start_line</em> to 0 to display fullscreen.</p>
      display IMPORTING caption        TYPE csequence DEFAULT TEXT-001
                        start_column   TYPE i DEFAULT 1
                        end_column     TYPE i DEFAULT 192
                        start_line     TYPE i DEFAULT 1
                        end_line       TYPE i DEFAULT 32
              RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_modal.

  PRIVATE SECTION.
    DATA:
        container TYPE REF TO cl_gui_container.

    DATA:
      caption        TYPE string,
      first_time_pbo TYPE abap_bool.
ENDCLASS.

CLASS zcl_ed_logger_display_modal IMPLEMENTATION.
  METHOD refresh.
    display = me.
    refresh_base( ).
  ENDMETHOD.

  METHOD set_settings.
    display = me.
    set_settings_base( logger = logger logs = logs selection = selection messages_only = messages_only ).
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pai.
    CASE command.
      WHEN zif_ed_screens_handler~c_status_commands-back OR zif_ed_screens_handler~c_status_commands-cancel
      OR zif_ed_screens_handler~c_status_commands-exit.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pbo.
    IF first_time_pbo = abap_false.
      RETURN.
    ENDIF.
    first_time_pbo = abap_false.

    container = zcl_ed_screens=>get_screen_container( ).
    create_alv( container ).
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pbo_change_header.
    header = caption.
  ENDMETHOD.

  METHOD display.
    display = me.

    first_time_pbo = abap_true.
    me->caption = caption.

    zcl_ed_screens=>prepare_next_screen( handler = me create_container = abap_false ).
    zcl_ed_screens=>call_next_screen( start_column = start_column end_column = end_column start_line = start_line end_line = end_line ).

    container->free( EXCEPTIONS OTHERS = 1 ).
  ENDMETHOD.
ENDCLASS.
