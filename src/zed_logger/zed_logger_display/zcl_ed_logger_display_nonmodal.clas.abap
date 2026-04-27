CLASS zcl_ed_logger_display_nonmodal DEFINITION PUBLIC INHERITING FROM zcl_ed_logger_display_base FINAL CREATE PRIVATE
  GLOBAL FRIENDS zcl_ed_logger_display_factory.

  PUBLIC SECTION.
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Loggers from all parameters are combined. Empty parameters are skipped.
      "! <br/> So e.g. using only logger displays only it, using logger+selection display logger+all logs found by selection.</p>
      "! @parameter messages_only | <p class="shorttext synchronized" lang="en">Can be true only if one logger is found!</p>
      set_settings IMPORTING logger         TYPE REF TO zif_ed_logger OPTIONAL
                             logs           TYPE zif_ed_logger_display=>tt_log OPTIONAL
                             selection      TYPE REF TO      zif_ed_logger_display=>t_selection OPTIONAL
                             messages_only  TYPE abap_bool DEFAULT abap_true
                   RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_nonmodal,
      refresh RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_nonmodal.

    METHODS:
      "! @parameter style | <p class="shorttext synchronized" lang="en">E.g CL_GUI_CONTROL=>WS_THICKFRAME</p>
      display IMPORTING caption        TYPE csequence DEFAULT TEXT-001
                        style          TYPE i DEFAULT 0
                        metric         TYPE i DEFAULT cl_gui_dialogbox_container=>metric_pixel
                        width          TYPE i DEFAULT 1280
                        height         TYPE i DEFAULT 720
                        top            TYPE i DEFAULT 1
                        left           TYPE i DEFAULT 1
              RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_nonmodal,
      is_closed RETURNING VALUE(is_container_closed) TYPE abap_bool,
      close RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_nonmodal.

  PRIVATE SECTION.
    METHODS:
      on_close FOR EVENT close OF cl_gui_dialogbox_container.

    DATA:
        container TYPE REF TO cl_gui_dialogbox_container.
ENDCLASS.

CLASS zcl_ed_logger_display_nonmodal IMPLEMENTATION.
  METHOD refresh.
    display = me.
    refresh_base( ).
  ENDMETHOD.

  METHOD set_settings.
    display = me.
    set_settings_base( logger = logger logs = logs selection = selection messages_only = messages_only ).
  ENDMETHOD.

  METHOD display.
    display = me.
    container = NEW cl_gui_dialogbox_container( caption = caption style = style
        metric = metric  width = width height = height top = top left = left ).

    SET HANDLER on_close FOR container.

    create_alv( me->container ).
  ENDMETHOD.

  METHOD close.
    display = me.
    IF container IS BOUND.
      container->free( EXCEPTIONS OTHERS = 1 ).
      FREE: container.
    ENDIF.
  ENDMETHOD.

  METHOD on_close.
    close( ).
  ENDMETHOD.

  METHOD is_closed.
    is_container_closed = xsdbool( container IS NOT BOUND OR container->is_alive( ) = container->state_dead ).
  ENDMETHOD.
ENDCLASS.
