CLASS zcl_ed_logger_display_cont DEFINITION PUBLIC INHERITING FROM zcl_ed_logger_display_base FINAL CREATE PRIVATE
  GLOBAL FRIENDS zcl_ed_logger_display_factory.

  PUBLIC SECTION.
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Loggers from all parameters are combined. Empty parameters are skipped.
      "! <br/> So e.g. using only logger displays only it, using logger+selection display logger+all logs found by selection.</p>
      "! @parameter messages_only | <p class="shorttext synchronized" lang="en">Can be true only if one logger is found!</p>
      set_settings IMPORTING logger         TYPE REF TO zif_ed_logger OPTIONAL
                             logs           TYPE zif_ed_logger_display=>tt_log OPTIONAL
                             selection      TYPE REF TO zif_ed_logger_display=>t_selection OPTIONAL
                             messages_only  TYPE abap_bool DEFAULT abap_true
                   RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_cont,
      refresh RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_cont.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Must be called in PBO!</p>
      "! @parameter container | <p class="shorttext synchronized" lang="en">You could use CL_GUI_DOCKING_CONTAINER.</p>
      display IMPORTING container      TYPE REF TO cl_gui_container
              RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_cont,
      "! <p class="shorttext synchronized" lang="en">Shortcut for common use case. Must be called in PBO!</p>
      display_in_docking IMPORTING side           TYPE i DEFAULT cl_gui_docking_container=>dock_at_right
                                   ratio          TYPE i DEFAULT 404
                         RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_cont,
      close_container  RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_cont.

  PRIVATE SECTION.
    DATA:
        container TYPE REF TO cl_gui_container.
ENDCLASS.

CLASS zcl_ed_logger_display_cont IMPLEMENTATION.
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
    me->container = container.
    create_alv( me->container ).
  ENDMETHOD.

  METHOD display_in_docking.
    display = me.
    display( NEW cl_gui_docking_container( ratio = ratio side = side ) ).
  ENDMETHOD.

  METHOD close_container.
    container->free( EXCEPTIONS OTHERS = 1 ).
  ENDMETHOD.
ENDCLASS.
