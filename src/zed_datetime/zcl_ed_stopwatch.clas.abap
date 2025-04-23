"! <p class="shorttext synchronized">Stopwatch</p>
"! <br/>TAGS: stopwatch; time; trace; performance
CLASS zcl_ed_stopwatch DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING start_immediately TYPE abap_bool DEFAULT abap_true,
      get_elapsed_microseconds RETURNING VALUE(microseconds) TYPE i,
      get_elapsed_miliseconds RETURNING VALUE(seconds) TYPE decfloat34,
      get_elapsed_seconds RETURNING VALUE(seconds) TYPE decfloat34,
      restart,
      pause,
      continue.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      microseconds_in_second     TYPE i VALUE 1000000,
      microseconds_in_milisecond TYPE i VALUE 1000.
    DATA:
      "! in microseconds
      started_at   TYPE i,
      elapsed_time TYPE i,
      is_running   TYPE abap_bool.
ENDCLASS.



CLASS zcl_ed_stopwatch IMPLEMENTATION.
  METHOD get_elapsed_miliseconds.
    seconds = get_elapsed_microseconds(  ) / microseconds_in_milisecond.
  ENDMETHOD.

  METHOD get_elapsed_seconds.
    seconds = get_elapsed_microseconds(  ) / microseconds_in_second.
  ENDMETHOD.

  METHOD pause.
    GET RUN TIME FIELD DATA(current_time).
    elapsed_time = elapsed_time + current_time - started_at.
    is_running = abap_false.
  ENDMETHOD.

  METHOD restart.
    GET RUN TIME FIELD started_at.
    elapsed_time = 0.
    is_running = abap_true.
  ENDMETHOD.

  METHOD continue.
    GET RUN TIME FIELD started_at.
    is_running = abap_true.
  ENDMETHOD.

  METHOD get_elapsed_microseconds.
    microseconds = elapsed_time.
    IF  is_running = abap_true.
      GET RUN TIME FIELD DATA(current_time).
      microseconds = microseconds + current_time - started_at.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    IF start_immediately = abap_true.
      restart( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
