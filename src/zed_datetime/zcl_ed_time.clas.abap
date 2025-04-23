"! <p class="shorttext synchronized" lang="en">Time functions</p>
"! <br/>TAGS; time; seconds; minutes; hours
CLASS zcl_ed_time DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized">Uses fm <em>DATE_CHECK_PLAUSIBILITY</em></p>
      "! @parameter errors | <p class="shorttext synchronized">Filled if time is not valid.</p>
      is_valid IMPORTING time TYPE t EXPORTING errors TYPE zted_errors RETURNING VALUE(is_valid) TYPE abap_bool,
      "! <p class="shorttext synchronized">Overflow will be wrapped around</p>
      create IMPORTING hours TYPE i minutes TYPE i DEFAULT 0 seconds TYPE i DEFAULT 0 RETURNING VALUE(time) TYPE t,

      seconds IMPORTING time TYPE t RETURNING VALUE(seconds) TYPE i,
      total_seconds IMPORTING time TYPE t RETURNING VALUE(seconds) TYPE i,
      seconds_till_midnight IMPORTING time TYPE t RETURNING VALUE(seconds) TYPE i,
      minutes IMPORTING time TYPE t RETURNING VALUE(minutes) TYPE i,
      total_minutes IMPORTING time TYPE t RETURNING VALUE(minutes) TYPE i,
      hours IMPORTING time TYPE t RETURNING VALUE(hours) TYPE i,
      "! @parameter interval | <p class="shorttext synchronized">H-hours &#124; M-minutes &#124; S-seconds</p>
      add_to_time IMPORTING interval TYPE c value TYPE i time TYPE t RETURNING VALUE(result_time) TYPE t RAISING zcx_ed_exception,
      "! @parameter interval | <p class="shorttext synchronized">H-hours &#124; M-minutes &#124; S-seconds</p>
      "! @parameter difference | <p class="shorttext synchronized">Always positive, assumes next day if <em>time_to</em> is lower than <em>time_from</em></p>
      difference IMPORTING interval TYPE c time_from TYPE t time_to TYPE t RETURNING VALUE(difference) TYPE i  RAISING zcx_ed_exception.

  PRIVATE SECTION.
    CONSTANTS:
        c_midnight TYPE t VALUE '240000'.
ENDCLASS.

CLASS zcl_ed_time IMPLEMENTATION.
  METHOD is_valid.
    CALL FUNCTION 'TIME_CHECK_PLAUSIBILITY'
      EXPORTING
        time                      = time                 " Transfer of date to be checked
      EXCEPTIONS
        plausibility_check_failed = 1                " Date is not plausible
        OTHERS                    = 2.
    is_valid = xsdbool( sy-subrc = 0 ).
    IF sy-subrc <> 0 AND sy-msgid IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO errors.
    ENDIF.
  ENDMETHOD.

  METHOD create.
    time(2) = COND #( WHEN hours > 24 THEN hours MOD 24 ELSE hours ).
    time+2(4) = '0000'.
    time = add_to_time( interval = 'M' value = minutes time = time ).
    time = add_to_time( interval = 'S' value = seconds time = time ).
  ENDMETHOD.

  METHOD seconds.
    seconds = time+4(2).
  ENDMETHOD.

  METHOD total_seconds.
    seconds = time.
  ENDMETHOD.

  METHOD seconds_till_midnight.
    seconds = c_midnight - time.
  ENDMETHOD.

  METHOD minutes.
    minutes = time+2(2).
  ENDMETHOD.

  METHOD total_minutes.
    minutes = 60 * hours( time ) + minutes( time ).
  ENDMETHOD.

  METHOD hours.
    hours = time(2).
  ENDMETHOD.

  METHOD add_to_time.
    CASE to_upper( interval ).
      WHEN 'H'. result_time = time + 60 * 60 * value.
      WHEN 'M'. result_time = time + 60 * value.
      WHEN 'S'. result_time = time + value.
      WHEN OTHERS.   RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |Unsupported interval='{ interval }'|.
    ENDCASE.
  ENDMETHOD.

  METHOD difference.
    difference = COND #( WHEN time_to >= time_from THEN time_to - time_from ELSE seconds_till_midnight( time_from ) + time_to ).

    CASE to_upper( interval ).
      WHEN 'H'. difference = difference DIV ( 60 * 60 ).
      WHEN 'M'. difference = difference DIV 60.
      WHEN 'S'.
      WHEN OTHERS. RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |Unsupported interval='{ interval }'|.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
