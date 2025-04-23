"! <p class="shorttext synchronized" lang="en">Date functions</p>
"! <br/>TAGS; date; day; month; year
CLASS zcl_ed_date DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      tt_day_attributes TYPE STANDARD TABLE OF casdayattr WITH EMPTY KEY.

    CLASS-METHODS:
      "! <p class="shorttext synchronized">Uses fm <em>DATE_CHECK_PLAUSIBILITY</em></p>
      "! @parameter errors | <p class="shorttext synchronized">Filled if date is not valid.</p>
      is_valid IMPORTING date TYPE d EXPORTING errors TYPE zted_errors RETURNING VALUE(is_valid) TYPE abap_bool,
      "! <p class="shorttext synchronized">Overflow will be converted (e.g. 33.01 = 02.02).</p>
      create IMPORTING year TYPE i month TYPE i DEFAULT 1 day TYPE i DEFAULT 1 RETURNING VALUE(date) TYPE d,
      day IMPORTING date TYPE d RETURNING VALUE(day) TYPE i,
      "! <p class="shorttext synchronized"> Based on ISO 8601 - 1st week  is one with 4th January.</p> (in other words, it's the one with first Thursday )
      week  IMPORTING date TYPE d RETURNING VALUE(week) TYPE kweek,
      "! @parameter day | <p class="shorttext synchronized">Monday - 1, Sunday - 7.</p>
      weekday IMPORTING date TYPE d RETURNING VALUE(day) TYPE i,
      month IMPORTING date TYPE d RETURNING VALUE(month) TYPE i,
      first_day_in_month IMPORTING date TYPE d RETURNING VALUE(first_day_in_month) TYPE d,
      last_day_in_month IMPORTING date TYPE d RETURNING VALUE(last_day_in_month) TYPE d,
      days_in_month IMPORTING date TYPE d RETURNING VALUE(days) TYPE i,
      year IMPORTING date TYPE d RETURNING VALUE(year) TYPE i,
      day_in_year IMPORTING date TYPE d RETURNING VALUE(days) TYPE i,
      days_in_year IMPORTING date TYPE d RETURNING VALUE(days) TYPE i,
      "! @parameter interval | <p class="shorttext synchronized">Y-year &#124; M-month &#124; W-week &#124; D-day</p>
      add_to_date IMPORTING interval TYPE c value TYPE i date TYPE d RETURNING VALUE(result_date) TYPE d RAISING zcx_ed_exception,
      "! @parameter interval | <p class="shorttext synchronized">Y-year &#124; M-month &#124; W-week &#124; D-day</p>
      difference IMPORTING interval TYPE c date_from TYPE d date_to TYPE d RETURNING VALUE(difference) TYPE i RAISING zcx_ed_exception,
      "! <p class="shorttext synchronized" lang="en">CCalls fm <em>DAY_ATTRIBUTES_GET</em></p>
      get_calendar_days IMPORTING factory_calendar TYPE wfcid DEFAULT space holiday_calendar TYPE hident DEFAULT space
                                  date_from TYPE scdatum DEFAULT sy-datum date_to TYPE scdatum DEFAULT sy-datum
                                  language TYPE syst_langu DEFAULT sy-langu non_iso TYPE os_boolean DEFAULT abap_false
                        EXPORTING year_of_valid_from TYPE cyear year_of_valid_to TYPE cyear returncode TYPE syst_subrc
                        RETURNING VALUE(days) TYPE tt_day_attributes
                        RAISING zcx_ed_exception.

  PRIVATE SECTION.
    CONSTANTS:
      known_thursday TYPE d VALUE '19700101'.
ENDCLASS.

CLASS zcl_ed_date IMPLEMENTATION.
  METHOD is_valid.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = date                 " Transfer of date to be checked
      EXCEPTIONS
        plausibility_check_failed = 1                " Date is not plausible
        OTHERS                    = 2.
    is_valid = xsdbool( sy-subrc = 0 ).
    IF sy-subrc <> 0 AND sy-msgid IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO errors.
    ENDIF.
  ENDMETHOD.

  METHOD create.
    date(4) = year.
    date+4(4) = '0101'.
    date = add_to_date( interval = 'M' value = ( month - 1 ) date = date ).
    date = add_to_date( interval = 'D' value = ( day - 1 ) date = date ).
  ENDMETHOD.

  METHOD day.
    day = date+6(2).
  ENDMETHOD.

  METHOD week.
    DATA(fourth_january) = create( year = year( date ) month = 1 day = 4 ).
    DATA(week_diff_to_first_week) = difference( date_from = fourth_january date_to = date interval = 'W' ).

    IF week_diff_to_first_week >= 0.
      week = |{ date(4) }{ week_diff_to_first_week + 1 ALIGN = RIGHT PAD = '0' WIDTH = 2 }|.
    ELSE.
      DATA(fourth_january_prev_year) = create( year = year( date ) - 1 month = 1 day = 4 ).
      week_diff_to_first_week = difference( date_from = fourth_january_prev_year date_to = date interval = 'W' ).
      week = |{ year( fourth_january_prev_year ) ALIGN = RIGHT PAD = '0' WIDTH = 4 }{
          week_diff_to_first_week + 1 ALIGN = RIGHT PAD = '0' WIDTH = 2 }|.
    ENDIF.
  ENDMETHOD.

  METHOD weekday.
    "+3, because MOD is 0 on thursdays, and we want 0 on Monday
    "+1 after, to move 0-6 range to 1-7
    day = ( date + 3 - known_thursday ) MOD 7 + 1.
  ENDMETHOD.

  METHOD month.
    month = date+4(2).
  ENDMETHOD.

  METHOD first_day_in_month.
    first_day_in_month = date.
    first_day_in_month+6(2) = '01'.
  ENDMETHOD.

  METHOD last_day_in_month.
    last_day_in_month = date.
    last_day_in_month+6(2) = days_in_month( date ).
  ENDMETHOD.

  METHOD days_in_month.
    CASE date+4(2).
      WHEN '01' OR '03' OR '05' OR '07' OR '08' OR '10' OR '12'.
        days = 31.
      WHEN '02'.
        days = COND #( WHEN year( date ) MOD 4 = 0 THEN 29 ELSE 28 ).
      WHEN '04' OR '06' OR '09' OR '11'.
        days = 30.
    ENDCASE.
  ENDMETHOD.

  METHOD year.
    year = date(4).
  ENDMETHOD.

  METHOD day_in_year.
    days = date - create( year = year( date ) ) + 1.
  ENDMETHOD.

  METHOD days_in_year.
    days = COND #( WHEN year( date ) MOD 4 = 0 THEN 366 ELSE 365 ).
  ENDMETHOD.

  METHOD add_to_date.
    result_date = date.

    CASE to_upper( interval ).
      WHEN 'Y'. " YEAR
        result_date(4) = CONV i( date(4) ) + value.
        IF result_date+4(4) = '0229'. " Check for leap year
          result_date+6(2) = COND #( WHEN result_date+2(2) MOD 4 = 0 THEN 29 ELSE 28 ).
        ENDIF.

      WHEN 'M'. " MONTH
        " Instead of adding months to given date (problematic when e.g. add 1 month to December,
        " because it rolls over to next year), consider adding corrected number of months to January
        DATA(months_to_add) = value + CONV i( result_date+4(2) ) - 1.
        result_date+4(2) = '01'.
        " First add all years in months, so it leaves < 12 months
        DATA(years_to_add) = CONV i( trunc( CONV f( months_to_add ) / 12 ) ).
        result_date(4) = CONV i( date(4) ) + years_to_add.
        " Add remaining months
        IF months_to_add - 12 * years_to_add >= 0.
          result_date+4(2) = |{ 1 + months_to_add - 12 * years_to_add ALIGN = RIGHT PAD = '0' WIDTH = 2 }|.
        ELSE.
          result_date(4) = CONV i( result_date(4) ) - 1.
          result_date+4(2) = |{ 12 + 1 + months_to_add - 12 * years_to_add ALIGN = RIGHT PAD = '0' WIDTH = 2 }|.
        ENDIF.

        " Check for correct number of days in month
        IF day( result_date ) >= day( last_day_in_month( result_date ) ).
          result_date = last_day_in_month( result_date ).
        ENDIF.
      WHEN 'W'. " WEEK
        result_date = result_date + 7 * value.

      WHEN 'D'. " DAY
        result_date = result_date + value.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |Unsupported interval='{ interval }'|.
    ENDCASE.
  ENDMETHOD.

  METHOD difference.
    CASE to_upper( interval ).
      WHEN 'Y'. " YEAR
        difference = year( date_to ) - year( date_from ).

      WHEN 'M'. " MONTH
        difference = 12 * ( year( date_to ) - year( date_from ) ) + month( date_to ) - month( date_from ).

      WHEN 'W'. " WEEK
        difference = trunc(  ( CONV f( date_to  - date_from ) ) / 7 ). " Apparently 12 / 7 = 2, because SAP rounds it ¯\_(ツ)_/¯
        DATA(weekday_diff) = ( abs( date_to - date_from ) ) MOD 7.
        IF date_from <= date_to AND weekday_diff >= weekday( date_to ).
          difference = difference + 1.
        ELSEIF date_from > date_to AND weekday_diff > 7 - weekday( date_to ).
          difference = difference - 1.
        ENDIF.

      WHEN 'D'. " DAY
        difference = date_to - date_from.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |Unsupported interval='{ interval }'|.
    ENDCASE.
  ENDMETHOD.

  METHOD get_calendar_days.
    CALL FUNCTION 'DAY_ATTRIBUTES_GET'
      EXPORTING
        factory_calendar           = factory_calendar            " Factory Calendar
        holiday_calendar           = holiday_calendar            " Public Holiday Calendar
        date_from                  = date_from         " From Date
        date_to                    = date_to        " To Date
        language                   = language        " Language
        non_iso                    = non_iso
      IMPORTING
        year_of_valid_from         = year_of_valid_from
        year_of_valid_to           = year_of_valid_to
        returncode                 = returncode                 " Return Code
      TABLES
        day_attributes             = days                 " Daily attributes
      EXCEPTIONS
        factory_calendar_not_found = 1                " Factory calendar is not in buffer
        holiday_calendar_not_found = 2                " Holiday calendar is not in buffer
        date_has_invalid_format    = 3
        date_inconsistency         = 4
        OTHERS                     = 5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |DAY_ATTRIBUTES_GET rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
