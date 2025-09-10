"! <p class="shorttext synchronized">Simple popups</p>
"! <br/>TAGS: display; popup; question; user; confirm
CLASS zcl_ed_popup DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
        tt_fields TYPE STANDARD TABLE OF sval WITH EMPTY KEY.

    CLASS-METHODS:
     yes_no IMPORTING title TYPE csequence DEFAULT space question TYPE csequence
                      start_col TYPE i DEFAULT 25 start_row TYPE i DEFAULT 6
            RETURNING VALUE(confirmed) TYPE abap_bool
            RAISING zcx_ed_exception,
     "! @parameter result | <p class="shorttext synchronized" lang="en">Y - Yes, N - No, C - Cancel</p>
     yes_no_cancel IMPORTING title TYPE csequence DEFAULT space question TYPE csequence start_col TYPE i DEFAULT 25 start_row TYPE i DEFAULT 6
                   RETURNING VALUE(result) TYPE string
                   RAISING zcx_ed_exception,
     "! <p class="shorttext synchronized">Display popup to fill values</p>
     request_values IMPORTING title TYPE csequence DEFAULT space start_col TYPE i DEFAULT 25 start_row TYPE i DEFAULT 6 check_existance TYPE abap_bool DEFAULT abap_true
                    CHANGING fields TYPE tt_fields
                    RETURNING VALUE(confirmed) TYPE abap_bool
                    RAISING zcx_ed_exception.
ENDCLASS.

CLASS zcl_ed_popup IMPLEMENTATION.
  METHOD yes_no.
    DATA answer TYPE c LENGTH 1.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = title
        text_question         = question
        text_button_1         = TEXT-001
        icon_button_1         = '@0V@'  "ICON_OKAY
        text_button_2         = TEXT-002
        icon_button_2         = '@0W@' "ICON_CANCEL
        display_cancel_button = abap_false
        start_column          = start_col
        start_row             = start_row
        popup_type            = 'ICON_MESSAGE_QUESTION'
      IMPORTING
        answer                = answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |POPUP_TO_CONFIRM rc={ sy-subrc }|.
    ENDIF.

    confirmed = xsdbool( answer = '1' ).
  ENDMETHOD.

  METHOD yes_no_cancel.
    DATA answer TYPE c LENGTH 1.

    "Different icons since ICON_CANCEL
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = title
        text_question         = question
        text_button_1         = TEXT-001
        icon_button_1         = '@5B@'  "ICON_LED_GREEN
        text_button_2         = TEXT-002
        icon_button_2         = '@5C@' "ICON_LED_RED
        display_cancel_button = abap_true
        start_column          = start_col
        start_row             = start_row
        popup_type            = 'ICON_MESSAGE_QUESTION'
      IMPORTING
        answer                = answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |POPUP_TO_CONFIRM rc={ sy-subrc }|.
    ENDIF.

    result = COND #( WHEN answer = '1' THEN 'Y' WHEN answer = '2' THEN 'N' ELSE 'C' ).
  ENDMETHOD.

  METHOD request_values.
    DATA:  return_code TYPE c LENGTH 1.
    CALL FUNCTION 'POPUP_GET_VALUES_DB_CHECKED'
      EXPORTING
        check_existence = check_existance            " Ind. whether table lines must exist
        popup_title     = title                 " Text of title line
        start_column    = start_col             " Start column of the dialog box
        start_row       = start_row              " Start line of the dialog box
      IMPORTING
        returncode      = return_code                 " User response
      TABLES
        fields          = fields                 " Table fields, values and attributes
      EXCEPTIONS
        error_in_fields = 1                " FIELDS were transferred incorrectly
        OTHERS          = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |POPUP_GET_VALUES_DB_CHECKED rc={ sy-subrc }|.
    ENDIF.
    confirmed = xsdbool( return_code = space ).
  ENDMETHOD.
ENDCLASS.
