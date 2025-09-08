"! <p class="shorttext synchronized" lang="en">Generic result with information about errors</p>
"! <br/>TAGS: result; error; errors
CLASS zcl_ed_errors DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
        tt_error TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    CONSTANTS:
            c_max_alv_line_length TYPE i VALUE 128.

    METHODS:
      constructor IMPORTING errors TYPE REF TO zcl_ed_errors OPTIONAL,
      add IMPORTING error TYPE csequence,
      append IMPORTING errors TYPE REF TO zcl_ed_errors,
      has_any RETURNING VALUE(has_any) TYPE abap_bool,
      get RETURNING VALUE(errors) TYPE tt_error,
      "! @parameter length_restriction | <p class="shorttext synchronized" lang="en">0 means no restriction</p>
      get_as_string IMPORTING length_restriction TYPE i DEFAULT 0
                    RETURNING VALUE(error)       TYPE string.

  PRIVATE SECTION.
    DATA:
        errors_tab TYPE tt_error.
ENDCLASS.

CLASS zcl_ed_errors IMPLEMENTATION.
  METHOD constructor.
    IF errors IS BOUND.
      APPEND LINES OF errors->get( ) TO errors_tab.
    ENDIF.
  ENDMETHOD.

  METHOD add.
    APPEND error TO errors_tab.
  ENDMETHOD.

  METHOD append.
    APPEND LINES OF errors->get( ) TO errors_tab.
  ENDMETHOD.

  METHOD has_any.
    has_any = xsdbool( lines( errors_tab ) <> 0 ).
  ENDMETHOD.

  METHOD get.
    errors = me->errors_tab.
  ENDMETHOD.

  METHOD get_as_string.
    LOOP AT errors_tab REFERENCE INTO DATA(error_ref).
      error = |{ error } { error_ref->* }|.
      IF sy-tabix = 1. "Remove extra space.
        error = substring( val = error off = 1 len = strlen( error ) - 1 ).
      ENDIF.

      IF length_restriction = 0.
        CONTINUE.
      ENDIF.

      IF strlen( error ) > length_restriction.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF length_restriction > 0 AND strlen( error ) > length_restriction.
      error = substring( val = error off = 0 len = length_restriction ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
