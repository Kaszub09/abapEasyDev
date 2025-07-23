"! <p class="shorttext synchronized">String manipulation with length/offset checks</p>
"! <br/>TAGS: strings
CLASS zcl_ed_string DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      left IMPORTING val TYPE csequence len TYPE i RETURNING VALUE(left) TYPE string,
      right IMPORTING val TYPE csequence len TYPE i RETURNING VALUE(right) TYPE string,
      sub IMPORTING val TYPE csequence off TYPE i DEFAULT 0 len TYPE i RETURNING VALUE(sub) TYPE string.
ENDCLASS.

CLASS zcl_ed_string IMPLEMENTATION.
  METHOD left.
    IF len > strlen( val ).
      left = val.
    ELSE.
      left = substring( val = val len = len ).
    ENDIF.
  ENDMETHOD.

  METHOD right.
    IF len > strlen( val ).
      right = val.
    ELSE.
      right = substring( val = val off = strlen( val ) - len len = len ).
    ENDIF.
  ENDMETHOD.

  METHOD sub.
    IF off >= strlen( val ).
      RETURN.
    ENDIF.

    DATA(length) = len.
    IF strlen( val ) < off + len.
      length = strlen( val ) - off.
    ENDIF.
    sub = substring( val = val off = off len = length ).
  ENDMETHOD.
ENDCLASS.
