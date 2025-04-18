"! <p class="shorttext synchronized">String manipulation</p>
"! <br/>TAGS: strings
CLASS zcl_ed_string DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      left IMPORTING val TYPE csequence len TYPE i RETURNING VALUE(left) TYPE string,
      right IMPORTING val TYPE csequence len TYPE i RETURNING VALUE(right) TYPE string.
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
ENDCLASS.
