"! <p class="shorttext synchronized" lang="en">Custom exception class for easy throwing with custom message</p>
"! <br/>TAGS: exception
CLASS zcx_ed_exception DEFINITION PUBLIC INHERITING FROM cx_dynamic_check.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING custom_message TYPE string OPTIONAL,
      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA:
      custom_message TYPE string.
ENDCLASS.

CLASS zcx_ed_exception IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ).
    me->custom_message = custom_message.
  ENDMETHOD.

  METHOD get_text.
    result = me->custom_message.
  ENDMETHOD.
ENDCLASS.
