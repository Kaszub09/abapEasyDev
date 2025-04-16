"! <p class="shorttext synchronized" lang="en">Custom exception class for easy throwing with custom message.
"! <em>SUBRC</em> can be checked for specific error if needed (e.g. from function module exception.</p>
"! <br/>TAGS: exception
CLASS zcx_ed_exception DEFINITION PUBLIC INHERITING FROM cx_dynamic_check.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING custom_message TYPE string OPTIONAL subrc TYPE syst_subrc  DEFAULT sy-subrc,
      get_text REDEFINITION.
    DATA:
      subrc TYPE syst_subrc READ-ONLY.
  PRIVATE SECTION.
    DATA:
      custom_message TYPE string.
ENDCLASS.

CLASS zcx_ed_exception IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ).
    me->custom_message = custom_message.
    me->subrc = subrc.
  ENDMETHOD.

  METHOD get_text.
    result = me->custom_message.
  ENDMETHOD.
ENDCLASS.
