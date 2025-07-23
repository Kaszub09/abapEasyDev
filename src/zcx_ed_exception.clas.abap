"! <p class="shorttext synchronized">Custom exception class for easy throwing with custom message.
"! <em>SUBRC</em> can be checked for specific error if needed (e.g. from function module exception.</p>
"! <br/>TAGS: exception
CLASS zcx_ed_exception DEFINITION PUBLIC INHERITING FROM cx_dynamic_check.
  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized">Alternative to <em>constructor</em> since sy-subrc is stored implicitly.
      "! With <em>constructor</em> must be copied to helper variable.</p>
      throw IMPORTING custom_message TYPE string VALUE(subrc) TYPE syst_subrc DEFAULT sy-subrc previous LIKE previous OPTIONAL
            RAISING zcx_ed_exception.

    METHODS:
      "! @parameter subrc | <p class="shorttext synchronized">Can't be passed by sy-subrc!
      "! It's cleared before constructor is called - use helper variable before</p>
      constructor IMPORTING custom_message TYPE string subrc TYPE syst_subrc OPTIONAL previous LIKE previous OPTIONAL,
      get_text REDEFINITION.

    DATA:
      subrc TYPE syst_subrc READ-ONLY.

  PRIVATE SECTION.
    DATA:
      custom_message TYPE string.
ENDCLASS.

CLASS zcx_ed_exception IMPLEMENTATION.
  METHOD throw.
    RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = custom_message subrc = subrc previous = previous.
  ENDMETHOD.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ).
    me->custom_message = custom_message.
    me->subrc = subrc.
    me->previous = previous.
  ENDMETHOD.

  METHOD get_text.
    result = me->custom_message.
DATA(subrc) = CONV sy-subrc( sy-subrc ).
  ENDMETHOD.
ENDCLASS.
