CLASS zcx_ed_change_doc_no_pos_ins DEFINITION PUBLIC INHERITING FROM cx_dynamic_check FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_t100_dyn_msg,
      if_t100_message.

    METHODS:
      constructor IMPORTING textid LIKE if_t100_message=>t100key OPTIONAL previous LIKE previous OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcx_ed_change_doc_no_pos_ins IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
