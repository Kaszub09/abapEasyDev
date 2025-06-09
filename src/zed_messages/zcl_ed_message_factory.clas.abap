"! <p class="shorttext synchronized">Message factory</p>
"! <br/>TAGS: message; mail; factory
CLASS zcl_ed_message_factory DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ed_message_factory_inj.

  PUBLIC SECTION.
    CLASS-METHODS:
      create RETURNING VALUE(message) TYPE REF TO zif_ed_message.

  PRIVATE SECTION.
    CLASS-DATA:
        message_obj TYPE REF TO zif_ed_message.
ENDCLASS.

CLASS zcl_ed_message_factory IMPLEMENTATION.
  METHOD create.
    message = COND #( WHEN message_obj IS BOUND THEN message_obj ELSE NEW zcl_ed_message( ) ).
  ENDMETHOD.
ENDCLASS.
