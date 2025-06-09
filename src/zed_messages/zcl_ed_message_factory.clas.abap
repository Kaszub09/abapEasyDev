"! <p class="shorttext synchronized">Message factory</p>
"! <br/>TAGS: message; mail; factory; send
CLASS zcl_ed_message_factory DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ed_message_factory_inj.

  PUBLIC SECTION.
    CLASS-METHODS:
     "! @parameter content_type | <p class="shorttext synchronized" lang="en">E.g. HTM for html or TXT/RAW for simple text.</p>
     create IMPORTING content_type TYPE so_obj_tp DEFAULT space header TYPE so_obj_des body TYPE string
            RETURNING VALUE(message) TYPE REF TO zif_ed_message
            RAISING cx_document_bcs cx_send_req_bcs.

  PRIVATE SECTION.
    CLASS-DATA:
        message_obj TYPE REF TO zif_ed_message.
ENDCLASS.

CLASS zcl_ed_message_factory IMPLEMENTATION.
  METHOD create.
    message = COND #( WHEN message_obj IS BOUND THEN message_obj
        ELSE NEW zcl_ed_message( content_type = content_type header = header body = body ) ).
  ENDMETHOD.
ENDCLASS.
