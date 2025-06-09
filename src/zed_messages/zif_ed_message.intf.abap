INTERFACE zif_ed_message PUBLIC.
  METHODS:
    add_recipient_mail IMPORTING mail TYPE ad_smtpadr
                       RETURNING VALUE(self) TYPE REF TO zif_ed_message
                       RAISING cx_address_bcs cx_send_req_bcs,
    add_recipient_user IMPORTING user TYPE uname express TYPE abap_bool DEFAULT abap_false
                       RETURNING VALUE(self) TYPE REF TO zif_ed_message
                       RAISING cx_address_bcs cx_send_req_bcs,
    add_recipient_mail_from_user IMPORTING user TYPE uname
                                 RETURNING VALUE(self) TYPE REF TO zif_ed_message
                                 RAISING cx_address_bcs cx_send_req_bcs,
    "! <p class="shorttext synchronized" lang="en">WARNING! <strong>set_content</strong> must be called first!</p>
    "! @parameter att_type | <p class="shorttext synchronized" lang="en">E.g. XLS for Excel.</p>
    add_attachment IMPORTING att_type TYPE so_obj_tp DEFAULT space name TYPE so_obj_des att TYPE xstring
                   RETURNING VALUE(self) TYPE REF TO zif_ed_message
                   RAISING cx_document_bcs,
    "! <p class="shorttext synchronized" lang="en">Must be called before <strong>add_attachment</strong></p>
    "! @parameter content_type | <p class="shorttext synchronized" lang="en">E.g. HTM for html or TXT/RAW for simple text.</p>
    set_content IMPORTING content_type TYPE so_obj_tp DEFAULT space header TYPE so_obj_des body TYPE string
                RETURNING VALUE(self) TYPE REF TO zif_ed_message
                RAISING cx_document_bcs,
    set_note IMPORTING note TYPE string
             RETURNING VALUE(self) TYPE REF TO zif_ed_message
             RAISING cx_send_req_bcs,
    "! <p class="shorttext synchronized" lang="en">You must call COMMIT WORK after for message to be sent.</p>
    send RETURNING VALUE(send_to_all) TYPE abap_bool
         RAISING cx_send_req_bcs.
ENDINTERFACE.
