"! <p class="shorttext synchronized" lang="en">Message</p>
"! "! <br/>TAGS: message; mail; send
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
    add_recipients_from_dist_list IMPORTING distribution_list TYPE so_obj_nam is_private TYPE abap_bool
                                  RETURNING VALUE(self) TYPE REF TO zif_ed_message
                                  RAISING cx_address_bcs cx_send_req_bcs,
    "! @parameter att_type | <p class="shorttext synchronized" lang="en">E.g. XLS for Excel.</p>
    add_attachment IMPORTING att_type TYPE so_obj_tp DEFAULT space name TYPE so_obj_des att TYPE xstring
                   RETURNING VALUE(self) TYPE REF TO zif_ed_message
                   RAISING cx_document_bcs,
    set_note IMPORTING note TYPE string
             RETURNING VALUE(self) TYPE REF TO zif_ed_message
             RAISING cx_send_req_bcs,
    "! <p class="shorttext synchronized" lang="en">You must call COMMIT WORK after for message to be sent.</p>
    send RETURNING VALUE(send_to_all) TYPE abap_bool
         RAISING cx_send_req_bcs.
ENDINTERFACE.
