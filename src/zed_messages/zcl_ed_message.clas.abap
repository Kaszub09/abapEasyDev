CLASS zcl_ed_message DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ed_message_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_message.

    METHODS:
      constructor.

  PRIVATE SECTION.
    DATA:
      message  TYPE REF TO cl_bcs,
      document TYPE REF TO cl_document_bcs.
ENDCLASS.

CLASS zcl_ed_message IMPLEMENTATION.
  METHOD constructor.
    message = cl_bcs=>create_persistent( ).
  ENDMETHOD.

  METHOD zif_ed_message~add_recipient_mail.
    message->add_recipient( cl_cam_address_bcs=>create_internet_address( mail ) ).
    self = me.
  ENDMETHOD.

  METHOD zif_ed_message~add_recipient_user.
    message->add_recipient( i_recipient = cl_sapuser_bcs=>create( user ) i_express = express ).
    self = me.
  ENDMETHOD.

  METHOD zif_ed_message~add_recipient_mail_from_user.
    message->add_recipient( cl_cam_address_bcs=>create_user_home_address( i_commtype = 'INT' i_user = user ) ).
    self = me.
  ENDMETHOD.

  METHOD zif_ed_message~add_recipients_from_dist_list.
    message->add_recipient( cl_distributionlist_bcs=>getu_persistent( i_dliname = distribution_list i_private = is_private ) ).
    self = me.
  ENDMETHOD.

  METHOD zif_ed_message~add_attachment.
    document->add_attachment( i_attachment_type = att_type i_attachment_subject = name
        i_attachment_size = CONV #( xstrlen( att ) ) i_att_content_hex = cl_bcs_convert=>xstring_to_solix( att ) ).
    self = me.
  ENDMETHOD.

  METHOD zif_ed_message~set_content.
    document = cl_document_bcs=>create_document( i_type = content_type i_subject = header
        i_length = CONV #( strlen( body ) ) i_text = cl_bcs_convert=>string_to_soli( body ) ).
    self = me.
  ENDMETHOD.

  METHOD zif_ed_message~set_note.
    message->set_note( cl_bcs_convert=>string_to_soli( note ) ).
    self = me.
  ENDMETHOD.

  METHOD zif_ed_message~send.
    message->set_document( document ).
    send_to_all = message->send( ).
  ENDMETHOD.

ENDCLASS.
