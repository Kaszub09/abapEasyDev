"! <p class="shorttext synchronized">Easy messages</p>
"! <br/>TAGS: message; exception; bapiret2; bapi_coru_return; bdcmsgcoll
CLASS zcl_ed_msg DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS:
      get IMPORTING text TYPE csequence v1 TYPE any OPTIONAL v2 TYPE any OPTIONAL v3 TYPE any OPTIONAL v4 TYPE any OPTIONAL RETURNING VALUE(msg) TYPE string,
      "! <p class="shorttext synchronized" lang="en">Throw <em>zcx_ed_exception</em> with given message</p>
      throw IMPORTING text TYPE csequence v1 TYPE any OPTIONAL v2 TYPE any OPTIONAL v3 TYPE any OPTIONAL v4 TYPE any OPTIONAL RAISING zcx_ed_exception,
      get_from_bapiret2 IMPORTING bapiret2 TYPE bapiret2 RETURNING VALUE(msg) TYPE string,
      get_from_bapi_coru_return IMPORTING bapi_coru_return TYPE bapi_coru_return RETURNING VALUE(msg) TYPE string,
      get_from_bdcmsgcoll IMPORTING bdcmsgcoll TYPE bdcmsgcoll RETURNING VALUE(msg) TYPE string,
      get_from_sy RETURNING VALUE(msg) TYPE string.
ENDCLASS.

CLASS zcl_ed_msg IMPLEMENTATION.
  METHOD get.
    msg = text.
    IF v1 IS SUPPLIED.
      msg = replace( val = msg sub = '&1' occ = 0 with = v1 ).
    ENDIF.
    IF v2 IS SUPPLIED.
      msg = replace( val = msg sub = '&2' occ = 0 with = v2 ).
    ENDIF.
    IF v3 IS SUPPLIED.
      msg = replace( val = msg sub = '&3' occ = 0 with = v3 ).
    ENDIF.
    IF v4 IS SUPPLIED.
      msg = replace( val = msg sub = '&4' occ = 0 with = v4 ).
    ENDIF.
  ENDMETHOD.

  METHOD throw.
    RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = get( text = text v1 = v1 v2 = v2 v3 = v3 v4 = v4 ).
  ENDMETHOD.

  METHOD get_from_bapiret2.
    MESSAGE ID bapiret2-id TYPE bapiret2-type NUMBER bapiret2-number WITH bapiret2-message_v1 bapiret2-message_v2 bapiret2-message_v3 bapiret2-message_v4 INTO msg.
  ENDMETHOD.

  METHOD get_from_bapi_coru_return.
    MESSAGE ID bapi_coru_return-id TYPE bapi_coru_return-type NUMBER bapi_coru_return-number
      WITH bapi_coru_return-message_v1 bapi_coru_return-message_v2 bapi_coru_return-message_v3 bapi_coru_return-message_v4 INTO msg.
  ENDMETHOD.

  METHOD get_from_bdcmsgcoll.
    MESSAGE ID bdcmsgcoll-msgid TYPE bdcmsgcoll-msgtyp NUMBER bdcmsgcoll-msgnr
      WITH bdcmsgcoll-msgv1 bdcmsgcoll-msgv2 bdcmsgcoll-msgv3 bdcmsgcoll-msgv4 INTO msg.
  ENDMETHOD.

  METHOD get_from_sy.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO msg.
  ENDMETHOD.
ENDCLASS.
