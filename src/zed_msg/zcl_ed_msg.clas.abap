"! <p class="shorttext synchronized">Easy messages</p>
"! <br/>TAGS: message; exception; bapiret2; bapi_coru_return; bdcmsgcoll
class ZCL_ED_MSG definition
  public
  final
  create public .

public section.

  class-methods GET
    importing
      !TEXT type CSEQUENCE
      !V1 type ANY optional
      !V2 type ANY optional
      !V3 type ANY optional
      !V4 type ANY optional
    returning
      value(MSG) type STRING .
      "! <p class="shorttext synchronized" lang="en">Throw &lt;em&gt;zcx_ed_exception&lt;/em&gt; with given message</p>
  class-methods THROW
    importing
      !TEXT type CSEQUENCE
      !V1 type ANY optional
      !V2 type ANY optional
      !V3 type ANY optional
      !V4 type ANY optional
    raising
      ZCX_ED_EXCEPTION .
  class-methods GET_FROM_BAPIRET2
    importing
      !BAPIRET2 type BAPIRET2
    returning
      value(MSG) type STRING .
  class-methods GET_FROM_BAPI_CORU_RETURN
    importing
      !BAPI_CORU_RETURN type BAPI_CORU_RETURN
    returning
      value(MSG) type STRING .
  class-methods GET_FROM_BDCMSGCOLL
    importing
      !BDCMSGCOLL type BDCMSGCOLL
    returning
      value(MSG) type STRING .
  class-methods GET_FROM_SY
    returning
      value(MSG) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ED_MSG IMPLEMENTATION.


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


  METHOD throw.
    RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = get( text = text v1 = v1 v2 = v2 v3 = v3 v4 = v4 ).
  ENDMETHOD.
ENDCLASS.
