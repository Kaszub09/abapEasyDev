CLASS zcl_ed_icf_file_handler DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      if_rest_resource~get REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ed_icf_file_handler IMPLEMENTATION.


  METHOD if_rest_resource~get.
    DATA(path) =  mo_request->get_header_field( '~path' ).
    IF zcl_ed_string=>right( val = path len = 1 ) = '/'.
      path = zcl_ed_string=>left( val = path len = strlen( path ) - 1 ).
    ENDIF.
    DATA(entity) = me->mo_response->create_entity( ).

    SELECT SINGLE * FROM zed_icf_binary WHERE path = @path INTO @DATA(icf_binary).
    IF sy-subrc = 0.
      entity->set_content_type( CONV #( icf_binary-media_type ) ).
      me->mo_response->create_entity( )->set_binary_data( icf_binary-content ).
      me->mo_response->set_status( 200 ).
      RETURN.
    ENDIF.

    me->mo_response->set_status( 404 ).
  ENDMETHOD.
ENDCLASS.
