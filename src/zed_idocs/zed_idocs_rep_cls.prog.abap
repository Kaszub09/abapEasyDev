CLASS lcl_report DEFINITION INHERITING FROM zcl_ea_salv_table_ext_row_info.
  PUBLIC SECTION.
    METHODS:
      prepare_report.

  PROTECTED SECTION.
    METHODS:
      on_added_function REDEFINITION,
      on_link_click REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_functions,
        bd87 TYPE sy-ucomm VALUE 'BD87',
        we19 TYPE sy-ucomm VALUE 'WE19',
      END OF c_functions.

    TYPES:
      BEGIN OF t_output.
        INCLUDE TYPE zed_idocs.
      TYPES:
      END OF t_output,
      tt_output TYPE STANDARD TABLE OF t_output WITH EMPTY KEY.

    METHODS:
      prepare_functions,
      prepare_columns.

    DATA:
        output TYPE tt_output.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD prepare_report.
    SELECT * FROM zed_idocs
    WHERE docnum IN @s_docnum AND serial IN @s_serial AND credat IN @s_credat AND direct IN @s_direct AND idoctp IN @s_idoctp
        AND cimtyp IN @s_cimtyp AND mestyp IN @s_mestyp
    ORDER BY credat DESCENDING, cretim DESCENDING
    INTO CORRESPONDING FIELDS OF TABLE @output
    UP TO @p_rownum ROWS.

    set_data( REF #( output ) ).

    prepare_columns( ).

    prepare_functions( ).
  ENDMETHOD.

  METHOD prepare_columns.
    columns->set_as_exception( 'STATUS_EXCEPTION_COL_VALUE' ).

    columns->set_color( column = 'DOCNUM'  color =  VALUE #( col = 1 int = 1 ) ).

    columns->set_color( column = 'DIRECT'  color =  VALUE #( col = 4 ) ).
    columns->set_color( column = 'RCVPOR'  color =  VALUE #( col = 4 ) ).
    columns->set_color( column = 'RCVPRT'  color =  VALUE #( col = 4 ) ).
    columns->set_color( column = 'RCVPRN'  color =  VALUE #( col = 4 ) ).
    columns->set_color( column = 'SNDPOR'  color =  VALUE #( col = 4 ) ).
    columns->set_color( column = 'SNDPRT'  color =  VALUE #( col = 4 ) ).
    columns->set_color( column = 'SNDPRN'  color =  VALUE #( col = 4 ) ).

    columns->set_color( column = 'IDOCTP'  color =  VALUE #( col = 3 ) ).
    columns->set_color( column = 'CIMTYP'  color =  VALUE #( col = 3 ) ).
    columns->set_color( column = 'EVCODE'  color =  VALUE #( col = 3 ) ).

    columns->set_color( column = 'EVENTT'  color =  VALUE #( col = 7 ) ).
    columns->set_color( column = 'ROUTID'  color =  VALUE #( col = 7 ) ).
    columns->set_color( column = 'EVENTN'  color =  VALUE #( col = 7 ) ).
    columns->set_color( column = 'EVENXX'  color =  VALUE #( col = 7 ) ).
    columns->set_color( column = 'DESCRP'  color =  VALUE #( col = 7 ) ).

    columns->set_as_hotspot( 'DOCNUM' ).

    columns->set_as_hotspot( 'RCVPOR' ).
    columns->set_as_hotspot( 'RCVPRT' ).
    columns->set_as_hotspot( 'RCVPRN' ).
    columns->set_as_hotspot( 'SNDPOR' ).
    columns->set_as_hotspot( 'SNDPRT' ).
    columns->set_as_hotspot( 'SNDPRN' ).

    columns->set_as_hotspot( 'MESTYP' ).

    columns->set_as_hotspot( 'IDOCTP' ).
    columns->set_as_hotspot( 'CIMTYP' ).

    columns->set_as_hotspot( 'ROUTID' ).
  ENDMETHOD.

  METHOD on_link_click.
    IF row = 0.
      RETURN.
    ENDIF.
    DATA(row_ref) = REF #( output[ row ] ).

    CASE column.
      WHEN 'DOCNUM'.
        CALL FUNCTION 'BAPI_IDOCAPPL_DISPLAY'
          EXPORTING
            idocnumber = row_ref->docnum
          EXCEPTIONS
            OTHERS     = 1.

      WHEN 'RCVPOR' OR 'RCVPRT' OR 'RCVPRN' OR 'SNDPOR' OR 'SNDPRT' OR 'SNDPRN'.
        NEW zcl_ed_bdc(
          )->begin_screen( program = 'SAPMSEDIPARTNER' dynpro = '0100'
          )->set_code( '=TRPO'
          )->begin_screen( program = 'SAPMSEDIPARTNER' dynpro = '0200'
          )->set_value( fnam = 'GSO_PNUM-LOW' fval = COND #( WHEN column(3) = 'RCV' THEN row_ref->rcvprn ELSE row_ref->sndprn )
          )->set_value( fnam = 'GSO_PTYP-LOW' fval = COND #( WHEN column(3) = 'RCV' THEN row_ref->rcvprt ELSE row_ref->sndprt )
          )->set_code( '=CRET'
          )->call_transaction( tcode = 'WE20' options = zcl_ed_bdc=>get_options( ) ).

      WHEN 'CIMTYP' OR 'IDOCTP'.
        IF column = 'CIMTYP' AND row_ref->cimtyp = space. "IDOCTP must exist
          RETURN.
        ENDIF.
        DATA(bdc) = NEW zcl_ed_bdc(
          )->begin_screen( program = 'SAPMSED5' dynpro = '0010' ).
        bdc->set_value( fnam = 'SED5STRUC-OBJECT' fval = COND #( WHEN column = 'IDOCTP' THEN row_ref->idoctp ELSE row_ref->cimtyp ) ).
        bdc->set_value( fnam = COND #( WHEN column = 'IDOCTP' THEN 'SED5STRUC-SELECT_ORG' ELSE 'SED5STRUC-SELECT_EXT' ) fval = CONV #( abap_true ) ).
        bdc->set_code( '=DISP'
          )->call_transaction( tcode = 'WE30' options = zcl_ed_bdc=>get_options( ) ).

      WHEN 'ROUTID'.
        IF row_ref->routid = space.
          RETURN.
        ENDIF.
        NEW zcl_ed_bdc(
          )->begin_screen( program = 'SAPLSFUNCTION_BUILDER' dynpro = '1008'
          )->set_value( fnam = 'RS38L-NAME' fval = CONV #( row_ref->routid )
          )->set_code( '=WB_DISPLAY'
          )->call_transaction( tcode = 'SE37' options = zcl_ed_bdc=>get_options( nobinpt = abap_true ) ).

      WHEN 'MESTYP'.
        IF row_ref->mestyp = space.
          RETURN.
        ENDIF.
        NEW zcl_ed_bdc(
          )->begin_screen( program = 'SAPMSEDMESTYP' dynpro = '0100'
          )->set_code( '=SEAR'
          )->begin_screen( program = 'SAPMSEDMESTYP' dynpro = '0300'
          )->set_value( fnam = 'EDI_HELP-MESTYP' fval = CONV #( row_ref->mestyp )
          )->set_value( fnam = COND #( WHEN row_ref->direct = '2' THEN 'EDI_HELP-INCOMING' ELSE 'EDI_HELP-OUTGOING' ) fval = conv #( abap_true )
          )->set_code( '=CONT'
          )->call_transaction( tcode = 'WE64' options = zcl_ed_bdc=>get_options( nobinpt = abap_true ) ).
    ENDCASE.
  ENDMETHOD.

  METHOD prepare_functions.
    functions->add_function( function = c_functions-bd87 description = VALUE #( icon_id = '@15@' icon_text = TEXT-f01 ) ).
    functions->add_function( function = c_functions-we19 description = VALUE #( icon_id = '@14@' icon_text = TEXT-f02  ) ).
  ENDMETHOD.

  METHOD on_added_function.
    DATA(selected_rows) = alv_table->get_selections( )->get_selected_rows( ).
    IF lines( selected_rows ) > 1 AND lines( output ) <> 1.
      MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    DATA(row_ref) = REF #( output[ VALUE #( selected_rows[ 1 ] DEFAULT 1 ) ] ).

    CASE function.
      WHEN c_functions-bd87.
        NEW zcl_ed_bdc(
          )->begin_screen( program = 'RBDMON00' dynpro = '1100'
          )->set_value( fnam = 'SX_DOCNU-LOW' fval = CONV #( row_ref->docnum )
          )->set_value( fnam = 'SX_UPDDA-LOW' fval = space
          )->set_value( fnam = 'SX_UPDDA-HIGH' fval = space
          )->set_value( fnam = 'SX_PRPRN-LOW' fval = space
          )->set_code( '=CRET'
          )->call_transaction( tcode = 'BD87' options = zcl_ed_bdc=>get_options( nobinpt = abap_true ) ).

      WHEN c_functions-we19.
        NEW zcl_ed_bdc(
          )->begin_screen( program = 'SAPMSED7' dynpro = '0010'
          )->set_value( fnam = 'MSED7START-SEL_EXIDOC' fval = CONV #( abap_true )
          )->set_value( fnam = 'MSED7START-EXIDOCNUM' fval = CONV #( row_ref->docnum )
          )->set_code( '=CREA'
          )->call_transaction( tcode = 'WE19' options = zcl_ed_bdc=>get_options( ) ).

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
