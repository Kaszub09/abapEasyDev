CLASS lcl_report DEFINITION INHERITING FROM zcl_ea_salv_table.
  PUBLIC SECTION.
    METHODS:
      prepare_report.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF t_output,
        is_global    TYPE abap_bool,
        infoset      TYPE aqs_sgname,
        infoset_text TYPE aqs_txt80,
        group        TYPE aqs_bgname,
        query        TYPE aqs_quname,
        query_tab    TYPE aqs_tname,
        query_text   TYPE aqs_txt255,
        tcode        TYPE tcode,
        tcode_text   TYPE ttext_stct,
      END OF t_output,
      tt_output TYPE STANDARD TABLE OF t_output WITH EMPTY KEY
        WITH  NON-UNIQUE SORTED KEY query COMPONENTS query.

    METHODS:
      on_link_click REDEFINITION.

    METHODS:
      fill_transaction_info.

    DATA:
     output TYPE tt_output.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD prepare_report.
    "--------------------------------------------------
    "Prepare queries based on transactions - name should be enough
    DATA: query_range_from_tcode TYPE RANGE OF tcode.

    IF lines( s_tcode[] ) > 0.
      SELECT param FROM tstcp WHERE tcode IN @s_tcode INTO TABLE @DATA(transactions).
      LOOP AT transactions REFERENCE INTO DATA(transaction).
        DATA(matcher) = cl_abap_matcher=>create( pattern = |EXTDREPORT=([^;]+);| text = transaction->param ).
        IF matcher->find_next( ).
          APPEND VALUE #( sign = 'I' option = 'EQ' low = matcher->get_submatch( 1 ) ) TO query_range_from_tcode.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "--------------------------------------------------
    SELECT FROM aqgscat AS infoset
      LEFT JOIN aqgts AS infoset_texts ON infoset_texts~sprsl = @sy-langu AND infoset_texts~clas = infoset~clas AND infoset_texts~head = @abap_true
      LEFT JOIN aqgdbbs AS infoset_group ON infoset_group~clas = infoset~clas
      LEFT JOIN aqgqcat AS query ON query~clas = infoset~clas AND query~num = infoset_group~num
      LEFT JOIN aqgqstruc AS query_struct ON query_struct~usergroup = query~num AND query_struct~query = query~qnum
      LEFT JOIN aqgtq AS query_texts ON query_texts~sprsl = @sy-langu AND query_texts~num = query~num AND query_texts~qnum = query~qnum
        AND query_texts~head = @abap_true
    FIELDS @abap_true AS is_global, infoset~clas AS infoset, infoset_texts~text AS infoset_text, infoset_group~num AS group,
        query~qnum AS query, query_struct~gobjname AS query_tab, query_texts~text AS query_text
    WHERE infoset~clas IN @s_clas AND infoset_group~num IN @s_num AND query~qnum IN @s_qnum AND @abap_true IN @s_global
        AND query~qnum IN @query_range_from_tcode
    INTO CORRESPONDING FIELDS OF TABLE @output.

    SELECT FROM aqlscat AS infoset
      LEFT JOIN aqlts AS infoset_texts ON infoset_texts~sprsl = @sy-langu AND infoset_texts~clas = infoset~clas AND infoset_texts~head = @abap_true
     " LEFT JOIN aqgdbbs AS infoset_group ON infoset_group~clas = infoset~clas
      LEFT JOIN aqlqcat AS query ON query~clas = infoset~clas "AND query~num = infoset_group~num
      LEFT JOIN aqlqstruc AS query_struct ON query_struct~usergroup = query~num AND query_struct~query = query~qnum
      LEFT JOIN aqltq AS query_texts ON query_texts~sprsl = @sy-langu AND query_texts~num = query~num AND query_texts~qnum = query~qnum
        AND query_texts~head = @abap_true
    FIELDS @abap_false AS is_global, infoset~clas AS infoset, infoset_texts~text AS infoset_text, query~num AS group,
        query~qnum AS query, query_struct~gobjname AS query_tab, query_texts~text AS query_text
    WHERE infoset~clas IN @s_clas AND query~num IN @s_num AND query~qnum IN @s_qnum AND @abap_false IN @s_global
        AND query~qnum IN @query_range_from_tcode
    APPENDING CORRESPONDING FIELDS OF TABLE @output.


    "--------------------------------------------------
    fill_transaction_info( ).

    set_data( REF #( output ) ).

    "--------------------------------------------------
    DATA(blue) = VALUE lvc_s_colo( col = 1 ).
    columns->set_color( column = 'INFOSET_TEXT' color = blue ).
    columns->set_color( column = 'QUERY_TEXT' color = blue ).
    columns->set_color( column = 'TCODE_TEXT' color = blue ).

    columns->set_as_hotspot( 'INFOSET' ).
    columns->set_as_hotspot( 'QUERY' ).
    columns->set_as_hotspot( 'QUERY_TAB' ).
    columns->set_as_hotspot( 'TCODE' ).

    columns->set_fixed_text( column = 'IS_GLOBAL' text = TEXT-001 ).
  ENDMETHOD.

  METHOD fill_transaction_info.
    DATA: query_param TYPE RANGE OF tcode.

    query_param = VALUE #( FOR <query> IN output ( sign = 'I' option = 'CP' low = |*{ <query>-query }*| ) ).

    SELECT FROM tstcp
        LEFT JOIN tstct ON tstct~sprsl = @sy-langu AND tstct~tcode = tstcp~tcode
    FIELDS tstcp~tcode, param, ttext
    WHERE param IN @query_param
    INTO TABLE @DATA(transactions).

    LOOP AT transactions REFERENCE INTO DATA(tran_descr).
      DATA(matcher) = cl_abap_matcher=>create( pattern = |EXTDREPORT=([^;]+);| text = tran_descr->param ).
      IF matcher->find_next( ).
        DATA(query) = CONV aqs_quname( matcher->get_submatch( 1 ) ).
        LOOP AT output REFERENCE INTO DATA(row) USING KEY query WHERE query = query.
          IF tran_descr->param CP |*REPORT={ row->group }*|.
            row->tcode = tran_descr->tcode.
            row->tcode_text = tran_descr->ttext.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_link_click.
    IF row = 0. RETURN. ENDIF.
    DATA(row_ref) = REF #( output[ row ] ).

    DATA(bdc) = NEW zcl_ed_bdc( ).
    "Leave as abap_true since group selection in SQ01 is broken without it
    DATA(options) = bdc->get_options( nobiend = abap_true nobinpt = abap_true ).
    CASE column.
      WHEN 'INFOSET'.
        bdc->begin_screen( program = 'SAPMS38O' dynpro = '0050'
          "Set global/mandt
          )->set_code( '=WSHD'
          )->begin_screen( program = 'SAPLAQWS' dynpro = '0100'
          )->set_value( fnam = COND #( WHEN row_ref->is_global = abap_true THEN 'RAD2' ELSE 'RAD1' )  fval = 'X'
          )->set_code( '=SELC'
          )->begin_screen( program = 'SAPMS38O' dynpro = '0050'
          "Set infoset name
          )->set_value( fnam = 'RS38Q-NAME' fval = CONV #( row_ref->infoset )
          )->call_transaction( tcode = 'SQ02' options = options ).

      WHEN 'QUERY'.
        bdc->begin_screen( program = 'SAPMS38R' dynpro = '0050'
          "Set global/mandt
          )->set_code( '=WSHD'
          )->begin_screen( program = 'SAPLAQWS' dynpro = '0100'
          )->set_value( fnam = COND #( WHEN row_ref->is_global = abap_true THEN 'RAD2' ELSE 'RAD1' )  fval = 'X'
          )->set_code( '=SELC'
          )->begin_screen( program = 'SAPMS38R' dynpro = '0050'

          "Can't select on ALV
*          )->set_code( '=CHUG'
*          )->begin_screen( program = 'SAPLSLVC_FULLSCREEN' dynpro = '0700'
*          )->set_cursor( '04/03'
*          )->set_code( '=&ILT'

        "Doesn't work (requires abap_false nobiend and nobinpt) - probably SQ01 or straight up CL_SALV_TABLE is broken?
        "Can't select at all
*          )->set_code( '=CHUG'
*          )->begin_screen( program = 'SAPMSSY0' dynpro = '0120'
*          )->set_cursor( '04/03'
*          )->set_code( '=&ILT'
*          )->begin_screen( program = 'SAPLSSEL' dynpro = '1104'
*          )->set_value( fnam = '%%DYN001-LOW' fval = CONV #( row_ref->group )
*          )->set_code( '=CRET'
*          )->begin_screen( program = 'SAPMSSY0' dynpro = '0120'
*          )->set_cursor( '04/03'
*          )->set_code( '=SELC'

        "Sometimes work, sometimes doesn't
          )->set_code( '=SQ03'
          )->begin_screen( program = 'SAPMS38S' dynpro = '0050'
          )->set_value( fnam = 'RS38S-BGNUM' fval = CONV #( row_ref->group )
          )->set_code( '=BACK'
          "Set query name
          )->begin_screen( program = 'SAPMS38R' dynpro = '0050'
          )->set_value( fnam = 'RS38R-QNUM' fval = CONV #( row_ref->query )
          )->call_transaction( tcode = 'SQ01' options = options ).

      WHEN 'QUERY_TAB'.
        DATA(lib_parameter) = CONV rs38l_fnam( |{ row_ref->query_tab }EXTR| ).
        SET PARAMETER ID 'LIB' FIELD lib_parameter.
        CALL TRANSACTION 'SE37' WITH AUTHORITY-CHECK.

      WHEN 'TCODE'.
        CALL TRANSACTION row_ref->tcode WITH AUTHORITY-CHECK.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
