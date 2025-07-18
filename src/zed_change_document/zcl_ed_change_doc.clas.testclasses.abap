 "=================================================================
 "-----------------------------------------------------------------
 CLASS tcl_create_tab DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

   PRIVATE SECTION.
     TYPES:
       tt_zed_cd_test_tab TYPE STANDARD TABLE OF zed_cd_test_tab WITH EMPTY KEY,
       BEGIN OF t_expected.
         INCLUDE TYPE zed_cd_test_tab.
       TYPES:
         zed_change_indicator_field TYPE cdchngindh,
       END OF t_expected,
       tt_expected TYPE STANDARD TABLE OF t_expected WITH EMPTY KEY.

     METHODS:
       create_tab_with_indicator FOR TESTING,
       create_sorted_tab FOR TESTING.

     CLASS-DATA:
       table_name TYPE tabname VALUE 'ZED_CD_TEST_TAB'.
 ENDCLASS.

 CLASS tcl_create_tab IMPLEMENTATION.
   METHOD create_tab_with_indicator.
     DATA(tab) = VALUE tt_zed_cd_test_tab( ( key_no_track = 'B' ) ( key_no_track = 'A' ) ).
     DATA(tab_with_indicator) = zcl_ed_change_doc_factory=>create( )->create_table_with_indicator(
        table_name = table_name original_table = REF #( tab ) indicator = 'X' ).
     FIELD-SYMBOLS: <tab_with_indicator> TYPE table.
     ASSIGN tab_with_indicator->* TO <tab_with_indicator>.

     DATA(expected_tab) = VALUE tt_expected( ( key_no_track = 'B' zed_change_indicator_field = 'X' ) ( key_no_track = 'A' zed_change_indicator_field = 'X' ) ).

     cl_abap_unit_assert=>assert_equals( act = <tab_with_indicator> exp = expected_tab ).
   ENDMETHOD.

   METHOD create_sorted_tab.
     DATA(tab) = VALUE tt_zed_cd_test_tab( ( key_no_track = 'B' ) ( key_no_track = 'A' ) ).
     DATA(tab_with_indicator) = zcl_ed_change_doc_factory=>create( )->create_table_with_indicator(
        table_name = table_name original_table = REF #( tab ) indicator = 'X' sort = abap_true ).
     FIELD-SYMBOLS: <tab_with_indicator> TYPE table.
     ASSIGN tab_with_indicator->* TO <tab_with_indicator>.

     DATA(expected_tab) = VALUE tt_expected( ( key_no_track = 'A' zed_change_indicator_field = 'X' ) ( key_no_track = 'B' zed_change_indicator_field = 'X' ) ).

     cl_abap_unit_assert=>assert_equals( act = <tab_with_indicator> exp = expected_tab ).
   ENDMETHOD.
 ENDCLASS.

 "=================================================================
 "-----------------------------------------------------------------
 CLASS tcl_clear_force_tracking DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

   PRIVATE SECTION.
     TYPES:
         tt_zed_cd_test_tab TYPE STANDARD TABLE OF zed_cd_test_tab WITH EMPTY KEY.

     CLASS-METHODS:
       class_setup,
       class_teardown.

     METHODS:
       clear_force_after_change     FOR TESTING,
       clear_force_after_error FOR TESTING,
       check_force_is_cleared.

     CONSTANTS:
       c_objectclass TYPE cdobjectcl VALUE 'OBJECTCLASS'.

     CLASS-DATA:
       table_name      TYPE tabname  VALUE 'ZED_CD_TEST_TAB',
       single_inserted TYPE zed_cd_test_tab.

     DATA:
      cut          TYPE REF TO zif_ed_change_doc.
 ENDCLASS.

 CLASS tcl_clear_force_tracking IMPLEMENTATION.
   METHOD class_teardown.
     ROLLBACK WORK.
   ENDMETHOD.

   METHOD class_setup.
     single_inserted = VALUE #( key_no_track = 'IKNT' non_key_no_track = 'INE' ).
   ENDMETHOD.

   METHOD clear_force_after_change.
     cut = zcl_ed_change_doc_factory=>create( objectclass = c_objectclass ).
     cut->open( ).
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true after = REF #( single_inserted ) ).
     cut->close( ).

     check_force_is_cleared( ).
   ENDMETHOD.

   METHOD clear_force_after_error.
     cut = zcl_ed_change_doc_factory=>create( objectclass = c_objectclass ).
     cut->open( ).
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true after = REF #( single_inserted ) ).
     cut->close( ).

     TRY.
         cut->change_single( table_name = space force_cd_on_all_fields = abap_true after = REF #( single_inserted ) ).
         cl_abap_unit_assert=>fail( |Exception should be raised| ).
       CATCH zcx_ed_exception.
         check_force_is_cleared( ).
     ENDTRY.
   ENDMETHOD.

   METHOD check_force_is_cleared.
     "Should be loaded at this point
     FIELD-SYMBOLS: <tabinfo> TYPE table.
     ASSIGN ('(SAPLSCD0)TABINFO') TO <tabinfo>.

     DATA(found) = abap_false.
     LOOP AT <tabinfo> ASSIGNING FIELD-SYMBOL(<tabinfo_row>) WHERE ('tabname = table_name').
       found = abap_true.
       ASSIGN COMPONENT 'LOGFLAG' OF STRUCTURE <tabinfo_row> TO FIELD-SYMBOL(<logflag>).
       IF sy-subrc <> 0.
         cl_abap_unit_assert=>fail( |LOGFLAG assignment should not fail| ).
       ENDIF.
       IF <logflag> = 'F'.
         cl_abap_unit_assert=>fail( |No LOGFLAG field should have F assigned| ).
       ENDIF.
     ENDLOOP.
     IF found = abap_false.
       cl_abap_unit_assert=>fail( |Should have found table { table_name } in memory| ).
     ENDIF.
   ENDMETHOD.
 ENDCLASS.

 "=================================================================
 "-----------------------------------------------------------------
 CLASS tcl_fields_saving DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

   PRIVATE SECTION.
     TYPES:
         tt_zed_cd_test_tab TYPE STANDARD TABLE OF zed_cd_test_tab WITH EMPTY KEY.

     CLASS-METHODS:
       class_setup,
       class_teardown.

     METHODS:
       change_save_tracked_fields     FOR TESTING,
       change_save_non_tracked_fields FOR TESTING,
       ins_del_save_key_only           FOR TESTING,
       ins_del_save_non_initial        FOR TESTING,
       ins_del_save_all_fields         FOR TESTING,
       save_multi              FOR TESTING.

     CONSTANTS:
       c_objectclass TYPE cdobjectcl VALUE 'OBJECTCLASS'.

     CLASS-DATA:
       table_name             TYPE tabname  VALUE 'ZED_CD_TEST_TAB',
       single_deleted         TYPE zed_cd_test_tab,
       single_inserted        TYPE zed_cd_test_tab,
       single_before_modified TYPE zed_cd_test_tab,
       single_modified        TYPE zed_cd_test_tab,
       multi_deleted          TYPE tt_zed_cd_test_tab,
       multi_inserted         TYPE tt_zed_cd_test_tab,
       multi_before_modified  TYPE tt_zed_cd_test_tab,
       multi_modified         TYPE tt_zed_cd_test_tab.

     DATA:
      cut          TYPE REF TO zif_ed_change_doc.
 ENDCLASS.

 CLASS tcl_fields_saving IMPLEMENTATION.
   METHOD class_teardown.
     ROLLBACK WORK.
   ENDMETHOD.

   METHOD class_setup.
     single_deleted = VALUE #( mandt = sy-mandt key_no_track = 'DKNT' non_key_no_track = 'DNE' ).
     single_inserted = VALUE #( mandt = sy-mandt key_no_track = 'IKNT' non_key_no_track = 'INE' ).
     single_before_modified = VALUE #( mandt = sy-mandt key_track = 'BMKT' non_key_no_track = 'OLD' non_key_track = 'OLD' ).
     single_modified = VALUE #( mandt = sy-mandt key_track = 'MKT' non_key_no_track = 'NEW' non_key_track = 'NEW' ).

     multi_before_modified = VALUE #( mandt = sy-mandt non_key_track = '1' ( dec_with_sign = '12' ) ( dec_with_sign = '100' ) ( dec_with_sign = '-40' ) ).
     multi_modified = VALUE #( mandt = sy-mandt non_key_track = '2' ( dec_with_sign = '-40' ) ( dec_with_sign = '12' ) ( dec_with_sign = '200' ) ).
     multi_deleted = VALUE #( mandt = sy-mandt ( key_track = 'DEL_1' ) ).
     multi_inserted = VALUE #( mandt = sy-mandt ( key_track = 'INS_1' ) ).
   ENDMETHOD.

   METHOD change_save_tracked_fields.
     cut = zcl_ed_change_doc_factory=>create( objectclass = c_objectclass objectid = 'SAVE_TRACKED_FIELDS' ).
     cut->open( ).
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_false
       before = REF #( single_before_modified ) after = REF #( single_modified )
       save_fields_on_deletion = cut->c_save_mode-all save_fields_on_insertion = cut->c_save_mode-all ).
     DATA(change_nr) = cut->close( ).

     "Check result
     SELECT * FROM cdpos WHERE changenr = @change_nr INTO TABLE @DATA(cdpos).

     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ objectclas = c_objectclass objectid = 'SAVE_TRACKED_FIELDS'
        chngind = 'U' fname = 'NON_KEY_TRACK' value_old = 'OLD' value_new = 'NEW' ] ) ) ).
     cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( cdpos ) ).
   ENDMETHOD.

   METHOD change_save_non_tracked_fields.
     cut = zcl_ed_change_doc_factory=>create( objectclass = c_objectclass objectid = 'SAVE_NON_TRACKED_FIELDS' ).
     cut->open( ).
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true
       before = REF #( single_before_modified ) after = REF #( single_modified )
       save_fields_on_deletion = cut->c_save_mode-all save_fields_on_insertion = cut->c_save_mode-all ).
     DATA(change_nr) = cut->close( ).

     "Check result
     SELECT * FROM cdpos WHERE changenr = @change_nr INTO TABLE @DATA(cdpos).

     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'U' fname = 'NON_KEY_NO_TRACK' value_old = 'OLD' value_new = 'NEW' ] ) ) ).
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'U' fname = 'NON_KEY_TRACK' value_old = 'OLD' value_new = 'NEW' ] ) ) ).
     cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( cdpos ) ).
   ENDMETHOD.

   METHOD ins_del_save_key_only.
     cut = zcl_ed_change_doc_factory=>create( objectclass = c_objectclass objectid = 'SAVE_KEY_ONLY' ).
     cut->open( ).
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true before = REF #( single_deleted )
       save_fields_on_deletion = cut->c_save_mode-none save_fields_on_insertion = cut->c_save_mode-none ).
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true after = REF #( single_inserted )
      save_fields_on_deletion = cut->c_save_mode-none save_fields_on_insertion = cut->c_save_mode-none ).
     DATA(change_nr) = cut->close( ).

     "Check result
     SELECT * FROM cdpos WHERE changenr = @change_nr INTO TABLE @DATA(cdpos).

     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'I' fname = 'KEY' value_old = space value_new = space ] ) ) ).
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'D' fname = 'KEY' value_old = space value_new = space ] ) ) ).

     cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( cdpos ) ).
   ENDMETHOD.

   METHOD ins_del_save_non_initial.
     cut = zcl_ed_change_doc_factory=>create( objectclass = c_objectclass objectid = 'SAVE_NON_INITIAL' ).
     cut->open( ).
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true before = REF #( single_deleted )
      save_fields_on_deletion = cut->c_save_mode-non_initial save_fields_on_insertion = cut->c_save_mode-non_initial ).
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true after = REF #( single_inserted )
      save_fields_on_deletion = cut->c_save_mode-non_initial save_fields_on_insertion = cut->c_save_mode-non_initial ).
     DATA(change_nr) = cut->close( ).

     "Check result
     SELECT * FROM cdpos WHERE changenr = @change_nr INTO TABLE @DATA(cdpos).

     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'J' fname = 'NON_KEY_NO_TRACK' value_old = space value_new = 'INE' ] ) ) ).
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'E' fname = 'NON_KEY_NO_TRACK' value_old = 'DNE' value_new = space ] ) ) ).

     cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( cdpos ) ).
   ENDMETHOD.

   METHOD ins_del_save_all_fields.
     cut = zcl_ed_change_doc_factory=>create( objectclass = c_objectclass objectid = 'SAVE_ALL_FIELDS' ).
     cut->open( ).
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true before = REF #( single_deleted )
      save_fields_on_deletion = cut->c_save_mode-all save_fields_on_insertion = cut->c_save_mode-all ).
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true after = REF #( single_inserted )
      save_fields_on_deletion = cut->c_save_mode-all save_fields_on_insertion = cut->c_save_mode-all ).
     DATA(change_nr) = cut->close( ).

     "Check result
     SELECT * FROM cdpos WHERE changenr = @change_nr INTO TABLE @DATA(cdpos).

     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'J' fname = 'NON_KEY_NO_TRACK' value_old = space value_new = 'INE' ] ) ) ).
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'J' fname = 'NON_KEY_TRACK' value_old = space value_new = space ] ) ) ).
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'E' fname = 'NON_KEY_NO_TRACK' value_old = 'DNE' value_new = space ] ) ) ).
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'E' fname = 'NON_KEY_TRACK' value_old = space value_new = space ] ) ) ).

     cl_abap_unit_assert=>assert_equals( exp = 4 act = lines( cdpos ) ).
   ENDMETHOD.

   METHOD save_multi.
     cut = zcl_ed_change_doc_factory=>create( objectclass = c_objectclass objectid = 'SAVE_MULTI' ).
     cut->open( ).
     cut->change_multi( table_name = table_name
       before = cut->create_table_with_indicator( table_name = table_name original_table = REF #( multi_before_modified ) indicator = 'U' sort = abap_true )
       after = cut->create_table_with_indicator( table_name = table_name original_table = REF #( multi_modified ) indicator = 'U' sort = abap_true )
       save_fields_on_deletion = cut->c_save_mode-all save_fields_on_insertion = cut->c_save_mode-all ).

     cut->change_multi( table_name = table_name
        before = cut->create_table_with_indicator( table_name = table_name original_table = REF #( multi_deleted ) indicator = 'D' )
        save_fields_on_deletion = cut->c_save_mode-all save_fields_on_insertion = cut->c_save_mode-all ).

     cut->change_multi( table_name = table_name
        after = cut->create_table_with_indicator( table_name = table_name original_table = REF #( multi_inserted ) indicator = 'I' )
        save_fields_on_deletion = cut->c_save_mode-all save_fields_on_insertion = cut->c_save_mode-all ).
     DATA(change_nr) = cut->close( ).

     "Check result
     SELECT * FROM cdpos WHERE changenr = @change_nr INTO TABLE @DATA(cdpos).

     "4 from change
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'U' fname = 'NON_KEY_TRACK' value_old = '1' value_new = '2' ] ) ) ).
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'U' fname = 'NON_KEY_TRACK' value_old = '1' value_new = '2' ] ) ) ).
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'E' fname = 'NON_KEY_TRACK' value_old = '1' value_new = space ] ) ) ).
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'J' fname = 'NON_KEY_TRACK' value_old = space value_new = '2' ] ) ) ).
     "1 from delete
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'E' fname = 'NON_KEY_TRACK' value_old = space value_new = space ] ) ) ).
     "1 from insert
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'J' fname = 'NON_KEY_TRACK' value_old = space value_new = space ] ) ) ).

     cl_abap_unit_assert=>assert_equals( exp = 6 act = lines( cdpos ) ).
   ENDMETHOD.
 ENDCLASS.
