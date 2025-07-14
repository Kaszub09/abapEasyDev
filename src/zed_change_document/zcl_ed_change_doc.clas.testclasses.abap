
 CLASS tcl_create_tab DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

   PRIVATE SECTION.
     TYPES:
      tt_zed_cd_test_tab TYPE STANDARD TABLE OF zed_cd_test_tab WITH EMPTY KEY.

     METHODS:
       create_tab FOR TESTING.

     CLASS-DATA:
       table_name             TYPE tabname  VALUE 'ZED_CD_TEST_TAB'.

     DATA:
      cut          TYPE REF TO zif_ed_change_doc.
 ENDCLASS.

 CLASS tcl_create_tab IMPLEMENTATION.
   METHOD create_tab.
     cut = zcl_ed_change_doc_factory=>create( ).
     DATA(tab) = VALUE tt_zed_cd_test_tab( ( key_no_track = 'B' ) ( key_no_track = 'A' ) ).
     DATA(tab_with_indicator) = cut->create_table_with_indicator( table_name = table_name original_table = REF #( tab )
        indicator = 'X' sort = abap_true ).

     FIELD-SYMBOLS: <tab> TYPE table.
     ASSIGN tab_with_indicator->* TO <tab>.

     IF lines( <tab> ) <> 2.
       cl_abap_unit_assert=>fail( |Incorrect  numbeer of lines| ).
     ENDIF.

     LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<row>).
       ASSIGN COMPONENT 'KEY_NO_TRACK' OF STRUCTURE <row> TO FIELD-SYMBOL(<key_no_track>).
       CASE sy-tabix.
         WHEN 1.
           IF <key_no_track> <> 'A'.
             cl_abap_unit_assert=>fail( |Table not sorted| ).
           ENDIF.

         WHEN 2.
           IF <key_no_track> <> 'B'.
             cl_abap_unit_assert=>fail( |Table not sorted| ).
           ENDIF.

         WHEN OTHERS.
           cl_abap_unit_assert=>fail( |There should be 2 rows| ).
       ENDCASE.

       ASSIGN COMPONENT 7 OF STRUCTURE <row> TO FIELD-SYMBOL(<indicator>).
       IF <indicator> <> 'X'.
         cl_abap_unit_assert=>fail( |Indicator should be X| ).
       ENDIF.
     ENDLOOP.
   ENDMETHOD.
 ENDCLASS.

 "=================================================================
 "=================================================================
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
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true inserted = REF #( single_inserted ) ).
     cut->close( ).

     check_force_is_cleared( ).
   ENDMETHOD.

   METHOD clear_force_after_error.
     cut = zcl_ed_change_doc_factory=>create( objectclass = c_objectclass ).
     cut->open( ).
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true inserted = REF #( single_inserted ) ).
     cut->close( ).

     TRY.
         cut->change_single( table_name = space force_cd_on_all_fields = abap_true inserted = REF #( single_inserted ) ).
         cl_abap_unit_assert=>fail( |Exception should be raised| ).
       CATCH zcx_ed_exception INTO DATA(cx). " TODO: variable is assigned but never used (ABAP cleaner)
         check_force_is_cleared( ).
     ENDTRY.
   ENDMETHOD.

   METHOD check_force_is_cleared.
     "Should be loaded at this point
     FIELD-SYMBOLS: <tabinfo> TYPE table.

     ASSIGN ('(SAPLSCD0)TABINFO') TO <tabinfo>.

     DATA(found) = abap_false.
     LOOP AT <tabinfo> ASSIGNING FIELD-SYMBOL(<tabinfo_row>) WHERE ('tabname = ''ZED_CD_TEST_TAB''').
       found = abap_true.
       ASSIGN COMPONENT 'LOGFLAG' OF STRUCTURE <tabinfo_row> TO FIELD-SYMBOL(<logflag>).
       IF sy-subrc <> 0.
         cl_abap_unit_assert=>fail( |Assignment should not fail| ).
       ENDIF.
       IF <logflag> = 'F'.
         cl_abap_unit_assert=>fail( |No field should have F assigned| ).
       ENDIF.
     ENDLOOP.
     IF found = abap_false.
       cl_abap_unit_assert=>fail( |Should have table ZED_CD_TEST_TAB in memory| ).
     ENDIF.
   ENDMETHOD.
 ENDCLASS.

 "=================================================================
 "=================================================================
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
     single_deleted = VALUE #( key_no_track = 'DKNT' non_key_no_track = 'DNE' ).
     single_inserted = VALUE #( key_no_track = 'IKNT' non_key_no_track = 'INE' ).
     single_before_modified = VALUE #( key_track = 'BMKT' non_key_no_track = 'OLD' non_key_track = 'OLD' ).
     single_modified = VALUE #( key_track = 'MKT' non_key_no_track = 'NEW' non_key_track = 'NEW' ).

     multi_before_modified = VALUE #( non_key_track = '1' ( dec_with_sign = '12' ) ( dec_with_sign = '-12' ) ( dec_with_sign = '-40' ) ).
     multi_modified = VALUE #( non_key_track = '2' ( dec_with_sign = '-40' ) ( dec_with_sign = '12' ) ( dec_with_sign = '-12' ) ).
     multi_deleted = VALUE #( ( key_track = 'DEL_1' ) ).
     multi_inserted = VALUE #( ( key_track = 'INS_1' ) ).
   ENDMETHOD.

   METHOD change_save_tracked_fields.
     cut = zcl_ed_change_doc_factory=>create( objectclass = c_objectclass objectid = 'SAVE_TRACKED_FIELDS' ).
     cut->open( ).
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_false
       before_modified = REF #( single_before_modified ) modified = REF #( single_modified )
       save_fields_on_deletion = cut->c_save_mode-all save_fields_on_insertion = cut->c_save_mode-all ).
     DATA(change_nr) = cut->close( ).

     "Check result
     SELECT * FROM cdpos WHERE changenr = @change_nr AND objectclas = @c_objectclass AND objectid = 'SAVE_TRACKED_FIELDS' INTO TABLE @DATA(cdpos).

     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'U' fname = 'NON_KEY_TRACK' value_old = 'OLD' value_new = 'NEW' ] ) ) ).
     cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( cdpos ) ).
   ENDMETHOD.

   METHOD change_save_non_tracked_fields.
     cut = zcl_ed_change_doc_factory=>create( objectclass = c_objectclass objectid = 'SAVE_NON_TRACKED_FIELDS' ).
     cut->open( ).
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true
       before_modified = REF #( single_before_modified ) modified = REF #( single_modified )
       save_fields_on_deletion = cut->c_save_mode-all save_fields_on_insertion = cut->c_save_mode-all ).
     DATA(change_nr) = cut->close( ).

     "Check result
     SELECT * FROM cdpos WHERE changenr = @change_nr AND objectclas = @c_objectclass AND objectid = 'SAVE_NON_TRACKED_FIELDS' INTO TABLE @DATA(cdpos).

     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'U' fname = 'NON_KEY_NO_TRACK' value_old = 'OLD' value_new = 'NEW' ] ) ) ).
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'U' fname = 'NON_KEY_TRACK' value_old = 'OLD' value_new = 'NEW' ] ) ) ).
     cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( cdpos ) ).
   ENDMETHOD.

   METHOD ins_del_save_key_only.
     cut = zcl_ed_change_doc_factory=>create( objectclass = c_objectclass objectid = 'SAVE_KEY_ONLY' ).
     cut->open( ).
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true
       inserted = REF #( single_inserted ) deleted = REF #( single_deleted )
       save_fields_on_deletion = cut->c_save_mode-none save_fields_on_insertion = cut->c_save_mode-none ).
     DATA(change_nr) = cut->close( ).

     "Check result
     SELECT * FROM cdpos WHERE changenr = @change_nr AND objectclas = @c_objectclass AND objectid = 'SAVE_KEY_ONLY'
         AND ( ( tabkey LIKE '%DKNT%' AND chngind = 'D' ) OR ( tabkey LIKE '%IKNT%' AND chngind = 'I' ) )
       INTO TABLE @DATA(cdpos).

     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'I' fname = 'KEY' ] ) ) ).
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'D' fname = 'KEY' ] ) ) ).

     cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( cdpos ) ).
   ENDMETHOD.

   METHOD ins_del_save_non_initial.
     cut = zcl_ed_change_doc_factory=>create( objectclass = c_objectclass objectid = 'SAVE_NON_INITIAL' ).
     cut->open( ).
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true
       inserted = REF #( single_inserted ) deleted = REF #( single_deleted )
       save_fields_on_deletion = cut->c_save_mode-non_initial save_fields_on_insertion = cut->c_save_mode-non_initial ).
     DATA(change_nr) = cut->close( ).

     "Check result
     SELECT * FROM cdpos
       WHERE changenr = @change_nr AND objectclas = @c_objectclass AND objectid = 'SAVE_NON_INITIAL'
         AND ( ( tabkey LIKE '%DKNT%' AND chngind = 'E' ) OR ( tabkey LIKE '%IKNT%' AND chngind = 'J' ) )
       INTO TABLE @DATA(cdpos).

     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'J' fname = 'NON_KEY_NO_TRACK' ] ) ) ).
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'E' fname = 'NON_KEY_NO_TRACK' ] ) ) ).

     cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( cdpos ) ).
   ENDMETHOD.

   METHOD ins_del_save_all_fields.
     cut = zcl_ed_change_doc_factory=>create( objectclass = c_objectclass objectid = 'SAVE_ALL_FIELDS' ).
     cut->open( ).
     cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true
       inserted = REF #( single_inserted ) deleted = REF #( single_deleted )
       save_fields_on_deletion = cut->c_save_mode-all save_fields_on_insertion = cut->c_save_mode-all ).
     DATA(change_nr) = cut->close( ).

     "Check result
     SELECT * FROM cdpos
       WHERE changenr = @change_nr AND objectclas = @c_objectclass AND objectid = 'SAVE_ALL_FIELDS'
         AND ( ( tabkey LIKE '%DKNT%' AND chngind = 'E' ) OR ( tabkey LIKE '%IKNT%' AND chngind = 'J' ) )
       INTO TABLE @DATA(cdpos).

     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'J' fname = 'NON_KEY_TRACK' ] ) ) ).
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'J' fname = 'NON_KEY_TRACK' ] ) ) ).
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'E' fname = 'NON_KEY_NO_TRACK' ] ) ) ).
     cl_abap_unit_assert=>assert_true( xsdbool( line_exists( cdpos[ chngind = 'E' fname = 'NON_KEY_NO_TRACK' ] ) ) ).

     cl_abap_unit_assert=>assert_equals( exp = 4 act = lines( cdpos ) ).
   ENDMETHOD.

   METHOD save_multi.
     cut = zcl_ed_change_doc_factory=>create( objectclass = c_objectclass objectid = 'SAVE_MULTI' ).
     cut->open( ).
     cut->change_multi( table_name = table_name
       before_modified = cut->create_table_with_indicator( table_name = table_name original_table = REF #( multi_before_modified ) indicator = 'U' sort = abap_true )
       modified = cut->create_table_with_indicator( table_name = table_name original_table = REF #( multi_modified ) indicator = 'U' sort = abap_true )
       deleted = cut->create_table_with_indicator( table_name = table_name original_table = REF #( multi_deleted ) indicator = 'D' )
       inserted = cut->create_table_with_indicator( table_name = table_name original_table = REF #( multi_inserted ) indicator = 'I' )
       save_fields_on_deletion = cut->c_save_mode-all save_fields_on_insertion = cut->c_save_mode-all ).
     DATA(change_nr) = cut->close( ).

     "Check result
     SELECT * FROM cdpos
       WHERE changenr = @change_nr AND objectclas = @c_objectclass AND objectid = 'SAVE_MULTI' AND tabkey <> @space
       INTO TABLE @DATA(cdpos).

     cl_abap_unit_assert=>assert_equals( exp = 5 act = lines( cdpos ) ).
   ENDMETHOD.
 ENDCLASS.
