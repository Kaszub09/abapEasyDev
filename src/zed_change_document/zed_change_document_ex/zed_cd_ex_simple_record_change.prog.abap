*&---------------------------------------------------------------------*
*& Report zed_change_doc_simple
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_cd_ex_simple_record_change.

"Two structures before/after changes
DATA(old) = VALUE tadir( object = 'R3TR' obj_name = 'ZED_TEST_REP' author = 'OLD_AUTHOR' ).
DATA(new) = VALUE tadir( object = 'R3TR' obj_name = 'ZED_TEST_REP' author = 'NEW_AUTHOR' ).

"Create cd
DATA(cd) = zcl_ed_change_doc_factory=>create( objectclass = 'TADIR_SIMPLE' table_name = 'TADIR' objectid = 'OBJECT A' ).
"Objectclass and objectid could be overwritten, but are taken from factory method if not supplied
cd->open( ).
"Force cd logging since 'author' field isn't normally tracked for changes
cd->change_single( before = REF #( old ) after = REF #( new ) force_cd_on_all_fields = abap_true ).
cd->close( ).

"Later on changes can be viewed via transaction RSSCD100
CALL TRANSACTION 'RSSCD100' WITH AUTHORITY-CHECK.
