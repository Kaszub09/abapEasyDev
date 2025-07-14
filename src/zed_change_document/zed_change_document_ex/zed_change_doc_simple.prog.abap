*&---------------------------------------------------------------------*
*& Report zed_change_doc_simple
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_change_doc_simple.

"Two structures before/after changes
DATA(old) = VALUE tadir( object = 'R3TR' obj_name = 'ZED_TEST_REP' author = 'OLD_AUTHOR' ).
DATA(new) = VALUE tadir( object = 'R3TR' obj_name = 'ZED_TEST_REP' author = 'NEW_AUTHOR' ).

"Create cd
DATA(cd) = zcl_ed_change_doc_factory=>create( objectclass = 'TADIR_SIMPLE' table_name = 'TADIR' objectid = 'OBJECT A' ).
"Objectclass and objectid could be overwritten, but are taken from factory method if left as initial
cd->open( ).
"Force cd logging since 'author' field isn't normally tracked for changes
cd->change_single( before_modified = REF #( old ) modified = REF #( new ) force_cd_on_all_fields = abap_true ).
cd->close( ).

"Later on can be viewed via transaction RSSCD100
CALL TRANSACTION 'RSSCD100' WITH AUTHORITY-CHECK.
