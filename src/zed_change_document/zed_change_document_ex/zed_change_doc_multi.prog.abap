*&---------------------------------------------------------------------*
*& Report zed_change_doc_multi
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_change_doc_multi.
TYPES: tt_tadir TYPE STANDARD TABLE OF tadir WITH EMPTY KEY.

DATA(cd) = zcl_ed_change_doc_factory=>create( objectclass = 'TADIR_TAB' table_name = 'TADIR' ).

"-----------------------------------------------------------------
"INSERTED
DATA(inserted) = VALUE tt_tadir(
    ( object = 'R3TR' obj_name = 'ZED_CD_INSERTED1' author = 'INSERTED' )
    ( object = 'R3TR' obj_name = 'ZED_CD_INSERTED2' author = 'INSERTED2' devclass = 'PACKAGE')
).

"Each object should have it's own ID
cd->open( objectid = 'OBJECT 1' ).
"Save non-inital fields only
cd->change_multi( inserted = cd->create_table_with_indicator( REF #( inserted ) )
    save_fields_on_insertion = zif_ed_change_doc=>c_save_mode-non_initial
    force_cd_on_all_fields = abap_true ).
cd->close( objectid = 'OBJECT 1' ).


"-----------------------------------------------------------------
"CHANGED
DATA(old) = VALUE tt_tadir(
    ( object = 'R3TR' obj_name = 'ZED_CD_CHANGED2' author = 'NOT_CHANGED' )
    ( object = 'R3TR' obj_name = 'ZED_CD_CHANGED1' author = 'BEFORE_CHANGE' )
    ( object = 'R3TR' obj_name = 'ZED_CD_DELETED' author = 'DELETED' )
).
DATA(new) = VALUE tt_tadir(
    ( object = 'R3TR' obj_name = 'ZED_CD_CHANGED1' author = 'CHANGED')
    ( object = 'R3TR' obj_name = 'ZED_CD_CHANGED2' author = 'NOT_CHANGED' )
).

"Each object should have it's own ID
cd->open( objectid = 'OBJECT 2' ).
"Force cd logging since 'author' field isn't normally tracked for changes
"Sort tables if they aren't
"Should detect by keys what was changed and what not
cd->change_multi( before_modified = cd->create_table_with_indicator( original_table = REF #( old ) sort = abap_true )
    modified = cd->create_table_with_indicator( original_table = REF #( new ) )
    force_cd_on_all_fields = abap_true
    save_fields_on_deletion = cd->c_save_mode-non_initial ).
cd->close( objectid = 'OBJECT 2' ).
