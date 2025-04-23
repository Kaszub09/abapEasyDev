CLASS zcl_ed_change_doc DEFINITION PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS zcl_ed_change_doc_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_change_doc.

    CLASS-METHODS:
      force_cd_if_needed CHANGING tabinfo TYPE ANY TABLE.

    METHODS:
      constructor IMPORTING objectclass TYPE cdobjectcl objectid TYPE cdobjectv table_name TYPE tabname OPTIONAL.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_save_info,
        docu_delete    TYPE abap_bool,
        docu_insert    TYPE abap_bool,
        docu_delete_if TYPE abap_bool,
        docu_insert_if TYPE abap_bool,
      END OF t_save_info.

    METHODS:
      get_save_info IMPORTING save_fields_on_deletion TYPE i save_fields_on_insertion TYPE i RETURNING VALUE(info) TYPE t_save_info.

    DATA:
      force_cd            TYPE REF TO lcl_force_cd_marker,
      table_descr_manager TYPE REF TO lcl_table_descr_manager,
      objectclass         TYPE cdobjectcl,
      objectid            TYPE cdobjectv,
      table_name          TYPE tabname.
ENDCLASS.

CLASS zcl_ed_change_doc IMPLEMENTATION.
  METHOD constructor.
    me->objectclass = objectclass.
    me->objectid = objectid.
    me->table_name = table_name.
    force_cd = NEW #( ).
    table_descr_manager = NEW #( ).
  ENDMETHOD.

  METHOD zif_ed_change_doc~open.
    CALL FUNCTION 'CHANGEDOCUMENT_OPEN'
      EXPORTING
        objectclass      = COND #( WHEN objectclass IS INITIAL THEN me->objectclass ELSE objectclass )
        objectid         = COND #( WHEN objectid IS INITIAL THEN me->objectid ELSE objectid )
      EXCEPTIONS
        sequence_invalid = 1                " No CLOSE was called for last object
        OTHERS           = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CHANGEDOCUMENT_OPEN subrc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_change_doc~close.
    CALL FUNCTION 'CHANGEDOCUMENT_CLOSE'
      EXPORTING
        date_of_change          = date_of_change
        objectclass             = COND #( WHEN objectclass IS INITIAL THEN me->objectclass ELSE objectclass )
        objectid                = COND #( WHEN objectid IS INITIAL THEN me->objectid ELSE objectid )
        tcode                   = tcode
        time_of_change          = time_of_change
        username                = username
        object_change_indicator = object_change_indicator
      IMPORTING
        changenumber            = changenumber
      EXCEPTIONS
        header_insert_failed    = 1                " SQL error during INSET of header
        no_position_inserted    = 2                " No items were entered
        object_invalid          = 3                " OPEN was called with other object
        open_missing            = 4                " No OPEN was performed
        position_insert_failed  = 5                " G/L account number
        OTHERS                  = 6.
    IF sy-subrc = 2 AND skip_no_pos_ins_error = abap_false.
      RAISE EXCEPTION TYPE zcx_ed_change_doc_no_pos_ins.
    ELSEIF  sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CHANGEDOCUMENT_CLOSE subrc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_change_doc~change_multi.
    FIELD-SYMBOLS:
      <inserted>        TYPE table,
      <deleted>         TYPE table,
      <before_modified> TYPE table,
      <modified>        TYPE table,
      <empty>           TYPE table.

    TRY.
        DATA(tablename) = COND tabname( WHEN table_name IS INITIAL THEN me->table_name ELSE table_name ).
        force_cd->set_force_cd( force_marker = force_cd_on_all_fields table_name = tablename ).
        DATA(save_info) = get_save_info( save_fields_on_deletion = save_fields_on_deletion save_fields_on_insertion = save_fields_on_insertion ).

        "Empty since you always must pass both tables
        DATA(empty_table) = table_descr_manager->create_table_with_indicator( table_name = tablename
            original_table = table_descr_manager->create_empty_table( tablename ) ).
        ASSIGN empty_table->* TO <empty>.

        "NEW ENTRY
        IF inserted IS BOUND.
          ASSIGN inserted->* TO <inserted>.
          IF lines( <inserted> ) > 0.
            CALL FUNCTION 'CHANGEDOCUMENT_MULTIPLE_CASE2'
              EXPORTING
                change_indicator       = 'I'
                tablename              = tablename
                table_old              = <empty>
                table_new              = <inserted>
                docu_insert            = save_info-docu_insert
                docu_insert_if         = save_info-docu_insert_if
              EXCEPTIONS
                nametab_error          = 1
                open_missing           = 2
                position_insert_failed = 3
                OTHERS                 = 4.

            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CHANGEDOCUMENT_MULTIPLE_CASE2 subrc={ sy-subrc } - Insert|.
            ENDIF.
          ENDIF.
        ENDIF.

        "DELETED ENTRY
        IF deleted IS BOUND.
          ASSIGN deleted->* TO <deleted>.
          IF lines( <deleted> ) > 0.
            CALL FUNCTION 'CHANGEDOCUMENT_MULTIPLE_CASE2'
              EXPORTING
                change_indicator       = 'D'
                tablename              = tablename
                table_old              = <deleted>
                table_new              = <empty>
                docu_delete            = save_info-docu_delete
                docu_delete_if         = save_info-docu_delete_if
              EXCEPTIONS
                nametab_error          = 1
                open_missing           = 2
                position_insert_failed = 3
                OTHERS                 = 4.

            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CHANGEDOCUMENT_MULTIPLE_CASE2 subrc={ sy-subrc } - Delete|.
            ENDIF.
          ENDIF.
        ENDIF.

        "MODIFIED ENTRY
        IF modified IS BOUND AND before_modified IS BOUND.
          ASSIGN modified->* TO <modified>.
          ASSIGN before_modified->* TO <before_modified>.
          IF lines( <modified> ) > 0.
            CALL FUNCTION 'CHANGEDOCUMENT_MULTIPLE_CASE2'
              EXPORTING
                change_indicator       = 'U'
                tablename              = tablename
                table_old              = <before_modified>
                table_new              = <modified>
                docu_delete            = save_info-docu_delete
                docu_delete_if         = save_info-docu_delete_if
                docu_insert            = save_info-docu_insert
                docu_insert_if         = save_info-docu_insert_if
              EXCEPTIONS
                nametab_error          = 1
                open_missing           = 2
                position_insert_failed = 3
                OTHERS                 = 4.

            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CHANGEDOCUMENT_MULTIPLE_CASE2 subrc={ sy-subrc } - Modified|.
            ENDIF.
          ENDIF.
        ENDIF.
        force_cd->clear_force_cd( force_marker = force_cd_on_all_fields table_name = tablename  ).

      CATCH cx_root INTO DATA(cx_root).
        "^Try to at least restore CD function-group if messed with earlier
        force_cd->clear_force_cd( force_marker = force_cd_on_all_fields table_name = tablename  ).
        RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = cx_root->get_longtext( ).

    ENDTRY.
  ENDMETHOD.

  METHOD zif_ed_change_doc~change_single.
    TRY.
        DATA(tablename) = COND tabname( WHEN table_name IS INITIAL THEN me->table_name ELSE table_name ).
        force_cd->set_force_cd( force_marker = force_cd_on_all_fields table_name = tablename  ).
        DATA(save_info) = get_save_info( save_fields_on_deletion = save_fields_on_deletion save_fields_on_insertion = save_fields_on_insertion ).

        "NEW ENTRY
        IF inserted IS BOUND.
          ASSIGN inserted->* TO FIELD-SYMBOL(<inserted>).

          CALL FUNCTION 'CHANGEDOCUMENT_SINGLE_CASE'
            EXPORTING
              change_indicator       = 'I'
              tablename              = tablename
              workarea_new           = <inserted>
              docu_insert            = save_info-docu_insert
              docu_insert_if         = save_info-docu_insert_if
            EXCEPTIONS
              nametab_error          = 1                " Error when calling NAMETAB_GET
              open_missing           = 2                " No OPEN was performed
              position_insert_failed = 3                " SQL error occurred during insert item
              OTHERS                 = 4.

          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CHANGEDOCUMENT_SINGLE_CASE subrc={ sy-subrc } - Insert|.
          ENDIF.

        ENDIF.

        "DELETED ENTRY
        IF deleted IS BOUND.
          ASSIGN deleted->* TO FIELD-SYMBOL(<deleted>).

          CALL FUNCTION 'CHANGEDOCUMENT_SINGLE_CASE'
            EXPORTING
              change_indicator       = 'D'
              tablename              = tablename
              workarea_old           = <deleted>
              docu_insert            = save_info-docu_insert
              docu_insert_if         = save_info-docu_insert_if
            EXCEPTIONS
              nametab_error          = 1                " Error when calling NAMETAB_GET
              open_missing           = 2                " No OPEN was performed
              position_insert_failed = 3                " SQL error occurred during insert item
              OTHERS                 = 4.

          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CHANGEDOCUMENT_SINGLE_CASE subrc={ sy-subrc } - Delete|.
          ENDIF.

        ENDIF.

        "MODIFIED ENTRY
        IF modified IS BOUND AND before_modified IS BOUND.
          ASSIGN modified->* TO FIELD-SYMBOL(<modified>).
          ASSIGN before_modified->* TO FIELD-SYMBOL(<before_modified>).

          CALL FUNCTION 'CHANGEDOCUMENT_SINGLE_CASE'
            EXPORTING
              change_indicator       = 'U'
              tablename              = tablename
              workarea_old           = <before_modified>
              workarea_new           = <modified>
              docu_delete            = save_info-docu_delete
              docu_delete_if         = save_info-docu_delete_if
              docu_insert            = save_info-docu_insert
              docu_insert_if         = save_info-docu_insert_if
            EXCEPTIONS
              nametab_error          = 1                " Error when calling NAMETAB_GET
              open_missing           = 2                " No OPEN was performed
              position_insert_failed = 3                " SQL error occurred during insert item
              OTHERS                 = 4.

          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CHANGEDOCUMENT_SINGLE_CASE subrc={ sy-subrc } - Modified|.
          ENDIF.
        ENDIF.

        force_cd->clear_force_cd( force_marker = force_cd_on_all_fields table_name = tablename ).

      CATCH cx_root INTO DATA(cx_root).
        "^Try to at least restore CD function-group if messed with earlier
        force_cd->clear_force_cd( force_marker = force_cd_on_all_fields table_name = tablename ).
        RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = cx_root->get_longtext( ).

    ENDTRY.
  ENDMETHOD.

  METHOD zif_ed_change_doc~create_table_with_indicator.
    DATA(tablename) = COND tabname( WHEN table_name IS INITIAL THEN me->table_name ELSE table_name ).
    table_with_indicator = table_descr_manager->create_table_with_indicator( table_name = tablename
       original_table = original_table indicator = indicator sort = sort ).
  ENDMETHOD.

  METHOD get_save_info.
    info-docu_delete = xsdbool( save_fields_on_deletion <> zif_ed_change_doc~c_save_mode-none ).
    info-docu_delete_if = xsdbool( save_fields_on_deletion <> zif_ed_change_doc~c_save_mode-all ).
    info-docu_insert = xsdbool( save_fields_on_insertion <> zif_ed_change_doc~c_save_mode-none ).
    info-docu_insert_if = xsdbool( save_fields_on_insertion <> zif_ed_change_doc~c_save_mode-all ).
  ENDMETHOD.

  METHOD force_cd_if_needed.
    lcl_force_cd_marker=>force_cd_if_needed( CHANGING tabinfo = tabinfo ).
  ENDMETHOD.
ENDCLASS.


