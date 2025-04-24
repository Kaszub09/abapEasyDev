"! <p class="shorttext synchronized">Sapscript texts</p>
"! <br/>TAGS: STXL; SAPSCRIPT;TEXT
CLASS zcl_ed_sapscript_text DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_text,
        object     TYPE tdobject,
        name       TYPE tdobname,
        id         TYPE tdid,
        lang       TYPE tdspras,
        text_lines TYPE STANDARD TABLE OF tline WITH EMPTY KEY,
        text       TYPE string,
      END OF t_text,
      tt_text         TYPE HASHED TABLE OF t_text WITH UNIQUE KEY object name id lang WITH NON-UNIQUE SORTED KEY sort_name COMPONENTS name,
      tt_object_range TYPE RANGE OF tdobject,
      tt_name_range   TYPE RANGE OF tdobname,
      tt_id_range     TYPE RANGE OF tdid,
      tt_lang_range   TYPE RANGE OF tdspras.

    INTERFACES:
      if_amdp_marker_hdb.

    METHODS:
    get_all_texts RETURNING VALUE(texts) TYPE tt_text,
    "! <p class="shorttext synchronized" lang="en">Fill <em>text_lines</em> and <em>text</em> in given table</p>
    fill_texts CHANGING texts TYPE tt_text,
    get_texts IMPORTING objects TYPE tt_object_range OPTIONAL names TYPE tt_name_range OPTIONAL
                        ids TYPE tt_id_range OPTIONAL langs TYPE tt_lang_range OPTIONAL
              RETURNING VALUE(texts) TYPE tt_text,
    "! <p class="shorttext synchronized" lang="en">Similar to <em>get_texts</em> but names can be passed in any table
    "! - to avoid problem of range being too big</p>
    "! @parameter names_table | <p class="shorttext synchronized" lang="en">any table with column specified in <em>names_col</em></p>
    "! @parameter names_col | <p class="shorttext synchronized" lang="en">name of column in <em>names_table</em> with names</p>
    get_texts_tab IMPORTING objects TYPE tt_object_range OPTIONAL names_table TYPE ANY TABLE names_col TYPE string
                            ids TYPE tt_id_range OPTIONAL langs TYPE tt_lang_range OPTIONAL
                  RETURNING VALUE(texts) TYPE tt_text.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_stxl_raw,
        clustr TYPE sybin2,
        clustd TYPE stxldummy2,
      END OF t_stxl_raw,
      tt_stxl_raw TYPE STANDARD TABLE OF t_stxl_raw WITH EMPTY KEY,
      BEGIN OF t_text_binary,
        object      TYPE tdobject,
        name        TYPE tdobname,
        id          TYPE tdid,
        lang        TYPE tdspras,
        counter     TYPE sybin1,
        size        TYPE sybin2,
        text_binary TYPE stxldummy2,
      END OF t_text_binary,
      tt_text_binary TYPE STANDARD TABLE OF t_text_binary WITH EMPTY KEY,
      BEGIN OF t_name,
        name TYPE tdobname,
      END OF t_name,
      tt_name TYPE STANDARD TABLE OF t_name WITH EMPTY KEY.

    METHODS:
      append_data IMPORTING existing_only TYPE abap_bool DEFAULT abap_false CHANGING texts_binary TYPE tt_text_binary texts TYPE tt_text,
      load_data_amdp IMPORTING VALUE(selection_filter) TYPE string VALUE(names_table) TYPE tt_name EXPORTING VALUE(texts_binary) TYPE tt_text_binary.
ENDCLASS.

CLASS zcl_ed_sapscript_text IMPLEMENTATION.
  METHOD fill_texts.
    IF lines( texts ) = 0.
      RETURN.
    ENDIF.

    DATA texts_binary TYPE tt_text_binary.

    SELECT tdobject AS object, tdname AS name, tdid AS id, tdspras AS lang, srtf2 AS counter, clustr AS size, clustd AS text_binary FROM stxl
    FOR ALL ENTRIES IN @texts
    WHERE relid = 'TX' AND tdobject = @texts-object AND tdname = @texts-name AND tdid = @texts-id AND tdspras = @texts-lang
    INTO TABLE @texts_binary.

    append_data( EXPORTING existing_only = abap_true CHANGING texts_binary = texts_binary texts = texts ).
  ENDMETHOD.

  METHOD get_all_texts.
    DATA texts_binary TYPE tt_text_binary.

    SELECT tdobject AS object, tdname AS name, tdid AS id, tdspras AS lang, srtf2 AS counter, clustr AS size, clustd AS text_binary FROM stxl
    INTO TABLE @texts_binary.

    append_data(  CHANGING texts_binary = texts_binary texts = texts ).
  ENDMETHOD.

  METHOD get_texts.
    DATA texts_binary TYPE tt_text_binary.

    SELECT tdobject AS object, tdname AS name, tdid AS id, tdspras AS lang, srtf2 AS counter, clustr AS size, clustd AS text_binary FROM stxl
    WHERE relid = 'TX' AND tdobject IN @objects AND tdname IN @names AND tdid IN @ids AND tdspras IN @langs
    INTO TABLE @texts_binary.

    append_data(  CHANGING texts_binary = texts_binary texts = texts ).
  ENDMETHOD.

  METHOD get_texts_tab.
    DATA(selection_filter) = cl_shdb_seltab=>combine_seltabs( it_named_seltabs = VALUE #(
    ( name = 'OBJECTS' dref = REF #( objects )  ) ( name = 'IDS' dref = REF #( ids )  ) ( name = 'LANGS' dref = REF #( langs )  ) )
    iv_client_field = 'MANDT' ).

    DATA names TYPE tt_name.
    LOOP AT names_table ASSIGNING FIELD-SYMBOL(<names_row>).
      ASSIGN COMPONENT names_col OF STRUCTURE <names_row> TO FIELD-SYMBOL(<name>).
      APPEND VALUE #( name = <name> ) TO names.
    ENDLOOP.

    load_data_amdp( EXPORTING selection_filter = selection_filter names_table = names IMPORTING texts_binary = DATA(texts_binary) ).
    append_data( CHANGING texts_binary = texts_binary texts = texts ).
  ENDMETHOD.

  METHOD load_data_amdp BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT USING stxl.
    stxl_filtered = APPLY_FILTER( stxl, :selection_filter );
    texts_binary = SELECT tdobject AS object, tdname AS name, tdid AS id, tdspras AS lang, srtf2 AS counter, clustr AS size, clustd AS text_binary
                    FROM :stxl_filtered
                    WHERE relid = 'TX' AND tdname in ( select * from :names_table );
  ENDMETHOD.

  METHOD append_data.
    "sort, in case some text has more than one line
    SORT texts_binary BY object name id lang counter ASCENDING.

    LOOP AT texts_binary REFERENCE INTO DATA(raw_group)
    GROUP BY ( object = raw_group->object name = raw_group->name id = raw_group->id lang = raw_group->lang ).
      "Get correct reference
      DATA(text_entry) = REF #( texts[ object = raw_group->object name = raw_group->name id = raw_group->id lang = raw_group->lang ] OPTIONAL ).
      IF NOT text_entry IS BOUND AND existing_only = abap_false.
        INSERT VALUE #( object = raw_group->object name = raw_group->name id = raw_group->id lang = raw_group->lang ) INTO TABLE texts REFERENCE INTO text_entry.
      ENDIF.

      IF text_entry IS NOT BOUND.
        CONTINUE.
      ENDIF.

      DATA(stxl_raw) = VALUE tt_stxl_raw( ).

      "Write binary to table
      LOOP AT GROUP raw_group REFERENCE INTO DATA(raw_text).
        APPEND VALUE #( clustr = raw_text->size clustd = raw_text->text_binary ) TO stxl_raw.
      ENDLOOP.

      "Import binary as SAPSCRIPT text lines
      IMPORT tline = text_entry->text_lines FROM INTERNAL TABLE stxl_raw.

      "Combine lines to string, ignore formatting
      LOOP AT text_entry->text_lines REFERENCE INTO DATA(text_line).
        text_entry->text = |{ text_entry->text }{ text_line->tdline }|.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
