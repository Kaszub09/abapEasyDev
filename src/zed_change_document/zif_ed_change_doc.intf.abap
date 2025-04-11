"! <p class="shorttext synchronized">Change document for any object. No need to create SCDO every time. Open-&gt;Change-&gt;Close.</p>
"! <br/>TAGS: change document
INTERFACE zif_ed_change_doc PUBLIC.
  CONSTANTS:
    "! Decide whether to save (and which ) fields on deletion/insertion
    BEGIN OF c_save_mode,
      none        TYPE i VALUE 0,
      non_initial TYPE i VALUE 1,
      all         TYPE i VALUE 2,
    END OF c_save_mode.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Must be called before any changes</p>
    "! @parameter objectclass | <p class="shorttext synchronized" lang="en">Taken from constructor parameter if not supplied</p>
    "! @parameter objectid | <p class="shorttext synchronized" lang="en">Taken from constructor parameter if not supplied</p>
    open IMPORTING objectclass TYPE cdobjectcl DEFAULT space
                   objectid    TYPE cdobjectv DEFAULT space
         RAISING   zcx_ed_exception,
    "! <p class="shorttext synchronized">CD must be opened first</p>
    "! @parameter table_name | <p class="shorttext synchronized">Must be valid DDIC table. Taken from constructor parameter if not supplied</p>
    "! @parameter force_cd_on_all_fields | <p class="shorttext synchronized">Create CD even on Data Elements without CD logflag</p>
    "! @parameter inserted | <p class="shorttext synchronized">Must be ref to structure of type <em>table_name</em></p>
    "! @parameter deleted | <p class="shorttext synchronized">Must be ref to structure of type <em>table_name</em></p>
    "! @parameter before_modified | <p class="shorttext synchronized">Must be ref to structure of type <em>table_name</em></p>
    "! @parameter modified | <p class="shorttext synchronized">Must be ref to structure of type <em>table_name</em></p>
    "! @parameter save_fields_on_deletion | <p class="shorttext synchronized">Check <em>c_save_mode</em></p>
    "! @parameter save_fields_on_insertion | <p class="shorttext synchronized">Check <em>c_save_mode</em></p>
    change_single IMPORTING table_name               TYPE tabname OPTIONAL
                            force_cd_on_all_fields   TYPE abap_bool DEFAULT abap_false
                            inserted                 TYPE REF TO data OPTIONAL
                            deleted                  TYPE REF TO data OPTIONAL
                            before_modified          TYPE REF TO data OPTIONAL
                            modified                 TYPE REF TO data OPTIONAL
                            save_fields_on_deletion  TYPE i DEFAULT c_save_mode-all
                            save_fields_on_insertion TYPE i DEFAULT c_save_mode-all
                  RAISING   zcx_ed_exception,
    "! <p class="shorttext synchronized">CD must be opened first. All tables must be sorted.</p>
    "! @parameter table_name | <p class="shorttext synchronized">Must be valid DDIC table. Taken from constructor parameter if not supplied</p>
    "! @parameter force_cd_on_all_fields | <p class="shorttext synchronized">Create CD even on Data Elements without CD logflag</p>
    "! @parameter inserted | <p class="shorttext synchronized">Must be ref to table <em>table_name</em> with <em>c</em> field</p>
    "! @parameter deleted | <p class="shorttext synchronized">Must be ref to table <em>table_name</em> with <em>c</em> field</p>
    "! @parameter before_modified | <p class="shorttext synchronized">Must be ref to table <em>table_name</em> with <em>c</em> field</p>
    "! @parameter modified | <p class="shorttext synchronized">Must be ref to table <em>table_name</em> with <em>c</em> field</p>
    "! @parameter save_fields_on_deletion | <p class="shorttext synchronized">Check <em>c_save_mode</em></p>
    "! @parameter save_fields_on_insertion | <p class="shorttext synchronized">Check <em>c_save_mode</em></p>
    change_multi IMPORTING table_name               TYPE tabname OPTIONAL
                           force_cd_on_all_fields   TYPE abap_bool DEFAULT abap_false
                           inserted                 TYPE REF TO data OPTIONAL
                           deleted                  TYPE REF TO data OPTIONAL
                           before_modified          TYPE REF TO data OPTIONAL
                           modified                 TYPE REF TO data OPTIONAL
                           save_fields_on_deletion  TYPE i DEFAULT c_save_mode-all
                           save_fields_on_insertion TYPE i DEFAULT c_save_mode-all
                 RAISING   zcx_ed_exception,
    "! <p class="shorttext synchronized">CD must be opened first</p>
    "! @parameter objectclass | <p class="shorttext synchronized" lang="en">Taken from constructor parameter if not supplied</p>
    "! @parameter objectid | <p class="shorttext synchronized" lang="en">Taken from constructor parameter if not supplied</p>
    "! @parameter object_change_indicator | <p class="shorttext synchronized">For header entry only</p>
    "! @parameter skip_no_pos_ins_error | <p class="shorttext synchronized">Don't raise <em>zcx_cd_no_position_inserted</em> if no changes detected</p>
    close IMPORTING objectclass             TYPE cdobjectcl DEFAULT space
                    objectid                TYPE cdobjectv DEFAULT space
                    date_of_change          TYPE d DEFAULT sy-datum
                    tcode                   TYPE syst_tcode DEFAULT sy-tcode
                    time_of_change          TYPE t DEFAULT sy-uzeit
                    username                TYPE syst_uname DEFAULT sy-uname
                    object_change_indicator TYPE cdchngindh DEFAULT 'U'
                    skip_no_pos_ins_error   TYPE abap_bool DEFAULT abap_true
          EXPORTING changenumber            TYPE cdchangenr
          RAISING   zcx_ed_exception
                    zcx_ed_change_doc_no_pos_ins,
    "! <p class="shorttext synchronized">Converts table to be used with <em>change_multi</em></p>
    "! @parameter table_name | <p class="shorttext synchronized">Must be valid DDIC table. Taken from constructor parameter if not supplied</p>
    "! @parameter original_table | <p class="shorttext synchronized">Must be ref to table <em>table_name</em></p>
    "! @parameter indicator | <p class="shorttext synchronized">Value of added c field (D, I, U or space)</p>
    "! @parameter sort | <p class="shorttext synchronized">Sort table by key fields</p>
    "! @parameter table_with_indicator | <p class="shorttext synchronized">Ref to table <em>table_name</em> with added <em>c</em> field</p>
    create_table_with_indicator IMPORTING table_name                  TYPE tabname OPTIONAL
                                          original_table              TYPE REF TO data
                                          indicator                   TYPE c DEFAULT space
                                          sort                        TYPE abap_bool DEFAULT abap_false
                                RETURNING VALUE(table_with_indicator) TYPE REF TO data.
ENDINTERFACE.
