"! <p class="shorttext synchronized" lang="en">Selection</p>
"! <br/>TAGS: selection; free selections
INTERFACE zif_ed_selection PUBLIC.
  TYPES:
    BEGIN OF t_field_selection,
      field       TYPE fieldname,
      is_selected TYPE abap_bool,
      range       TYPE RANGE OF rsdslow,
    END OF t_field_selection,
    tt_field_selection TYPE STANDARD TABLE OF t_field_selection WITH EMPTY KEY
        WITH UNIQUE SORTED KEY field COMPONENTS field,
    BEGIN OF t_table_selection,
      table  TYPE tabname,
      fields TYPE tt_field_selection,
    END OF t_table_selection,
    tt_table_selection TYPE STANDARD TABLE OF t_table_selection WITH EMPTY KEY
        WITH UNIQUE SORTED KEY table COMPONENTS table,

    BEGIN OF t_aliases,
      table TYPE tabname,
      alias TYPE string,
    END OF t_aliases,
    tt_aliases TYPE SORTED TABLE OF t_aliases WITH UNIQUE KEY table.

  METHODS:
    "! <p class="shorttext synchronized">Display in popup or fullscreen.</p>
    "! @parameter user_confirmed | <p class="shorttext synchronized">User clicked confirmed, otherwise cancelled selection</p>
    display IMPORTING start_column          TYPE i DEFAULT 0
                      end_column            TYPE i DEFAULT 128
                      start_line            TYPE i DEFAULT 0
                      end_line              TYPE i DEFAULT 24
                      handler               TYPE REF TO zif_ed_selection_handler OPTIONAL
            RETURNING VALUE(user_confirmed) TYPE abap_bool
            RAISING   zcx_ed_exception,
    "! <p class="shorttext synchronized" lang="en">Get all selections as single where clause with aliases if supplied.</p>
    "! @parameter add_aliases | <p class="shorttext synchronized">Adds alias (or table name if alias is not found ) to the field.</p>
    get_as_where_clause IMPORTING add_aliases TYPE abap_bool DEFAULT abap_true RETURNING VALUE(where) TYPE string.

  DATA:
    "! Updated only after user confirms.
    selection TYPE tt_table_selection READ-ONLY,
    "! Tables names in where clause will be substituted with aliases.
    aliases   TYPE tt_aliases.
ENDINTERFACE.
