CLASS lcl_force_cd_marker DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    CONSTANTS:
      c_memory_id TYPE c LENGTH 40 VALUE 'ZCL_ED_CHANGE_DOC-FORCE_CD_MARKER'.

    CLASS-METHODS:
      force_cd_if_needed CHANGING tabinfo TYPE ANY TABLE.

    METHODS:
      set_force_cd IMPORTING force_marker TYPE abap_bool table_name TYPE tabname,
      clear_force_cd IMPORTING force_marker TYPE abap_bool table_name TYPE tabname.
ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
"=================================================================

CLASS lcl_table_descr_manager DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      create_table_with_indicator IMPORTING table_name TYPE tabname original_table TYPE REF TO data
                                            indicator TYPE c DEFAULT space sort TYPE abap_bool DEFAULT abap_false
                                  RETURNING VALUE(table_with_indicator) TYPE REF TO data,
      create_empty_table IMPORTING table_name TYPE tabname RETURNING VALUE(empty_table) TYPE REF TO data.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_table_info,
        name   TYPE tabname,
        handle TYPE REF TO cl_abap_tabledescr,
      END OF t_table_info,
      tt_table_info TYPE  HASHED TABLE OF t_table_info WITH UNIQUE KEY name.

    CONSTANTS:
        c_cd_field TYPE fieldname VALUE 'ZED_CHANGE_INDICATOR_FIELD'.

    METHODS:
      get_table_handle IMPORTING table_name TYPE tabname RETURNING VALUE(handle) TYPE REF TO cl_abap_tabledescr.

    DATA:
        tables_info TYPE tt_table_info.
ENDCLASS.
