"! <p class="shorttext synchronized" lang="en">Mass change
"! <br/>Check <strong>zcl_ed_mass_example</strong> on how to implement it.
"! <br/>TAGS: mass; change;
INTERFACE zif_ed_mass PUBLIC.
  TYPES:
    "! Must be included inside data table definition.
    "! <br/>Only <strong>result</strong> can be changed during save, <strong>mass_info</strong> should no be touched.
    BEGIN OF t_table_include,
      BEGIN OF result,
        success TYPE zted_success,
        msg     TYPE zted_msg,
        logger  TYPE REF TO zif_ed_logger,
      END OF result,
      BEGIN OF mass_int,
        color     TYPE lvc_t_scol,
        style     TYPE lvc_t_styl,
        exception TYPE c LENGTH 1,
      END OF mass_int,
    END OF t_table_include,

    BEGIN OF t_config,
      layout_key           TYPE salv_s_layout_key,
      disable_read_from_db TYPE abap_bool,
      BEGIN OF docu,
        id     TYPE doku_id,
        object TYPE doku_obj,
      END OF docu,
    END OF t_config,

    tt_hidden_columns TYPE SORTED TABLE OF fieldname WITH UNIQUE KEY table_line.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Just use <strong>CREATE DATA TABLE_REF TYPE [your table type].</strong></p>
    create_table RETURNING VALUE(table_ref) TYPE REF TO data,
    "! <p class="shorttext synchronized" lang="en">Modify ALV before display, e.g. hide some columns etc.</p>
    modify_alv IMPORTING alv TYPE REF TO zcl_ea_alv_table,
    "! <p class="shorttext synchronized" lang="en">You can hide some fields et.c you don't want to be read from file</p>
    modify_dtti_target IMPORTING target TYPE REF TO zif_dtti_target,
    "! <p class="shorttext synchronized" lang="en">You can cast ref to your table type</p>
    fill_table_with_db_data IMPORTING table_ref TYPE REF TO data,
    "! <p class="shorttext synchronized" lang="en">Do not change table order.</p>
    "! @parameter table_ref | <p class="shorttext synchronized" lang="en">You should fill <strong>result</strong> field with change result.</p>
    "! @parameter hidden_columns | <p class="shorttext synchronized" lang="en">Columns hidden by user
    "! - can be interpreted as 'don't change those fields'</p>
    save IMPORTING table_ref TYPE REF TO data hidden_columns TYPE tt_hidden_columns.

  DATA:
    config TYPE t_config READ-ONLY.
ENDINTERFACE.
