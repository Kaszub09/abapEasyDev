"! <p class="shorttext synchronized" lang="en">Mass change
"! <br/>Check <strong>zcl_ed_mass_example</strong> on how to implement it.
"! <br/>TAGS: mass; change;
INTERFACE zif_ed_mass PUBLIC.
  TYPES:
    "! Must be included inside data table definition.
    "! <br/>Only <strong>result</strong> can be changed during save, <strong>mass_int</strong> should no be touched.
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
      header_text TYPE string,
      layout_key  TYPE salv_s_layout_key,
      "! Standard table to preserve order
      functions   TYPE STANDARD TABLE OF stb_button WITH EMPTY KEY
        WITH UNIQUE SORTED KEY function COMPONENTS function,
      BEGIN OF docu,
        id     TYPE doku_id,
        object TYPE doku_obj,
      END OF docu,
    END OF t_config.

  CONSTANTS:
    BEGIN OF c_functions,
      documentation       TYPE ui_func VALUE 'DOCUMENTATION',
      read_from_clipboard TYPE ui_func VALUE 'READ_FROM_CLIPBOARD',
      read_from_file      TYPE ui_func VALUE 'READ_FROM_FILE',
      read_from_db        TYPE ui_func VALUE 'READ_FROM_DB',
      save                TYPE ui_func VALUE 'SAVE',
    END OF c_functions.

  METHODS:
    "! @parameter callback | <p class="shorttext synchronized" lang="en">Usually only needed in <strong>function</strong>,
    "! but can be saved if needed elsewhere</p>
    setup IMPORTING callback TYPE REF TO zif_ed_mass_callback
          CHANGING  config   TYPE t_config,
    "! <p class="shorttext synchronized" lang="en">Just use <strong>CREATE DATA TABLE_REF TYPE [your table type].</strong></p>
    create_table RETURNING VALUE(table_ref) TYPE REF TO data,
    "! <p class="shorttext synchronized" lang="en">Modify ALV before display, e.g. hide some columns etc.</p>
    modify_alv DEFAULT IGNORE IMPORTING alv TYPE REF TO zcl_ea_alv_table,
    "! <p class="shorttext synchronized" lang="en">You can hide some fields et.c you don't want to be read from file</p>
    modify_dtti_target DEFAULT IGNORE IMPORTING target TYPE REF TO zif_dtti_target,
    "! <p class="shorttext synchronized" lang="en">You can cast ref to your table type</p>
    fill_table_with_db_data DEFAULT IGNORE IMPORTING table_ref TYPE REF TO data,
    "! @parameter callback | <p class="shorttext synchronized" lang="en">Same as in setup, usually only needed here.</p>
    function IMPORTING function TYPE ui_func callback TYPE REF TO zif_ed_mass_callback.
ENDINTERFACE.
