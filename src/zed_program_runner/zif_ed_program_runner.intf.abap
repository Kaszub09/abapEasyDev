"! <p class="shorttext synchronized">Program runner interface</p>
"! <br/>TAGS: program runner; list; export; memory
INTERFACE zif_ed_program_runner PUBLIC.
  TYPES:
    tt_selection_table TYPE STANDARD TABLE OF rsparamsl_255 WITH EMPTY KEY,
    tt_list            TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
    BEGIN OF t_run_with_data_ref,
      table_ref TYPE REF TO data,
      "! Useful to recreate salv
      metadata  TYPE cl_salv_bs_runtime_info=>s_type_metadata,
    END OF t_run_with_data_ref,
    BEGIN OF t_converted_data_ref,
      "! Since salv references this table, it mustn't go out of scope
      table_ref TYPE REF TO data,
      salv      TYPE REF TO cl_salv_table,
    END OF t_converted_data_ref.

  METHODS:
    run_with_list IMPORTING program_name TYPE programm variant_name TYPE variant OPTIONAL selection_table TYPE tt_selection_table OPTIONAL
                  RETURNING VALUE(list) TYPE tt_list
                  RAISING zcx_ed_exception,
    "! <p class="shorttext synchronized" lang="en">Standard report result (with SALV/ALV) is:</p>
    "! <br/> [PROGRAM TITLE] - sometimes not present
    "! <br/> ----------------------------
    "! <br/> [HEADERS]
    "! <br/> ----------------------------
    "! <br/> [FIRST ROW]
    "! <br/> ...
    "! <br/> [LAST ROW]
    "! <br/> ----------------------------
    "! <br/>
    "! <br/> Be careful with wide reports since the line could be broken if about 1000 characters is exceeded.
    "! <br/> Usual delimiter is vertical line '&#124;', present also before first and after last column.
    clean_list IMPORTING rows_to_remove_from_start TYPE i DEFAULT 4 remove_last_row TYPE abap_bool DEFAULT abap_true
                         remove_first_last_char TYPE abap_bool DEFAULT abap_true
               CHANGING list TYPE tt_list,
    run_with_data_ref IMPORTING program_name TYPE programm variant_name TYPE variant OPTIONAL selection_table TYPE tt_selection_table OPTIONAL
                      RETURNING VALUE(result) TYPE t_run_with_data_ref
                      RAISING cx_salv_bs_sc_runtime_info,
    convert_data_ref IMPORTING data_ref_result TYPE t_run_with_data_ref
                     RETURNING VALUE(converted) TYPE t_converted_data_ref.

ENDINTERFACE.
