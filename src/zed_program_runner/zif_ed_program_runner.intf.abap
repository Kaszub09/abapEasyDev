"! <p class="shorttext synchronized">Program runner interface</p>
"! <br/>TAGS: program runner; list; export; memory
INTERFACE zif_ed_program_runner PUBLIC.
  TYPES:
    tt_selection_table TYPE STANDARD TABLE OF rsparamsl_255 WITH EMPTY KEY,
    tt_list            TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

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
               CHANGING list TYPE tt_list.

ENDINTERFACE.
