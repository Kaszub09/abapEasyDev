INTERFACE zif_ed_screens_handler PUBLIC.
  TYPES:
    BEGIN OF t_command,
      command     TYPE syst_ucomm,
      description TYPE smp_dyntxt,
    END OF t_command,
    tt_command         TYPE STANDARD TABLE OF t_command WITH EMPTY KEY,
    tt_status_commands TYPE STANDARD TABLE OF sy-ucomm WITH EMPTY KEY.

  CONSTANTS:
    BEGIN OF c_status_commands,
      ok            TYPE sy-ucomm VALUE 'OK',
      save          TYPE sy-ucomm VALUE 'SAVE',
      back          TYPE sy-ucomm VALUE 'BACK',
      exit          TYPE sy-ucomm VALUE 'EXIT',
      cancel        TYPE sy-ucomm VALUE 'CANCEL',
      print         TYPE sy-ucomm VALUE 'PRINT',
      search        TYPE sy-ucomm VALUE 'SEARCH',
      search_plus   TYPE sy-ucomm VALUE 'SEARCH+',
      p_minus_minus TYPE sy-ucomm VALUE 'P--',
      p_minus       TYPE sy-ucomm VALUE 'P-',
      p_plus        TYPE sy-ucomm VALUE 'P+',
      p_plus_plus   TYPE sy-ucomm VALUE 'P++',
    END OF c_status_commands.

  METHODS:
    pbo DEFAULT IGNORE IMPORTING dynnr TYPE sy-dynnr,
    pbo_get_status_commands_to_ex DEFAULT IGNORE IMPORTING dynnr TYPE sy-dynnr RETURNING VALUE(commands_to_exclude) TYPE tt_status_commands,
    pbo_get_toolbar_commands DEFAULT IGNORE IMPORTING dynnr TYPE sy-dynnr RETURNING VALUE(commands_to_set) TYPE tt_command,
    pbo_change_header DEFAULT IGNORE IMPORTING dynnr TYPE sy-dynnr CHANGING header TYPE syst_title,
    pai DEFAULT IGNORE IMPORTING dynnr TYPE sy-dynnr VALUE(command) TYPE sy-ucomm.
ENDINTERFACE.
