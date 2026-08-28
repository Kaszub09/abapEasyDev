"! <p class="shorttext synchronized" lang="en">MASS Callback, for interaction with ZCL_ED_MASS</p>
INTERFACE zif_ed_mass_callback PUBLIC.
  TYPES:
    tt_hidden_columns TYPE SORTED TABLE OF fieldname WITH UNIQUE KEY table_line.

  METHODS:
    "! @parameter hidden_columns | <p class="shorttext synchronized" lang="en">Columns hidden by user
    "! - can be interpreted as 'don't change those fields'</p>
    get_hidden_columns RETURNING VALUE(hidden_columns) TYPE tt_hidden_columns,
    get_selected_rows RETURNING VALUE(rows) TYPE lvc_t_row,
    "! <p class="shorttext synchronized" lang="en">You should fill <strong>result</strong> field with change result.</p>
    "! @parameter refresh_mass_int_info | <p class="shorttext synchronized" lang="en">Refresh colors, style based on success etc.</p>
    refresh_alv IMPORTING only_selected_rows TYPE abap_bool DEFAULT abap_true refresh_mass_int_info TYPE abap_bool DEFAULT abap_true,
    get_table_ref RETURNING VALUE(table_ref) TYPE REF TO data,
    ask_confirm_save RETURNING VALUE(is_confirmed) TYPE abap_bool,
    are_rows_selected IMPORTING ask_for_sel_if_none_sel TYPE abap_bool DEFAULT abap_true RETURNING VALUE(are_selected) TYPE abap_bool.
ENDINTERFACE.
