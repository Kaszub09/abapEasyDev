INTERFACE zif_ed_selection_handler PUBLIC.
  METHODS:
    before_exit IMPORTING user_confirmed TYPE abap_bool selection TYPE zif_ed_selection=>tt_table_selection
                CHANGING stop_exit TYPE abap_bool.
ENDINTERFACE.
