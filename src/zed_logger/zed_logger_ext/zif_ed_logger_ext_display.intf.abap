"! <p class="shorttext synchronized" lang="en">Logger - display extension</p>
INTERFACE zif_ed_logger_ext_display PUBLIC .

  METHODS:
    get_display_cont IMPORTING messages_only  TYPE abap_bool DEFAULT abap_true
                     RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_cont,
    get_display_modal IMPORTING messages_only  TYPE abap_bool DEFAULT abap_true
                      RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_modal,
    get_display_nonmodal IMPORTING messages_only  TYPE abap_bool DEFAULT abap_true
                         RETURNING VALUE(display) TYPE REF TO zcl_ed_logger_display_nonmodal.
ENDINTERFACE.
