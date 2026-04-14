"! <p class="shorttext synchronized" lang="en">Logger extensions - modify as needed</p>
INTERFACE zif_ed_logger_ext PUBLIC.
  DATA:
    msg     TYPE REF TO zif_ed_logger_ext_msg READ-ONLY,
    "! Shortcuts for ZCL_ED_LOGGER_DISPLAY_FACTORY
    display TYPE REF TO zif_ed_logger_ext_display READ-ONLY.
ENDINTERFACE.
