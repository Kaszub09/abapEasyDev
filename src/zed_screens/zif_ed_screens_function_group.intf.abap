INTERFACE zif_ed_screens_function_group PUBLIC.
  CLASS-METHODS:
    raise_pbo IMPORTING dynnr TYPE sy-dynnr DEFAULT sy-dynnr,
    raise_pai IMPORTING dynnr TYPE sy-dynnr DEFAULT sy-dynnr VALUE(command) TYPE sy-ucomm.
ENDINTERFACE.
