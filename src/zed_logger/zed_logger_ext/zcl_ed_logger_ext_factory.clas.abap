CLASS zcl_ed_logger_ext_factory DEFINITION PUBLIC FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      create IMPORTING logger TYPE REF TO zif_ed_logger RETURNING VALUE(ext) TYPE REF TO zif_ed_logger_ext.
ENDCLASS.

CLASS zcl_ed_logger_ext_factory IMPLEMENTATION.
  METHOD create.
    DATA(ext_obj) = NEW zcl_ed_logger_ext( ).
    ext_obj->zif_ed_logger_ext~msg = NEW zcl_ed_logger_ext_msg( logger ).
    ext = ext_obj.
  ENDMETHOD.
ENDCLASS.
