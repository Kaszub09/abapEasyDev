"! <p class="shorttext synchronized" lang="en">Empty implementation of logger.
"! Can be used at instantiation to avoid <em>IF logger IS BOUND</em> everywhere, when you want to skip logging.</p>
CLASS zcl_ed_logger_empty DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ed_logger_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_logger.
ENDCLASS.

CLASS zcl_ed_logger_empty IMPLEMENTATION.
  METHOD zif_ed_logger~add.
  ENDMETHOD.

  METHOD zif_ed_logger~clear_all_logs.
  ENDMETHOD.

  METHOD zif_ed_logger~e.
  ENDMETHOD.

  METHOD zif_ed_logger~i.
  ENDMETHOD.

  METHOD zif_ed_logger~s.
  ENDMETHOD.

  METHOD zif_ed_logger~save.
  ENDMETHOD.

  METHOD zif_ed_logger~w.
  ENDMETHOD.
ENDCLASS.
