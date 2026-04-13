"! <p class="shorttext synchronized">Empty implementation of logger.
"! Can be used at instantiation to avoid <em>IF logger IS BOUND</em> everywhere, when you want to skip logging.</p>
CLASS zcl_ed_logger_empty DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ed_logger_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_logger.
ENDCLASS.

CLASS zcl_ed_logger_empty IMPLEMENTATION.
  METHOD zif_ed_logger~add.
    logger = me.
  ENDMETHOD.

  METHOD zif_ed_logger~clear_all_logs.
    logger = me.
  ENDMETHOD.

  METHOD zif_ed_logger~e.
    logger = me.
  ENDMETHOD.

  METHOD zif_ed_logger~i.
    logger = me.
  ENDMETHOD.

  METHOD zif_ed_logger~s.
    logger = me.
  ENDMETHOD.

  METHOD zif_ed_logger~save.
    logger = me.
  ENDMETHOD.

  METHOD zif_ed_logger~w.
    logger = me.
  ENDMETHOD.

  METHOD zif_ed_logger~change_header.
    logger = me.
  ENDMETHOD.
ENDCLASS.
