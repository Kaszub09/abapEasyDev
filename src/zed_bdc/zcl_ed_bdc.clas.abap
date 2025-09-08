"! <p class="shorttext synchronized">BDC</p>
"! <br/>TAGS: bdc; call transaction
CLASS zcl_ed_bdc DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      tt_bdc TYPE STANDARD TABLE OF bdcdata WITH EMPTY KEY,
      tt_msg TYPE STANDARD TABLE OF bdcmsgcoll WITH EMPTY KEY,
      BEGIN OF t_call_result,
        "! 0 - The processing of the called transaction was successful.
        "! <br/> &lt;1000 - Error in the called transaction. If a message was sent within the transaction, it can be received using the addition MESSAGES.
        "! <br/> 1001 - Processing error.
        subrc TYPE sy-subrc,
        msgs  TYPE tt_msg,
      END OF t_call_result,
      t_mode TYPE c LENGTH 1.

    CONSTANTS:
      BEGIN OF c_mode,
        show_screen               TYPE ctu_mode VALUE 'A',
        show_screen_on_error      TYPE ctu_mode VALUE 'E',
        no_screens_stop_on_bp     TYPE ctu_mode VALUE 'N',
        no_screens_debugger_on_bp TYPE ctu_mode VALUE 'P',
      END OF c_mode,
      BEGIN OF c_update,
        asynchronous TYPE ctu_update VALUE 'A',
        synchronous  TYPE ctu_update VALUE 'S',
        local        TYPE ctu_update VALUE 'L',
      END OF c_update.

    METHODS:
      begin_screen IMPORTING program TYPE bdc_prog dynpro TYPE bdc_dynr RETURNING VALUE(self) TYPE REF TO zcl_ed_bdc,
      set_value IMPORTING fnam TYPE fnam_____4 fval TYPE bdc_fval RETURNING VALUE(self) TYPE REF TO zcl_ed_bdc,
      set_code IMPORTING fval TYPE bdc_fval RETURNING VALUE(self) TYPE REF TO zcl_ed_bdc,
      set_cursor IMPORTING fval TYPE bdc_fval RETURNING VALUE(self) TYPE REF TO zcl_ed_bdc,
      call_transaction IMPORTING tcode              TYPE sy-tcode
                                 options            TYPE ctu_params
                       RETURNING VALUE(call_result) TYPE t_call_result
                       RAISING   zcx_ed_exception,
      get_options IMPORTING dismode        TYPE ctu_mode DEFAULT c_mode-show_screen_on_error
                            updmode        TYPE ctu_update DEFAULT c_update-synchronous
                            cattmode       TYPE ctu_catt DEFAULT space
                            defsize        TYPE ctu_defsze DEFAULT abap_false
                            racommit       TYPE ctu_rafc DEFAULT abap_false
                            nobinpt        TYPE ctu_nobim DEFAULT abap_false
                            nobiend        TYPE ctu_noben DEFAULT abap_false
                  RETURNING VALUE(options) TYPE ctu_params.

    DATA:
        tab TYPE tt_bdc READ-ONLY.
ENDCLASS.

CLASS zcl_ed_bdc IMPLEMENTATION.
  METHOD begin_screen.
    self = me.
    APPEND VALUE #( program = program dynpro = dynpro dynbegin = 'X' ) TO tab.
  ENDMETHOD.

  METHOD set_value.
    self = me.
    APPEND VALUE #( fnam = fnam fval = fval ) TO tab.
  ENDMETHOD.

  METHOD set_code.
    self = me.
    APPEND VALUE #( fnam = 'BDC_OKCODE' fval = fval ) TO tab.
  ENDMETHOD.

  METHOD set_cursor.
    self = me.
    APPEND VALUE #( fnam = 'BDC_CURSOR' fval = fval ) TO tab.
  ENDMETHOD.

  METHOD call_transaction.
    TRY.
        CALL TRANSACTION tcode WITH AUTHORITY-CHECK USING tab MESSAGES INTO call_result-msgs OPTIONS FROM options.
        call_result-subrc = sy-subrc.
      CATCH cx_sy_authorization_error INTO DATA(cx).
        zcx_ed_exception=>throw( cx->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_options.
    options = VALUE #( dismode = dismode updmode = updmode cattmode = cattmode defsize = defsize
        racommit = racommit nobinpt = nobinpt nobiend = nobiend ).
  ENDMETHOD.
ENDCLASS.
