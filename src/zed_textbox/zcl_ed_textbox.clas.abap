"! <p class="shorttext synchronized">Textbox</p>
"! <br/>TAGS: textbox; text; edit; display
CLASS zcl_ed_textbox DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES:
      zif_ed_screens_handler.

    METHODS:
      display IMPORTING in_edit_mode TYPE abap_bool DEFAULT abap_true line_size TYPE i DEFAULT 255
                        start_column TYPE i DEFAULT 0 end_column TYPE i DEFAULT 128 start_line TYPE i DEFAULT 0 end_line TYPE i DEFAULT 24
              CHANGING text_table TYPE soli_tab OPTIONAL
              RETURNING VALUE(save_clicked) TYPE abap_bool.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_commands,
        save   TYPE syst_ucomm VALUE 'SAVE',
        cancel TYPE syst_ucomm VALUE 'CANCEL',
      END OF c_commands.

    DATA:
      in_edit_mode     TYPE abap_bool,
      was_save_clicked TYPE abap_bool,
      line_size        TYPE i,
      text_table_ref   TYPE REF TO soli_tab,
      container        TYPE REF TO cl_gui_container,
      text_editor      TYPE REF TO cl_gui_textedit.
ENDCLASS.

CLASS zcl_ed_textbox IMPLEMENTATION.
  METHOD display.
    me->in_edit_mode = in_edit_mode.
    me->was_save_clicked = abap_false.
    me->line_size = line_size.
    me->text_table_ref = REF #( text_table ).

    zcl_ed_screens=>prepare_next_screen( handler = me create_container = abap_false ).
    zcl_ed_screens=>call_next_screen( start_column = start_column end_column = end_column start_line = start_line end_line = end_line ).

    save_clicked = was_save_clicked.
    container->free( ).
    FREE: container, text_editor.
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pbo_get_toolbar_commands.
    IF in_edit_mode = abap_true.
      commands_to_set = VALUE #( ( command = c_commands-save description = VALUE #( icon_id = '@2L@' text = TEXT-c01 icon_text = TEXT-c01  ) )
        ( command = c_commands-cancel description = VALUE #( icon_id = '@0W@' text = TEXT-c02  icon_text = TEXT-c02  ) ) ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pbo.
    IF container IS BOUND.
      RETURN.
    ENDIF.
    container = zcl_ed_screens=>get_screen_container( ).
    text_editor = NEW #( container ).
    text_editor->set_wordwrap_behavior( wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position wordwrap_position = line_size ).
    text_editor->set_text_as_r3table(
      EXPORTING
        table           = text_table_ref->*                 " table with text
      EXCEPTIONS
        error_dp        = 1                " Error while sending R/3 table to TextEdit control!
        error_dp_create = 2                " ERROR_DP_CREATE
        OTHERS          = 3 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |SET_TEXT_AS_R3TABLE rc={ sy-subrc }|.
    ENDIF.
    IF in_edit_mode = abap_false.
      text_editor->set_readonly_mode(
        EXPORTING
          readonly_mode          = 1             " read-only mode; eq 0: OFF ; ne 0: ON
        EXCEPTIONS
          error_cntl_call_method = 1                " Error while setting read-only mode!
          invalid_parameter      = 2                " INVALID_PARAMETER
          OTHERS                 = 3 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |SET_READONLY_MODE rc={ sy-subrc }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pai.
    CASE command.
      WHEN c_commands-save.
        text_editor->get_text_as_r3table(
            EXPORTING
              only_when_modified     = 0            " get text only when modified
            IMPORTING
              table                  = text_table_ref->*                " text as R/3 table
*              is_modified            =                  " modify status of text
            EXCEPTIONS
              error_dp               = 1                " Error while retrieving text table via DataProvider control!
              error_cntl_call_method = 2                " Error while retrieving a property from TextEdit control
              error_dp_create        = 3                " Error while creating DataProvider Control
              potential_data_loss    = 4                " Potential data loss: use get_text_as_stream instead
              OTHERS                 = 5 ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |GET_TEXT_AS_R3TABLE rc={ sy-subrc }|.
        ENDIF.
        was_save_clicked = abap_true.
        LEAVE TO SCREEN 0.

      WHEN c_commands-cancel OR zif_ed_screens_handler=>c_status_commands-back OR zif_ed_screens_handler=>c_status_commands-exit OR zif_ed_screens_handler=>c_status_commands-cancel.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
