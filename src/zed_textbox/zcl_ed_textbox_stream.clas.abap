"! <p class="shorttext synchronized">Textbox</p>
"! <br/>TAGS: textbox; text; edit; display
CLASS zcl_ed_textbox_stream DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES:
      zif_ed_screens_handler.

    METHODS:
      display IMPORTING in_edit_mode TYPE abap_bool DEFAULT abap_true line_size TYPE i DEFAULT 255
                        start_column TYPE i DEFAULT 0 end_column TYPE i DEFAULT 128 start_line TYPE i DEFAULT 0 end_line TYPE i DEFAULT 24
              CHANGING  textstream TYPE string OPTIONAL
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
      textstream       TYPE string,
      container        TYPE REF TO cl_gui_container,
      text_editor      TYPE REF TO cl_gui_textedit.
ENDCLASS.

CLASS zcl_ed_textbox_stream IMPLEMENTATION.
  METHOD display.
    me->in_edit_mode = in_edit_mode.
    me->was_save_clicked = abap_false.
    me->line_size = line_size.
    me->textstream = textstream.

    zcl_ed_screens=>prepare_next_screen( handler = me create_container = abap_false ).
    zcl_ed_screens=>call_next_screen( start_column = start_column end_column = end_column start_line = start_line end_line = end_line ).

    textstream = me->textstream.
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
    text_editor->set_textstream(
      EXPORTING
        text           = textstream
      EXCEPTIONS
       error_cntl_call_method = 1
       not_supported_by_gui = 2
       OTHERS          = 3 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |SET_TEXTSTREAM rc={ sy-subrc }|.
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
        text_editor->get_textstream(
          EXPORTING
            only_when_modified     = 0            " get text only when modified
          IMPORTING
            text                   = textstream " Text as String with Carriage Returns and Linefeeds
          EXCEPTIONS
            error_cntl_call_method = 1                " Error while retrieving a property from TextEdit control
            not_supported_by_gui   = 2                " Method is not supported by installed GUI
            OTHERS                 = 3 ).
        cl_gui_cfw=>flush( EXCEPTIONS OTHERS = 1 ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |GET_TEXTSTREAM rc={ sy-subrc }|.
        ENDIF.

        was_save_clicked = abap_true.
        LEAVE TO SCREEN 0.

      WHEN c_commands-cancel OR zif_ed_screens_handler=>c_status_commands-back OR zif_ed_screens_handler=>c_status_commands-exit OR zif_ed_screens_handler=>c_status_commands-cancel.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
