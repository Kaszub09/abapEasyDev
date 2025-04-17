"! <p class="shorttext synchronized">PDF display</p>
"! "! <br/>TAGS: display; scree0n; PDF</p>
CLASS zcl_ed_pdf_display DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_screens_handler.

    METHODS:
      display_pdf IMPORTING pdf TYPE xstring  RAISING zcx_ed_exception,
      display_pdf_in_popup IMPORTING pdf TYPE xstring
                                     start_column TYPE i DEFAULT 1 start_line TYPE i DEFAULT 1
                                     end_column TYPE i DEFAULT 128 end_line TYPE i DEFAULT 48
                           RAISING zcx_ed_exception.

  PRIVATE SECTION.
    METHODS:
      prepare_html_viewer IMPORTING pdf TYPE xstring container TYPE REF TO cl_gui_container RAISING zcx_ed_exception.

    DATA:
      pdf         TYPE xstring,
      html_viewer TYPE REF TO cl_gui_html_viewer.
ENDCLASS.

CLASS zcl_ed_pdf_display IMPLEMENTATION.
  METHOD display_pdf.
    prepare_html_viewer( pdf = pdf container = zcl_ed_screens=>prepare_next_screen( ) ).
    zcl_ed_screens=>call_next_screen( ).
    FREE:html_viewer.
  ENDMETHOD.

  METHOD display_pdf_in_popup.
    me->pdf = pdf.
    zcl_ed_screens=>prepare_next_screen( handler = me create_container = abap_false ).
    zcl_ed_screens=>call_next_screen( start_column = start_column end_column = end_column start_line = start_line end_line = end_line ).
    FREE:html_viewer, me->pdf.
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pbo.
    prepare_html_viewer( pdf = pdf container = zcl_ed_screens=>get_screen_container( sy-dynnr ) ).
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pai.
    CASE command.
      WHEN zif_ed_screens_handler=>c_status_commands-back OR zif_ed_screens_handler=>c_status_commands-exit OR zif_ed_screens_handler=>c_status_commands-cancel.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD prepare_html_viewer.
    html_viewer = NEW cl_gui_html_viewer( container ).

    DATA url TYPE char255.
    DATA(pdf_tab) = cl_bcs_convert=>xstring_to_solix( pdf ).

    html_viewer->load_data(
      EXPORTING
*        url                    =                  " URL
        type                   = 'application'           " Type of a MIME Object
        subtype                = 'pdf'           " Subtype of a MIME Object
        size                   = xstrlen( pdf )                " Length of Data
*        encoding               =                  " Encoding for MIME Object
*        charset                =                  " Encoding for MIME Object
*        needfiltering          = 0                " If it is 1 or 2, content is filtered, else no filter
*        language               =
*        i_tidyt                =                  " For special calls only
      IMPORTING
        assigned_url           = url                 " URL
      CHANGING
        data_table             = pdf_tab                 " data table
*        iscontentchanged       =                  " Sets to 1 if the content is filtered else it is 0
      EXCEPTIONS
        dp_invalid_parameter   = 1                " invalid parameter in a DP call
        dp_error_general       = 2                " gerneral error in a DP call
        cntl_error             = 3                " error
        html_syntax_notcorrect = 4                " HTML data is invalid and check all the tags' syntax
        OTHERS                 = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |HTML_VIEWER->LOAD_DATA rc={ sy-subrc }|.
    ENDIF.

    html_viewer->show_url(
      EXPORTING
        url                    = url                 " URL
*        frame                  =                  " frame where the data should be shown
*        in_place               = ' X'             " Is the document displayed in the GUI?
      EXCEPTIONS
        cntl_error             = 1                " Error in CFW Call
        cnht_error_not_allowed = 2                " Navigation outside R/3 is not allowed
        cnht_error_parameter   = 3                " Incorrect parameters
        dp_error_general       = 4                " Error in DP FM call
        OTHERS                 = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |HTML_VIEWER->SHOW_URL rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
