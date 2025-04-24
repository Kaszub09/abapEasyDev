"! <p class="shorttext synchronized" lang="en">Documentation</p>
"! <br/>TAGS: docu; documentation; show
CLASS zcl_ed_docu DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_class,
        "! Translation with SE63->Other_text->C6->RE.
        report       TYPE doku_class VALUE 'RE',
        "! Translation with SE63->Other_text->H5->RE.
        domain       TYPE doku_class VALUE 'DO',
        "! Translation with SE63->Other_text->C6->DE.
        data_element TYPE doku_class VALUE 'DE',
        "! Translation with SE63->Other_text->H5->TB.
        table        TYPE doku_class VALUE 'TB',
        "! Include number in name. Translation with SE63->Other_text->C6->NA.
        messsage     TYPE doku_class VALUE 'NA',
        book         TYPE doku_class VALUE 'BOOK',
      END OF c_class.

    CLASS-METHODS:
      "! @parameter fallback_language | <p class="shorttext synchronized" lang="en">
      "! Tries to display docu in this language if <em>doklangu</em> doesn't exists</p>
      show IMPORTING dokclass                      TYPE doku_class
                     doklangu                      TYPE doku_langu DEFAULT sy-langu
                     dokname                       TYPE string
                     doktitle                      TYPE doku_title DEFAULT space
                     called_by_program             TYPE programm DEFAULT space
                     called_by_dynp                TYPE dynnr DEFAULT space
                     called_for_tab                TYPE tabname DEFAULT space
                     called_for_field              TYPE fieldname DEFAULT space
                     called_for_tab_fld_btch_input TYPE dynfnam DEFAULT space
                     msg_var_1                     TYPE any DEFAULT space
                     msg_var_2                     TYPE any DEFAULT space
                     msg_var_3                     TYPE any DEFAULT space
                     msg_var_4                     TYPE any DEFAULT space
                     called_by_cuaprog             TYPE programm DEFAULT space
                     called_by_cuastat             TYPE cuastatus OPTIONAL
                     short_text                    TYPE syst_batch DEFAULT space
                     classic_sapscript             TYPE char1 DEFAULT space
                     mes_program_name              TYPE syrepid DEFAULT space
                     mes_include_name              TYPE syrepid DEFAULT space
                     mes_line_number               TYPE int4 OPTIONAL
                     mes_exception                 TYPE seoclsname DEFAULT space
                     fallback_language             TYPE doku_langu DEFAULT 'E'
           RAISING   zcx_ed_exception.
ENDCLASS.

CLASS zcl_ed_docu IMPLEMENTATION.
  METHOD show.
    DATA links TYPE STANDARD TABLE OF tline WITH EMPTY KEY.

    CALL FUNCTION 'HELP_OBJECT_SHOW'
      EXPORTING
        dokclass                      = dokclass                         " Document class
        doklangu                      = doklangu                         " Language, for help -> always Sy-Langu
        dokname                       = dokname                          " Document name
        doktitle                      = doktitle                            " Window title
        called_by_program             = called_by_program                " calling program
        called_by_dynp                = called_by_dynp                   " calling screen
        called_for_tab                = called_for_tab                   " Field: Reference table
        called_for_field              = called_for_field                 " Field name
        called_for_tab_fld_btch_input = called_for_tab_fld_btch_input    " Batch input relevance structure/field
        msg_var_1                     = msg_var_1                        " Message: Variable 1
        msg_var_2                     = msg_var_2                        " Message: Variable 2
        msg_var_3                     = msg_var_3                        " Message: Variable 3
        msg_var_4                     = msg_var_4                        " Message: Variable 4
        called_by_cuaprog             = called_by_cuaprog
        called_by_cuastat             = called_by_cuastat
        short_text                    = short_text
        classic_sapscript             = classic_sapscript
        mes_program_name              = mes_program_name
        mes_include_name              = mes_include_name
        mes_line_number               = mes_line_number
        mes_exception                 = mes_exception
      TABLES
        links                         = links
      EXCEPTIONS
        object_not_found              = 1
        sapscript_error               = 2
        OTHERS                        = 3.
    IF sy-subrc = 0.
      RETURN.
    ELSEIF sy-subrc = 1 AND fallback_language <> space.
      CALL FUNCTION 'HELP_OBJECT_SHOW'
        EXPORTING
          dokclass                      = dokclass                         " Document class
          doklangu                      = fallback_language                         " Language, for help -> always Sy-Langu
          dokname                       = dokname                          " Document name
          doktitle                      = doktitle                            " Window title
          called_by_program             = called_by_program                " calling program
          called_by_dynp                = called_by_dynp                   " calling screen
          called_for_tab                = called_for_tab                   " Field: Reference table
          called_for_field              = called_for_field                 " Field name
          called_for_tab_fld_btch_input = called_for_tab_fld_btch_input    " Batch input relevance structure/field
          msg_var_1                     = msg_var_1                        " Message: Variable 1
          msg_var_2                     = msg_var_2                        " Message: Variable 2
          msg_var_3                     = msg_var_3                        " Message: Variable 3
          msg_var_4                     = msg_var_4                        " Message: Variable 4
          called_by_cuaprog             = called_by_cuaprog
          called_by_cuastat             = called_by_cuastat
          short_text                    = short_text
          classic_sapscript             = classic_sapscript
          mes_program_name              = mes_program_name
          mes_include_name              = mes_include_name
          mes_line_number               = mes_line_number
          mes_exception                 = mes_exception
        TABLES
          links                         = links
        EXCEPTIONS
          object_not_found              = 1
          sapscript_error               = 2
          OTHERS                        = 3.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |HELP_OBJECT_SHOW rc={ sy-subrc }|.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |HELP_OBJECT_SHOW rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
