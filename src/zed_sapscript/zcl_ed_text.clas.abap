CLASS zcl_ed_text DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_text_key,
        tdid     TYPE tdid,
        tdspras  TYPE spras,
        tdname   TYPE tdobname,
        tdobject TYPE tdobject,
      END OF t_text_key,
      t_local_catalog TYPE c LENGTH 1,
      tt_line         TYPE STANDARD TABLE OF tline WITH DEFAULT KEY,
      tt_object       TYPE STANDARD TABLE OF stxdrobj WITH DEFAULT KEY,
      tt_name         TYPE STANDARD TABLE OF stxdrname WITH DEFAULT KEY,
      tt_id           TYPE STANDARD TABLE OF stxdrid WITH DEFAULT KEY,
      tt_language     TYPE STANDARD TABLE OF stxdrlang WITH DEFAULT KEY.

    CLASS-METHODS:
      init_text IMPORTING text_key           TYPE t_text_key
                          throw_if_not_found TYPE abap_bool DEFAULT abap_false
                EXPORTING header             TYPE thead
                RAISING   zcx_ed_exception,
      read_text IMPORTING text_key           TYPE t_text_key
                          throw_if_not_found TYPE abap_bool DEFAULT abap_false
                EXPORTING header             TYPE thead
                          old_line_counter   TYPE tdtxtlines
                          lines              TYPE tt_line
                RAISING   zcx_ed_exception,
      read_text_ret IMPORTING text_key         TYPE t_text_key
                    EXPORTING header           TYPE thead
                              old_line_counter TYPE tdtxtlines
                    RETURNING VALUE(lines)     TYPE tt_line
                    RAISING   zcx_ed_exception,
      save_text IMPORTING header     TYPE thead
                          lines      TYPE tt_line
                EXPORTING new_header TYPE thead
                RAISING   zcx_ed_exception,
      lines_to_string IMPORTING lines         TYPE tt_line
                                language      TYPE syst_langu DEFAULT sy-langu
                      RETURNING VALUE(string) TYPE string,
      "! @parameter iv_fw | <p class="shorttext synchronized" lang="en">Format width</p>
      string_to_lines IMPORTING string       TYPE string
                                language     TYPE syst_langu DEFAULT sy-langu
                                iv_fw        TYPE int1 DEFAULT 72
                      RETURNING VALUE(lines) TYPE tt_line.

  PRIVATE SECTION.
    CONSTANTS:
       c_tdline_size TYPE int1 VALUE 132.
ENDCLASS.

CLASS zcl_ed_text IMPLEMENTATION.
  METHOD init_text.
    DATA: lines TYPE tt_line.

    CALL FUNCTION 'INIT_TEXT'
      EXPORTING
        id       = text_key-tdid
        language = text_key-tdspras
        name     = text_key-tdname
        object   = text_key-tdobject
      IMPORTING
        header   = header
      TABLES
        lines    = lines                 " Initialized table for lines
      EXCEPTIONS
        id       = 1                " Text ID invalid
        language = 2                " Invalid language
        name     = 3                " Invalid text name
        object   = 4                " Invalid text object
        OTHERS   = 5.
    IF sy-subrc <> 0.
      IF sy-msgid IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(msg).
      ENDIF.
      zcl_ed_msg=>throw( |{ msg } (INIT_TEXT={ sy-subrc })| ).
    ENDIF.
  ENDMETHOD.

  METHOD read_text.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = text_key-tdid
        language                = text_key-tdspras
        name                    = text_key-tdname
        object                  = text_key-tdobject
      IMPORTING
        header                  = header
        old_line_counter        = old_line_counter
      TABLES
        lines                   = lines                 " Lines of text read
      EXCEPTIONS
        id                      = 1                " Text ID invalid
        language                = 2                " Invalid language
        name                    = 3                " Invalid text name
        not_found               = 4                " Text not found
        object                  = 5                " Invalid text object
        reference_check         = 6                " Reference chain interrupted
        wrong_access_to_archive = 7                " Archive handle invalid for access
        OTHERS                  = 8.
    IF sy-subrc = 0 OR ( sy-subrc = 4 AND throw_if_not_found = abap_false ).
      RETURN.
    ENDIF.

    IF sy-msgid IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(msg).
    ENDIF.
    zcl_ed_msg=>throw( |{ msg } (READ_TEXT={ sy-subrc })| ).
  ENDMETHOD.

  METHOD save_text.
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header    = header
      IMPORTING
        newheader = new_header                 " Text header (changed)
      TABLES
        lines     = lines
      EXCEPTIONS
        id        = 1                " Text ID in text header invalid
        language  = 2                " Language in text header invalid
        name      = 3                " Text name in text header invalid
        object    = 4                " Text object in text header invalid
        OTHERS    = 5.
    IF sy-subrc <> 0.
      DATA(subrc) = sy-subrc.
      IF sy-msgid IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(msg).
      ENDIF.
      zcl_ed_msg=>throw( |{ msg } (SAVE_TEXT={ subrc })| ).
    ENDIF.
  ENDMETHOD.

  METHOD lines_to_string.
    DATA: stream_lines    TYPE string_table.
    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      EXPORTING
        language     = language
        lf           = abap_true
      IMPORTING
        stream_lines = stream_lines
      TABLES
        itf_text     = lines.

    LOOP AT stream_lines REFERENCE INTO DATA(stream_line).
      IF sy-tabix <> 1.
        string = |{ string }{ cl_abap_char_utilities=>newline }|.
      ENDIF.
      string = |{ string }{ stream_line->* }|.
    ENDLOOP.
  ENDMETHOD.

  METHOD string_to_lines.
    "Comment from inside function:
    "* in case 'LF' change STREAM_TABLE to TEXT_STREAM
    "Like, just why, SAP?
    CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
      EXPORTING
        stream_lines = VALUE string_table( ( string ) )
        language     = language
        lf           = abap_true
        iv_fw        = iv_fw
        iv_lw        = c_tdline_size
      TABLES
        itf_text     = lines.
  ENDMETHOD.

  METHOD read_text_ret.
    read_text( EXPORTING text_key = text_key IMPORTING header = header old_line_counter = old_line_counter lines = lines ).
  ENDMETHOD.
ENDCLASS.
