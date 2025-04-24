*&---------------------------------------------------------------------*
*& Report zed_sapscript_copy_lang
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_sapscript_copy_lang.


PARAMETERS:
  p_txtnam TYPE tdobname,
  p_txtid  TYPE tdid DEFAULT 'ST',
  p_lanfro TYPE tdspras DEFAULT 'E',
  p_lanto  TYPE tdspras DEFAULT 'E',
  p_overwr TYPE checkbox DEFAULT abap_false.

START-OF-SELECTION.
  DATA:
    lines  TYPE STANDARD TABLE OF tline WITH EMPTY KEY,
    header TYPE thead.

  "CHECK IF TARGET EXISTS
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = p_txtid                " Text ID of text to be read
      language                = p_lanto                " Language of text to be read
      name                    = p_txtnam                " Name of text to be read
      object                  = 'TEXT'                 " Object of text to be read
    IMPORTING
      header                  = header
    TABLES
      lines                   = lines
    EXCEPTIONS
      id                      = 1                " Text ID invalid
      language                = 2                " Invalid language
      name                    = 3                " Invalid text name
      not_found               = 4                " Text not found
      object                  = 5                " Invalid text object
      reference_check         = 6                " Reference chain interrupted
      wrong_access_to_archive = 7                " Archive handle invalid for access
      OTHERS                  = 8.

  DATA(target_exists) = xsdbool( sy-subrc = 0 ).
  IF p_overwr = abap_false AND target_exists = abap_true.
    MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF sy-subrc <> 0 AND sy-subrc <> 4.
    MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  "GET TEXT
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = p_txtid                " Text ID of text to be read
      language                = p_lanfro                " Language of text to be read
      name                    = p_txtnam                " Name of text to be read
      object                  = 'TEXT'                 " Object of text to be read
    IMPORTING
      header                  = header
    TABLES
      lines                   = lines
    EXCEPTIONS
      id                      = 1                " Text ID invalid
      language                = 2                " Invalid language
      name                    = 3                " Invalid text name
      not_found               = 4                " Text not found
      object                  = 5                " Invalid text object
      reference_check         = 6                " Reference chain interrupted
      wrong_access_to_archive = 7                " Archive handle invalid for access
      OTHERS                  = 8.
  IF sy-subrc <> 0.
    MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  "CHANGE LANGUAGE
  header-tdspras = p_lanto.

  "SAVE TEXT
  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      header   = header                " Text header of text to be saved
    TABLES
      lines    = lines               " Lines of text to be saved
    EXCEPTIONS
      id       = 1                " Text ID in text header invalid
      language = 2                " Language in text header invalid
      name     = 3                " Text name in text header invalid
      object   = 4                " Text object in text header invalid
      OTHERS   = 5.
  IF sy-subrc = 0.
    MESSAGE TEXT-004 TYPE 'S'.
    LEAVE LIST-PROCESSING.
  ELSE.
    MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
