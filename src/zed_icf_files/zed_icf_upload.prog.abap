*&---------------------------------------------------------------------*
*& Report zed_icf_upload
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_icf_upload.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
PARAMETERS:
  p_path   TYPE zted_icf_path,
  p_medtyp TYPE zted_icf_media_type.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-s02.
PARAMETERS:
  p_file TYPE string.
SELECTION-SCREEN END OF BLOCK b02.

INITIALIZATION.
  DATA(fe) = zcl_ed_file_explorer_factory=>create_gui( ).

START-OF-SELECTION.
  IF p_file = space.
    RETURN.
  ENDIF.

  TRY.
      DATA(zed_icf_binary) = VALUE zed_icf_binary( path = p_path media_type = p_medtyp content = fe->file_read_as_bin( p_file ) ).
      MODIFY zed_icf_binary FROM @zed_icf_binary.
      IF sy-subrc <> 0.
        zcl_ed_msg=>throw( TEXT-e01 ).
      ENDIF.
      MESSAGE TEXT-001 TYPE 'S'.

    CATCH zcx_ed_exception INTO DATA(cx).
      MESSAGE cx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA(fd) = zcl_ed_file_explorer_factory=>create_dialogue( ).
  p_file = fd->pick_file( ).
