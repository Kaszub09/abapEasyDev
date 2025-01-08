*&---------------------------------------------------------------------*
*& Report zed_upload_image
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_upload_image.

PARAMETERS:
  p_img_id TYPE zed_images-image_id,
  p_bin    TYPE string.


START-OF-SELECTION.
  IF p_bin = space.
    RETURN.
  ENDIF.

  DATA(image) = VALUE zed_images( mandt = sy-mandt image_id = p_img_id  ).
  DATA data_tab TYPE solix_tab.
  cl_gui_frontend_services=>gui_upload( EXPORTING filename = p_bin filetype = 'BIN' CHANGING data_tab = data_tab EXCEPTIONS OTHERS = 1 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  image-image_binary = cl_bcs_convert=>solix_to_xstring( data_tab ).

  MODIFY zed_images FROM image.
  MESSAGE |Success| TYPE 'S'.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bin.
  DATA:
    desktop_directory TYPE string,
    files             TYPE filetable,
    files_count       TYPE i,
    user_action       TYPE i.

  cl_gui_frontend_services=>get_desktop_directory( CHANGING desktop_directory = desktop_directory ).
  cl_gui_cfw=>update_view( ). "Magic line to actually fill desktop value

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
       initial_directory       = desktop_directory
     CHANGING
       file_table              = files
       rc                      = files_count
       user_action             = user_action ).

  IF user_action = cl_gui_frontend_services=>action_ok AND files_count = 1 .
    p_bin = files[ 1 ]-filename.
  ENDIF.
