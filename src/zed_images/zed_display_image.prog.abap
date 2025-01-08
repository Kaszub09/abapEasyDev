*&---------------------------------------------------------------------*
*& Report zed_upload_image
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_display_image.

PARAMETERS:
  p_img_id TYPE zed_images-image_id.


START-OF-SELECTION.
  DATA(image) = zcl_ed_image=>read_from_db( image_id = p_img_id ).
  image->display_in_screen( ).
