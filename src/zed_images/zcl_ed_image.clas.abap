"! <p class="shorttext synchronized" lang="en">Easy display of images</p>
"! <br/>TAGS: display; image
CLASS zcl_ed_image DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_screens_handler.

    CLASS-METHODS:
      read_from_db IMPORTING image_id TYPE zed_images-image_id RETURNING VALUE(image) TYPE REF TO zcl_ed_image RAISING zcx_ed_exception.

    METHODS:
        constructor IMPORTING image TYPE xstring OPTIONAL,
        load_from_xstring IMPORTING image TYPE xstring,
        display_in_container IMPORTING container TYPE REF TO cl_gui_container display_mode TYPE i DEFAULT cl_gui_picture=>display_mode_fit_center
                             RETURNING VALUE(picture_container) TYPE REF TO cl_gui_picture,
        display_in_screen IMPORTING display_mode TYPE i DEFAULT cl_gui_picture=>display_mode_fit_center
                                   start_column TYPE i DEFAULT 0 end_column TYPE i DEFAULT 128
                                   start_line TYPE i DEFAULT 0 end_line TYPE i DEFAULT 24.
    DATA:
      image_binary TYPE xstring READ-ONLY,
      image_url    TYPE char255 READ-ONLY.
  PRIVATE SECTION.
    DATA:
      first_time_pbo TYPE abap_bool,
      display_mode   TYPE i.
ENDCLASS.

CLASS zcl_ed_image IMPLEMENTATION.
  METHOD constructor.
    load_from_xstring( image ).
  ENDMETHOD.

  METHOD display_in_container.
    picture_container = NEW cl_gui_picture( container ).
    picture_container->set_display_mode( display_mode ).
    picture_container->load_picture_from_url_async( image_url ).
  ENDMETHOD.

  METHOD load_from_xstring.
    image_binary = image.

    DATA(solix) = cl_bcs_convert=>xstring_to_solix( image_binary ).

    CALL FUNCTION 'DP_CREATE_URL'
      EXPORTING
        type     = 'IMAGE'                 " MIME Type
        subtype  = space                " MIME Subtype
        lifetime = 'T' "T - transaction
      TABLES
        data     = solix
      CHANGING
        url      = image_url.
  ENDMETHOD.

  METHOD display_in_screen.
    zcl_ed_screens=>prepare_next_screen( handler = me create_container = abap_false ).
    me->display_mode = display_mode.
    first_time_pbo = abap_true.
    zcl_ed_screens=>call_next_screen( start_column = start_column end_column = end_column start_line = start_line end_line = end_line ).
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pai.
    CASE command.
      WHEN zif_ed_screens_handler=>c_status_commands-back OR zif_ed_screens_handler=>c_status_commands-exit OR zif_ed_screens_handler=>c_status_commands-cancel.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pbo.
    IF first_time_pbo = abap_false.
      RETURN.
    ENDIF.

    first_time_pbo = abap_false.
    DATA(picture_container) = NEW cl_gui_picture( zcl_ed_screens=>get_screen_container( dynnr ) ).
    picture_container->set_display_mode( display_mode ).
    picture_container->load_picture_from_url_async( image_url ).
  ENDMETHOD.

  METHOD read_from_db.
    SELECT SINGLE FROM zed_images FIELDS image_binary WHERE image_id = @image_id INTO @DATA(image_binary).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |Image '{ image_id }' not found.|.
    ENDIF.
    image = NEW zcl_ed_image( image_binary ).
  ENDMETHOD.
ENDCLASS.
