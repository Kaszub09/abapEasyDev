CLASS zcl_ed_file_explorer_dialogue DEFINITION PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS zcl_ed_file_explorer_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_file_dialogue.
ENDCLASS.

CLASS zcl_ed_file_explorer_dialogue IMPLEMENTATION.
  METHOD zif_ed_file_dialogue~get_file_filter.
    LOOP AT file_types INTO DATA(file_type).
      CASE file_type.
        WHEN zif_ed_file_dialogue~c_file_type-all. file_filter = |{ file_filter }{ TEXT-f00 }|.
        WHEN zif_ed_file_dialogue~c_file_type-txt. file_filter = |{ file_filter }{ TEXT-f01 }|.
        WHEN zif_ed_file_dialogue~c_file_type-csv. file_filter = |{ file_filter }{ TEXT-f02 }|.
        WHEN zif_ed_file_dialogue~c_file_type-xlsx. file_filter = |{ file_filter }{ TEXT-f03 }|.
        WHEN zif_ed_file_dialogue~c_file_type-xml. file_filter = |{ file_filter }{ TEXT-f04 }|.
        WHEN zif_ed_file_dialogue~c_file_type-pdf. file_filter = |{ file_filter }{ TEXT-f05 }|.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_ed_file_dialogue~pick_file.
    DATA:
      files       TYPE zif_ed_file_dialogue~tt_file_paths,
      rc          TYPE i,
      user_action TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = window_title                 " Title Of File Open Dialog
*        default_extension       =                  " Default Extension
*        default_filename        =                  " Default File Name
        file_filter             = file_filter                 " File Extension Filter String
*        with_encoding           =                  " File Encoding
        initial_directory       = initial_directory                 " Initial Directory
        multiselection          = abap_false                 " Multiple selections poss.
      CHANGING
        file_table              = files                 " Table Holding Selected Files
        rc                      = rc                 " Return Code, Number of Files or -1 If Error Occurred
        user_action             = user_action                 " User Action (See Class Constants ACTION_OK, ACTION_CANCEL)
*        file_encoding           =
      EXCEPTIONS
        file_open_dialog_failed = 1                " "Open File" dialog failed
        cntl_error              = 2                " Control error
        error_no_gui            = 3                " No GUI available
        not_supported_by_gui    = 4                " GUI does not support this
        OTHERS                  = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG rc={ sy-subrc }|.
    ENDIF.

    IF user_action = cl_gui_frontend_services=>action_ok AND lines( files ) > 0.
      file_path = files[ 1 ]-filename.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_dialogue~pick_files.
    DATA:
      rc          TYPE i,
      user_action TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = window_title                 " Title Of File Open Dialog
*        default_extension       =                  " Default Extension
*        default_filename        =                  " Default File Name
        file_filter             = file_filter                 " File Extension Filter String
*        with_encoding           =                  " File Encoding
        initial_directory       = initial_directory                 " Initial Directory
        multiselection          = abap_true                 " Multiple selections poss.
      CHANGING
        file_table              = file_paths                 " Table Holding Selected Files
        rc                      = rc                 " Return Code, Number of Files or -1 If Error Occurred
        user_action             = user_action                 " User Action (See Class Constants ACTION_OK, ACTION_CANCEL)
*        file_encoding           =
      EXCEPTIONS
        file_open_dialog_failed = 1                " "Open File" dialog failed
        cntl_error              = 2                " Control error
        error_no_gui            = 3                " No GUI available
        not_supported_by_gui    = 4                " GUI does not support this
        OTHERS                  = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG rc={ sy-subrc }|.
    ENDIF.
    IF user_action <> cl_gui_frontend_services=>action_ok.
      FREE: file_paths.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_dialogue~save_file.
    DATA:
      filename    TYPE string,
      path        TYPE string,
      user_action TYPE i.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title              = window_title                 " Window Title
*        default_extension         =                  " Default Extension
        default_file_name         = default_name                 " Default File Name
*        with_encoding             =
*        file_filter               =                  " File Type Filter Table
        initial_directory         = initial_directory                 " Initial Directory
*        prompt_on_overwrite       = 'X'
      CHANGING
        filename                  = filename                 " File Name to Save
        path                      = path                 " Path to File
        fullpath                  = file_path                 " Path + File Name
        user_action               = user_action                 " User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)
*        file_encoding             =
      EXCEPTIONS
        cntl_error                = 1                " Control error
        error_no_gui              = 2                " No GUI available
        not_supported_by_gui      = 3                " GUI does not support this
        invalid_default_file_name = 4                " Invalid default file name
        OTHERS                    = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG rc={ sy-subrc }|.
    ENDIF.
    IF user_action <> cl_gui_frontend_services=>action_ok.
      FREE: file_path.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_dialogue~pick_dir.
    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = window_title                  " Title of Browsing Window
        initial_folder       = initial_directory                  " Start Browsing Here
      CHANGING
        selected_folder      = dir_path                 " Folder Selected By User
      EXCEPTIONS
        cntl_error           = 1                " Control error
        error_no_gui         = 2                " No GUI available
        not_supported_by_gui = 3                " GUI does not support this
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_dialogue~get_desktop_dir.
    cl_gui_frontend_services=>get_desktop_directory( CHANGING desktop_directory = dir ).
    cl_gui_cfw=>update_view( ). "Magic line to actually fill desktop value
  ENDMETHOD.
ENDCLASS.
