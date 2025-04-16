CLASS zcl_ed_file_explorer_gui DEFINITION PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS zcl_ed_file_explorer_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_file_explorer.

    METHODS:
      constructor.
ENDCLASS.

CLASS zcl_ed_file_explorer_gui IMPLEMENTATION.
  METHOD constructor.
    cl_gui_frontend_services=>get_file_separator(
      CHANGING
        file_separator       = zif_ed_file_explorer~file_separator
      EXCEPTIONS
        not_supported_by_gui = 1
        error_no_gui         = 2
        cntl_error           = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CL_GUI_FRONTEND_SERVICES=>GET_FILE_SEPARATOR rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~file_exists.
    cl_gui_frontend_services=>file_exist(
       EXPORTING
         file                 = path                  " File to Check
       RECEIVING
         result               = exists
       EXCEPTIONS
         cntl_error           = 1                " Control error
         error_no_gui         = 2                " Error: No GUI
         wrong_parameter      = 3                " Incorrect parameter
         not_supported_by_gui = 4                " GUI does not support this
         OTHERS               = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CL_GUI_FRONTEND_SERVICES=>FILE_EXIST rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~file_copy.
    zif_ed_file_explorer~dir_create( zif_ed_file_explorer~dir_get_parent( destination_path ) ).

    cl_gui_frontend_services=>file_copy(
      EXPORTING
        source               = source_path                 " Source
        destination          = destination_path                 " Destination
        overwrite            = abap_true            " Overrides if Destination Exists
      EXCEPTIONS
        cntl_error           = 1                " Control error
        error_no_gui         = 2                " No GUI Available
        wrong_parameter      = 3                " Incorrect parameter
        disk_full            = 4                " Disk full
        access_denied        = 5                " Access Denied to Source or Destination File
        file_not_found       = 6                " Source File not Found
        destination_exists   = 7                " Destination Already Exists
        unknown_error        = 8                " Unknown error
        path_not_found       = 9                " Path to Which You Want to Copy File(s) Does not Exist
        disk_write_protect   = 10               " Disk Is Write-Protected
        drive_not_ready      = 11               " Disk drive not ready
        not_supported_by_gui = 12               " GUI does not support this
        OTHERS               = 13 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CL_GUI_FRONTEND_SERVICES=>FILE_COPY rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~file_move.
    zif_ed_file_explorer~file_copy( source_path = source_path destination_path = destination_path ).
    zif_ed_file_explorer~file_delete_if_exists( source_path ).
  ENDMETHOD.

  METHOD zif_ed_file_explorer~file_delete_if_exists.
    IF NOT zif_ed_file_explorer~file_exists( path ).
      RETURN.
    ENDIF.

    DATA rc TYPE i. "What's the point of RC in function since we can check subrc anyway?
    cl_gui_frontend_services=>file_delete(
      EXPORTING
        filename             = path                     " Name of the file to be deleted
      CHANGING
        rc                   = rc               " Return Code
      EXCEPTIONS
        file_delete_failed   = 1                " Could not delete file
        cntl_error           = 2                " Control error
        error_no_gui         = 3                " Error: No GUI
        file_not_found       = 4                " File not found
        access_denied        = 5                " Access denied
        unknown_error        = 6                " Unknown error
        not_supported_by_gui = 7                " GUI does not support this
        wrong_parameter      = 8                " Wrong parameter
        OTHERS               = 9 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CL_GUI_FRONTEND_SERVICES=>FILE_DELETE rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~file_get_extension.
    DATA(last_dot_index) = find( val = path sub = '.' occ = -1 ). "0-based
    extension = COND #( WHEN last_dot_index < 0 THEN ||
                        ELSE substring( val = path off = last_dot_index + 1 len = strlen( path ) - last_dot_index - 1 ) ).
    IF in_upper_case = abap_true.
      extension = to_upper( extension ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~file_get_name.
    DATA(last_file_separator_index) = find( val = path sub = zif_ed_file_explorer~file_separator occ = -1 ). "0-based
    name = substring(  val = path off = last_file_separator_index + 1  len = strlen( path ) - last_file_separator_index - 1  ).

    IF with_extension = abap_false AND name CA '.'.
      name = substring( val = name len = find( val = name sub = '.' occ = -1 ) ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~file_read_as_text.
    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = path             " Name of file
        filetype                = 'ASC'            " File Type (ASCII, Binary)
*        has_field_separator     = space            " Columns Separated by Tabs in Case of ASCII Upload
*        header_length           = 0                " Length of Header for Binary Data
*        read_by_line            = space              " File Written Line-By-Line to the Internal Table
*        dat_mode                = space            " Numeric and date fields are in DAT format in WS_DOWNLOAD
        codepage                = codepage                 " Character Representation for Output
*        ignore_cerr             = abap_true        " Ignore character set conversion errors?
*        replacement             = '#'              " Replacement Character for Non-Convertible Characters
*        virus_scan_profile      =                  " Virus Scan Profile
*      IMPORTING
*        filelength              =                  " File Length
*        header                  =                  " File Header in Case of Binary Upload
      CHANGING
        data_tab                = lines                 " Transfer table for file contents
*        isscanperformed         = space            " File already scanned
      EXCEPTIONS
        file_open_error         = 1                " File does not exist and cannot be opened
        file_read_error         = 2                " Error when reading file
        no_batch                = 3                " Cannot execute front-end function in background
        gui_refuse_filetransfer = 4                " Incorrect front end or error on front end
        invalid_type            = 5                " Incorrect parameter FILETYPE
        no_authority            = 6                " No upload authorization
        unknown_error           = 7                " Unknown error
        bad_data_format         = 8                " Cannot Interpret Data in File
        header_not_allowed      = 9                " Invalid header
        separator_not_allowed   = 10               " Invalid separator
        header_too_long         = 11               " Header information currently restricted to 1023 bytes
        unknown_dp_error        = 12               " Error when calling data provider
        access_denied           = 13               " Access to File Denied
        dp_out_of_memory        = 14               " Not enough memory in data provider
        disk_full               = 15               " Storage medium is full.
        dp_timeout              = 16               " Data provider timeout
        not_supported_by_gui    = 17               " GUI does not support this
        error_no_gui            = 18               " GUI not available
        OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~file_save_as_text.
    zif_ed_file_explorer~dir_create( zif_ed_file_explorer~dir_get_parent( path ) ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
*        bin_filesize              =                      " File length for binary files
        filename                  = path                     " Name of file
        filetype                  = 'ASC'                " File type (ASCII, binary ...)
        append                    = appending                " Character Field of Length 1
*        write_field_separator     = space                " Separate Columns by Tabs in Case of ASCII Download
*        header                    = '00'                 " Byte Chain Written to Beginning of File in Binary Mode
*        trunc_trailing_blanks     = space                " Do not Write Blank at the End of Char Fields
*        write_lf                  = 'X'                  " Insert CR/LF at End of Line in Case of Char Download
*        col_select                = space                " Copy Only Selected Columns of the Table
*        col_select_mask           = space                " Vector Containing an 'X' for the Column To Be Copied
*        dat_mode                  = space                " Numeric and date fields are in DAT format in WS_DOWNLOAD
*        confirm_overwrite         = space                " Overwrite File Only After Confirmation
*        no_auth_check             = space                " Switch off Check for Access Rights
        codepage                  =  codepage                    " Character Representation for Output
*        ignore_cerr               = abap_true            " Ignore character set conversion errors?
*        replacement               = '#'                  " Replacement Character for Non-Convertible Characters
*        write_bom                 = space                " If set, writes a Unicode byte order mark
*        trunc_trailing_blanks_eol = 'X'                  " Remove Trailing Blanks in Last Column
*        wk1_n_size                = space
*        wk1_t_format              = space
*        wk1_t_size                = space
*        show_transfer_status      = 'X'                  " Enables suppression of transfer status message
*        fieldnames                =                      " Table Field Names
        write_lf_after_last_line   = write_lf_after_last_line                  " Writes a CR/LF after final data record
*        virus_scan_profile        = '/SCET/GUI_DOWNLOAD' " Virus Scan Profile
*      IMPORTING
*        filelength                =                      " Number of bytes transferred
      CHANGING
        data_tab                  = lines                     " Transfer table
      EXCEPTIONS
        file_write_error          = 1                    " Cannot write to file
        no_batch                  = 2                    " Cannot execute front-end function in background
        gui_refuse_filetransfer   = 3                    " Incorrect Front End
        invalid_type              = 4                    " Invalid value for parameter FILETYPE
        no_authority              = 5                    " No Download Authorization
        unknown_error             = 6                    " Unknown error
        header_not_allowed        = 7                    " Invalid header
        separator_not_allowed     = 8                    " Invalid separator
        filesize_not_allowed      = 9                    " Invalid file size
        header_too_long           = 10                   " Header information currently restricted to 1023 bytes
        dp_error_create           = 11                   " Cannot create DataProvider
        dp_error_send             = 12                   " Error Sending Data with DataProvider
        dp_error_write            = 13                   " Error Writing Data with DataProvider
        unknown_dp_error          = 14                   " Error when calling data provider
        access_denied             = 15                   " Access to File Denied
        dp_out_of_memory          = 16                   " Not enough memory in data provider
        disk_full                 = 17                   " Storage medium is full.
        dp_timeout                = 18                   " Data provider timeout
        file_not_found            = 19                   " Could not find file
        dataprovider_exception    = 20                   " General Exception Error in DataProvider
        control_flush_error       = 21                   " Error in Control Framework
        not_supported_by_gui      = 22                   " GUI does not support this
        error_no_gui              = 23                   " GUI not available
        OTHERS                    = 24 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~file_read_as_bin.
    DATA:
      solix_tab  TYPE solix_tab,
      filelength TYPE i.

    cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename                = path             " Name of file
      filetype                = 'BIN'            " File Type (ASCII, Binary)
*        has_field_separator     = space            " Columns Separated by Tabs in Case of ASCII Upload
*        header_length           = 0                " Length of Header for Binary Data
*        read_by_line            = space              " File Written Line-By-Line to the Internal Table
*        dat_mode                = space            " Numeric and date fields are in DAT format in WS_DOWNLOAD
*        codepage                =                  " Character Representation for Output
*        ignore_cerr             = abap_true        " Ignore character set conversion errors?
*        replacement             = '#'              " Replacement Character for Non-Convertible Characters
*        virus_scan_profile      =                  " Virus Scan Profile
      IMPORTING
        filelength              = filelength                 " File Length
*        header                  =                  " File Header in Case of Binary Upload
    CHANGING
      data_tab                = solix_tab                 " Transfer table for file contents
*        isscanperformed         = space            " File already scanned
    EXCEPTIONS
      file_open_error         = 1                " File does not exist and cannot be opened
      file_read_error         = 2                " Error when reading file
      no_batch                = 3                " Cannot execute front-end function in background
      gui_refuse_filetransfer = 4                " Incorrect front end or error on front end
      invalid_type            = 5                " Incorrect parameter FILETYPE
      no_authority            = 6                " No upload authorization
      unknown_error           = 7                " Unknown error
      bad_data_format         = 8                " Cannot Interpret Data in File
      header_not_allowed      = 9                " Invalid header
      separator_not_allowed   = 10               " Invalid separator
      header_too_long         = 11               " Header information currently restricted to 1023 bytes
      unknown_dp_error        = 12               " Error when calling data provider
      access_denied           = 13               " Access to File Denied
      dp_out_of_memory        = 14               " Not enough memory in data provider
      disk_full               = 15               " Storage medium is full.
      dp_timeout              = 16               " Data provider timeout
      not_supported_by_gui    = 17               " GUI does not support this
      error_no_gui            = 18               " GUI not available
      OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD rc={ sy-subrc }|.
    ENDIF.

    content = cl_bcs_convert=>solix_to_xstring( it_solix = solix_tab iv_size = filelength ).
  ENDMETHOD.

  METHOD zif_ed_file_explorer~file_save_as_bin.
    zif_ed_file_explorer~dir_create( zif_ed_file_explorer~dir_get_parent( path ) ).

    DATA(solix_tab) = cl_bcs_convert=>xstring_to_solix( content ).
    cl_gui_frontend_services=>gui_download(
       EXPORTING
         bin_filesize              = xstrlen( content )                     " File length for binary files
         filename                  = path                     " Name of file
         filetype                  = 'BIN'                " File type (ASCII, binary ...)
         append                    = appending                " Character Field of Length 1
*        write_field_separator     = space                " Separate Columns by Tabs in Case of ASCII Download
*        header                    = '00'                 " Byte Chain Written to Beginning of File in Binary Mode
*        trunc_trailing_blanks     = space                " Do not Write Blank at the End of Char Fields
*        write_lf                  = 'X'                  " Insert CR/LF at End of Line in Case of Char Download
*        col_select                = space                " Copy Only Selected Columns of the Table
*        col_select_mask           = space                " Vector Containing an 'X' for the Column To Be Copied
*        dat_mode                  = space                " Numeric and date fields are in DAT format in WS_DOWNLOAD
*        confirm_overwrite         = space                " Overwrite File Only After Confirmation
*        no_auth_check             = space                " Switch off Check for Access Rights
*        codepage                  =                      " Character Representation for Output
*        ignore_cerr               = abap_true            " Ignore character set conversion errors?
*        replacement               = '#'                  " Replacement Character for Non-Convertible Characters
*        write_bom                 = space                " If set, writes a Unicode byte order mark
*        trunc_trailing_blanks_eol = 'X'                  " Remove Trailing Blanks in Last Column
*        wk1_n_size                = space
*        wk1_t_format              = space
*        wk1_t_size                = space
*        show_transfer_status      = 'X'                  " Enables suppression of transfer status message
*        fieldnames                =                      " Table Field Names
*        write_lf_after_last_line  = 'X'                  " Writes a CR/LF after final data record
*        virus_scan_profile        = '/SCET/GUI_DOWNLOAD' " Virus Scan Profile
*      IMPORTING
*        filelength                =                      " Number of bytes transferred
       CHANGING
         data_tab                  = solix_tab                     " Transfer table
       EXCEPTIONS
         file_write_error          = 1                    " Cannot write to file
         no_batch                  = 2                    " Cannot execute front-end function in background
         gui_refuse_filetransfer   = 3                    " Incorrect Front End
         invalid_type              = 4                    " Invalid value for parameter FILETYPE
         no_authority              = 5                    " No Download Authorization
         unknown_error             = 6                    " Unknown error
         header_not_allowed        = 7                    " Invalid header
         separator_not_allowed     = 8                    " Invalid separator
         filesize_not_allowed      = 9                    " Invalid file size
         header_too_long           = 10                   " Header information currently restricted to 1023 bytes
         dp_error_create           = 11                   " Cannot create DataProvider
         dp_error_send             = 12                   " Error Sending Data with DataProvider
         dp_error_write            = 13                   " Error Writing Data with DataProvider
         unknown_dp_error          = 14                   " Error when calling data provider
         access_denied             = 15                   " Access to File Denied
         dp_out_of_memory          = 16                   " Not enough memory in data provider
         disk_full                 = 17                   " Storage medium is full.
         dp_timeout                = 18                   " Data provider timeout
         file_not_found            = 19                   " Could not find file
         dataprovider_exception    = 20                   " General Exception Error in DataProvider
         control_flush_error       = 21                   " Error in Control Framework
         not_supported_by_gui      = 22                   " GUI does not support this
         error_no_gui              = 23                   " GUI not available
         OTHERS                    = 24 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~dir_convert_path.
    IF strlen( path ) > 0 AND substring( val = path off = strlen( path ) - 1 len = 1 ) <> zif_ed_file_explorer~file_separator.
      dir_path = |{ path }{ zif_ed_file_explorer~file_separator }|.
    ELSE.
      dir_path = path.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~dir_get_parent.
    DATA(last_file_separator_index) = find( val = path sub = zif_ed_file_explorer~file_separator occ = -1 ). "0-based
    IF last_file_separator_index > 0.
      dir_path = substring( val = path off = 0 len = last_file_separator_index + 1  ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~dir_exists.
    cl_gui_frontend_services=>directory_exist(
      EXPORTING
        directory            = path                  " Directory name
      RECEIVING
        result               = exists                 " Result
      EXCEPTIONS
        cntl_error           = 1                " Control error
        error_no_gui         = 2                " No GUI available
        wrong_parameter      = 3                " Incorrect parameter
        not_supported_by_gui = 4                " GUI does not support this
        OTHERS               = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~dir_create.
    IF zif_ed_file_explorer~dir_exists( path ).
      RETURN.
    ENDIF.

    DATA(dir_parent) = zif_ed_file_explorer~dir_get_parent( path ).
    IF strlen( dir_parent ) > 0 AND zif_ed_file_explorer~dir_exists( dir_parent ) = abap_false.
      zif_ed_file_explorer~dir_create( dir_parent ).
    ENDIF.

    DATA rc TYPE i.
    cl_gui_frontend_services=>directory_create(
      EXPORTING
        directory                = path                 " Directory name
      CHANGING
        rc                       = rc                 " Return Code
      EXCEPTIONS
        directory_create_failed  = 1                " Could not create directory
        cntl_error               = 2                " A Control Error Occurred
        error_no_gui             = 3                " No GUI available
        directory_access_denied  = 4                " Access denied
        directory_already_exists = 5                " Directory already exists
        path_not_found           = 6                " Path does not exist
        unknown_error            = 7                " Unknown error
        not_supported_by_gui     = 8                " GUI does not support this
        wrong_parameter          = 9                " Wrong parameter
        OTHERS                   = 10 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~dir_delete_if_exists.
    IF NOT zif_ed_file_explorer~dir_exists( path ).
      RETURN.
    ENDIF.

    DATA rc TYPE i.
    cl_gui_frontend_services=>directory_delete(
      EXPORTING
        directory               = path                 " Directory to Delete
      CHANGING
        rc                      = rc                 " Return Code
      EXCEPTIONS
        directory_delete_failed = 1                " Could not delete directory
        cntl_error              = 2                " Control error
        error_no_gui            = 3                " No GUI available
        path_not_found          = 4                " Path not found
        directory_access_denied = 5                " Access denied
        unknown_error           = 6                " Unknown error
        not_supported_by_gui    = 7                " GUI does not support this
        wrong_parameter         = 8                " Wrong parameter
        OTHERS                  = 9 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CL_GUI_FRONTEND_SERVICES=>DIRECTORY_DELETE rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~dir_get_all_files.
    DATA count TYPE i.

    cl_gui_frontend_services=>directory_list_files_ext(
      EXPORTING
        directory                   = path                 " Directory To Search
        filter                      = filter            " File filter
        files_only                  = files_only                 " Return only Files, no Directories
        directories_only            = directories_only                 " Return only Directories, no Files
      CHANGING
        file_table                  = files                 " Return Table for the Found Files
        count                       = count                 " Number of Files/Dir Found
      EXCEPTIONS
        cntl_error                  = 1                " Control error
        directory_list_files_failed = 2                " Could not list files in the directory
        wrong_parameter             = 3                " Incorrect parameter combination
        error_no_gui                = 4                " No GUI available
        not_supported_by_gui        = 5                " GUI does not support this
        OTHERS                      = 6 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES_EXT rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
