CLASS zcl_ed_file_explorer_as DEFINITION PUBLIC FINAL CREATE PRIVATE
    GLOBAL FRIENDS zcl_ed_file_explorer_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_file_explorer.
    METHODS:
      constructor.
ENDCLASS.

CLASS zcl_ed_file_explorer_as IMPLEMENTATION.
  METHOD constructor.
    zif_ed_file_explorer~file_separator = COND #( WHEN to_upper( sy-opsys ) CP '*WINDOWS*' THEN '\' ELSE '/' ).
  ENDMETHOD.

  METHOD zif_ed_file_explorer~file_exists.
    DATA(directory) = CONV btctext80( zif_ed_file_explorer~dir_get_parent( path ) ).
    DATA(file) = CONV btctext80( zif_ed_file_explorer~file_get_name( path = path with_extension = abap_true ) ).
    CALL FUNCTION 'PFL_CHECK_DIRECTORY'
      EXPORTING
        directory                   = directory
        filname                     = file
      EXCEPTIONS
        pfl_dir_not_exist           = 1                " Directory does not exist
        pfl_permission_denied       = 2                " No write authorization for directory
        pfl_cant_build_dataset_name = 3                " Temporary file cannot be generated
        pfl_file_not_exist          = 4
        pfl_authorization_missing   = 5
        OTHERS                      = 6.
    CASE sy-subrc.
      WHEN 0. exists = abap_true.
      WHEN 1 OR 4. exists = abap_false.
      WHEN OTHERS. RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |PFL_CHECK_DIRECTORY rc={ sy-subrc }|.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~file_copy.
    zif_ed_file_explorer~dir_create( zif_ed_file_explorer~dir_get_parent( destination_path ) ).

    DATA(source_full_path_c) = CONV char255( source_path ).
    DATA(destination_full_path_c) = CONV char255( destination_path ).

    CALL FUNCTION 'SCMS_FILE_COPY'
      EXPORTING
        src_filename = source_full_path_c
        dst_filename = destination_full_path_c
      EXCEPTIONS
        read_failed  = 1
        write_failed = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |SCMS_FILE_COPY rc={ sy-subrc }|.
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

    TRY.
        DELETE DATASET path.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |DELETE DATASET rc={ sy-subrc }|.
        ENDIF.
      CATCH cx_sy_file_authority cx_sy_file_open INTO DATA(cx).
        RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = cx->get_text( ).
    ENDTRY.
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
    TRY.
        IF codepage IS INITIAL.
          OPEN DATASET path FOR INPUT IN TEXT MODE ENCODING DEFAULT WITH SMART LINEFEED.
        ELSE.
          OPEN DATASET path FOR INPUT IN LEGACY  TEXT MODE CODE PAGE codepage WITH SMART LINEFEED.
        ENDIF.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |OPEN DATASET FOR TEXT READ rc={ sy-subrc }|.
        ENDIF.

        DATA(end_of_file) = abap_false.
        DATA(line) = ||.
        WHILE end_of_file = abap_false.
          READ DATASET path INTO line.
          IF sy-subrc <> 0.
            end_of_file = abap_true.
          ELSE.
            APPEND line TO lines.
          ENDIF.
        ENDWHILE.

        CLOSE DATASET path.

      CATCH cx_sy_file_close cx_sy_too_many_files cx_sy_pipes_not_supported cx_sy_file_authority cx_sy_conversion_codepage
        cx_sy_codepage_converter_init cx_sy_file_open INTO DATA(cx).
        CLOSE DATASET path.
        RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = cx->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~file_save_as_text.
    zif_ed_file_explorer~dir_create( zif_ed_file_explorer~dir_get_parent( path ) ).

    TRY.
        IF codepage = space OR codepage = zif_ed_file_explorer~c_cp-utf8.
          IF appending = abap_true.
            OPEN DATASET path FOR APPENDING IN TEXT MODE ENCODING UTF-8.
          ELSE.
            OPEN DATASET path FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
          ENDIF.

        ELSE.
          IF appending = abap_true.
            OPEN DATASET path FOR APPENDING IN LEGACY TEXT MODE CODE PAGE codepage.
          ELSE.
            OPEN DATASET path FOR OUTPUT IN LEGACY TEXT MODE CODE PAGE codepage.
          ENDIF.
        ENDIF.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |OPEN DATASET FOR TEXT WRITE rc={ sy-subrc }|.
        ENDIF.

        LOOP AT lines REFERENCE INTO DATA(line).
          TRANSFER line->* TO path.
        ENDLOOP.

        CLOSE DATASET path.

      CATCH cx_sy_file_close cx_sy_too_many_files cx_sy_pipes_not_supported cx_sy_file_authority cx_sy_conversion_codepage
        cx_sy_codepage_converter_init cx_sy_file_open INTO DATA(cx).
        CLOSE DATASET path.
        RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = cx->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~file_read_as_bin.
    TRY.
        OPEN DATASET path FOR INPUT IN BINARY MODE.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |OPEN DATASET FOR BIN READ rc={ sy-subrc }|.
        ENDIF.

        READ DATASET path INTO content.

        CLOSE DATASET path.

      CATCH cx_sy_file_close cx_sy_too_many_files cx_sy_pipes_not_supported cx_sy_file_authority cx_sy_conversion_codepage
        cx_sy_codepage_converter_init cx_sy_file_open INTO DATA(cx).
        CLOSE DATASET path.
        RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = cx->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~file_save_as_bin.
    zif_ed_file_explorer~dir_create( zif_ed_file_explorer~dir_get_parent( path ) ).

    TRY.
        IF appending = abap_true.
          OPEN DATASET path FOR APPENDING IN BINARY MODE.
        ELSE.
          OPEN DATASET path FOR OUTPUT IN BINARY MODE.
        ENDIF.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |OPEN DATASET FOR BIN WRITE rc={ sy-subrc }|.
        ENDIF.

        TRANSFER content TO path.

        CLOSE DATASET path.

      CATCH cx_sy_file_close cx_sy_too_many_files cx_sy_pipes_not_supported cx_sy_file_authority cx_sy_conversion_codepage
        cx_sy_codepage_converter_init cx_sy_file_open INTO DATA(cx).
        CLOSE DATASET path.
        RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = cx->get_text( ).
    ENDTRY.
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
    DATA(directory) = CONV char128( path ).
    CALL FUNCTION 'PFL_CHECK_DIRECTORY'
      EXPORTING
        directory_long              = directory
      EXCEPTIONS
        pfl_dir_not_exist           = 1                " Directory does not exist
        pfl_permission_denied       = 2                " No write authorization for directory
        pfl_cant_build_dataset_name = 3                " Temporary file cannot be generated
        pfl_file_not_exist          = 4
        pfl_authorization_missing   = 5
        OTHERS                      = 6.
    CASE sy-subrc.
      WHEN 0. exists = abap_true.
      WHEN 1. exists = abap_false.
      WHEN OTHERS. RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |PFL_CHECK_DIRECTORY rc={ sy-subrc }|.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~dir_create.
    IF zif_ed_file_explorer~dir_exists( path ).
      RETURN.
    ENDIF.
    RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |Not implemented|.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~dir_delete_if_exists.
    IF NOT zif_ed_file_explorer~dir_exists( path ).
      RETURN.
    ENDIF.
    RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |Not implemented|.
  ENDMETHOD.

  METHOD zif_ed_file_explorer~dir_get_all_files.
    DATA(directory_c) = CONV pfeflnamel( path ).
    DATA file_tbl TYPE STANDARD TABLE OF salfldir.

    CALL FUNCTION 'RZL_READ_DIR_LOCAL'
      EXPORTING
        name               = directory_c                " Dataset name
      TABLES
        file_tbl           = file_tbl                  " Lines
      EXCEPTIONS
        argument_error     = 1                " Incorrect call
        not_found          = 2                " File not found
        no_admin_authority = 3
        OTHERS             = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |RZL_READ_DIR_LOCAL rc={ sy-subrc }|.
    ENDIF.

    DELETE file_tbl WHERE name = '.' OR name = '..'.
    files = CORRESPONDING #( file_tbl MAPPING filename = name  filelength  = size ).
  ENDMETHOD.
ENDCLASS.
