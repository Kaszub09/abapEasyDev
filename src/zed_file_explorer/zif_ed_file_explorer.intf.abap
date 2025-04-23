"! <p class="shorttext synchronized">File explorer interface</p>
"! <br/>TAGS: file; directory; file explorer; save; load
INTERFACE zif_ed_file_explorer PUBLIC.
  TYPES:
    tt_file_info_ext TYPE STANDARD TABLE OF file_info WITH EMPTY KEY,
    tt_string_table  TYPE STANDARD TABLE OF string WITH EMPTY KEY.

  CONSTANTS:
    BEGIN OF c_cp,
      "! UTF-16LE Unicode / ISO/IEC 10646
      utf16le       TYPE abap_encoding  VALUE '4103',
      "! Unicode UTF-8
      utf8          TYPE abap_encoding  VALUE '4110',
      "! Microsoft 1252, Superset of ISO 8859-1
      microsoft1252 TYPE abap_encoding  VALUE '1160',
      "! Microsoft Windows 1250 for Central Europe
      microsoft1250 TYPE abap_encoding  VALUE '1404',
    END OF c_cp.

  METHODS: "File manipulations
    "! @parameter path | <p class="shorttext synchronized">Could be case sensitive depending on system. Isn't on Windows.</p>
    file_exists IMPORTING path TYPE string RETURNING VALUE(exists) TYPE abap_bool RAISING zcx_ed_exception,
    "! @parameter destination_path | <p class="shorttext synchronized">Existing file is overwritten.</p>
    file_copy IMPORTING source_path TYPE string destination_path TYPE string RAISING zcx_ed_exception,
    "! @parameter destination_path | <p class="shorttext synchronized">Existing file is overwritten.</p>
    file_move IMPORTING source_path TYPE string destination_path TYPE string create_dir_if_needed TYPE abap_bool DEFAULT abap_true RAISING zcx_ed_exception,
    "! <p class="shorttext synchronized">Error is raised if file exists and can't be deleted.</p>
    file_delete_if_exists IMPORTING path TYPE string RAISING zcx_ed_exception,
    file_get_extension IMPORTING path TYPE string in_upper_case TYPE abap_bool DEFAULT abap_true RETURNING VALUE(extension) TYPE string,
    file_get_name IMPORTING path TYPE string with_extension TYPE abap_bool DEFAULT abap_true RETURNING VALUE(name) TYPE string,
    "! @parameter codepage | <p class="shorttext">If left empty SAP should try to detect correct one</p>
    file_read_as_text IMPORTING path TYPE string codepage TYPE abap_encoding DEFAULT space
                      RETURNING VALUE(lines) TYPE tt_string_table
                      RAISING zcx_ed_exception,
    "! @parameter appending | <p class="shorttext synchronized">Append to file if exists instead of overwriting</p>
    "! @parameter write_lf | <p class="shorttext synchronized">Writes a CR/LF after final data record</p>
    file_save_as_text IMPORTING path TYPE string codepage TYPE abap_encoding DEFAULT space
                                appending TYPE abap_bool DEFAULT abap_true write_lf_after_last_line TYPE abap_bool DEFAULT abap_true
                      CHANGING lines TYPE tt_string_table
                      RAISING zcx_ed_exception,
    file_read_as_bin IMPORTING path TYPE string RETURNING VALUE(content) TYPE xstring RAISING zcx_ed_exception,
    "! @parameter appending | <p class="shorttext synchronized">Append to file if exists instead of overwriting</p>
    file_save_as_bin IMPORTING path TYPE string content TYPE xstring appending TYPE abap_bool DEFAULT abap_true RAISING zcx_ed_exception.

  METHODS: "Directory manipulations
    dir_convert_path IMPORTING path TYPE string RETURNING VALUE(dir_path) TYPE string,
    dir_get_parent IMPORTING path TYPE string RETURNING VALUE(dir_path) TYPE string,
    "! @parameter path | <p class="shorttext synchronized">Could be case sensitive depending on system.</p>
    dir_exists IMPORTING path TYPE string RETURNING VALUE(exists) TYPE abap_bool RAISING zcx_ed_exception,
    dir_create IMPORTING path TYPE string RETURNING VALUE(exists) TYPE abap_bool RAISING zcx_ed_exception,
    dir_delete_if_exists IMPORTING path TYPE string RAISING zcx_ed_exception,
    "! <p class="shorttext synchronized">Return data may be incomplete on Application Server.</p>
    dir_get_all_files IMPORTING path TYPE string filter TYPE string DEFAULT '*.*'
                                files_only TYPE abap_bool OPTIONAL  directories_only TYPE abap_bool OPTIONAL
                      RETURNING VALUE(files) TYPE tt_file_info_ext RAISING zcx_ed_exception.

  DATA:
    file_separator TYPE c READ-ONLY.
ENDINTERFACE.
