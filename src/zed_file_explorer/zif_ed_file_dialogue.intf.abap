"! <p class="shorttext synchronized">File dialogue interface</p>
"! <br/>TAGS: file; directory; file explorer; dialogue
INTERFACE zif_ed_file_dialogue PUBLIC.
  TYPES:
    tt_file_types TYPE STANDARD TABLE OF i WITH EMPTY KEY,
    tt_file_paths TYPE STANDARD TABLE OF file_table WITH EMPTY KEY.

  CONSTANTS:
    BEGIN OF c_file_type,
      all  TYPE i VALUE 0,
      txt  TYPE i VALUE 1,
      csv  TYPE i VALUE 2,
      xlsx TYPE i VALUE 3,
      xml  TYPE i VALUE 4,
      pdf  TYPE i VALUE 5,
    END OF c_file_type.

  METHODS:
    get_file_filter IMPORTING file_types TYPE tt_file_types RETURNING VALUE(file_filter) TYPE string,
    pick_file IMPORTING window_title TYPE string OPTIONAL file_filter TYPE string OPTIONAL initial_directory TYPE string OPTIONAL
              RETURNING VALUE(file_path) TYPE string
              RAISING zcx_ed_exception,
    pick_files IMPORTING window_title TYPE string OPTIONAL file_filter TYPE string OPTIONAL initial_directory TYPE string OPTIONAL
              RETURNING VALUE(file_paths) TYPE tt_file_paths
              RAISING zcx_ed_exception,
    save_file IMPORTING window_title TYPE string OPTIONAL default_name TYPE string OPTIONAL initial_directory TYPE string OPTIONAL
              RETURNING VALUE(file_path) TYPE string
              RAISING zcx_ed_exception,
    pick_dir IMPORTING window_title TYPE string OPTIONAL initial_directory TYPE string OPTIONAL
             RETURNING VALUE(dir_path) TYPE string
             RAISING zcx_ed_exception,
    get_desktop_dir RETURNING VALUE(dir) TYPE string.
ENDINTERFACE.
