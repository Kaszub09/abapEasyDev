"! <p class="shorttext synchronized" lang="en">Logger
"! <br/>TAGS: logger; log</p>
INTERFACE zif_ed_logger PUBLIC.

  TYPES:
    "! Standarised SAP message representation
    BEGIN OF t_sap_msg,
      msgid TYPE msgid,
      msgno TYPE msgno,
      msgty TYPE msgty,
      msgv1 TYPE msgv1,
      msgv2 TYPE msgv2,
      msgv3 TYPE msgv3,
      msgv4 TYPE msgv4,
    END OF t_sap_msg,
    "! Standard log message representation
    BEGIN OF t_log_message,
      level           TYPE zted_log_detail_level,
      "! Detail level
      msg_type        TYPE msgty,
      created_at_date TYPE zted_log_created_at_date,
      created_at_time TYPE zted_log_created_at_time,
      msg             TYPE string,
      "! If msg is JSON (e.g. a table), to display correctly later on
      is_json         TYPE abap_bool,
      "! If it is, sap_msg contains full info. Otherwise, log is just text in msg
      is_sap_msg      TYPE abap_bool,
      sap_msg         TYPE t_sap_msg BOXED,
      "! '&#124' separated, should be same structure (or substructure) as context definition
      context_values  TYPE string,
    END OF t_log_message,
    tt_log_message TYPE STANDARD TABLE OF t_log_message WITH EMPTY KEY,

    BEGIN OF t_log.
      INCLUDE TYPE zed_logs.
    TYPES:
      messages TYPE tt_log_message,
    END OF t_log,
    tt_log TYPE STANDARD TABLE OF t_log WITH EMPTY KEY,

    BEGIN OF t_settings,
      "! Save after adding every log
      autosave                    TYPE abap_bool,
      "! Don't add logs below specified level - can be used to run logs normally or with detail if needed
      logging_level               TYPE zted_log_detail_level,
      "! When exception is added, add also previous exceptions up to this level
      exception_drilldown_level   TYPE i,
      "! Skip initial fields if structure/table is stored as JSON to save space
      compress_json TYPE abap_bool,
      BEGIN OF second_con,
        "! Name of 2nd connection (look for term 'service connection'), should start with 'R/3*[name]'
        name   TYPE string,
        "! Use 2nd connection - useful to save logs even in case of exceptions/rollbacks/dumps
        use    TYPE abap_bool,
        "! Commit 2nd connection after every save
        commit TYPE abap_bool,
      END OF second_con,
    END OF t_settings.

  CONSTANTS:
    "! Guidelines for default logging level
    BEGIN OF c_log_level,
      minimal  TYPE zted_log_detail_level VALUE 1,
      standard TYPE zted_log_detail_level VALUE 3,
      detailed   TYPE zted_log_detail_level VALUE 5,
    END OF c_log_level,
    c_2nd_con_name TYPE string VALUE 'R/3*ZABAP_EASY_DEV_LOGGER',
    "! For easy separation of log lines
    BEGIN OF c_line,
      eq     TYPE  string VALUE '================================================================================================================================',
      hyphen TYPE  string VALUE '--------------------------------------------------------------------------------------------------------------------------------',
      under  TYPE  string VALUE '--------------------------------------------------------------------------------------------------------------------------------',
    END OF c_line.

  METHODS:
    "! @parameter msg_type | <p class="shorttext synchronized" lang="en">Taken from obj if it is SAP standard message</p>
    add IMPORTING obj           TYPE any DEFAULT sy
                  msg_type      TYPE msgty DEFAULT space
                  level         TYPE i DEFAULT c_log_level-standard
                  context_data  TYPE REF TO data OPTIONAL
                    PREFERRED PARAMETER obj
        RETURNING VALUE(logger) TYPE REF TO zif_ed_logger
        RAISING   zcx_ed_exception,
    i IMPORTING obj           TYPE any DEFAULT sy
                level         TYPE i DEFAULT c_log_level-standard
                context_data  TYPE REF TO data OPTIONAL
                  PREFERRED PARAMETER obj
      RETURNING VALUE(logger) TYPE REF TO zif_ed_logger
      RAISING   zcx_ed_exception,
    s IMPORTING obj           TYPE any DEFAULT sy
                level         TYPE i DEFAULT c_log_level-standard
                context_data  TYPE REF TO data OPTIONAL
                  PREFERRED PARAMETER obj
      RETURNING VALUE(logger) TYPE REF TO zif_ed_logger
      RAISING   zcx_ed_exception,
    w IMPORTING obj           TYPE any DEFAULT sy
                level         TYPE i DEFAULT c_log_level-standard
                context_data  TYPE REF TO data OPTIONAL
                  PREFERRED PARAMETER obj
      RETURNING VALUE(logger) TYPE REF TO zif_ed_logger
      RAISING   zcx_ed_exception,
    e IMPORTING obj           TYPE any DEFAULT sy
                level         TYPE i DEFAULT c_log_level-standard
                context_data  TYPE REF TO data OPTIONAL
                  PREFERRED PARAMETER obj
      RETURNING VALUE(logger) TYPE REF TO zif_ed_logger
      RAISING   zcx_ed_exception,
    save RETURNING VALUE(logger) TYPE REF TO zif_ed_logger,
    "! <p class="shorttext synchronized" lang="en">Useful, if you e.g. use log to store and display check during document validation etc.</p>
    clear_all_logs.

  DATA:
    "! Can be changed any time
    settings TYPE REF TO t_settings,
    log      TYPE zif_ed_logger=>t_log READ-ONLY,
    "! Use for conversions between string and context data
    context  TYPE REF TO zcl_ed_logger_context,
    "! Use for conversion between messages internal representation and hex in database representation
    hex      TYPE REF TO zcl_ed_logger_hex.
ENDINTERFACE.
