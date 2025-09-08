"! <p class="shorttext synchronized" lang="en">Logger - messages extension</p>
INTERFACE zif_ed_logger_ext_msg PUBLIC .

  CONSTANTS:
          c_max_alv_line_length TYPE i VALUE 128.

  METHODS:
    has_errors RETURNING VALUE(has_errors) TYPE abap_bool,
    has_warnings RETURNING VALUE(has_warnings) TYPE abap_bool,
    has_warnings_no_errors RETURNING VALUE(has_warnings_no_errors) TYPE abap_bool,
    "! @parameter length_restriction | <p class="shorttext synchronized" lang="en">0 means no restriction</p>
    "! @parameter msgty | <p class="shorttext synchronized" lang="en">space means no filter</p>
    get_as_string IMPORTING length_restriction TYPE i DEFAULT 0 msgty_filter TYPE msgty DEFAULT space RETURNING VALUE(msgs_string) TYPE string,
    "! @parameter length_restriction | <p class="shorttext synchronized" lang="en">0 means no restriction</p>
    get_errors_as_string IMPORTING length_restriction TYPE i DEFAULT 0 RETURNING VALUE(errors_string) TYPE string.
ENDINTERFACE.
