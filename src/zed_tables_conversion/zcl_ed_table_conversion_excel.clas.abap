"! <p class="shorttext synchronized" lang="en">Table conversion - excel</p>
"! <br/>TAGS; Excel; export; import; table; conversion
CLASS zcl_ed_table_conversion_excel DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      tt_string TYPE ANY TABLE OF string,
      BEGIN OF t_wks_data,
        name           TYPE string,
        internal_table TYPE REF TO data,
      END OF t_wks_data,
      tt_wks_data TYPE STANDARD TABLE OF t_wks_data WITH EMPTY KEY.

    CLASS-METHODS:
      "! <p class="shorttext synchronized"> Each sheet is converted to table, where each line is structure with components A, B, C etc. (column names in Excel).
      "! <br/> Dates are formatted as YYYY-MM-DD.
      "! <br/> Numbers are without spaces, and decimal point is dot.
      "! @parameter filename | <p class="shorttext synchronized">Can be xlsx or xlsm.</p>
      import_from_excel IMPORTING filename TYPE string RETURNING VALUE(tables) TYPE tt_wks_data,
      "! <p class="shorttext synchronized" lang="en">Uses standard salv container for export. Formats date etc. Flattens substructures. Recommended.
      "! @parameter filename | <p class="shorttext synchronized" lang="en">Must have 'XLSX' extension.</p>
      "! @parameter data_table | <p class="shorttext synchronized" lang="en">Accepts table lines with substructures. Subtables are skipped.</p>
      export_to_xlsx IMPORTING filename TYPE string headers_from_ddic TYPE abap_bool DEFAULT abap_true
                        fields_to_skip TYPE tt_string OPTIONAL CHANGING data_table TYPE ANY TABLE.
ENDCLASS.

CLASS zcl_ed_table_conversion_excel IMPLEMENTATION.
  METHOD import_from_excel.
    "Upload and parse excel file
    DATA it_bin_data TYPE w3mimetabtype.

    cl_gui_frontend_services=>gui_upload( EXPORTING filename = filename filetype = 'BIN' CHANGING data_tab = it_bin_data ).
    DATA(file_as_xstring) = cl_bcs_convert=>solix_to_xstring( it_bin_data ).
    DATA(excel) = NEW cl_fdt_xl_spreadsheet( document_name = filename  xdocument = file_as_xstring ).

    "Read worksheets
    excel->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = DATA(worksheets) ).
    LOOP AT worksheets REFERENCE INTO DATA(wks).
      APPEND VALUE #( name = wks->* internal_table = excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( wks->* ) ) TO tables.
    ENDLOOP.
  ENDMETHOD.

  METHOD export_to_xlsx.
    cl_salv_table=>factory( IMPORTING r_salv_table = DATA(salv) CHANGING t_table = data_table ).

    "Update columns if needed, medium text is visible after export.
    LOOP AT salv->get_columns( )->get( ) REFERENCE INTO DATA(col).
      IF strlen( col->r_column->get_medium_text( ) ) = 0 OR headers_from_ddic = abap_false.
        col->r_column->set_medium_text( CONV #( col->columnname ) ).
      ENDIF.

      IF NOT fields_to_skip IS INITIAL.
        col->r_column->set_visible( xsdbool( NOT line_exists( fields_to_skip[ table_line = col->columnname ] ) ) ).
      ENDIF.
    ENDLOOP.

    "Export
    DATA(lv_xml_bytes) = salv->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx ).
    cl_scp_change_db=>xstr_to_xtab( EXPORTING im_xstring = lv_xml_bytes IMPORTING ex_size = DATA(lv_size) ex_xtab = DATA(it_raw_data) ).
    cl_gui_frontend_services=>gui_download( EXPORTING filename = filename filetype = 'BIN' bin_filesize = lv_size CHANGING data_tab = it_raw_data ).
  ENDMETHOD.
ENDCLASS.
