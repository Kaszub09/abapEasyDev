CLASS zcl_ed_program_runner DEFINITION PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS zcl_ed_program_runner_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_program_runner.

  PRIVATE SECTION.
    METHODS:
      import_list CHANGING list TYPE zif_ed_program_runner=>tt_list RAISING zcx_ed_exception.
ENDCLASS.

CLASS zcl_ed_program_runner IMPLEMENTATION.
  METHOD zif_ed_program_runner~run_with_list.
    CALL FUNCTION 'LIST_FREE_MEMORY'.

    SUBMIT (program_name)
    USING SELECTION-SET variant_name
    WITH SELECTION-TABLE selection_table
    EXPORTING LIST TO MEMORY AND RETURN.

    import_list( CHANGING list = list ).
  ENDMETHOD.

  METHOD import_list.
    DATA listobject TYPE STANDARD TABLE OF abaplist WITH EMPTY KEY.

    CALL FUNCTION 'LIST_FROM_MEMORY'
      TABLES
        listobject = listobject
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |LIST_FROM_MEMORY rc={ sy-subrc }|.
    ENDIF.
    CALL FUNCTION 'LIST_FREE_MEMORY'.

    CALL FUNCTION 'LIST_TO_ASCI'
      EXPORTING
        with_line_break    = 'X'
      IMPORTING
        list_string_ascii  = list
      TABLES
        listobject         = listobject
      EXCEPTIONS
        empty_list         = 1                " The list object transported is empty
        list_index_invalid = 2                " Invalid list index (LIST_INDEX)
        OTHERS             = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ed_exception EXPORTING custom_message = |LIST_TO_ASCI rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_program_runner~clean_list.
    DELETE list INDEX lines( list ).
    DELETE list FROM 1 TO rows_to_remove_from_start.

    IF remove_first_last_char = abap_true.
      LOOP AT list REFERENCE INTO DATA(line).
        IF strlen( line->* ) >= 2.
          line->* = substring( val = line->* off = 1 len = strlen( line->* ) - 2 ).
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ed_program_runner~run_with_data_ref.
    cl_salv_bs_runtime_info=>set( display = abap_false metadata = abap_true data = abap_true  ).

    SUBMIT (program_name)
    USING SELECTION-SET variant_name
    WITH SELECTION-TABLE selection_table
    EXPORTING LIST TO MEMORY AND RETURN.

    result-metadata = cl_salv_bs_runtime_info=>get_metadata( ).
    cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = result-table_ref ).
    cl_salv_bs_runtime_info=>clear_all( ).
  ENDMETHOD.

  METHOD zif_ed_program_runner~convert_data_ref.
    "create SALV
    converted-table_ref = data_ref_result-table_ref.
    ASSIGN converted-table_ref->* TO FIELD-SYMBOL(<table>).
    cl_salv_table=>factory( IMPORTING r_salv_table = converted-salv CHANGING t_table = <table> ).

    "columns
    LOOP AT data_ref_result-metadata-t_fcat REFERENCE INTO DATA(col).
      converted-salv->get_columns( )->get_column( col->fieldname )->set_technical( col->tech ).
      converted-salv->get_columns( )->get_column( col->fieldname )->set_visible( xsdbool( col->no_out = abap_false ) ).
      IF col->do_sum IS NOT INITIAL.
        converted-salv->get_aggregations( )->add_aggregation( columnname = col->fieldname
           "Check CL_GUI_ALV_GRID, search &AVERAGE for those letter. Like SAP couldn't just fucking include them in DO_SUM description.
           aggregation = COND #( WHEN col->do_sum = 'X' THEN if_salv_c_aggregation=>total
                                 WHEN col->do_sum = 'C' THEN if_salv_c_aggregation=>average
                                 WHEN col->do_sum = 'B' THEN if_salv_c_aggregation=>minimum
                                 WHEN col->do_sum = 'A' THEN if_salv_c_aggregation=>maximum ) ).
      ENDIF.
    ENDLOOP.

    "filters
    LOOP AT data_ref_result-metadata-t_filter REFERENCE INTO DATA(filter).
      converted-salv->get_filters( )->add_filter( columnname = filter->fieldname sign = filter->sign option = filter->option
        low = filter->low high = filter->high ).
    ENDLOOP.

    "sorts
    DATA(sort_copy) = data_ref_result-metadata-t_sort.
    SORT sort_copy BY spos.
    LOOP AT sort_copy REFERENCE INTO DATA(sort).
      converted-salv->get_sorts( )->add_sort( columnname = sort->fieldname position = CONV #( sort->spos )
        subtotal = sort->subtot
        obligatory = sort->obligatory
        sequence = COND #( WHEN sort->up = abap_true THEN if_salv_c_sort=>sort_up
                           WHEN sort->down = abap_true THEN if_salv_c_sort=>sort_down
                           ELSE if_salv_c_sort=>sort_none ) ).
      IF sort->subtot = abap_true AND sort->expa = abap_true.
        converted-salv->get_sorts( )->set_compressed_subtotal( sort->fieldname ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_ed_program_runner~export_data_ref.
    DATA(result_tab) = cl_salv_ex_util=>factory_result_data_table( r_data = data_ref_result-table_ref
        t_fieldcatalog = data_ref_result-metadata-t_fcat t_sort = data_ref_result-metadata-t_sort
        t_filter = data_ref_result-metadata-t_filter ).

    cl_salv_bs_lex=>export_from_result_data_table( EXPORTING is_format = file_format ir_result_data_table = result_tab
          IMPORTING er_result_file = exported ).
  ENDMETHOD.

ENDCLASS.
