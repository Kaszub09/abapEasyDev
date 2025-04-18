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
        line->* = substring( val = line->* off = 1 len = strlen( line->* ) - 2 ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
