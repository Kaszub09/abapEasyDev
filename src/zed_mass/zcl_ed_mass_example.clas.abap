"! <p class="shorttext synchronized" lang="en">Example class for mass change</p>
CLASS zcl_ed_mass_example DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_mass.

    METHODS:
      constructor.

  PRIVATE SECTION.
    TYPES:
      "! It's best to declare  table type upfront.
      "! Remember about including "INCLUDE TYPE zif_ed_mass=&gt;t_table_include."
      BEGIN OF t_data,
        some_hidden_index TYPE i,
        matnr             TYPE matnr,
        werks             TYPE werks_d.
        INCLUDE TYPE zif_ed_mass=>t_table_include.
      TYPES:
      END OF t_data,
      tt_data TYPE STANDARD TABLE OF t_data WITH EMPTY KEY.
ENDCLASS.

CLASS zcl_ed_mass_example IMPLEMENTATION.
  METHOD constructor.
    "Documentation could be directly in class or e.g. in WORD from SO72
    zif_ed_mass~config = VALUE #( layout_key = VALUE #( report = sy-repid ) docu = VALUE #( id = 'CL' object = 'ZCL_ED_MASS_EXAMPLE' ) ).
  ENDMETHOD.

  METHOD zif_ed_mass~fill_table_with_db_data.
    "Use zif_ed_selection to allow user to select data.
    "You could also display selection screen build e.g. in function group.
    DATA(tab) = CAST tt_data( table_ref ).
    DATA(sel) = zcl_ed_selection_factory=>create_from_selection( selection = VALUE #(
     ( table = 'MARC' fields = VALUE #( ( field = 'MATNR' ) ( field = 'WERKS' is_selected = abap_true ) ) ) )  ).
    sel->aliases = VALUE #( ( alias = 'MARC_ALIAS' table = 'MARC' ) ).

    "User cancelled, exit.
    IF NOT sel->display( start_column = 1 start_line = 1 ).
      RETURN.
    ENDIF.

    "Don't recreate table, just select data into existing reference.
    DATA(where) = sel->get_as_where_clause( ).
    SELECT * FROM marc AS marc_alias WHERE (where) INTO CORRESPONDING FIELDS OF TABLE @tab->*.
  ENDMETHOD.

  METHOD zif_ed_mass~create_table.
    "Just create table of declared type.
    table_ref = NEW tt_data( ).
  ENDMETHOD.

  METHOD zif_ed_mass~modify_dtti_target.
    "You can modify target before user can import data from file, hide some fields or set as required etc.
    target->set_field_is_hidden( 'SOME_HIDDEN_INDEX' ).
    target->set_field_is_required( 'MATNR' ).
  ENDMETHOD.

  METHOD zif_ed_mass~save.
    "DATA(tab) = CAST tt_data( table_ref ). "Somehow Eclipse has problems with IntelliSense, so just use field-symbols
    "Assign to field symbol for ease of use.
    FIELD-SYMBOLS: <tab> TYPE zcl_ed_mass_example=>tt_data.
    ASSIGN table_ref->* TO <tab>.

    "Do not change table row order, because it may change display order, which can surprise user.
    "Simulate processing - you could even group by some fields and batch process.
    "Most common scenario is BAPI or lock->change->unlock.
    "You can check hidden columns and treat them as fields to skip change.
    "You can also do some verification, e.g. if fields are non-empty.
    LOOP AT <tab> REFERENCE INTO DATA(row).
      DATA(index) = sy-tabix.
      "Progress bar for good measure
      cl_progress_indicator=>progress_indicate( i_text = |Processing record { index }/{ lines( <tab> ) }|
        i_output_immediately = 'X' i_processed = index i_total = lines( <tab> ) ).

      "Processing...

      "Fill result field with information about any errors/successes
      CLEAR: row->result.
      CASE index.
        WHEN 1 OR 6.
          row->result-success = abap_true.
          row->result-msg = |Successfully processed { index }|.

        WHEN 2 OR 3 OR 4.
          row->result-success = abap_false.
          row->result-msg = |Errors { index }|.

        WHEN 5.
          "You can even attach detailed log if necessary
          row->result-logger = zcl_ed_logger_factory=>create_temporary_logger( ).
          row->result-logger->e( 'Error' )->w( 'Warning' )->e( 'Some other error' ).
          row->result-success = xsdbool( NOT row->result-logger->ext-msg->has_errors( ) ).
          row->result-msg = |Details in log... { row->result-logger->ext-msg->get_errors_as_string( length_restriction = 128 ) }|.

        WHEN 7.
          "You can even attach detailed log if necessary
          row->result-logger = zcl_ed_logger_factory=>create_temporary_logger( ).
          row->result-logger->w( 'Warning' )->w( 'Some other warnig' ).
          row->result-success = xsdbool( NOT row->result-logger->ext-msg->has_errors( ) ).
          row->result-msg = |Details in log... { row->result-logger->ext-msg->get_as_string( length_restriction = 128 ) }|.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_ed_mass~modify_alv.
    alv->columns->set_as_hidden( 'SOME_HIDDEN_INDEX' ).
    "Set to get value check against check table
    alv->columns->set_ddic_field( column = 'MATNR' field = 'MATNR' table = 'MARC' ).
  ENDMETHOD.
ENDCLASS.
