"! <p class="shorttext synchronized">Example class for mass change. Run ZED_MASS with this class.</p>
CLASS zcl_ed_mass_example DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_ed_mass.

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

    METHODS:
      function_save IMPORTING selected_rows TYPE lvc_t_row.

    CONSTANTS:
      BEGIN OF c_add_functions,
        show_msg TYPE ui_func VALUE 'SHOW_MSG',
      END OF c_add_functions.

    DATA:
        mass_callback TYPE REF TO zif_ed_mass_callback.
ENDCLASS.

CLASS zcl_ed_mass_example IMPLEMENTATION.
  METHOD zif_ed_mass~setup.
    config-layout_key = VALUE #( report = sy-repid ). "Needed so separate programs have separate layouts

    "Documentation could be directly in class or e.g. in WORD from SO72
    config-docu = VALUE #( id = 'CL' object = 'ZCL_ED_MASS_EXAMPLE' ).

    "You can add more function there, or delete/edit standard ones (e.g. disable read from DB or change icon/description of SAVE function)
    APPEND VALUE #( function = c_add_functions-show_msg text = TEXT-001 icon = '@W0@' ) TO config-functions.
    config-functions[ function = zif_ed_mass=>c_functions-save ]-text = 'SUPER SAVE'.

    "Grab the class so later on we can ask about selected rows, hidden columns,
    mass_callback = callback.
  ENDMETHOD.

  METHOD zif_ed_mass~create_table.
    "Just create table of defined type, basically copy/paste everytime
    table_ref = NEW tt_data( ).
  ENDMETHOD.

  METHOD zif_ed_mass~modify_alv.
    alv->columns->set_as_hidden( 'SOME_HIDDEN_INDEX' ).
    "Set to get value check against check table
    alv->columns->set_ddic_field( column = 'MATNR' field = 'MATNR' table = 'MARC' ).
  ENDMETHOD.

  METHOD zif_ed_mass~modify_dtti_target.
    "You can modify target before user can import data from file, hide some fields or set as required etc.
    target->set_field_is_hidden( 'SOME_HIDDEN_INDEX' ).
    target->set_field_is_required( 'MATNR' ).
  ENDMETHOD.

  METHOD zif_ed_mass~fill_table_with_db_data.
    "You can use zif_ed_selection to allow user to select data,
    "or display selection screen build e.g. in function group.
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

  METHOD zif_ed_mass~function.
    CASE function.
      WHEN zif_ed_mass=>c_functions-save.
        IF ( NOT mass_callback->are_rows_selected( ) ) OR ( NOT mass_callback->ask_confirm_save( ) ).
          RETURN.
        ENDIF.
        function_save( mass_callback->get_selected_rows( ) ).
        mass_callback->refresh_alv( ). "Remember to refresh ALV

      WHEN c_add_functions-show_msg.
        MESSAGE TEXT-001 TYPE 'I'.
    ENDCASE.
  ENDMETHOD.

  METHOD function_save.
    DATA(tab) = CAST tt_data( mass_callback->get_table_ref( ) ).

    "Do not change table row order, because it may change display order, which can surprise user.
    "Simulate processing - you could even group by some fields and batch process.
    "Most common scenario is BAPI or lock->change->unlock.
    "You can check hidden columns and treat them as fields to skip change.
    "You can also do some verification, e.g. if fields are non-empty.
    "Usually you work on selected rows only
    LOOP AT selected_rows REFERENCE INTO DATA(selected_row).
      DATA(row) = REF t_data( tab->*[ selected_row->index ] ).

      DATA(index) = sy-tabix.
      "Progress bar for good measure - can affect performance when refreshed too frequently
      cl_progress_indicator=>progress_indicate( i_text = |Processing record { index }/{ lines( selected_rows ) }|
        i_output_immediately = 'X' i_processed = index i_total = lines( selected_rows ) ).

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
          row->result-success = xsdbool( NOT row->result-logger->ext->msg->has_errors( ) ).
          row->result-msg = |Details in log... { row->result-logger->ext->msg->get_errors_as_string( length_restriction = 128 ) }|.

        WHEN 7.
          "You can even attach detailed log if necessary
          row->result-logger = zcl_ed_logger_factory=>create_temporary_logger( ).
          row->result-logger->w( 'Warning' )->w( 'Some other warnig' ).
          row->result-success = xsdbool( NOT row->result-logger->ext->msg->has_errors( ) ).
          row->result-msg = |Details in log... { row->result-logger->ext->msg->get_as_string( length_restriction = 128 ) }|.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
