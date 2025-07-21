    TYPES:
      BEGIN OF t_struct,
        object   TYPE trobjtype,
        obj_name TYPE sobj_name,
        object2  TYPE trobjtype,
        dec      TYPE decfloat34,
        int      TYPE i,
      END OF t_struct.

    "=================================================================
    "-----------------------------------------------------------------
    CLASS tcl_context_conversion DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

      PRIVATE SECTION.
        METHODS:
          setup,
          conv_data_to_string FOR TESTING,
          conv_partial_data_to_string FOR TESTING,
          conv_string_to_data FOR TESTING,
          conv_incomplete_string_to_data FOR TESTING,
          store_restore FOR TESTING,
          raise_err_if_not_exists.

        DATA:
          context_data   TYPE t_struct,
          context_string TYPE string,
          cut            TYPE REF TO zcl_ed_logger_context.
    ENDCLASS.

    CLASS tcl_context_conversion IMPLEMENTATION.
      METHOD setup.
        cut = zcl_ed_logger_context=>create_from_ref( NEW t_struct( ) ).
        context_data = VALUE t_struct( object = 'R3TR' obj_name = 'TEST' object2 = 'XX' dec = '123456.789' int = 1 ).
        context_string = |R3TR{ cut->c_value_delimiter }TEST{ cut->c_value_delimiter }XX{ cut->c_value_delimiter }123456.789{ cut->c_value_delimiter }1|.
      ENDMETHOD.

      METHOD conv_data_to_string.
        cl_abap_unit_assert=>assert_equals( act = cut->convert_data_to_string( REF #( context_data ) ) exp = context_string ).
      ENDMETHOD.

      METHOD conv_partial_data_to_string.
        TYPES:
          BEGIN OF t_partial_struct,
            obj_name TYPE sobj_name,
          END OF t_partial_struct.

        DATA(context_data) = CORRESPONDING t_partial_struct( context_data ).
        DATA(string_data) = cut->convert_data_to_string( REF #( context_data ) ).

        DATA(exp) = |{ cut->c_value_delimiter }TEST{ cut->c_value_delimiter }{ cut->c_value_delimiter }{ cut->c_value_delimiter }|.
        cl_abap_unit_assert=>assert_equals( act = string_data exp = exp ).
      ENDMETHOD.

      METHOD conv_string_to_data.
        DATA(context_data_ref) = cut->convert_string_to_data( context_string ).
        ASSIGN context_data_ref->* TO FIELD-SYMBOL(<context_data>).
        cl_abap_unit_assert=>assert_equals( act = <context_data> exp = context_data ).
      ENDMETHOD.

      METHOD conv_incomplete_string_to_data.
        DATA(context_data_ref) = cut->convert_string_to_data( |{ cut->c_value_delimiter }TEST{ cut->c_value_delimiter }| ).
        ASSIGN context_data_ref->* TO FIELD-SYMBOL(<context_data>).
        cl_abap_unit_assert=>assert_equals( act = <context_data> exp = VALUE t_struct( obj_name = 'TEST' ) ).
      ENDMETHOD.

      METHOD store_restore.
        DATA(context_data_ref) = cut->convert_string_to_data( cut->convert_data_to_string( REF #( context_data ) ) ).
        ASSIGN context_data_ref->* TO FIELD-SYMBOL(<context_data>).
        cl_abap_unit_assert=>assert_equals( act = <context_data> exp = context_data ).
      ENDMETHOD.
      METHOD raise_err_if_not_exists.
        cut = zcl_ed_logger_context=>create_empty( ).
        TRY.
            cut->convert_string_to_data( || ).
            cl_abap_unit_assert=>fail( |Expected errors| ).
          CATCH zcx_ed_exception.
        ENDTRY.
        TRY.
            cut->convert_data_to_string( REF #( context_data )  ).
            cl_abap_unit_assert=>fail( |Expected errors| ).
          CATCH zcx_ed_exception.
        ENDTRY.
      ENDMETHOD.

    ENDCLASS.

    "=================================================================
    "-----------------------------------------------------------------
    CLASS tcl_context_storing DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

      PRIVATE SECTION.
        METHODS:
          setup,
          store_restore FOR TESTING,
          store_restore_empty FOR TESTING.

        DATA:
          context_data   TYPE t_struct,
          context_string TYPE string,
          cut            TYPE REF TO zcl_ed_logger_context.
    ENDCLASS.

    CLASS tcl_context_storing IMPLEMENTATION.
      METHOD setup.
        cut = zcl_ed_logger_context=>create_from_ref( NEW t_struct( ) ).
        context_data = VALUE t_struct( object = 'R3TR' obj_name = 'TEST' object2 = 'XX' dec = '123456.789' int = 1 ).
        context_string = |R3TR{ cut->c_value_delimiter }TEST{ cut->c_value_delimiter }XX{ cut->c_value_delimiter }123456.789{ cut->c_value_delimiter }1|.
      ENDMETHOD.

      METHOD store_restore.
        cl_abap_unit_assert=>assert_true( cut->exists( ) ).

        cut = zcl_ed_logger_context=>create_from_col_info( cut->get_context_col_info( ) ).
        cl_abap_unit_assert=>assert_true( cut->exists( ) ).

        DATA(context_data_ref) = cut->convert_string_to_data( context_string ).
        ASSIGN context_data_ref->* TO FIELD-SYMBOL(<context_data>).
        cl_abap_unit_assert=>assert_equals( act = <context_data> exp = context_data ).
      ENDMETHOD.

      METHOD store_restore_empty.
        cut = zcl_ed_logger_context=>create_empty( ).
        cl_abap_unit_assert=>assert_false( cut->exists( ) ).

        cut = zcl_ed_logger_context=>create_from_col_info( cut->get_context_col_info( ) ).
        cl_abap_unit_assert=>assert_false( cut->exists( ) ).
      ENDMETHOD.
    ENDCLASS.
