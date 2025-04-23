CLASS ltcl_conversion DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_simple_table,
        col1 TYPE i,
        col2 TYPE i,
      END OF t_simple_table,
      tt_simple_table TYPE STANDARD TABLE OF t_simple_table WITH DEFAULT KEY,
      tt_string       TYPE TABLE OF string WITH DEFAULT KEY,
      "-----------------------------------------------------------------
      BEGIN OF t_advanced_test_table,
        col_int        TYPE i,
        col_date       TYPE d,
        col_time       TYPE t,
        col_decfloat16 TYPE decfloat16,
        matnr          TYPE matnr,
        meins          TYPE meins,
        lifnr          TYPE lifnr,
        string         TYPE string,
        BEGIN OF substructure,
          col1 TYPE i,
          col2 TYPE d,
        END OF substructure,
        simple_table   TYPE tt_simple_table,
        string_table   TYPE tt_string.
        INCLUDE TYPE t_simple_table AS simple_include.
      TYPES:
      END OF t_advanced_test_table,
      tt_advanced_test_table TYPE STANDARD TABLE OF t_advanced_test_table WITH EMPTY KEY,
      "-----------------------------------------------------------------
      BEGIN OF t_test_table,
        col_int        TYPE i,
        col_date       TYPE d,
        col_time       TYPE t,
        col_decfloat16 TYPE decfloat16,
        matnr          TYPE matnr,
        meins          TYPE meins,
        lifnr          TYPE lifnr,
        string         TYPE string,
        BEGIN OF substructure,
          col1 TYPE i,
          col2 TYPE d,
        END OF substructure.
        INCLUDE TYPE t_simple_table AS simple_include.
      TYPES:
      END OF t_test_table,
      tt_test_table TYPE STANDARD TABLE OF t_test_table WITH EMPTY KEY.

    "-----------------------------------------------------------------
    CLASS-DATA:
      advanced_test_table TYPE tt_advanced_test_table,
      test_table          TYPE tt_test_table.

    DATA:
      cut           TYPE REF TO zcl_ed_table_conversion_csv.  "class under test

    CLASS-METHODS:
      class_setup.

    METHODS:
      setup,
      convert_test_table FOR TESTING,
      convert_advanced_test_table FOR TESTING.
ENDCLASS.

CLASS ltcl_conversion IMPLEMENTATION.
  METHOD class_setup.
    "Initialize test data
    APPEND VALUE #( col_int = 1 col_date = '20230101' col_time = '121315' col_decfloat16 = '122.43' meins = 'ST' lifnr = '1234'
                    string = |OLD LINE{ cl_abap_char_utilities=>newline }NEW_LINE|
                    substructure = VALUE #( col1 = 123 col2 = 32 )
                    simple_table = VALUE #( ( col1 = -1 col2 = -3 ) ( col1 = -1 col2 = -3 ) )
                    string_table = VALUE #( ( |string1| ) ( |string2| ) )
                    col1 = 0 col2 = 1 )
                    TO advanced_test_table.
    APPEND VALUE #( col_int = 4 col_date = '20210101' col_decfloat16 = '-2122.43' matnr = 'MATNR2' meins = 'ST' lifnr = '1234'
                    string = |Quotes:"qweqwe"|
                    substructure = VALUE #( col1 = 123 col2 = 32 )
                    string_table = VALUE #( ( |string1| ) ( |string2| ) )
                    col1 = 0 col2 = 1 )
                    TO advanced_test_table.
    APPEND VALUE #( col_int = 1 col_date = '20230101' col_time = '121315' col_decfloat16 = '122.43' matnr = 'MATNR3' meins = 'ST' lifnr = '1234'
                    substructure = VALUE #( col2 = 32 )
                    simple_table = VALUE #( ( col1 = -1 col2 = -33 ) ( col1 = -1 col2 = -31 ) )
                    string_table = VALUE #( ( |aqwe| ) ( |123 231| )  ( |123 231| ) )
                    col1 = 0 col2 = 1 )
                    TO advanced_test_table.
    test_table = CORRESPONDING #( advanced_test_table ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD convert_advanced_test_table.
    DATA(csv) = cut->convert_it_to_csv( data_table = advanced_test_table ).
    SELECT SINGLE reptext FROM dd04t WHERE rollname = 'MATNR' AND ddlanguage = @sy-langu INTO @DATA(matnr_header).
    SELECT SINGLE reptext FROM dd04t WHERE rollname = 'MEINS' AND ddlanguage = @sy-langu INTO @DATA(meins_header).
    SELECT SINGLE reptext FROM dd04t WHERE rollname = 'LIFNR' AND ddlanguage = @sy-langu INTO @DATA(lifnr_header).
    SELECT SINGLE mseh3 FROM t006a WHERE msehi = 'ST' AND spras = @sy-langu INTO @DATA(meins_value).

    DATA(expected) = |COL_INT;COL_DATE;COL_TIME;COL_DECFLOAT16;{ matnr_header };{ meins_header };{ lifnr_header };STRING;SUBSTRUCTURE-COL1;SUBSTRUCTURE-COL2;SIMPLE_TABLE;STRING_TABLE;COL1;COL2\n| &
                     |1;01.01.2023;12:13:15;122,43;;{ meins_value };1234;"OLD LINE\n| &
                     |NEW_LINE";123;02.02.0001;"COL1;COL2\n| &
                     |-1;-3\n| &
                     |-1;-3\n| &
                     |";"\\TYPE=STRING\n| &
                     |string1\n| &
                     |string2\n| &
                     |";0;1\n| &
                     |4;01.01.2021;00:00:00;-2 122,43;MATNR2;{ meins_value };1234;"Quotes:""qweqwe""";123;02.02.0001;"COL1;COL2\n| &
                     |";"\\TYPE=STRING\n| &
                     |string1\n| &
                     |string2\n| &
                     |";0;1\n| &
                     |1;01.01.2023;12:13:15;122,43;MATNR3;{ meins_value };1234;;0;02.02.0001;"COL1;COL2\n| &
                     |-1;-33\n| &
                     |-1;-31\n| &
                     |";"\\TYPE=STRING\n| &
                     |aqwe\n| &
                     |123 231\n| &
                     |123 231\n| &
                     |";0;1\n|.
    cl_abap_unit_assert=>assert_equals( act = csv exp = expected ).
  ENDMETHOD.

  METHOD convert_test_table.
    DATA(csv) = cut->convert_it_to_csv( data_table = test_table ).
    SELECT SINGLE reptext FROM dd04t WHERE rollname = 'MATNR' AND ddlanguage = @sy-langu INTO @DATA(matnr_header).
    SELECT SINGLE reptext FROM dd04t WHERE rollname = 'MEINS' AND ddlanguage = @sy-langu INTO @DATA(meins_header).
    SELECT SINGLE reptext FROM dd04t WHERE rollname = 'LIFNR' AND ddlanguage = @sy-langu INTO @DATA(lifnr_header).
    SELECT SINGLE mseh3 FROM t006a WHERE msehi = 'ST' AND spras = @sy-langu INTO @DATA(meins_value).

    DATA(expected) = |COL_INT;COL_DATE;COL_TIME;COL_DECFLOAT16;{ matnr_header };{ meins_header };{ lifnr_header };STRING;SUBSTRUCTURE-COL1;SUBSTRUCTURE-COL2;COL1;COL2\n1;01.01.2023;12:13:15;122,43;;{ meins_value };1234;| &&
    |"OLD LINE\nNEW_LINE";123;02.02.0001;0;1\n4;01.01.2021;00:00:00;-2 122,43;MATNR2;{ meins_value };1234;"Quotes:""| &&
    |qweqwe""";123;02.02.0001;0;1\n1;01.01.2023;12:13:15;122,43;MATNR3;{ meins_value };1234;;0;02.02.0001;0;1\n|.
    cl_abap_unit_assert=>assert_equals( act = csv exp = expected ).
  ENDMETHOD.
ENDCLASS.
