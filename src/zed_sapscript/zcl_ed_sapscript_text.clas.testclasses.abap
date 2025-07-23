"For tests to work you must add in SO10:

"   ZED_TEST_1
"   ST
"   EN
"===== with text ====
"   Text1
"   Line1

"===== AND ====

"   ZED_TEST_2
"   ST
"   EN
"   Text2
"   Line2

CLASS tcl_read_texts DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      setup,
      get_texts FOR TESTING,
      get_texts_tab FOR TESTING,
      fill_texts FOR TESTING,
      verify_texts IMPORTING texts TYPE zcl_ed_sapscript_text=>tt_text.

    DATA:
        cut TYPE REF TO zcl_ed_sapscript_text.
ENDCLASS.

CLASS tcl_read_texts IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD get_texts.
    DATA(texts) = cut->get_texts( names = VALUE #( sign = 'I' option = 'EQ' ( low = 'ZED_TEST_1' ) ( low = 'ZED_TEST_2' ) ) ).
    verify_texts( texts ).
  ENDMETHOD.

  METHOD get_texts_tab.
    DATA(tab) = VALUE zcl_ed_sapscript_text=>tt_name_range( ( low = 'ZED_TEST_1' ) ( low = 'ZED_TEST_2' ) ).
    DATA(texts) = cut->get_texts_tab( names_table = tab names_col = 'LOW' ).
    verify_texts( texts ).
  ENDMETHOD.

  METHOD fill_texts.
    DATA(texts) = VALUE zcl_ed_sapscript_text=>tt_text( object = 'TEXT' id = 'ST' lang = 'E' ( name = 'ZED_TEST_1' ) ( name = 'ZED_TEST_2' ) ).
    cut->fill_texts( CHANGING texts = texts  ).
    verify_texts( texts ).
  ENDMETHOD.

  METHOD verify_texts.
    cl_abap_unit_assert=>assert_equals( act = texts[ name = 'ZED_TEST_1' ]-text exp = |Text1{ cl_abap_char_utilities=>newline }Line1| ).
    cl_abap_unit_assert=>assert_equals( act = texts[ name = 'ZED_TEST_1' ]-text_lines[ 1 ]-tdline exp = |Text1| ).
    cl_abap_unit_assert=>assert_equals( act = texts[ name = 'ZED_TEST_1' ]-text_lines[ 2 ]-tdline exp = |Line1| ).

    cl_abap_unit_assert=>assert_equals( act = texts[ name = 'ZED_TEST_2' ]-text exp = |Text2{ cl_abap_char_utilities=>newline }Line2| ).
    cl_abap_unit_assert=>assert_equals( act = texts[ name = 'ZED_TEST_2' ]-text_lines[ 1 ]-tdline exp = |Text2| ).
    cl_abap_unit_assert=>assert_equals( act = texts[ name = 'ZED_TEST_2' ]-text_lines[ 2 ]-tdline exp = |Line2| ).
  ENDMETHOD.
ENDCLASS.
