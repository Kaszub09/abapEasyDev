*"* use this source file for your ABAP unit test classes
CLASS ltcl_cleaner DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      escape_attribute_characters FOR TESTING,
      escape_node_characters FOR TESTING,
      remove_empty_date FOR TESTING,
      remove_empty_time FOR TESTING,
      remove_empty_date_time FOR TESTING,
      remove_empty_nodes FOR TESTING,
      remove_empty_attributes FOR TESTING,
      remove_all FOR TESTING.

ENDCLASS.


CLASS ltcl_cleaner IMPLEMENTATION.
  METHOD escape_attribute_characters.
    DATA(attribute) = |ABC & 123 " "" ' OOO < >|.
    DATA(escaped_attribute) = |ABC &amp; 123 &quot; &quot;&quot; ' OOO &lt; >|.

    attribute = zcl_ed_xml_cleaner=>escape_attribute_value( attribute ).
    cl_abap_unit_assert=>assert_equals( act = attribute exp = escaped_attribute ).
  ENDMETHOD.

  METHOD escape_node_characters.
    DATA(text) = |ABC & 123 " "" ' OOO < >|.
    DATA(escaped_text) = |ABC &amp; 123 " "" ' OOO &lt; >|.

    text = zcl_ed_xml_cleaner=>escape_node_value( text ).
    cl_abap_unit_assert=>assert_equals( act = text exp = escaped_text ).
  ENDMETHOD.

  METHOD remove_empty_attributes.
    DATA(text) = |<node att1="" att2="">value</node><node2 att=""/>|.
    DATA(clean_text) = |<node>value</node><node2/>|.

    text = zcl_ed_xml_cleaner=>remove_empty( xml_string = text attributes = abap_true nodes = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = text exp = clean_text ).
  ENDMETHOD.

  METHOD remove_empty_date.
    DATA(text) = |<node att1="0000-00-00" att2="00:00:00">value</node>|.
    DATA(clean_text) = |<node att1="" att2="00:00:00">value</node>|.

    text = zcl_ed_xml_cleaner=>remove_empty( xml_string = text attributes = abap_false nodes = abap_false date = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = text exp = clean_text ).
  ENDMETHOD.

  METHOD remove_empty_date_time.
    DATA(text) = |<node att1="0000-00-00" att2="0000-00-00T00:00:00">value</node>|.
    DATA(clean_text) = |<node att1="0000-00-00" att2="">value</node>|.

    text = zcl_ed_xml_cleaner=>remove_empty( xml_string = text attributes = abap_false nodes = abap_false date_time = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = text exp = clean_text ).
  ENDMETHOD.

  METHOD remove_empty_nodes.
    DATA(text) = |<node></node><node2 att1=""></node2><node3/>|.
    DATA(clean_text) = |<node2 att1=""></node2>|.

    text = zcl_ed_xml_cleaner=>remove_empty( xml_string = text attributes = abap_false nodes = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = text exp = clean_text ).
  ENDMETHOD.

  METHOD remove_empty_time.
    DATA(text) = |<node att1="0000-00-00" att2="00:00:00">value</node>|.
    DATA(clean_text) = |<node att1="0000-00-00" att2="">value</node>|.

    text = zcl_ed_xml_cleaner=>remove_empty( xml_string = text attributes = abap_false nodes = abap_false time = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = text exp = clean_text ).
  ENDMETHOD.

  METHOD remove_all.
    DATA(text) = |<node att1="0000-00-00" att2="00:00:00" att3="0000-00-00T00:00:00"></node><node att=""/><node></node><node_date>0000-00-00</node_date><result>val</result>|.
    DATA(clean_text) = |<result>val</result>|.

    text = zcl_ed_xml_cleaner=>remove_empty( xml_string = text attributes = abap_true nodes = abap_true date = abap_true time = abap_true date_time = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = text exp = clean_text ).
  ENDMETHOD.

ENDCLASS.
