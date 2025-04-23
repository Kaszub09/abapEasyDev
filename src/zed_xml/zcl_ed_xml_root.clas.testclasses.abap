*"* use this source file for your ABAP unit test classes
CLASS ltcl_sample_xml DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_sample_xml FOR TESTING.

ENDCLASS.


CLASS ltcl_sample_xml IMPLEMENTATION.


  METHOD test_sample_xml.
    "XML from https://learn.microsoft.com/en-us/previous-versions/windows/desktop/ms762271(v=vs.85)
    DATA(root) = NEW zcl_ed_xml_root( name = 'catalog' encoding = space ).

    DATA(book) = NEW zcl_ed_xml_node( 'book' ).
    book->append_child_alt( name = 'author' value ='Gambardella, Matthew' ).
    book->append_attribute_alt( name = 'id' value ='bk101' ).
    book->append_child_alt( name = 'title' value = |XML Developer's Guide| ).
    book->append_child_alt( name = 'genre' value = 'Computer' ).
    book->append_child_alt( name = 'price' value = '44.95' ).
    APPEND zcl_ed_xml_node=>create_from_date( name = 'publish_date' date = '20001001' ) TO book->children.
    book->append_child_alt( name = 'description' value = 'An in-depth look at creating applications with XML.' ).
    APPEND book TO root->children.

    DATA(book2) = NEW zcl_ed_xml_node( 'book' ).
    book2->append_child_alt( name = 'author' value ='Ralls, Kim' ).
    book2->append_attribute_alt( name = 'id' value ='bk102' ).
    book2->append_child_alt( name = 'title' value = |Midnight Rain| ).
    book2->append_child_alt( name = 'genre' value = 'Fantasy' ).
    book2->append_child_alt( name = 'price' value = '5.95' ).
    APPEND zcl_ed_xml_node=>create_from_date( name = 'publish_date' date = '20001216' ) TO book2->children.
    book2->append_child_alt( name = 'description' value = 'A former architect battles corporate zombies, an evil sorceress, and her own childhood to become queen of the world.' ).
    APPEND book2 TO root->children.

    DATA(expected_xml) = |<?xml version="1.0"?><catalog><book id="bk101"><author>Gambardella, Matthew</author><title>XML Developer's Guide</title><genre>Computer</genre><price>44.95</price><publish_date>2000-10-01</publish_date><description>An in-de| &&
|pth look at creating applications with XML.</description></book><book id="bk102"><author>Ralls, Kim</author><title>Midnight Rain</title><genre>Fantasy</genre><price>5.95</price><publish_date>2000-12-16</publish_date><description>A former architect | &&
|battles corporate zombies, an evil sorceress, and her own childhood to become queen of the world.</description></book></catalog>|.

    cl_abap_unit_assert=>assert_equals( act = root->get_xml( ) exp = expected_xml ).
  ENDMETHOD.

ENDCLASS.
