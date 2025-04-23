"! <p class="shorttext synchronized">XML Attribute</p>
"! <br/>TAGS: XML; attribute;
CLASS zcl_ed_xml_attribute DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    DATA:
      name      TYPE string,
      value     TYPE string,
      namespace TYPE string.

    METHODS:
      constructor IMPORTING name TYPE string value TYPE string OPTIONAL namespace TYPE string OPTIONAL,
      get_xml RETURNING VALUE(xml) TYPE string.
ENDCLASS.

CLASS zcl_ed_xml_attribute IMPLEMENTATION.
  METHOD get_xml.
    xml = |{ name }="{ zcl_ed_xml_cleaner=>escape_attribute_value( value ) }"|.
    IF strlen( namespace ) > 0.
      xml = |{ namespace }:{ xml }|.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    me->name = name.
    me->value = value.
    me->namespace = namespace.
  ENDMETHOD.
ENDCLASS.
