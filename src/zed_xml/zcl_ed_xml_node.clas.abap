"! <p class="shorttext synchronized">XML node</p>
"! <br/> TAGS: XML; node
CLASS zcl_ed_xml_node DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_attribute,
        name      TYPE string,
        attribute TYPE REF TO zcl_ed_xml_attribute,
      END OF t_attribute,
      tt_attribute TYPE STANDARD TABLE OF t_attribute WITH EMPTY KEY WITH UNIQUE SORTED KEY name COMPONENTS name,
      BEGIN OF t_namespace,
        name  TYPE string,
        value TYPE string,
      END OF t_namespace,
      tt_namespace TYPE STANDARD TABLE OF t_namespace WITH EMPTY KEY WITH UNIQUE SORTED KEY name COMPONENTS name,
      tt_node      TYPE STANDARD TABLE OF REF TO zcl_ed_xml_node WITH EMPTY KEY.

    CLASS-METHODS:
      create_from_date IMPORTING name TYPE string date TYPE d tag_namespace TYPE string OPTIONAL
                       RETURNING VALUE(node) TYPE REF TO zcl_ed_xml_node,
      create_from_time IMPORTING name TYPE string time TYPE t tag_namespace TYPE string OPTIONAL
                       RETURNING VALUE(node) TYPE REF TO zcl_ed_xml_node,
      create_from_date_time IMPORTING name TYPE string date TYPE d time TYPE t tag_namespace TYPE string OPTIONAL
                            RETURNING VALUE(node) TYPE REF TO zcl_ed_xml_node.

    METHODS:
      constructor IMPORTING name TYPE string value TYPE string OPTIONAL tag_namespace TYPE string OPTIONAL,
      append_attribute IMPORTING attribute TYPE REF TO zcl_ed_xml_attribute RAISING cx_sy_itab_duplicate_key,
      append_attribute_alt IMPORTING name TYPE string value TYPE string OPTIONAL namespace TYPE string OPTIONAL RAISING cx_sy_itab_duplicate_key,
      append_child_alt IMPORTING name TYPE string value TYPE string OPTIONAL tag_namespace TYPE string OPTIONAL,
      get_xml RETURNING VALUE(xml) TYPE string.

    DATA:
      name          TYPE string,
      value         TYPE string,
      tag_namespace TYPE string,
      attributes    TYPE tt_attribute,
      children      TYPE tt_node,
      namespaces    TYPE tt_namespace.


ENDCLASS.

CLASS zcl_ed_xml_node IMPLEMENTATION.
  METHOD create_from_date.
    node = NEW #( name = name value = |{ date(4) }-{ date+4(2) }-{ date+6(2) }| tag_namespace = tag_namespace ).
  ENDMETHOD.

  METHOD create_from_time.
    node = NEW #( name = name value = |{ time(2) }:{ time+2(2) }:{ time+4(2) }| tag_namespace = tag_namespace ).
  ENDMETHOD.

  METHOD create_from_date_time.
    node = NEW #( name = name value = |{ date(4) }-{ date+4(2) }-{ date+6(2) }T{ time(2) }:{ time+2(2) }:{ time+4(2) }| tag_namespace = tag_namespace ).
  ENDMETHOD.

  METHOD constructor.
    me->name = name.
    me->value = value.
    me->tag_namespace = tag_namespace.
  ENDMETHOD.

  METHOD append_attribute.
    APPEND VALUE #( name = attribute->name attribute = attribute ) TO attributes.
  ENDMETHOD.

  METHOD append_attribute_alt.
    APPEND VALUE #( name = name attribute = NEW #( name = name value = value namespace = namespace  ) ) TO attributes.
  ENDMETHOD.

  METHOD append_child_alt.
    APPEND NEW #( name = name value = value tag_namespace = tag_namespace ) TO children.
  ENDMETHOD.

  METHOD get_xml.
    "Tag name
    DATA(tag) = COND #( WHEN strlen( tag_namespace ) > 0 THEN |{ tag_namespace }:{ name }| ELSE |{ name }| ).
    xml = |<{ tag }|.
    "Namespaces
    LOOP AT me->namespaces REFERENCE INTO DATA(namespace).
      DATA(namespace_xml) = |xmlns:{ namespace->name }="{ namespace->value }"|.
      xml = |{ xml } { namespace_xml }|.
    ENDLOOP.
    "Attributes
    LOOP AT me->attributes REFERENCE INTO DATA(attribute).
      DATA(attribute_xml) = attribute->attribute->get_xml( ).
      xml = |{ xml } { attribute_xml }|.
    ENDLOOP.

    "Close tag.
    IF strlen( value ) = 0 AND lines( me->children ) = 0.
      xml = |{ xml }/>|.
    ELSE.
      xml = |{ xml }>|.
      "Value
      IF strlen( value ) > 0.
        DATA(escaped_xml) = zcl_ed_xml_cleaner=>escape_node_value( value ).
        xml = |{ xml }{ escaped_xml }|.
      ENDIF.
      "Child nodes
      LOOP AT me->children REFERENCE INTO DATA(child).
        DATA(child_xml) = child->*->get_xml( ).
        xml = |{ xml }{ child_xml }|.
      ENDLOOP.
      "close tag
      xml = |{ xml }</{ tag }>|.
    ENDIF.
  ENDMETHOD.



ENDCLASS.
