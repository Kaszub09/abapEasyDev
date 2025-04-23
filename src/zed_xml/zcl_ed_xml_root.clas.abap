"! <p class="shorttext synchronized" lang="en">XML root</p>
"! <br/>TAGS: XML; root
CLASS zcl_ed_xml_root DEFINITION PUBLIC INHERITING FROM zcl_ed_xml_node FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING name TYPE string value TYPE string OPTIONAL namespace TYPE string OPTIONAL
                            version TYPE string DEFAULT '1.0' encoding TYPE string DEFAULT 'utf-8',
      get_xml REDEFINITION.
  PRIVATE SECTION.
    DATA:
        xml_prefix TYPE string.
ENDCLASS.

CLASS zcl_ed_xml_root IMPLEMENTATION.
  METHOD get_xml.
    xml = |{ xml_prefix }{ super->get_xml( ) }|.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( name = name value = value tag_namespace = tag_namespace ).
    xml_prefix = |<?xml|.
    IF strlen( version ) > 0.
      xml_prefix = |{ xml_prefix } version="{ version }"|.
    ENDIF.
    IF strlen( encoding ) > 0.
      xml_prefix = |{ xml_prefix } encoding="{ encoding }"|.
    ENDIF.
    xml_prefix = |{ xml_prefix }?>|.
  ENDMETHOD.

ENDCLASS.
