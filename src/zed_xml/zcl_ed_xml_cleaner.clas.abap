"! <p class="shorttext synchronized">XML cleaner</p>
"! <br/> TAGS: XML; cleaner; escape;
CLASS zcl_ed_xml_cleaner DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
    class_constructor,
    "! <p class="shorttext synchronized">Cleans xml of empty tags and/or attributes.</p>
    "! @parameter attributes | <p class="shorttext synchronized">Remove empty (attribute="") attributes</p>
    "! @parameter nodes | <p class="shorttext synchronized">Remove empty (&lt;node&gt;&lt;/node&gt; or &lt;node/&gt;) nodes or not</p>
    "! @parameter zero_times_and_dates | <p class="shorttext synchronized">Remove attributes and nodes with empty date (0000-00-00) and/or time (00:00:00).
    "! Don't use if 00:00:00 is valid time.</p>
    "! @parameter date | <p class="shorttext synchronized" lang="en">Remove empty date (0000-00-00)</p>
    "! @parameter time | <p class="shorttext synchronized" lang="en">Remove empty time (00:00:00)</p>
    "! @parameter date_time | <p class="shorttext synchronized" lang="en">Remove empty date time (0000-00-00T00:00:00)</p>
    remove_empty IMPORTING xml_string TYPE string attributes TYPE abap_bool DEFAULT abap_true nodes TYPE abap_bool DEFAULT abap_true
                           date TYPE abap_bool DEFAULT abap_false time TYPE abap_bool DEFAULT abap_false date_time TYPE abap_bool DEFAULT abap_false
                 RETURNING VALUE(processed_xml_string) TYPE string,
    escape_node_value IMPORTING value TYPE string RETURNING VALUE(escaped_xml) TYPE string,
    escape_attribute_value IMPORTING value TYPE string RETURNING VALUE(escaped_xml) TYPE string.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_escape_chara,
        chara   TYPE c LENGTH 1,
        escaped TYPE string,
      END OF t_escape_chara,
      tt_escape_chara TYPE STANDARD TABLE OF t_escape_chara WITH EMPTY KEY.

    CLASS-DATA:
      text_escape_characters      TYPE tt_escape_chara,
      attribute_escape_characters TYPE tt_escape_chara.
ENDCLASS.

CLASS zcl_ed_xml_cleaner IMPLEMENTATION.
  METHOD class_constructor.
    "& must be first to avoid escaping in in escape characters
    INSERT VALUE #( chara = '&' escaped = '&amp;' ) INTO TABLE text_escape_characters.
    INSERT VALUE #( chara = '<' escaped = '&lt;' ) INTO TABLE text_escape_characters.

    INSERT VALUE #( chara = '&' escaped = '&amp;' ) INTO TABLE attribute_escape_characters.
    INSERT VALUE #( chara = '<' escaped = '&lt;' ) INTO TABLE attribute_escape_characters.
    INSERT VALUE #( chara = '"' escaped = '&quot;' ) INTO TABLE attribute_escape_characters.
  ENDMETHOD.

  METHOD remove_empty.
    processed_xml_string = xml_string.

    DATA:
      regex_datetime_attr TYPE string,
      regex_datetime_node TYPE string.

    IF date = abap_true.
      regex_datetime_attr = |\|"0000-00-00"|.
      regex_datetime_node = |\|>0000-00-00<|.
    ENDIF.

    IF time = abap_true.
      regex_datetime_attr = |{ regex_datetime_attr }\|"00:00:00"|.
      regex_datetime_node = |{ regex_datetime_node }\|>00:00:00<|.
    ENDIF.

    IF date_time = abap_true.
      regex_datetime_attr = |{ regex_datetime_attr }\|"0000-00-00T00:00:00"|.
      regex_datetime_node = |{ regex_datetime_node }\|>0000-00-00T00:00:00<|.
    ENDIF.

    IF strlen( regex_datetime_attr ) > 0.
      regex_datetime_attr = substring( val = regex_datetime_attr off = 1 len = strlen( regex_datetime_attr ) - 1 ).
      regex_datetime_node = substring( val = regex_datetime_node off = 1 len = strlen( regex_datetime_node ) - 1 ).
      processed_xml_string = replace( val = processed_xml_string regex = regex_datetime_attr with = '""' occ = 0 ).
      processed_xml_string = replace( val = processed_xml_string regex = regex_datetime_node with = '><' occ = 0 ).
    ENDIF.

    IF attributes = abap_true.
      processed_xml_string = replace( val = processed_xml_string regex = | [^<>\\\\\\/"=\\s]+=""| with = `` occ = 0  ).
    ENDIF.

    IF nodes = abap_true.
      DATA(empty_node_pattern) = |<[^<>="\\s\\/\\\\]+\\/>\|<[^<>="\\s\\/\\\\]+><\\/[^<>="\\s\\/\\\\]+>|.
      WHILE cl_abap_matcher=>contains( pattern = empty_node_pattern text = processed_xml_string ) = abap_true.
        processed_xml_string = replace( val = processed_xml_string regex = empty_node_pattern with = `` occ = 0  ).
      ENDWHILE.
    ENDIF.
  ENDMETHOD.

  METHOD escape_node_value.
    "TODO - skip escaping in CDATA
    escaped_xml = value.
    LOOP AT text_escape_characters REFERENCE INTO DATA(escape_character).
      escaped_xml = replace( val = escaped_xml occ = 0 sub = escape_character->chara with = escape_character->escaped ).
    ENDLOOP.
  ENDMETHOD.

  METHOD escape_attribute_value.
    escaped_xml = value.
    LOOP AT attribute_escape_characters REFERENCE INTO DATA(escape_character).
      escaped_xml = replace( val = escaped_xml occ = 0 sub = escape_character->chara with = escape_character->escaped ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
