"! <p class="shorttext synchronized">Date time</p>
"! <br/>TAGS: date; time; formatting; parsing
CLASS zcl_ed_datetime DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_interval_index,
        interval TYPE c LENGTH 1,
        index    TYPE i,
      END OF t_interval_index,
      tt_interval_index TYPE SORTED TABLE OF t_interval_index WITH UNIQUE KEY interval,
      BEGIN OF t_regex_info,
        regex     TYPE string,
        intervals TYPE tt_interval_index,
      END OF t_regex_info.

    CLASS-METHODS:
      class_constructor,
      "! @parameter mask | <p class="shorttext synchronized">Mask to look for in string, will be used in regex,
      "! so special characters must be escaped.
      "! <br/>Supports: d&#124;dd=day; M&#124;M=month; y&#124;yyyy-year; h&#124;hh=hours; m&#124;mm-minutes; s&#124;ss-seconds</p>
      "! <br/>Each component should be present no more than once.
      "! @parameter regex_info | <p class="shorttext synchronized">Can be cached between <em>parse_string</em> calls.</p>
      prepare_regex IMPORTING mask TYPE string RETURNING VALUE(regex_info) TYPE t_regex_info,
      "! <p class="shorttext synchronized">Tries to parse given string to date/time object, if no match is found they are initial</p>
      "! @parameter string | <p class="shorttext synchronized">String with text to parse</p>
      "! @parameter regex_info | <p class="shorttext synchronized">Should be generated via <em>prepare_regex</em>.
      "! Can be cached by caller to avoid recalculating it every time.</p>
      parse_string IMPORTING string TYPE string regex_info TYPE t_regex_info EXPORTING date TYPE d time TYPE t,
      "! <p class="shorttext synchronized">Formats date and time using given mask,</p>
      "! @parameter mask | <p class="shorttext synchronized">Accepts standards denotement - d&#124;dd=day M&#124;MM=month y&#124;yyyy=year h&#124;hh=hours m&#124;mm=minutes s&#124;ss=seconds</p>
      format_date_time IMPORTING mask TYPE string date TYPE d OPTIONAL time TYPE t OPTIONAL RETURNING VALUE(formatted) TYPE string.

  PRIVATE SECTION.
    CLASS-DATA:
              regex_dict TYPE SORTED TABLE OF zted_dictionary WITH UNIQUE KEY k.
ENDCLASS.

CLASS zcl_ed_datetime IMPLEMENTATION.
  METHOD class_constructor.
    INSERT VALUE #( k = 'd' val = '([0-9]{1,2})' ) INTO TABLE regex_dict.
    INSERT VALUE #( k = 'dd' val = '([0-9]{2})'  ) INTO TABLE regex_dict.
    INSERT VALUE #( k = 'M' val = '([0-9]{1,2})' ) INTO TABLE regex_dict.
    INSERT VALUE #( k = 'MM' val = '([0-9]{2})'  ) INTO TABLE regex_dict.
    INSERT VALUE #( k = 'y' val = '([0-9]{1,4})' ) INTO TABLE regex_dict.
    INSERT VALUE #( k = 'yyyy' val = '([0-9]{4})' ) INTO TABLE regex_dict.
    INSERT VALUE #( k = 'h' val = '([0-9]{1,2})' ) INTO TABLE regex_dict.
    INSERT VALUE #( k = 'hh' val = '([0-9]{2})'  ) INTO TABLE regex_dict.
    INSERT VALUE #( k = 'm' val = '([0-9]{1,2})' ) INTO TABLE regex_dict.
    INSERT VALUE #( k = 'mm' val = '([0-9]{2})'  ) INTO TABLE regex_dict.
    INSERT VALUE #( k = 's' val = '([0-9]{1,2})' ) INTO TABLE regex_dict.
    INSERT VALUE #( k = 'ss' val = '([0-9]{2})'  ) INTO TABLE regex_dict.


  ENDMETHOD.

  METHOD prepare_regex.
    "Replace characters in mask to create regex
    "[D]+ is greedy operator, btw. +? for lazy not yet supported
    DATA(matcher) = cl_abap_matcher=>create( pattern = '([d]+|[M]+|[y]+|[h]+|[m]+|[s]+)' text = mask ignore_case = abap_false ).
    DATA(index) = 1.
    WHILE matcher->find_next( ).
      DATA(replacement) = REF #( regex_dict[ k = substring( val = matcher->text off = matcher->get_match( )-offset
                                                                    len = matcher->get_match( )-length ) ] OPTIONAL ).
      IF replacement IS BOUND.
        INSERT VALUE #( interval = replacement->k(1) index = index ) INTO TABLE regex_info-intervals.
        matcher->replace_found( replacement->val ).
        index = index + 1.
      ENDIF.
    ENDWHILE.

    regex_info-regex = matcher->text.
  ENDMETHOD.

  METHOD parse_string.
    DATA(matcher) = cl_abap_matcher=>create( pattern = regex_info-regex text = string ).

    IF NOT matcher->find_next( ).
      RETURN.
    ENDIF.

    " Read pattern and convert to date
    IF line_exists( regex_info-intervals[ interval = 'd' ] ).
      DATA(day) = CONV i( matcher->get_submatch( regex_info-intervals[ interval = 'd' ]-index ) ).
    ENDIF.
    IF line_exists( regex_info-intervals[ interval = 'M' ] ).
      DATA(month) = CONV i( matcher->get_submatch( regex_info-intervals[ interval = 'M' ]-index ) ).
    ENDIF.
    IF line_exists( regex_info-intervals[ interval = 'y' ] ).
      DATA(year) = CONV i( matcher->get_submatch( regex_info-intervals[ interval = 'y' ]-index ) ).
    ENDIF.

    IF line_exists( regex_info-intervals[  interval = 'h' ] ).
      DATA(hour) = CONV i( matcher->get_submatch(  regex_info-intervals[ interval = 'h' ]-index ) ).
    ENDIF.
    IF line_exists( regex_info-intervals[  interval = 'm' ] ).
      DATA(minute) = CONV i( matcher->get_submatch( regex_info-intervals[ interval = 'm' ]-index ) ).
    ENDIF.
    IF line_exists( regex_info-intervals[  interval = 's' ] ).
      DATA(second) = CONV i( matcher->get_submatch( regex_info-intervals[ interval = 's' ]-index ) ).
    ENDIF.

    date = |{ year WIDTH = 4 PAD = '0' ALIGN = RIGHT }{ month WIDTH = 2 PAD = '0' ALIGN = RIGHT }{ day WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
    time = zcl_ed_time=>create( hours = hour minutes = minute seconds = second ).
  ENDMETHOD.

  METHOD format_date_time.
    DATA pattern_dict TYPE SORTED TABLE OF zted_dictionary WITH UNIQUE KEY k.
    INSERT VALUE #( k = 'd' val = |{ zcl_ed_date=>day( date ) }| ) INTO TABLE pattern_dict.
    INSERT VALUE #( k = 'dd' val = |{ zcl_ed_date=>day( date ) ALIGN = RIGHT PAD = '0' WIDTH = 2 }| ) INTO TABLE pattern_dict.
    INSERT VALUE #( k = 'M' val = |{ zcl_ed_date=>month( date ) }| ) INTO TABLE pattern_dict.
    INSERT VALUE #( k = 'MM' val = |{ zcl_ed_date=>month( date ) ALIGN = RIGHT PAD = '0' WIDTH = 2  }| ) INTO TABLE pattern_dict.
    INSERT VALUE #( k = 'y' val = |{ zcl_ed_date=>year( date ) }| ) INTO TABLE pattern_dict.
    INSERT VALUE #( k = 'yyyy' val = |{ zcl_ed_date=>year( date ) ALIGN = RIGHT PAD = '0' WIDTH = 4  }| ) INTO TABLE pattern_dict.
    INSERT VALUE #( k = 'h' val = |{ zcl_ed_time=>hours( time ) }| ) INTO TABLE pattern_dict.
    INSERT VALUE #( k = 'hh' val = |{ zcl_ed_time=>hours( time ) ALIGN = RIGHT PAD = '0' WIDTH = 2  }| ) INTO TABLE pattern_dict.
    INSERT VALUE #( k = 'm' val = |{ zcl_ed_time=>minutes( time ) }| ) INTO TABLE pattern_dict.
    INSERT VALUE #( k = 'mm' val = |{ zcl_ed_time=>minutes( time )  ALIGN = RIGHT PAD = '0' WIDTH = 2  }| ) INTO TABLE pattern_dict.
    INSERT VALUE #( k = 's' val = |{ zcl_ed_time=>seconds( time ) }| ) INTO TABLE pattern_dict.
    INSERT VALUE #( k = 'ss' val = |{ zcl_ed_time=>seconds( time ) ALIGN = RIGHT PAD = '0' WIDTH = 2  }| ) INTO TABLE pattern_dict.

    DATA(matcher) = cl_abap_matcher=>create( pattern = '([d]+|[M]+|[y]+|[h]+|[m]+|[s]+)' text = mask ignore_case = abap_false ).
    WHILE matcher->find_next( ).
      DATA(replacement) = REF #( pattern_dict[ k = substring( val = matcher->text off = matcher->get_match( )-offset
                                                                    len = matcher->get_match( )-length ) ] OPTIONAL ).
      IF replacement IS BOUND.
        matcher->replace_found( replacement->val ).
      ENDIF.
    ENDWHILE.

    formatted = matcher->text.
  ENDMETHOD.
ENDCLASS.
