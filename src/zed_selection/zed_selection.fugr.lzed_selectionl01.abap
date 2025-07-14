CLASS lcl_descriptions DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      get_description IMPORTING table TYPE tabname field TYPE fieldname OPTIONAL RETURNING VALUE(description) TYPE lvc_value,
      clear_descriptions_cache.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_descriptions,
        table       TYPE tabname,
        field       TYPE fieldnam,
        description TYPE string,
      END OF t_descriptions,
      tt_descriptions TYPE SORTED TABLE OF t_descriptions WITH UNIQUE KEY table field.

    DATA:
        descriptions TYPE tt_descriptions.
ENDCLASS.

CLASS lcl_descriptions IMPLEMENTATION.
  METHOD get_description.
    DATA(description_ref) = REF #( descriptions[ table = table field = field ] OPTIONAL ).
    IF description_ref IS NOT BOUND.
      INSERT VALUE #( table = table field = field ) INTO TABLE descriptions REFERENCE INTO description_ref.

      IF strlen( field ) = 0.
        description_ref->description = table.
        SELECT SINGLE ddtext FROM dd02t WHERE tabname = @table INTO @description_ref->description.

      ELSE.
        description_ref->description = field.
        SELECT SINGLE FROM dd03l
            LEFT JOIN dd03t ON dd03t~tabname = dd03l~tabname AND dd03t~ddlanguage = @sy-langu
                            AND dd03t~as4local = dd03l~as4local AND dd03t~fieldname = dd03l~fieldname
            LEFT JOIN dd04t ON dd04t~rollname = dd03l~rollname AND dd04t~ddlanguage = @sy-langu AND dd04t~as4local = dd03l~as4local
        FIELDS CASE WHEN dd04t~rollname IS NULL THEN dd03t~ddtext ELSE dd04t~ddtext END AS text
        WHERE dd03l~tabname = @table AND dd03l~fieldname = @field
        INTO @description_ref->description.
      ENDIF.
    ENDIF.

    description = description_ref->description.
  ENDMETHOD.

  METHOD clear_descriptions_cache.
    FREE: descriptions.
  ENDMETHOD.
ENDCLASS.
