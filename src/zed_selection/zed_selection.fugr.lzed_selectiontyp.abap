TYPES:
  BEGIN OF t_field_selection,
    table       TYPE tabname,
    field       TYPE fieldname,
    is_selected TYPE abap_bool,
    node_key    TYPE  salv_de_node_key,
    range       TYPE RANGE OF rsdslow,
  END OF t_field_selection,
  tt_field_selection TYPE STANDARD TABLE OF t_field_selection WITH EMPTY KEY
      WITH NON-UNIQUE SORTED KEY node COMPONENTS node_key,

  BEGIN OF t_tree_display,
    field TYPE fieldname,
  END OF t_tree_display,
  tt_tree_display TYPE STANDARD TABLE OF t_tree_display WITH EMPTY KEY.
