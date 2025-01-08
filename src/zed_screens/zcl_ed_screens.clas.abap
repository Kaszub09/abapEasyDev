
CLASS zcl_ed_screens DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES:
      "For internal use only
      zif_ed_screens_function_group.

    CONSTANTS:
      "! Screen info in case you want to use it manually inside PBO module of event handler
      BEGIN OF c_screen_info,
        program   TYPE syrepid VALUE 'SAPLZED_SCREENS',
        container TYPE c LENGTH 30 VALUE 'CONTAINER',
        status    TYPE string VALUE 'DYNAMIC_COMMANDS',
        title     TYPE string VALUE 'HEADER',
      END OF c_screen_info.

    CLASS-METHODS:
        class_constructor,
        "! <p class="shorttext synchronized" lang="en">Must be called every time before <em>call_next_screen</em></p>
        "! @parameter handler | <p class="shorttext synchronized" lang="en">Handler to be called in PBO/PAI events.
        "!  If empty default handler is used, which simply exits screen on exit command.</p>
        "! @parameter with_toolbar | <p class="shorttext synchronized" lang="en">Use screen with toolbar (commands supplied via handler)</p>
        "! @parameter create_container | <p class="shorttext synchronized" lang="en">Create container on screen. It's freed after calling screen.
        "!  You can also create container e.g. inside PBO event of handler - then you are responsible for freeing it.</p>
        "! @parameter container | <p class="shorttext synchronized" lang="en">Returns created container or not bound if <em>create_container</em> is false.
        "!  If you want to call screen in popup, you must use <em>get_screen_container</em> inside screen PBO, otherwise it won't be visible.</p>
        prepare_next_screen IMPORTING handler TYPE REF TO zif_ed_screens_handler OPTIONAL
                                      with_toolbar TYPE abap_bool DEFAULT abap_true
                                      create_container TYPE abap_bool DEFAULT abap_true
                                      PREFERRED PARAMETER handler
                            RETURNING VALUE(container) TYPE REF TO cl_gui_container,
        "! <p class="shorttext synchronized" lang="en"><em>prepare_next_screen</em> must be called before</p>
        call_next_screen IMPORTING start_column TYPE i DEFAULT 0 end_column TYPE i DEFAULT 128
                               start_line TYPE i DEFAULT 0 end_line TYPE i DEFAULT 24,
        "! <p class="shorttext synchronized" lang="en">Use inside PBO when calling screen in popup,
        "!  since if you want to use container in popup window, it must be created inside PBO. Otherwise not needed.</p>
        get_screen_container IMPORTING dynnr TYPE dynnr recreate TYPE abap_bool DEFAULT abap_true RETURNING VALUE(container) TYPE REF TO cl_gui_container.

    CLASS-DATA:
        screens_with_toolbar TYPE abap_bool VALUE abap_true.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_screen_info,
        dynnr        TYPE dynnr,
        with_toolbar TYPE abap_bool,
        container    TYPE REF TO cl_gui_container,
        handler      TYPE REF TO zif_ed_screens_handler,
      END OF t_screen_info,
      tt_screen_info TYPE STANDARD TABLE OF t_screen_info WITH EMPTY KEY
        WITH UNIQUE SORTED KEY screen COMPONENTS dynnr,
      tt_dynnr       TYPE STANDARD TABLE OF dynnr WITH EMPTY KEY.

    CLASS-METHODS:
      free_screen IMPORTING screen TYPE t_screen_info,
      pop_next_screen IMPORTING with_toolbar TYPE abap_bool RETURNING VALUE(dynnr) TYPE dynnr.

    CLASS-DATA:
      free_dynnr_with_toolbar    TYPE tt_dynnr,
      free_dynnr_without_toolbar TYPE tt_dynnr,
      screen_stack               TYPE tt_screen_info.
ENDCLASS.

CLASS zcl_ed_screens IMPLEMENTATION.
  METHOD class_constructor.
    "Leave numbers as descending, so lowest one is picked first.
    free_dynnr_with_toolbar = VALUE #( ( '0010' ) ( '0009' ) ( '0008' ) ( '0007' ) ( '0006' ) ( '0005' ) ( '0004' ) ( '0003' ) ( '0002' ) ( '0001' ) ).
    free_dynnr_without_toolbar = VALUE #( ( '0060' ) ( '0059' ) ( '0058' ) ( '0057' ) ( '0056' ) ( '0055' ) ( '0054' ) ( '0053' ) ( '0052' ) ( '0051' ) ).
  ENDMETHOD.

  METHOD call_next_screen.
    DATA(screen) = screen_stack[ lines( screen_stack ) ].

    CALL FUNCTION 'ZED_SCREENS_CALL_SCREEN'
      EXPORTING
        dynnr        = screen-dynnr
        start_column = start_column
        end_column   = end_column
        start_line   = start_line
        end_line     = end_line.

    free_screen( screen ).
  ENDMETHOD.

  METHOD prepare_next_screen.
    DATA(dynnr) = pop_next_screen( with_toolbar ).

    IF create_container = abap_true.
      container = NEW cl_gui_custom_container( repid = c_screen_info-program dynnr = dynnr container_name = c_screen_info-container ).
    ENDIF.

    APPEND VALUE #( dynnr = dynnr with_toolbar = with_toolbar container = container
        handler = COND #( WHEN handler IS BOUND THEN handler ELSE NEW zcl_ed_screens_default_handler( ) ) ) TO screen_stack.
  ENDMETHOD.

  METHOD zif_ed_screens_function_group~raise_pai.
    screen_stack[ KEY screen dynnr = dynnr ]-handler->pai( dynnr = dynnr command = command ).
  ENDMETHOD.

  METHOD zif_ed_screens_function_group~raise_pbo.
    DATA(screen_info) = REF #( screen_stack[ KEY screen dynnr = dynnr ] ).

    IF screen_info->with_toolbar = abap_true.
      DATA(toolbar_commands) = screen_info->handler->pbo_get_toolbar_commands( dynnr ).
      CALL FUNCTION 'ZED_SCREENS_SET_COMMANDS' EXPORTING dynamic_commands = toolbar_commands.
    ENDIF.
    DATA(status_commands_to_exclude) = screen_info->handler->pbo_get_status_commands_to_ex( dynnr ).
    SET PF-STATUS c_screen_info-status OF PROGRAM c_screen_info-program EXCLUDING status_commands_to_exclude.

    DATA(header) = sy-title.
    screen_info->handler->pbo_change_header( EXPORTING dynnr = dynnr CHANGING header = header ).
    SET TITLEBAR c_screen_info-title OF PROGRAM c_screen_info-program WITH header.

    screen_info->handler->pbo( dynnr ).
  ENDMETHOD.

  METHOD free_screen.
    IF screen-container IS BOUND.
      screen-container->free( EXCEPTIONS cntl_error = 1 ).
    ENDIF.

    DATA(dynnr_tab) = COND #( WHEN screen-with_toolbar = abap_true THEN REF tt_dynnr( free_dynnr_with_toolbar ) ELSE REF tt_dynnr( free_dynnr_with_toolbar ) ).
    APPEND screen-dynnr TO dynnr_tab->*.

    DELETE screen_stack USING KEY screen WHERE dynnr = screen-dynnr.
  ENDMETHOD.

  METHOD pop_next_screen.
    DATA(dynnr_tab) = COND #( WHEN with_toolbar = abap_true THEN REF tt_dynnr( free_dynnr_with_toolbar ) ELSE REF tt_dynnr( free_dynnr_without_toolbar ) ).
    dynnr = dynnr_tab->*[ lines( dynnr_tab->* ) ].
    DELETE dynnr_tab->* INDEX lines( dynnr_tab->* ).
  ENDMETHOD.

  METHOD get_screen_container.
    DATA(screen) = REF #( screen_stack[ KEY screen dynnr = dynnr ] ).
    IF recreate = abap_true.
      IF screen->container IS BOUND.
        screen->container->free( EXCEPTIONS cntl_error = 1 ).
      ENDIF.
      screen->container = NEW cl_gui_custom_container( repid = c_screen_info-program dynnr = dynnr container_name = c_screen_info-container ).
    ENDIF.

    container = screen->container.
  ENDMETHOD.

ENDCLASS.
