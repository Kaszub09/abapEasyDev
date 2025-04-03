*&---------------------------------------------------------------------*
*& Report zed_screens_example_call
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_screens_ex_display_few_scr.

"Table with data to display something
TYPES:
  BEGIN OF t_table,
    description TYPE c LENGTH 20,
  END OF t_table,
  tt_table TYPE STANDARD TABLE OF t_table WITH EMPTY KEY.


"Handler for commands
CLASS lcl_handler DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES:
      zif_ed_screens_handler.
    METHODS:
      constructor IMPORTING in_popup TYPE abap_bool DEFAULT abap_false.
  PRIVATE SECTION.
    CLASS-DATA:
        random TYPE REF TO cl_abap_random.

    METHODS:
      get_random_int RETURNING VALUE(random_int) TYPE i.
    DATA:
      first_time_pbo TYPE abap_bool VALUE abap_true,
      in_popup       TYPE abap_bool.
ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD constructor.
    me->in_popup = in_popup.
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pbo_get_toolbar_commands.
    commands_to_set = VALUE #( ( command = |CALL_POPUP| description = VALUE #(  text = |Create next screen in popup|  ) )
        ( command = |CALL_FULLSCREEN| description = VALUE #( text = |Create next screen in fullscreen| ) ) ).
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pai.
    CASE command.
      WHEN 'CALL_POPUP'.
        "SALV table and container must be created inside PBO, otherwise they are not displayed in popup call screen.
        "But we have to prepare screen anyway
        zcl_ed_screens=>prepare_next_screen( handler = NEW lcl_handler( abap_true ) create_container = abap_false ).
        zcl_ed_screens=>call_next_screen( start_column = 9 start_line = 8 ).

      WHEN 'CALL_FULLSCREEN'.
        "When we call screen in fullscreen, we can create container outside PBO, before CALL SCREEN.
        DATA(container) = zcl_ed_screens=>prepare_next_screen( handler = NEW lcl_handler( ) ).
        DATA(table) = VALUE tt_table( ( description = |I'm screen { CONV i( dynnr ) + 1 }| ) ( description = |Random: { get_random_int( ) }| ) ).
        cl_salv_table=>factory( EXPORTING r_container = container IMPORTING r_salv_table = DATA(salv_table) CHANGING t_table = table ).
        salv_table->display( ).
        zcl_ed_screens=>call_next_screen( ).

      WHEN  zif_ed_screens_handler=>c_status_commands-back OR zif_ed_screens_handler=>c_status_commands-cancel OR zif_ed_screens_handler=>c_status_commands-exit.
        "Close current screen
        LEAVE TO SCREEN 0.

    ENDCASE.
  ENDMETHOD.

  METHOD zif_ed_screens_handler~pbo.
    IF in_popup = abap_false OR first_time_pbo = abap_false.
      RETURN.
    ENDIF.

    "Usually there is no need to recreate container in PBO more than once.
    DATA(table) = VALUE tt_table( ( description = |I'm screen { dynnr }| ) ( description = |Random: { get_random_int( ) }| ) ).
    DATA(container) = zcl_ed_screens=>get_screen_container( dynnr ).
    cl_salv_table=>factory( EXPORTING r_container = container IMPORTING r_salv_table = DATA(salv_table) CHANGING t_table = table ).
    salv_table->display( ).
    first_time_pbo = abap_false.
  ENDMETHOD.


  METHOD zif_ed_screens_handler~pbo_change_header.
    "You could use the same handler for several screens - in which case you can differentiate between them using dynnr.
    "They are  1-10 for toolbar screens, or 51-60 for screens without toolbar.
    header = |This is screen '{ dynnr }'|.
  ENDMETHOD.


  METHOD get_random_int.
    IF NOT random IS BOUND.
      random = cl_abap_random=>create( ).
    ENDIF.
    random_int = random->intinrange( low = 0 high = 10000 ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(table) = VALUE tt_table( ( description = |I'm screen 0001| ) ).
  "Get container, use it to create SALV_TABLE, display it
  DATA(container) = zcl_ed_screens=>prepare_next_screen( handler = NEW lcl_handler( ) ).
  cl_salv_table=>factory( EXPORTING r_container = container IMPORTING r_salv_table = DATA(salv_table) CHANGING t_table = table ).
  salv_table->display( ).
  zcl_ed_screens=>call_next_screen( ).
