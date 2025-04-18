"! <p class="shorttext synchronized">Program runner factory</p>
"! <br/>TAGS: program runner; factory
CLASS zcl_ed_program_runner_factory DEFINITION PUBLIC FINAL CREATE PUBLIC
GLOBAL FRIENDS zcl_ed_program_runner_factoryi.

  PUBLIC SECTION.
    CLASS-METHODS:
      create RETURNING VALUE(program_runner) TYPE REF TO zif_ed_program_runner.

  PRIVATE SECTION.
    CLASS-DATA:
        pr TYPE REF TO zif_ed_program_runner.
ENDCLASS.

CLASS zcl_ed_program_runner_factory IMPLEMENTATION.
  METHOD create.
    IF NOT pr IS BOUND.
      pr = NEW zcl_ed_program_runner( ).
    ENDIF.
    program_runner = pr.
  ENDMETHOD.
ENDCLASS.
