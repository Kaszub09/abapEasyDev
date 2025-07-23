CLASS zcl_ed_program_runner_factoryi DEFINITION PUBLIC FINAL CREATE PRIVATE FOR TESTING.

  PUBLIC SECTION.
    CLASS-METHODS:
      inject IMPORTING program_runner TYPE REF TO zif_ed_program_runner OPTIONAL.
ENDCLASS.

CLASS zcl_ed_program_runner_factoryi IMPLEMENTATION.
  METHOD inject.
    zcl_ed_program_runner_factory=>pr = program_runner.
  ENDMETHOD.
ENDCLASS.
