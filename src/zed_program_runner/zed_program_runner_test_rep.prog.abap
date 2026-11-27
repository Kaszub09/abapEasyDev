*&---------------------------------------------------------------------*
*& Report zed_program_runner_test_rep
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_program_runner_test_rep.
CLASS lcl DEFINITION PUBLIC FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
    copy CHANGING source TYPE table target TYPE table.
  
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD copy.
    target = CORRESPONDING #( source ).
  ENDMETHOD.
ENDCLASS.

start-of-selection.
write: /'LINE_1', /'LINE_2'.
