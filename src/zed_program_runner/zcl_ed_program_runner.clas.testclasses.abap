*"* use this source file for your ABAP unit test classes
CLASS ltcl_program_runner DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      run_test_program FOR TESTING.
ENDCLASS.


CLASS ltcl_program_runner IMPLEMENTATION.


  METHOD run_test_program.
    DATA(pr) = zcl_ed_program_runner_factory=>create( ).
    DATA(result) = pr->run_with_list( program_name = 'ZED_PROGRAM_RUNNER_TEST_REP' ).
    DATA(expected) = VALUE zif_ed_program_runner=>tt_list( ( |LINE_21| ) ( |LINE_2| ) ).
    cl_abap_unit_assert=>assert_equals( act = result exp = expected ).
  ENDMETHOD.

ENDCLASS.
