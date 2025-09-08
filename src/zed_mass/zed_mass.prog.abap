*&---------------------------------------------------------------------*
*& Report zed_mass
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_mass.

PARAMETERS: p_class TYPE string.

START-OF-SELECTION.
  DATA mass_change_object TYPE REF TO zif_ed_mass.
  CREATE OBJECT mass_change_object TYPE (p_class).
  DATA(rep) = NEW zcl_ed_mass( ).
  rep->run( mass_change_object ).
