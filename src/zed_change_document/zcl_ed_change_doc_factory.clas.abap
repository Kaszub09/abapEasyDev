"! <p class="shorttext synchronized" lang="en">Change document factory</p>
"! <br/>TAGS: change document; factory
CLASS zcl_ed_change_doc_factory DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ed_change_doc_factory_inj.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! @parameter objectclass | <p class="shorttext synchronized">Name of CD object (e.g. from SCDO).
      "! Leave empty if will be supplied in <em>open</em>, <em>close</em> methods.</p>
      "! @parameter objectid |  <p class="shorttext synchronized">Object ID inside CD object.
      "! Leave empty if will be supplied in <em>open</em>, <em>close</em> methods.
      "! <br/>Something that ties records from all tables in SCDO, like common foreign key,
      "! e.g. document number for both header and positions tables
      "! @parameter table_name |  <p class="shorttext synchronized">Leave empty if will be supplied in <em>change</em> methods.</p>
      create IMPORTING objectclass TYPE cdobjectcl OPTIONAL objectid TYPE cdobjectv OPTIONAL table_name TYPE tabname OPTIONAL
             RETURNING VALUE(cd) TYPE REF TO zif_ed_change_doc.

  PRIVATE SECTION.
    CLASS-DATA:
        cd_mock TYPE REF TO zif_ed_change_doc.
ENDCLASS.

CLASS zcl_ed_change_doc_factory IMPLEMENTATION.
  METHOD create.
    IF cd_mock IS BOUND.
      cd = cd_mock.
      RETURN.
    ENDIF.

    cd = NEW zcl_ed_change_doc( objectclass = objectclass objectid = objectid table_name = table_name ).
  ENDMETHOD.
ENDCLASS.
