CLASS zcl_ed_icf_file_router DEFINITION PUBLIC INHERITING FROM cl_rest_http_handler FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: if_rest_application~get_root_handler REDEFINITION.
ENDCLASS.

CLASS zcl_ed_icf_file_router IMPLEMENTATION.
  METHOD if_rest_application~get_root_handler.
    DATA(lo_router) = NEW cl_rest_router( ).
    ro_root_handler = lo_router.

    lo_router->attach( iv_template = '' iv_handler_class = 'ZCL_ED_ICF_FILE_HANDLER' ).
    lo_router->attach( iv_template = '/' iv_handler_class = 'ZCL_ED_ICF_FILE_HANDLER' ).
    lo_router->attach( iv_template = '/{f1}' iv_handler_class = 'ZCL_ED_ICF_FILE_HANDLER' ).
    lo_router->attach( iv_template = '/{f1}/{f2}' iv_handler_class = 'ZCL_ED_ICF_FILE_HANDLER' ).
    lo_router->attach( iv_template = '/{f1}/{f2}/{f3}' iv_handler_class = 'ZCL_ED_ICF_FILE_HANDLER' ).
    lo_router->attach( iv_template = '/{f1}/{f2}/{f3}/{f4}' iv_handler_class = 'ZCL_ED_ICF_FILE_HANDLER' ).
    lo_router->attach( iv_template = '/{f1}/{f2}/{f3}/{f4}/{f5}' iv_handler_class = 'ZCL_ED_ICF_FILE_HANDLER' ).
  ENDMETHOD.
ENDCLASS.
