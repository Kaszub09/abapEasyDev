*&---------------------------------------------------------------------*
*& Report zed_logs_ex_simple_use
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_logs_ex_context.

"Our context will be delivery number and position - it will be displayed next to log
TYPES:
  BEGIN OF t_context,
    vbeln TYPE vbeln_vl,
    posnr TYPE posnr_vl,
  END OF t_context.

DATA(logger) = zcl_ed_logger_factory=>create_logger( category = 'EXAMPLE_LOG' ext_id = |Test by { sy-uname }| expiry_date = CONV #( sy-datum + 7 )
    context = zcl_ed_logger_factory=>create_context_from_ref( NEW t_context(  ) ) ).

"You can create context object on the fly...
logger->i( obj = 'Starting delivery processing...' context_data = NEW t_context( vbeln = '7000001234' ) ).
logger->s(  obj = 'Item processed successfully' context_data = NEW t_context( vbeln = '7000001234' posnr = '000010' ) ).
"Or one upfront and use it
DATA(delivery) = VALUE t_context( vbeln = '7000001234' posnr = '000020' ).
logger->s(  obj = 'Item processed successfully' context_data = REF #( delivery ) ).
delivery-posnr = '000030'.
logger->e(  obj = 'Error when processing item - negative amount'  context_data = REF #( delivery ) ).

zcl_ed_logger_factory=>create_display( )->display_log( logger = logger  ).
