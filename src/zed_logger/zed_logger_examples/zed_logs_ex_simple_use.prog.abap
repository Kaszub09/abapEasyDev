*&---------------------------------------------------------------------*
*& Report zed_logs_ex_simple_use
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zed_logs_ex_simple_use.

"We leave default settings, which means second connection and autosave, so logs are saved even in case of dumps or rollbacks.
"Expiry date as today+7, so they are deleted after a week (provided bg job runs)
DATA(logger) = zcl_ed_logger_factory=>create_logger( category = 'EXAMPLE_LOG' ext_id = |Test by { sy-uname }| expiry_date = CONV #( sy-datum + 7 ) ).

"Informative message
logger->i( 'Simple message' ).

"Log whole table in json format
SELECT * UP TO 4 ROWS FROM sflight INTO TABLE @DATA(flights).
APPEND VALUE #( carrid = 'ABC' connid = '1234' ) TO flights. "Dummy entry in case sflight is empty
APPEND VALUE #( carrid = 'DEF' connid = '0007' ) TO flights. "Dummy entry in case sflight is empty
logger->s( flights ).

"By default add message stored in sy variable
MESSAGE e003(38) WITH 'PARAMETER1' INTO DATA(dummy).
logger->add( ).

"You can also add whole table of messages, e.g. from BAPI call or BDC call
DATA bapiret2_tab TYPE STANDARD TABLE OF bapiret2 WITH EMPTY KEY.
APPEND VALUE #( id = 'BANK_JC' number = '003' type = 'W' ) TO bapiret2_tab.
APPEND VALUE #( id = 'BANK_JC' number = '004' type = 'E' ) TO bapiret2_tab.
logger->add( bapiret2_tab ).

logger->i( logger->c_line-eq )->i( 'Some message bewteen eq signs to catch attenton' )->i( logger->c_line-eq ).

"You can display log in full screen or popup
zcl_ed_logger_factory=>create_display( )->display_log( logger = logger start_column = 1 start_line = 1 ).
