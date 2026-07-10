@AbapCatalog.sqlViewName: 'zed_log_cat'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Log category'
@Metadata.ignorePropagatedAnnotations: true
define view zed_log_category
  as select distinct from zed_logs
  left outer join         zed_logger_conf on zed_logger_conf.category = zed_logs.category
{
  zed_logs.category,
  zed_logger_conf.activate
}
