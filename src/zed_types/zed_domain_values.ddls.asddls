@AbapCatalog.sqlViewName: 'ZED_DOMAIN_VALUE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Domain values'
@Metadata.ignorePropagatedAnnotations: true
define view ZED_DOMAIN_VALUES as select from dd07t
{
  domname,
  valpos,
  ddtext,
  domval_ld,
  domval_hd,
  domvalue_l 
}
//restrict AS4LOCAL and AS4VERS to avoid duplicates
where ddlanguage = $session.system_language and as4local = 'A' and dd07t.as4vers = '0000'
