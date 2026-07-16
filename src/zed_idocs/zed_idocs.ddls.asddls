@AbapCatalog.sqlViewName: 'zed_idocs_v'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'iDOCs'
@Metadata.ignorePropagatedAnnotations: true
define view zed_idocs
  as select from zed_idocs_metainfo as hdr

{
  //EDIDC
  docnum,
  serial,
  docrel,
  status,
  hdr.status_exception_col_value,
  stat_descrp,
  doctyp,
  direct,
  rcvpor,
  rcvprt,
  rcvprn,
  sndpor,
  sndprt,
  sndprn,
  credat,
  cretim,
  mestyp,
  hdr.mestyp_descrp,
  idoctp,
  cimtyp,
  maxsegnum,
  //EDP21
  evcode,
  //IDOCS_OPS
  eventt,
  routid,
  eventn,
  evenxx,
  descrp
}
