@AbapCatalog.sqlViewName: 'zed_idocs_meta'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'iDOCs metainfor'
@Metadata.ignorePropagatedAnnotations: true
//2 - INBOUND
define view zed_idocs_metainfo
  as select from         edidc
  left outer to one join stacust  on stacust.status = edidc.status
  left outer to one join stalight on stalight.statva = stacust.statva
  left outer to one join teds2    on  teds2.status = edidc.status
                                  and teds2.langua = $session.system_language
  left outer to one join edimsgt  on  edimsgt.mestyp = edidc.mestyp
                                  and edimsgt.langua = $session.system_language
  left outer to one join edp21    on  edp21.sndprn = edidc.sndprn
                                  and edp21.sndprt = edidc.sndprt
                                  and edp21.sndpfc = edidc.sndpfc
                                  and edp21.mestyp = edidc.mestyp
                                  and edp21.mescod = edidc.mescod
                                  and edp21.mesfct = edidc.mesfct
                                  and edp21.test   = edidc.test
  left outer to one join tede2    on tede2.evcode = edp21.evcode
  left outer to one join ede2t    on  ede2t.evcode = tede2.evcode
                                  and ede2t.langua = $session.system_language
{
  //EDIDC
  edidc.docnum,
  edidc.docrel,
  edidc.status,
  //Check IDOC_TREE_CONTROLF01, line 527, before ICON_CREATE
  case when stalight.stalight = '1' then '2'
       when stalight.stalight = '2' then '3'
       when stalight.stalight = '3' then '1'
       else '2'
  end            as status_exception_col_value,
  teds2.descrp   as stat_descrp,
  edidc.doctyp,
  edidc.direct,
  edidc.rcvpor,
  edidc.rcvprt,
  edidc.rcvprn,
  edidc.sndpor,
  edidc.sndprt,
  edidc.sndprn,
  edidc.credat,
  edidc.cretim,
  edidc.mestyp,
  edimsgt.descrp as mestyp_descrp,
  edidc.idoctp,
  edidc.cimtyp,
  edidc.maxsegnum,
  //EDP21
  edp21.evcode,
  //TEDE
  tede2.eventt,
  tede2.evenid   as routid,
  tede2.eventn,
  tede2.evenxx,
  ede2t.descrp
}
where
  edidc.direct = '2'

union all select from  edidc
left outer to one join stacust  on stacust.status = edidc.status
left outer to one join stalight on stalight.statva = stacust.statva
left outer to one join teds2    on  teds2.status = edidc.status
                                and teds2.langua = $session.system_language
left outer to one join edimsgt  on  edimsgt.mestyp = edidc.mestyp
                                and edimsgt.langua = $session.system_language
left outer to one join edp13    on  edp13.rcvprn = edidc.rcvprn
                                and edp13.rcvprt = edidc.rcvprt
                                and edp13.rcvpfc = edidc.rcvpfc
                                and edp13.mestyp = edidc.mestyp
                                and edp13.mescod = edidc.mescod
                                and edp13.mesfct = edidc.mesfct
                                and edp13.test   = edidc.test
//While technically may not be true, we don't actually wnat duplicates if we skip fields fro mthis join
left outer to one join edp12    on  edp12.rcvprn = edp13.rcvprn
                                and edp12.rcvprt = edp13.rcvprt
                                and edp12.rcvpfc = edp13.rcvpfc
                                and edp12.mestyp = edp13.mestyp
left outer to one join tede1    on tede1.evcode = edp12.evcoda
left outer to one join ede1t    on  ede1t.evcoda = tede1.evcode
                                and ede1t.langua = $session.system_language

{
  //EDIDC
  edidc.docnum,
  edidc.docrel,
  edidc.status,
  //Check IDOC_TREE_CONTROLF01, line 527, before ICON_CREATE
  case when stalight.stalight = '1' then '2'
       when stalight.stalight = '2' then '3'
       when stalight.stalight = '3' then '1'
       else '2'
  end            as status_exception_col_value,
  teds2.descrp   as stat_descrp,
  edidc.doctyp,
  edidc.direct,
  edidc.rcvpor,
  edidc.rcvprt,
  edidc.rcvprn,
  edidc.sndpor,
  edidc.sndprt,
  edidc.sndprn,
  edidc.credat,
  edidc.cretim,
  edidc.mestyp,
  edimsgt.descrp as mestyp_descrp,
  edidc.idoctp,
  edidc.cimtyp,
  edidc.maxsegnum,
  //EDP21
  edp12.evcoda   as evcode,
  //TEDE
  tede1.eventt,
  tede1.routid,
  tede1.eventn,
  tede1.evenxx,
  ede1t.descrp
}
where
  edidc.direct = '1'
