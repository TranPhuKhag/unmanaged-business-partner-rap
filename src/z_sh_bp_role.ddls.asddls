@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View Search Help for BP Role'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.dataCategory: #VALUE_HELP
@Search.searchable: true
define view entity Z_SH_BP_ROLE 
 as select distinct from tb003
 inner join tb003t
 on tb003.role = tb003t.role
 and tb003t.spras = $session.system_language
{
  @Search.defaultSearchElement: true
  key tb003.role as BusinessPartnerRole,
      @Consumption.filter.hidden: true
      tb003.rolecategory as BPRoleCat,
      @Consumption.filter.hidden: true
      tb003.bpview as BPView,
      @Consumption.filter.hidden: true
      tb003.posnr as PositionofBusinessPartner,
      @Consumption.filter.hidden: true
      tb003t.rltitl as Title,
      @Consumption.filter.hidden: true
      tb003t.rltxt as Description
}
