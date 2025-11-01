@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View Search Help for BP Group'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity Z_SH_BP_GROUP 
as select distinct from tb001
 inner join tb002
 on tb001.bu_group = tb002.bu_group
 and tb002.spras = $session.system_language
{
  @Search.defaultSearchElement: true
  key tb001.bu_group as BusinessPartnerGroup,
      @Consumption.filter.hidden: true
      tb001.nrrng    as NumberRange,
      @Consumption.filter.hidden: true
      tb002.txt15    as Shortname,
      @Consumption.filter.hidden: true
      tb002.txt40    as Description
}
