@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Business Partner - Base Data'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #L,
    dataClass: #MIXED
}
@ObjectModel.transactionalProcessingEnabled: true 

define root view entity Z_I_BUSINESSPARTNER
  as select from but000 as bp
//  association [0..*] to Z_I_BP_ROLE as _Roles on $projection.BusinessPartner = _Roles.BusinessPartner
    composition [0..*] of Z_I_BP_ROLE as _Roles
    composition [0..*] of  Z_I_BP_ADDRESS as _Address
{
  key bp.partner      as BusinessPartner,
      bp.type         as BusinessPartnerCategory,
      bp.bpkind       as BusinessPartnerKind,
      bp.bu_group     as BusinessPartnerGroup,
      bp.name_first   as FirstName,
      bp.name_last    as LastName,
      bp.name_org1    as OrganizationName1,
      bp.title_aca1   as AcademicTitle,
//      bp.xdele        as CentralArchivingFlag,
      _Roles,
      _Address,
      @Semantics.systemDateTime.lastChangedAt: true  // <--- THÊM DÒNG NÀY
      bp.chtim as LastChangedAt

}
where
  bp.xdele = ''
