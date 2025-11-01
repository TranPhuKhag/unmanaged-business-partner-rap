@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View for Business Partner'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@UI.headerInfo: {
  typeName: 'Business Partner',
  typeNamePlural: 'Business Partners',
  title: {
    type: #STANDARD,
    value: 'BusinessPartner'
  }
}

define root view entity Z_C_BusinessPartner
provider contract transactional_query
  as projection on Z_I_BUSINESSPARTNER
  
//  association [0..*] to Z_C_BP_Role as _Roles on $projection.BusinessPartner = _Roles.BusinessPartner
{
//  @UI.selectionField: [{ position: 10 }]
  @Consumption.filter: { mandatory: true }
  @Consumption.valueHelpDefinition: [{
      entity: {
        name: 'Z_SH_BP_ROLE_GROUP',
        element: 'BusinessPartner'
      }
  }]
  @UI.lineItem: [{ position: 10 }]
  key BusinessPartner,

  BusinessPartnerCategory,
  BusinessPartnerGroup,
  BusinessPartnerKind,
  LastName,
  OrganizationName1,
  AcademicTitle,
  FirstName,
  
  @UI.hidden: true  // Ẩn trường này khỏi UI vì người dùng không cần thấy
  LastChangedAt,
  
  
//  _Roles.BPRole as Rolec
  _Roles : redirected to composition child Z_C_BP_Role,
  _Address : redirected to composition child Z_C_BP_ADDRESS
}

