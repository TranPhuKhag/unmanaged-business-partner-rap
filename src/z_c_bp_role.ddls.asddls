@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View for Business Partner Role'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType: {
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define view entity Z_C_BP_Role
  as projection on Z_I_BP_ROLE
{
  @Consumption.hidden: true
  key BusinessPartner,
  
  @UI.lineItem: [{
    position: 10
  }]
  key BPRole,
  
  @UI.lineItem: [{
    position: 20
  }]
  ValidFrom,
  
  @UI.lineItem: [{
    position: 30
  }]
  ValidTo,
  
  @UI.lineItem: [{
    position: 40
  }]
  Role,
  
  @UI.lineItem: [{
    position: 50
  }]
  Authority,
  
  /* Associations */
  _BusinessPartner : redirected to parent Z_C_BusinessPartner
}
