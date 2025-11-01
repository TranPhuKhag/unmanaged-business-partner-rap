@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View for Address'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity Z_C_BP_ADDRESS
    as projection on Z_I_BP_ADDRESS
{
    @Consumption.hidden: true
    key BusinessPartner,
    @UI.lineItem: [{
    position: 10
    }]
    key Addrnumber,
    PostCode2,
    Region,
    Country,
    Langu,
    Street,
    HouseNum1,
    PostCode1,
    City1,
    
     /* Associations */
    _BusinessPartner : redirected to parent Z_C_BusinessPartner
}
