@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help for BP, Role, and Group'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #L,
    dataClass: #MIXED
}
@Search.searchable: true
@ObjectModel.dataCategory: #VALUE_HELP

define view entity Z_SH_BP_ROLE_GROUP
  as select from    Z_I_BUSINESSPARTNER as bp
    left outer join but020 as addr 
      on bp.BusinessPartner = addr.partner
    left outer join but100 as role
      on bp.BusinessPartner = role.partner
    left outer join adr6 as email 
      on addr.addrnumber = email.addrnumber
    left outer join adr2 as phone 
      on addr.addrnumber = phone.addrnumber


{
      @Search.fuzzinessThreshold: 0.8
      @Consumption.filter.hidden: true
  key bp.BusinessPartner            as BusinessPartner,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Consumption.valueHelpDefinition: [{
            entity: {
                name: 'Z_SH_BP_Role',
                element: 'BusinessPartnerRole'
            }
          }]
      @UI.selectionField: [{ position: 10 }]
  key role.rltyp            as BusinessPartnerRole,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Consumption.valueHelpDefinition: [{
            entity: {
                name: 'Z_SH_BP_GROUP',
                element: 'BusinessPartnerGroup'
            }
          }]
      @UI.selectionField: [{ position: 20 }]
      bp.BusinessPartnerGroup           as BusinessPartnerGroup,

//      @Consumption.filter.hidden: true
//      bp.name_first         as FirstName,
//
//      @Consumption.filter.hidden: true
//      bp.name_last          as LastName,
//
//      @Consumption.filter.hidden: true
//      bp.name_org1          as OrganizationName1,

      @Consumption.filter.hidden: true
      bp.FirstName         as SH_FirstName,
    
      @Consumption.filter.hidden: true
      bp.LastName          as SH_LastName,
    
      @Consumption.filter.hidden: true
      bp.OrganizationName1          as SH_OrganizationName1,
  
      @Consumption.filter.hidden: true
      max(email.smtp_addr)  as EmailAddress, // E-Mail Address

      @Consumption.filter.hidden: true
      max(phone.tel_number) as PhoneNumber // Telephone
}

group by
  bp.BusinessPartner,
  role.rltyp,
  bp.BusinessPartnerGroup,
  bp.FirstName,
  bp.LastName,
  bp.OrganizationName1
