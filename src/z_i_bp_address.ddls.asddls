@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS view for Unique Address'
@ObjectModel: {
  compositionRoot: true
}

define view entity Z_I_BP_ADDRESS
  as select from    but020 as bpaddr
    left outer join adrc   as addr on addr.addrnumber = bpaddr.addrnumber
  association to parent Z_I_BUSINESSPARTNER as _BusinessPartner on $projection.BusinessPartner = _BusinessPartner.BusinessPartner

{
  key bpaddr.partner    as BusinessPartner,
  key bpaddr.addrnumber as Addrnumber,
      addr.post_code2   as PostCode2, // POST_CODE2 (Postal Code/City)
      addr.country      as Country,   // Country/Reg.
      addr.langu        as Langu,     // Langue
      addr.region       as Region,    // Region
      addr.street       as Street,    // STREET Street/House Number
      addr.house_num1   as HouseNum1, // HOUSE_NUM1
      addr.post_code1   as PostCode1, // POST_CODE1 (Postal Code/City)
      addr.city1        as City1,     // CITY1 (City)

      _BusinessPartner
}
