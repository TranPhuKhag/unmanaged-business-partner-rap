@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View for Business Partner Role'

define view entity Z_I_BP_ROLE
  as select from but100
  association to parent Z_I_BUSINESSPARTNER as _BusinessPartner
    on $projection.BusinessPartner = _BusinessPartner.BusinessPartner
{
  key partner as BusinessPartner,
  key rltyp   as BPRole,
  valid_from as ValidFrom,
  valid_to as ValidTo,
  role as Role,
  authority as Authority,
  
  _BusinessPartner
}
