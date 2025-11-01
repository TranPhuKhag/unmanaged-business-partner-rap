CLASS zcl_query_bp DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_rap_query_provider. "
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_query_bp IMPLEMENTATION.
  METHOD if_rap_query_provider~select.

    DATA(lt_parameters) = io_request->get_parameters( ).

    READ TABLE lt_parameters WITH KEY parameter_name = 'p_partner' INTO DATA(ls_parameter).

    IF sy-subrc = 0 AND ls_parameter-value IS NOT INITIAL.

      DATA(lv_partner) = ls_parameter-value.

      SELECT *
        FROM Z_I_BUSINESSPARTNER
        WHERE BusinessPartner = @lv_partner
        INTO TABLE @DATA(lt_business_partners).

      io_response->set_data( lt_business_partners ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
