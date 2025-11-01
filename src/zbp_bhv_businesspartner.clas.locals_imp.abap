CLASS lhc_Z_I_BusinessPartner DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE Z_I_BusinessPartner.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE Z_I_BusinessPartner.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE Z_I_BusinessPartner.

    METHODS read FOR READ
      IMPORTING keys FOR READ Z_I_BusinessPartner RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK Z_I_BusinessPartner.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Z_I_BusinessPartner RESULT result.

    METHODS rba_Address FOR READ
      IMPORTING keys_rba
                  FOR READ Z_I_BusinessPartner\_Address
                  FULL result_requested
      RESULT    result
                  LINK association_links.

    METHODS rba_Roles FOR READ
      IMPORTING keys_rba
                  FOR READ Z_I_BusinessPartner\_Roles
                  FULL result_requested
      RESULT    result
                  LINK association_links.

    METHODS cba_Address FOR MODIFY
      IMPORTING entities_cba
                  FOR CREATE Z_I_BusinessPartner\_Address.

    METHODS cba_Roles FOR MODIFY
      IMPORTING entities_cba
                  FOR CREATE Z_I_BusinessPartner\_Roles.

    METHODS precheck_create FOR PRECHECK
      IMPORTING entities FOR CREATE Z_I_BusinessPartner.

    METHODS precheck_cba_Roles FOR PRECHECK
      IMPORTING entities FOR CREATE Z_I_BusinessPartner\_Roles.

    METHODS precheck_cba_Address FOR PRECHECK
      IMPORTING entities FOR CREATE Z_I_BusinessPartner\_Address.

    CONSTANTS:
      gc_msgid         TYPE symsgid VALUE 'YGRP01_SE1744',
      gc_msgnum_invalid_value          TYPE symsgno VALUE '001',
      gc_msgnum_field_missing          TYPE symsgno VALUE '002',
      gc_msgnum_not_in_customizing     TYPE symsgno VALUE '005',

      gc_cat_person           TYPE but000-type VALUE '1',
      gc_cat_organization     TYPE but000-type VALUE '2',

      gc_msgtype_error TYPE symsgty VALUE 'E',
      gc_msgtype_abort TYPE symsgty VALUE 'A'.


    TYPES tt_businesspartner_failed   TYPE TABLE FOR FAILED  z_i_businesspartner.
    TYPES tt_businesspartner_reported TYPE TABLE FOR REPORTED z_i_businesspartner.

    METHODS map_bapi_messages
      IMPORTING
        iv_cid         TYPE string OPTIONAL
        IT_Messages    TYPE bapiret2_t
      EXPORTING
        EV_FailedAdded TYPE abap_bool
      CHANGING
        CT_Failed      TYPE tt_businesspartner_failed
        CT_Reported    TYPE tt_businesspartner_reported.

    TYPES tt_addresss_failed TYPE TABLE FOR FAILED z_i_bp_address.
    TYPES tt_address_reported TYPE TABLE FOR REPORTED z_i_bp_address.

    METHODS map_message_assoc_to_address
      IMPORTING
        iv_cid         TYPE string OPTIONAL
        IV_IsDependend TYPE abap_bool DEFAULT abap_false
        IT_Messages    TYPE bapiret2_t
      EXPORTING
        EV_FailedAdded TYPE abap_bool
      CHANGING
        CT_Failed      TYPE tt_addresss_failed
        CT_Reported    TYPE tt_address_reported
      .
    TYPES tt_role_failed TYPE TABLE FOR FAILED z_i_bp_role.
    TYPES tt_role_reported TYPE TABLE FOR REPORTED z_i_bp_role.
    METHODS map_message_assoc_to_role
      IMPORTING
        iv_cid         TYPE string OPTIONAL
        IV_IsDependend TYPE abap_bool DEFAULT abap_false
        IT_Messages    TYPE bapiret2_t
      EXPORTING
        EV_FailedAdded TYPE abap_bool
      CHANGING
        CT_Failed      TYPE tt_role_failed
        CT_Reported    TYPE tt_role_reported
      .


ENDCLASS.

CLASS lhc_Z_I_BusinessPartner IMPLEMENTATION.

  METHOD create.
    LOOP AT entities INTO DATA(entity).
      DATA:
        "exporting param for bapi func
        lv_partnercategory      TYPE bapibus1006_head-partn_cat,
        lv_partnergroup         TYPE bapibus1006_head-partn_grp,

        "data struc
        ls_centraldata          TYPE bapibus1006_central,
        ls_centraldata_person   TYPE bapibus1006_central_person,
        ls_addressdata          TYPE bapibus1006_address,
        ls_centraldata_organiza TYPE bapibus1006_central_organ,

        "data tables
        lt_telefondata          TYPE TABLE OF bapiadtel,
        lt_emaildata            TYPE TABLE OF bapiadsmtp,
        lt_return               TYPE TABLE OF bapiret2,

        lv_businesspartner      TYPE bu_partner.


      lv_partnercategory = entity-%data-BusinessPartnerCategory.
      lv_partnergroup    = entity-%data-BusinessPartnerGroup.

      "ls_centraldata-title_key = entity-%data-AcademicTitle.
      ls_centraldata-partnertype = entity-%data-BusinessPartnerKind.

      IF entity-%data-BusinessPartnerCategory = gc_cat_person.
        ls_centraldata_person-firstname = entity-%data-FirstName.
        ls_centraldata_person-lastname  = entity-%data-LastName.
        ls_centraldata_person-correspondlanguage = sy-langu.
        ls_centraldata_person-title_aca1 = entity-%data-AcademicTitle.
      ENDIF.

      IF entity-%data-BusinessPartnerCategory = gc_cat_organization.
        ls_centraldata_organiza = VALUE bapibus1006_central_organ(
                                        name1 = entity-%data-OrganizationName1
                                      ).
      ENDIF.

      CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
        EXPORTING
          partnercategory         = lv_partnercategory
          partnergroup            = lv_partnergroup
          centraldata             = ls_centraldata
          centraldataperson       = ls_centraldata_person
          centraldataorganization = ls_centraldata_organiza
          addressdata             = ls_addressdata
        IMPORTING
          businesspartner         = lv_businesspartner
        TABLES
*         telefondata             = lt_telefondata
*         e_maildata              = lt_emaildata
          return                  = lt_return.

      map_bapi_messages(
        EXPORTING
          iv_cid         = entity-%cid
          IT_Messages    = lt_return
        IMPORTING
          EV_FailedAdded = DATA(lv_failed)
        CHANGING
          CT_Failed      = failed-z_i_businesspartner
          CT_Reported    = reported-z_i_businesspartner
      ).

      IF lv_failed = abap_false.
        APPEND VALUE #(
            %cid            = entity-%cid
            BusinessPartner = lv_businesspartner
        ) TO mapped-z_i_businesspartner.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD update.
    LOOP AT entities INTO DATA(entity).
      DATA:
        "data struc
        ls_head                   TYPE bapibus1006_head,
        ls_centraldata            TYPE bapibus1006_central,
        ls_centraldata_person     TYPE bapibus1006_central_person,
        ls_centraldata_organiza   TYPE bapibus1006_central_organ,

        "data struc_X
        ls_centraldata_x          TYPE bapibus1006_central_x,
        ls_centraldata_person_x   TYPE bapibus1006_central_person_x,
        ls_centraldata_organiza_x TYPE bapibus1006_central_organ_x,


        "data tables
        lt_telefondata            TYPE TABLE OF bapiadtel,
        lt_emaildata              TYPE TABLE OF bapiadsmtp,
        lt_return                 TYPE TABLE OF bapiret2,

        lv_businesspartner        TYPE bu_partner.

      ls_head-partn_cat = entity-%data-BusinessPartnerCategory.
      ls_head-partn_grp = entity-%data-BusinessPartnerGroup.
      ls_head-bpartner  = entity-%data-BusinessPartner.

      " Set data
      IF entity-%data-BusinessPartnerKind IS NOT INITIAL.
        ls_centraldata-partnertype = entity-%data-BusinessPartnerKind.
        ls_centraldata_x-partnertype = abap_true.
      ENDIF.

      IF entity-%data-BusinessPartnerCategory = gc_cat_person.

        IF entity-%data-FirstName IS NOT INITIAL.
          ls_centraldata_person-firstname = entity-%data-FirstName.
          ls_centraldata_person_x-firstname = abap_true.
        ENDIF.

        IF entity-%data-LastName IS NOT INITIAL.
          ls_centraldata_person-lastname  = entity-%data-LastName.
          ls_centraldata_person_x-lastname  = abap_true.
        ENDIF.

        IF entity-%data-AcademicTitle IS NOT INITIAL.
          ls_centraldata_person-title_aca1 = entity-%data-AcademicTitle.
          ls_centraldata_person_X-title_aca1 = abap_true.
        ENDIF.

      ENDIF.

      IF entity-%data-BusinessPartnerCategory = gc_cat_organization.
        ls_centraldata_organiza_x-name1 = abap_true.
        ls_centraldata_organiza = VALUE bapibus1006_central_organ(
                                        name1 = entity-%data-OrganizationName1
                                      ).
      ENDIF.

      CALL FUNCTION 'BAPI_BUPA_CENTRAL_CHANGE'
        EXPORTING
          businesspartner           = entity-%data-BusinessPartner
          centraldata               = ls_centraldata
          centraldataperson         = ls_centraldata_person
          centraldataorganization   = ls_centraldata_organiza
          centraldata_x             = ls_centraldata_x
          centraldataperson_x       = ls_centraldata_person_x
          centraldataorganization_x = ls_centraldata_organiza_x
        TABLES
          return                    = lt_return.

      map_bapi_messages(
        EXPORTING
          IT_Messages    = lt_return
        IMPORTING
          EV_FailedAdded = DATA(lv_failed)
        CHANGING
          CT_Failed      = failed-z_i_businesspartner
          CT_Reported    = reported-z_i_businesspartner
      ).

      IF lv_failed = abap_false.
        APPEND VALUE #(
            BusinessPartner = entity-%data-BusinessPartner
        ) TO mapped-z_i_businesspartner.
      ENDIF.
    ENDLOOP..
  ENDMETHOD.

  METHOD delete.
    DATA:
      lv_businesspartner TYPE bu_partner,

      ls_centraldata     TYPE bapibus1006_central,
      ls_centraldata_x   TYPE bapibus1006_central_x,

      lt_return          TYPE TABLE OF bapiret2
      .
    LOOP AT keys INTO DATA(ls_key).

      IF ls_key-BusinessPartner IS NOT INITIAL.
        lv_businesspartner = ls_key-BusinessPartner.
        ls_centraldata-centralarchivingflag = 'X'.
        ls_centraldata_x-centralarchivingflag = abap_true.
      ENDIF.

      CALL FUNCTION 'BAPI_BUPA_CENTRAL_CHANGE'
        EXPORTING
          businesspartner = ls_key-BusinessPartner
          centraldata     = ls_centraldata
          centraldata_x   = ls_centraldata_x
        TABLES
          return          = lt_return.

      map_bapi_messages(
        EXPORTING
          IT_Messages    = lt_return
        IMPORTING
          EV_FailedAdded = DATA(lv_failed)
        CHANGING
          CT_Failed      = failed-z_i_businesspartner
          CT_Reported    = reported-z_i_businesspartner
      ).

    ENDLOOP.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
    DATA: lt_return TYPE TABLE OF bapiret2.

    LOOP AT keys INTO DATA(ls_key).

      CLEAR lt_return.

      CALL FUNCTION 'BUPA_ENQUEUE'
        EXPORTING
          iv_partner = ls_key-BusinessPartner
        TABLES
          et_return  = lt_return.

      LOOP AT lt_return INTO DATA(ls_return) WHERE type = gc_msgtype_error OR type = gc_msgtype_abort.

        APPEND VALUE #(
            %key = ls_key
            %fail-cause = if_abap_behv=>cause-locked
        ) TO failed-z_i_businesspartner.

        APPEND VALUE #(
          %key = ls_key
          %msg = new_message(
                    id       = ls_return-id
                    number   = ls_return-number
                    v1       = ls_return-message_v1
                    v2       = ls_return-message_v2
                    v3       = ls_return-message_v3
                    v4       = ls_return-message_v4
                    severity = if_abap_behv_message=>severity-error )
        ) TO reported-z_i_businesspartner.
        EXIT.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD rba_Address.
*    DATA:
*      lv_businesspartner TYPE bu_partner,
*      lt_businesspartner TYPE TABLE OF bapibus1006_head,
*      lt_address         TYPE TABLE OF bapibus1006_address,
*
*      lt_return          TYPE bapiret2_t
*      .
*    LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<businesspartner_rba>) GROUP BY <businesspartner_rba>-BusinessPartner.
*      CALL FUNCTION 'BAPI_BUPA_GET_NUMBERS'
*        EXPORTING
*          businesspartner    = <businesspartner_rba>-BusinessPartner
*        IMPORTING
*          businesspartnerout = lt_businesspartner
*        TABLES
*          return             = lt_return.
*
*      map_bapi_messages(
*        EXPORTING
*          IT_Messages    = lt_return
*        IMPORTING
*          EV_FailedAdded = DATA(failed_added)
*        CHANGING
*          CT_Failed      = failed-z_i_businesspartner
*          CT_Reported    = reported-z_i_businesspartner
*      ).
*      IF failed_added = abap_false.
*        LOOP AT lt_businesspartner ASSIGNING FIELD-SYMBOL(<LFS_Address>).
*          DATA: lv_address TYPE ad_addrnum.
*          CLEAR: lv_address.
*          lv_address = <LFS_Address>-addr_no.
*          CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
*            EXPORTING
*              businesspartner = <businesspartner_rba>-BusinessPartner
*              addressguid     = lv_address
*            IMPORTING
*              addressdata     = lt_address
*            TABLES
*              return          = lt_return.
*
*          map_message_assoc_to_address(
*            EXPORTING
*              IV_IsDependend = abap_true
*              IT_Messages    = lt_return
*            CHANGING
*              CT_Failed      = failed-address
*              CT_Reported    = reported-address
*          ).
*          LOOP AT lt_address ASSIGNING FIELD-SYMBOL(<address_data>).
*            DATA: address  LIKE LINE OF result.
*            INSERT
*                VALUE #(
*                    source-%tky = <businesspartner_rba>-BusinessPartner
*                    target-%tky = VALUE #(
*                                   BusinessPartner = <businesspartner_rba>-BusinessPartner
*                                   Addrnumber = <LFS_Address>-addr_no
*                     )
*                     ) INTO TABLE association_links.
*
*            IF result_requested = abap_true.
*              CLEAR: address.
*              address-BusinessPartner = <businesspartner_rba>-BusinessPartner.
*              address-Addrnumber     = <LFS_Address>-addr_no.
*              address-Street         = <address_data>-street.
*              address-City1          = <address_data>-city.
*              address-PostCode1      = <address_data>-postl_cod1.
*              address-Region         = <address_data>-region.
*              address-Country        = <address_data>-country.
*              address-HouseNum1     = <address_data>-house_no.
*              address-PostCode2      = <address_data>-postl_cod2.
*              address-Langu          = <address_data>-langu.
*              INSERT address INTO TABLE result.
*              .
*            ENDIF.
*          ENDLOOP.
*        ENDLOOP.
*      ENDIF.
*    ENDLOOP.
*
*    SORT association_links BY target ASCENDING.
*    DELETE ADJACENT DUPLICATES FROM association_links COMPARING ALL FIELDS.
*
*    SORT result BY %tky ASCENDING.
*    DELETE ADJACENT DUPLICATES FROM result COMPARING ALL FIELDS.

  ENDMETHOD.

  METHOD rba_Roles.
    LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<LFS_roles_rba>) GROUP BY <LFS_roles_rba>-BusinessPartner.
      " Your code logic for Roles association read goes here
    ENDLOOP.
  ENDMETHOD.

  METHOD cba_Address.
    DATA:
      lt_businesspartner TYPE bapibus1006_head,
      lt_addressdata     TYPE bapibus1006_address,
      lv_ADDRESSGUID     TYPE but020-guid,

      lt_travel          TYPE bapibus1006_head-bpartner,

      lt_return          TYPE TABLE OF bapiret2
      .


    LOOP AT entities_cba ASSIGNING FIELD-SYMBOL(<LFS_BusinessPartner>).
      DATA(bpartner) = <LFS_BusinessPartner>-BusinessPartner.
      CALL FUNCTION 'BAPI_BUPA_GET_NUMBERS'
        EXPORTING
          businesspartner    = bpartner
        IMPORTING
          businesspartnerout = lt_travel
        TABLES
          return             = lt_return.
      map_bapi_messages(
        EXPORTING
          iv_cid         = <LFS_BusinessPartner>-%cid_ref
          IT_Messages    = lt_return
        IMPORTING
          EV_FailedAdded = DATA(failed_added)
        CHANGING
          CT_Failed      = failed-z_i_businesspartner
          CT_Reported    = reported-z_i_businesspartner
      ).
      IF failed_added = abap_true.
        LOOP AT <LFS_BusinessPartner>-%target ASSIGNING FIELD-SYMBOL(<LFS_Address>).

          map_message_assoc_to_address(
            EXPORTING
              iv_cid         = <LFS_Address>-%cid
              IV_IsDependend = abap_true
              IT_Messages    = lt_return
            CHANGING
              CT_Failed      = failed-address
              CT_Reported    = reported-address
          ).
        ENDLOOP.
      ELSE.
        LOOP AT <LFS_BusinessPartner>-%target ASSIGNING FIELD-SYMBOL(<LFS_Address_Detail>).
          " Data Address
          lt_addressdata-postl_cod2 = <LFS_Address_Detail>-PostCode2.
          lt_addressdata-region     = <LFS_Address_Detail>-Region.
          lt_addressdata-country    = <LFS_Address_Detail>-country.
          lt_addressdata-langu    = <LFS_Address_Detail>-langu.
          lt_addressdata-street    = <LFS_Address_Detail>-street.
          lt_addressdata-house_no    = <LFS_Address_Detail>-HouseNum1.
          lt_addressdata-postl_cod1    = <LFS_Address_Detail>-PostCode1.
          lt_addressdata-city    = <LFS_Address_Detail>-City1.
          lt_businesspartner-bpartner = bpartner.

          CALL FUNCTION 'BAPI_BUPA_ADDRESS_ADD'
            EXPORTING
              "BUSINESSPARTNER              = lt_businesspartner
              businesspartner = bpartner
              addressdata     = lt_addressdata
            IMPORTING
              addressguid     = lv_ADDRESSGUID
            TABLES
              return          = lt_return.
          map_message_assoc_to_address(
            EXPORTING
              iv_cid         = <LFS_Address_Detail>-%cid
              IT_Messages    = lt_return
            IMPORTING
              EV_FailedAdded = failed_added
            CHANGING
              CT_Failed      = failed-address
              CT_Reported    = reported-address
          ).
          IF failed_added = abap_false.
            INSERT
               VALUE #(
               %cid = <LFS_Address_Detail>-%cid
                BusinessPartner = bpartner
                Addrnumber = lv_ADDRESSGUID
              ) INTO TABLE mapped-address.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD cba_Roles.
    DATA:
      lt_businesspartner TYPE bapibus1006_head,
      lt_roles           TYPE TABLE OF bapibus1006_bproles,

      lt_return          TYPE bapiret2_t
      .

    LOOP AT entities_cba ASSIGNING FIELD-SYMBOL(<LFS_Roles_BusinessPartner>).
      DATA(lv_bpartner) = <LFS_Roles_BusinessPartner>-BusinessPartner.
      " Get existing roles for the business partner
      CALL FUNCTION 'BAPI_BUPA_ROLES_GET_2'
        EXPORTING
          businesspartner      = lv_bpartner
        TABLES
          businesspartnerroles = lt_roles
          return               = lt_return.

      map_bapi_messages(
        EXPORTING
          iv_cid         = <LFS_Roles_BusinessPartner>-%cid_ref
          IT_Messages    = lt_return
        IMPORTING
          EV_FailedAdded = DATA(lv_FailedAdded)
        CHANGING
          CT_Failed      = failed-z_i_businesspartner
          CT_Reported    = reported-z_i_businesspartner
      ).
      LOOP AT <LFS_Roles_BusinessPartner>-%target ASSIGNING FIELD-SYMBOL(<LFS_Roles>).
        IF <LFS_Roles>-BPRole IS INITIAL.

    CONTINUE. " Bỏ qua nếu không có Role
  ENDIF.
        READ TABLE lt_roles WITH KEY partnerrole = <LFS_Roles>-BPRole TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          " Role already exists, skip adding
          CONTINUE.

        ELSE.
          IF lv_FailedAdded = abap_false.
            CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
              EXPORTING
                businesspartner     = lv_bpartner
                businesspartnerrole = <LFS_Roles>-BPRole
                validfromdate       = sy-datum
              TABLES
                return              = lt_return.

            map_message_assoc_to_role(
              EXPORTING
                iv_cid         = <LFS_Roles>-%cid
                IV_IsDependend = abap_true
                IT_Messages    = lt_return
              IMPORTING
                EV_FailedAdded = lv_FailedAdded
              CHANGING
                CT_Failed      = failed-role
                CT_Reported    = reported-role
            ).
            IF lv_FailedAdded = abap_false.
              APPEND
                  VALUE #(
                      %cid =  <LFS_Roles>-%cid
                      BusinessPartner = lv_bpartner
                      BPRole = <LFS_Roles>-BPRole
                  ) TO mapped-Role.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD map_bapi_messages.
    EV_FailedAdded  = abap_false.

    LOOP AT IT_Messages INTO DATA(ls_message).

      IF ls_message-type = gc_msgtype_error OR ls_message-type = gc_msgtype_abort.
        IF ls_message-id = 'R11' AND ls_message-number = '579'.
          " Thông báo info lên UI – không phải lỗi
          APPEND VALUE #(
            %cid = iv_cid
            %msg = new_message(
                     severity = if_abap_behv_message=>severity-information
                     id = gc_msgid
                     number = '000'
                   )
            %element-BusinessPartnerGroup = if_abap_behv=>mk-on
          ) TO CT_Reported.

          CONTINUE.
        ELSE.
          APPEND VALUE #(
            %cid        = iv_cid
            %fail-cause = zcl_bp_aux=>get_cause_from_message(   " 👈 Gọi method phụ để ánh xạ
                            msgid = ls_message-id
                            msgno = ls_message-number )
          ) TO CT_Failed.
          EV_FailedAdded  = abap_true.
        ENDIF.
      ENDIF.

      " Luôn thêm vào REPORTED để hiện lên UI
      APPEND VALUE #(
        %cid = iv_cid
        %msg = new_message(
                  id       = ls_message-id
                  number   = ls_message-number
                  v1       = ls_message-message_v1
                  v2       = ls_message-message_v2
                  v3       = ls_message-message_v3
                  v4       = ls_message-message_v4
                  severity = if_abap_behv_message=>severity-error )
*                  severity = SWITCH #( ls_message-type
*                             WHEN 'I' THEN if_abap_behv_message=>severity-information
*                             WHEN 'W' THEN if_abap_behv_message=>severity-warning
*                             WHEN GC_MSGTYPE_ERROR OR GC_MSGTYPE_ABORT THEN if_abap_behv_message=>severity-error
*                             ELSE if_abap_behv_message=>severity-error ) )
        %element-BusinessPartnerGroup = if_abap_behv=>mk-on
      ) TO CT_Reported.

    ENDLOOP.
    " DEBUG INFO – append to REPORTED để trace
    APPEND VALUE #(
      %cid = iv_cid
      %msg = new_message(
                id       = gc_msgid
                number   = '999'
                v1       = ls_message-id
                v2 = ls_message-number
                severity = if_abap_behv_message=>severity-information )
      %element-BusinessPartner = if_abap_behv=>mk-on
    ) TO CT_Reported.

    IF EV_FailedAdded  = abap_false.

      APPEND VALUE #(
        %cid = iv_cid
        %msg = new_message_with_text(
                 severity = if_abap_behv_message=>severity-success
                 text     = |Đã tạo thành công Business Partner: 1112|
               )
      ) TO CT_Reported.

    ENDIF.

  ENDMETHOD.

  METHOD map_message_assoc_to_address.
    ASSERT iv_cid IS NOT INITIAL. " In a create case, the %cid has to be present
    EV_FailedAdded  = abap_false.
    LOOP AT IT_Messages INTO DATA(ls_message).
      IF ls_message-type = gc_msgtype_error OR ls_message-type = gc_msgtype_abort.
        APPEND VALUE #( %cid = iv_cid
                        %fail-cause = zcl_bp_aux=>get_cause_from_message(
                        msgid = ls_message-id
                        msgno = ls_message-number
                        is_dependend = IV_IsDependend
                        ) )
                TO CT_Failed.
        EV_FailedAdded  = abap_true.
      ENDIF.
      APPEND VALUE #(
         %cid = iv_cid
         %msg = new_message(
                   id       = ls_message-id
                   number   = ls_message-number
                   v1       = ls_message-message_v1
                   v2       = ls_message-message_v2
                   v3       = ls_message-message_v3
                   v4       = ls_message-message_v4
                   severity = if_abap_behv_message=>severity-error )
         ) TO CT_Reported.
    ENDLOOP.
  ENDMETHOD.
  METHOD map_message_assoc_to_role.
    ASSERT iv_cid IS NOT INITIAL. " In a create case, the %cid has to be present
    EV_FailedAdded  = abap_false.
    LOOP AT IT_Messages INTO DATA(ls_message).
      IF ls_message-type = gc_msgtype_error OR ls_message-type = gc_msgtype_abort.
        APPEND VALUE #( %cid = iv_cid
                        %fail-cause = zcl_bp_aux=>get_cause_from_message(
                        msgid = ls_message-id
                        msgno = ls_message-number
                        is_dependend = IV_IsDependend
                        ) )
                TO CT_Failed.
        EV_FailedAdded  = abap_true.
      ENDIF.
      APPEND VALUE #(
         %cid = iv_cid
         %msg = new_message(
                   id       = ls_message-id
                   number   = ls_message-number
                   v1       = ls_message-message_v1
                   v2       = ls_message-message_v2
                   v3       = ls_message-message_v3
                   v4       = ls_message-message_v4
                   severity = if_abap_behv_message=>severity-error )
         ) TO CT_Reported.
    ENDLOOP.
  ENDMETHOD.

  METHOD precheck_create.
    LOOP AT entities INTO DATA(ls_entity).

      " VALIDATE BUSINESSPARTNERCATEGORY
      CASE ls_entity-%data-BusinessPartnerCategory.
        WHEN gc_cat_person.
          IF ls_entity-%data-FirstName IS INITIAL OR
                ls_entity-%data-LastName IS INITIAL.
            APPEND VALUE #(
              %cid        = ls_entity-%cid
              %fail-cause = if_abap_behv=>cause-not_found
            ) TO failed-z_i_businesspartner.
            APPEND VALUE #(
              %cid = ls_entity-%cid
              %msg = new_message(
                       id       = gc_msgid
                       number   = '003'
                       v1       =  TEXT-001
                       severity = if_abap_behv_message=>severity-error )
              %element-FirstName = if_abap_behv=>mk-on
              %element-LastName  = if_abap_behv=>mk-on
            ) TO reported-z_i_businesspartner.
          ENDIF.
        WHEN gc_cat_organization.
          IF ls_entity-%data-OrganizationName1 IS INITIAL.
            APPEND VALUE #(
              %cid        = ls_entity-%cid
              %fail-cause = if_abap_behv=>cause-not_found
            ) TO failed-z_i_businesspartner.
            APPEND VALUE #(
              %cid = ls_entity-%cid
              %msg = new_message(
                       id       = gc_msgid
                       number   = '004'
                       v1       = TEXT-002
                       severity = if_abap_behv_message=>severity-error )
              %element-OrganizationName1 = if_abap_behv=>mk-on
            ) TO reported-z_i_businesspartner.
          ENDIF.
        WHEN '3'.
        WHEN OTHERS. " Invalid Category
          APPEND VALUE #(
               %cid        = ls_entity-%cid
               %fail-cause = if_abap_behv=>cause-not_found
             ) TO failed-z_i_businesspartner.
          APPEND VALUE #(
            %cid = ls_entity-%cid
            %msg = new_message(
                     id       = gc_msgid
                     number   = gc_msgnum_invalid_value
                     v1       = 'Business Partner Category'
                     v2       = '1,2,3'
                     severity = if_abap_behv_message=>severity-error )
            %element-BusinessPartnerCategory = if_abap_behv=>mk-on
          ) TO reported-z_i_businesspartner.
      ENDCASE.


      " VALIDATION BUSINESSPARTNERTYPE
      IF ls_entity-%data-BusinessPartnerKind IS INITIAL.
        APPEND VALUE #(
          %cid        = ls_entity-%cid
          %fail-cause = if_abap_behv=>cause-not_found
        ) TO failed-z_i_businesspartner.
        APPEND VALUE #(
          %cid = ls_entity-%cid
          %msg = new_message(
                   id       = gc_msgid
                   number   = gc_msgnum_field_missing
                   v1       = 'Business Partner Type'
                   severity = if_abap_behv_message=>severity-error )
          %element-BusinessPartnerKind = if_abap_behv=>mk-on
        ) TO reported-z_i_businesspartner.
      ELSE.
        SELECT bpkind
          FROM tb004
          INTO @DATA(lv_bpkind)
          WHERE bpkind = @ls_entity-%data-BusinessPartnerKind.
        ENDSELECT.
        IF lv_bpkind IS INITIAL.
          APPEND VALUE #(
            %cid        = ls_entity-%cid
            %fail-cause = if_abap_behv=>cause-not_found
          ) TO failed-z_i_businesspartner.
          APPEND VALUE #(
            %cid = ls_entity-%cid
            %msg = new_message(
                     id       = gc_msgid
                     number   = gc_msgnum_not_in_customizing
                     v1       = 'Business Partner Type'
                     v2       = 'TB004'
                     severity = if_abap_behv_message=>severity-error )
            %element-BusinessPartnerKind = if_abap_behv=>mk-on
          ) TO reported-z_i_businesspartner.
        ENDIF.
      ENDIF.
      " VAlidation Business Partner Grouping
      IF ls_entity-%data-BusinessPartnerGroup IS INITIAL.
        APPEND VALUE #(
          %cid        = ls_entity-%cid
          %fail-cause = if_abap_behv=>cause-not_found
        ) TO failed-z_i_businesspartner.
        APPEND VALUE #(
          %cid = ls_entity-%cid
          %msg = new_message(
                   id       = gc_msgid
                   number   = gc_msgnum_field_missing
                   v1       = 'Business Partner Group'
                   v2       = 'TB002'
                   severity = if_abap_behv_message=>severity-error )
          %element-BusinessPartnerGroup = if_abap_behv=>mk-on
        ) TO reported-z_i_businesspartner.
      ELSE.
        SELECT bu_group
          FROM tb002
          INTO @DATA(lv_BU_GROUP)
          WHERE bu_group = @ls_entity-%data-BusinessPartnerGroup.
        ENDSELECT.
        IF lv_BU_GROUP IS INITIAL.
          APPEND VALUE #(
            %cid        = ls_entity-%cid
            %fail-cause = if_abap_behv=>cause-not_found
          ) TO failed-z_i_businesspartner.
          APPEND VALUE #(
            %cid = ls_entity-%cid
            %msg = new_message(
                     id       = gc_msgid
                     number   = gc_msgnum_not_in_customizing
                     v1       = 'Business Partner Group'
                     v2       = 'TB002'
                     severity = if_abap_behv_message=>severity-error )
            %element-BusinessPartnerGroup = if_abap_behv=>mk-on
          ) TO reported-z_i_businesspartner.
        ENDIF.
      ENDIF.
      " Validation Academic Title (Key)
      IF ls_entity-%data-AcademicTitle IS NOT INITIAL.
        SELECT title_key
          FROM tsad2
          INTO @DATA(lv_title_key)
          WHERE title_key = @ls_entity-%data-AcademicTitle.
        ENDSELECT.
        IF lv_title_key IS INITIAL.
          APPEND VALUE #(
            %cid        = ls_entity-%cid
            %fail-cause = if_abap_behv=>cause-not_found
          ) TO failed-z_i_businesspartner.
          APPEND VALUE #(
            %cid = ls_entity-%cid
            %msg = new_message(
                     id       = gc_msgid
                     number   = gc_msgnum_not_in_customizing
                     v1       = 'Academic Title'
                     v2       = 'TSAD2'
                     severity = if_abap_behv_message=>severity-error )
            %element-AcademicTitle = if_abap_behv=>mk-on
          ) TO reported-z_i_businesspartner.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD precheck_cba_Roles.
    LOOP AT entities INTO DATA(ls_entity).
      LOOP AT ls_entity-%target INTO DATA(ls_role).
        " Validate Region exists in TB003 for the given Country
        IF ls_role-BPRole IS NOT INITIAL.
          SELECT role
          FROM tb003
          INTO @DATA(lv_role)
          WHERE role = @ls_role-BPRole.
          ENDSELECT.
          IF lv_role IS INITIAL.
            APPEND VALUE #(
                %cid        = ls_role-%cid
                %fail-cause = if_abap_behv=>cause-not_found
            ) TO failed-role.
            APPEND VALUE #(
                %cid = ls_role-%cid
                %msg = new_message(
                         id       = gc_msgid
                         number   = gc_msgnum_not_in_customizing
                         v1       = 'Business Partner Role'
                         v2       = 'TB003'
                         severity = if_abap_behv_message=>severity-error )
                %element-BPRole = if_abap_behv=>mk-on
            ) TO reported-role.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD precheck_cba_Address.
    LOOP AT entities INTO DATA(ls_entity).
      LOOP AT ls_entity-%target INTO DATA(ls_address).
        IF ls_address-Country IS INITIAL.
          APPEND VALUE #(
            %cid        = ls_address-%cid
            %fail-cause = if_abap_behv=>cause-not_found
          ) TO failed-address.
          APPEND VALUE #(
            %cid = ls_address-%cid
            %msg = new_message(
                     id       = gc_msgid
                     number   = gc_msgnum_field_missing
                     v1       = 'Country'
                     severity = if_abap_behv_message=>severity-error )
            %element-Country = if_abap_behv=>mk-on
          ) TO reported-address.
        ELSE.
          " Validate Country exists in T005
          SELECT land1
          FROM t005
          INTO @DATA(lv_land1)
          WHERE land1 = @ls_address-Country.
          ENDSELECT.
          IF lv_land1 IS INITIAL.
            APPEND VALUE #(
                %cid        = ls_address-%cid
                %fail-cause = if_abap_behv=>cause-not_found
            ) TO failed-address.
            APPEND VALUE #(
                %cid = ls_address-%cid
                %msg = new_message(
                         id       = gc_msgid
                         number   = gc_msgnum_not_in_customizing
                         v1       = 'Country'
                         v2       = 'T005'
                         severity = if_abap_behv_message=>severity-error )
                %element-Country = if_abap_behv=>mk-on
            ) TO reported-address.
          ENDIF.
        ENDIF.
        IF ls_address-Region IS NOT INITIAL.
          " Validate Region exists in T005s for the given Country
          SELECT bland
          FROM t005S
          INTO @DATA(lv_region)
          WHERE land1 = @ls_address-Country
            AND bland  = @ls_address-Region.
          ENDSELECT.
          IF lv_region IS INITIAL.
            APPEND VALUE #(
                %cid        = ls_address-%cid
                %fail-cause = if_abap_behv=>cause-not_found
            ) TO failed-address.
            APPEND VALUE #(
                %cid = ls_address-%cid
                %msg = new_message(
                         id       = gc_msgid
                         number   = gc_msgnum_not_in_customizing
                         v1       = 'Region'
                         v2       = 'T005s'
                         severity = if_abap_behv_message=>severity-error )
                %element-Region = if_abap_behv=>mk-on
            ) TO reported-address.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


CLASS lsc_Z_I_BUSINESSPARTNER DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_Z_I_BUSINESSPARTNER IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
