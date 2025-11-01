CLASS lhc_Address DEFINITION INHERITING FROM CL_ABAP_BEHAVIOR_HANDLER.
  PRIVATE SECTION.

    METHODS UPDATE FOR MODIFY
      IMPORTING ENTITIES FOR UPDATE Address.

    METHODS DELETE FOR MODIFY
      IMPORTING KEYS FOR DELETE Address.

    METHODS READ FOR READ
      IMPORTING KEYS FOR READ Address RESULT RESULT.

    METHODS rba_Businesspartner FOR READ
      IMPORTING KEYS_RBA FOR READ Address\_Businesspartner FULL RESULT_REQUESTED RESULT RESULT LINK ASSOCIATION_LINKS.


     TYPES tt_address_failed TYPE TABLE FOR FAILED Z_I_BP_ADDRESS.
     TYPES tt_address_reported TYPE taBLE FOR REPORTED Z_I_BP_ADDRESS.

     METHODS MAP_MESSAGE
        IMPORTING
            cid TYPE STRING OPTIONAL
            businesspartner_id TYPE AD_ADDRNUM
            messages TYPE BAPIRET2_T
        EXPORTING
            failed_added TYPE ABAP_BOOL
        CHANGING
            failed TYPE tt_address_failed
            reported TYPE tt_address_reported.
ENDCLASS.

CLASS lhc_Address IMPLEMENTATION.

  METHOD UPDATE.
  ENDMETHOD.

  METHOD DELETE.
  ENDMETHOD.

  METHOD READ.
  ENDMETHOD.

  METHOD rba_Businesspartner.
  ENDMETHOD.

  METHOD MAP_MESSAGE.
  failed_added = abap_false.
    LOOP AT MESSAGES INTO DATA(LS_MSG).
        IF LS_MSG-TYPE = 'E' or LS_MSG-TYPE = 'A'.
            APPEND VALUE #( %cid = cid
                            businesspartner = businesspartner_id
                            %fail-cause = ZCL_BP_AUX=>GET_CAUSE_FROM_MESSAGE(
                            MSGID = ls_msg-ID
                            msgno = ls_msg-NUMBER
                            ) )
                   TO failed.
       failed_added = abap_true.
        ENDIF.
        APPEND VALUE #( %msg = NEW_MESSAGE(
                        id = ls_msg-ID
                        number = ls_msg-NUMBER
                        severity = IF_ABAP_BEHV_MESSAGE=>SEVERITY-ERROR
                        v1 = LS_MSG-MESSAGE_V1
                        v2 = ls_msg-MESSAGE_V2
                        v3 = ls_msg-MESSAGE_V3
                        v4 = ls_msg-MESSAGE_V4 )
                 %cid = cid
                 BUSINESSPARTNER = BUSINESSPARTNER_ID )
                  TO REPORTED.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
