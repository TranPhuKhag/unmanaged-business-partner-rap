CLASS ZCL_BP_AUX DEFINITION
  PUBLIC
  INHERITING FROM cl_abap_behv
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
   CLASS-METHODS get_cause_from_message
      IMPORTING
        msgid             TYPE symsgid
        msgno             TYPE symsgno
        is_dependend      TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(fail_cause) TYPE if_abap_behv=>t_fail_cause.
ENDCLASS.


CLASS ZCL_BP_AUX IMPLEMENTATION.
 METHOD get_cause_from_message.
    fail_cause = if_abap_behv=>cause-unspecific.

    IF msgid = 'YGRP01_SE1744'.
      CASE msgno.
        WHEN '001'. "Not found
          fail_cause = if_abap_behv=>cause-not_found.
        WHEN '002'. "Unauthorized
          fail_cause = if_abap_behv=>cause-unauthorized.
      ENDCASE.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
