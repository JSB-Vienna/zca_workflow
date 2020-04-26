CLASS zcx_ca_wf_utils DEFINITION
  PUBLIC
  INHERITING FROM zcx_ca_param
  CREATE PUBLIC .

  PUBLIC SECTION.


    CONSTANTS:
      BEGIN OF zcx_ca_wf_utils,
        msgid TYPE symsgid VALUE 'ZCA',
        msgno TYPE symsgno VALUE '077',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ca_wf_utils .
    CONSTANTS:
      BEGIN OF no_act_wis_found,
        msgid TYPE symsgid VALUE 'ZCA',
        msgno TYPE symsgno VALUE '078',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_act_wis_found .
    CONSTANTS:
      BEGIN OF no_unique_result,
        msgid TYPE symsgid VALUE 'ZCA',
        msgno TYPE symsgno VALUE '079',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_unique_result .
    CONSTANTS:
      BEGIN OF act_wi_in_stat_err,
        msgid TYPE symsgid VALUE 'ZCA',
        msgno TYPE symsgno VALUE '080',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF act_wi_in_stat_err .
    CONSTANTS:
      BEGIN OF no_wi_found,
        msgid TYPE symsgid VALUE 'ZCA',
        msgno TYPE symsgno VALUE '083',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_wi_found .
    CONSTANTS:
      BEGIN OF server_loc_incompl,
        msgid TYPE symsgid VALUE 'ZCA',
        msgno TYPE symsgno VALUE '108',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF server_loc_incompl .
    CONSTANTS:
      BEGIN OF no_alias_found,
        msgid TYPE symsgid VALUE '/IWBEP/CM_V4_RUNTIME',
        msgno TYPE symsgno VALUE '112',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE 'MV_MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_alias_found .

    "! <p class="shorttext synchronized" lang="en">My own name</p>
    CONSTANTS c_zcx_ca_wf_utils TYPE seoclsname VALUE 'ZCX_CA_WF_UTILS' ##NO_TEXT.

    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    METHODS constructor
      IMPORTING
        !textid    LIKE if_t100_message=>t100key OPTIONAL
        !previous  LIKE previous OPTIONAL
        !mt_return TYPE bapiret2_t OPTIONAL
        !mv_subrc  TYPE syst_subrc OPTIONAL
        !mv_msgty  TYPE syst_msgty OPTIONAL
        !mv_msgv1  TYPE syst_msgv OPTIONAL
        !mv_msgv2  TYPE syst_msgv OPTIONAL
        !mv_msgv3  TYPE syst_msgv OPTIONAL
        !mv_msgv4  TYPE syst_msgv OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_ca_wf_utils IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous  = previous
        mt_return = mt_return
        mv_subrc  = mv_subrc
        mv_msgty  = mv_msgty
        mv_msgv1  = mv_msgv1
        mv_msgv2  = mv_msgv2
        mv_msgv3  = mv_msgv3
        mv_msgv4  = mv_msgv4.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
