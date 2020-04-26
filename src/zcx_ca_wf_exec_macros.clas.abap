CLASS zcx_ca_wf_exec_macros DEFINITION
  PUBLIC
  INHERITING FROM zcx_ca_param
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">My own name</p>
    CONSTANTS c_zcx_ca_wf_exec_macros TYPE seoclsname VALUE 'ZCX_CA_WF_EXEC_MACROS' ##NO_TEXT.

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



CLASS zcx_ca_wf_exec_macros IMPLEMENTATION.


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
