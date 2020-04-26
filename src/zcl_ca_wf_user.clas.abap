"! <p class="shorttext synchronized" lang="en">BC extended user</p>
CLASS zcl_ca_wf_user DEFINITION PUBLIC
                     CREATE PROTECTED.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      bi_object,
      bi_persistent,
      if_workflow,
      if_xo_const_message,
      zif_ca_c_bool.

*   a l i a s e s
    ALIASES:
*     Boolean flags
      c_false              FOR zif_ca_c_bool~c_false,
      c_true               FOR zif_ca_c_bool~c_true,
*     BI_OBJECT methods
      default_attr_value   FOR bi_object~default_attribute_value,
      execute_def_method   FOR bi_object~execute_default_method,
      release              FOR bi_object~release,
*     BI_PERSISTENT methods
      find_by_lpor         FOR bi_persistent~find_by_lpor,
      lpor                 FOR bi_persistent~lpor,
      refresh              FOR bi_persistent~refresh.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Type Id</p>
      c_my_typeid          TYPE sibftypeid        VALUE 'ZCL_CA_WF_USER'  ##no_text.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     b u s i n e s s   o b j e c t s
      "! <p class="shorttext synchronized" lang="en">BO type USR01 - SAP user address data</p>
      mbo_usr01   TYPE sibflporb READ-ONLY VALUE zif_ca_c_wf_bos=>cbo_usr01 ##NO_TEXT,
      "! <p class="shorttext synchronized" lang="en">BO type USR01 - SAP user master data / methods</p>
      mbo_user    TYPE sibflporb READ-ONLY VALUE zif_ca_c_wf_bos=>cbo_user ##NO_TEXT,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">BAPI reference structure for addresses (contact person)</p>
      ms_address  TYPE bapiaddr3 READ-ONLY,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Default attribute with prepared object key</p>
      mv_def_attr TYPE text80 READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">User Name in User Master Record</p>
      mv_user_id  TYPE xubname READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Personnel number</p>
      mv_pernr    TYPE pernr_d READ-ONLY.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Create instance</p>
      "!
      "! @parameter is_lpor      | <p class="shorttext synchronized" lang="en">Business object/class key</p>
      "! @parameter iv_user_id   | <p class="shorttext synchronized" lang="en">User Name in User Master Record</p>
      "! @parameter iv_exist_chk | <p class="shorttext synchronized" lang="en">1 = Execute existence check</p>
      "! @parameter ro_instance  | <p class="shorttext synchronized" lang="en">Common WF object: Extended BO USR01 informations</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      get_instance
        IMPORTING
          is_lpor            TYPE sibflpor OPTIONAL
          iv_user_id         TYPE xubname DEFAULT sy-uname
          iv_exist_chk       TYPE dml_boolean DEFAULT c_true
            PREFERRED PARAMETER iv_user_id
        RETURNING
          VALUE(ro_instance) TYPE REF TO zcl_ca_wf_user
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Create instance for an workflow agent (type US only)</p>
      "!
      "! @parameter iv_wf_agent  | <p class="shorttext synchronized" lang="en">Workflow agent (type + user id)</p>
      "! @parameter iv_exist_chk | <p class="shorttext synchronized" lang="en">1 = Execute existence check</p>
      "! @parameter ro_instance  | <p class="shorttext synchronized" lang="en">Common WF object: BC extending BO USR01 + BO USER infos</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      get_instance_from_wf_agent
        IMPORTING
          iv_wf_agent        TYPE swp_agent
          iv_exist_chk       TYPE dml_boolean DEFAULT c_true
        RETURNING
          VALUE(ro_instance) TYPE REF TO zcl_ca_wf_user
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Remove instance from buffer</p>
      "!
      "! @parameter is_lpor      | <p class="shorttext synchronized" lang="en">Workflow instance key</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      remove
        IMPORTING
          is_lpor TYPE sibflpor
        RAISING
          zcx_ca_param.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
      "!
      "! @parameter iv_user_id   | <p class="shorttext synchronized" lang="en">User Name in User Master Record</p>
      "! @parameter iv_exist_chk | <p class="shorttext synchronized" lang="en">1 = Execute existence check</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      constructor
        IMPORTING
          iv_user_id   TYPE xubname
          iv_exist_chk TYPE dml_boolean
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Display object in standard transaction</p>
      display.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_e            FOR  if_xo_const_message~error,
      c_msgty_i            FOR  if_xo_const_message~info,
      c_msgty_s            FOR  if_xo_const_message~success,
      c_msgty_w            FOR  if_xo_const_message~warning.
**     Application log
*      c_log_object         FOR  zif_xx_c_name~c_log_object,
*      c_log_subobj         FOR  zif_xx_c_name~c_log_subobj.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Common WF object: Execution of workflow macros</p>
      mo_wf_macros   TYPE REF TO zcl_ca_wf_exec_macros,
*      "! <p class="shorttext synchronized" lang="en">Common object: Application log (BAL)</p>
*      mo_log        TYPE REF TO zcl_ca_log,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Workflow instance key</p>
      ms_lpor        TYPE sibflpor,
      "! <p class="shorttext synchronized" lang="en">USR01-Instance to execute/determine of methods/attribs.</p>
      mbo_usr01_inst TYPE obj_record,
      "! <p class="shorttext synchronized" lang="en">USER-Instance to execute/determine of methods/attribs.</p>
      mbo_user_inst  TYPE obj_record.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Check existence of object</p>
      "!
      "! @parameter iv_user_id   | <p class="shorttext synchronized" lang="en">User Name in User Master Record</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      check_existence
        IMPORTING
          iv_user_id TYPE xubname
        RAISING
          zcx_ca_dbacc
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Create instance of BO USER object</p>
      "!
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">Common WF exception: Macro execution errors</p>
      create_bo_user_instance
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Create instance of BO USR01 object</p>
      "!
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">Common WF exception: Macro execution errors</p>
      create_bo_usr01_instance
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Set personnel number late</p>
      "!
      "! @parameter iv_pernr | <p class="shorttext synchronized" lang="en">Personnel number</p>
      set_pernr
        IMPORTING
          iv_pernr TYPE pernr_d.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   l o c a l   t y p e   d e f i n i t i o n
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Buffered instance</p>
      BEGIN OF ty_s_buffer.
        INCLUDE TYPE sibflpor AS s_key.
      TYPES:
        o_instance TYPE REF TO zcl_ca_wf_user,
      END   OF ty_s_buffer,
      "! <p class="shorttext synchronized" lang="en">Instance buffer</p>
      ty_t_buffer TYPE SORTED TABLE OF ty_s_buffer
                                   WITH UNIQUE KEY instid.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Instance buffer</p>
      mt_buffer     TYPE ty_t_buffer.
ENDCLASS.



CLASS zcl_ca_wf_user IMPLEMENTATION.

  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_lpor    TYPE sibflpor,
      lv_user_id TYPE xubname.

*    TRY.
    IF is_lpor IS NOT INITIAL.
      "Set key into structured definition
      IF is_lpor-instid IS NOT INITIAL.  "Avoid destruction of type conform initial values
        lv_user_id = CONV #( is_lpor-instid ).
      ENDIF.
      ls_lpor  = is_lpor.
      "Set these values in any case, e. g. to create/get an instance only with the key string
      ls_lpor-typeid = zcl_ca_wf_user=>c_my_typeid.
      ls_lpor-catid  = swfco_objtype_cl.

    ELSEIF iv_user_id IS NOT INITIAL.
      lv_user_id = iv_user_id.
      ls_lpor = VALUE #( instid = CONV #( iv_user_id )
                         typeid = zcl_ca_wf_user=>c_my_typeid
                         catid  = swfco_objtype_cl ).

    ELSE.
      "At least one of the following parameters must be passed: &1 &2 &3 &4
      RAISE EXCEPTION TYPE zcx_ca_param
        EXPORTING
          textid   = zcx_ca_param=>at_least_one
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IS_LPOR'
          mv_msgv2 = 'IV_USER_ID'
          mv_msgv3 = space
          mv_msgv4 = space ##no_text.
    ENDIF.

    "If approval id is still not available create no instance
    IF lv_user_id IS INITIAL.
      RETURN.
    ENDIF.

    "Is an instance already created?
    READ TABLE mt_buffer INTO DATA(ls_buffer)
                         WITH KEY s_key = ls_lpor.
    IF sy-subrc EQ 0.
      "Refresh some data
      ls_buffer-o_instance->refresh( ).

    ELSE.
*          "Create application log to be able to log other creation errors here
*          DATA(lo_log) = zcl_ca_log=>get_instance_from_buffer(
*                                               iv_object = c_log_object
*                                               iv_subobj = c_log_subobj
*                                               is_lpor   = CORRESPONDING #( ls_lpor ) ).

      "Create instance of payment approval object
      ls_buffer-o_instance = NEW #( iv_user_id   = lv_user_id
                                    iv_exist_chk = iv_exist_chk ).
*                                    io_log   = lo_log ).

      ls_buffer-s_key = ls_buffer-o_instance->lpor( ).
      INSERT ls_buffer INTO TABLE mt_buffer.
    ENDIF.

    ro_instance = ls_buffer-o_instance.

*      CATCH zcx_ca_error INTO DATA(lx_error).
*        IF lo_log IS BOUND.
*          lo_log->add_msg_exc( ix_excep = lx_error
*                               iv_all   = c_true ).
*          lo_log->save( iv_commit      = c_false   "Commit by WF runtime environment
*                        iv_in_upd_task = c_true ).
*        ENDIF.
*    ENDTRY.
  ENDMETHOD.                    "get_instance


  METHOD get_instance_from_wf_agent.
    "-----------------------------------------------------------------*
    "   Get instance from workflow agent (type US only)
    "-----------------------------------------------------------------*
    IF iv_wf_agent(2) NE swfco_org_user.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_param
        EXPORTING
          textid   = zcx_ca_param=>param_invalid
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IV_WF_AGENT(2)'
          mv_msgv2 = CONV #( iv_wf_agent(2) ).
    ENDIF.

    ro_instance = zcl_ca_wf_user=>get_instance( iv_user_id   = iv_wf_agent+2(12)
                                                iv_exist_chk = iv_exist_chk ).
  ENDMETHOD.                    "get_instance_from_wf_agent


  METHOD remove.
    "-----------------------------------------------------------------*
    "   Remove instance from buffer
    "-----------------------------------------------------------------*
    IF is_lpor IS INITIAL.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_param
        EXPORTING
          textid   = zcx_ca_param=>param_invalid
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IS_LPOR'
          mv_msgv2 = 'INITIAL' ##no_text.
    ENDIF.

    DELETE mt_buffer WHERE s_key EQ is_lpor.
  ENDMETHOD.                    "remove


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    IF iv_user_id IS INITIAL.
      RETURN.
    ENDIF.

    "Check existence of object
    DATA(lv_user_id) = iv_user_id.        "For testing purposes only
    IF iv_exist_chk EQ c_true.
      me->check_existence( lv_user_id ).
    ENDIF.

    me->ms_lpor-typeid = c_my_typeid.
    me->ms_lpor-catid  = swfco_objtype_cl.

    "Complete and keep several attributes
    me->ms_lpor-instid = me->mv_user_id = lv_user_id.

    me->mo_wf_macros     = NEW #( ).
    me->mbo_usr01-instid = lv_user_id.
    me->mbo_user-instid  = lv_user_id.

    "Set default attribute = resolved into readable key
    me->default_attr_value( ).
  ENDMETHOD.                    "constructor


  METHOD check_existence.
    "-----------------------------------------------------------------*
    "   Check existence of object
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_return  TYPE bapiret2_t.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username      = iv_user_id
        cache_results = abap_false
      IMPORTING
        address       = me->ms_address
      TABLES
        return        = lt_return.

    IF line_exists( lt_return[ type   = c_msgty_e
                               id     = '01'
                               number = '124' ] ).
      DATA(lx_dbacc) =
           CAST zcx_ca_dbacc(
                  zcx_ca_error=>create_exception(
                           iv_excp_cls = zcx_ca_dbacc=>c_zcx_ca_dbacc
                           iv_function = 'BAPI_USER_GET_DETAIL'
                           it_return   = lt_return ) )  ##no_text.
      IF lx_dbacc IS BOUND.
        RAISE EXCEPTION lx_dbacc.
      ENDIF.

    ELSE.
      DATA(lx_param) =
           CAST zcx_ca_param(
                  zcx_ca_error=>create_exception(
                           iv_excp_cls = zcx_ca_param=>c_zcx_ca_param
                           iv_function = 'BAPI_USER_GET_DETAIL'
                           it_return   = lt_return ) )  ##no_text.
      IF lx_param IS BOUND.
        RAISE EXCEPTION lx_param.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "check_existence


  METHOD display.
    "-----------------------------------------------------------------*
    "   Display object in standard transaction
    "-----------------------------------------------------------------*
    SET PARAMETER ID 'XUS' FIELD me->mv_user_id.
    CALL TRANSACTION 'SU01D' WITH AUTHORITY-CHECK
                              AND SKIP FIRST SCREEN.
  ENDMETHOD.                    "display


  METHOD set_pernr.
    "-----------------------------------------------------------------*
    "   Set personnel number late
    "-----------------------------------------------------------------*
    me->mv_pernr = iv_pernr.
  ENDMETHOD.                    "set_pernr


  METHOD create_bo_user_instance.
    "-----------------------------------------------------------------*
    "   Create instance of BO object
    "-----------------------------------------------------------------*
    IF me->mbo_user_inst IS INITIAL.
      me->mbo_user_inst = me->mo_wf_macros->create_bo(
                                         iv_bo_name  = me->mbo_user-typeid
                                         iv_bo_key   = me->mbo_user-instid ).
    ENDIF.
  ENDMETHOD.                    "create_bo_user_instance


  METHOD create_bo_usr01_instance.
    "-----------------------------------------------------------------*
    "   Create instance of BO object
    "-----------------------------------------------------------------*
    IF me->mbo_usr01_inst IS INITIAL.
      me->mbo_usr01_inst = me->mo_wf_macros->create_bo(
                                         iv_bo_name = me->mbo_usr01-typeid
                                         iv_bo_key  = me->mbo_usr01-instid ).
    ENDIF.
  ENDMETHOD.                    "create_bo_usr01_instance


  METHOD bi_object~default_attribute_value ##needed.
    "-----------------------------------------------------------------*
    "   Returns a description and/or prepared key of the object.
    "-----------------------------------------------------------------*
    me->mv_def_attr = |{ me->ms_address-fullname } ({ me->mv_user_id })|.
    result = REF #( me->mv_def_attr ).
  ENDMETHOD.                    "bi_object~default_attribute_value


  METHOD bi_object~execute_default_method ##needed.
    "-----------------------------------------------------------------*
    "   Execute default method
    "-----------------------------------------------------------------*
    me->display( ).
  ENDMETHOD.                    "bi_object~execute_default_method


  METHOD bi_object~release ##needed.
    "-----------------------------------------------------------------*
    "   Release instance
    "-----------------------------------------------------------------*

  ENDMETHOD.                    "bi_object~release


  METHOD bi_persistent~find_by_lpor.
    "-----------------------------------------------------------------*
    "   Create workflow / workitem instance
    "-----------------------------------------------------------------*
    TRY.
        result = CAST bi_persistent(
                         zcl_ca_wf_user=>get_instance( is_lpor = lpor ) ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        "As long as no exceptions are declared for this method, this is
        "currently the best solution.
        MESSAGE lx_catched TYPE c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "bi_persistent~find_by_lpor


  METHOD bi_persistent~lpor.
    "-----------------------------------------------------------------*
    "   Return instance key
    "-----------------------------------------------------------------*
    result = me->ms_lpor.
  ENDMETHOD.                    "bi_persistent~lpor


  METHOD bi_persistent~refresh ##needed.
    "-----------------------------------------------------------------*
    "   Refresh instance
    "-----------------------------------------------------------------*
    TRY.
        me->check_existence( me->mv_user_id ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        "As long as no exceptions are declared for this method, this is
        "currently the best solution.
        MESSAGE lx_catched TYPE c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "bi_persistent~refresh

ENDCLASS.



