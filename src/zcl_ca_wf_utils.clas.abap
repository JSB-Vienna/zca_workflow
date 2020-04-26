"! <p class="shorttext synchronized" lang="en">Common WF object: Utilities / Conversions</p>
CLASS zcl_ca_wf_utils DEFINITION PUBLIC
                                 FINAL
                                 CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_fsbp_const_range,
      if_xo_const_message,
      zif_ca_c_bool.

*   a l i a s e s
    ALIASES:
*     Boolean flags
      c_false              FOR  zif_ca_c_bool~c_false,
      c_true               FOR  zif_ca_c_bool~c_true.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Find active workitem (single task) to a workflow id</p>
      "!
      "! @parameter iv_wf_id        | <p class="shorttext synchronized" lang="en">Active WORKFLOWitem Id</p>
      "! @parameter rv_wi_id        | <p class="shorttext synchronized" lang="en">Active workitem Id</p>
      "! @raising   zcx_ca_wf_utils | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      find_active_wi_by_wf_id
        IMPORTING
          iv_wf_id        TYPE sww_wfid
        RETURNING
          VALUE(rv_wi_id) TYPE sww_wiid
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Get currently active workitem and its container</p>
      "!
      "! @parameter iv_raise_excep  | <p class="shorttext synchronized" lang="en">1=Raise exception if no WI was found; 0=Check result values</p>
      "! @parameter ev_wi_id        | <p class="shorttext synchronized" lang="en">Active workitem Id</p>
      "! @parameter eo_wi_cnt       | <p class="shorttext synchronized" lang="en">Container to active workitem</p>
      "! @raising   zcx_ca_wf_utils | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      get_active_wi_n_container
        IMPORTING
          iv_raise_excep TYPE dml_boolean DEFAULT c_true
        EXPORTING
          ev_wi_id       TYPE sww_wiid
          eo_wi_cnt      TYPE REF TO if_swf_cnt_container
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Check BAPI return parameter for errors</p>
      "!
      "! @parameter it_return | <p class="shorttext synchronized" lang="en">Table with messages after BAPI call</p>
      "! @parameter rv_msg    | <p class="shorttext synchronized" lang="en">Found message of type E, A or X, else it is empty</p>
      check_bapi_return_for_errors
        IMPORTING
          it_return     TYPE bapiret2_t
        RETURNING
          VALUE(rv_msg) TYPE bapi_msg,

      "! <p class="shorttext synchronized" lang="en">Convert user Id into actor (add US)</p>
      "!
      "! @parameter iv_uname | <p class="shorttext synchronized" lang="en">SAP user ID</p>
      "! @parameter rv_actor | <p class="shorttext synchronized" lang="en">Workflow actor with object type US</p>
      convert_user_2_actor
        IMPORTING
          iv_uname        TYPE xubname
        RETURNING
          VALUE(rv_actor) TYPE swp_agent,

      "! <p class="shorttext synchronized" lang="en">Convert WF actor into user Id (cut of org.type)</p>
      "!
      "! @parameter iv_actor | <p class="shorttext synchronized" lang="en">Workflow actor with object type US</p>
      "! @parameter rv_uname | <p class="shorttext synchronized" lang="en">SAP user ID</p>
      convert_actor_2_user
        IMPORTING
          iv_actor        TYPE swp_agent
        RETURNING
          VALUE(rv_uname) TYPE xubname,

      "! <p class="shorttext synchronized" lang="en">Wait some seonds for something, e. g for posting completed</p>
      "!
      "! @parameter iv_seconds   | <p class="shorttext synchronized" lang="en">Number of seconds to wait</p>
      "! @parameter rv_time_over | <p class="shorttext synchronized" lang="en">1 = Waiting time is over</p>
      wait_n_seconds
        IMPORTING
          iv_seconds          TYPE int2
        RETURNING
          VALUE(rv_time_over) TYPE dml_boolean,

      "! <p class="shorttext synchronized" lang="en">Create BOR Instance from key values</p>
      "!
      "! @parameter iv_type_id   | <p class="shorttext synchronized" lang="en">Name of BOR object</p>
      "! @parameter iv_key_1     | <p class="shorttext synchronized" lang="en">Key value 1</p>
      "! @parameter iv_key_2     | <p class="shorttext synchronized" lang="en">Key value 2</p>
      "! @parameter iv_key_3     | <p class="shorttext synchronized" lang="en">Key value 3</p>
      "! @parameter iv_key_4     | <p class="shorttext synchronized" lang="en">Key value 4</p>
      "! @parameter rbo_instance | <p class="shorttext synchronized" lang="en">BOR instance</p>
      create_bor_inst
        IMPORTING
          iv_type_id          TYPE sibftypeid
          iv_key_1            TYPE sibfinstid
          iv_key_2            TYPE sibfinstid OPTIONAL
          iv_key_3            TYPE sibfinstid OPTIONAL
          iv_key_4            TYPE sibfinstid OPTIONAL
        RETURNING
          VALUE(rbo_instance) TYPE sibflporb,

      "! <p class="shorttext synchronized" lang="en">Assemble base URL for access to Fiori Launchpad</p>
      "!
      "! @parameter iv_icf_node_path | <p class="shorttext synchronized" lang="en">Use constants /UI2/IF_START_URL=&gt&CO_F*</p>
      "! @parameter rv_url           | <p class="shorttext synchronized" lang="en">URL for branch into Fiori inbox</p>
      "! @raising   zcx_ca_conv      | <p class="shorttext synchronized" lang="en">Common exception: Conversion failed</p>
      "! @raising   zcx_ca_wf_utils  | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      assemble_base_url_launchpad
        IMPORTING
          iv_icf_node_path TYPE string DEFAULT /ui2/if_start_url=>co_flp
        RETURNING
          VALUE(rv_url)    TYPE string
        RAISING
          zcx_ca_conv
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Assemble URL for Fiori inbox displaying all work items</p>
      "!
      "! @parameter rv_url           | <p class="shorttext synchronized" lang="en">URL for branch into Fiori inbox</p>
      "! @raising   zcx_ca_wf_utils  | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      assemble_url_fiori_inbox_all
        RETURNING
          VALUE(rv_url) TYPE string
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Assemble URL for Fiori inbox displaying all work items</p>
      "!
      "! @parameter iv_wi_id         | <p class="shorttext synchronized" lang="en">Workitem Id</p>
      "! @parameter rv_url           | <p class="shorttext synchronized" lang="en">URL for branch into Fiori inbox</p>
      "! @raising   zcx_ca_wf_utils  | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      assemble_url_fiori_for_wi
        IMPORTING
          iv_wi_id      TYPE sww_wiid
        RETURNING
          VALUE(rv_url) TYPE string
        RAISING
          zcx_ca_wf_utils.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   a l i a s e s
    ALIASES:
      c_opt_eq             FOR if_fsbp_const_range~option_equal,
      c_sign_e             FOR if_fsbp_const_range~sign_exclude,
      c_sign_i             FOR if_fsbp_const_range~sign_include,
*     Message types
      c_msgty_e            FOR  if_xo_const_message~error,
      c_msgty_i            FOR  if_xo_const_message~info,
      c_msgty_s            FOR  if_xo_const_message~success,
      c_msgty_w            FOR  if_xo_const_message~warning.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Parameter to call specific workitem</p>
      c_link_fiori_spec_wi TYPE string            VALUE `&/detail/&1/&2/TaskCollection(SAP__Origin='&1',InstanceID='&2')` ##no_text.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
**     o b j e c t   r e f e r e n c e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      mo_...               TYPE REF TO x..
*
**     d a t a   r e f e r e n c e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      mr_...               TYPE REF TO x..
*
**     t a b l e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      mt_...               TYPE x..
*
**     s t r u c t u r e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      ms_...               TYPE x..

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Assembled URL</p>
      mv_url               TYPE string.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Add further parameter to current URL</p>
      "!
      "! @parameter iv_name     | <p class="shorttext synchronized" lang="en">Parameter name</p>
      "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">Unconverted parameter value</p>
      "! @parameter iv_curr_url | <p class="shorttext synchronized" lang="en">Current URL, otherwise kept in class</p>
      "! @parameter rv_url      | <p class="shorttext synchronized" lang="en">Completetd URL</p>
      "! @raising   zcx_ca_conv | <p class="shorttext synchronized" lang="en">Common exception: Conversion failed</p>
      add_param_to_fiori_url
        IMPORTING
          iv_name       TYPE string
          iv_value      TYPE data
          iv_curr_url   TYPE string             OPTIONAL
        RETURNING
          VALUE(rv_url) TYPE string
        RAISING
          zcx_ca_conv.

ENDCLASS.


CLASS zcl_ca_wf_utils IMPLEMENTATION.

  METHOD find_active_wi_by_wf_id.
    "-----------------------------------------------------------------*
    "   Find active workitem (single task) to a workflow id
    "-----------------------------------------------------------------*
    DATA(lo_dep_wis) = cl_swf_utl_get_dependant_wis=>get_instance( iv_wf_id ).
    lo_dep_wis->set_status_filter(
                        VALUE #(   sign   = c_sign_e
                                   option = c_opt_eq
                                 ( low    = swfco_wi_status_waiting )
                                 ( low    = swfco_wi_status_ready )
                                 ( low    = swfco_wi_status_selected )
                                 ( low    = swfco_wi_status_started )
                                 ( low    = swfco_wi_status_committed )
                                 ( low    = swfco_wi_status_error ) ) ).
    lo_dep_wis->set_witype_filter(
                        VALUE #(   sign   = c_sign_e
                                   option = c_opt_eq
                                 ( low    = swfco_wi_normal )
                                 ( low    = swfco_wi_batch ) ) ).
    DATA(lt_act_wis) = lo_dep_wis->get_workitems( ).

    CASE lines( lt_act_wis ).
      WHEN 0.
        "Kein aktive Workflowaufgabe zu Workflow Id &MV_MSGV1& gefunden
        RAISE EXCEPTION TYPE zcx_ca_wf_utils
          EXPORTING
            textid   = zcx_ca_wf_utils=>no_act_wis_found
            mv_msgv1 = CONV #( |{ iv_wf_id ALPHA = OUT }| ).

      WHEN 1.
        "Everything is fine

      WHEN OTHERS.
        "Kein eindeutiges Ergebnis zu Workflow Id &MV_MSGV1& gefunden
        RAISE EXCEPTION TYPE zcx_ca_wf_utils
          EXPORTING
            textid   = zcx_ca_wf_utils=>no_unique_result
            mv_msgv1 = CONV #( |{ iv_wf_id ALPHA = OUT }| ).
    ENDCASE.

    "Status prüfen und ggfs. Meldung ausgeben
    DATA(ls_act_wi) = lt_act_wis[ 1 ].

    IF ls_act_wi-wi_stat EQ swfco_wi_status_error.
      "Found active workitem is in status 'ERROR'
      RAISE EXCEPTION TYPE zcx_ca_wf_utils
        EXPORTING
          textid = zcx_ca_wf_utils=>act_wi_in_stat_err.
    ENDIF.

    rv_wi_id = ls_act_wi-wi_id.
  ENDMETHOD.                    "find_active_wi_by_wf_id


  METHOD get_active_wi_n_container.
    "-----------------------------------------------------------------*
    "   Get currently active workitem and its container
    "-----------------------------------------------------------------*
    CLEAR: ev_wi_id,
           eo_wi_cnt.

    cl_swf_evt_requester=>get_workitem(
                                  IMPORTING
                                    ex_workitem_id = DATA(lv_wi_id) ).
    ev_wi_id = CONV #( lv_wi_id ).

    "The value is also initial, if it is called in a side methods of a workflow step!!
    IF ev_wi_id IS INITIAL.
      CASE iv_raise_excep.
        WHEN c_false.
          RETURN.

        WHEN c_true.
          "Currently no workitem is executed or is called by a side method
          RAISE EXCEPTION TYPE zcx_ca_wf_utils
            EXPORTING
              textid   = zcx_ca_wf_utils=>no_wi_found
              mv_msgty = 'I' ##no_text.
      ENDCASE.
    ENDIF.

    TRY.
        eo_wi_cnt = zcl_ca_wf_wapi_utils=>read_container( iv_wi_id = ev_wi_id ).

      CATCH zcx_ca_param
            cx_swf_ifs_exception INTO DATA(lx_error).
        IF iv_raise_excep EQ c_true.
          DATA(lx_wf_utils) =
                CAST zcx_ca_wf_utils(
                         zcx_ca_error=>create_exception(
                                  iv_excp_cls   = zcx_ca_wf_utils=>c_zcx_ca_wf_utils
                                  iv_class      = 'ZCL_CA_WF_WAPI_UTILS'
                                  iv_method     = 'READ_CONTAINER'
                                  ix_error      = lx_error ) )  ##no_text.
          IF lx_wf_utils IS BOUND.
            RAISE EXCEPTION lx_wf_utils.
          ENDIF.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_active_wi_n_container


  METHOD check_bapi_return_for_errors.
    "-----------------------------------------------------------------*
    "   Check BAPI return parameter for errors
    "-----------------------------------------------------------------*
    CLEAR rv_msg.
    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<ls_return>)
                      WHERE type CA 'EAX' ##no_text.
      EXIT.
    ENDLOOP.
    IF sy-subrc EQ 0.
      rv_msg = <ls_return>-message.
    ENDIF.
  ENDMETHOD.                    "check_bapi_return_for_errors


  METHOD convert_user_2_actor.
    "-----------------------------------------------------------------*
    "   Convert user Id into actor (add US)
    "-----------------------------------------------------------------*
    rv_actor = |{ swfco_org_user }{ iv_uname }|.
  ENDMETHOD.                    "convert_user_2_actor


  METHOD convert_actor_2_user.
    "-----------------------------------------------------------------*
    "   Convert WF actor into user Id (cut of org.type)
    "-----------------------------------------------------------------*
    IF iv_actor(2) EQ swfco_org_user.
      rv_uname = iv_actor+2.
    ENDIF.
  ENDMETHOD.                    "convert_actor_2_user


  METHOD wait_n_seconds.
    "-----------------------------------------------------------------*
    "   Wait some seonds for something, e. g for posting completed
    "-----------------------------------------------------------------*
    WAIT UP TO iv_seconds SECONDS.
    rv_time_over = c_true.
  ENDMETHOD.                    "wait_n_seconds


  METHOD create_bor_inst.
    "-----------------------------------------------------------------*
    "   Create BOR Instance from key values
    "-----------------------------------------------------------------*
    rbo_instance =
          VALUE #( catid  = swfco_objtype_bor
                   typeid = iv_type_id
                   instid = |{ iv_key_1 }{ iv_key_2 }{ iv_key_3 }{ iv_key_4 }| ).
  ENDMETHOD.                    "create_bor_inst


  METHOD assemble_base_url_launchpad.
    "-----------------------------------------------------------------*
    "   Assemble base URL for access to Fiori Launchpad
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_param       TYPE string,
      lv_protocol    TYPE string,
      lv_host        TYPE string,
      lv_port        TYPE string,
      lv_langu       TYPE laiso,
      lv_access_mode TYPE c.

    CLEAR mv_url.
    cl_http_server=>get_location(
                            EXPORTING
                              application  = iv_icf_node_path
                            IMPORTING
                              host         = lv_host
                              port         = lv_port
                              out_protocol = lv_protocol ).
    IF lv_protocol IS INITIAL OR
       lv_host     IS INITIAL OR
       lv_port     IS INITIAL.
      "Server location could not be determined / assembled
      RAISE EXCEPTION TYPE zcx_ca_wf_utils
        EXPORTING
          textid   = zcx_ca_wf_utils=>server_loc_incompl
          mv_msgty = c_msgty_e
          mv_msgv1 = CONV #( iv_icf_node_path ).
    ENDIF.

    mv_url = |{ lv_protocol }://{ lv_host }:{ lv_port }{ iv_icf_node_path }|.
    TRANSLATE mv_url TO LOWER CASE.

    add_param_to_fiori_url( iv_name = `sap-client`
                   iv_value = sy-mandt ) ##no_text.

    add_param_to_fiori_url( iv_name  = `sap-language`
                   iv_value = sy-langu ) ##no_text.

    GET PARAMETER ID 'ACCESSIBILITY_MODE' FIELD lv_access_mode.
    IF lv_access_mode IS NOT INITIAL.
      add_param_to_fiori_url( iv_name  = `sap-accessibility`
                     iv_value = abap_true ) ##no_text.
    ENDIF.

    rv_url = mv_url.
  ENDMETHOD.                    "assemble_base_url_launchpad


  METHOD assemble_url_fiori_inbox_all.
    "-----------------------------------------------------------------*
    "   Assemble URL for Fiori inbox displaying all work items
    "-----------------------------------------------------------------*
    TRY.
        assemble_base_url_launchpad( ).

        add_param_to_fiori_url( iv_name  = `#WorkflowTask-displayInbox?allItems`
                       iv_value = `true` ) ##no_text.

        rv_url = mv_url.

      CATCH zcx_ca_conv INTO DATA(lx_catched).
        DATA(lx_error) =
             CAST zcx_ca_wf_utils(
                    zcx_ca_error=>create_exception(
                             iv_excp_cls = zcx_ca_wf_utils=>c_zcx_ca_wf_utils
                             ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "assemble_url_fiori_inbox_all


  METHOD assemble_url_fiori_for_wi.
    "-----------------------------------------------------------------*
    "   Assemble URL for Fiori inbox displaying all work items
    "-----------------------------------------------------------------*
    TRY.
        assemble_base_url_launchpad( ).

        add_param_to_fiori_url( iv_name  = `#WorkflowTask-displayInbox?allItems`
                       iv_value = `true` ) ##no_text.

        "Determine
        DATA(lo_dest_finder) = /iwfnd/cl_destin_finder=>get_destination_finder( ).
        DATA(lv_client)      = cl_abap_syst=>get_client( ).
        LOOP AT lo_dest_finder->get_system_aliases_list( ) ASSIGNING FIELD-SYMBOL(<ls_alias>)
                                                           WHERE software_version EQ '/IWPGW/BWF' ##no_text
                                                             AND target_sysid     EQ sy-sysid
                                                             AND target_client    EQ lv_client.

          DATA(lv_param) =
                     replace( val  = c_link_fiori_spec_wi
                              sub  = `&1`   with = <ls_alias>-system_alias
                              occ  = 0 ).  "= all occurrences
          lv_param = replace( val  = lv_param
                              sub  = `&2`   with = iv_wi_id
                              occ  = 0 ).  "= all occurrences
          EXIT.
        ENDLOOP.
        IF sy-subrc NE 0.
          "No system alias found for service group '&1' SID '&2' client '&3'
          RAISE EXCEPTION TYPE zcx_ca_wf_utils
            EXPORTING
              textid   = zcx_ca_wf_utils=>no_alias_found
              mv_msgty = c_msgty_e
              mv_msgv1 = '/IWPGW/BWF'
              mv_msgv2 = CONV #( sy-sysid )
              mv_msgv3 = CONV #( lv_client ) ##no_text.
        ENDIF.

        rv_url = mv_url = mv_url && lv_param.

      CATCH zcx_ca_conv INTO DATA(lx_catched).
        DATA(lx_error) =
             CAST zcx_ca_wf_utils(
                    zcx_ca_error=>create_exception(
                             iv_excp_cls = zcx_ca_wf_utils=>c_zcx_ca_wf_utils
                             ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "assemble_url_fiori_for_wi


  METHOD add_param_to_fiori_url.
    "-----------------------------------------------------------------*
    "   Add further parameter to current URL
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_url   TYPE string,
      lv_value TYPE char50,
      lv_param TYPE string.

    IF iv_curr_url IS NOT INITIAL.
      lv_url = iv_curr_url.
    ELSE.
      lv_url = mv_url.
    ENDIF.

    zcl_ca_conv=>int_2_ext(
                        EXPORTING
                          iv_int_value = iv_value
                        IMPORTING
                          ev_ext_value = lv_value ).

    lv_param = condense( iv_name && `=` && lv_value ) ##no_text.

    IF lv_url CA `?` ##no_text.
      lv_url = lv_url && `&` && lv_param ##no_text.
    ELSE.
      lv_url = lv_url && `?` && lv_param ##no_text.
    ENDIF.

    rv_url = mv_url = lv_url.
  ENDMETHOD.                    "add_url_param

ENDCLASS.
