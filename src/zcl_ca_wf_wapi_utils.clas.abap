"! <p class="shorttext synchronized" lang="en">Common WF object: Wrapped WAPI function modules</p>
CLASS zcl_ca_wf_wapi_utils DEFINITION PUBLIC
                                      CREATE PUBLIC.

  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_ca_c_bool.

*   a l i a s e s
    ALIASES:
*     Boolean flags
      c_false              FOR  zif_ca_c_bool~c_false,
      c_true               FOR  zif_ca_c_bool~c_true.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Determine WAPI error message and raise exception</p>
      "!
      "! @parameter it_msg_stru  | <p class="shorttext synchronized" lang="en">Returned messages of WAPI call</p>
      "! @parameter iv_function  | <p class="shorttext synchronized" lang="en">Name of WAPI FM</p>
      check_sap_wapi_result
        IMPORTING
          it_msg_stru TYPE swr_msgtab
          iv_function TYPE rs38l_fnam,

      "! <p class="shorttext synchronized" lang="en">Wrapped WAPI FM READ_CONTAINER</p>
      "!
      "! @parameter iv_wi_id             | <p class="shorttext synchronized" lang="en">Returned messages of WAPI call</p>
      "! @parameter iv_langu             | <p class="shorttext synchronized" lang="en">Name of WAPI FM</p>
      "! @parameter iv_buff_access       | <p class="shorttext synchronized" lang="en">1 = Get buffered container</p>
      "! @parameter ro_cnt               | <p class="shorttext synchronized" lang="en">Container instance</p>
      read_container
        IMPORTING
          iv_wi_id       TYPE sww_wiid
          iv_langu       TYPE syst_langu DEFAULT sy-langu
          iv_buff_access TYPE dml_boolean DEFAULT c_true
        RETURNING
          VALUE(ro_cnt)  TYPE REF TO if_swf_cnt_container,

      "! <p class="shorttext synchronized" lang="en">Wrapped WAPI FM CREATE_EVENT_EXTENDED (for BOR + classes!)</p>
      "!
      "! @parameter is_lpor              | <p class="shorttext synchronized" lang="en">Object instance</p>
      "! @parameter iv_event             | <p class="shorttext synchronized" lang="en">Event name</p>
      "! @parameter iv_evt_langu         | <p class="shorttext synchronized" lang="en">Event language</p>
      "! @parameter iv_do_commit         | <p class="shorttext synchronized" lang="en">1 = Do commit</p>
      "! @parameter io_evt_cnt           | <p class="shorttext synchronized" lang="en">Instance of WI container</p>
      "! @parameter iv_xml_cnt           | <p class="shorttext synchronized" lang="en">WI container as XML stream (like IFS_XML_CONTAINER)</p>
      create_event_extended
        IMPORTING
          is_lpor      TYPE sibflporb
          iv_event     TYPE sibfevent
          iv_evt_langu TYPE syst_langu DEFAULT sy-langu
          iv_do_commit TYPE dml_boolean DEFAULT c_true
          io_evt_cnt   TYPE REF TO if_swf_cnt_container OPTIONAL
          iv_xml_cnt   TYPE xstring OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Wrapped WAPI FM SAP_WAPI_WORKITEMS_TO_OBJECT</p>
      "!
      "! @parameter is_lpor             | <p class="shorttext synchronized" lang="en">Object instance</p>
      "! @parameter iv_status_var       | <p class="shorttext synchronized" lang="en">Sel. variant for WI status -> see fixed vals of domain</p>
      "! @parameter iv_top_lvl_only     | <p class="shorttext synchronized" lang="en">1 = Select only top level items</p>
      "! @parameter iv_only_top_lvl_out | <p class="shorttext synchronized" lang="en">1 = Return only top level items</p>
      "! @parameter is_time_range       | <p class="shorttext synchronized" lang="en">Selection in this time range</p>
      "! @parameter iv_read_text        | <p class="shorttext synchronized" lang="en">1 = Reading also text elements</p>
      "! @parameter iv_langu            | <p class="shorttext synchronized" lang="en">Language for reading texts</p>
      "! @parameter iv_use_auto_tasks   | <p class="shorttext synchronized" lang="en">1 = Use automatically determined task list</p>
      "! @parameter iv_log_del_tasks    | <p class="shorttext synchronized" lang="en">1 = Consider also (logically) deleted elements</p>
      "! @parameter it_task_filter      | <p class="shorttext synchronized" lang="en">Task filter</p>
      "! @parameter rt_wihdrs           | <p class="shorttext synchronized" lang="en">Result with found workitems</p>
      get_workitems_to_object
        IMPORTING
          is_lpor             TYPE sibflporb
          iv_status_var       TYPE swr_stavar
          iv_top_lvl_only     TYPE dml_boolean DEFAULT c_false
          iv_only_top_lvl_out TYPE dml_boolean DEFAULT c_true
          is_time_range       TYPE swr_timint OPTIONAL
          iv_read_text        TYPE dml_boolean DEFAULT c_false
          iv_langu            TYPE syst_langu DEFAULT sy-langu
          iv_use_auto_tasks   TYPE dml_boolean DEFAULT c_true
          iv_log_del_tasks    TYPE dml_boolean DEFAULT c_false
          it_task_filter      TYPE swrttask OPTIONAL
        RETURNING
          VALUE(rt_wihdrs)    TYPE swrtwihdr,

      "! <p class="shorttext synchronized" lang="en">Wrapped WAPI FM WORKITEM_COMPLETE (saves also container)</p>
      "!
      "! @parameter iv_wi_id             | <p class="shorttext synchronized" lang="en">Returned messages of WAPI call</p>
      "! @parameter iv_act_agent         | <p class="shorttext synchronized" lang="en">Actual agent (user id)</p>
      "! @parameter iv_langu             | <p class="shorttext synchronized" lang="en">Name of WAPI FM</p>
      "! @parameter iv_do_commit         | <p class="shorttext synchronized" lang="en">1 = Do commit</p>
      "! @parameter io_cnt               | <p class="shorttext synchronized" lang="en">Instance of WI container</p>
      "! @parameter iv_xml_cnt           | <p class="shorttext synchronized" lang="en">WI container as XML stream (like IFS_XML_CONTAINER)</p>
      "! @parameter rv_new_status        | <p class="shorttext synchronized" lang="en">Status after completion</p>
      complete_workitem
        IMPORTING
          iv_wi_id             TYPE sww_wiid
          iv_act_agent         TYPE syuname DEFAULT sy-uname
          iv_langu             TYPE sylangu DEFAULT sy-langu
          iv_do_commit         TYPE dml_boolean DEFAULT c_true
          io_cnt               TYPE REF TO if_swf_cnt_container OPTIONAL
          iv_xml_cnt           TYPE xstring OPTIONAL
        RETURNING
          VALUE(rv_new_status) TYPE sww_wistat.
ENDCLASS.



CLASS zcl_ca_wf_wapi_utils IMPLEMENTATION.

  METHOD check_sap_wapi_result.
    "-----------------------------------------------------------------*
    "   Determine WAPI error message and raise exception
    "-----------------------------------------------------------------*
    LOOP AT it_msg_stru ASSIGNING FIELD-SYMBOL(<ls_wf_msg>)
                        WHERE msgty CA zcx_ca_error=>c_msgty_err.
      DATA(lx_error) =
           CAST zcx_ca_workflow(
                  zcx_ca_intern=>create_exception(
                           iv_excp_cls = zcx_ca_workflow=>c_zcx_ca_workflow
                           iv_function = iv_function
                           is_msg      = CORRESPONDING #( <ls_wf_msg> ) ) ).
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "check_sap_wapi_result


  METHOD complete_workitem.
    "-----------------------------------------------------------------*
    "   Complete a workitem and, if necessary, save changed container
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_msg_stru TYPE swr_msgtab,
      lv_xml_cont TYPE xstring.

    IF iv_xml_cnt IS NOT INITIAL.
      lv_xml_cont = iv_xml_cnt.

    ELSEIF io_cnt IS BOUND.
      cl_swf_ifs_conversion_base=>to_ifs_xml(
                                        EXPORTING
                                          source_container = CAST #( io_cnt )
                                        IMPORTING
                                          ifs_xml_stream   = lv_xml_cont
                                          error_handle     = DATA(lx_catched) ).

      IF lx_catched IS BOUND.
        DATA(lx_error) =
             CAST zcx_ca_workflow(
                    zcx_ca_intern=>create_exception(
                             iv_excp_cls = zcx_ca_workflow=>c_zcx_ca_workflow
                             iv_class    = 'CL_SWF_IFS_CONVERSION_BASE'
                             iv_method   = 'TO_IFS_XML'
                             ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'SAP_WAPI_WORKITEM_COMPLETE'
      EXPORTING
        workitem_id       = iv_wi_id
        actual_agent      = iv_act_agent
        language          = iv_langu
        do_commit         = CONV xfeld( xsdbool( iv_do_commit EQ c_true ) )
        ifs_xml_container = lv_xml_cont
      IMPORTING
        new_status        = rv_new_status
      TABLES
        message_struct    = lt_msg_stru.

    "Check result and raise exception
    check_sap_wapi_result( it_msg_stru = lt_msg_stru
                           iv_function = 'SAP_WAPI_WORKITEM_COMPLETE' ) ##no_text.
  ENDMETHOD.                    "complete_workitem


  METHOD get_workitems_to_object.
    "-----------------------------------------------------------------*
    "   Find workitem to given
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_msg_stru          TYPE swr_msgtab.

    CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
      EXPORTING
        object_por               = is_lpor
        top_level_items          = CONV xfeld( xsdbool( iv_top_lvl_only EQ c_true ) )
        selection_status_variant = iv_status_var
        time                     = is_time_range
        text                     = CONV xfeld( xsdbool( iv_read_text EQ c_true ) )
        output_only_top_level    = CONV xfeld( xsdbool( iv_only_top_lvl_out EQ c_true ) )
        language                 = iv_langu
        determine_task_filter    = CONV xfeld( xsdbool( iv_use_auto_tasks EQ c_true ) )
        removed_objects          = CONV xfeld( xsdbool( iv_log_del_tasks EQ c_true ) )
      TABLES
        task_filter              = it_task_filter
        worklist                 = rt_wihdrs
        message_struct           = lt_msg_stru.

    "Check result and raise exception
    check_sap_wapi_result( it_msg_stru = lt_msg_stru
                           iv_function = 'SAP_WAPI_WORKITEMS_TO_OBJECT' ) ##no_text.
  ENDMETHOD.                    "get_workitems_to_object


  METHOD create_event_extended.
    "-----------------------------------------------------------------*
    "   Wrapped WAPI FM CREATE_EVENT_EXTENDED (for BOR + classes!)
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_msg_stru TYPE swr_msgtab,
      lv_xml_cont TYPE xstring.

    IF iv_xml_cnt IS NOT INITIAL.
      lv_xml_cont = iv_xml_cnt.

    ELSEIF io_evt_cnt IS BOUND.
      cl_swf_ifs_conversion_base=>to_ifs_xml(
                                        EXPORTING
                                          source_container = CAST #( io_evt_cnt )
                                        IMPORTING
                                          ifs_xml_stream   = lv_xml_cont
                                          error_handle     = DATA(lx_catched) ).

      IF lx_catched IS BOUND.
        DATA(lx_error) =
             CAST zcx_ca_workflow(
                    zcx_ca_intern=>create_exception(
                             iv_excp_cls = zcx_ca_workflow=>c_zcx_ca_workflow
                             iv_class    = 'CL_SWF_IFS_CONVERSION_BASE'
                             iv_method   = 'TO_IFS_XML'
                             ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'SAP_WAPI_CREATE_EVENT_EXTENDED'
      EXPORTING
        catid             = is_lpor-catid
        typeid            = is_lpor-typeid
        instid            = is_lpor-instid
        event             = CONV string( iv_event )
        commit_work       = CONV xfeld( xsdbool( iv_do_commit EQ c_true ) )
        event_language    = iv_evt_langu
        ifs_xml_container = lv_xml_cont
      TABLES
        message_struct    = lt_msg_stru.

    "Check result and raise exception
    check_sap_wapi_result( it_msg_stru = lt_msg_stru
                           iv_function = 'SAP_WAPI_CREATE_EVENT_EXTENDED' ) ##no_text.
  ENDMETHOD.                    "create_event_extended


  METHOD read_container.
    "-----------------------------------------------------------------*
    "   Determine WAPI error message and raise exception
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_catched  TYPE REF TO cx_swf_ifs_exception,
      lx_error    TYPE REF TO zcx_ca_workflow,
      lt_msg_stru TYPE swr_msgtab,
      lv_xml_cnt  TYPE xstring,
      lv_task_id  TYPE swd_step_t.

    CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
      EXPORTING
        workitem_id       = iv_wi_id
        language          = iv_langu
        buffered_access   = CONV xfeld( xsdbool( iv_buff_access EQ c_true ) )
      IMPORTING
        ifs_xml_container = lv_xml_cnt
      TABLES
        message_struct    = lt_msg_stru.

    "Check result and raise exception
    check_sap_wapi_result( it_msg_stru = lt_msg_stru
                           iv_function = 'SAP_WAPI_READ_CONTAINER' ) ##no_text.

    "Create container instance using the task id
    SELECT SINGLE wi_rh_task INTO lv_task_id
                             FROM  swwwihead
                             WHERE wi_id EQ iv_wi_id.

    "Convert XML container into container instance
    TRY.
        cl_swf_cnt_factory=>create_task_container(
                                            EXPORTING
                                              im_task_id        = lv_task_id
                                            IMPORTING
                                              ex_task_container = ro_cnt ).

      CATCH cx_swf_ifs_exception INTO lx_catched.
        lx_error = CAST zcx_ca_workflow(
                            zcx_ca_intern=>create_exception(
                                           iv_excp_cls = zcx_ca_workflow=>c_zcx_ca_workflow
                                           iv_class    = 'CL_SWF_CNT_FACTORY'
                                           iv_method   = 'CREATE_TASK_CONTAINER'
                                           ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.

    cl_swf_ifs_conversion_base=>import_from_ifs_xml(
                                        EXPORTING
                                          ifs_xml_stream   = lv_xml_cnt
                                          target_container = ro_cnt
                                        IMPORTING
                                          error_handle     = lx_catched ).
    IF lx_catched IS BOUND.
      lx_error = CAST zcx_ca_workflow(
                            zcx_ca_intern=>create_exception(
                                           iv_excp_cls = zcx_ca_workflow=>c_zcx_ca_workflow
                                           iv_class    = 'CL_SWF_IFS_CONVERSION_BASE'
                                           iv_method   = 'IMPORT_FROM_IFS_XML'
                                           ix_error    = lx_catched ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "read_container

ENDCLASS.

