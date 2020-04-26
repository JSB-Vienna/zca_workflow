"! <p class="shorttext synchronized" lang="en">Common WF object: Execution of workflow/BO macros</p>
CLASS zcl_ca_wf_exec_macros DEFINITION PUBLIC
                                       FINAL
                                       CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get BO key unstructured</p>
      "!
      "! @parameter ibo_inst              | <p class="shorttext synchronized" lang="en">Business object instance</p>
      "! @parameter rv_bo_key             | <p class="shorttext synchronized" lang="en">Business object key</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">Common WF exception: Macro execution errors</p>
      get_bo_key
        IMPORTING
          ibo_inst         TYPE obj_record
        RETURNING
          VALUE(rv_bo_key) TYPE sibfboriid
        RAISING
          zcx_ca_wf_exec_macros.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Create BO object reference</p>
      "!
      "! @parameter iv_bo_name            | <p class="shorttext synchronized" lang="en">Business object name</p>
      "! @parameter iv_bo_key             | <p class="shorttext synchronized" lang="en">Business object key</p>
      "! @parameter rbo_inst              | <p class="shorttext synchronized" lang="en">Business object instance</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">Common WF exception: Macro execution errors</p>
      create_bo
        IMPORTING
          iv_bo_name      TYPE sibftypeid
          iv_bo_key       TYPE sibfboriid
        RETURNING
          VALUE(rbo_inst) TYPE obj_record
        RAISING
          zcx_ca_wf_exec_macros.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
**   s t a t i c   a t t r i b u t e s
*    CLASS-DATA:
**     o b j e c t   r e f e r e n c e s
**     description
*      mo_...               TYPE REF TO x..
*
**     t a b l e s
**     description
*      mt_...               TYPE x..
*
**     s t r u c t u r e s
**     description
*      ms_...               TYPE x..
*
**     s i n g l e   v a l u e s
**     description
*      mv_...               TYPE x..
ENDCLASS.                     "zcl_ca_wf_exec_macros  DEFINITION


CLASS zcl_ca_wf_exec_macros IMPLEMENTATION.

  METHOD create_bo.
    "-----------------------------------------------------------------*
    "   Create BO object reference
    "-----------------------------------------------------------------*
    swc_create_object  rbo_inst  iv_bo_name  iv_bo_key.
    IF sy-subrc NE 0.
      DATA(lx_error) =
            CAST zcx_ca_wf_exec_macros(
                     zcx_ca_error=>create_exception(
                                iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                iv_function = |SWC_CREATE_OBJECT RBO_{ iv_bo_name }|
                                is_msg      = CORRESPONDING #( syst )
                                iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "create_bo


  METHOD get_bo_key.
    "-----------------------------------------------------------------*
    "   Get BO key unstructured
    "-----------------------------------------------------------------*
    swc_get_object_key  ibo_inst  rv_bo_key.
    IF sy-subrc NE 0.
      DATA(lx_error) =
            CAST zcx_ca_wf_exec_macros(
                     zcx_ca_error=>create_exception(
                                iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                iv_function = 'SWC_GET_OBJECT_KEY'
                                is_msg      = CORRESPONDING #( syst )
                                iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "get_bo_key

ENDCLASS.                     "zcl_ca_wf_exec_macros  IMPLEMENTATION


