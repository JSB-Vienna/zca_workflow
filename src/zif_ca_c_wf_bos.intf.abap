"! <p class="shorttext synchronized" lang="en">Common WF objects: Constants with predefined BO types</p>
INTERFACE zif_ca_c_wf_bos PUBLIC.
* c o n s t a n t s
  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">BO type IMAGE - ArchiveLink Document</p>
    BEGIN OF cbo_image,
      instid TYPE sibfboriid VALUE space,
      typeid TYPE sibftypeid VALUE 'IMAGE' ##no_text,
      catid  TYPE sibfcatid  VALUE swfco_objtype_bor,
    END   OF cbo_image,

    "! <p class="shorttext synchronized" lang="en">BO type USER - SAP user master data / methods</p>
    BEGIN OF cbo_user,
      instid TYPE sibfboriid VALUE space,
      typeid TYPE sibftypeid VALUE 'USER' ##no_text,
      catid  TYPE sibfcatid  VALUE swfco_objtype_bor,
    END   OF cbo_user,

    "! <p class="shorttext synchronized" lang="en">BO type USR01 - SAP user - address data</p>
    BEGIN OF cbo_usr01,
      instid TYPE sibfboriid VALUE space,
      typeid TYPE sibftypeid VALUE 'USR01' ##no_text,
      catid  TYPE sibfcatid  VALUE swfco_objtype_bor,
    END   OF cbo_usr01,

    "! <p class="shorttext synchronized" lang="en">BO type WFNOTE - Workflow note</p>
    BEGIN OF cbo_wfnote,
      instid TYPE sibfboriid VALUE space,
      typeid TYPE sibftypeid VALUE 'WFNOTE' ##no_text,
      catid  TYPE sibfcatid  VALUE swfco_objtype_bor,
    END   OF cbo_wfnote.

ENDINTERFACE.
