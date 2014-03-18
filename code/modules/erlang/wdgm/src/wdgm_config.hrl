-define(CONFIG_FILE,
        list_to_atom(
          car_xml:name_of(
            hd(car_xml:get_containers_by_def(
                 "WdgMConfigSet",
                 car_xml:file(wdgm_xml:config_file())))))).

-define(CFG, car_xml:file(wdgm_xml:config_file())).

-ifdef(NOLSPRIO).
-define(LSPRIO, dont_prioritize_ls).
-else.
-define(LSPRIO, prioritize_ls).
-endif.

-record(wdgmgeneral,
        {defensive_behavior,
         dem_stopped_supervision_report,
         dev_error_detect,
         immediate_reset,
         off_mode_enabled,
         supervision_cycle,
         version_check_foreign_module,
         version_info_api,
         caller_ids}).

-record(tst_cfg1,
        {initial_mode_id}).

-record(wdgm,
        {wdgmgeneral, tst_cfg1}).

-record(state,
        {initialized=false,
         currentMode=-1,
         globalstatus = 'WDGM_GLOBAL_STATUS_DEACTIVATED',
         originalCfg=#wdgm{},
         expiredSEid,
         expiredsupervisioncycles=0,
         expired_supervision_cycles_tol=0,
         aliveTable,
         deadlineTable,
         logicalTable,
         supervisedentities}).
-record(alive,
        {cpid,
         supervision_reference_cycles,
         expected_alive_indications,
         minmargin,
         maxmargin,
         alive_counter=0,
         status='WDGM_CORRECT'}).
-record(deadline,
        {startCP,
         stopCP,
         minmargin,
         maxmargin,
         timestamp=0,
         timer=0}).
-record(logical,
        {initCP,
         finalCPs,
         cps_in_graph,
         graph,
         activity,
         storedCP,
         is_internal}).
-record(supervisedentity,
        {seid,
         localstatus          ,
         localalivestatus     ,
         localdeadlinestatus  ,
         locallogicalstatus   ,
         failed_alive_supervision_cycle_tol,
         failed_reference_supervision_cycles = 0,
         supervision_cycles   = 0}).
