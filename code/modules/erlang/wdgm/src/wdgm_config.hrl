-record(wdgmgeneral, {
	  defensive_behavior,
	  dem_stopped_supervision_report,
	  dev_error_detect,
	  immediate_reset,
	  off_mode_enabled,
	  supervision_cycle,
	  version_check_foreign_module,
	  version_info_api,
	  caller_ids}).

-record(tst_cfg1, {
	  initial_mode_id,
          expired_supervision_cycles_tol}).

-record(wdgm, {wdgmgeneral, tst_cfg1}).

-record(state, {initialized=false,
                currentMode=-1,
                globalstatus,
                originalCfg=#wdgm{},
                expiredSEid,
                expiredsupervisioncycles=0,
                aliveTable,
                deadlineTable,
                logicalTable,
                supervisedentities,
                errormsg}).
-record(alive, {cpid,
                alive_counter=0}).
-record(deadline, {startCP,
                   stopCP,
                   timer_status,
                   timestamp=0}).
-record(logical, {initCP,
                  stopCP,
                  activity}).
-record(supervisedentity, {seid,
                           localstatus='WDGM_LOCAL_STATUS_OK',
                           localalivestatus='WDGM_CORRECT',
                           localdeadlinestatus='WDGM_CORRECT',
                           locallogicalstatus='WDGM_CORRECT',
                           failed_alive_supervision_cycle_tol,
                           failed_reference_supervision_cycles=0,
                           supervision_cycles=0}).
