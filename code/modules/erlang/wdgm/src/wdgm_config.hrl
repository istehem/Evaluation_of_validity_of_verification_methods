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
	  initial_mode_id}).

-record(wdgm, {wdgmgeneral, tst_cfg1}).
