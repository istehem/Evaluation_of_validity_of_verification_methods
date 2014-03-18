-define(CONFIG_FILE,
        list_to_atom(
          car_xml:name_of(
            hd(car_xml:get_containers_by_def(
                 "WdgMConfigSet",
                 car_xml:file(wdgm_xml:config_file())))))).

-define(CFG, car_xml:file(wdgm_xml:config_file())).
-define(WDGMSTATEM, wdgm_statem_eqc).
-define(C_CODE, wdgm_wrapper).

%% Which compiler to use
-ifdef(bullseye).
-define(COMPILER,"C:/Program\ Files\ \(x86\)/BullseyeCoverage/bin/gcc.exe").
-else.
-define(COMPILER,"gcc").
-endif.

%% Bullseye C coverage
-ifdef(bullseye).
-define(COPY_FILE,copy_bullseye_cov_file()).
-define(COVER_OPTS,[{d,bullseye}]).
-else.
-define(COVER_OPTS,[]).
-define(COPY_FILE,ok).
-endif.

%% Cover Erlang coverage
-ifdef(linecover).
-define(SETUP_COVERAGE,setup_coverage()).
-define(RUN_COVERAGE,run_coverage()).
-else.
-define(SETUP_COVERAGE,ok).
-define(RUN_COVERAGE,ok).
-endif.

%% Write history to file
-ifdef(history).
-define(WRITE_HISTORY(T,H),write_history_to_file(T,H)).
-else.
-define(WRITE_HISTORY(T,H),ok).
-endif.

%% Prioritize logical supervision?
-ifdef(NOLSPRIO).
-define(LSPRIO, dont_prioritize_ls).
-else.
-define(LSPRIO, prioritize_ls).
-endif.

%% CONFIG DEFINES
-define(EXAMPLE_CONFIG, "examples/WdgM_VID41_ExampleConfiguration_001_cfg1.arxml").
-define(FREESCALE_CONFIG, "freescale/wdgm_freescale_pip.arxml").
-define(BSI_CONFIG, "bsi/wdgm_bsi.arxml").
-define(EXAMPLE_ONLY_AS_CONFIG, "examples/WdgM_VID41_ExampleConfiguration_ONLY_AS.arxml").

-ifdef(EXAMPLE).
-ifndef(CONFIG).
-define(CONFIG, ?EXAMPLE_CONFIG).
-endif.
-endif.

-ifdef(FREESCALE).
-ifndef(CONFIG).
-define(CONFIG, ?FREESCALE_CONFIG).
-endif.
-endif.

-ifdef(BSI).
-ifndef(CONFIG).
-define(CONFIG, ?BSI_CONFIG).
-endif.
-endif.

-ifdef(EXAMPLE_ONLY_AS).
-ifndef(CONFIG).
-define(CONFIG, ?EXAMPLE_ONLY_AS_CONFIG).
-endif.
-endif.

-ifndef(CONFIG).
-define(CONFIG, ?EXAMPLE_CONFIG).
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
