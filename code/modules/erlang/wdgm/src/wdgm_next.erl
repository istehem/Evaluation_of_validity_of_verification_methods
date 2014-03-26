%%% @author  <sebastianwo@MEG-865>
%%% @copyright (C) 2013,
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2013 by  <sebastianwo@MEG-865>

-module(wdgm_next).

-include_lib("eqc/include/eqc.hrl").
-include("wdgm_config.hrl").

-compile(export_all).

%%% -WdgM_Init------------------------------------------------------------------
init_next(S, _Ret, [{_,Is_Null}]) ->
  case not Is_Null of
   true ->
    Rs = wdgm_xml:start(),
    {_, R} = (hd(Rs)), %% why do we get a list of records?
    ModeId = S#state.originalCfg#wdgm.tst_cfg1#tst_cfg1.initial_mode_id,
    NewS =
    #state{initialized   = true,
           currentMode   = ModeId,
           originalCfg   = R,
           expired_supervision_cycles_tol = wdgm_config_params:get_expired_supervision_cycles(ModeId),
           globalstatus  = 'WDGM_GLOBAL_STATUS_OK',
           deadlineTable = wdgm_helper:reset_deadline_table(ModeId, []), %% [WDGM298]
           logicalTable  = wdgm_helper:reset_logical_table(wdgm_config_params:get_internal_graphs(), %% [WDGM296]
                                               true, [])
                        ++ wdgm_helper:reset_logical_table(wdgm_config_params:get_external_graphs(ModeId), %% [WDGM296]
                                               false, []),
           aliveTable    = wdgm_helper:reset_alive_table(ModeId, [])},
      {NewSupervisedEntities, _} = wdgm_helper:reset_supervised_entities(NewS, ModeId),
    NewS#state{supervisedentities = NewSupervisedEntities};
  _ -> S
  end.

%%% -WdgM_GetMode---------------------------------------------------------------
getmode_next(S, _Ret, _Args) ->
  S.

%%% -WdgM_SetMode---------------------------------------------------------------
setmode_next(S, _Ret, [ModeId, Cid]) ->
  case
    (S#state.globalstatus == 'WDGM_GLOBAL_STATUS_OK' orelse
     S#state.globalstatus == 'WDGM_GLOBAL_STATUS_FAILED') andalso
    lists:keyfind(ModeId, 1, wdgm_config_params:get_modes()) /= false andalso
    lists:member(Cid, S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.caller_ids) andalso
    S#state.initialized andalso
    (S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.off_mode_enabled orelse
     not wdgm_config_params:will_disable_watchdog(ModeId))
  of
    true ->
      {NewSupervisedEntities, RetainedSEIds} = wdgm_helper:reset_supervised_entities(S, ModeId),
      S#state{currentMode        = ModeId,
              expired_supervision_cycles_tol = wdgm_config_params:get_expired_supervision_cycles(ModeId),
              supervisedentities = NewSupervisedEntities,
              deadlineTable      =
		lists:keymerge(2,
			       lists:keysort(2, wdgm_helper:reset_deadline_table(ModeId, RetainedSEIds)),
			       lists:keysort(2, S#state.deadlineTable)),
              logicalTable       =
		lists:keymerge(2,
			       lists:keysort(2,
					     wdgm_helper:reset_logical_table(
					       wdgm_config_params:get_internal_graphs(),
					       true, RetainedSEIds)
					     ++ wdgm_helper:reset_logical_table(
						  wdgm_config_params:get_external_graphs(ModeId),
						  false, RetainedSEIds)),
			       lists:keysort(2, S#state.logicalTable)),
	      aliveTable         =
		lists:keymerge(2,
			       lists:keysort(2, wdgm_helper:reset_alive_table(ModeId, RetainedSEIds)),
			       lists:keysort(2, S#state.aliveTable))};
    false -> %% [WDGM316], [WDGM145]
      S %% if WdgIf_SetMode failed set globalstatus='WDGM_GLOBAL_STATUS_STOPPED'? %% [WDGM139]
  end.

%%% -WdgM_DeInit----------------------------------------------------------------
deinit_next(S, _Ret, _Args) ->
    case S#state.globalstatus of
      'WDGM_GLOBAL_STATUS_OK' ->
        S#state{initialized  = false,
                globalstatus = 'WDGM_GLOBAL_STATUS_DEACTIVATED', %% [WDGM286]
                currentMode  = -1};
      _ -> S
    end.

%%% -WdgM_CheckpointReached-----------------------------------------------------
checkpointreached_next(S, _Ret, Args = [SEid, CPid]) ->
  case not wdgm_post:checkpoint_postcondition(S, Args) of
    true ->
      AS = case lists:keyfind(CPid, 2, S#state.aliveTable) of
               false   -> S;
               AliveCP -> AliveTable = lists:keyreplace(CPid, 2,
                                                        S#state.aliveTable,
                                                        AliveCP#alive{alive_counter=AliveCP#alive.alive_counter+1}),
                          S#state{aliveTable=AliveTable}
             end,
      DS = wdgm_checkpointreached:deadlinereached(AS, SEid, CPid),
      wdgm_checkpointreached:logicalreached(DS, SEid, CPid);
    false -> S
  end.

%%% -WdgM_GetLocalStatus--------------------------------------------------------
getlocalstatus_next(S, _Ret, _Args) ->
  S.


%%% -WdgM_GetGlobalStatus-------------------------------------------------------
getglobalstatus_next(S, _Ret, _Args) ->
  S.

%%% -WdgM_PerformReset----------------------------------------------------------
performreset_next(S, _Ret, _Args) ->
  S.

%%% -WdgM_GetFirstExpiredSEID---------------------------------------------------
getfirstexpiredseid_next(S, _Ret, _Args) ->
  S.

%%% -WdgM_MainFunction----------------------------------------------------------
mainfunction_next(S, _Ret, _Args) ->
  case
    S#state.initialized
  of
    true  -> wdgm_main:global_status(S);
    false -> S
  end.

%%% -WdgM_GetVersionInfo--------------------------------------------------------
getversioninfo_next(S, _Ret, _Args) ->
  S.
