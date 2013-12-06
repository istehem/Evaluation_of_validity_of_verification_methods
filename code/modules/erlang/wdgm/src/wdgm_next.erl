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
init_next(S, _Ret, _Args) ->
  Rs = wdgm_xml:start(),
  {_, R} = (hd(Rs)), %% why do we get a list of records?
  ModeId = S#state.originalCfg#wdgm.tst_cfg1#tst_cfg1.initial_mode_id,
  NewS =
    #state{initialized   = true,
           currentMode   = ModeId,
           originalCfg   = R,
           expired_supervision_cycles_tol = wdgm_config_params:get_expired_supervision_cycles(ModeId),
           globalstatus  = 'WDGM_GLOBAL_STATUS_OK',
           deadlineTable = wdgm_helper:reset_deadline_table(ModeId), %% [WDGM298]
           logicalTable  = wdgm_helper:reset_logical_table(wdgm_config_params:get_internal_graphs(),
                                               true)
                        ++ wdgm_helper:reset_logical_table(wdgm_config_params:get_external_graphs(ModeId),
                                               false),
           aliveTable    = wdgm_helper:reset_alive_table(ModeId)},
  NewS#state{supervisedentities = wdgm_helper:reset_supervised_entities(NewS, ModeId)}.

%%% -WdgM_GetMode---------------------------------------------------------------
getmode_next(S, _Ret, _Args) ->
  S.

%%% -WdgM_SetMode---------------------------------------------------------------
setmode_next(S, Ret, [ModeId, _Cid]) ->
  case Ret of
    0 -> case
           (S#state.globalstatus == 'WDGM_GLOBAL_STATUS_OK' orelse
            S#state.globalstatus == 'WDGM_GLOBAL_STATUS_FAILED')
         of
           true ->
             S#state{currentMode = ModeId,
                     expired_supervision_cycles_tol = wdgm_config_params:get_expired_supervision_cycles(ModeId),
                     supervisedentities = wdgm_helper:reset_supervised_entities(S, ModeId),
                     deadlineTable      = wdgm_helper:reset_deadline_table(ModeId),
                     logicalTable       = lists:filter(fun (Logical) ->
                                                           Logical#logical.is_internal
                                                       end,
                                                       S#state.logicalTable) %% dont reset SE internal graphs
                                       ++ wdgm_helper:reset_logical_table(wdgm_config_params:get_external_graphs(ModeId),
                                                              false),
                     aliveTable         = wdgm_helper:reset_alive_table(ModeId)};
           false -> %% [WDGM316], [WDGM145]
             S
         end;
    _ -> S %% if WdgIf_SetMode failed set globalstatus='WDGM_GLOBAL_STATUS_STOPPED'? %% [WDGM139]
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
