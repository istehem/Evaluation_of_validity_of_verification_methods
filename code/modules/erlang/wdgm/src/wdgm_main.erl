-module(wdgm_main).

-compile(export_all).
-include("wdgm_config.hrl").

%%-GLOBAL STATUS----------------------------------------------------------------

global_status(S) ->
  NewSEs = [local_SE_status(S, SE) || SE <- S#state.supervisedentities],
  {Failed, Expired} = lists:foldl(fun (SE, {Failed, Expired}) ->
                                      {(Failed  orelse 'WDGM_LOCAL_STATUS_FAILED'  == SE#supervisedentity.localstatus),
                                       (Expired orelse 'WDGM_LOCAL_STATUS_EXPIRED' == SE#supervisedentity.localstatus)}
                                  end, {false, false}, NewSEs),
  EXPIRED_TOL = S#state.originalCfg#wdgm.tst_cfg1#tst_cfg1.expired_supervision_cycles_tol,
  EXPIRED_CYCLES = S#state.expiredsupervisioncycles,
  {Status, NewExpiredCycles} = check_global_status({S#state.globalstatus, Failed, Expired}, EXPIRED_TOL, EXPIRED_CYCLES),
  EXPIRED_SE = (lists:keyfind('WDGM_LOCAL_STATUS_EXPIRED', 3, NewSEs)),
  S#state{globalstatus=Status,
          expiredSEid=case EXPIRED_SE of
                        false -> undefined;
                        SE -> SE#supervisedentity.seid
                      end,
          expiredsupervisioncycles=NewExpiredCycles,
          supervisedentities=NewSEs}.

check_global_status({'WDGM_GLOBAL_STATUS_OK', false, false}, _, EXPIRED_CYCLES) ->
  {'WDGM_GLOBAL_STATUS_OK', EXPIRED_CYCLES}; %% [WDGM078]
check_global_status({'WDGM_GLOBAL_STATUS_OK', true, false}, _, EXPIRED_CYCLES) ->
  {'WDGM_GLOBAL_STATUS_FAILED', EXPIRED_CYCLES}; %% [WDGM076]
check_global_status({'WDGM_GLOBAL_STATUS_OK', _, true}, EXPIRED_TOL, EXPIRED_CYCLES) when EXPIRED_TOL > 0 ->
  {'WDGM_GLOBAL_STATUS_EXPIRED', EXPIRED_CYCLES}; %% [WDGM215]
check_global_status({'WDGM_GLOBAL_STATUS_OK', _, true}, EXPIRED_TOL, EXPIRED_CYCLES) when EXPIRED_TOL == 0 ->
  {'WDGM_GLOBAL_STATUS_STOPPED', EXPIRED_CYCLES}; %% [WDGM216]
check_global_status({'WDGM_GLOBAL_STATUS_FAILED', true, false}, _, EXPIRED_CYCLES) ->
  {'WDGM_GLOBAL_STATUS_FAILED', EXPIRED_CYCLES}; %% [WDGM217]
check_global_status({'WDGM_GLOBAL_STATUS_FAILED', false, false}, _, EXPIRED_CYCLES) ->
  {'WDGM_GLOBAL_STATUS_OK', EXPIRED_CYCLES}; %% [WDGM218]
check_global_status({'WDGM_GLOBAL_STATUS_FAILED', _, true}, EXPIRED_TOL, EXPIRED_CYCLES) when EXPIRED_TOL > 0 ->
  {'WDGM_GLOBAL_STATUS_EXPIRED', EXPIRED_CYCLES}; %% [WDGM077]
check_global_status({'WDGM_GLOBAL_STATUS_FAILED', _, true}, EXPIRED_TOL, EXPIRED_CYCLES) when EXPIRED_TOL == 0 ->
  {'WDGM_GLOBAL_STATUS_STOPPED', EXPIRED_CYCLES}; %% [WDGM117]
check_global_status({'WDGM_GLOBAL_STATUS_EXPIRED', _, true}, EXPIRED_TOL, EXPIRED_CYCLES) when EXPIRED_CYCLES =< EXPIRED_TOL ->
  {'WDGM_GLOBAL_STATUS_EXPIRED', EXPIRED_CYCLES+1}; %% [WDGM219]
check_global_status({'WDGM_GLOBAL_STATUS_EXPIRED', _, true}, EXPIRED_TOL, EXPIRED_CYCLES) when EXPIRED_CYCLES > EXPIRED_TOL ->
  {'WDGM_GLOBAL_STATUS_STOPPED', EXPIRED_CYCLES}; %% [WDGM220]
check_global_status({'WDGM_GLOBAL_STATUS_STOPPED', _, _}, _, EXPIRED_CYCLES) ->
  {'WDGM_GLOBAL_STATUS_STOPPED', EXPIRED_CYCLES}; %% [WDGM221]
check_global_status({_, _, _}, _, EXPIRED_CYCLES) ->
  {'WDGM_GLOBAL_STATUS_STOPPED', EXPIRED_CYCLES}. %% should never get here

%%-LOCAL SE STATUS--------------------------------------------------------------

local_SE_status(S, SE) ->
  AliveRes = alive_supervision(S, SE),
  DeadlineRes = deadline_supervision(SE),
  LogicalRes = logical_supervision(SE),
  FAIL_TOL = SE#supervisedentity.failed_alive_supervision_cycle_tol,
  FAIL_CYCLES = SE#supervisedentity.failed_reference_supervision_cycles,
  {LocalStatus, FailCycles} = check_local_status({SE#supervisedentity.localstatus, AliveRes, DeadlineRes, LogicalRes}, FAIL_TOL, FAIL_CYCLES),
  SE#supervisedentity{localstatus=LocalStatus,
                      localalivestatus=AliveRes,
                      supervision_cycles=SE#supervisedentity.supervision_cycles+1,
                      failed_reference_supervision_cycles=FailCycles}.

check_local_status({'WDGM_LOCAL_STATUS_OK',
                    'WDGM_CORRECT',
                    'WDGM_CORRECT',
                    'WDGM_CORRECT'},
                   _, FAIL_CYCLES) ->
  {'WDGM_LOCAL_STATUS_OK', FAIL_CYCLES}; %% [WDGM201]
check_local_status({'WDGM_LOCAL_STATUS_OK',
                    'WDGM_INCORRECT',
                    _,
                    _},
                   FAIL_TOL, FAIL_CYCLES)
  when FAIL_TOL == 0 ->
  {'WDGM_LOCAL_STATUS_EXPIRED', FAIL_CYCLES}; %% [WDGM202]
check_local_status({'WDGM_LOCAL_STATUS_OK',
                    _,
                    'WDGM_INCORRECT',
                    _},
                   _, FAIL_CYCLES) ->
  {'WDGM_LOCAL_STATUS_EXPIRED', FAIL_CYCLES}; %% [WDGM202]
check_local_status({'WDGM_LOCAL_STATUS_OK',
                    _,
                    _,
                    'WDGM_INCORRECT'},
                   _, FAIL_CYCLES) ->
  {'WDGM_LOCAL_STATUS_EXPIRED', FAIL_CYCLES}; %% [WDGM202]
check_local_status({'WDGM_LOCAL_STATUS_OK',
                    'WDGM_INCORRECT',
                    'WDGM_CORRECT',
                    'WDGM_CORRECT'},
                   FAIL_TOL, FAIL_CYCLES)
  when FAIL_TOL > 0 ->
  {'WDGM_LOCAL_STATUS_FAILED', FAIL_CYCLES+1}; %% [WDGM203]
check_local_status({'WDGM_LOCAL_STATUS_FAILED',
                    'WDGM_INCORRECT',
                    'WDGM_CORRECT',
                    'WDGM_CORRECT'},
                   FAIL_TOL, FAIL_CYCLES)
  when FAIL_CYCLES =< FAIL_TOL ->
  {'WDGM_LOCAL_STATUS_FAILED', FAIL_CYCLES+1}; %% [WDGM204]
check_local_status({'WDGM_LOCAL_STATUS_FAILED',
                    'WDGM_CORRECT',
                    'WDGM_CORRECT',
                    'WDGM_CORRECT'},
                   _, FAIL_CYCLES)
  when FAIL_CYCLES > 1 ->
  {'WDGM_LOCAL_STATUS_FAILED', FAIL_CYCLES-1}; %% [WDGM300]
check_local_status({'WDGM_LOCAL_STATUS_FAILED',
                    'WDGM_CORRECT',
                    'WDGM_CORRECT',
                    'WDGM_CORRECT'},
                   _, FAIL_CYCLES)
  when FAIL_CYCLES == 1 ->
  {'WDGM_LOCAL_STATUS_OK', FAIL_CYCLES-1}; %% [WDGM205]
check_local_status({'WDGM_LOCAL_STATUS_FAILED',
                    'WDGM_INCORRECT',
                    _,
                    _},
                   FAIL_TOL, FAIL_CYCLES)
  when FAIL_CYCLES > FAIL_TOL ->
  {'WDGM_LOCAL_STATUS_EXPIRED', FAIL_CYCLES}; %% [WDGM206]
check_local_status({'WDGM_LOCAL_STATUS_FAILED',
                    _,
                    'WDGM_INCORRECT',
                    _},
                   _, FAIL_CYCLES) ->
  {'WDGM_LOCAL_STATUS_EXPIRED', FAIL_CYCLES}; %% [WDGM206]
check_local_status({'WDGM_LOCAL_STATUS_FAILED',
                    _,
                    _,
                    'WDGM_INCORRECT'},
                   _, FAIL_CYCLES) ->
  {'WDGM_LOCAL_STATUS_EXPIRED', FAIL_CYCLES}; %% [WDGM206]
check_local_status({'WDGM_LOCAL_STATUS_DEACTIVATED',
                    _, _, _},
                   _, FAIL_CYCLES) ->
  {'WDGM_LOCAL_STATUS_DEACTIVATED', FAIL_CYCLES}; %% [WDGM208]
check_local_status({'WDGM_LOCAL_STATUS_EXPIRED',
                    _, _, _},
                   _, FAIL_CYCLES) ->
  {'WDGM_LOCAL_STATUS_EXPIRED', FAIL_CYCLES}; %% [WDGM]
check_local_status({_, _, _, _},
                   _, FAIL_CYCLES) ->
  {'WDGM_LOCAL_STATUS_EXPIRED', FAIL_CYCLES}. %% should never get here

%%-ALIVE SUPERVISION------------------------------------------------------------
%% @doc should not do anything with the state, just need it for the checking.
%% checks all CPs have the correct behaviour of a SE.
alive_supervision(S, SE) ->
  CPs_for_SE = wdgm_config_params:get_CPs_of_SE(SE#supervisedentity.seid),
  Correct = lists:foldl(fun (X, Correct) -> (Correct andalso (X == 'WDGM_CORRECT' orelse
                                                              X == dontcheckme)) end,
                        true,
                        [check_CP_within_SE(S, CPref)
                         || CPref <- wdgm_config_params:get_checkpoints_for_mode(S#state.currentMode, 'AS'),
                            lists:member(wdgm_config_params:get_checkpoint_id(CPref), CPs_for_SE)]),
  case Correct of
    true ->
      'WDGM_CORRECT';
    false ->
      'WDGM_INCORRECT'
  end.

%% @doc should not do anything with the state, just need it for the checking.
%% checks if a CP have the correct behaviour.
check_CP_within_SE(S, CPref) ->
  CPid = wdgm_config_params:get_checkpoint_id(CPref),
  CPstate = lists:keyfind(CPid, 2, S#state.aliveTable),
  SEid = wdgm_config_params:get_SE_id(CPref),
  SE = lists:keyfind(SEid, 2, S#state.supervisedentities),
  {SRC, EAI, MinMargin, MaxMargin} = hd(wdgm_config_params:get_AS_for_CP(S#state.currentMode, CPid)),
  SC = SE#supervisedentity.supervision_cycles+1, %% because dont update in when SC==0
  check_aliveness(CPstate#alive.alive_counter, SC, SRC, EAI, MinMargin, MaxMargin).

check_aliveness(_, 0, _, _, _, _) ->
  'WDGM_CORRECT';
check_aliveness(AC, SC, SRC, EAI, MinMargin, MaxMargin) ->
  case SC rem SRC == 0 of
    true ->
      I = algorithm_for_alive_supervision(AC, EAI),
      case
        I =< MaxMargin andalso
        I >= -MinMargin
      of
        true -> 'WDGM_CORRECT';
        _ -> 'WDGM_INCORRECT'
      end;
    false -> check_aliveness(AC, SC-1, SRC, EAI, MinMargin, MaxMargin)
  end.

algorithm_for_alive_supervision(AliveCounter, EAI) ->
  AliveCounter-EAI.
%%  AliveCounter-SupervisionCycles+(SRC-EAI). %% [WDGM074] Stämmer detta?


%%-DEADLINE SUPERVISION---------------------------------------------------------

deadline_supervision(SE) ->
  SE#supervisedentity.localdeadlinestatus.

%%-LOGICAL SUPERVISION----------------------------------------------------------

logical_supervision(SE) ->
  SE#supervisedentity.locallogicalstatus.
