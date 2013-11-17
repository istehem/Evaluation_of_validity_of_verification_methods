-module(wdgm_main_eqc).

-compile(export_all).
-include("wdgm_config.hrl").

init() ->
    %% set local supervision status to OK || DEACTIVATED
    ok.

local_SE_status(S, SE) ->
    AliveRes = alive_supervision(S, SE),
    DeadlineRes = deadline_supervision(SE),
    LogicalRes = logical_supervision(SE),
    FAIL_TOL = SE#supervisedentity.failed_alive_supervision_cycle_tol,
    FAIL_CYCLES = SE#supervisedentity.failed_reference_supervision_cycles,
    check_local_status({SE#supervisedentity.localstatus, AliveRes, DeadlineRes, LogicalRes}, FAIL_TOL, FAIL_CYCLES).

check_local_status({'WDGM_LOCAL_STATUS_OK', 'WDGM_CORRECT', 'WDGM_CORRECT', 'WDGM_CORRECT'}, _, _) ->
    {'WDGM_LOCAL_STATUS_OK', 0}; %% [WDGM201]
check_local_status({'WDGM_LOCAL_STATUS_OK', 'WDGM_INCORRECT', 'WDGM_CORRECT', 'WDGM_CORRECT'}, FAIL_TOL, _)
  when FAIL_TOL == 0 ->
    {'WDGM_LOCAL_STATUS_EXPIRED', 0}; %% [WDGM202]
check_local_status({'WDGM_LOCAL_STATUS_OK', 'WDGM_CORRECT', 'WDGM_INCORRECT', 'WDGM_CORRECT'}, _, _) ->
    {'WDGM_LOCAL_STATUS_EXPIRED', 0}; %% [WDGM202]
check_local_status({'WDGM_LOCAL_STATUS_OK', 'WDGM_CORRECT', 'WDGM_CORRECT', 'WDGM_INCORRECT'}, _, _) ->
    {'WDGM_LOCAL_STATUS_EXPIRED', 0}; %% [WDGM202]
check_local_status({'WDGM_LOCAL_STATUS_OK', 'WDGM_INCORRECT', 'WDGM_CORRECT', 'WDGM_CORRECT'}, FAIL_TOL, FAIL_CYCLES)
  when FAIL_TOL > 0 ->
    {'WDGM_LOCAL_STATUS_FAILED', FAIL_CYCLES+1}; %% [WDGM203]
check_local_status({'WDGM_LOCAL_STATUS_FAILED', 'WDGM_INCORRECT', 'WDGM_CORRECT', 'WDGM_CORRECT'}, FAIL_TOL, FAIL_CYCLES)
  when FAIL_CYCLES =< FAIL_TOL ->
    {'WDGM_LOCAL_STATUS_FAILED', FAIL_CYCLES+1}; %% [WDGM204]
check_local_status({'WDGM_LOCAL_STATUS_FAILED', 'WDGM_CORRECT', 'WDGM_CORRECT', 'WDGM_CORRECT'}, _, FAIL_CYCLES)
  when FAIL_CYCLES > 1 ->
    {'WDGM_LOCAL_STATUS_FAILED', FAIL_CYCLES-1}; %% [WDGM300]
check_local_status({'WDGM_LOCAL_STATUS_FAILED', 'WDGM_CORRECT', 'WDGM_CORRECT', 'WDGM_CORRECT'}, _, FAIL_CYCLES)
  when FAIL_CYCLES == 1 ->
    {'WDGM_LOCAL_STATUS_OK', FAIL_CYCLES-1}; %% [WDGM205]
check_local_status({'WDGM_LOCAL_STATUS_FAILED', 'WDGM_INCORRECT', 'WDGM_CORRECT', 'WDGM_CORRECT'}, FAIL_TOL, FAIL_CYCLES)
  when FAIL_CYCLES > FAIL_TOL ->
    {'WDGM_LOCAL_STATUS_EXPIRED', 0}; %% [WDGM206]
check_local_status({'WDGM_LOCAL_STATUS_FAILED', 'WDGM_CORRECT', 'WDGM_INCORRECT', 'WDGM_CORRECT'}, _, _) ->
    {'WDGM_LOCAL_STATUS_EXPIRED', 0}; %% [WDGM206]
check_local_status({'WDGM_LOCAL_STATUS_FAILED', 'WDGM_CORRECT', 'WDGM_CORRECT', 'WDGM_INCORRECT'}, _, _) ->
    {'WDGM_LOCAL_STATUS_EXPIRED', 0}; %% [WDGM206]
check_local_status({_, _, _, _}, _, _) -> {'WDGM_LOCAL_STATUS_EXPIRED', 0}. %% should never get here

global_status(S) ->
    NewSEs = [local_SE_status(S, SE) || SE <- S#state.supervisedentities],
    {Failed, Expired} = lists:foldl(fun (SE, {Failed, Expired}) ->
                                            {(Failed  orelse 'WDGM_LOCAL_STATUS_FAILED'  == SE#supervisedentity.localstatus),
                                             (Expired orelse 'WDGM_LOCAL_STATUS_EXPIRED' == SE#supervisedentity.localstatus)}
                                    end, {false, false}, NewSEs),
    EXPIRED_TOL = S#state.originalCfg#tst_cfg1.expired_supervision_cycles_tol,
    EXPIRED_CYCLES = S#state.expiredsupervisioncycles,
    {Status, NewExpiredCycles} = check_global_status({S#state.globalstatus, Failed, Expired}, EXPIRED_TOL, EXPIRED_CYCLES),
    S#state{globalstatus=Status, expiredsupervisioncycles=NewExpiredCycles, supervisedentities=NewSEs}.

check_global_status({'WDGM_GLOBAL_STATUS_OK', false, false}, _, _) ->
    {'WDGM_GLOBAL_STATUS_OK', 0}; %% [WDGM078]
check_global_status({'WDGM_GLOBAL_STATUS_OK', true, false}, _, _) ->
    {'WDGM_GLOBAL_STATUS_FAILED', 0}; %% [WDGM076]
check_global_status({'WDGM_GLOBAL_STATUS_OK', _, true}, EXPIRED_TOL, _) when EXPIRED_TOL > 0 ->
    {'WDGM_GLOBAL_STATUS_EXPIRED', 0}; %% [WDGM215]
check_global_status({'WDGM_GLOBAL_STATUS_OK', _, true}, EXPIRED_TOL, _) when EXPIRED_TOL == 0 ->
    {'WDGM_GLOBAL_STATUS_STOPPED', 0}; %% [WDGM216]
check_global_status({'WDGM_GLOBAL_STATUS_FAILED', true, false}, _, _) ->
    {'WDGM_GLOBAL_STATUS_FAILED', 0}; %% [WDGM217]
check_global_status({'WDGM_GLOBAL_STATUS_FAILED', false, false}, _, _) ->
    {'WDGM_GLOBAL_STATUS_OK', 0}; %% [WDGM218]
check_global_status({'WDGM_GLOBAL_STATUS_FAILED', _, true}, EXPIRED_TOL, _) when EXPIRED_TOL > 0 ->
    {'WDGM_GLOBAL_STATUS_EXPIRED', 0}; %% [WDGM077]
check_global_status({'WDGM_GLOBAL_STATUS_FAILED', _, true}, EXPIRED_TOL, _) when EXPIRED_TOL == 0 ->
    {'WDGM_GLOBAL_STATUS_STOPPED', 0}; %% [WDGM117]
check_global_status({'WDGM_GLOBAL_STATUS_EXPIRED', _, true}, EXPIRED_TOL, EXPIRED_CYCLES) when EXPIRED_CYCLES =< EXPIRED_TOL ->
    {'WDGM_GLOBAL_STATUS_EXPIRED', EXPIRED_CYCLES+1}; %% [WDGM219]
check_global_status({'WDGM_GLOBAL_STATUS_EXPIRED', _, true}, EXPIRED_TOL, EXPIRED_CYCLES) when EXPIRED_CYCLES > EXPIRED_TOL ->
    {'WDGM_GLOBAL_STATUS_STOPPED', 0}; %% [WDGM220]
check_global_status({'WDGM_GLOBAL_STATUS_STOPPED', _, _}, _, _) ->
    {'WDGM_GLOBAL_STATUS_STOPPED', 0}; %% [WDGM221]
check_global_status({_, _, _}, _, _) ->
    {'WDGM_GLOBAL_STATUS_STOPPED', 0}. %% should never get here

%%-ALIVE SUPERVISION------------------------------------------------------------
%% @doc should not do anything with the state, just need it for the checking.
%% checks all CPs have the correct behaviour of a SE.
alive_supervision(S, SE) ->
    CPs_for_SE = wdgm_config_params:get_CPs_of_SE(SE),
    Correct = lists:foldl(fun (X, Correct) -> (Correct orelse X == 'WDGM_CORRECT') end,
                          true,
                          [check_CP_within_SE(S,CP) || CPref <- wdgm_config_params:get_checkpoints_for_mode(S#state.currentMode, 'AS'),
                                                       CP = wdgm_config_params:get_checkpoint_id(CPref),
                                                       lists:member(CP, CPs_for_SE)]),
    case Correct of
        true ->
            'WDGM_CORRECT';
        false ->
            'WDGM_INCORRECT'
    end.

%% @doc should not do anything with the state, just need it for the checking.
%% checks if a CP have the correct behaviour.
check_CP_within_SE(S, CP) ->
    CPstate = lists:keyfind(CP, 2, S#state.aliveTable),
    SEid = wdgm_config_params:get_SEid_from_CP(S#state.currentMode, CPstate#alive.cpid),
    SE = lists:keyfind(SEid, 2, S#state.supervisedentities),
    {SRC, EAI, MinMargin, MaxMargin} = wdgm_config_params:get_AS_for_CP(S#state.currentMode, CP),
    I = algorithm_for_alive_supervision(CPstate#alive.alive_counter,
                                        SE#supervisedentity.supervision_cycles,
                                        SRC,
                                        EAI),
    case
                 I =< MaxMargin andalso
                 I >= -MinMargin
    of
        true -> 'WDGM_CORRECT';
        _ -> 'WDGM_INCORRECT'
    end.

algorithm_for_alive_supervision(AliveCounter, SupervisionCycles, SRC, EAI) ->
    AliveCounter-SupervisionCycles+(SRC-EAI).


%%-DEADLINE SUPERVISION---------------------------------------------------------

deadline_supervision(_SE) ->
    ok.

%%-LOGICAL SUPERVISION----------------------------------------------------------

logical_supervision(_SE) ->
    ok.
