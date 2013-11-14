-module(wdgm_main_eqc).

-compile(export_all).
-include("wdgm_init_eqc.erl").

init() ->
    %% set local supervision status to OK || DEACTIVATED
    ok.

local_SE_status(S, SE) ->
    AliveRes = alive_supervision(S, SE),
    %% DeadlineRes = deadline_supervision(SE),
    %% LogicalRes = logical_supervision(SE),
    FAIL_TOL = 'WdgMFailedAliveSupervisionRefCycleTol',
    case
        {SE#supervisedentity.localstatus, AliveRes, DeadlineRes, LogicalRes}
    of
        {'WDGM_LOCAL_STATUS_OK', 'WDGM_CORRECT', 'WDGM_CORRECT', 'WDGM_CORRECT'}
                                         -> 'WDGM_LOCAL_STATUS_OK'; %% [WDGM201]
        {'WDGM_LOCAL_STATUS_OK', 'WDGM_INCORRECT', 'WDGM_CORRECT', 'WDGM_CORRECT'}
          when FAIL_TOL == 0        -> 'WDGM_LOCAL_STATUS_EXPIRED'; %% [WDGM202]
        {'WDGM_LOCAL_STATUS_OK', 'WDGM_CORRECT', 'WDGM_INCORRECT', 'WDGM_CORRECT'}
                                    -> 'WDGM_LOCAL_STATUS_EXPIRED'; %% [WDGM202]
        {'WDGM_LOCAL_STATUS_OK', 'WDGM_CORRECT', 'WDGM_CORRECT', 'WDGM_INCORRECT'}
                                    -> 'WDGM_LOCAL_STATUS_EXPIRED'; %% [WDGM202]
        {'WDGM_LOCAL_STATUS_OK', 'WDGM_INCORRECT', 'WDGM_CORRECT', 'WDGM_CORRECT'}
          when FAIL_TOL > 0
                      -> FAIL_CYCLES++, 'WDGM_LOCAL_STATUS_FAILED'; %% [WDGM203]
        {'WDGM_LOCAL_STATUS_FAILED', 'WDGM_INCORRECT', 'WDGM_CORRECT', 'WDGM_CORRECT'}
          when FAIL_CYCLES <= FAIL_TOL
                      -> FAIL_CYCLES++, 'WDGM_LOCAL_STATUS_FAILED'; %% [WDGM204]
        {'WDGM_LOCAL_STATUS_FAILED', 'WDGM_CORRECT', 'WDGM_CORRECT', 'WDGM_CORRECT'}
          when FAIL_CYCLES > 1
                      -> FAIL_CYCLES--, 'WDGM_LOCAL_STATUS_FAILED'; %% [WDGM300]
        {'WDGM_LOCAL_STATUS_FAILED', 'WDGM_CORRECT', 'WDGM_CORRECT', 'WDGM_CORRECT'}
          when FAIL_CYCLES == 1
                          -> FAIL_CYCLES--, 'WDGM_LOCAL_STATUS_OK'; %% [WDGM205]
        {'WDGM_LOCAL_STATUS_FAILED', 'WDGM_INCORRECT', 'WDGM_CORRECT', 'WDGM_CORRECT'}
          when FAIL_CYCLES > FAIL_TOL
                                    -> 'WDGM_LOCAL_STATUS_EXPIRED'; %% [WDGM206]
        {'WDGM_LOCAL_STATUS_FAILED', 'WDGM_CORRECT', 'WDGM_INCORRECT', 'WDGM_CORRECT'}
                                    -> 'WDGM_LOCAL_STATUS_EXPIRED'; %% [WDGM206]
        {'WDGM_LOCAL_STATUS_FAILED', 'WDGM_CORRECT', 'WDGM_CORRECT', 'WDGM_INCORRECT'}
                                    -> 'WDGM_LOCAL_STATUS_EXPIRED'; %% [WDGM206]
        {_, _, _, _} -> 'WDGM_LOCAL_STATUS_EXPIRED' %% should never get here
    end.

global_status(S) ->
    Statuses = [local_SE_status(S, SE) || SE <- S#state.supervisedentities],
    {Failed, Expired} = lists:foldl(fun (X, {Failed, Expired}) ->
                                            {(Failed  || 'WDGM_LOCAL_STATUS_FAILED'  == X),
                                             (Expired || 'WDGM_LOCAL_STATUS_EXPIRED' == X)}
                                    end, {false, false}, Statuses),
    update_statuses(), %% update local SE statuses
    EXPIRED_TOL = WdgMExpiredSupervisionCycleTol,
    EXPIRED_CYCLES = S#state.expiredsupervisioncycles,
    case
        {S#state.globalstatus, Failed, Expired}
    of
        {'WDGM_GLOBAL_STATUS_OK', false, false} ->
            'WDGM_GLOBAL_STATUS_OK'; %% [WDGM078]
        {'WDGM_GLOBAL_STATUS_OK', true, false} ->
            'WDGM_GLOBAL_STATUS_FAILED'; %% [WDGM076]
        {'WDGM_GLOBAL_STATUS_OK', _, true} when EXPIRED_TOL > 0 ->
            'WDGM_GLOBAL_STATUS_EXPIRED'; %% [WDGM215]
        {'WDGM_GLOBAL_STATUS_OK', _, true} when EXPIRED_TOL == 0 ->
            'WDGM_GLOBAL_STATUS_STOPPED'; %% [WDGM216]
        {'WDGM_GLOBAL_STATUS_FAILED', true, false} ->
            'WDGM_GLOBAL_STATUS_FAILED'; %% [WDGM217]
        {'WDGM_GLOBAL_STATUS_FAILED', false, false} ->
            'WDGM_GLOBAL_STATUS_OK'; %% [WDGM218]
        {'WDGM_GLOBAL_STATUS_FAILED', _, true} when EXPIRED_TOL > 0 ->
            'WDGM_GLOBAL_STATUS_EXPIRED'; %% [WDGM077]
        {'WDGM_GLOBAL_STATUS_FAILED', _, true} when EXPIRED_TOL == 0 ->
            'WDGM_GLOBAL_STATUS_STOPPED'; %% [WDGM117]
        {'WDGM_GLOBAL_STATUS_EXPIRED', _, true} when EXPIRED_CYCLES <= EXPIRED_TOL ->
            EXPIRED_CYCLES++, 'WDGM_GLOBAL_STATUS_EXPIRED'; %% [WDGM219]
        {'WDGM_GLOBAL_STATUS_EXPIRED', _, true} when EXPIRED_CYCLES > EXPIRED_TOL ->
            'WDGM_GLOBAL_STATUS_STOPPED'; %% [WDGM220]
        {'WDGM_GLOBAL_STATUS_STOPPED', _, _} ->
            'WDGM_GLOBAL_STATUS_STOPPED'; %% [WDGM221]
        {_, _, _} ->
            'WDGM_GLOBAL_STATUS_STOPPED' %% should never get here
    end.

%% @doc should not do anything with the state, just need it for the checking.
%% checks all CPs have the correct behaviour of a SE.
alive_supervision(S, SE) ->
    CPs_for_SE = get_checkpoints_for_SE(SE),
    Correct = lists:foldl(fun (X, Correct) -> (Correct || X == 'WDGM_CORRECT') end,
                          true,
                          [check_CP_within_SE(S,CP) || CP <- get_checkpoints_from_ASs(),
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
    CPstate = lists:keyfind(CPid, 2, S#state.aliveTable),
    SEid = get_SEid_from_CP(CP),
    SE = lists:keyfind(SEid, 2, S#state.supervisedentities),
    SRC = car_xml:get_value("WdgMSupervisionReferenceCycle", AS), %% TODO: lyft ur
    EAI = car_xml:get_value("WdgMExpectedAliveIndications", AS), %% TODO: lyft ur
    I = algorithm_for_alive_supervision(CPstate#alive.alive_counter,
                                        SE#supervisedentity.supervision_cycles,
                                        SRC,
                                        EAI),
    case
                 I =< car_xml:get_value("WdgMMaxMargin", AS) andalso  %% TODO: lyft ur
                 I >= -car_xml:get_value("WdgMMinMargin", AS) %% TODO: lyft ur
    of
        true -> 'WDGM_CORRECT';
        _ -> 'WDGM_INCORRECT'
    end.

algorithm_for_alive_supervision(AliveCounter, SupervisionCycles, SRC, EAI) ->
    AliveCounter-SupervisionCycles+(SRC-EAI).
