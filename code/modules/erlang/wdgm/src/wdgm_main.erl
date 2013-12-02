-module(wdgm_main).

-compile(export_all).
-include("wdgm_config.hrl").

%%-GLOBAL STATUS----------------------------------------------------------------

global_status(S) ->
  %% NewSEsAndAliveCPs = [local_SE_status(S, SE) || SE <- S#state.supervisedentities],
  %% NewSEs = lists:map(fun (Elem) -> element(1, Elem) end, NewSEsAndAliveCPs),
  %% NewAliveTable = lists:map(fun (Elem) -> element(2, Elem) end, NewSEsAndAliveCPs),
  NewS = foldresult(fun local_SE_status/2, S, S#state.supervisedentities),
  {Failed, Expired} = lists:foldl(fun (SE, {Failed, Expired}) ->
                                      {(Failed  orelse 'WDGM_LOCAL_STATUS_FAILED'  == SE#supervisedentity.localstatus),
                                       (Expired orelse 'WDGM_LOCAL_STATUS_EXPIRED' == SE#supervisedentity.localstatus)}
                                  end, {false, false}, NewS#state.supervisedentities),
  EXPIRED_TOL = NewS#state.expired_supervision_cycles_tol,
  EXPIRED_CYCLES = NewS#state.expiredsupervisioncycles,
  {Status, NewExpiredCycles} = check_global_status({NewS#state.globalstatus, Failed, Expired}, EXPIRED_TOL, EXPIRED_CYCLES),
  EXPIRED_SE = (lists:keyfind('WDGM_LOCAL_STATUS_EXPIRED', 3, NewS#state.supervisedentities)),
  NewS#state{
    globalstatus=Status,
    expiredSEid=case EXPIRED_SE of
                  false -> undefined;
                  SE -> SE#supervisedentity.seid
                end,
    expiredsupervisioncycles=NewExpiredCycles}.

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
  NewS = alive_supervision(S, SE),
  NewSE = lists:keyfind(SE#supervisedentity.seid, 2, NewS#state.supervisedentities),
  AliveRes = NewSE#supervisedentity.localalivestatus,
  DeadlineRes = deadline_supervision(NewSE),
  LogicalRes = logical_supervision(NewSE),
  FAIL_TOL = NewSE#supervisedentity.failed_alive_supervision_cycle_tol,
  FAIL_CYCLES = NewSE#supervisedentity.failed_reference_supervision_cycles,
  {LocalStatus, FailCycles} = check_local_status({NewSE#supervisedentity.localstatus, AliveRes, DeadlineRes, LogicalRes}, FAIL_TOL, FAIL_CYCLES),
  NewS#state{supervisedentities=
               lists:keyreplace(NewSE#supervisedentity.seid, 2, NewS#state.supervisedentities,
                                NewSE#supervisedentity{localstatus=LocalStatus,
                                                       supervision_cycles=SE#supervisedentity.supervision_cycles+1,
                                                       failed_reference_supervision_cycles=FailCycles})}.

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
  NewS = foldresult(fun check_CP_within_SE/2,
                    S,
                    [CPref
                     || CPref <- wdgm_config_params:get_checkpoints_for_mode(S#state.currentMode, 'AS'),
                        lists:member(wdgm_config_params:get_checkpoint_id(CPref), CPs_for_SE)]),
  AliveTableForSE = [AliveCP#alive.status
                     || AliveCP <- NewS#state.aliveTable,
                        lists:member(AliveCP#alive.cpid, CPs_for_SE),
                        (SE#supervisedentity.supervision_cycles+1) rem AliveCP#alive.supervision_reference_cycles == 0],
  Correct = lists:all(fun (Elem) -> Elem == 'WDGM_CORRECT' end, AliveTableForSE),
  LocalSEalivestatus=
    case {Correct, AliveTableForSE} of
      {_, []}    -> SE#supervisedentity.localalivestatus;
      {true, _}  ->
        'WDGM_CORRECT';
      {false, _} ->
        'WDGM_INCORRECT'
    end,
  NewS#state{supervisedentities=lists:keyreplace(SE#supervisedentity.seid,
                                                 2,
                                                 NewS#state.supervisedentities,
                                                 SE#supervisedentity{localalivestatus=LocalSEalivestatus})}.

%% checks if a CP have the correct behaviour.
check_CP_within_SE(S, CPref) ->
  CPid = wdgm_config_params:get_checkpoint_id(CPref),
  CPstate = lists:keyfind(CPid, 2, S#state.aliveTable),
  SEid = wdgm_config_params:get_SE_id(CPref),
  SE = lists:keyfind(SEid, 2, S#state.supervisedentities),
  SRC = CPstate#alive.supervision_reference_cycles,
  EAI = CPstate#alive.expected_alive_indications,
  MinMargin = CPstate#alive.minmargin,
  MaxMargin = CPstate#alive.maxmargin,
  SC = SE#supervisedentity.supervision_cycles+1, %% because dont update when SC==0
  {NewCPalivestatus, NewCPalivecounter} =
    case SC rem SRC == 0 of
      true ->
        I = algorithm_for_alive_supervision(CPstate#alive.alive_counter, EAI),
        case
          I =< MaxMargin andalso
          I >= -MinMargin
        of
          true -> {'WDGM_CORRECT', 0};
          _    -> {'WDGM_INCORRECT', 0}
        end;
      false    -> {CPstate#alive.status, CPstate#alive.alive_counter}
    end,
  S#state{aliveTable=lists:keyreplace(CPid, 2,
                                      S#state.aliveTable,
                                      CPstate#alive{status=NewCPalivestatus,
                                                    alive_counter=NewCPalivecounter})}.

algorithm_for_alive_supervision(AliveCounter, EAI) ->
  AliveCounter-EAI.
%%  AliveCounter-SupervisionCycles+(SRC-EAI). %% [WDGM074] Stämmer detta?


%%-DEADLINE SUPERVISION---------------------------------------------------------

deadline_supervision(SE) ->
  SE#supervisedentity.localdeadlinestatus.

%%-LOGICAL SUPERVISION----------------------------------------------------------

logical_supervision(SE) ->
  SE#supervisedentity.locallogicalstatus.


%%-HELPER functions-------------------------------------------------------------
foldresult(_Fun, State, []) ->
  State;
foldresult(Fun, State, [X|Xs]) ->
  case
    %% så modellen passar c-koden => break out if expired
    lists:keyfind('WDGM_LOCAL_STATUS_EXPIRED', 3, State#state.supervisedentities)
  of
    false ->
      NewS = Fun(State, X),
      foldresult(Fun, NewS, Xs);
    _ -> State
  end.
