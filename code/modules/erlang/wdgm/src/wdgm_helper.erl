%%% @author  <sebastianwo@MEG-865>
%%% @copyright (C) 2013,
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2013 by  <sebastianwo@MEG-865>

-module(wdgm_helper).

-include("wdgm_config.hrl").
-include("wdgm_wrapper.hrl").

-compile(export_all).

%% used by mainfunction and checkpointreached
check_same_supervisionstatus(_, [], _) ->
  true;
check_same_supervisionstatus(S, [L|Ls], C) ->
  SE = lists:keyfind(C, 2, S#state.supervisedentities),
  %% Due to different implementations the new state may not be updated when
  %% there exists a local or global status that has expired, Updating or not
  %% updateing the state is correct according to the specification hence
  %% supervision status can not always be checked.
  S#state.globalstatus == 'WDGM_GLOBAL_STATUS_EXPIRED' orelse
    S#state.globalstatus == 'WDGM_GLOBAL_STATUS_STOPPED' orelse
    case SE#supervisedentity.localstatus of
      'WDGM_LOCAL_STATUS_EXPIRED' -> true;
      _                           -> check_same_supervisionstatus(S, Ls, C+1)
    end
    andalso
    L#'WdgM_SupervisedEntityMonitor_Tag'.supervision_status == SE#supervisedentity.localstatus andalso
    L#'WdgM_SupervisedEntityMonitor_Tag'.logicalsupervision_result == SE#supervisedentity.locallogicalstatus andalso
    L#'WdgM_SupervisedEntityMonitor_Tag'.deadlinesupervision_result == SE#supervisedentity.localdeadlinestatus andalso
    L#'WdgM_SupervisedEntityMonitor_Tag'.alivesupervision_result == SE#supervisedentity.localalivestatus.


%% used by setmode
check_next_supervisionstatus(_, [], _) -> true;
check_next_supervisionstatus(S, [L|Ls], C) ->
  SE = lists:keyfind(C, 2, S#state.supervisedentities),
  (S#state.globalstatus == 'WDGM_GLOBAL_STATUS_OK' orelse
   S#state.globalstatus == 'WDGM_GLOBAL_STATUS_FAILED') andalso
  case L#'WdgM_SupervisedEntityMonitor_Tag'.supervision_status of
    'WDGM_LOCAL_STATUS_OK' ->
       SE#supervisedentity.localstatus == 'WDGM_LOCAL_STATUS_OK' orelse %% [WDGM182]
        SE#supervisedentity.localstatus == 'WDGM_LOCAL_STATUS_DEACTIVATED' %% [WDGM209]
        andalso check_next_supervisionstatus(S, Ls, C+1);
    'WDGM_LOCAL_STATUS_DEACTIVATED' ->
      ((SE#supervisedentity.localstatus == 'WDGM_LOCAL_STATUS_OK' orelse %% [WDGM207]
        SE#supervisedentity.localstatus == 'WDGM_LOCAL_STATUS_FAILED' %% [WDGM291]
        andalso
          (L#'WdgM_SupervisedEntityMonitor_Tag'.alivesupervision_result == 'WDGM_CORRECT' andalso
           L#'WdgM_SupervisedEntityMonitor_Tag'.deadlinesupervision_result == 'WDGM_CORRECT' andalso
           L#'WdgM_SupervisedEntityMonitor_Tag'.logicalsupervision_result == 'WDGM_CORRECT' andalso
           L#'WdgM_SupervisedEntityMonitor_Tag'.failed_reference_supervisioncycles == 0)) %% [WDGM315]
       orelse
       SE#supervisedentity.localstatus == 'WDGM_LOCAL_STATUS_DEACTIVATED') %% [WDGM182]
        andalso
        check_next_supervisionstatus(S, Ls, C+1);
    Status ->
      SE#supervisedentity.localstatus == Status %% [WDGM182]
        andalso check_next_supervisionstatus(S, Ls, C+1)
  end.


is_allowed_mode(ModeId) ->
  lists:keymember(ModeId, 1, wdgm_config_params:get_modes()).


%% used by init
check_supervisionstatus([]) -> true;
check_supervisionstatus([L|Ls]) ->
  case L#'WdgM_SupervisedEntityMonitor_Tag'.supervision_status of
    'WDGM_LOCAL_STATUS_OK' ->
      check_supervision_results(L) andalso check_supervisionstatus(Ls);
    'WDGM_LOCAL_STATUS_DEACTIVATED' ->
      check_supervisionstatus(Ls)
  end.

check_supervision_results({_, _, 'WDGM_LOCAL_STATUS_OK', _, 'WDGM_CORRECT', 'WDGM_CORRECT', 'WDGM_CORRECT'}) ->
  true;
check_supervision_results(_) ->
  false.

findKeyIndex(E, P, Ls) -> findKeyIndex(E, P, Ls, 1).
findKeyIndex(_, _, [], _) -> not_found;
findKeyIndex(Elem, P, [Tuple|Ls],N) -> case element(P, Tuple) of
                                         Elem -> N;
                                         _ -> findKeyIndex(Elem, P, Ls, N+1)
                                       end.

reset_alive_table(ModeId) ->
  lists:map(fun (CPref) ->
                CPid = wdgm_config_params:get_checkpoint_id(CPref),
                {SRC, EAI, Min, Max} = hd(wdgm_config_params:get_AS_for_CP(ModeId, CPid)),
                #alive{cpid          = CPid,
                       supervision_reference_cycles = SRC,
                       expected_alive_indications   = EAI,
                       minmargin     = Min,
                       maxmargin     = Max,
                       alive_counter = 0}
            end,
            wdgm_config_params:get_checkpoints_for_mode(ModeId, 'AS')).

reset_deadline_table(ModeId) ->
  lists:map(fun (DS) ->
                {Start, Stop, Min, Max} = wdgm_config_params:get_deadline_params(DS),
                #deadline{startCP    = Start,
                          stopCP     = Stop,
                          minmargin  = Min,
                          maxmargin  = Max,
                          timestamp  = 0,
                          timer      = 0}
            end,
            wdgm_config_params:get_deadline_supervision(ModeId)).

reset_logical_table(Table, Is_Internal) ->
  lists:map(fun ({Init, Finals, Transitions}) ->
                #logical{initCP       = Init,
                         finalCPs     = Finals,
                         cps_in_graph =
                           lists:usort(lists:flatmap(fun ({A,B}) ->
                                                         [A, B]
                                                     end,
                                                     Transitions)),
                         graph        = Transitions,
                         activity     = false,
                         is_internal  = Is_Internal}
            end,
            Table).

reset_supervised_entities(S, ModeId) ->
  case S#state.supervisedentities of
    undefined -> %% not initialized...
      [new_SE_record(ModeId,
                     SEid,
                     wdgm_config_params:is_activated_SE_in_mode(ModeId, SEid))
       || SEid <- wdgm_config_params:get_SEs_from_LS(ModeId)];
    SEs ->
      [case
         {wdgm_config_params:is_activated_SE_in_mode(ModeId, SEid),
         (lists:keyfind(SEid, 2, SEs))#supervisedentity.localstatus /= 'WDGM_LOCAL_STATUS_DEACTIVATED'}
       of
         {true, true}  -> %% [WDGM182]
           FailedAliveTol = wdgm_config_params:get_LSP_failedtolerance(ModeId, SEid),
           SE = lists:keyfind(SEid, 2, SEs),
           SE#supervisedentity{
             failed_alive_supervision_cycle_tol=FailedAliveTol,
             supervision_cycles=0};
         {true, false} -> new_SE_record(ModeId, SEid, true); %% [WDGM209];
         {false, _}    -> new_SE_record(ModeId, SEid, false) %% [WDGM207], [WDGM291]
       end
       || SEid <- wdgm_config_params:get_SEs_from_LS(ModeId)]
  end.

new_SE_record(ModeId, SEid, Activated) ->
  LocalStatus =
    case Activated of
      true -> 'WDGM_LOCAL_STATUS_OK';
      false -> 'WDGM_LOCAL_STATUS_DEACTIVATED'
    end,
  FailedAliveTol = wdgm_config_params:get_LSP_failedtolerance(ModeId, SEid),
  #supervisedentity{seid=SEid,
                    localstatus=LocalStatus,
                    localalivestatus='WDGM_CORRECT',
                    localdeadlinestatus='WDGM_CORRECT',
                    locallogicalstatus='WDGM_CORRECT',
                    failed_alive_supervision_cycle_tol=FailedAliveTol,
                    failed_reference_supervision_cycles=0,
                    supervision_cycles=0}.

%% could possible search for invalid variables.
%% This is Mecels variable names.
%% Maybe need to check that ModeId is valid.
check_deadlinetimestamps(ModeId) -> %% [WDGM298]
  Len = length(wdgm_config_params:get_deadline_supervision(ModeId)),
  lists:all(fun(DRec) -> DRec#'WdgM_DeadLineSupervisionMonitor_Tag'.timestamp_var == 0 end, (eqc_c:read_array((eqc_c:value_of('WdgM_MonitorTableRef'))#'WdgM_MonitorTableRefType_Tag'.'DeadLineSupervisionMonitorTablePtr', Len))).


%% could possible search for invalid variables.
%% This is Mecels variable names.
%% Maybe need to check that ModeId is valid.
check_logicalactivityflag(ModeId) -> %% [WDGM296]
  ILen = length(wdgm_config_params:get_internal_graphs()),
  ELen = length(wdgm_config_params:get_external_graphs(ModeId)),
  lists:all(fun(DRec) -> DRec#'WdgM_LogicalSupervisionMonitor_Tag'.activity_flag == 0 end, (eqc_c:read_array((eqc_c:value_of('WdgM_MonitorTableRef'))#'WdgM_MonitorTableRefType_Tag'.'LogicalSupervisionMonitorTablePtr', ILen+ELen))).
