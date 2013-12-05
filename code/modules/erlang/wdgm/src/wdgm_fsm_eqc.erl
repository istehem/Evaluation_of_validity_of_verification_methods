-module(wdgm_fsm_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-compile(export_all).
-compile({parse_transform,eqc_group_commands}).

-include("wdgm_config.hrl").
-define(C_CODE, wdgm_wrapper).
-include_lib("../ebin/wdgm_wrapper.hrl").


init(S) ->
    {ok,wdgm_global_status_ok,S}.
%% we can asynchronously lock the wdgm_fsm,
wdgm_global_status_ok(_E,S) ->
    {next_state,wdgm_global_status_ok,S}.

wdgm_global_status_ok(E,F,S) ->
    io:fwrite("###~p,~p###",[E,F]),
    {next_state,wdgm_global_status_ok,S}.

start() ->
    wdgm_eqc:start(),
    gen_fsm:start({local,wdgm_fsm},?MODULE,[],[]).

stop() ->
    gen_fsm:sync_send_all_state_event(wdgm_fsm,stop).

handle_sync_event(stop,_,_,_) ->
    {stop,normal,ok,[]}.

terminate(_,_,_) ->
    ok.

wdgm_global_status_ok(S) ->
    [initwdgm_command(S),
     getmode_command(S)].


initial_state() ->
    wdgm_global_status_ok.

initial_state_data() ->
  Rs = wdgm_xml:start(),
  {_, R} = (hd(Rs)), %% why do we get a list of records?
  #state{originalCfg=R}.

%%------------------------------------------------------------------------------
%%------------------------------------------------------------------------------
%%------------------------------------------------------------------------------

%% -WdgM_Init-------------------------------------------------------------------

initwdgm_pre(_,_,S) ->
  S#state.initialized /= true.

initwdgm_command(_S) ->
  {wdgm_global_status_ok,{call, ?MODULE, initwdgm, [frequency([{20, return({eqc_c:address_of('Tst_Cfg1'), false})},
                                   {0, return({{ptr, int, 0}, true})}])]}}.

initwdgm({Ptr, Is_Null}) ->
  gen_fsm:send_event(wdgm_fsm,{write,Ptr,Is_Null}),
  ?C_CODE:'WdgM_Init'(Ptr).

initwdgm_post(_F,_T,S, [{_, Is_Null}], _Ret) ->
  InitialMode = S#state.originalCfg#wdgm.tst_cfg1#tst_cfg1.initial_mode_id,
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  _OffModeEnabled = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.off_mode_enabled,
  check_supervisionstatus(eqc_c:value_of('WdgM_SupervisedEntityMonitorTable')) andalso %% [WDGM268], [WDGM269:446
   eqc_c:value_of('WdgM_GlobalStatus') == 'WDGM_GLOBAL_STATUS_OK' andalso %% [WDGM285]

%    check_deadlinetimestamps() andalso %% [WDGM298]
%    check_logicalactivityflag() andalso %% [WDGM296]
%    check_all_global_and_statics() andalso %% [WDGM018]
%    eqc_c:value_of('SeIdLocalStatusExpiredFirst') == 0 %% [WDGM350]
%    andalso

    (eqc_c:value_of('WdgM_CurrentMode') == InitialMode orelse %% [WDGM135]
      (DevErrorDetect
       andalso
         (Is_Null %% [WDGM255]


%          orelse
%          not is_allowed_config() orelse %% [WDGM010]
%          (not OffModeEnabled andalso is_disabled_watchdogs()) %% [WDGM030]

))).

initwdgm_next(_F,_T,S, _Ret, _Args) ->
  Rs = wdgm_xml:start(),
  {_, R} = (hd(Rs)), %% why do we get a list of records?
  ModeId = S#state.originalCfg#wdgm.tst_cfg1#tst_cfg1.initial_mode_id,
  NewS =
    #state{initialized   = true,
           currentMode   = ModeId,
           originalCfg   = R,
           expired_supervision_cycles_tol = wdgm_config_params:get_expired_supervision_cycles(ModeId),
           globalstatus  = 'WDGM_GLOBAL_STATUS_OK',
           deadlineTable = reset_deadline_table(ModeId), %% [WDGM298]
           logicalTable  = reset_logical_table(wdgm_config_params:get_internal_graphs(),
                                               true)
                        ++ reset_logical_table(wdgm_config_params:get_external_graphs(ModeId),
                                               false),
           aliveTable    = reset_alive_table(ModeId)},
  NewS#state{supervisedentities = reset_supervised_entities(NewS, ModeId)}.

%% -WdgM_GetMode----------------------------------------------------------------

getmode_pre(_,_,S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect
    orelse
    S#state.initialized. %% [WDGM253]

getmode_command(_S) ->
  {wdgm_global_status_ok,{call, ?MODULE, getmode, [frequency([{20, return(false)},
                                       {0, return(true)}])]}}.

getmode(Is_Null) ->
  gen_fsm:send_event(wdgm_fsm,{getmode,Is_Null}),
  Mp =
    case Is_Null of
      true  -> {ptr, "uint8", 0};
      false -> eqc_c:alloc("uint8")
    end,
  R = ?C_CODE:'WdgM_GetMode'(Mp),
  case Is_Null of
    true  -> {R, null};
    false -> {R, eqc_c:deref(Mp)}
  end.

getmode_post(_,_,S, [Is_Null], {R, Mode}) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  case R of
    0 -> eq(Mode, S#state.currentMode); %% [WDGM170]
    1 -> DevErrorDetect andalso (Is_Null orelse %% [WDGM254]
                                 not S#state.initialized) %% [WDGM253]
  end.


%%------------------------------------------------------------------------------
%%------------------------------------------------------------------------------
%%------------------------------------------------------------------------------

%prop_wdgm_fsm() ->
%    ?FORALL(Cmds,commands(?MODULE),
%	    begin
%		start(),
%		{H,_S,Res} = run_commands(?MODULE,Cmds),
%		stop(),
%		aggregate(zip(state_names(H),command_names(Cmds)),
%			  Res == ok)
%	    end).


weight(_,_,{_,_,getmode,_}) -> 1;
weight(_,_,_)               -> 1.

prop_wdgm_fsm() ->
  ?SETUP( fun () -> start(),
                    fun () -> ok end
          end,
          ?FORALL(Cmds, commands(?MODULE),
                  begin
                    eqc_c:restart(),
                    gen_fsm:start({local,wdgm_fsm},?MODULE,[],[]),
                    {H,S,Res} = run_commands(?MODULE,Cmds),
                    stop(),
                    pretty_commands(?MODULE,Cmds,{H,S,Res},
                                    aggregate(collect_res(H,S,Res,Cmds),
                                              Res == ok))
                  end)).

collect_res(_H,_S,_Res,Cmds) ->
  command_names(Cmds).
  %Xs = lists:filter(fun({_,_,{_,_,Name,_}}) -> Name == initwdgm end,Cmds),

%  case Cmds of
%    [] -> [0];
%    Ys -> [{length(Xs)/length(Ys),length(Ys)}]
%  end.

%read_probability(locked,_,_) ->
%    0.6;
%read_probability(unlocked,_,_) ->
%    0.4.


%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% -Helper-functions------------------------------------------------------------

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


not_within_allowed_range(ModeId) ->
  ModeId < 0 orelse ModeId > 255.


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
         {true, true} -> %% [WDGM182]
           FailedAliveTol = wdgm_config_params:get_LSP_failedtolerance(ModeId, SEid),
           SE = lists:keyfind(SEid, 2, SEs),
           SE#supervisedentity{
             failed_alive_supervision_cycle_tol=FailedAliveTol,
             supervision_cycles=0};
         {true, false} -> new_SE_record(ModeId, SEid, true); %% [WDGM209];
         {false, _} -> new_SE_record(ModeId, SEid, false) %% [WDGM207], [WDGM291]
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


