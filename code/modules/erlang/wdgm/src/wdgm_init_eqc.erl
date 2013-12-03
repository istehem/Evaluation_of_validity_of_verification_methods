-module(wdgm_init_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

-include("wdgm_config.hrl").
-define(C_CODE, wdgm_wrapper).
-include_lib("../ebin/wdgm_wrapper.hrl").


initial_state() ->
  Rs = wdgm_xml:start(),
  {_, R} = (hd(Rs)), %% why do we get a list of records?
  #state{originalCfg=R}.

%% -WdgM_Init-------------------------------------------------------------------

init_pre(S) ->
  S#state.initialized /= true.

init_command(_S) ->
  {call, ?MODULE, init, [frequency([{20, return({eqc_c:address_of('Tst_Cfg1'), false})},
                                   {0, return({{ptr, int, 0}, true})}])]}.

init({Ptr, _}) ->
  ?C_CODE:'WdgM_Init'(Ptr).

init_post(S, [{_, Is_Null}], _Ret) ->
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
           deadlineTable = reset_deadline_table(ModeId), %% [WDGM298]
           logicalTable  = reset_logical_table(ModeId),
           aliveTable    = reset_alive_table(ModeId)},
  NewS#state{supervisedentities = reset_supervised_entities(NewS, ModeId)}.

%% -WdgM_GetMode----------------------------------------------------------------

getmode_pre(S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect
    orelse
    S#state.initialized. %% [WDGM253]

getmode_command(_S) ->
  {call, ?MODULE, getmode, [frequency([{20, return(false)},
                                       {0, return(true)}])]}.

getmode(Is_Null) ->
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

getmode_post(S, [Is_Null], {R, Mode}) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  case R of
    0 -> eq(Mode, S#state.currentMode); %% [WDGM170]
    1 -> DevErrorDetect andalso (Is_Null orelse %% [WDGM254]
                                 not S#state.initialized) %% [WDGM253]
  end.

%% -WdgM_SetMode----------------------------------------------------------------

setmode_pre(S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect
   orelse
    S#state.initialized.

setmode_command(_S) ->
  {call, ?MODULE, setmode, [choose(0,3), choose(1,2)]}.

setmode(Mode, CallerId) ->
  ?C_CODE:'WdgM_SetMode'(Mode, CallerId).

setmode_post(S, [ModeId, Cid], Ret) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  (S#state.globalstatus /= 'WDGM_GLOBAL_STATUS_OK' andalso
   S#state.globalstatus /= 'WDGM_GLOBAL_STATUS_FAILED' andalso
   S#state.currentMode == eqc_c:value_of('WdgM_CurrentMode')) orelse %% [WDGM316], [WDGM145]


  ((S#state.globalstatus == eqc_c:value_of('WdgM_GlobalStatus') orelse  %% [WDGM317]
    S#state.globalstatus == undefined) andalso %% if not initialized everything under will failed
%%    S#state.expiredsupervisioncycles == eqc_c:value_of(expiredsupervisioncycles) andalso %% [WDGM317]

    case Ret of
      0 ->
        check_next_supervisionstatus(S, eqc_c:value_of('WdgM_SupervisedEntityMonitorTable'), 0); %% [WDGM207], [WDGM291], [WDGM209], [WDGM182], [WDGM316]
      1 ->
        DevErrorDetect andalso (not_within_allowed_range(ModeId) orelse %% [WDGM020]
                                 not S#state.initialized orelse %% [WDGM021]
%%%          orelse (not OffModeEnabled andalso is_disabled_watchdogs()) orelse %% [WDGM031]
            lists:member(Cid, S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.caller_ids)) %% [WDGM245]
    end).
%% [WDGM186], [WDGM142]

setmode_next(S, Ret, [ModeId, _Cid]) ->
  case Ret of
    0 -> case
           (S#state.globalstatus == 'WDGM_GLOBAL_STATUS_OK' orelse
            S#state.globalstatus == 'WDGM_GLOBAL_STATUS_FAILED')
         of
           true ->
             S#state{currentMode = ModeId,
                     expired_supervision_cycles_tol = wdgm_config_params:get_expired_supervision_cycles(ModeId),
                     supervisedentities = reset_supervised_entities(S, ModeId),
                     deadlineTable      = reset_deadline_table(ModeId),
                     logicalTable       = reset_logical_table(ModeId),
                     aliveTable         = reset_alive_table(ModeId)};
           false -> %% [WDGM316], [WDGM145]
             S
         end;
    _ -> S %% if WdgIf_SetMode failed set globalstatus='WDGM_GLOBAL_STATUS_STOPPED'? %% [WDGM139]
  end.

%% -WdgM_DeInit-----------------------------------------------------------------

deinit_pre(S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect
    orelse
    S#state.initialized.

deinit_command(_S) ->
  {call, ?MODULE, deinit, []}.

deinit() ->
  ?C_CODE:'WdgM_DeInit'().

%% [WDGM154] should check something with WdgM_SetMode
deinit_post(S, _Args, _Ret) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  case S#state.globalstatus of
    'WDGM_GLOBAL_STATUS_OK' -> eq(eqc_c:value_of('WdgM_GlobalStatus'),
                                  'WDGM_GLOBAL_STATUS_DEACTIVATED'); %% [WDGM286]
    undefined               -> (DevErrorDetect andalso not S#state.initialized); %% [WDGM288]
    Status                  -> eq(eqc_c:value_of('WdgM_GlobalStatus'), Status) %% lack of wdgm286?
  end.

deinit_next(S, _Ret, _Args) ->
    case S#state.globalstatus of
      'WDGM_GLOBAL_STATUS_OK' ->
        S#state{initialized  = false,
                globalstatus = 'WDGM_GLOBAL_STATUS_DEACTIVATED', %% [WDGM286]
                currentMode  = -1};
      _ -> S
    end.

%% -WdgM_CheckpointReached------------------------------------------------------

checkpointreached_pre(S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect
    orelse
    S#state.initialized.

checkpointreached_command(_S) ->
  {call, ?MODULE, checkpointreached,
   ?LET(SeID, choose(0,4), checkpoint_gen(SeID))}.

checkpoint_gen(SeID) ->
  [SeID, oneof(wdgm_config_params:get_CPs_of_SE(SeID)++[999])].

%% uint16 SupervisedEntityIdType, uint16 CheckpointIdType
checkpointreached(SeID, CPId) ->
  ?C_CODE:'WdgM_CheckpointReached'(SeID, CPId).

checkpointreached_post(S, Args=[SEid, CPId], Ret) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  MonitorTable = eqc_c:value_of('WdgM_SupervisedEntityMonitorTable'),
  case Ret of
    1 -> DevErrorDetect andalso
           checkpoint_postcondition(S, Args) andalso %% [WDGM278], [WDGM279], [WDGM284], [WDGM319]
           (S#state.supervisedentities == undefined orelse check_same_supervisionstatus(S, MonitorTable, 0));
    0 -> NextS = checkpointreached_next(S, 0, [SEid, CPId]),
         NextS#state.globalstatus == 'WDGM_GLOBAL_STATUS_EXPIRED' orelse
          check_same_supervisionstatus(NextS, MonitorTable, 0) %% [WDGM322], [WDGM323]
  end.

checkpoint_postcondition(S, [SeID, CPId]) ->
  not S#state.initialized orelse %% [WDGM279]
    not lists:member(CPId, wdgm_config_params:get_CPs_of_SE(SeID)) orelse %% [WDGM284]
    not wdgm_config_params:is_activated_SE_in_mode(S#state.currentMode, SeID). %% [WDGM319] ([WDGM278])

checkpointreached_next(S, _Ret, Args = [SEid, CPid]) ->
  case not checkpoint_postcondition(S, Args) of
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

%% -WdgM_UpdateAliveCounter-----------------------------------------------------
%% Deprecated

%% -WdgM_GetLocalStatus---------------------------------------------------------

getlocalstatus_pre(S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect
    orelse
    S#state.initialized.

getlocalstatus_command(_S) ->
  {call, ?MODULE, getlocalstatus, [choose(1,5),
                                    frequency([{20, return(false)},
                                               {0, return(true)}])]}.

getlocalstatus(SEid, Is_Null) ->
  Sp =
    case Is_Null of
      true  -> {ptr, "WdgM_LocalStatusType", 0};
      false -> eqc_c:alloc("WdgM_LocalStatusType")
    end,
  R  = ?C_CODE:'WdgM_GetLocalStatus'(SEid, Sp),
  case Is_Null of
    true  -> {R, null};
    false -> {R, eqc_c:deref(Sp)}
  end.

getlocalstatus_post(S, [SEid, Is_Null], {Ret, Status}) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  case Ret of
    0 ->   SE = lists:keyfind(SEid, 2, S#state.supervisedentities),
           ((S#state.expiredSEid /= undefined andalso S#state.expiredSEid < SEid) orelse
           Status == SE#supervisedentity.localstatus); %% [WDGM171].
    1 -> DevErrorDetect andalso (not S#state.initialized orelse %% [WDGM173]
                                 Is_Null orelse %% [WDGM257]
                                 lists:keyfind(SEid, 2, S#state.supervisedentities) == false) %% [WDGM172]
  end.

getlocalstatus_next(S, _Ret, _Args) ->
  S.

%% -WdgM_GetGlobalStatus--------------------------------------------------------

getglobalstatus_pre(S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect
    orelse
    S#state.initialized.

getglobalstatus_command(_S) ->
  {call, ?MODULE, getglobalstatus, [frequency([{20, return(false)},
                                               {0, return(true)}])]}.

getglobalstatus(Is_Null) ->
  Sp =
    case Is_Null of
      true  -> {ptr, "WdgM_GlobalStatusType", 0};
      false -> eqc_c:alloc("WdgM_GlobalStatusType")
    end,
  R = ?C_CODE:'WdgM_GetGlobalStatus'(Sp),
  case Is_Null of
    true  -> {R, null};
    false -> {R, eqc_c:deref(Sp)}
  end.

getglobalstatus_post(S, [Is_Null], Ret) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  case Ret of
    {0, R} -> eq(R, S#state.globalstatus);
    {1, _} -> DevErrorDetect andalso (Is_Null orelse %% [WDGM344], [WDGM258]
                                      not S#state.initialized) %% [WDGM176]
  end.

getglobalstatus_next(S, _Ret, _Args) ->
  S.

%% -WdgM_PerformReset-----------------------------------------------------------

performreset_pre(S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect
    orelse
    S#state.initialized.

performreset_command (_S) ->
  {call, ?MODULE, performreset, []}.

performreset() ->
  ?C_CODE:'WdgM_PerformReset'().

performreset_post(S, _Args, _Ret) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  DevErrorDetect orelse S#state.initialized. %% [WDGM270]
%% [WDGM232] går inte att göra i dagsläget pga avsaknad av WDGIF
%% [WDGM233] ??? global status not to be considered anymore

performreset_next(S, _Ret, _Args) ->
  S.

%% -WdgM_GetFirstExpiredSEID----------------------------------------------------

getfirstexpiredseid_pre(_S) ->
  true. %% [WDGM348]

getfirstexpiredseid_command(_S) ->
  {call, ?MODULE, getfirstexpiredseid, [frequency([{20, return(false)},
                                                   {0, return(true)}])]}.

getfirstexpiredseid(Is_Null) ->
  Sp =
    case Is_Null of
      true -> {ptr, "WdgM_SupervisedEntityIdType", 0};
      false -> eqc_c:alloc("WdgM_SupervisedEntityIdType")
    end,
  R = ?C_CODE:'WdgM_GetFirstExpiredSEID'(Sp),
  case Is_Null of
    true -> {R, null};
    false -> {R, eqc_c:deref(Sp)}
  end.

getfirstexpiredseid_post(S, [Is_Null], Ret) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.defensive_behavior,
  case Ret of
    {0, SEid} -> SEid == S#state.expiredSEid; %% [WDGM349]
    {1, SEid} -> (S#state.expiredSEid == undefined andalso
                  SEid == 0) %% [WDGM349]
                   orelse
                     (DevErrorDetect andalso Is_Null) %% [WDGM347]
  end.

getfirstexpiredseid_next(S, _Ret, _Args) ->
  S.

%% -WdgM_MainFunction-----------------------------------------------------------

mainfunction_pre(S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.defensive_behavior orelse
    S#state.initialized. %% [WDGM039]

mainfunction_command(_S) ->
  {call, ?MODULE, mainfunction, []}.

mainfunction() ->
  ?C_CODE:'WdgM_MainFunction'().

mainfunction_post(S, _Args, _Ret) ->
  %% [WDGM325] set localstatus based on [WDGM201], [WDGM202], [WDGM203], [WDGM204], [WDGM300], [WDGM205], [WDGM206], [WDGM208]
  %% [WDGM214] globalstatus == mainfunction_next.globalstatus
  %% [WDGM326] set globalstatus based on [WDGM078], [WDGM076], [WDGM215], [WDGM216], [WDGM217], [WDGM218], [WDGM077], [WDGM117], [WDGM219], [WDGM220], [WDGM221]
  %% [WDGM324] perform alive supervision based on [WDGM098], [WDGM074], [WDGM115], [WDGM083]
  %% WDGIF [WDGM223], [WDGM328]
  %% OS [WDGM275]
  %% manage corresponding error handling [WDGM327]
  %% S#state.globalstatus == 'WDGM_GLOBAL_STATUS_DEACTIVATED' %% [WDGM063] where???? maybe in OS?

  DefensiveBehaviour = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.defensive_behavior,
  GlobalStatus = eqc_c:value_of('WdgM_GlobalStatus'),
  NextS = mainfunction_next(S, 0, 0),
  MonitorTable = eqc_c:value_of('WdgM_SupervisedEntityMonitorTable'),

  Behaviour =
    ((GlobalStatus == 'WDGM_GLOBAL_STATUS_EXPIRED' andalso
      eqc_c:value_of('SeIdLocalStatusExpiredFirst') /= 0 andalso
      NextS#state.expiredSEid /= undefined) %% [WDGM351]
     orelse
       (S#state.initialized andalso
        (NextS#state.globalstatus == 'WDGM_GLOBAL_STATUS_EXPIRED'
         orelse check_same_supervisionstatus(NextS, MonitorTable, 0))) %% [WDGM325]
       andalso
     NextS#state.globalstatus == GlobalStatus), %% [WDGM214], [WDGM326]
  case DefensiveBehaviour of
    true ->
      case S#state.initialized of
        true -> Behaviour;
        false ->
          (S#state.globalstatus == GlobalStatus orelse (S#state.globalstatus == undefined
            andalso GlobalStatus == 'WDGM_GLOBAL_STATUS_OK')) andalso
            (S#state.supervisedentities == undefined orelse
            check_same_supervisionstatus(S, MonitorTable, 0))
      end; %% [WDGM039]
    false -> Behaviour
  end.

mainfunction_next(S, _Ret, _Args) ->
  case
    S#state.initialized andalso
    S#state.globalstatus /= 'WDGM_GLOBAL_STATUS_STOPPED' %%  [WDGM221]
  of
    true  -> wdgm_main:global_status(S);
    false -> S
  end.

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% -Helper-functions------------------------------------------------------------

%% used by mainfunction and checkpointreached
check_same_supervisionstatus(_, [], _) ->
  true;
check_same_supervisionstatus(S, [L|Ls], C) ->
  SE = lists:keyfind(C, 2, S#state.supervisedentities),
  L#'WdgM_SupervisedEntityMonitor_Tag'.supervision_status == SE#supervisedentity.localstatus andalso
  %% Due to the c implementation the new state will not be updated when there exists a local status that has expired,
  %% Updating or not updateing the state is correct according to the specification,
  %% hence this property can not be checked here.
    case SE#supervisedentity.localstatus of
      'WDGM_LOCAL_STATUS_EXPIRED' -> true;
      _                           -> check_same_supervisionstatus(S, Ls, C+1) andalso
                                       L#'WdgM_SupervisedEntityMonitor_Tag'.logicalsupervision_result == SE#supervisedentity.locallogicalstatus andalso
                                       L#'WdgM_SupervisedEntityMonitor_Tag'.deadlinesupervision_result == SE#supervisedentity.localdeadlinestatus andalso
                                       L#'WdgM_SupervisedEntityMonitor_Tag'.alivesupervision_result == SE#supervisedentity.localalivestatus
    end.


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

reset_logical_table(ModeId) ->
  lists:map(fun ({Init, Finals, Transitions}) ->
                #logical{initCP       = Init,
                         finalCPs     = Finals,
                         cps_in_graph =
                           lists:usort(lists:flatmap(fun ({A,B}) ->
                                                         [A, B]
                                                     end,
                                                     Transitions)),
                         graph        = Transitions,
                         activity     = false}
            end,
            wdgm_config_params:get_internal_graphs() ++
              wdgm_config_params:get_external_graphs(ModeId)).

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


%% -Frequency-------------------------------------------------------------------

-spec weight(S :: eqc_statem:symbolic_state(), Command :: atom()) -> integer().
weight(_S, setmode) -> 1;
weight(_S, checkpointreached) -> 1;
weight(_S, init) -> 1;
weight(_S, _Cmd) -> 1.

%% -Properties------------------------------------------------------------------

prop_wdgm_init() ->
  ?SETUP( fun () -> start(),
                    fun () -> ok end
          end,
          ?FORALL(Cmds, commands(?MODULE),
                  begin
                    eqc_c:restart(),
                    {H,S,Res} = run_commands(?MODULE,Cmds),
                    pretty_commands(?MODULE,Cmds,{H,S,Res},
                                    aggregate(command_names(Cmds),
                                              Res == ok))
                  end)).

start () ->
  wdgm_eqc:start().
