%%% @author  <sebastianwo@MEG-865>
%%% @copyright (C) 2013,
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2013 by  <sebastianwo@MEG-865>

-module(wdgm_post).

-include_lib("eqc/include/eqc.hrl").

-include("wdgm_config.hrl").

-compile(export_all).

%%% -WdgM_Init------------------------------------------------------------------

init_post(S, Args=[{_, Is_Null}], Ret) ->
  InitialMode = S#state.originalCfg#wdgm.tst_cfg1#tst_cfg1.initial_mode_id,
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  _OffModeEnabled = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.off_mode_enabled,
  ((wdgm_helper:check_supervisionstatus(eqc_c:value_of('WdgM_SupervisedEntityMonitorTable')) andalso %% [WDGM268], [WDGM269:446
    eqc_c:value_of('WdgM_GlobalStatus') == 'WDGM_GLOBAL_STATUS_OK' %% [WDGM285]
    andalso

  %%    check_deadlinetimestamps() andalso %% [WDGM298]
  %%    check_logicalactivityflag() andalso %% [WDGM296]
  %%    check_all_global_and_statics() andalso %% [WDGM018]
  %%    eqc_c:value_of('SeIdLocalStatusExpiredFirst') == 0 %% [WDGM350]
  %%    andalso

      eqc_c:value_of('WdgM_CurrentMode') == InitialMode) %% [WDGM135]
       orelse
         (DevErrorDetect
          andalso
            (Is_Null %% [WDGM255]


             %%          orelse
             %%          not is_allowed_config() orelse %% [WDGM010]
             %%          (not OffModeEnabled andalso is_disabled_watchdogs()) %% [WDGM030]
            )))
  andalso eqc_c:value_of('WdgM_GlobalStatus') == (wdgm_next:init_next(S,Ret,Args))#state.globalstatus .

%%% -WdgM_GetMode---------------------------------------------------------------

getmode_post(S, [Is_Null], {R, Mode}) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  case R of
    0 -> Mode == S#state.currentMode; %% [WDGM170]
    1 -> DevErrorDetect andalso (Is_Null orelse %% [WDGM254]
                                 not S#state.initialized) %% [WDGM253]
  end.

%%% -WdgM_SetMode---------------------------------------------------------------

setmode_post(S, [ModeId, Cid], Ret) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  (S#state.globalstatus /= 'WDGM_GLOBAL_STATUS_OK' andalso
   S#state.globalstatus /= 'WDGM_GLOBAL_STATUS_FAILED' andalso
   S#state.currentMode == eqc_c:value_of('WdgM_CurrentMode')) %% [WDGM316], [WDGM145]
    orelse
      ((S#state.globalstatus == eqc_c:value_of('WdgM_GlobalStatus') orelse  %% [WDGM317]
        S#state.globalstatus == undefined) andalso %% if not initialized everything under will failed
       %%    S#state.expiredsupervisioncycles == eqc_c:value_of(expiredsupervisioncycles) andalso %% [WDGM317]

       case Ret of
         0 ->
           wdgm_helper:check_next_supervisionstatus(S, eqc_c:value_of('WdgM_SupervisedEntityMonitorTable'), 0); %% [WDGM207], [WDGM291], [WDGM209], [WDGM182], [WDGM316]
         1 ->
           DevErrorDetect
             andalso
               (wdgm_helper:not_within_allowed_range(ModeId) orelse %% [WDGM020]
                not S#state.initialized orelse %% [WDGM021]
                %% orelse (not OffModeEnabled andalso is_disabled_watchdogs()) orelse %% [WDGM031]
                lists:member(Cid, S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.caller_ids)) %% [WDGM245]
       end).
%% [WDGM186], [WDGM142]


%%% -WdgM_DeInit----------------------------------------------------------------

%% [WDGM154] should check something with WdgM_SetMode
deinit_post(S, _Args, _Ret) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  case S#state.globalstatus of
    'WDGM_GLOBAL_STATUS_OK' -> eqc_c:value_of('WdgM_GlobalStatus') ==
                                  'WDGM_GLOBAL_STATUS_DEACTIVATED'; %% [WDGM286]
    undefined               -> (DevErrorDetect andalso not S#state.initialized); %% [WDGM288]
    Status                  -> eqc_c:value_of('WdgM_GlobalStatus') == Status %% lack of wdgm286?
  end.

%%% -WdgM_CheckpointReached-----------------------------------------------------

checkpointreached_post(S, Args=[SEid, CPId], Ret) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  MonitorTable = eqc_c:value_of('WdgM_SupervisedEntityMonitorTable'),
  case Ret of
    1 -> DevErrorDetect andalso
           checkpoint_postcondition(S, Args) andalso %% [WDGM278], [WDGM279], [WDGM284], [WDGM319]
           (S#state.supervisedentities == undefined orelse
            S#state.supervisedentities == [] orelse
            wdgm_helper:check_same_supervisionstatus(S, MonitorTable, 0));
    0 -> NextS = wdgm_next:checkpointreached_next(S, 0, [SEid, CPId]),
         wdgm_helper:check_same_supervisionstatus(NextS, MonitorTable, 0) %% [WDGM322], [WDGM323]
  end.

checkpoint_postcondition(S, [SeID, CPId]) ->
  not S#state.initialized orelse %% [WDGM279]
    not lists:member(CPId, wdgm_config_params:get_CPs_of_SE(SeID)) orelse %% [WDGM284]
    not wdgm_config_params:is_activated_SE_in_mode(S#state.currentMode, SeID). %% [WDGM319] ([WDGM278])

%%% -WdgM_UpdateAliveCounter----------------------------------------------------
%% Deprecated

%%% -WdgM_GetLocalStatus--------------------------------------------------------

getlocalstatus_post(S, [SEid, Is_Null], {Ret, Status}) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  case Ret of
    0 ->   SE = lists:keyfind(SEid, 2, S#state.supervisedentities),
           (S#state.expiredSEid /= undefined orelse
           Status == SE#supervisedentity.localstatus); %% [WDGM171].
    1 -> DevErrorDetect andalso (not S#state.initialized orelse %% [WDGM173]
                                 Is_Null orelse %% [WDGM257]
                                 lists:keyfind(SEid, 2, S#state.supervisedentities) == false) %% [WDGM172]
  end.


%%% -WdgM_GetGlobalStatus-------------------------------------------------------

getglobalstatus_post(S, [Is_Null], Ret) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  case Ret of
    {0, R} -> R == S#state.globalstatus;
    {1, _} -> DevErrorDetect andalso (Is_Null orelse %% [WDGM344], [WDGM258]
                                      not S#state.initialized) %% [WDGM176]
  end.


%%% -WdgM_PerformReset----------------------------------------------------------

performreset_post(S, _Args, _Ret) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect,
  DevErrorDetect orelse S#state.initialized. %% [WDGM270]
%% [WDGM232] går inte att göra i dagsläget pga avsaknad av WDGIF
%% [WDGM233] ??? global status not to be considered anymore


%%% -WdgM_GetFirstExpiredSEID---------------------------------------------------

getfirstexpiredseid_post(S, [Is_Null], Ret) ->
  DevErrorDetect = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.defensive_behavior,
  case Ret of
    {0, SEid} -> SEid == S#state.expiredSEid; %% [WDGM349]
    {1, SEid} -> (S#state.expiredSEid == undefined andalso
                  SEid == 0) %% [WDGM349]
                   orelse
                     (DevErrorDetect andalso Is_Null) %% [WDGM347]
  end.


%%% -WdgM_MainFunction----------------------------------------------------------

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
  NextS = wdgm_next:mainfunction_next(S, 0, 0),
  MonitorTable = eqc_c:value_of('WdgM_SupervisedEntityMonitorTable'),

  Behaviour =
  NextS#state.globalstatus == GlobalStatus andalso %% [WDGM214], [WDGM326]
  ((GlobalStatus == 'WDGM_GLOBAL_STATUS_EXPIRED' andalso
      eqc_c:value_of('SeIdLocalStatusExpiredFirst') /= 0 andalso
      NextS#state.expiredSEid /= undefined) %% [WDGM351]
     orelse
       (S#state.initialized andalso
         wdgm_helper:check_same_supervisionstatus(NextS, MonitorTable, 0))), %% [WDGM325]

  case DefensiveBehaviour of
    true ->
      case S#state.initialized of
        true -> Behaviour;
        false ->
          (S#state.globalstatus == GlobalStatus orelse (S#state.globalstatus == undefined
            andalso GlobalStatus == 'WDGM_GLOBAL_STATUS_OK')) andalso
            (S#state.supervisedentities == undefined orelse
            wdgm_helper:check_same_supervisionstatus(S, MonitorTable, 0))
      end; %% [WDGM039]
    false -> Behaviour
  end.
