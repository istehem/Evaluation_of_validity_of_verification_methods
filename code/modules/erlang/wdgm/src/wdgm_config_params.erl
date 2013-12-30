-module(wdgm_config_params).

-compile([export_all, debug_info]).

-define(CFG, car_xml:file(wdgm_xml:config_file())).

%%% Get the existing modes
%%% Return: [{ModeId, ModeName}]
get_modes() ->
  [{car_xml:get_value("WdgMModeId", Cg), element(2, Cg)}
   || Cg <- car_xml:get_containers_by_def("WdgMMode", ?CFG)].

%%% Get a specific mode
%%% Return: the container for a mode
get_mode(Id) ->
  {_, N} = lists:keyfind(Id, 1, get_modes()),
  car_xml:get_container(N, car_xml:get_containers_by_def(
			     "WdgMMode", ?CFG)).

%%% Get the existing supervised entities
%%% Return: [{SEid, SEName}]
get_supervised_entities() ->
  [{car_xml:get_value("WdgMSupervisedEntityId", Cg), element(2, Cg)}
   || Cg <- car_xml:get_containers_by_def("WdgMSupervisedEntity", ?CFG)].

%%% Get a specific supervised entity
%%% Return: the container for the SE
get_supervised_entity(Id) ->
  case lists:keyfind(Id, 1, get_supervised_entities()) of
    {_, N} ->
      car_xml:get_container(N, car_xml:get_containers_by_def(
                                 "WdgMSupervisedEntity", ?CFG));
    false -> []
  end.

%%% Get supervised entities from Local supervision params
%%% Return: [SEId]
get_SEs_from_LS(ModeId) ->
  [car_xml:get_value(
     "WdgMSupervisedEntityId",
     car_xml:get_container(X,
                           car_xml:file(wdgm_xml:config_file())))
   || X <- wdgm_config_params:get_checkpoints_for_mode(ModeId, 'LSP')].

%%% Get supervision functions for a specific mode
%%% Return: container for the def 'Which' in a Mode
get_supervision_function(ModeId, Which) ->
  car_xml:get_containers_by_def(Which, element(7, get_mode(ModeId))).
get_deadline_supervision(ModeId) ->
  get_supervision_function(ModeId, "WdgMDeadlineSupervision").
get_alive_supervision(ModeId) ->
  get_supervision_function(ModeId, "WdgMAliveSupervision").
get_externallogical_supervision(ModeId) ->
  get_supervision_function(ModeId, "WdgMExternalLogicalSupervision").
get_localstatusparams(ModeId) ->
  get_supervision_function(ModeId, "WdgMLocalStatusParams").

%%% get the failed supervision cycle tolerance for a specific SE
%%% Return: FailedTol
get_LSP_failedtolerance(ModeId, SEid) ->
  hd([car_xml:get_value("WdgMFailedAliveSupervisionRefCycleTol",LSP)
      || LSP <- wdgm_config_params:get_localstatusparams(ModeId),
         car_xml:get_value(
           "WdgMSupervisedEntityId",
           car_xml:get_container(
             car_xml:get_value("WdgMLocalStatusSupervisedEntityRef",
                               LSP),
             car_xml:file(wdgm_xml:config_file())))
           == SEid]).

%%% Given a checkpoint reference, gets the SE id
%%% Return: SEid
get_SE_id(CheckpointRef) ->
  car_xml:get_value("WdgMSupervisedEntityId", car_xml:parent(car_xml:get_container(CheckpointRef, ?CFG), ?CFG)).

%%% Given a checkpoint reference, gets the checkpoint id
%%% Return: CPid
get_checkpoint_id(CheckpointRef) ->
  car_xml:get_value("WdgMCheckpointId", car_xml:get_container(CheckpointRef, ?CFG)).
%%% Get the checkpoints of a given SE
%%% Return: [CPid]
get_CPs_of_SE(SeID) ->
  SE = wdgm_config_params:get_supervised_entity(SeID),
  [car_xml:get_value("WdgMCheckpointId", X)
   || X <- car_xml:get_containers_by_def("WdgMCheckpoint", SE),
      SE /= []].

%%% Get the parameters needed for deadline supervision
%%% Return: {DSStartRef, DSStopRef, DeadlineMin, DeadlineMax}
get_deadline_params(DS) ->
  {get_checkpoint_id(car_xml:get_value("WdgMDeadlineStartRef", DS)),
   get_checkpoint_id(car_xml:get_value("WdgMDeadlineStopRef", DS)),
   car_xml:get_value("WdgMDeadlineMin", DS),
   car_xml:get_value("WdgMDeadlineMax", DS)}.

%%% Gets all checkpoints for a supervision function
%%% Return: [CPRef]
get_checkpoints_for_mode(ModeId, Which) ->
  case Which of
    'AS' ->
      WS = "WdgMAliveSupervisionCheckpointRef",
      Ls = get_alive_supervision(ModeId);
    'DSstart' ->
      WS = "WdgMDeadlineStartRef",
      Ls = get_deadline_supervision(ModeId);
    'DSstop' ->
      WS = "WdgMDeadlineStopRef",
      Ls = get_deadline_supervision(ModeId);
    'ELSinit' ->
      WS = "WdgMExternalCheckpointInitialRef",
      Ls = get_externallogical_supervision(ModeId);
    'ELSfinal' ->
      WS = "WdgMExternalCheckpointFinalRef",
      Ls = get_externallogical_supervision(ModeId);
    'LSP' ->
      WS = "WdgMLocalStatusSupervisedEntityRef",
      Ls = get_localstatusparams(ModeId)
  end,
  [car_xml:get_value(WS, X) || X <- Ls].

%%% checks if a SE is activated
%%% Return: Boolean
is_activated_SE_in_mode(ModeID, SeID) ->
  case ModeID of
    -1 -> false;
    _  ->
      is_activated_SE_in_AS(ModeID,SeID) orelse
        is_activated_SE_in_DS(ModeID,SeID) orelse
        is_activated_SE_in_ELS(ModeID,SeID)
  end.

%%% Checks if a SE has AS,DS or ELS
is_activated_SE_in_AS(ModeId, SeID) ->
  lists:member(SeID, lists:map(fun get_SE_id/1, get_checkpoints_for_mode(ModeId, 'AS'))).
is_activated_SE_in_DS(ModeId, SeID) ->
  lists:member(SeID, lists:map(fun get_SE_id/1, get_checkpoints_for_mode(ModeId, 'DSstart')
			                      ++get_checkpoints_for_mode(ModeId, 'DSstop'))).
is_activated_SE_in_ELS(ModeId, SeID) ->
  lists:member(SeID, lists:map(fun get_SE_id/1, get_checkpoints_for_mode(ModeId, 'ELSinit')
			                      ++get_checkpoints_for_mode(ModeId, 'ELSfinal'))).


%%%% used by wdgm_Main and helper

%%% get the alive supervision parameters for a given checkpoint
%%% Return: {SRC, EAI, MinMargin, MaxMargin}
get_AS_for_CP(ModeId, CPid) ->
    [{car_xml:get_value("WdgMSupervisionReferenceCycle", AS),
      car_xml:get_value("WdgMExpectedAliveIndications", AS),
      car_xml:get_value("WdgMMinMargin", AS),
      car_xml:get_value("WdgMMaxMargin", AS)
     }
     || AS <- get_alive_supervision(ModeId),
        CPid == get_checkpoint_id(car_xml:get_value("WdgMAliveSupervisionCheckpointRef", AS))].

%%%% used by wdgm_Init and wdgm_SetMode

get_internal_graph(SEid) ->
  SE = get_supervised_entity(SEid),
  {get_checkpoint_id(car_xml:get_value("WdgMInternalCheckpointInitialRef", SE)),
   [get_checkpoint_id(V) || V <- car_xml:get_values("WdgMInternallCheckpointFinalRef", SE)],
   [{get_checkpoint_id(car_xml:get_value("WdgMInternalTransitionSourceRef", Transition)),
     get_checkpoint_id(car_xml:get_value("WdgMInternalTransitionDestRef", Transition))}
    || Transition <- car_xml:get_containers_by_def("WdgMInternalTransition", SE)]}.

get_internal_graphs() ->
  [get_internal_graph(SEid) || {SEid, _} <- get_supervised_entities()].

get_external_graph(ES) ->
  {get_checkpoint_id(car_xml:get_value("WdgMExternalCheckpointInitialRef", ES)),
   [get_checkpoint_id(V) || V <- car_xml:get_values("WdgMExternalCheckpointFinalRef", ES)],
   [{get_checkpoint_id(car_xml:get_value("WdgMExternalTransitionSourceRef", Transition)),
     get_checkpoint_id(car_xml:get_value("WdgMExternalTransitionDestRef", Transition))}
    || Transition <- car_xml:get_containers_by_def("WdgMExternalTransition", ES)]}.

get_external_graphs(ModeId) ->
  [get_external_graph(ES) || ES <- get_externallogical_supervision(ModeId)].


%%%% used by wdgm_SetMode

get_expired_supervision_cycles(ModeId) ->
  car_xml:get_value("WdgMExpiredSupervisionCycleTol", get_mode(ModeId)).

%%% checks if a mode will disable the watchdog
%%% Return: boolean
will_disable_watchdog(ModeId) ->
  lists:any(fun (Elem) ->
                Elem == 'WDGIF_OFF_MODE'
            end,
            car_xml:get_values("WdgMWatchdogMode",
                               car_xml:get_containers_by_def("WdgMTrigger",
                                                             get_mode(ModeId)))).

%%%% used by checkpointreached

get_SE_of_CP(CPid) ->
  hd([SEid || {SEid, _} <- get_supervised_entities(),
              SECP      <- get_CPs_of_SE(SEid),
              SECP == CPid]).
