-module(wdgm_config_params).

-compile(export_all).

-define(CFG, car_xml:file(wdgm_xml:config_file())).

get_modes() ->
  [{car_xml:get_value("WdgMModeId", Cg), Name}
   || Cg = {container, Name, _XmlRef, _Hash, _Type, _Params, _Children, _Na, _Cont}
	<- car_xml:get_containers_by_def("WdgMMode", ?CFG)].

get_mode(Id) ->
  {_, N} = lists:keyfind(Id, 1, get_modes()),
  car_xml:get_container(N, car_xml:get_containers_by_def(
			     "WdgMMode", ?CFG)).


get_supervised_entities() ->
  [{car_xml:get_value("WdgMSupervisedEntityId", Cg), element(2, Cg)}
   || Cg <- car_xml:get_containers_by_def("WdgMSupervisedEntity", ?CFG)].

get_supervised_entity(Id) ->
  {_, N} = lists:keyfind(Id, 1, get_supervised_entities()),
  car_xml:get_container(N, car_xml:get_containers_by_def(
			     "WdgMSupervisedEntity", ?CFG)).

get_supervision_function(ModeId, Which) ->
  car_xml:get_containers_by_def(Which, element(7, get_mode(ModeId))).
get_deadline_supervision(ModeId) ->
  get_supervision_function(ModeId, "WdgMDeadlineSupervision").
get_alive_supervision(ModeId) ->
  get_supervision_function(ModeId, "WdgMAliveSupervision").
get_externallogical_supervision(ModeId) ->
  get_supervision_function(ModeId, "WdgMExternalLogicalSupervision").


get_SE_id(CheckpointRef) ->
  car_xml:get_value("WdgMSupervisedEntityId", car_xml:parent(car_xml:get_container(CheckpointRef, ?CFG), ?CFG)).

get_checkpoint_id(CheckpointRef) ->
  car_xml:get_value("WdgMCheckpointId", car_xml:get_container(CheckpointRef, ?CFG)).

get_CPs_of_SE(SeID) ->
  [car_xml:get_value("WdgMCheckpointId", X) || X <- car_xml:get_containers_by_def("WdgMCheckpoint", wdgm_config_params:get_supervised_entity(SeID))].


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
      Ls = get_externallogical_supervision(ModeId)
  end,
  [car_xml:get_value(WS, X) || X <- Ls].


is_supervised_entity_for_checkpoint(SeID, CPId) ->
  lists:member(CPId,
	       lists:flatten([car_xml:get_values("WdgMCheckpointId", Y)
			      || Y <- [car_xml:get_containers_by_def("WdgMCheckpoint", X)
				       || X <- car_xml:get_containers_by_def(
						 "WdgMSupervisedEntity",
						 ?CFG),
					  car_xml:get_value("WdgMSupervisedEntityId", X) == SeID]])).

is_activated_supervised_entity_in_mode(ModeID, SeID) ->
  is_activated_SE_in_AS(ModeID,SeID) orelse
    is_activated_SE_in_DS(ModeID,SeID) orelse
    is_activated_SE_in_ELS(ModeID,SeID).


is_activated_SE_in_AS(ModeId, SeID) ->
  lists:member(SeID, lists:map(fun get_SE_id/1, get_checkpoints_for_mode(ModeId, 'AS'))).
is_activated_SE_in_DS(ModeId, SeID) ->
  lists:member(SeID, lists:map(fun get_SE_id/1, get_checkpoints_for_mode(ModeId, 'DSstart')
			                      ++get_checkpoints_for_mode(ModeId, 'DSstop'))).
is_activated_SE_in_ELS(ModeId, SeID) ->
  lists:member(SeID, lists:map(fun get_SE_id/1, get_checkpoints_for_mode(ModeId, 'ELSinit')
			                      ++get_checkpoints_for_mode(ModeId, 'ELSfinal'))).
