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

get_container(Container, Which) ->
  case Which of
    'Params' ->
      {container, _Name, _XmlRef, _Hash, _Type, Params, _Children, _Na, _Cont} = Container,
      Params;
    'Children' ->
      {container, _Name, _XmlRef, _Hash, _Type, _Params, Children, _Na, _Cont} = Container,
      Children;
    _ -> ok
  end.



get_supervised_entities() ->
  [{car_xml:get_value("WdgMSupervisedEntityId", Cg), Name}
   || Cg = {container, Name, _XmlRef, _Hash, _Type, _Params, _Children, _Na, _Cont}
	<- car_xml:get_containers_by_def("WdgMSupervisedEntity", ?CFG)].

get_supervised_entity(Id) ->
  {_, N} = lists:keyfind(Id, 1, get_supervised_entities()),
  car_xml:get_container(N, car_xml:get_containers_by_def(
			     "WdgMSupervisedEntity", ?CFG)).

get_deadline_supervision(ModeId) ->
  car_xml:get_containers_by_def("WdgMDeadlineSupervision", wdgm_config_params:get_container(wdgm_config_params:get_mode(ModeId), 'Children')).

get_alive_supervision(ModeId) ->
  car_xml:get_containers_by_def("WdgMAliveSupervision", wdgm_config_params:get_container(wdgm_config_params:get_mode(ModeId), 'Children')).

get_externallogical_supervision(ModeId) ->
  car_xml:get_containers_by_def("WdgMExternalLogicalSupervision", wdgm_config_params:get_container(wdgm_config_params:get_mode(ModeId), 'Children')).

get_SE_id(CheckpointRef) ->
  car_xml:get_value("WdgMSupervisedEntityId", car_xml:parent(car_xml:get_container(CheckpointRef, ?CFG), ?CFG)).

get_checkpoint_id(CheckpointRef) ->
  car_xml:get_value("WdgMCheckpointId", car_xml:get_container(CheckpointRef, ?CFG)).

get_CPs_of_SE(SeID) ->
  [car_xml:get_value("WdgMCheckpointId", X) || X <- car_xml:get_containers_by_def("WdgMCheckpoint", wdgm_config_params:get_supervised_entity(SeID))].

get_DS_startcheckpoints_for_mode(ModeId) ->
  [car_xml:get_value("WdgMDeadlineStartRef", X) || X <- get_deadline_supervision(ModeId)].

get_DS_stopcheckpoints_for_mode(ModeId) ->
  [car_xml:get_value("WdgMDeadlineStopRef", X) || X <- get_deadline_supervision(ModeId)].

get_AS_checkpoints_for_mode(ModeId) ->
    [car_xml:get_value("WdgMAliveSupervisionCheckpointRef", X) || X <- get_alive_supervision(ModeId)].

get_ELS_initcheckpoints_for_mode(ModeId) ->
  [car_xml:get_value("WdgMExternalCheckpointInitialRef", X) || X <- get_externallogical_supervision(ModeId)].

get_ELS_finalcheckpoints_for_mode(ModeId) ->
  [car_xml:get_value("WdgMExternalCheckpointFinalRef", X) || X <- get_externallogical_supervision(ModeId)].

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
  lists:member(SeID, lists:map(fun get_SE_id/1, get_AS_checkpoints_for_mode(ModeId))).

is_activated_SE_in_DS(ModeId, SeID) ->
  lists:member(SeID, lists:map(fun get_SE_id/1, get_DS_startcheckpoints_for_mode(ModeId)
			       ++get_DS_stopcheckpoints_for_mode(ModeId))).

is_activated_SE_in_ELS(ModeId, SeID) ->
  lists:member(SeID, lists:map(fun get_SE_id/1, get_ELS_initcheckpoints_for_mode(ModeId)
			       ++get_ELS_finalcheckpoints_for_mode(ModeId))).
