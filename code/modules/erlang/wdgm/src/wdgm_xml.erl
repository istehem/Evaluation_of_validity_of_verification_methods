%%% @author  <>
%%% @copyright (C) 2013,
%%% @doc
%%%
%%% @end
%%% Created : 31 Oct 2013 by  <>

-module(wdgm_xml).

-include("wdgm_config.hrl").
-include_lib("car/include/car_xml.hrl").

-import(car_xml, [get_container/2, get_containers_by_def/2, get_value/2, get_values/2]).

-compile(export_all).
%-export([config_file/0, config/1, start/0, get_modes/0]).

config_file() ->
        wdgm_eqc:getPath(["c","WdgM","cfg","examples"]) ++ "WdgM_VID41_ExampleConfiguration_001_cfg1.arxml".

start() ->
  Cfg = config_file(),
  car:start(Cfg, [wdgm]),
  car_config:config().

config(Cfg) ->
  WdgM = get_container("_/WdgM", Cfg),
  General = #wdgmgeneral{ defensive_behavior = get_value("_/WdgMGeneral/WdgMDefensiveBehavior", WdgM),
			  dem_stopped_supervision_report = get_value("_/WdgMGeneral/WdgMDemStoppedSupervisionReport", WdgM),
			  dev_error_detect   = get_value("_/WdgMGeneral/WdgMDevErrorDetect", WdgM),
			  immediate_reset    = get_value("_/WdgMGeneral/WdgMImmediateReset", WdgM),
			  off_mode_enabled   = get_value("_/WdgMGeneral/WdgMOffModeEnabled", WdgM),
			  supervision_cycle  = get_value("_/WdgMGeneral/WdgMSupervisionCycle", WdgM),
			  version_check_foreign_module   = get_value("_/WdgMGeneral/WdgMVersionCheckForeignModule", WdgM),
			  version_info_api   = get_value("_/WdgMGeneral/WdgMVersionInfoApi", WdgM),
			  caller_ids         = get_values("_/WdgMGeneral/WdgMCallerIds/WdgMCallerId", WdgM) },
  Mode = get_value("_/Tst_Cfg1/WdgMInitialMode", WdgM),
  Test = #tst_cfg1{
	    initial_mode_id = get_value(Mode ++ "/WdgMModeId", Cfg)
	   },
%  Names = [N || {container, N, _XmlRef, _Hash, _Type, _Params, _Children, _Na, _Cont} <- car_xml:remove_children(car_xml:get_containers_by_def("WdgMMode", Cfg))],
  #wdgm{wdgmgeneral=General, tst_cfg1=Test}.