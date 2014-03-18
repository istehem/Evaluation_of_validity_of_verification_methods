%%% @author  <sebastianwo@MEG-865>
%%% @copyright (C) 2013,
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2013 by  <sebastianwo@MEG-865>

-module(wdgm_command).

-include_lib("eqc/include/eqc.hrl").
-include("wdgm_config.hrl").

-compile(export_all).


%%% -WdgM_Init------------------------------------------------------------------

init_command(_S) ->
  {call, ?WDGMSTATEM, init, [frequency(
                               [{20, return({eqc_c:address_of(?CONFIG_FILE), false})},
                                {1, return({{ptr, "WdgM_ConfigType", 0}, true})}])]}.

%%% -WdgM_GetMode---------------------------------------------------------------

getmode_command(_S) ->
  {call, ?WDGMSTATEM, getmode, [frequency([{20, return(false)},
                                           {1, return(true)}])]}.

%%% -WdgM_SetMode---------------------------------------------------------------

setmode_command(S) ->
  CallerIds = S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.caller_ids,
  CIds =
    case
      CallerIds == [] orelse
      CallerIds == undefined
    of
      true  -> [65535];
      false -> CallerIds
    end,
  {call, ?WDGMSTATEM, setmode, [frequency(
                                  [{30, oneof([Id || {Id, _} <- wdgm_config_params:get_modes()])},
                                   {1, return(255)}]),
                                frequency(
                                  [{30, oneof(CIds)},
                                   {1, return(65535)}])]}.

%%% -WdgM_DeInit----------------------------------------------------------------

deinit_command(_S) ->
  {call, ?WDGMSTATEM, deinit, []}.


%%% -WdgM_CheckpointReached-----------------------------------------------------

checkpointreached_command(S) ->
  {call, ?WDGMSTATEM, checkpointreached, checkpoint_gen(S)}.

checkpoint_gen(S) ->
  case wdgm_pre:checkpointreached_pre(S) of
    false -> [false];
    true ->
      SEs = wdgm_config_params:get_supervised_entities(),
      ActivatedSEid   = [SEid
                         || {SEid,_} <- SEs,
                            wdgm_config_params:is_activated_SE_in_mode(S#state.currentMode, SEid)],
      DeactivatedSEid = [SEid
                         || {SEid,_} <- SEs,
                            not lists:member(SEid, ActivatedSEid)],
      ?LET(SEid, frequency([{20, oneof(case ActivatedSEid of
                                          [] -> [65535];
                                          Xs -> Xs
                                       end)},   %% either choose one of the valid SEid
                                                % (This demands there is at least one ActivatedSEid)
                            {1, oneof(DeactivatedSEid++[65535])}]),
           return(case SEid of
             65535 -> [65535, 65535]; %% if the phony, also choose a phony CPid
             _   ->
               LCPs = ?LSPRIO(S, SEid),
               wdgm_checkpointreached:choose_SE_and_CP(S, LCPs)
           end))
  end.

%% prioritize logical supervision
prioritize_ls(S, SEid) ->
    lists:flatten(wdgm_checkpointreached:get_args_given_LS(S#state.logicalTable, SEid)).

%% don't prioritize logical supervision
dont_prioritize_ls(_S, SEid) ->
    lists:map(fun (CP) -> {found, SEid, CP} end, wdgm_config_params:get_CPs_of_SE(SEid)).


%%% -WdgM_UpdateAliveCounter----------------------------------------------------
%% Deprecated

%%% -WdgM_GetLocalStatus--------------------------------------------------------

getlocalstatus_command(S) ->
  SErecords =
    case
      S#state.supervisedentities == undefined orelse
      S#state.supervisedentities == []
    of
      true  -> [#supervisedentity{seid=65535}];
      false -> S#state.supervisedentities
    end,
  SEids = [SE#supervisedentity.seid || SE <- SErecords],
  {call, ?WDGMSTATEM, getlocalstatus, [frequency([{20, oneof(SEids)},
                                                  {1, return(65535)}]),
                                       frequency([{20, return(false)},
                                                  {1, return(true)}])]}.

%%% -WdgM_GetGlobalStatus-------------------------------------------------------

getglobalstatus_command(_S) ->
  {call, ?WDGMSTATEM, getglobalstatus, [frequency([{20, return(false)},
                                                   {1, return(true)}])]}.

%%% -WdgM_PerformReset----------------------------------------------------------

performreset_command (_S) ->
  {call, ?WDGMSTATEM, performreset, []}.

%%% -WdgM_GetFirstExpiredSEID---------------------------------------------------

getfirstexpiredseid_command(_S) ->
  {call, ?WDGMSTATEM, getfirstexpiredseid, [frequency([{20, return(false)},
                                                       {1, return(true)}])]}.


%%% -WdgM_MainFunction----------------------------------------------------------

mainfunction_command(_S) ->
  {call, ?WDGMSTATEM, mainfunction, []}.

%%% -WdgM_GetVersionInfo--------------------------------------------------------

getversioninfo_command(_S) ->
  {call, ?WDGMSTATEM, getversioninfo, [frequency([{20, return(false)},
                                                  {1, return(true)}])]}.
