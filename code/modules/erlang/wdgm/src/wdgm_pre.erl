%%% @author  <sebastianwo@MEG-865>
%%% @copyright (C) 2013,
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2013 by  <sebastianwo@MEG-865>

-module(wdgm_pre).

-include("wdgm_config.hrl").

-compile(export_all).

%%% -WdgM_Init------------------------------------------------------------------

init_pre(S) ->
  S#state.initialized /= true.

%%% -WdgM_GetMode---------------------------------------------------------------

getmode_pre(S) ->
  S#state.initialized %% [WDGM253]
    orelse
    S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect.

%%% -WdgM_SetMode---------------------------------------------------------------
setmode_pre(S) ->
  S#state.initialized
   orelse
    S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect.

%%% -WdgM_DeInit----------------------------------------------------------------
deinit_pre(S) ->
  S#state.initialized
    orelse
      S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect.

%%% -WdgM_CheckpointReached-----------------------------------------------------
checkpointreached_pre(S) ->
  S#state.initialized
    orelse
    S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect.

%%% -WdgM_UpdateAliveCounter----------------------------------------------------
%% Deprecated

%%% -WdgM_GetLocalStatus--------------------------------------------------------
getlocalstatus_pre(S) ->
  S#state.initialized
    orelse
    S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect.

%%% -WdgM_GetGlobalStatus-------------------------------------------------------
getglobalstatus_pre(S) ->
  S#state.initialized
    orelse
    S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect.

%%% -WdgM_PerformReset----------------------------------------------------------

performreset_pre(S) ->
  S#state.initialized
    orelse
    S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect.

%%% -WdgM_GetFirstExpiredSEID---------------------------------------------------

getfirstexpiredseid_pre(_S) ->
  true. %% [WDGM348]

%%% -WdgM_MainFunction----------------------------------------------------------

mainfunction_pre(S) ->
  S#state.initialized %% [WDGM039]
    orelse
    S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.defensive_behavior.

%%% -WdgM_GetVersionInfo----------------------------------------------------------

getversioninfo_pre(S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.version_info_api. %% [WDGM110]
