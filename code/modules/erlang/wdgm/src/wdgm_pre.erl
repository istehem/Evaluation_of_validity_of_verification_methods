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
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect
    orelse
    S#state.initialized. %% [WDGM253]


%%% -WdgM_SetMode---------------------------------------------------------------
setmode_pre(S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect
   orelse
    S#state.initialized.

%%% -WdgM_DeInit----------------------------------------------------------------
deinit_pre(S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect
    orelse
    S#state.initialized.


%%% -WdgM_CheckpointReached-----------------------------------------------------
checkpointreached_pre(S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect
    orelse
    S#state.initialized.

%%% -WdgM_UpdateAliveCounter----------------------------------------------------
%% Deprecated

%%% -WdgM_GetLocalStatus--------------------------------------------------------
getlocalstatus_pre(S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect
    orelse
    S#state.initialized.

%%% -WdgM_GetGlobalStatus-------------------------------------------------------
getglobalstatus_pre(S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect
    orelse
    S#state.initialized.

%%% -WdgM_PerformReset----------------------------------------------------------

performreset_pre(S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.dev_error_detect
    orelse
    S#state.initialized.

%%% -WdgM_GetFirstExpiredSEID---------------------------------------------------

getfirstexpiredseid_pre(_S) ->
  true. %% [WDGM348]

%%% -WdgM_MainFunction----------------------------------------------------------

mainfunction_pre(S) ->
  S#state.originalCfg#wdgm.wdgmgeneral#wdgmgeneral.defensive_behavior orelse
    S#state.initialized. %% [WDGM039]
