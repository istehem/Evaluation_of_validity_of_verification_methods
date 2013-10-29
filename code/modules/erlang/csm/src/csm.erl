-module(csm).
-include("inc/dem.hrl").

-record(env, {isInit=false, isInterrupted=false}).

% Should initialize all variables used by the Csm module
% if fail then it should report CSM_E_INIT_FAILED to DEM.
% This function shall only be called from within the ECU state manager.
csm_Init() when #env.isInit == true ->
  csm_E_INIT_FAILED;
csm_Init() ->
  #env{isInit = true},
  ok.

% Exists only if configurable CsmVersionInfoApi is true.
csm_GetVersionInfo(VersionInfo) ->
  {moduleid, vendorid, vendorversionnumber}.

% Exists only if configurable CsmUseSyncJobProcessing is false
% and configurable CsmUseInterruption is true.
csm_Interruption() when #env.isInterrupted == true ->
  ok;
csm_Interruption() ->
  #env{isInterrupted = true},
  do_some_form_of_interruption,
  ok.
