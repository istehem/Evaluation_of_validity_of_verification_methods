-module(csm).
-include("inc/dem.hrl").

-record(env, {isInit=false, isInterrupted=false}).

csm_Init() when #env.isInit == true ->
  csm_E_INIT_FAILED;
csm_Init() ->
  #env{isInit = true},
  ok.

csm_GetVersionInfo(VersionInfo) -> %pre compile time configurable CSM_VERSION_INFO_API : on/off
  {moduleid, vendorid, vendorversionnumber}.

csm_Interruption() when #env.isInterrupted == true ->
  ok;
csm_Interruption() ->
  #env{isInterrupted = true},
  do_some_form_of_interruption,
  ok.
