-module(csm).
-include("inc/dem.hrl").

csm_Init() when IsInit == true ->
  csm_E_INIT_FAILED;
csm_Init() ->
  IsInit = true,
  ok.

csm_GetVersionInfo(VersionInfo) -> %pre compile time configurable CSM_VERSION_INFO_API : on/off
  {moduleid, vendorid, vendorversionnumber}.

csm_Interruption() when IsInterrupted == true ->
  ok;
csm_Interruption() ->
  IsInterrupted = true,
  do_some_form_of_interruption,
  ok.
