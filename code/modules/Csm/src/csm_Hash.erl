-module(csm_Hash).
-include("inc/csm.hrl").
-include("inc/rte_Csm.hrl").
-include("inc/csm_Cfg.hrl").
-include("inc/csm_Cbk.hrl").
-include("inc/dem.hrl").
-include("inc/schM_Csm.hrl").
-include("inc/memMap.hrl").
%-include("inc/det.hrl"). % OPTIONAL
-include("../Cry/inc/cry_Hash.hrl").

csm_HashStart(CfgId) when IsStarted == true ->
  csm_E_NOT_OK;
csm_HashStart(CfgId) when IsActive == false ->
  IsStarted = true,
  IsActive = true,
  csm_E_OK;
csm_HashStart(CfgId) ->
  csm_E_BUSY.
