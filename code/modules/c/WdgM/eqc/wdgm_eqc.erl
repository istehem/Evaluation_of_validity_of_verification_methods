-module(wdgm_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-define(C_CODE, wdgm_wrapper).

%% -Eqc start-------------------------------------------------------------------

start() ->
  eqc_c:start(?C_CODE,
	      [{c_src, "wdgm_wrapper.c"},
	       {exclude_functions,[]},
	       {additional_files, ["../out/WdgM_Pbcfg.o","../out/WdgM_Lcfg.o",
				   "../out/Dem.o","../out/Det.o","../out/Mcu.o",
				   "../out/Rte.o","../out/SchM_WdgM.o",
				   "../out/WdgIf.o", "../out/WdgM.o"]}]) .
