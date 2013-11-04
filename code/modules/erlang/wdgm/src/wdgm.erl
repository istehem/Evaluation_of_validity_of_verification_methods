-module(wdgm).
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

getPath(Xs) ->
         filename:join(lists:takewhile(fun (X) -> X /= "modules" end,
                                       filename:split(filename:absname(".")))++["modules"] ++ Xs) ++ "/".

lib_dir() -> getPath(["c","WdgM","out"]).
src_dir() -> getPath(["c","WdgM","eqc"]).

start() ->
        eqc_c:start(wdgm_wrapper,[{c_src,src_dir() ++ "wdgm_wrapper.c"},
                                  {exclude_functions,[]},{additional_files,
                                                          [lib_dir() ++ "WdgM_Pbcfg.o",
                                                           lib_dir() ++ "WdgM_Lcfg.o",
                                                           lib_dir() ++ "Dem.o",
                                                           lib_dir() ++ "Det.o",
                                                           lib_dir() ++ "Mcu.o",
                                                           lib_dir() ++ "Rte.o",
                                                           lib_dir() ++ "SchM_WdgM.o",
                                                           lib_dir() ++ "WdgIf.o",
                                                           lib_dir() ++ "WdgM.o"]}]).

init() -> case eqc_c:running() of
                  true -> wdgm_wrapper:'WdgM_Init'(eqc_c:address_of('Tst_Cfg1'));
                  _    -> c_not_started
          end.

get_mode() ->
        Mp = eqc_c:alloc("uint8"),
        R = wdgm_wrapper:'WdgM_GetMode'(Mp),
        {R,eqc_c:deref(Mp)}.

%% segfault if first parameter other then 2
%% callerId works for oneof {0 .. 2}
set_mode(UI8_mode,UI16_callerId) ->
        wdgm_wrapper:'WdgM_SetMode'(UI8_mode,UI16_callerId).

get_global_status() ->
   Sp = eqc_c:alloc("WdgM_GlobalStatusType"),
   R  = wdgm_wrapper:'WdgM_GetGlobalStatus'(Sp),
   {R,eqc_c:deref(Sp)}.

get_local_status(UI16_SEID) ->
        Sp = eqc_c:alloc("WdgM_LocalStatusType"),
        R  = wdgm_wrapper:'WdgM_GetLocalStatus'(UI16_SEID,Sp),
        {R,eqc_c:deref(Sp)}.
