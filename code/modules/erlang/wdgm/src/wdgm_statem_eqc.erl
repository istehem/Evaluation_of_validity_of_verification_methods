-module(wdgm_statem_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

-include("wdgm_config.hrl").
-define(C_CODE, wdgm_wrapper).
-include_lib("../ebin/wdgm_wrapper.hrl").


initial_state() ->
  Rs = wdgm_xml:start(),
  {_, R} = (hd(Rs)), %% why do we get a list of records?
  #state{originalCfg=R}.

next_state(S, R, {call, _M, F, A}) ->
  apply(wdgm_next, list_to_atom(atom_to_list(F)++"_next"), [S, R, A]).

precondition(S, {call, _M, F, _A}) ->
  apply(wdgm_pre, list_to_atom(atom_to_list(F)++"_pre"), [S]).

postcondition(S, {call, _M, F, A}, R) ->
  apply(wdgm_post, list_to_atom(atom_to_list(F)++"_post"), [S, A, R]).

command(S) ->
  frequency([{weight(S, init), wdgm_command:init_command(S)},
             {weight(S, getmode), wdgm_command:getmode_command(S)},
             {weight(S, setmode), wdgm_command:setmode_command(S)},
             {weight(S, deinit), wdgm_command:deinit_command(S)},
             {weight(S, checkpointreached), wdgm_command:checkpointreached_command(S)},
             {weight(S, getlocalstatus), wdgm_command:getlocalstatus_command(S)},
             {weight(S, getglobalstatus), wdgm_command:getglobalstatus_command(S)},
             {weight(S, performreset), wdgm_command:performreset_command(S)},
             {weight(S, getfirstexpiredseid), wdgm_command:getfirstexpiredseid_command(S)},
             {weight(S, mainfunction), wdgm_command:mainfunction_command(S)}]).

%%% -WdgM_Init------------------------------------------------------------------

init({Ptr, _}) ->
  ?C_CODE:'WdgM_Init'(Ptr).


%%% -WdgM_GetMode---------------------------------------------------------------

getmode(Is_Null) ->
  Mp =
    case Is_Null of
      true  -> {ptr, "uint8", 0};
      false -> eqc_c:alloc("uint8")
    end,
  R = ?C_CODE:'WdgM_GetMode'(Mp),
  case Is_Null of
    true  -> {R, null};
    false -> {R, eqc_c:deref(Mp)}
  end.


%%% -WdgM_SetMode---------------------------------------------------------------

setmode(Mode, CallerId) ->
  ?C_CODE:'WdgM_SetMode'(Mode, CallerId).

%%% -WdgM_DeInit----------------------------------------------------------------

deinit() ->
  ?C_CODE:'WdgM_DeInit'().


%%% -WdgM_CheckpointReached-----------------------------------------------------

checkpointreached(SeID, CPId) ->
  ?C_CODE:'WdgM_CheckpointReached'(SeID, CPId).

%%% -WdgM_UpdateAliveCounter----------------------------------------------------
%% Deprecated

%%% -WdgM_GetLocalStatus--------------------------------------------------------

getlocalstatus(SEid, Is_Null) ->
  Sp =
    case Is_Null of
      true  -> {ptr, "WdgM_LocalStatusType", 0};
      false -> eqc_c:alloc("WdgM_LocalStatusType")
    end,
  R  = ?C_CODE:'WdgM_GetLocalStatus'(SEid, Sp),
  case Is_Null of
    true  -> {R, null};
    false -> {R, eqc_c:deref(Sp)}
  end.

%%% -WdgM_GetGlobalStatus-------------------------------------------------------

getglobalstatus(Is_Null) ->
  Sp =
    case Is_Null of
      true  -> {ptr, "WdgM_GlobalStatusType", 0};
      false -> eqc_c:alloc("WdgM_GlobalStatusType")
    end,
  R = ?C_CODE:'WdgM_GetGlobalStatus'(Sp),
  case Is_Null of
    true  -> {R, null};
    false -> {R, eqc_c:deref(Sp)}
  end.


%%% -WdgM_PerformReset----------------------------------------------------------

performreset() ->
  ?C_CODE:'WdgM_PerformReset'().


%%% -WdgM_GetFirstExpiredSEID---------------------------------------------------

getfirstexpiredseid(Is_Null) ->
  Sp =
    case Is_Null of
      true -> {ptr, "WdgM_SupervisedEntityIdType", 0};
      false -> eqc_c:alloc("WdgM_SupervisedEntityIdType")
    end,
  R = ?C_CODE:'WdgM_GetFirstExpiredSEID'(Sp),
  case Is_Null of
    true -> {R, null};
    false -> {R, eqc_c:deref(Sp)}
  end.

%%% -WdgM_MainFunction----------------------------------------------------------

mainfunction() ->
  ?C_CODE:'WdgM_MainFunction'().


%%% -Frequency------------------------------------------------------------------

-spec weight(S :: eqc_statem:symbolic_state(), Command :: atom()) -> integer().
weight(_S, setmode) -> 0;
weight(_S, checkpointreached) -> 25;
weight(_S, mainfunction) -> 10;
weight(S, init) -> case S#state.initialized of
                      true -> 0;
                      _    -> 200
                   end;
weight(S,deinit) -> case S#state.initialized of
                      true -> 1;
                      _    -> 0
                    end;
weight(_S,getfirstexpiredseid_pre) -> 0;
weight(_S,getmode)                 -> 0;
weight(_S,getglobalstatus)         -> 0;
weight(_S, _Cmd) -> 1.

%%% -Properties-----------------------------------------------------------------

prop_wdgm_init() ->
  ?SETUP( fun () -> start(),
                    fun () -> ok end
          end,
          ?FORALL(Cmds, commands(?MODULE),
                  begin
                    eqc_c:restart(),
                    {H,S,Res} = run_commands(?MODULE,Cmds),
                    pretty_commands(?MODULE,Cmds,{H,S,Res},
                                    aggregate(collect_res(H,S,Res,Cmds),
                                              Res == ok))
                  end)).

start () ->
  wdgm_eqc:start().

collect_res(_H,_S,_Res,Cmds) ->
  case Cmds of
    []                      -> [{none,0}];
    [{_,_,{_,_,Name,_}}|Xs] -> [{Name,length(Xs)}]
  end.
