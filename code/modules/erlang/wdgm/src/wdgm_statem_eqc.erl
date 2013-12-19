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
%% mainfunction(_A) ->
%%   ?C_CODE:'WdgM_MainFunction'().
%% mainfunction(_A,_B) ->
%%   ?C_CODE:'WdgM_MainFunction'().
%% mainfunction(_A,_B,_C) ->
%%   ?C_CODE:'WdgM_MainFunction'().


%%% -Frequency------------------------------------------------------------------

-spec weight(S :: eqc_statem:symbolic_state(), Command :: atom()) -> integer().
weight(_S, setmode)                -> 2;
weight(S,  checkpointreached) ->
  case S#state.globalstatus of
    'WDGM_GLOBAL_STATUS_FAILED'    -> 75;
    _                              -> 25
  end;
weight(S,  mainfunction) ->
  case S of
    _                              -> 10
  end;
weight(S,  init) ->
  case S#state.initialized of
    true                           -> 1;
    _                              -> 200
  end;
weight(S,  deinit) ->
  case S#state.initialized of
    true                           -> 2;
    _                              -> 1
  end;
weight(_S, getfirstexpiredseid)    -> 2;
weight(_S, getmode)                -> 2;
weight(_S, getlocalstatus)         -> 2;
weight(_S, getglobalstatus)        -> 2;
weight(_S, performreset)           -> 2;
weight(_S, _Cmd)                   -> 1.

%%% -Properties-----------------------------------------------------------------

prop_wdgm_init() ->
  ?SETUP( fun () -> start(),
                    fun () -> ok end
          end,
          ?FORALL(Cmds, non_empty(commands(?MODULE)), %% Use eqc_gen:vector/2 in combination ?LET
                                                      %% for more commands
                  begin
                    rename_bullseye_cov_file(),
                    eqc_c:restart(),
                    {H,S,Res} = run_commands(?MODULE,Cmds),
                    pretty_commands(?MODULE,Cmds,{H,S,Res},
                                    aggregate(collect_globalstatus(H,S,Res,Cmds),
                                              aggregate(collect_init(H,S,Res,Cmds),
                                                        aggregate(collect_length(H,S,Res,Cmds),
                                                                  aggregate(collect_given_cmd_length(H,S,Res,Cmds),
                                                        Res == ok)))))
                  end)).

rename_bullseye_cov_file() ->
 Path = wdgm_eqc:getPath(["..","coverage"]),
 file:rename(Path ++ "test.cov",
             Path ++ (fun({X,Y,Z}) -> integer_to_list(X) ++ integer_to_list(Y) ++ integer_to_list(Z) ++ ".cov" end) (now())).

start () ->
  wdgm_eqc:start().

collect_globalstatus(H, S, _Res, _Cmds) ->
  lists:nthtail(1,
                [GS#state.globalstatus
                 || {GS,_} <- H++[{S, ok}]]).

collect_init(_H,_S,_Res,Cmds) ->
  case collect_init(Cmds, 0) of
    -1 -> [{no_init}];
    0  -> [{init_first}];
    Nr -> [{init_at, Nr}]
  end.
collect_init(Cmds, Nr) ->
  case Cmds of
    []                      -> -1;
    [{_,_,{_,_,init,_}}|_]  -> Nr;
    [_|Xs]                  -> collect_init(Xs, Nr+1)
  end.

collect_length(_,_,_,Cmds) ->
  [{length_of_CmdList, length(Cmds)}].

collect_given_cmd_length(_H,_,_,Cmds) ->
  case Cmds of
    [] -> [{none, 0}];
    _  -> lists:map(fun ({_,_,{_,_,Cmd,_}}) ->
                        {list_to_atom("nr_of_"++atom_to_list(Cmd)),
                         length(
                           lists:filter(fun ({_,_,{_,_,Name,_}}) ->
                                            Name == Cmd
                                        end, Cmds))}
                    end,
                    Cmds)
  end.
