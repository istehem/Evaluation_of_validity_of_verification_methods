-module(wdgm_statem_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile([export_all, debug_info]).

-include("wdgm_types.hrl").
-include("wdgm_config.hrl").
-include_lib("wdgm_wrapper.hrl").

%%% QuickCheck specific functions ==============================================
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
  frequency([{weight(S, init)
              , wdgm_command:init_command(S)},
             {weight(S, getmode)
              , wdgm_command:getmode_command(S)},
             {weight(S, setmode)
              , wdgm_command:setmode_command(S)},
             {weight(S, deinit)
              , wdgm_command:deinit_command(S)},
             {weight(S, checkpointreached)
              , wdgm_command:checkpointreached_command(S)},
             {weight(S, getlocalstatus)
              , wdgm_command:getlocalstatus_command(S)},
             {weight(S, getglobalstatus)
              , wdgm_command:getglobalstatus_command(S)},
             {weight(S, performreset)
              , wdgm_command:performreset_command(S)},
             {weight(S, getfirstexpiredseid)
              , wdgm_command:getfirstexpiredseid_command(S)},
             {weight(S, mainfunction)
              , wdgm_command:mainfunction_command(S)},
             {weight(S, getversioninfo)
              , wdgm_command:getversioninfo_command(S)}]).

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
      true  -> {ptr, "WdgM_SupervisedEntityIdType", 0};
      false -> eqc_c:alloc("WdgM_SupervisedEntityIdType")
    end,
  R = ?C_CODE:'WdgM_GetFirstExpiredSEID'(Sp),
  case Is_Null of
    true  -> {R, null};
    false -> {R, eqc_c:deref(Sp)}
  end.

%%% -WdgM_MainFunction----------------------------------------------------------

mainfunction() ->
  ?C_CODE:'WdgM_MainFunction'().

%%% -WdgM_GetVersionInfo--------------------------------------------------------

getversioninfo(Is_Null) ->
  Sp =
    case Is_Null of
      true  -> {ptr, "Std_VersionInfoType", 0};
      false -> eqc_c:alloc("Std_VersionInfoType")
    end,
  R = ?C_CODE:'WdgM_GetVersionInfo'(Sp),
  case Is_Null of
    true  -> {R, null};
    false -> {R, eqc_c:deref(Sp)}
  end.

%%% -Frequency------------------------------------------------------------------

weight(S, setmode) ->
  case S#state.globalstatus of
    'WDGM_GLOBAL_STATUS_OK'        -> 7;
    'WDGM_GLOBAL_STATUS_FAILED'    -> 10;
    _                              -> 1
  end;
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
weight(_S, getversioninfo)         -> 2;
weight(_S, _Cmd)                   -> 1.

%%% -Properties-----------------------------------------------------------------

prop_wdgm_init() ->
  ?SETUP( fun () -> start(),
                    ?SETUP_COVERAGE,
                    fun () -> ?RUN_COVERAGE end
          end,
          ?WHENFAIL(?RUN_COVERAGE,
            ?FORALL(
                  Cmds, non_empty(commands(?MODULE)), %% Use eqc_gen:vector/2 in combination ?LET
                                                      %% for more commands
                  %Cmds,call_seq(),
                  begin
                    ?COPY_FILE,
                    eqc_c:restart(),
                    {H,S,Res} = run_commands(?MODULE,Cmds),
                    ?WRITE_HISTORY(statuses, H),
                    ?WRITE_HISTORY(commands, Cmds),
                    pretty_commands(
                      ?MODULE, Cmds, {H,S,Res},
                      aggregate(
                        collect_globalstatus(H,S,Res,Cmds),
                        aggregate(
                          collect_init(H,S,Res,Cmds),
                          aggregate(
                            collect_length(H,S,Res,Cmds),
                            aggregate(
                              collect_given_cmd_length(H,S,Res,Cmds),
                              aggregate(
                                collect_cmds_in_current_mode(H,S,Res,Cmds),
                                aggregate(
                                  collect_state_transitions(H,S,Res,Cmds),
                                  collect(S#state.currentMode,
                                          Res == ok))))))))
                  end))).

%%% copies the bullseye output file to current time <MS><S><mS>.cov
copy_bullseye_cov_file() ->
 Path = wdgm_eqc:getPath(["..","coverage"]),
 file:copy(Path ++ "test.cov",
           Path ++ ( fun({X,Y,Z}) ->
                         integer_to_list(X) ++
                           integer_to_list(Y) ++
                           integer_to_list(Z) ++ ".cov"
                     end) (now())).


run_coverage() ->
  wdgm_line_coverage:analyse(wdgm_line_coverage:modules(),[html]).

setup_coverage() ->
  wdgm_line_coverage:compile().

start () ->
  wdgm_eqc:start().

%%% lists all global statuses in the states
collect_globalstatus(H, S, _Res, _Cmds) ->
  lists:nthtail(1,
                [GS#state.globalstatus
                 || {GS,_} <- H++[{S, ok}]]).

%%% lists where init first is seen
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

%%% lists the length of the generated command sequence
collect_length(_,_,_,Cmds) ->
  [{length_of_CmdList, length(Cmds)}].

%%% lists the number of a given command
collect_given_cmd_length(_H,_,_,Cmds) ->
  case Cmds of
    [] -> [{none, 0}];
    _  ->
      lists:map(fun (Cmd) ->
                    {list_to_atom("nr_of_" ++ atom_to_list(Cmd)),
                     length(
                       lists:filter(fun ({_,_,{_,_,Name,_}}) ->
                                        Name == Cmd
                                    end,
                                    Cmds))}
                end,
                lists:usort([Cmd || {_,_,{_,_,Cmd,_}} <- Cmds]))
  end.

%%% lists the number of commands given the current mode
collect_cmds_in_current_mode(H,S,_,Cmds) ->
  case Cmds of
    [] -> [{none, 0}];
    _  ->
      ModeList = lists:nthtail(1,
                               [GS#state.currentMode
                                || {GS,_} <- H++[{S, ok}]]),
      LengthOfMode = fun(Mode) ->
                         length(lists:filter(fun (Elem) ->
                                                 Elem == Mode end,
                                             ModeList))
                     end,
      [{list_to_atom("cmds_in_currentmode_" ++ integer_to_list(Mode)),
        LengthOfMode(Mode)}
       || Mode <- [0,1,2,3], LengthOfMode(Mode) /= 0]
  end.

write_history_to_file(FA, H) ->
  File = "history_"++
    atom_to_list(FA)++"_"++
    atom_to_list(wdgm_xml:which_config())++".txt",
  case file:open(File,[append]) of
    {ok,IODevice} ->
      case FA of
        statuses -> io:fwrite(IODevice,"~w\n",[lists:map(fun({S,_}) -> S#state.globalstatus end, H)]);
        commands -> io:fwrite(IODevice,"~w\n",[lists:map(fun({_set, _var, {_call, _M, F, _A}}) -> F end, H)])
      end,
      file:close(IODevice);
    _             -> ok
  end.


collect_state_transitions(H,S,_Res, _Cmds) ->
  States = H++[{S,ok}],
  case length(States) >= 2 of
    true ->
      G1 = (element(1, lists:nth(1, States)))#state.globalstatus,
      G2 = (element(1, lists:nth(2, States)))#state.globalstatus,
      collect_state_transitions(G1, G2, lists:nthtail(2, States));
    false -> []
  end.
collect_state_transitions(_, _, []) ->
  [];
collect_state_transitions(G1, G2, [{Ns, _}|Ss]) ->
  case G1 /= G2 of
    true  -> [{G1, G2}];
    false -> []
  end++collect_state_transitions(G2, Ns#state.globalstatus, Ss).

call_seq() -> return
              ([
               {set,{var,1},{call,?MODULE,init,[{eqc_c:address_of(?CONFIG_FILE), false}]}},
               {set,{var,2},{call,?MODULE,checkpointreached,[1,7]}},
               {set,{var,3},{call,?MODULE,checkpointreached,[1,7]}},
               {set,{var,4},{call,?MODULE,mainfunction,[]}},
               {set,{var,5},{call,?MODULE,mainfunction,[]}},
               {set,{var,6},{call,?MODULE,checkpointreached,[0,0]}},
               {set,{var,7},{call,?MODULE,checkpointreached,[0,0]}},
               {set,{var,8},{call,?MODULE,checkpointreached,[1,7]}},
               {set,{var,9},{call,?MODULE,checkpointreached,[1,7]}},
               {set,{var,10},{call,?MODULE,checkpointreached,[4,27]}},
               {set,{var,11},{call,?MODULE,checkpointreached,[4,27]}},
               {set,{var,12},{call,?MODULE,mainfunction,[]}},
               {set,{var,13},{call,?MODULE,mainfunction,[]}},
               {set,{var,14},{call,?MODULE,mainfunction,[]}}
              ]).
