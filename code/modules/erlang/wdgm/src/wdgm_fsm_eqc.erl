-module(wdgm_fsm_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-compile(export_all).
-compile({parse_transform,eqc_group_commands}).

-include("wdgm_config.hrl").

-define(COMMAND,wdgm_command).

init([]) ->
  {ok,wdgm_not_init,[]}.

start_statem() ->
    wdgm_eqc:start().

start() ->
    gen_fsm:start({global,wdgm_fsm},?MODULE,[],[]).

stop() ->
    gen_fsm:sync_send_all_state_event({global,wdgm_fsm},stop).

handle_sync_event(stop,_,_,_) ->
    {stop,normal,ok,[]}.

terminate(_,_,_) ->
    ok.

wdgm_not_init(_S) ->
  [{'WDGM_GLOBAL_STATUS_OK',?COMMAND:init_command(initial_state_data())}].
wdgm_not_init(E,S) ->
  {next_state,E,S}.
wdgm_not_init(E,C,S) ->
  gen_fsm:reply(C, ok),
  {next_state,E,S}.

'WDGM_GLOBAL_STATUS_OK'(S) ->
  function_list(S)
    ++
    [
     {'WDGM_GLOBAL_STATUS_OK',?COMMAND:init_command(S)},
     {'WDGM_GLOBAL_STATUS_DEACTIVATED',?COMMAND:deinit_command(S)},
     {'WDGM_GLOBAL_STATUS_OK',?COMMAND:mainfunction_command(S)},
     {'WDGM_GLOBAL_STATUS_FAILED',?COMMAND:mainfunction_command(S,S)},
     {'WDGM_GLOBAL_STATUS_EXPIRED',?COMMAND:mainfunction_command(S,S,S)},
     {'WDGM_GLOBAL_STATUS_STOPPED',?COMMAND:mainfunction_command(S,S,S,S)}
    ].
'WDGM_GLOBAL_STATUS_FAILED'(S) ->
  function_list(S)++
    [
     {'WDGM_GLOBAL_STATUS_FAILED',?COMMAND:init_command(S)},
     {'WDGM_GLOBAL_STATUS_FAILED',?COMMAND:deinit_command(S)},
     {'WDGM_GLOBAL_STATUS_OK',?COMMAND:mainfunction_command(S)},
     {'WDGM_GLOBAL_STATUS_FAILED',?COMMAND:mainfunction_command(S,S)},
     {'WDGM_GLOBAL_STATUS_EXPIRED',?COMMAND:mainfunction_command(S,S,S)},
     {'WDGM_GLOBAL_STATUS_STOPPED',?COMMAND:mainfunction_command(S,S,S,S)}
    ].
'WDGM_GLOBAL_STATUS_EXPIRED'(S) ->
  function_list(S)++
    [
     {'WDGM_GLOBAL_STATUS_EXPIRED',?COMMAND:init_command(S)},
     {'WDGM_GLOBAL_STATUS_EXPIRED',?COMMAND:deinit_command(S)},
     {'WDGM_GLOBAL_STATUS_EXPIRED',?COMMAND:mainfunction_command(S)},
     {'WDGM_GLOBAL_STATUS_STOPPED',?COMMAND:mainfunction_command(S,S)}
    ].
'WDGM_GLOBAL_STATUS_STOPPED'(S) ->
  function_list(S)++
    [
     {'WDGM_GLOBAL_STATUS_STOPPED',?COMMAND:init_command(S)},
     {'WDGM_GLOBAL_STATUS_STOPPED',?COMMAND:deinit_command(S)},
     {'WDGM_GLOBAL_STATUS_STOPPED',?COMMAND:mainfunction_command(S)}
    ].
'WDGM_GLOBAL_STATUS_DEACTIVATED'(S) ->
 function_list(S)++
    [
     {'WDGM_GLOBAL_STATUS_OK',?COMMAND:init_command(S)},
     {'WDGM_GLOBAL_STATUS_DEACTIVATED', ?COMMAND:deinit_command(S)}
    ].

'WDGM_GLOBAL_STATUS_OK'(E,S) ->
  {next_state,E,S}.
'WDGM_GLOBAL_STATUS_FAILED'(E,S) ->
  {next_state,E,S}.
'WDGM_GLOBAL_STATUS_EXPIRED'(E,S) ->
  {next_state,E,S}.
'WDGM_GLOBAL_STATUS_STOPPED'(E,S) ->
  {next_state,E,S}.
'WDGM_GLOBAL_STATUS_DEACTIVATED'(E,S) ->
  {next_state,E,S}.

%% 'WDGM_GLOBAL_STATUS_OK'(E,C,S) ->
%%   io:fwrite("##~p\n", [E]),
%%   gen_fsm:reply(C, ok),
%%   {next_state,E,S}.
%% 'WDGM_GLOBAL_STATUS_FAILED'(E,C,S) ->
%%   gen_fsm:reply(C, ok),
%%   {next_state,E,S}.
%% 'WDGM_GLOBAL_STATUS_EXPIRED'(E,C,S) ->
%%   gen_fsm:reply(C, ok),
%%   {next_state,E,S}.
%% 'WDGM_GLOBAL_STATUS_STOPPED'(E,C,S) ->
%%   gen_fsm:reply(C, ok),
%%   {next_state,E,S}.
%% 'WDGM_GLOBAL_STATUS_DEACTIVATED'(E,C,S) ->
%%   gen_fsm:reply(C, ok),
%%   {next_state,E,S}.

function_list(S) ->
    [
     {history,?COMMAND:getmode_command(S)},
     {history,?COMMAND:setmode_command(S)},
     {history,?COMMAND:checkpointreached_command(S)},
     {history,?COMMAND:getglobalstatus_command(S)},
     {history,?COMMAND:getlocalstatus_command(S)},
     {history,?COMMAND:performreset_command(S)},
     {history,?COMMAND:getfirstexpiredseid_command(S)}
%%     {history,?COMMAND:deinit_command(S)},
%%     {history,?COMMAND:mainfunction_command(S)},
%%     {history,?COMMAND:init_command(S)}
    ].

initial_state() ->
    wdgm_not_init.

initial_state_data() ->
  Rs = wdgm_xml:start(),
  {_, R} = (hd(Rs)), %% why do we get a list of records?
  #state{originalCfg=R}.

%%------------------------------------------------------------------------------
%%------------------------------------------------------------------------------
%%------------------------------------------------------------------------------

precondition(FS, _, S, {call, _M, F, _A}) ->
  case FS of
    wdgm_not_init -> true;
    _             -> apply(wdgm_pre, list_to_atom(atom_to_list(F)++"_pre"), [S])
  end.

postcondition(FS, _, S, {call, _M, F, A}, R) ->
  case FS of
    wdgm_not_init -> true;
    _             -> apply(wdgm_post, list_to_atom(atom_to_list(F)++"_post"), [S, A, R])
  end.

next_state_data(FS, _, S, R, {call, _M, F, A}) ->
  NewA =
  case A of
    [] -> [void];
    _ -> [A]
  end,
  case FS of
   wdgm_not_init -> %gen_fsm:send_event({global,wdgm_fsm},'WDGM_GLOBAL_STATUS_OK'),
                    apply(wdgm_next, list_to_atom(atom_to_list(F)++"_next"), [S,R] ++ NewA);
   _             ->
                    SNew = apply(wdgm_next, list_to_atom(atom_to_list(F)++"_next"), [S,R] ++ NewA),
                    %gen_fsm:send_event({global,wdgm_fsm},SNew#state.globalstatus),
                    SNew
  end.

prop_wdgm_fsm() ->
  ?SETUP( fun () -> start_statem(),
                    fun () -> ok end
          end,
          ?FORALL(Cmds, commands(?MODULE),
                  begin
                    eqc_c:restart(),
                    start(),
                    {H,S,Res} = run_commands(?MODULE,Cmds),
                    stop(),
                    pretty_commands(?MODULE,Cmds,{H,S,Res},
                                    aggregate(collect_res(H,S,Res,Cmds),
                                              Res == ok))
                  end)).

collect_res(H,_S,_Res,Cmds) ->
  zip(state_names(H),command_names(Cmds)).
  %Xs = lists:filter(fun({_,_,{_,_,Name,_}}) -> Name == initwdgm end,Cmds),

%  case Cmds of
%    [] -> [0];
%    Ys -> [{length(Xs)/length(Ys),length(Ys)}]
%  end.

%priority(F,F,{call,_,setmode,_}) ->
%  100;
%priority(_,_,{call,_,setmode,_}) ->
%  0.

%priority(_,_,_) -> io:fwrite("Trams\n").
%read_probability(F,_,_) ->
%    0.6;
%read_probability(unlocked,_,_) ->
%    0.4.
