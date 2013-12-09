-module(wdgm_fsm_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-compile(export_all).
-compile({parse_transform,eqc_group_commands}).

-include("wdgm_config.hrl").

init([]) ->
  {ok,wdgm_global_status_ok,[]}.

wdgm_global_status_ok(_E,S) ->
  {next_state,wdgm_global_status_ok,S}.

wdgm_global_status_ok(_E,_F,S) ->
  {next_state,wdgm_global_status_ok,S}.

wdgm_global_status_stopped(_E, S) ->
  {next_state, wdgm_global_status_stopped, S}.

start() ->
    wdgm_eqc:start(),
    gen_fsm:start({local,wdgm_fsm},?MODULE,[],[]).

stop() ->
    gen_fsm:sync_send_all_state_event(wdgm_fsm,stop).

handle_sync_event(stop,_,_,_) ->
    {stop,normal,ok,[]}.

terminate(_,_,_) ->
    ok.

wdgm_global_status_ok(S) ->
    [init_command(S),
     getmode_command(S),
     setmode_command(S),
     deinit_command(S),
     checkpointreached_command(S),
     getglobalstatus_command(S),
     getlocalstatus_command(S),
     performreset_command(S),
     getfirstexpiredseid_command(S),
     mainfunction_command(S)
    ].

wdgm_global_status_stopped(S) ->
    [init_command(S),
     getmode_command(S),
     setmode_command(S),
     deinit_command(S),
     checkpointreached_command(S),
     getglobalstatus_command(S),
     getlocalstatus_command(S),
     performreset_command(S),
     getfirstexpiredseid_command(S),
     mainfunction_command(S)].

initial_state() ->
    wdgm_global_status_ok.

initial_state_data() ->
  Rs = wdgm_xml:start(),
  {_, R} = (hd(Rs)), %% why do we get a list of records?
  #state{originalCfg=R}.

%%------------------------------------------------------------------------------
%%------------------------------------------------------------------------------
%%------------------------------------------------------------------------------

precondition(wdgm_global_status_ok, wdgm_global_status_ok,
             S, {call, _M, F, _A}) ->
  case S#state.globalstatus of
    'WDGM_GLOBAL_STATUS_OK' ->
      apply(wdgm_pre, list_to_atom(atom_to_list(F)++"_pre"), [S]);
    _ -> false
  end;
precondition(_F, _T, S, {call, _M, F, _A}) ->
  apply(wdgm_pre, list_to_atom(atom_to_list(F)++"_pre"), [S]).


postcondition(_, _, S, {call, _M, F, A}, R) ->
  apply(wdgm_post, list_to_atom(atom_to_list(F)++"_post"), [S, A, R]).

next_state_data(_, _, S, R, {call, _M, F, A}) ->
  apply(wdgm_next, list_to_atom(atom_to_list(F)++"_next"), [S, R, A]).

%% -WdgM_Init-------------------------------------------------------------------

init_command(S) ->
  {history, wdgm_statem_eqc:init(S)}.

%% -WdgM_GetMode----------------------------------------------------------------

getmode_command(S) ->
  {history, wdgm_command:getmode_command(S)}.

%% -WdgM_SetMode----------------------------------------------------------------

setmode_command(S) ->
  {history, wdgm_command:setmode_command(S)}.

%% -WdgM_DeInit-----------------------------------------------------------------

deinit_command(S) ->
  {history, wdgm_command:deinit_command(S)}.

%% -WdgM_CheckpointReached------------------------------------------------------

checkpointreached_command(S) ->
  {history, wdgm_command:checkpointreached_command(S)}.

%% -WdgM_UpdateAliveCounter-----------------------------------------------------
%% Deprecated

%% -WdgM_GetLocalStatus---------------------------------------------------------

getlocalstatus_command(S) ->
  {history, wdgm_command:getlocalstatus_command(S)}.

%% -WdgM_GetGlobalStatus--------------------------------------------------------

getglobalstatus_command(S) ->
  {history, wdgm_command:getglobalstatus_command(S)}.

%% -WdgM_PerformReset-----------------------------------------------------------

performreset_command(S) ->
  {history, wdgm_command:performreset_command(S)}.

%% -WdgM_GetFirstExpiredSEID----------------------------------------------------

getfirstexpiredseid_command(S) ->
  {history, wdgm_command:getfirstexpiredseid_command(S)}.

%% -WdgM_MainFunction-----------------------------------------------------------

mainfunction_command(S) ->
  {history, wdgm_command:mainfunction_command(S)}.

%%------------------------------------------------------------------------------
%%------------------------------------------------------------------------------
%%------------------------------------------------------------------------------


prop_wdgm_fsm() ->
  ?SETUP( fun () -> start(),
                    fun () -> ok end
          end,
          ?FORALL(Cmds, commands(?MODULE),
                  begin
                    eqc_c:restart(),
                    gen_fsm:start({local,wdgm_fsm},?MODULE,[],[]),
                    {H,S,Res} = run_commands(?MODULE,Cmds),
                    stop(),
                    pretty_commands(?MODULE,Cmds,{H,S,Res},
                                    aggregate(collect_res(H,S,Res,Cmds),
                                              Res == ok))
                  end)).

collect_res(_H,_S,_Res,Cmds) ->
  command_names(Cmds).
  %Xs = lists:filter(fun({_,_,{_,_,Name,_}}) -> Name == initwdgm end,Cmds),

%  case Cmds of
%    [] -> [0];
%    Ys -> [{length(Xs)/length(Ys),length(Ys)}]
%  end.

%read_probability(locked,_,_) ->
%    0.6;
%read_probability(unlocked,_,_) ->
%    0.4.
