-module(wdgm_fsm_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-compile(export_all).
-compile({parse_transform,eqc_group_commands}).

-include("wdgm_config.hrl").

-define(COMMAND,wdgm_command).

start_statem() ->
    ok. %wdgm_eqc:start().

wdgm_ok()
  -> ok. % when S#state.globalstatus == 'WDGM_GLOBAL_STATUS_OK' -> S.

wdgm_deactivated(_)
  -> ok. %when S#state.globalstatus == 'WDGM_GLOBAL_STATUS_DEACTIVATED' -> S.

wdgm_stopped()
  -> ok.

valid(SD,S) ->
  case SD#state.globalstatus of
    S -> [];
    _ -> [elad]
  end.
%when S#state.globalstatus == 'WDGM_GLOBAL_STATUS_STOPPED' -> S.

'WDGM_GLOBAL_STATUS_DEACTIVATED'(_S) ->
  [{'WDGM_GLOBAL_STATUS_OK',{call,?MODULE,wdgm_ok,[]}}].

'WDGM_GLOBAL_STATUS_OK'(S) ->
  [{'WDGM_GLOBAL_STATUS_OK',{call,?MODULE,wdgm_ok,?LAZY(valid(S,'WDGM_GLOBAL_STATUS_OK'))}},
   %{'WDGM_GLOBAL_STATUS_DEACTIVATED',{call,?MODULE,all,[valid(S,'WDGM_GLOBAL_STATUS_DEACTIVATED')]}}].
%   {'WDGM_GLOBAL_STATUS_EXPIRED',{call,?MODULE,all,[?LAZY(valid(S,'WDGM_GLOBAL_STATUS_EXPIRED'))]}}].
%   {'WDGM_GLOBAL_STATUS_FAILED',{call,?MODULE,all,[valid(S,'WDGM_GLOBAL_STATUS_FAILED')]}},
   {'WDGM_GLOBAL_STATUS_STOPPED',{call,?MODULE,wdgm_stopped,?LAZY(valid(S,'WDGM_GLOBAL_STATUS_STOPPED'))}}
  ].

%'WDGM_GLOBAL_STATUS_EXPIRED'(S) ->
%   [{'WDGM_GLOBAL_STATUS_EXPIRED',{call,?MODULE,all,[valid(S,'WDGM_GLOBAL_STATUS_EXPIRED')]}},
%    {'WDGM_GLOBAL_STATUS_STOPPED',{call,?MODULE,all,[valid(S,'WDGM_GLOBAL_STATUS_STOPPED')]}}
%  ].
%
%'WDGM_GLOBAL_STATUS_FAILED'(S) ->
%  [{'WDGM_GLOBAL_STATUS_OK',{call,?MODULE,all,[valid(S,'WDGM_GLOBAL_STATUS_OK')]}},
%   {'WDGM_GLOBAL_STATUS_EXPIRED',{call,?MODULE,all,[valid(S,'WDGM_GLOBAL_STATUS_EXPIRED')]}},
%   {'WDGM_GLOBAL_STATUS_FAILED',{call,?MODULE,all,[valid(S,'WDGM_GLOBAL_STATUS_FAILED')]}},
%   {'WDGM_GLOBAL_STATUS_STOPPED',{call,?MODULE,all,[valid(S,'WDGM_GLOBAL_STATUS_STOPPED')]}}
%  ].


'WDGM_GLOBAL_STATUS_STOPPED'(S) ->
   [{'WDGM_GLOBAL_STATUS_STOPPED',{call,?MODULE,wdgm_stopped,[S]}}].

initial_state() ->
  'WDGM_GLOBAL_STATUS_DEACTIVATED'.

initial_state_data() ->
  Rs = wdgm_xml:start(),
  {_, R} = (hd(Rs)), %% why do we get a list of records?
  %wdgm_command:init_command(#state{originalCfg=R}),
  wdgm_next:init_next(#state{originalCfg=R},ok,[{ok,false}]).
  %Rs = wdgm_xml:start(),
  %{_, R} = (hd(Rs)), %% why do we get a list of records?
  %#state{originalCfg=R}.

%%------------------------------------------------------------------------------
%%------------------------------------------------------------------------------
%%------------------------------------------------------------------------------

precondition(_FS, _, _S, {call, _M, _F, _A}) ->
  true.
  %apply(wdgm_pre, list_to_atom(atom_to_list(F)++"_pre"), [S]).

postcondition(_FS, _, _S, {call, _M, _F, _A}, _R) ->
  true.
  %    apply(wdgm_post, list_to_atom(atom_to_list(F)++"_post"), [S, A, R]).

next_state_data(_FS, _, S, _R, {call, _M, _F, _A}) ->
  S.
  %  apply(wdgm_next, list_to_atom(atom_to_list(F)++"_next"), [S,R] ++ A).

%%------------------------------------------------------------------------------
%%------------------------------------------------------------------------------
%%------------------------------------------------------------------------------

prop_wdgm_fsm() ->
  ?SETUP( fun () -> start_statem(),
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

collect_res(_H,_S,_Res,Cmds) ->
  command_names(Cmds).
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
