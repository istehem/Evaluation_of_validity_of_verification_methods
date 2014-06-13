-module(q_eqc).
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-record(state,  {ptr, size, elements}).

start() ->
   eqc_c:start(q,[verbose,
                  {c_src, "q_api.h"},
                  {additional_files, ["queue.o"]}]).


% new
new(Size) ->
   q:new(Size).
new_args(_S) ->
   [nat()].
new_pre(_S, [Size]) ->
   Size > 0.
new_next(_S, Ptr, [Size]) ->
   #state{ptr=Ptr, size=Size, elements=[]}.
new_post(_S, [_Size], Ret) ->
  case Ret of
    {ptr, "Queue", _} -> true;
    _                 -> false
  end.

% put
put(Ptr, Val) ->
   q:put(Ptr, Val).
put_args(S) ->
   [S#state.ptr, int()].
put_pre(S) ->
   (S#state.ptr /= undefined) andalso
   (length(S#state.elements) < S#state.size).
put_next(S, _V, [_, X]) ->
   S#state{elements = S#state.elements++[X]}.
put_post(_S, [_, Value], Ret) ->
   Ret == Value.

% get
get(Ptr) ->
   q:get(Ptr).
get_args(S) ->
   [S#state.ptr].
get_pre(S) ->
   (S#state.ptr /= undefined) andalso
   (length(S#state.elements) > 0).
get_next(S, _V, [_]) ->
   S#state{elements = tl(S#state.elements)}.
get_post(S, [_], Ret) ->
  Ret == hd(S#state.elements).

% space
space(Ptr) ->
   q:space(Ptr).
space_args(S) ->
   [S#state.ptr].
space_pre(S) ->
   S#state.ptr /= undefined.
%% space_return(S, [_]) ->
%%    S#state.size + length(S#state.elements).
space_post(S, [_], Ret) ->
   (0 =< Ret) and (Ret =< S#state.size).

% ...
initial_state() ->
   #state{}.

prop_q() ->
   ?FORALL(Cmds, commands(?MODULE),
      begin
         %eqc_c:restart(),
         {H, S, Res} = run_commands(?MODULE, Cmds),
         pretty_commands(
           ?MODULE, Cmds, {H, S, Res},
           aggregate(collect_cmds(Cmds), Res==ok))
      end).

collect_cmds(Cmds) ->
  [C || {set, _, {call, _, C, _}} <- Cmds].
