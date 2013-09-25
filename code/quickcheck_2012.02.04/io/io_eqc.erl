-module(io_eqc).
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

% utils
filenames() -> 
   ["A.fil", "B.fil", "C.fil"].

filename() ->
   elements(filenames()).

% write
write(Filename, Bytes) ->
   file:write_file(Filename, Bytes).

write_args(_S) ->
   [filename(), binary()].

write_next(S, _V, [Fn, C]) ->
   case lists:keyfind(Fn, 1, S) of
      false -> [{Fn, C}|S];
      _ -> lists:keyreplace(Fn, 1, S, {Fn, C}) 
   end.

write_return(_S, _Args) ->
   ok.

% read
read(Filename) ->
   file:read_file(Filename).

read_args(_S) ->
   [filename()].

read_return(S, [Fn]) ->
   case lists:keyfind(Fn, 1, S) of
      false -> {error, enoent};
      {Fn, C} -> {ok, C}
   end.

% delete
delete(Filename) ->
   file:delete(Filename).

delete_args(_S) ->
   [filename()].

delete_next(S, _V, [Fn]) ->
   lists:keydelete(Fn, 1, S).

delete_return(S, [Fn]) ->
   case lists:keyfind(Fn, 1, S) of
      false -> {error, enoent};
      {_Fn, _C} -> ok
   end.

% ...
initial_state() ->
   [].

prop_q() ->
   ?FORALL(Cmds, commands(?MODULE),
      begin
         [file:delete(Fn) || Fn <- filenames()],
         {H, S, Res} = run_commands(?MODULE, Cmds),
         pretty_commands(?MODULE, Cmds, {H, S, Res}, Res==ok)
      end).

