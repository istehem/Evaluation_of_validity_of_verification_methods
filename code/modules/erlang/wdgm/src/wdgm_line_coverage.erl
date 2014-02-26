-module(wdgm_line_coverage).
-compile(export_all).

compile() ->
  lists:map(fun(X) -> case cover:is_compiled(X) of
                          false -> cover:compile_module("../src/" ++ atom_to_list(X),[debug_info]);
                          _     -> ok
                      end
                    end,modules()).

modules() ->
  [
    wdgm_next,
    wdgm_post,
    wdgm_pre,
    wdgm_main,
    wdgm_checkpointreached
    %wdgm_statem_eqc
    %wdgm_command, %% Seems not to be running, probable cause "only seen in generation step"
  ].


analyse(Xs,Opts) ->
 Ys = analyse(Xs,Opts,[]),
 {_,L,R} = lists:foldl(
             fun({M0,L0,R0},{_,L1,R1}) ->
                begin
                  io:fwrite("Coverage for module " ++ atom_to_list(M0) ++ " = " ++ io_lib:format("~.2f",[(R0/L0)*100]) ++ "%\n"),
                  {M0,L0+L1,R0+R1}
                end
             end,
             {none,0,0},Ys),
 io:fwrite("Total Coverage" ++ " = " ++ io_lib:format("~.2f",[(R/L)*100]) ++ "%\n").


analyse([],_,Xr) ->
  cover:stop(),
  Xr;
analyse([X|Xs],Opts,Xr) ->
  File = atom_to_list(X) ++ ".coverdata",
  case file:read_file_info(File) of
    {ok,_} -> case lists:member(X,cover:imported_modules()) of
                true -> ok;
                _    -> cover:import(File)
              end;
    _      -> ok
  end,
  case Opts of
    false -> {_,Ans} = cover:analyse(X,coverage,line),
             io:fwrite("line_coverage=~s\n",[]),
             cover:analyse_to_file(X);
    _     -> {_,Ans} = cover:analyse(X,coverage,line),
             cover:analyse_to_file(X,Opts)
  end,
  cover:export(File,X),
  cover:reset(X),
  analyse(Xs,Opts,[return_cover_stats(Ans,0,length(Ans),X)|Xr]).

return_cover_stats([],N,L,M) ->
  {M,L,N};
return_cover_stats([A|Ans],N,L,M) ->
  case A of
    {{M,_},{1,_}} -> return_cover_stats(Ans,N + 1,L,M);
    _             -> return_cover_stats(Ans,N,L,M)
  end.
