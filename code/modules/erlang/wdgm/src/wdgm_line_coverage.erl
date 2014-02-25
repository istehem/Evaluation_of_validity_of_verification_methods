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

analyse([],_) ->
  cover:stop();
analyse([X|Xs],Opts) ->
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
             io:fwrite("line_coverage=~p\n",[return_cover_stats(Ans,0,length(Ans),X)]),
             cover:analyse_to_file(X);
    _     -> {_,Ans} = cover:analyse(X,coverage,line),
             io:fwrite("line_coverage=~p\n",[return_cover_stats(Ans,0,length(Ans),X)]),
             cover:analyse_to_file(X,Opts)
  end,
  cover:export(File,X),
  cover:reset(X),
  analyse(Xs,Opts).

return_cover_stats([],N,L,M) ->
  "Coverage for module " ++ atom_to_list(M) ++ "=" ++ float_to_list((N/L)*100) ++ "%";
return_cover_stats([A|Ans],N,L,M) ->
  case A of
    {{M,_},{1,_}} -> return_cover_stats(Ans,N + 1,L,M);
    _             -> return_cover_stats(Ans,N,L,M)
  end.
