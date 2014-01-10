-module(wdgm_line_coverage).
-compile(export_all).

compile() ->
  lists:map(fun(X) -> cover:compile_module("../src/" ++ atom_to_list(X)) end,modules()).

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
    {ok,_} -> cover:import(File);
    _      -> ok
  end,
  case Opts of
    false -> %cover:analyse(X,coverage,line),
             cover:analyse_to_file(X);
    _     -> %cover:analyse(X,coverage,line)
             cover:analyse_to_file(X,Opts)
  end,
  cover:export(File,X),
  cover:reset(X),
  analyse(Xs,Opts).

