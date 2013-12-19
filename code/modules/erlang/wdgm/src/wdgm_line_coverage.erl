-module(wdgm_line_coverage).
-compile(export_all).

compile() ->
  cover:compile_directory("../src").

modules() ->
  [
    wdgm_next,
    wdgm_post,
    wdgm_pre,
    wdgm_main,
    wdgm_checkpointreached,
    wdgm_statem_eqc
    %wdgm_command, %% Seems not to be running, probable cause "only seen in generation step"
  ].

analyse(_,[],_) ->
  ok;
analyse(N,[X|Xs],Opts) ->
  case Opts of
    false -> %cover:analyse(X,coverage,line),
             cover:analyse_to_file(X);
    _     -> %cover:analyse(X,coverage,line)
             cover:analyse_to_file(X,Opts)
  end,
  cover:reset(X),
  analyse(N,Xs,Opts).

run_coverage(N,Opts) ->
  compile(),
  eqc:quickcheck(eqc:numtests(N,wdgm_statem_eqc:prop_wdgm_init())),
  analyse(N,modules(),Opts).

generate_coverage_report(N) ->
  run_coverage(N,false).

generate_coverage_report(N,_Opts) ->
  run_coverage(N,[html]).




