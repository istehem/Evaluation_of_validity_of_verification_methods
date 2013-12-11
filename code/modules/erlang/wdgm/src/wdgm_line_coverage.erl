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

write_runsuits_and_analyse(_,[],_) ->
  ok;
write_runsuits_and_analyse(S,[X|Xs],Opts) ->
  eqc_suite:write(X,S),
  cover:reset(X),
  eqc_suite:run(wdgm_statem_eqc:prop_wdgm_init(),X),
  case Opts of
    false -> cover:analyse_to_file(X);
    _     -> cover:analyse_to_file(X,Opts)
  end,
  write_runsuits_and_analyse(S,Xs,Opts).

run_coverage(N,Opts) ->
  compile(),
  S = eqc_suite:feature_based(
    eqc_suite:line_coverage(modules(),
      eqc:numtests(N,
        wdgm_statem_eqc:prop_wdgm_init()))),
  write_runsuits_and_analyse(S,modules(),Opts).

generate_coverage_report(N) ->
  run_coverage(N,false).

generate_coverage_report(N,_Opts) ->
  run_coverage(N,[html]).




