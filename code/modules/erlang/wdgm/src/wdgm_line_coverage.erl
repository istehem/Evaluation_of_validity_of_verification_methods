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

write_runsuits_and_analyse(_,[]) ->
  ok;
write_runsuits_and_analyse(S,[X|Xs]) ->
  eqc_suite:run(wdgm_statem_eqc:prop_wdgm_init(),X),
  eqc_suite:write(X,S),
  cover:analyse_to_file(X),
  write_runsuits_and_analyse(S,Xs).

run_coverage(N) ->
  compile(),
  S = eqc_suite:feature_based(
    eqc_suite:line_coverage(modules(),
      eqc:numtests(N,
        wdgm_statem_eqc:prop_wdgm_init()))),
  write_runsuits_and_analyse(S,modules()),
  cover:reset().

