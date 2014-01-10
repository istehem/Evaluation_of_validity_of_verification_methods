-module(compile_all).

-compile(export_all).

compile() ->
  compile:file("../src/wdgm"),
  compile:file("../src/wdgm_app"),
  compile:file("../src/wdgm_config_params"),
  compile:file("../src/wdgm_init_eqc"),
  %compile:file("../src/wdgm_mocking_eqc"),
  compile:file("../src/wdgm_checkpointreached"),
  compile:file("../src/wdgm_eqc"),
  compile:file("../src/wdgm_main"),
  compile:file("../src/wdgm_sup"),
  compile:file("../src/wdgm_xml").

