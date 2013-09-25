%%% @author Hans Svensson <>
%%% @copyright (C) 2012, Hans Svensson
%%% @doc
%%% Modelling End-to-end data protection (AUTOSAR)
%%%
%%% E2E basically consists of two functions, Protect - which
%%% is used by the sender and, Check - which is used at the
%%% receiving side. Protect has very little state, and is tested
%%% with a non-stateful property. Check is tested with a state
%%% machine.
%%% @end
%%% Created : 13 Nov 2012 by Hans Svensson <>

-module(api).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).


%% --- Test API
start() ->
  eqc_c:start(e2e, [verbose,
                    keep_files,
                    {c_src, "src/E2E_P01.c"},
                    {cppflags, " -I. -Istd -Iinc -ggdb3 "},
                  % {cflags, " -ftest-coverage -fprofile-arcs "}, %% keep_files,
                    {additional_files, ["src/Crc.c","src/E2E.c","src/E2E_P02.c"]},
                    {timeout, (1 bsl 32) - 1}]).

start_crc() ->
  eqc_c:start(crc, [verbose,{c_src, "src/Crc.c"},
                    {cppflags, " -I. -Istd -Iinc"}]).

setupmingw() ->
   os:putenv("PATH",os:getenv("PATH")++";c:\\MinGW\\bin").

start_gdbserver() ->
   fun(Exe) -> {os:find_executable("gdbserver"), [" localhost:1234 " ++ Exe]} end.

test({N, sec}) ->
  eqc:quickcheck(eqc:testing_time(N, e2e_eqc:prop_e2e()));
test({N, min}) ->
  eqc:quickcheck(eqc:testing_time(N*60, e2e_eqc:prop_e2e()));
test(N) when is_integer(N) ->
  eqc:quickcheck(eqc:numtests(N, e2e_eqc:prop_e2e())).

check() ->
  eqc:check(eqc_statem:show_states(e2e_eqc:prop_e2e())).

recheck() ->
  eqc:recheck(e2e_eqc:prop_e2e()).
