-module(crc_eqc).

-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

uint8() -> choose(0, 255).

crc_start() -> eqc_c:start(crc,[verbose,{c_src,"crc_test.c"},{cppflags, " -Isrc -Istd -Iinc"},{additional_files, ["Crc.o"]}]).

start_thread(Name, Type) ->
   [_, Host] = string:tokens(atom_to_list(node()), "@"),
   Node = 
      case slave:start(Host, Name) of
         {ok, N} -> N;
         {error, {already_running, N}} -> N
      end,
   rpc:call(Node, os, putenv, ["PATH", os:getenv("PATH")++";c:\\MinGW\\bin"]),
   rpc:call(Node, eqc_c, start,
      [crc, [verbose, {c_src, "crc_test.c"}, {cppflags, " -I"++Type++"/std -I"++Type++"/inc"}, {additional_files, [Type++"/Crc.o"]}]]),
   Node.

kill_node(Node) ->
   rpc:call(Node, erlang, halt, []).

prop_compare(Node1, Node2) ->
   ?FORALL({Data, StartValue, FirstRun}, {list(uint8()), uint8(), elements([1,0])},
   begin
      N = length(Data),
      F = fun() -> 
         Ptr = eqc_c:create_array("uint8", Data),
         crc:'Crc_CalculateCRC8'(Ptr, N, StartValue, FirstRun)
      end,
      Result1 = rpc:call(Node1, erlang, apply, [F, []]),
      Result2 = rpc:call(Node2, erlang, apply, [F, []]),
      ?WHENFAIL(
         io:format("Results = ~p=~p, ~p=~p\n", [Node1, Result1, Node2, Result2]), 
         Result1 == Result2)
   end).

prop_magiccheck() ->
   ?FORALL(Data, list(uint8()),
      begin
         Ptr = eqc_c:create_array("uint8", Data),
         N = length(Data),
         Res1 = crc:'Crc_CalculateCRC8'(Ptr, N, 16#FF, 1),
         Ptr2 = eqc_c:create_array("uint8", Data++[Res1]),
         Res2 = crc:'Crc_CalculateCRC8'(Ptr2, N+1, 16#FF, 1),
         (16#C4 == Res2 bxor 16#FF)
      end).

prop_check16() ->
   ?FORALL(StartValue, choose(0,16#FFFF),
      begin
         Ptr = eqc_c:create_array("uint8", "123456789"),
         16#29b1 == crc:'Crc_CalculateCRC16'(Ptr, 9, StartValue, 1)
      end).

prop_magiccheck16() ->
   ?FORALL(Data, list(uint8()),
      begin
         Ptr = eqc_c:create_array("uint8", Data),
         N = length(Data),
         Res1 = crc:'Crc_CalculateCRC16'(Ptr, N, 16#FFFF, 1),
         Ptr2 = eqc_c:create_array("uint8", Data++[Res1 bsr 8, Res1 band 16#FF]),
         Res2 = crc:'Crc_CalculateCRC16'(Ptr2, N+2, 16#FFFF, 1),
         (16#0000 == Res2 bxor 16#0000)
      end).

prop_check32() ->
   ?FORALL(StartValue, choose(0, 16#FFFFFFFF),
      begin
         Ptr = eqc_c:create_array("uint8", "123456789"),
         16#CBF43926 == crc:'Crc_CalculateCRC32'(Ptr, 9, StartValue, 1)
      end).

prop_check() ->
   ?FORALL(StartValue, choose(0, 255),
      begin
         Ptr = eqc_c:create_array("uint8","123456789"),
         16#4b == crc:'Crc_CalculateCRC8'(Ptr, 9, StartValue, 1)
      end).

