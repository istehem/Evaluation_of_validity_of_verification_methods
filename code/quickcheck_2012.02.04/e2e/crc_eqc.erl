%%% @author Thomas Arts <>
%%% @copyright (C) 2013, Thomas Arts
%%% @doc
%%%
%%% @end
%%% Created : 23 Jan 2013 by Thomas Arts <>

-module(crc_eqc).

-include_lib("eqc/include/eqc.hrl").
-define(THREADPREFIX,"eqc_c_thread_").
-define(THREADED(Nodes,Expr), [ {Node,rpc:call(Node,erlang,apply,[fun() -> Expr end,[]])} || Node<-Nodes ]).

-compile(export_all).
-record(mcrc, {poly, bits, initial, xorval, refl = false}).


check() ->
  Ptr = eqc_c:create_array("uint8","123456789"), 
  16#4B = e2e:'Crc_CalculateCRC8'(Ptr,9,16#FF,1).


prop_check() ->
  ?FORALL(Crc_StartValue,choose(0,255),
          collect(Crc_StartValue,
          begin
            Ptr = eqc_c:create_array("uint8","123456789"), 
            16#4B == e2e:'Crc_CalculateCRC8'(Ptr,9,Crc_StartValue,1)
          end)).

byte() ->
  choose(0,255).

prop_magic_check() ->
  ?FORALL(Data, list(byte()),
    collect(Data,
    begin
      Ptr1 = eqc_c:create_array("uint8",Data), 
      N = length(Data),
      Res1 = e2e:'Crc_CalculateCRC8'(Ptr1,N,16#FF,1),
      Ptr2 = eqc_c:create_array("uint8",Data++[Res1]),
      Res2 = e2e:'Crc_CalculateCRC8'(Ptr2,N+1,16#FF,1),
      (16#C4 == Res2 bxor 16#FF)
    end)). 




start_multiple(Dirs) ->
  start_threads([ {list_to_atom(hd(string:tokens(Dir,"/"))),Dir} || Dir<-Dirs]).

start_threads(ThreadSpecs) when is_list(ThreadSpecs) ->
  [ start_thread(Name,SrcPath) || {Name,SrcPath}<-ThreadSpecs ].

start_thread(Name,Path) ->
  case string:tokens(atom_to_list(node()),"@") of
    [_,"nohost"] ->
      exit(need_host_name);
    [_,Host] ->
      %% If thread has been started before, this fails
      %% Todo: compatible with eqc_c:start, i.e., kill exisiting
      %% but be careful not to take node down if it is not a thread
      ThreadName = list_to_atom(lists:concat([?THREADPREFIX,Name])),
      Node = 
        case slave:start(Host,ThreadName) of
          {ok,N} -> N;
          {error,{already_running,N}} -> N
        end,
      Dir = filename:dirname(Path),
      rpc:call(Node,eqc_c,start,[crc,[{c_src, Path},{cppflags, lists:concat([" -I",Dir," -I . "])}]]),
      Node
  end.

stop_threads(Nodes) ->
  [ slave:stop(Node) || Node<-Nodes].


block() ->
%   shuffle(lists:seq(0,255)).
%  ?LET(N,choose(0,1024),vector(N,byte())).
   ?SIZED(Size,resize(2+Size*3,list(byte()))).

prop_compare(Nodes) ->
  ?FORALL({Data,Byte,Bool}, {block(),byte(),default(0,elements([1,0]))}, 
     begin
       N = length(Data),
       Results = 
       ?THREADED(Nodes,
                 begin 
                   Ptr = eqc_c:create_array("uint8",Data),
                   e2e:'Crc_CalculateCRC8'(Ptr,N,Byte,Bool)
                 end),
       ?WHENFAIL(io:format("Results = ~p\n",[Results]),
                 length(lists:usort([ R || {_,R}<-Results ])) == 1)
                                 
     end). 
  


prop_compare_model() ->
  ?FORALL({Data,Byte,Bool}, {block(),byte(),elements([1,0])}, 
     begin
       N = length(Data),
       %%store(N),
       Results = [ {c, begin 
                         Ptr = eqc_c:create_array("uint8",Data),
                         e2e:'Crc_CalculateCRC8'(Ptr,N,Byte,Bool)
                       end},
                   {model,
                        mcrc_8(Data, Byte, Bool)}
                 ],
       length(lists:usort([ R || {_,R}<-Results ])) == 1
                                 
     end). 
  

meta_collect() ->
  collector_stop(),
  timer:sleep(10),
  collector_start(),
  R = meta_collect(lists:seq(0,255),[]),
  io:format("\n"),
  R.

meta_collect([],Stat) ->
  Stat;
meta_collect([Byte|Bytes],Stat) ->
  io:format("~p ",[Byte]),
  NewStat = meta_collect(Byte,lists:seq(0,7),Stat),
  meta_collect(Bytes,NewStat).

meta_collect(Byte,[],Stat) ->
  Stat;
meta_collect(Byte,[Bit|Bits],Stat) ->
  crc:flipBit8(Byte,Bit),
  reset(),
  false = eqc:quickcheck(eqc:on_output(fun(_,_) -> ok end, 
                                        eqc_gen:noshrink(eqc:testing_time(20,prop_compare_model())))),
  Data = data(),
  crc:flipBit8(Byte,Bit),
  true = eqc:check(eqc:on_output(fun(_,_) -> ok end, prop_compare_model())),
  meta_collect(Byte,Bits,[{Data,Byte,Bit}|Stat]).


collector_start() ->
  register(the_collector,spawn(fun() -> loop([]) end)).

loop(Stored) ->
  receive
    {data,From} ->
      From ! {data,Stored},
      loop(Stored);
    {store,From,Data} ->
      From ! stored,
      loop([Data|Stored]);
    {reset,From} ->
      From ! reset,
      loop([]);
    {stop,From} ->
      From ! stop,
      stop
  end.

data() ->
  the_collector ! {data,self()},
  receive
    {data,Data} -> {lists:sum(Data),length(Data)}
  end.

store(Data) ->
  the_collector ! {store,self(),Data},
  receive
    stored -> ok
  end.

reset() ->
  the_collector ! {reset,self()},
  receive
    reset -> ok
  end.

collector_stop() ->
  catch the_collector ! {stop,self()},
  receive
    stop -> ok
  after 10 -> ok
  end.


%%% model CRC8 %%%%


mcrc_8(Data, SVal, First)    -> 
  mcrc(Data, SVal, First, 
       #mcrc{poly = 16#1D, bits = 8, initial = 16#FF, xorval = 16#FF}).

high_bit(N, Bits) ->
  <<B:1, _Rest/bitstring>> = <<N:Bits>>,
  B.

pow2(N) -> round(math:pow(2, N)).

mcrc(Data, _SVal, 1, Conf) ->
  mcrc(Data, Conf#mcrc.initial, Conf) bxor Conf#mcrc.xorval;
mcrc(Data, SVal, 0, Conf) ->
  mcrc(Data, Conf#mcrc.xorval bxor SVal, Conf) bxor Conf#mcrc.xorval.

mcrc([], Crc, _Conf) -> Crc;
mcrc([B | Data], Crc, Conf) ->
  B1 = B bsl (Conf#mcrc.bits - 8),
  Crc1 = mcrc_loop(8, Crc, B1, Conf),
  mcrc(Data, Crc1, Conf).

mcrc_loop(0, Crc, _B, _Conf) -> Crc;
mcrc_loop(K, Crc, B, Conf = #mcrc{ bits = Bs }) ->
  Crc1 = (Crc bsl 1) rem pow2(Bs),
  B1   = (B bsl 1) rem pow2(Bs),
  case high_bit(Crc bxor B, Bs) of
    1 -> mcrc_loop(K-1, Crc1 bxor Conf#mcrc.poly, B1, Conf);
    0 -> mcrc_loop(K-1, Crc1, B1, Conf)
  end.


%%%%%%% END MODEL of CRC %%%%%%%%
