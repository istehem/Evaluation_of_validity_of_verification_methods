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

-module(e2e_eqc_only_protect).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-include("e2e.hrl").

-compile(export_all).

-define(TRUE, 1).
-define(FALSE, 0).

-record(state,{cfg, sender, receiver}).

-record(sender,{counter}).

-record(receiver,{rcv}).

-record(cfg, { data_id,
               data_length,
               max_delta_counter_init }).

-record(mcrc, {poly , bits, initial ,
        xorval , refl }).

pow2(N) -> 
   1 bsl N.

%% --- Testing Protect
config() ->
  ?LET({DataId, LenBytes, MaxDelta},
       {choose(0, 65535), choose(2, 30), choose(1, 14)},
       #cfg{ data_length = LenBytes,
             max_delta_counter_init = MaxDelta,
             data_id = DataId }).

%% Fill C struct with data
cfg_p01(Cfg) ->
  #'E2E_P01ConfigType_Tag'{
         'CounterOffset' = 8,
         'CRCOffset' = 0,
         'DataID' = Cfg#cfg.data_id,
         'DataIDMode' = 'E2E_P01_DATAID_BOTH',
         'DataLength' = 8*Cfg#cfg.data_length,
         'MaxDeltaCounterInit' = Cfg#cfg.max_delta_counter_init }.
  
%% 'LostData' and 'Status' are out parameters.
rcv(#cfg{max_delta_counter_init = M}) ->
  #'E2E_P01ReceiverStateType'{
     'LastValidCounter'  = 0,
     'MaxDeltaCounter'   = M,
     'WaitForFirstData'  = ?TRUE,
     'NewDataAvailable'  = ?FALSE}.

senderstate(Counter) ->
  #'E2E_P01SenderStateType_Tag'{'Counter'=Counter}.



uint8() ->
  choose(0,255).


flip(Bit, Data) ->
  <<Prefix:Bit,B:1,Tail/bitstring>> = list_to_binary(Data),
  binary_to_list(<< <<Prefix:Bit>>/bitstring, <<(1-B):1>>/bitstring, Tail/bitstring>>).




%% E2E_P01Protect


e2e_protect(ModelConfig, ModelCounter, Data) ->
  Config = alloc(cfg_p01(ModelConfig)),
  Counter = alloc(senderstate(ModelCounter)),
  DataPtr = alloc(Data),
  Ret = e2e:'E2E_P01Protect'(Config, Counter, DataPtr),
  {Ret, deref(Counter), eqc_c:read_array(DataPtr, length(Data))}.

e2e_protect_args(State) ->
  [State#state.cfg, (State#state.sender)#sender.counter, vector((State#state.cfg)#cfg.data_length,uint8()) ].

e2e_protect_next(S, _, [_, ModelCounter,_]) ->
  S#state{ sender = #sender{counter = (ModelCounter + 1) rem 15} }.

e2e_protect_return(_State,[ModelConfig, ModelCounter, Data]) ->
  DataId  = ModelConfig#cfg.data_id,
  {0, increment(ModelCounter), model_protect(DataId, ModelCounter, Data)}.

model_protect(DataId, ModelCounter, Data) ->
  %% Hard code Profile 1A for now...
  Offset = 0,
  %% The 4 bit counter should go into the lower half of the byte
  <<_CRCPlaceHolder:8, CntData:4, _CntPlaceHolder:4, Bin/binary >> = list_to_binary(Data),
  %% In 1A we should use both bytes of DataId...
  NewData = <<CntData:4, ModelCounter:4, Bin/binary>>,  %% Page 36 of E2E

  <<High8ID:8, Low8ID:8>> = <<DataId:16>>,
  CRC1 = mcrc_8([Low8ID], 16#FF, 0),
  CRC2 = mcrc_8([High8ID], CRC1, 0),
  CRC3 =
    case Offset >= 8 of
      true -> mcrc_8(binary_to_list(<<CntData:4, ModelCounter:4>>),CRC2,0);  %% Page 36 of E2E
      false -> CRC2
    end,
  CRC4 =
    case 0 < length(Data)-1 of
      false -> CRC3;
      true ->
        mcrc_8(binary_to_list(NewData),CRC3,0)
    end,
  CRC = CRC4 bxor 16#FF,
  binary_to_list(<<CRC, NewData/binary>>).

  %% FullDataBin = <<Low8ID, High8ID, CntData:4, ModelCounter:4, Bin/binary>>,
  %% CRC = crc_eqc:mcrc_8(binary_to_list(FullDataBin), 16#ff, 0) bxor 16#ff,


initial_state(Cfg) ->
   #state{ cfg = Cfg,
           sender = #sender{counter = 0},
           receiver = #receiver{rcv = rcv(Cfg)} }.
    
%% @doc Default generated property
prop_e2e() ->
  ?FORALL(Cfg, config(),
    ?FORALL(Cmds, commands(?MODULE,initial_state(Cfg)),
            aggregate(command_names(Cmds),
                      begin
%%                        eqc_c:restart(),
                        {H, S, Res} = run_commands(?MODULE,Cmds),
                        pretty_commands(?MODULE, Cmds, {H, S, Res},
                                        Res == ok)
                      end))).

%% Local implementations




alloc(Bin) when is_binary(Bin) ->
  eqc_c:create_array("uint8", binary_to_list(Bin));
alloc(L) when is_list(L) ->
  eqc_c:create_array("uint8", L);
alloc(Struct) when is_tuple(Struct) ->
  eqc_c:alloc({struct, element(1,Struct)}, Struct).

deref(Ptr) ->
  case eqc_c:deref(Ptr) of
    #'E2E_P01SenderStateType_Tag'{ 'Counter' = Counter } -> 
      Counter;
    #'E2E_P01ReceiverStateType'{'LostData'         = LostData,
                                'Status'           = Status} ->
      {LostData,Status}
  end.

increment(N) ->
  (N + 1) rem 15.


%% updateRcvState(Cfg, Rcv = #rcv_p01{last_valid_counter  = LastValid,
%%                                    max_delta_counter   = MaxDelta,
%%                                    wait_for_first_data = WFFD},
%%                {NewData, {Corrupt, Data}}) ->
%%   NewMaxDelta = lists:min([MaxDelta+1, 14]),
%%   Rcv1 = Rcv#rcv_p01{ max_delta_counter = NewMaxDelta,
%%                       new_data_available = new_data_available(NewData)},
%%   case new_data_available(NewData) of
%%     ?TRUE ->
%%       case Corrupt of
%%         [] ->
%%           Counter = get_counter(Data),
%%           case WFFD of
%%             ?TRUE ->
%%               Rcv1#rcv_p01{ max_delta_counter   = Cfg#cfg_p01.max_delta_counter_init,
%%                             wait_for_first_data = 0,
%%                             last_valid_counter  = Counter,
%%                             status = 'E2E_P01STATUS_INITAL'
%%                           };
%%             ?FALSE ->
%%               Delta = ((15 + Counter) - LastValid) rem 15,
%%               if Delta == 0 ->
%%                   Rcv1#rcv_p01{ status = 'E2E_P01STATUS_REPEATED' };
%%                  Delta > NewMaxDelta ->
%%                   Rcv1#rcv_p01{ status = 'E2E_P01STATUS_WRONGSEQUENCE' };
%%                  true ->
%%                   Rcv1#rcv_p01{ max_delta_counter   = Cfg#cfg_p01.max_delta_counter_init,
%%                                 last_valid_counter  = Counter,
%%                                 lost_data = Delta - 1,
%%                                 status = if Delta == 1 -> 'E2E_P01STATUS_OK';
%%                                             true       -> 'E2E_P01STATUS_OKSOMELOST' end
%%                               }
%%               end
%%           end;
%%         _ ->
%%           Rcv1#rcv_p01{ status = 'E2E_P01STATUS_WRONGCRC' }
%%       end;
%%     ?FALSE ->
%%       Rcv1#rcv_p01{ status = 'E2E_P01STATUS_NONEWDATA' }
%%   end.

get_counter([_, CounterByte | _])->
  <<_:4, C:4>> = <<CounterByte>>,
  C.

prop_crc() ->
   ?FORALL({Data, StartValue, IsFirstCall}, {list(uint8()), uint8(), elements([1, 0])},
      begin
         DataLength = length(Data),
         DataPtr = eqc_c:create_array("uint8", Data),
         Crc8 = e2e:'Crc_CalculateCRC8'(DataPtr, DataLength, StartValue, IsFirstCall),
         equals(Crc8, mcrc_8(Data, StartValue, IsFirstCall))
      end).

mcrc_8() ->
  #mcrc{poly = 16#1D, bits = 8, initial = 16#FF, xorval = 16#FF}.
mcrc_8h2f() ->
  #mcrc{poly = 16#2F, bits = 8, initial = 16#FF, xorval = 16#FF}.
mcrc_16() ->
  #mcrc{poly = 16#1021, bits = 16, initial = 16#FFFF, xorval = 16#0000}.
mcrc_32() ->
  #mcrc{poly = 16#04C11DB7, bits = 32, initial = 16#FFFFFFFF,
        xorval = 16#FFFFFFFF, refl = fun reflect32/1}.

mcrc_8(Data, SVal, First)    -> mcrc(Data, SVal, First, mcrc_8()).
mcrc_8h2f(Data, SVal, First) -> mcrc(Data, SVal, First, mcrc_8h2f()).
mcrc_16(Data, SVal, First)   -> mcrc(Data, SVal, First, mcrc_16()).
mcrc_32(Data, SVal, First)   -> mcrc(Data, SVal, First, mcrc_32()).

high_bit(N, Bits) ->
  <<B:1, _Rest/bitstring>> = <<N:Bits>>,
  B.

mcrc(Data, _SVal, ?TRUE, Conf) ->
  mcrc(Data, Conf#mcrc.initial, Conf) bxor Conf#mcrc.xorval;
mcrc(Data, SVal, ?FALSE, Conf) ->
  mcrc(Data, Conf#mcrc.initial bxor SVal, Conf) bxor Conf#mcrc.xorval.

mcrc([], Crc, #mcrc{ refl = Refl }) when is_function(Refl) -> Refl(Crc);
mcrc([], Crc, _Conf) -> Crc;
mcrc([B | Data], Crc, Conf = #mcrc{ refl = Refl }) ->
  B1 = if is_function(Refl) -> Refl(B);
          true -> B bsl (Conf#mcrc.bits - 8) end, %% Align
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

reflectB(<<B1:1, B2:1, B3:1, B4:1, B5:1, B6:1, B7:1, B8:1>>) ->
  <<B8:1, B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1>>.

reflect8(N) when N < 256 ->
  <<R:8>> = reflectB(<<N>>),
  R.
reflect32(N) ->
  <<N1:8, N2:8, N3:8, N4:8>> = <<N:32>>,
  <<R:32>> = <<(reflect8(N4)):8, (reflect8(N3)):8, (reflect8(N2)):8, (reflect8(N1)):8>>,
  R.
