%%% @author Hans Svensson, Thomas Arts
%%% @copyright (C) 2013, Quviq AB
%%% @doc
%%% Modelling End-to-end data protection (AUTOSAR)
%%%
%%% E2E basically consists of two functions, Protect - which
%%% is used by the sender and, Check - which is used at the
%%% receiving side. Protect has very little state, and is tested
%%% with a non-stateful property. Check is tested with a state
%%% machine.
%%% @end
%%% Created : 13 Nov 2012 by Hans Svensson
%%% Modified: 03 Feb 2013 by Thomas Arts

-module(e2e_eqc_faulty).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-include("e2e.hrl").

-compile(export_all).

-define(TRUE, 1).
-define(FALSE, 0).

-record(state,{cfg, sender, receiver,
               data = [], %% the data that has been protected and is sent
               new_data = ?FALSE,
               corrupted = []  %% bits that are corrupted
              }).

-record(sender,{counter, msg}).

-record(receiver,{rcv}).

-record(cfg, { data_id,
               data_length,
               max_delta_counter_init }).


%% --- Testing Protect
config() ->
  #cfg{ data_length = choose(2, 30),  %% for P01 E2E0018
        max_delta_counter_init = choose(1, 14),
        data_id = choose(0, 65535) }.

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
  #'E2E_P01SenderStateType_Tag'{ 'Counter' = Counter}.


uint8() ->
  choose(0,255).



%% E2E_P01Protect


e2e_protect(ModelConfig, ModelCounter, Data) ->
  Config = alloc(cfg_p01(ModelConfig)),
  Counter = alloc(senderstate(ModelCounter)),
  DataPtr = alloc(Data),
  Ret = e2e:'E2E_P01Protect'(Config, Counter, DataPtr),
  {Ret, deref(Counter), eqc_c:read_array(DataPtr, length(Data))}.

e2e_protect_args(S) ->
  [S#state.cfg,
   (S#state.sender)#sender.counter,
   vector((S#state.cfg)#cfg.data_length,uint8()) ].

e2e_protect_next(S, _, [_Cfg, ModelCounter,Data]) ->
  S#state{ sender = #sender{counter = (ModelCounter + 1) rem 15,
                            msg = Data}}.

e2e_protect_return(_S,[#cfg{data_id = DataId}, ModelCounter, Data]) ->
  {0, increment(ModelCounter), model_protect(DataId, ModelCounter, Data)}.


model_protect(DataId, ModelCounter, Data) ->
  %% Hard code Profile 1A for now...
  Offset = 0,
  %% The 4 bit counter should go into the lower half of the byte
  <<_CRCPlaceHolder:8, CntData:4, CntPlaceHolder:4, Bin/binary >> = list_to_binary(Data),
  %% In 1A we should use both bytes of DataId...
  NewData = <<CntData:4, ModelCounter:4, Bin/binary>>,  %% Page 36 of E2E

  <<High8ID:8, Low8ID:8>> = <<DataId:16>>,
  CRC1 = crc_eqc:mcrc_8([DataId band 16#ff], 16#FF, 0),
  CRC2 = crc_eqc:mcrc_8([(DataId bsr 8) band 16#ff], CRC1, 0),
  CRC3 =
    case Offset >= 8 of
      true -> crc_eqc:mcrc_8(binary_to_list(<<CntData:4, ModelCounter:4>>),CRC2,0);  %% Page 36 of E2E
      false -> CRC2
    end,
  CRC4 =
    case 0 < length(Data)-1 of
      false -> CRC3;
      true ->
        crc_eqc:mcrc_8(binary_to_list(NewData),CRC3,0)
    end,
  CRC = CRC4 bxor 16#FF,
  binary_to_list(<<CRC, NewData/binary>>).

  %% FullDataBin = <<Low8ID, High8ID, CntData:4, ModelCounter:4, Bin/binary>>,
  %% CRC = crc_eqc:mcrc_8(binary_to_list(FullDataBin), 16#ff, 0) bxor 16#ff,


%% Sending the data, possibly corrupting it

send(_Corrupt) ->
  ok.

send_args(S) ->
  Data = (S#state.sender)#sender.msg,
  % [[]].
  [subset(3,lists:seq(0,8*length(Data)-1))].

subset(Max,L) ->
  ?LET({N,LShuf},{choose(0,Max),shuffle(L)},
       begin
         {Prefix,_Suffix} = lists:split(N,LShuf),
         Prefix
       end).


send_pre(S) ->
  (S#state.sender)#sender.msg /= undefined.

send_next(S,_,[Corrupt]) ->
  Msg = (S#state.sender)#sender.msg,
  ModelCounter = (S#state.sender)#sender.counter,
  DataId = (S#state.cfg)#cfg.data_id,
  Data = model_protect(DataId,ModelCounter,Msg),
  S#state{data = lists:foldr(fun(Bit,D) ->
                                 flip(Bit,D)
                             end, Data, Corrupt),
          new_data = ?TRUE,
          corrupted = Corrupt}.


%% E2E_P01Check

e2e_check(Cfg, RcvState, NewData, Data) ->
  Config = alloc(cfg_p01(Cfg)),
  %% Need to update the Receive state by setting NewDataAvailable
  Rcv = deref(RcvState),
  eqc_c:store(RcvState,Rcv#'E2E_P01ReceiverStateType'{'NewDataAvailable' = NewData}),
  DataPtr = alloc(Data),
  Ret = e2e:'E2E_P01Check'(Config, RcvState, DataPtr),
  {Ret,deref(RcvState)}.

% rcv is symbolic
e2e_check_args(S) ->
  [ S#state.cfg,
    {var,rcvstate},
    S#state.new_data,
    S#state.data].

e2e_check_pre(S, [_, _, NewData, Data]) ->
  S#state.new_data == NewData andalso S#state.data == Data.  %% For shrinking!

e2e_check_next(S, _, [Cfg, _, NewData, Data]) ->
  Rcv = (S#state.receiver)#receiver.rcv,
  ModelRcv =
    modelRcvState(Cfg,
                  Rcv#'E2E_P01ReceiverStateType'{'NewDataAvailable' = NewData},
                  {S#state.corrupted, Data}),
  S#state{ receiver = #receiver{rcv = ModelRcv},
           new_data = ?FALSE }.

%% e2e_check_post(S, [_, _, _, _], #'E2E_P01ReceiverStateType'{'Status' = Status}) ->
%%   Status /= 'E2E_P01STATUS_OK'.

e2e_check_return(S, [Cfg, _Rcv, NewData, Data]) ->
  Rcv = (S#state.receiver)#receiver.rcv,
  {0,modelRcvState(Cfg,
                   Rcv#'E2E_P01ReceiverStateType'{'NewDataAvailable' = NewData},
                   {S#state.corrupted, Data})}.


modelRcvState(Cfg, Rcv = #'E2E_P01ReceiverStateType'{
                      'LastValidCounter'  = LastValid,
                      'MaxDeltaCounter'   = MaxDelta,
                      'WaitForFirstData'  = WFFD,
                      'NewDataAvailable'  = NewData
                     },
               {Corrupt, Data}) ->
  NewMaxDelta = lists:min([MaxDelta+1, 14]),
  Rcv1 = Rcv#'E2E_P01ReceiverStateType'{ 'MaxDeltaCounter' = NewMaxDelta},
  case NewData of
    ?TRUE ->
      case Corrupt of
        [] ->
          Counter = get_counter(Data),
          case WFFD of
            ?TRUE ->
              Rcv1#'E2E_P01ReceiverStateType'{
                'MaxDeltaCounter'   = Cfg#cfg.max_delta_counter_init,
                'WaitForFirstData' = 0,
                'LastValidCounter'  = Counter,
                'Status' = 'E2E_P01STATUS_INITAL'};
            ?FALSE ->
              Delta = ((15 + Counter) - LastValid) rem 15,
              if Delta == 0 ->
                  Rcv1#'E2E_P01ReceiverStateType'{ 'Status' = 'E2E_P01STATUS_REPEATED' };
                 Delta > NewMaxDelta ->
                  Rcv1#'E2E_P01ReceiverStateType'{ 'Status' = 'E2E_P01STATUS_WRONGSEQUENCE' };
                 true ->
                  Rcv1#'E2E_P01ReceiverStateType'{
                    'MaxDeltaCounter'  = Cfg#cfg.max_delta_counter_init,
                    'LastValidCounter' = Counter,
                    'LostData'         = Delta - 1,
                    'Status'      = if Delta == 1 -> 'E2E_P01STATUS_OK';
                                       true       -> 'E2E_P01STATUS_OKSOMELOST' end
                   }
              end
          end;
        _ ->
          Rcv1#'E2E_P01ReceiverStateType'{ 'Status' = 'E2E_P01STATUS_WRONGCRC' }
      end;
    ?FALSE ->
      Rcv1#'E2E_P01ReceiverStateType'{ 'Status' = 'E2E_P01STATUS_NONEWDATA' }
  end.



%% @doc weight/2 - Distribution of calls
-spec weight(S :: eqc_statem:symbolic_state(), Command :: atom()) -> integer().
weight(_S, e2e_protect) -> 1;
weight(_S, send) -> 5;
weight(_S, e2e_check) -> 20.


initial_state(Cfg) ->
   Rcv = rcv(Cfg),
   #state{ cfg = Cfg,
           sender = #sender{counter = 0},
           receiver =
             %% add out parameters for type correctness
             #receiver{rcv = Rcv#'E2E_P01ReceiverStateType'{'LostData'=0,
                                                            'Status'='E2E_P01STATUS_NONEWDATA'}
                      }}.

%% @doc Default generated property
prop_e2e() ->
  ?FORALL(Cfg, config(),
    ?FORALL(Cmds, commands(?MODULE,initial_state(Cfg)),
            aggregate(command_names(Cmds),
                      begin
                        eqc_c:restart(),
                        InitRcv = ((initial_state(Cfg))#state.receiver)#receiver.rcv,
                        RcvPtr = alloc(InitRcv),
                        {H, S, Res} = run_commands(?MODULE,Cmds,[{rcvstate,RcvPtr}]),
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
    Struct -> Struct
  end.

increment(N) ->
  (N + 1) rem 15.



get_counter([_, CounterByte | _])->
  <<_:4, C:4>> = <<CounterByte>>,
  C.



flip(Bit, Data) ->
  <<Prefix:Bit,B:1,Tail/bitstring>> = list_to_binary(Data),
  binary_to_list(<< <<Prefix:Bit>>/bitstring, <<(1-B):1>>/bitstring, Tail/bitstring>>).




