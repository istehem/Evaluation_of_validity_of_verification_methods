-module(e2e_eqc).

-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

-include("e2e.hrl").

uint16() -> choose(0,16#FFFF).
uint8() -> choose(0,16#FF).

e2e_start() -> eqc_c:start(e2e,[verbose,{c_src,"e2e_stub.c"},{cppflags, " -Isrc -Istd -Iinc"},{additional_files, ["E2E.o","E2E_P01.o","E2E_P02.o","Crc.o"]}]).

e2e_ConfigType() -> 
   #'E2E_P01ConfigType_Tag'{
      'DataLength'=?LET(N,choose(0,30),N*8),
      'DataID'=uint16(), 
      'DataIDMode'=elements(['E2E_P01_DATAID_BOTH', 'E2E_P01_DATAID_ALT', 'E2E_P01_DATAID_LOW']),
      'MaxDeltaCounterInit'=uint8(), 
      'CRCOffset'=0, 
      'CounterOffset'=8}.

e2e_SenderStateType() ->
   #'E2E_P01SenderStateType_Tag'{
      'Counter'=choose(0, 14)}.


prop_protect() ->
   ?FORALL({Config, State}, {e2e_ConfigType(), e2e_SenderStateType()},
      ?FORALL(Data, vector(Config#'E2E_P01ConfigType_Tag'.'DataLength'/8,uint8()), 
         begin
            CfgPtr = eqc_c:alloc("E2E_P01ConfigType", Config),
            StatePtr = eqc_c:alloc("E2E_P01SenderStateType", State),
            DataPtr = eqc_c:create_array("uint8", Data),
            0 == e2e:'E2E_P01Protect'(CfgPtr, StatePtr, DataPtr)
         end)).

