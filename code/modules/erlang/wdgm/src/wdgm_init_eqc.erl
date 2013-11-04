-module(wdgm_init_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

-include("wdgm_config.hrl").
-define(C_CODE, wdgm_wrapper).
-include_lib("../ebin/wdgm_wrapper.hrl").

%-record('SupervisedEntityMonitorTable', {supervision_entities=[]}). %% [{status, {logicalS, deadlineS, aliveS}}]
-record(state, {initialized, currentMode}).

initial_state() ->
%  wdgm_xml:start(),
  #state{initialized=false, currentMode=#tst_cfg1.initial_mode_id}.

%% -WdgM_Init-------------------------------------------------------------------

wdgm_init_command(_S) ->
  {call, ?MODULE, wdgm_init, [eqc_c:address_of('Tst_Cfg1')]}.

wdgm_init(Ptr) ->
  ?C_CODE:'WdgM_Init'(Ptr).

wdgm_init_next(S, _Ret, _Args) ->
  S#state{initialized=true}.

wdgm_init_post(_S, _Args, _Ret) ->
  check_supervisionstatus(eqc_c:value_of('WdgM_SupervisedEntityMonitorTable')) andalso
    eqc_c:value_of('WdgM_GlobalStatus') == 'WDGM_GLOBAL_STATUS_OK' andalso
    eqc_c:value_of('WdgM_CurrentMode') == #tst_cfg1.initial_mode_id.
%% andalso
%%    additional checks if wdgmdeverrordetect is enabled

%% -WdgM_GetMode----------------------------------------------------------------

get_mode_pre(_S) ->
  true.
%%  S#state.initialized == true.

get_mode_command(_S) ->
  {call, ?MODULE, get_mode, []}.

get_mode() ->
  Mp = eqc_c:alloc("uint8"),
  R = ?C_CODE:'WdgM_GetMode'(Mp),
  {R,eqc_c:deref(Mp)}.

get_mode_post(S, _Args, {R, Mode}) ->
  (R == 0 andalso Mode == S#state.currentMode)
    orelse R == 1.
%%    additional checks if wdgmdeverrordetect is enabled

%% -WdgM_SetMode----------------------------------------------------------------

set_mode_pre(_S) ->
  eqc_c:value_of('WdgM_GlobalStatus') == 'WDGM_GLOBAL_STATUS_OK' orelse
    eqc_c:value_of('WdgM_GlobalStatus') == 'WDGM_GLOBAL_STATUS_FAILED'.
%%  S#state.initialized == true.

set_mode_command(_S) ->
  {call, ?MODULE, set_mode, [choose(0,2), oneof(#wdgmgeneral.caller_ids)]}.
%% Available modes: WDGIF_OFF_MODE:0, WDGIF_SLOW_MODE:1, WDGIF_FAST_MODE:2

set_mode(UI8_mode,UI16_callerId) ->
        ?C_CODE:'WdgM_SetMode'(UI8_mode,UI16_callerId).

set_mode_post(S, [M, _Cid], Ret) ->
  case Ret of
    0 -> M == eqc_c:value_of('WdgM_CurrentMode');
    1 -> M == S#state.currentMode
  end.

set_mode_next(S, Ret, [M, _Cid]) ->
  case Ret of
    0 -> S#state{currentMode = M};
    _ -> S
  end.



%% -Helper-functions------------------------------------------------------------

check_supervisionstatus([]) -> true;
check_supervisionstatus([L|Ls]) ->
  case L#'WdgM_SupervisedEntityMonitor_Tag'.supervision_status of
    'WDGM_LOCAL_STATUS_OK' ->
      check_supervision_results(L) andalso check_supervisionstatus(Ls);
    'WDGM_LOCAL_STATUS_DEACTIVATED' ->
      check_supervisionstatus(Ls)
  end.

check_supervision_results({_, _, 'WDGM_LOCAL_STATUS_OK', _, 'WDGM_CORRECT', 'WDGM_CORRECT', 'WDGM_CORRECT'}) ->
  true;
check_supervision_results(_) ->
  false.


%% -Properties------------------------------------------------------------------

prop_wdgm_init() ->
  ?SETUP( fun () -> start(),
		    fun () -> ok end
	  end,
	  ?FORALL(Cmds, commands(?MODULE),
		  begin
		    {H,S,Res} = run_commands(?MODULE,Cmds),
		    pretty_commands(?MODULE,Cmds,{H,S,Res},
				    aggregate(command_names(Cmds),
					      Res == ok))
		  end)).

start () ->
  wdgm_eqc:start().
