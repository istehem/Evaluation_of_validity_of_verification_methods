-module(wdgm_init_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

-include("wdgm_config.hrl").
-define(C_CODE, wdgm_wrapper).
-include_lib("wdgm_wrapper.hrl").

-record('SupervisedEntityMonitorTable', {supervision_entities=[]}). %% [{status, {logicalS, deadlineS, aliveS}}]

initial_state() ->
  wdgm_xml:start(),
  #'SupervisedEntityMonitorTable'{}.

%% -WdgM_Init-------------------------------------------------------------------

wdgmInit_command(_S) ->
  {call, ?C_CODE, 'WdgM_Init', [eqc_c:address_of('Tst_Cfg1')]}.

wdgmInit_next(S, _Ret, _Args) ->
  S#'SupervisedEntityMonitorTable'{supervision_entities = [a]}.

wdgmInit_post(S, _Args, _Ret) ->
  length(S#'SupervisedEntityMonitorTable'.supervision_entities) > 3.
%%check_supervisionstatus(eqc_c:value_of('WdgM_SupervisedEntityMonitorTable')) andalso
%%    eqc_c:value_of('WdgM_GlobalStatus') == 'WDGM_GLOBAL_STATUS_OK' andalso
%%    eqc_c:value_of('WdgM_CurrentMode') == #tst_cfg1.initial_mode_id andalso
%%    additional checks if wdgmdeverrordetect is enabled

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
%		    eqc_c:restart(),
		    {H,S,Res} = run_commands(?MODULE,Cmds),
		    pretty_commands(?MODULE,Cmds,{H,S,Res},
				    aggregate(command_names(Cmds),
					      Res == ok))
		  end)).

start () ->
  wdgm_eqc:start().
