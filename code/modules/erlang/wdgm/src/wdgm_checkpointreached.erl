-module(wdgm_checkpointreached).

-include("wdgm_config.hrl").

-compile([export_all]).

deadlinereached(S, SEid, CPid) ->
  DeadlineStart = lists:keyfind(CPid, 2, S#state.deadlineTable),
  DeadlineStop  = lists:keyfind(CPid, 3, S#state.deadlineTable),
  case {DeadlineStart, DeadlineStop} of
      {false, false} -> %% [WDGM299] ignore
        S;
      {_, false}     -> %% [WDGM228] start ids
      S#state{deadlineTable=lists:keyreplace(DeadlineStart#deadline.startCP,2,S#state.deadlineTable,
                       DeadlineStart#deadline{timestamp=DeadlineStart#deadline.timer + 1})};
      {false, _}     -> %% [WDGM229] stop ids
        Difference = DeadlineStop#deadline.timer-DeadlineStop#deadline.timestamp,
        SE = lists:keyfind(SEid, 2, S#state.supervisedentities),
        DeadlineMin = DeadlineStop#deadline.minmargin,
        DeadlineMax = DeadlineStop#deadline.maxmargin,
        Behaviour =
        case
          Difference =< DeadlineMax andalso
          Difference >= DeadlineMin andalso
          SE#supervisedentity.localdeadlinestatus == 'WDGM_CORRECT' %% dont destroy previous CP behaviour
          andalso DeadlineStop#deadline.timestamp /= 0 %% must be started before, [WDGM229]
        of
          true  -> 'WDGM_CORRECT'; %% [WDGM294]
          false -> 'WDGM_INCORRECT'
        end,
        UpdatedDeadlineTable = lists:keyreplace(CPid, 3, S#state.deadlineTable, DeadlineStop#deadline{timestamp=0}),
        NewSEs = lists:keyreplace(SEid, 2, S#state.supervisedentities, SE#supervisedentity{localdeadlinestatus=Behaviour}),
        S#state{deadlineTable=UpdatedDeadlineTable,
              supervisedentities=NewSEs}
  end.

logicalreached(S, SEid, CPid) ->
  IsGraphCP = [LR || LR <- S#state.logicalTable, LR /= [] andalso lists:member(CPid, LR#logical.cps_in_graph)],
  case IsGraphCP of
    []  -> S; %% [WDGM297]
    LRs -> %% [WDGM295]
      LR = hd(LRs),
      case verify_CP(LR, CPid) of
        'WDGM_CORRECT' -> %% [WDGM274], [WDGM252]
          ActivityFlag =
            case
              LR#logical.activity andalso % annars om man har grafer med samma
                                          % slutcheckpoint som startcheckpoint
                                          % så kommer den att fela
              lists:member(CPid, LR#logical.finalCPs)
            of
              true  -> false; %% [WDGM331]
              false -> true %% [WDGM332]
            end,
          S#state{logicalTable=
                    (S#state.logicalTable--
                       [LR])++
                    [LR#logical{storedCP=CPid, %% [WDGM246]
                                activity=ActivityFlag}]};
        'WDGM_INCORRECT' ->
          SE = lists:keyfind(SEid, 2, S#state.supervisedentities),
          case SE of
            false -> S;
            _ ->
              S#state{supervisedentities=
                        lists:keyreplace(SEid,
                                         2,
                                         S#state.supervisedentities,
                                         SE#supervisedentity{locallogicalstatus='WDGM_INCORRECT'})}
          end
      end
  end.

verify_CP(LogicalRec, CP) ->
  case LogicalRec#logical.activity of
    true  -> is_successor(LogicalRec, CP);
    false -> is_initial_CP(LogicalRec, CP)
  end.

is_successor(LogicalRec, CP) ->
  IsDestCPofStoredSource =
    lists:keyfind(CP,
                  2,
                  lists:filter(fun ({Source, _Dest}) ->
                                   Source == LogicalRec#logical.storedCP
                               end,
                               LogicalRec#logical.graph)),
  case IsDestCPofStoredSource of
    false -> 'WDGM_INCORRECT';
    _     -> 'WDGM_CORRECT'
  end.

is_initial_CP(LogicalRec, CP) ->
  case CP == LogicalRec#logical.initCP of
    true  -> 'WDGM_CORRECT';
    false -> 'WDGM_INCORRECT'
  end.


%%-Generator--------------------------------------------------------------------

%%=Logical======================================================================
%% gets the arguments for logical supervision to behave correctly when
%% checkpointreached is generated. It needs a proposed SEid and the list
%% of logical records.
%%
%% will return a list of SEid and a CPid and either found or nonefound
%% {found, SEid, CPid}     -> means there exists a transition with SE and CP
%% {nonefound, SEid, CPid} -> means that either the SE doesn't have a logical supervision
%%                            or there are no CPs of the SE(?)
get_args_given_LS(LRs, SEid) ->
  case LRs of
    undefined -> [{nonefound, SEid, CPid} || CPid <- wdgm_config_params:get_CPs_of_SE(SEid)];
    _ ->
      LogicalRecordsForSE =
        lists:usort(
          [ L
            || L  <- LRs,
               CP <- wdgm_config_params:get_CPs_of_SE(SEid),
               lists:member(CP, L#logical.cps_in_graph)]),
      case LogicalRecordsForSE of
        [] -> [{nonefound, SEid, CPid} || CPid <- wdgm_config_params:get_CPs_of_SE(SEid)];
        _  ->
          [case {L#logical.is_internal, L#logical.activity} of
             {true, true}   -> %% internal, started
               lists:map(fun (CPid) -> {found, SEid, CPid} end, get_next_cps(L));
             {true, false}  -> %% internal, not started
               [{found, SEid, L#logical.initCP}];
             {false, true}  -> %% external, started
               [{found, wdgm_config_params:get_SE_of_CP(CPid), CPid} || CPid <- get_next_cps(L)];
             {false, false} -> %% external, not started
               [{found, wdgm_config_params:get_SE_of_CP(CPid), CPid} || CPid <- lists:flatten([L#logical.initCP])]
           end
           || L <- LogicalRecordsForSE]
      end
  end.

%% returns a list of cps, that are the next in the graph to be visited
get_next_cps(LogicalRec) ->
  [element(2,G) || G <- LogicalRec#logical.graph,
                   element(1, G) == LogicalRec#logical.storedCP].

%%=Deadline=====================================================================

%% gets the arguments for deadline supervision to behave correctly when
%% checkpointreached is generated. It needs a proposed SEid, a proposed CPid and the list
%% of deadline records.
%%
%% there are 4 different return values
%% {mainfunction_needed, SEid, CPid} %% we cant do this checkpoint, because it will return WDGM_INCORRECT
%% {mustdo, SEid, CPid} %% we must do this checkpoint, otherwise next time
%%                         checkpointreached runs, it will return WDGM_INCORRECT
%% {maybe, SEid, CPid} %% it is a proposed checkpoint to be run, but it doesn't matter if mainfunction runs before
%% {nonefound, SEid, CPid} %% no checkpoint found. either there is no deadline records, or the CP isn't a valid CP
get_args_given_DS(DRs, SEid, CPid) ->
  case DRs of
    undefined ->
      [{nonefound, SEid, CP} || CP <- wdgm_config_params:get_CPs_of_SE(SEid)];
    _ ->
      DeadlineRecordsForSE =
        lists:usort(
          [ case
              {CP == D#deadline.startCP,
               (D#deadline.timer-D#deadline.timestamp) =< D#deadline.minmargin,
               (D#deadline.timer-D#deadline.timestamp) == D#deadline.maxmargin}
            of
              {false, false, _} -> {mainfunction_needed, SEid, CP};
              {false, _, true}  -> {mustdo, SEid, CP};
              _                 -> {maybe, SEid, CP}
            end
            || D  <- DRs,
               CP <- wdgm_config_params:get_CPs_of_SE(SEid),
               (CP == D#deadline.startCP andalso D#deadline.timestamp == 0)
                 orelse (CP == D#deadline.stopCP andalso D#deadline.timestamp /= 0)]),
      case DeadlineRecordsForSE of
        [] ->
          [{nonefound, SEid, CPid}];
        _ -> DeadlineRecordsForSE
      end
  end.


%%=Alive========================================================================

%% gets the arguments for alive supervision to behave correctly when
%% checkpointreached is generated. It needs a proposed SEid, a proposed CPid and the list
%% of alive records.
%%
%% there are 4 different return values
%% {mainfunction_needed, SEid, CPid} %% we cant do this checkpoint, because it will return WDGM_INCORRECT
%% {mustdo, SEid, CPid} %% we must do this checkpoint, otherwise next time
%%                         mainfunction runs, it will return WDGM_INCORRECT
%% {maybe, SEid, CPid} %% it is a proposed checkpoint to be run, but it doesn't matter if mainfunction runs before
%% {nonefound, SEid, CPid} %% no checkpoint found. either there is no alive records, or the CP isn't a valid CP
get_args_given_AS(ARs, SEid, CPid) ->
  case ARs of
    undefined ->
      [{nonefound, SEid, CP} || CP <- wdgm_config_params:get_CPs_of_SE(SEid)];
    _ ->
      AliveRecordsForSE =
        lists:usort(
          [ case
              {(A#alive.expected_alive_indications-A#alive.alive_counter) == A#alive.maxmargin,
               (A#alive.expected_alive_indications-A#alive.alive_counter) < A#alive.minmargin}
            of
              {true, _} -> {mainfunction_needed, SEid, CP};
              {_, true} -> {mustdo, SEid, CP};
              _         -> {maybe, SEid, CP}
            end
            || A  <- ARs,
               CP <- wdgm_config_params:get_CPs_of_SE(SEid),
               CP == A#alive.cpid]),
      case AliveRecordsForSE of
        [] ->
          [{nonefound, SEid, CPid}];
        _ -> AliveRecordsForSE
      end
  end.

%%=Validity of choosen SE and CP================================================

%% will take a proposed SE and CP and see if AS or DS wants to do it soon, or rather not
%% will return either
%% {prio, SE, CP}
%% {dont, SE, CP}
%% {maybe, SE, CP}
prioritize(S, ProposedSE, ProposedCP) ->
  DScheckpoints = wdgm_checkpointreached:get_args_given_DS(S#state.deadlineTable, ProposedSE, ProposedCP),
  AScheckpoints = wdgm_checkpointreached:get_args_given_AS(S#state.aliveTable, ProposedSE, ProposedCP),
  ASDSTable = DScheckpoints ++ AScheckpoints,
  case
    {lists:member({mustdo             , ProposedSE, ProposedCP}, ASDSTable),
     lists:member({mainfunction_needed, ProposedSE, ProposedCP}, ASDSTable)}
  of
    {true, _} -> {prio , ProposedSE, ProposedCP};
    {_, true} -> {dont , ProposedSE, ProposedCP};
    _         -> {maybe, ProposedSE, ProposedCP}
  end.

%% could be potentially dangerous
%% returns a SE and a CP
choose_SE_and_CP(S, LCPs) ->
  PrioritizedCPs =
    [prioritize(S, ProposedSE, ProposedCP)
     || {found, ProposedSE, ProposedCP} <- lists:flatten(LCPs)],
  case PrioritizedCPs of
    []                  -> [999,999]; %% waaat?
    _  ->
      case lists:keyfind(prio, 1, PrioritizedCPs) of
        false ->
          case lists:keyfind(maybe, 1, PrioritizedCPs) of
            false       -> [999,999]; %% mainfunction really needed
            {_, SE, CP} -> [SE, CP]
          end;
        {_, SE, CP}     -> [SE, CP]
      end
  end.
