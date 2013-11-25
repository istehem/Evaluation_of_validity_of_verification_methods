-module(wdgm_checkpointreached).

-include("wdgm_config.hrl").

-compile([export_all]).

deadlinereached(S, SEid, CPid) ->
  DeadlineStart = lists:keyfind(CPid, 2, S#state.deadlineTable),
  DeadlineStop  = lists:keyfind(CPid, 3, S#state.deadlineTable),
  {NewDeadline, Difference} =
    case {DeadlineStart, DeadlineStop} of
      {false, false} -> %% [WDGM299] ignore
        {false, 0};
      {_, false}     -> %% [WDGM228] start ids
        {DeadlineStart#deadline{timestamp=DeadlineStart#deadline.timer}, 0};
      {false, _}     -> %% [WDGM229] stop ids
        {DeadlineStop#deadline{timestamp=0},
         DeadlineStop#deadline.timer-DeadlineStop#deadline.timestamp}
    end,
  SE = lists:keyfind(SEid, 2, S#state.supervisedentities),
  case NewDeadline == false of
    true -> S;
    _    ->
      DeadlineMin = NewDeadline#deadline.minmargin,
      DeadlineMax = NewDeadline#deadline.maxmargin,
      Behaviour =
        case
          Difference =< DeadlineMax andalso
          Difference >= DeadlineMin andalso
          SE#supervisedentity.localdeadlinestatus == 'WDGM_CORRECT' %% dont destroy previous CP behaviour
        of
          true  -> 'WDGM_CORRECT'; %% [WDGM294]
          false -> 'WDGM_INCORRECT'
        end,
      %% one of these will not update the state
      NewDeadlineTable     = lists:keyreplace(CPid, 2, S#state.deadlineTable, NewDeadline),
      UpdatedDeadlineTable = lists:keyreplace(CPid, 3, NewDeadlineTable, NewDeadline),
      NewSEs = lists:keyreplace(SEid, 2, S#state.supervisedentities, SE#supervisedentity{localdeadlinestatus=Behaviour}),
      S#state{deadlineTable=UpdatedDeadlineTable,
              supervisedentities=NewSEs}
  end.

logicalreached(S, SEid, CPid) ->
  IsGraphCP = [LR || LR <- S#state.logicalTable, lists:member(CPid, LR#logical.cps_in_graph)],
  case IsGraphCP of
    []  -> S; %% [WDGM297]
    LRs -> %% [WDGM295]
      LR = hd(LRs),
      case verify_CP(LR, CPid) of
        'WDGM_CORRECT' -> %% [WDGM274], [WDGM252]
          ActivityFlag =
            case lists:member(CPid, LR#logical.finalCPs) of
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
    true  -> 'WDGM_CORRECT';
    false -> 'WDGM_INCORRECT'
  end.

is_initial_CP(LogicalRec, CP) ->
  case CP == LogicalRec#logical.initCP of
    true  -> 'WDGM_CORRECT';
    false -> 'WDGM_INCORRECT'
  end.
