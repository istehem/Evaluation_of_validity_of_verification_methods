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

%% lista med CPs tillhörande SE
%% lista med logicalrecords
%%-----
%% lista med logicalrecords för SEn
get_args_given_LS(LRs, SEid) ->
  case LRs of
    undefined -> [{SEid, CPid} || CPid <- wdgm_config_params:get_CPs_of_SE(SEid)];
    _ ->
      LogicalRecordsForSE =
        lists:usort(
          [ L
            || L   <- LRs,
               CPs <- wdgm_config_params:get_CPs_of_SE(SEid),
               lists:member(CPs, L#logical.cps_in_graph)]),
      [case {L#logical.is_internal, L#logical.activity} of
         {true, true}   -> %% internal, started
           lists:map(fun (Elem) -> {SEid, Elem} end, get_next_cps(L));
         {true, false}  -> %% internal, not started
           [{SEid, L#logical.initCP}];
         {false, true}  -> %% external, started
           [{wdgm_config_params:get_SE_of_CP(CPid), CPid} || CPid <- get_next_cps(L)];
         {false, false} -> %% external, not started
           [{wdgm_config_params:get_SE_of_CP(CPid), CPid} || CPid <- lists:flatten([L#logical.initCP])]
       end
       || L <- LogicalRecordsForSE]
  end.

get_next_cps(LogicalRec) ->
  [element(2,G) || G <- LogicalRec#logical.graph,
                   element(1, G) == LogicalRec#logical.storedCP].
