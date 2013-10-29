-module(tst_wdgm).
-compile(export_all).

-type uint8() :: non_neg_integer().
-type uint16() :: non_neg_integer().
-type uint32() :: non_neg_integer().

-type wdgM_ModeType() :: uint8().
-type wdgM_CheckpointIdType() :: uint16().
-type wdgM_SupervisedEntityIdType() :: uint16().

-record('WdgM_ConfigType', {initialMode :: uint8(),
                            wdgmModeCount :: uint8(),
                            wdgmModeTablePtr
                           }
       ).

-record('WdgM_AliveSupervisionType',
        {
         expectedAliveIndications :: uint8(),
         supervisionReferenceCycle :: uint8(),
         minMargin :: uint8(),
         maxMargin :: uint8(),
         checkpointId :: wdgM_CheckpointIdType(),
         aliveSupervisedEntityId :: wdgM_SupervisedEntityIdType()
        }
       ).

-record('WdgM_DeadlineSupervisionType',
        {
         deadlineMin :: uint32(),
         deadlineMax :: uint32(),
         deadlineStartCheckpointId :: wdgM_CheckpointIdType(),
         deadlineStopCheckpointId :: wdgM_CheckpointIdType(),
         deadlineSupervisedEntityId :: wdgM_SupervisedEntityIdType()
        }
       ).

-record('WdgM_ModeInfoType',
        {
         expiredSupervisionCycleTolerance :: uint8(),
         wdgmTriggerCount :: uint8(),
         aliveSupervisionCount :: uint16(),
         deadlineSupervisionCount :: uint16(),
         externalLogicalSupervisionCount :: uint16(),
         localStatusParmCount :: uint16(),
         aliveSupervisionTablePtr,
         deadlineSupervisionTablePtr,
         externalLogicalSupervisionTablePtr,
         localStatusParmTablePtr,
         wdgmTriggerTableptr
        }).

-record('WdgM_ExternalLogicalSupervision_Tag',
        {
         externalGraphIndex :: uint8(),
         initialCheckpointIdCnt :: uint16(),
         externalInitialCheckpointId, %Yet anouther fucking struct
         finalCheckpointIdCnt :: uint16(),
         externalFinalCheckpointId, %Yet anouther fucking struct
         externalTransCnt :: uint16(),
         externalTransitionTabPtr %Yet anouther fucking struct
        }).

aliveSupervisionTablePtr() ->
        eqc_c:store(
        aliveSupervisionTablePtr,
        #'WdgM_AliveSupervisionType'
        {
         expectedAliveIndications=0,
         supervisionReferenceCycle=0,
         minMargin=0,
         maxMargin=0,
         checkpointId=0,
         aliveSupervisedEntityId=0
        }
        ).

deadlineSupervisionTablePtr() ->
        eqc_c:store(
        deadlineSupervisionTablePtr,
        #'WdgM_DeadlineSupervisionType'
        {
         deadlineMin = 0,
         deadlineMax = 0,
         deadlineStartCheckpointId = 0,
         deadlineStopCheckpointId = 0,
         deadlineSupervisedEntityId = 0
        }
        ).

externalLogicalSupervisionTablePtr() -> undefined.

wdgmModeTablePtr() -> eqc_c:store(
                      wdgmModeTablePtr,
                      #'WdgM_ModeInfoType'
                      {
                        expiredSupervisionCycleTolerance=0,
                        wdgmTriggerCount=0,
                        aliveSupervisionCount=0,
                        deadlineSupervisionCount=0,
                        externalLogicalSupervisionCount=0,
                        localStatusParmCount=0,
                        aliveSupervisionTablePtr=aliveSupervisionTablePtr(),
                        deadlineSupervisionTablePtr=deadlineSupervisionTablePtr(),
                        externalLogicalSupervisionTablePtr=externalLogicalSupervisionTablePtr(),
                        localStatusParmTablePtr=0,
                        wdgmTriggerTableptr=0
                      }).

config_ptr() -> eqc_c:store('ConfigPtr',
                            #'WdgM_ConfigType'
                            {
                             initialMode=0,
                             wdgmModeCount=2,
                             wdgmModeTablePtr=wdgmModeTablePtr()
                            }).

start() ->
   eqc_c:start(trams,
               [{c_src, "eqc/WdgM_small.c"},
                {additional_files,["out/WdgM.o","out/WdgM_Lcfg.o","out/WdgM_Pbcfg.o"]}
               ]).


