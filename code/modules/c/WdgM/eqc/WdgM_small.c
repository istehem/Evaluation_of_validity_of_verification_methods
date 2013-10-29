#define FUNC(rettype, memclass) rettype
#define P2CONST(ptrtype, memclass, ptrclass) const ptrtype *
#define P2FUNC(rettype, ptrclass, fctname) rettype (*fctname)
#define VAR(type, memclass) type
#define P2VAR(ptrtype, memclass, ptrclass) /* PRQA S 3409 */ ptrtype *
#define CONST(type, memclass) /* PRQA S 3409 */ const type
#define CONSTP2CONST(ptrtype, memclass, ptrclass) /* PRQA S 3409 */ const ptrtype * const
#define MAX_NO_HASH_CONFIGS   2
#define E_OK     0x00
#define TRUE        1

// typedef unsigned char uint8;
// typedef unsigned short int uint16;
// typedef unsigned int uint32;
// typedef unsigned char boolean;

#define WDGM_SMALL

#include "../stub/SchM_WdgM.h"
#include "../stub/Det.h"
#include "../stub/Dem.h"
#include "../stub/Rte.h"
#include "../stub/Mcu.h"
#include "../stub/WdgIf.h"

typedef uint16   WdgM_CheckpointIdType;
typedef uint16   WdgM_SupervisedEntityIdType;

typedef enum WdgM_ActivationStatus_Tag
{
   WDGM_SUPERVISED_ENTITY_DEACTVATED,
   WDGM_SUPERVISED_ENTITY_ACTVATED
}WdgM_ActivationStatusType;

typedef struct WdgM_Trigger_Tag
{
   uint16 triggerReferenceCycle;  /**< Trigger Condition Value */
   uint8 wdgId;                  /**< Watchdog Id */
   WdgM_WdgModeType mode;        /**< Watchdog mode */
}WdgM_TriggerType;

typedef struct WdgM_ExternalTransition_Tag
{
   WdgM_CheckpointIdType externalSourceCheckpointId;  /**< Source Checkpoint Id in an external transition */
   WdgM_CheckpointIdType externalDestCheckpointId;    /**< Destination Checkpoint Id in an external transition */
} WdgM_ExternalTransitionType;

typedef struct WdgM_Internaltransition_Tag
{
   WdgM_CheckpointIdType internalSourceCheckpointId;  /**< Source Checkpoint Id in internal transition */
   WdgM_CheckpointIdType internalDeastCheckpointId;   /**< Destination Checkpoint Id in internal transition */
}WdgM_InternaltransitionType;

typedef struct WdgM_LocalStatusParms_Tag
{
   uint8 failed_SupervisionRefCycleTolerance;                     /**< Tolerance for failed supervision reference cycles */
   WdgM_SupervisedEntityIdType supervisedEntityId;                /**< Supervised Entity Id */
   WdgM_CheckpointIdType internalTransitionInitialCheckpointId;   /**< Initial checkpoint Id in internal logical supervision */
   uint8 finalCheckpointCnt;                                      /**< The no. of final checkpoints in internal logical supervision */
   P2CONST(WdgM_CheckpointIdType,AUTOMATIC,WDGM_APPL_DATA) internalTransitionFinalCheckpointId;    /**< Pointer to internal transition final checkpoint Ids*/
   uint16 internalTransCnt;                                       /**< The no. of transitions in internal logical supervision */
   P2CONST(WdgM_InternaltransitionType,AUTOMATIC,WDGM_APPL_CONST) internalTransitionTabPtr;        /**< Pointer to the transitions in internal logical supervision */
   WdgM_ActivationStatusType activationStatus;                    /**< Activation status of supervised entity */
   WdgM_CheckpointIdType entityInitialCheckpointId;               /**< Initial Checkpoint Id of the Supervised Entity */
   WdgM_CheckpointIdType entityFinalCheckpointId;                 /**< Final Checkpoint Id of the Supervised Entity*/
}WdgM_LocalStatusParmsType;

typedef struct WdgM_AliveSupervision_Tag
{
   uint8 expectedAliveIndications;     /**< The no. of Alive indications expected for a checkpoint */
   uint8 supervisionReferenceCycle;    /**< The amount of Supervision Cycles to be used as reference by the Alive Supervision */
   uint8 minMargin;                    /**< Minimum limit of tolerance for Alive Indications */
   uint8 maxMargin;                    /**< Maximum limit of tolerance for Alive Indications */
   WdgM_CheckpointIdType checkpointId; /**< Checkpoint Id */
   WdgM_SupervisedEntityIdType aliveSupervisedEntityId;  /**< Supervised Entity Id for Alive Supervision */
}WdgM_AliveSupervisionType;

typedef struct WdgM_DeadlineSupervision_Tag
{
   uint32 deadlineMin;     /**< shortest time span for meeting deadline */
   uint32 deadlineMax;     /**< longest time span for meeting deadline */
   WdgM_CheckpointIdType deadlineStartCheckpointId;         /**< Start Checkpoint Id for Deadline supervision */
   WdgM_CheckpointIdType deadlineStopCheckpointId;          /**< Stop Checkpoint Id for Deadline supervision  */
   WdgM_SupervisedEntityIdType deadlineSupervisedEntityId;  /**< Supervised Entity Id for Deadline Supervision*/
}WdgM_DeadlineSupervisionType;

typedef struct WdgM_ExternalLogicalSupervision_Tag
{
   uint8 externalGraphIndex;        /**< The index of the external graph */
   uint16 initialCheckpointIdCnt;   /**< The no. of initial checkpoints in external logical supervision */
   P2CONST(WdgM_CheckpointIdType,AUTOMATIC,WDGM_APPL_DATA) externalInitialCheckpointId;      /**< Pointer to external logical supervision initial checkpoint Ids */
   uint16 finalCheckpointIdCnt;     /**< The no. of final checkpoints in external logical supervision */
   P2CONST(WdgM_CheckpointIdType,AUTOMATIC,WDGM_APPL_DATA) externalFinalCheckpointId;        /**< Pointer to external logical supervision final checkpoint Ids */
   uint16 externalTransCnt;         /**< The no. of transitions in external logical supervision */
   P2CONST(WdgM_ExternalTransitionType,AUTOMATIC,WDGM_APPL_CONST) externalTransitionTabPtr;  /**< Pointer to the transitions in external logical supervision */
}WdgM_ExternalLogicalSupervisionType;

typedef struct WdgM_ModeInfo_Tag
{
   uint8 expiredSupervisionCycleTolerance;   /**< Amount of Expired Supervision Cycle */
   uint8 wdgmTriggerCount;                   /**< No. of watchdogs */
   uint16 aliveSupervisionCount;             /**< No. of Alive supervisions */
   uint16 deadlineSupervisionCount;          /**< No. of Deadline supervisions */
   uint16 externalLogicalSupervisionCount;   /**< No. of External logical supervisions */
   uint16 localStatusParmCount;              /**< No. of local status parameter */
   P2CONST(WdgM_AliveSupervisionType,AUTOMATIC,WDGM_APPL_CONST) aliveSupervisionTablePtr;       /**< Pointer to Alive Supervision parameters */
   P2CONST(WdgM_DeadlineSupervisionType,AUTOMATIC,WDGM_APPL_CONST) deadlineSupervisionTablePtr; /**< Pointer to Deadline Supervision parameters */
   P2CONST(WdgM_ExternalLogicalSupervisionType,AUTOMATIC,WDGM_APPL_CONST) externalLogicalSupervisionTablePtr;  /**< Pointer to External logical Supervision parameters */
   P2CONST(WdgM_LocalStatusParmsType,AUTOMATIC,WDGM_APPL_CONST) localStatusParmTablePtr;        /**< Pointer to Local status parameters */
   P2CONST(WdgM_TriggerType,AUTOMATIC,WDGM_APPL_CONST) wdgmTriggerTableptr;                     /**< Pointer to WdgM Triggers */
}WdgM_ModeInfoType;

typedef struct WdgM_Config_Tag
{
   uint8 initialMode;      /**< Initial mode of WdgM*/
   uint8 wdgmModeCount;    /**< No. of modes in WdgM*/
   P2CONST(WdgM_ModeInfoType,AUTOMATIC,WDGM_APPL_CONST) wdgmModeTablePtr;  /**< Pointer to mode parameters */
}WdgM_ConfigType;

#include "../stub/SchM_WdgM.c"
#include "../stub/Dem.c"
#include "../stub/Mcu.c"
#include "../stub/WdgIf.c"
#include "../stub/Rte.c"
#include "../stub/Det.c"

void WdgM_Init(const WdgM_ConfigType* ConfigPtr);
static FUNC(void,WDGM_CODE) WdgM_CalculeteGlobalStatus(void);
