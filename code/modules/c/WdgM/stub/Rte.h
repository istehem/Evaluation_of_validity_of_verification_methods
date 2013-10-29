#ifndef WDGM_SMALL
#include "Std_Types.h"
#include "Platform_Types.h"
#include "Compiler.h"
#else
#define RTE_STUB
#include "../gcc/Std_Types.h"
#include "../gcc/Platform_Types.h"
#include "../gcc/Compiler.h"
#endif

#ifdef RTE_STUB

typedef uint8 Rte_ModeType_WdgMMode;

FUNC(Std_ReturnType, RTE_CODE) Rte_Switch_WdgM_globalmode_currentMode(uint8 mode);
struct Rte_PDS_WdgM_WdgM_IndividualMode_P
{
   P2FUNC(Std_ReturnType, RTE_CODE, Switch_currentMode)(uint8 mode);
};

typedef struct Rte_CDS_WdgM_Tag
{
   /* Data Handles section. -----------------------*/
   /* Per-instance Memory Handles section. --------*/
   /* Inter-runnable Variable Handles section. ----*/
   /* Calibration Parameter Handles section. ------*/
   /* Exclusive-area API section. -----------------*/
   /* Port API section. ---------------------------*/
   struct Rte_PDS_WdgM_WdgM_IndividualMode_P mode000;
   struct Rte_PDS_WdgM_WdgM_IndividualMode_P mode001;
   struct Rte_PDS_WdgM_WdgM_IndividualMode_P mode002;
   struct Rte_PDS_WdgM_WdgM_IndividualMode_P mode003;
   struct Rte_PDS_WdgM_WdgM_IndividualMode_P mode004;
   /* Inter Runnable Variable API section. --------*/
   /* Inter Runnable Triggering API section. ------*/
   /* Vendor specific section. --------------------*/
} Rte_CDS_WdgM;

CONSTP2CONST(Rte_CDS_WdgM, RTE_CONST, RTE_APPL_CONST) Rte_Inst_WdgM;

#endif
