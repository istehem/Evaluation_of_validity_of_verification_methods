#include "Std_Types.h"
#include "Rte_WdgM.h"

FUNC(Std_ReturnType, RTE_CODE) Rte_Switch_WdgM_globalmode_currentMode(uint8 mode)
{
  return E_OK;
}

CONSTP2CONST(Rte_CDS_WdgM, RTE_CONST, RTE_APPL_CONST) Rte_Inst_WdgM = 0;

//const Rte_CDS_WdgM * const Rte_Inst_WdgM;
