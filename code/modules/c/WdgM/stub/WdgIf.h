//typedef uint8 WdgIf_ModeType;

/* #define WDGIF_OFF_MODE 0 */
/* #define WDGIF_SLOW_MODE 1 */
/* #define WDGIF_FAST_MODE 2 */

typedef enum WdgIf_Mode_Tag
{
   WDGIF_OFF_MODE   = 0,
   WDGIF_SLOW_MODE  = 1,
   WDGIF_FAST_MODE  = 2
}WdgIf_ModeType;
typedef WdgIf_ModeType WdgM_WdgModeType;


/* FUNC(void,WDGIF_CODE) WdgIf_SetTriggerCondition /\* FROM WDGIF WdgIf.c *\/ */
/* ( */
/*    VAR(unsigned char,AUTOMATIC) DeviceIndex, */
/*    VAR(unsigned short int,AUTOMATIC) Timeout */
/* ) */
/* { */
/* /\*    VAR(boolean,AUTOMATIC) no_error = TRUE; *\/ */
/* /\*    WdgIfAssertWrongIdRange(WDGIF_TRIGGER_SID, (DeviceIndex >= (uint8)WDGIF_NUMBER_OF_DEVICES), no_error); *\/ */
/* /\*    if(TRUE == no_error) *\/ */
/* /\*    { *\/ */
/* /\* #if (1 == WDGIF_NUMBER_OF_DEVICES ) *\/ */
/* /\*       WdgIf_CallTrigger(Timeout); *\/ */
/* /\* #else *\/ */
/* /\*       /\\* PRQA S 3689 ++ *\/ */
/* /\*        * Range of DeviceIndex is checked in the above function WdgIfAssertWrongIdRange *\/ */
/* /\*        *\\/ *\/ */
/* /\*       (*WdgIf_TriggerFctPtr[DeviceIndex])(Timeout); *\/ */
/* /\*       /\\* PRQA S 3689 --*\\/ *\/ */
/* /\* #endif *\/ */
/* /\*    } *\/ */
/* /\*    else *\/ */
/* /\*    { *\/ */
/* /\*    } *\/ */
/* } */

/* FUNC(Std_ReturnType,WDGIF_CODE) WdgIf_SetMode /\* FROM WDGIF WdgIf.c *\/ */
/* ( */
/*    VAR(uint8,AUTOMATIC) DeviceIndex, */
/*    VAR(WdgIf_ModeType,AUTOMATIC) WdgMode */
/* ) */
/* { */
/*    Std_ReturnType status = E_OK; */
/* /\*    VAR(boolean,AUTOMATIC) no_error = TRUE; *\/ */

/* /\*    WdgIfAssertWrongIdRange(WDGIF_SETMODE_SID, (DeviceIndex >= (uint8)WDGIF_NUMBER_OF_DEVICES), no_error); *\/ */
/* /\*    if (TRUE == no_error) *\/ */
/* /\*    { *\/ */
/* /\* #if (1 == WDGIF_NUMBER_OF_DEVICES) *\/ */
/* /\*       status = WdgIf_CallSetMode(WdgMode); *\/ */
/* /\* #else *\/ */
/* /\*       /\\* PRQA S 3689 ++ *\/ */
/* /\*        * Range of DeviceIndex is checked in the above function WdgIfAssertWrongIdRange *\/ */
/* /\*        *\\/ *\/ */
/* /\*       status = (*WdgIf_ModeFctPtr[DeviceIndex])(WdgMode); *\/ */
/* /\*       /\\* PRQA S 3689 --*\\/ *\/ */
/* /\* #endif *\/ */
/* /\*    } *\/ */
/* /\*    else *\/ */
/* /\*    { *\/ */
/* /\*       status = E_NOT_OK; *\/ */
/* /\*    } *\/ */

/*    return status; */
/* } */
