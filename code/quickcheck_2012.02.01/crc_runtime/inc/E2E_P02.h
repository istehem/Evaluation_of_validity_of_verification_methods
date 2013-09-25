#ifndef E2E_P02_H
#define E2E_P02_H

/**\file
 *
 * \brief E2E Profile 2 Header File for Picea.
 *
 *    This file describes the End to End SW protection Library module
 *    for Picea.\n
 *
 * \b Application:        E2E_P02.h \n
 * \b Target:             N.A. \n
 * \b Compiler:           N.A. \n
 * \b Autosar-Vendor-ID:  41 \n
 * \n
 * \b Module:             E2E_P02.h \n
 * \b File-Revision: %version: 5 %\n
 *
 * \b Changeable-by-user: No \n
 * \b Delivery-File:      Yes \n
 * \n
 * \b Module-Owner:       Picea Team \n
 * \b Location:           Mecel \n
 * \b Phone:              +46 31 720 44 00 \n
 * \b E-Mail:             autosoft(at)mecel.se \n
 * \n
 * \b Traceability-Info   AUTOSAR_SWS_E2ELibrary V2.0.0 \n
 * \b Classification:     Not classified \n
 * \b Deviations:         None \n
 * \n
 * \b Requirement(s):
 * \Requirements -->
 * \latexonly
 * \reqtrace{DSAR-SDD-E2E-3-R1}{E2E0219, E2E0117, E2E0118, E2E0119, E2E0120, E2E0121,
 *                    E2E0122, E2E0123, E2E0124, E2E0125, E2E0126, E2E0127, E2E0128,
 *                    E2E0129, E2E0130, E2E0132, E2E0133, E2E0134, E2E0135, E2E0136,
 *                    E2E0137, E2E0138, E2E0139, E2E0140, E2E0141, E2E0142, E2E0143,
 *                    E2E0145, E2E0146, E2E0147, E2E0148, E2E0149, E2E0150, E2E0151,
 *                    E2E0152, E2E0153, E2E0154, E2E0214, E2E0160, E2E0161, E2E0010,
 *                    E2E0017, E2E0018, E2E0020, E2E0021, E2E0022, E2E0221, E2E0217,
 *                    E2E0047, E2E0012, E2E0106, E2E0107, E2E0049, E2E0011}
 *
 * \endlatexonly
 */

/*============================================================================*
*
* Copyright 2012 Mecel Technologies, Inc. All Rights Reserved
*
*============================================================================*/

/* INCLUDE DIRECTIVES FOR OTHER HEADERS -------------------------------------- */

#include "E2E.h"

/* ENUMS --------------------------------------------------------------------- */

/* Enum for Configuration types---------------------------------------------- */

typedef struct E2E_P02ConfigType_Tag
{
   uint16 DataLength;
   uint8 DataIDList[16];
   uint8 MaxDeltaCounterInit;
}E2E_P02ConfigType;

/* Enums for Sender and Receiver types--------------------------------------- */

typedef struct E2E_P02SenderStateType_Tag
{
   uint8 Counter;
} E2E_P02SenderStateType;

typedef enum E2E_P02ReceiverStatusType_Tag
{
   E2E_P02STATUS_OK = 0x00,
   E2E_P02STATUS_NONEWDATA = 0x01,
   E2E_P02STATUS_WRONGCRC = 0x02,
   E2E_P02STATUS_INITAL = 0x04,
   E2E_P02STATUS_REPEATED = 0x08,
   E2E_P02STATUS_OKSOMELOST = 0x20,
   E2E_P02STATUS_WRONGSEQUENCE = 0x40
} E2E_P02ReceiverStatusType;

/* STRUCTURES ---------------------------------------------------------------- */

typedef struct
{
   uint8 LastValidCounter;
   uint8 MaxDeltaCounter;
   boolean WaitForFirstData;
   boolean NewDataAvailable;
   uint8 LostData;
   E2E_P02ReceiverStatusType Status;
} E2E_P02ReceiverStateType;

/**
 * Mark start of memory area for code
 */
#define E2E_START_SEC_CODE
#include "MemMap.h"

/**
 * \brief E2E_P02Protect
 *
 *
 * The function shall get the config type, state type and Data Pointer as function
 * parameters. This function writes the counter value in the Data pointer location
 * depending on the counter offset value received as function parameter. Finally CRC is
 * computed on the Data ID and the Data by calling the function E2E_CalculateCrc().
 * Finally the calculated CRC is written in Data pointer location.
 *
 * \remark Sync/Async:      Synchronous
 * \remark Reentrancy:      Non-Reentrant
 * \remark Caveats:         -
 * \remark Configuration:   NA
 *
 * \param pState (in)        A pointer pointing to the counter value for the sender side.
 *                          This parameter on the sender side, for the first transmission
 *                            request of a data element the counter shall be initialized
 *                            with 0 and shall be incremented by 1 for every subsequent
 *                            send request.
 * \param pConfig (in)       A pointer to a structure with Data Type related info:
 *                          Gives data on Data Id List,Data length and MaxDeltaCounterInit.
 * \param pData (in)         A pointer to a Data buffer on which CRC shall be calculated.
 * \retval E2E_E_OK         The request call was successful
 *         E2E_E_INPUTERR_NULL The request had a NULL POINTER.
 */

extern FUNC(Std_ReturnType,E2E_CODE) E2E_P02Protect(
   P2CONST(E2E_P02ConfigType, AUTOMATIC, E2E_APPL_DATA) pConfig,
   P2VAR(E2E_P02SenderStateType, AUTOMATIC, E2E_APPL_DATA) pState,
   P2VAR(uint8, AUTOMATIC, E2E_APPL_DATA) pData );

/**
 * \brief E2E_P02Check
 *
 * The function E2E_P02Check shall check the Counter and CRC of the received Data and
 * determine the check Status.
 *
 * The received CRC and calculated CRC over the Data ID and Data  are checked.
 * The received Counter is used to check the status of the Data by comparing
 * with the LastvalidCounter.
 *
 * \remark Sync/Async:      Synchronous
 * \remark Reentrancy:      Non-Reentrant
 * \remark Caveats:         -
 * \remark Configuration:   NA
 *
 * \param pState (in)        A pointer pointing to the counter value for the receiver side.
 *                          This parameter on the receiver side,the counter value of received
 *                          data is validated against the counter of previously received data.
 * \param pConfig (in)        A pointer to a structure with Data Type related info:
 *                          Gives data on Data Id List,Data length and MaxDeltaCounterInit.
 * \param pData (in)         A pointer to a Data buffer on which CRC shall be checked.
 * \retval E2E_E_OK         The request call was successful
 *         E2E_E_INPUTERR_NULL The request had a NULL POINTER.
 */

extern FUNC(Std_ReturnType,E2E_CODE)  E2E_P02Check(
   P2CONST(E2E_P02ConfigType, AUTOMATIC, E2E_APPL_DATA) pConfig,
   P2VAR(E2E_P02ReceiverStateType, AUTOMATIC, E2E_APPL_DATA) pState,
   P2CONST(uint8, AUTOMATIC, E2E_APPL_DATA) pData );

/**
 * Mark end of memory area for code
 */
/* PRQA S 5087 ++
 * MISRA RULE C197 VIOLATION:
 */
#define  E2E_STOP_SEC_CODE
#include "MemMap.h"
/* PRQA S 5087 -- */

#endif   /* #ifdef _E2E_P02_H */
/* END OF FILE -------------------------------------------------------------- */
