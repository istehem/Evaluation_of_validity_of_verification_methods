/**\file
 *
 * \brief Implementation file for Picea E2E Module - Profile 2
 *
 *    This file contains the implementation of the AUTOSAR basic sw module
 *     End to End Software Protection Library.\n
 *
 * \b Application:        E2E Library \n
 * \b Target:             see E2E_P02.h\n
 * \b Compiler:           see E2E_P02.h\n
 * \b Autosar-Vendor-ID:  see E2E_P02.h\n
 * \n
 * \b Module:             E2E_P02.c \n
 * \b File-Revision: %version: 4.1.2 %\n
 * \b Changeable-by-user: No \n
 * \b Delivery-File:      Yes \n
 * \n
 * \b Module-Owner:       see E2E_P02.h \n
 * \b Location:           see E2E_P02.h \n
 * \b Phone:              see E2E_P02.h \n
 * \b E-Mail:             see E2E_P02.h \n
 * \n
 * \b Traceability-Info   see E2E_P02.h \n
 * \b Classification:     see E2E_P02.h \n
 * \b Deviations:         see E2E_P02.h \n
 * \n
 * \b Requirement(s):
 * \latexonly
 * \endlatexonly
 */

/*============================================================================*
*
* Copyright 2012 Mecel Technologies, Inc. All Rights Reserved
*
*============================================================================*/

/*============================================================================*
* PREPROCESSOR DIRECTIVES
*============================================================================*/
/* INCLUDE HEADERS -------------------------------------- */
#include "E2E.h"

/* MACROS ----------------------------------------------- */
#define MAX_DATA_LEN_PRF2  2048 /* In terms of bits,so total 256 bytes */

/*==============================================
 * Version controls for all E2E header files.
 *==============================================*/

#define EXPECTED_E2E_SW_MAJOR_VERSION  (2)
#define EXPECTED_E2E_SW_MINOR_VERSION  (0)
#define EXPECTED_E2E_SW_PATCH_VERSION  (0)

/* Check E2E.h */
#if ((E2E_SW_MAJOR_VERSION != EXPECTED_E2E_SW_MAJOR_VERSION)||(E2E_SW_MINOR_VERSION != EXPECTED_E2E_SW_MINOR_VERSION)|| \
   (E2E_SW_PATCH_VERSION != EXPECTED_E2E_SW_PATCH_VERSION))
#error The SW version of the E2E.c file does not match the version of the E2E.h file
#endif

/* PRQA S 5087 ++
 * MISRA RULE C197 VIOLATION:
 */
#define  E2E_START_SEC_CODE
#include "MemMap.h"
/* PRQA S 5087 -- */

/* For detailed explanation on the exported functions see E2E_P02.h file. */

FUNC(Std_ReturnType,E2E_CODE) E2E_P02Protect( P2CONST(E2E_P02ConfigType, AUTOMATIC, E2E_APPL_DATA) pConfig,
                                              P2VAR(E2E_P02SenderStateType, AUTOMATIC, E2E_APPL_DATA) pState,
                                              P2VAR(uint8, AUTOMATIC, E2E_APPL_DATA) pData )
{
   uint8 CRC = 0,Assigned_DataID = 0;
   uint16 LenInBytes= 0;

   Std_ReturnType Ret_Status = E2E_E_OK;

   if((NULL_PTR != pConfig)&&(NULL_PTR != pState)&&(NULL_PTR != pData))
   {
      if ((pConfig->DataLength <= MAX_DATA_LEN_PRF2)
          && (pState->Counter < 0x10))
      {
         if ((pState->Counter < 15)==TRUE)
         {
            pState->Counter++;
         }
         else
         {
            pState->Counter = 0;
         }

         pData[1] = (uint8)(((pData[1]) & 0xF0) | (pState->Counter & 0x0F));

         Assigned_DataID = pConfig->DataIDList[pState->Counter];

         LenInBytes = (pConfig->DataLength/8)-1;

         /* Call the calculate the CRC function */
         CRC = Crc_CalculateCRC8H2F((uint8 *)&(pData[1]),LenInBytes,Assigned_DataID, FALSE);
         /* Place of CRC storage */
         pData[0]=CRC;
      }
      else
      {
         /* Status shall be wrong when there is any erroneous value eg: out of range value */
         Ret_Status = E2E_E_INPUTERR_WRONG;
      }
   }
   else
   {
      Ret_Status = E2E_E_INPUTERR_NULL;
   }
   return Ret_Status;
}

/* For detailed explanation on the exported functions see E2E_P02.h file. */

FUNC(Std_ReturnType,E2E_CODE)  E2E_P02Check (P2CONST(E2E_P02ConfigType, AUTOMATIC, E2E_APPL_DATA) pConfig,
                                             P2VAR(E2E_P02ReceiverStateType, AUTOMATIC, E2E_APPL_DATA) pState,
                                             P2CONST(uint8, AUTOMATIC, E2E_APPL_DATA) pData )
{
   uint8 ReceivedCounter=0,Rxd_DataID=0,CalculatedCRC = 0;
   uint16 LenInBytes=0;
   sint8 DeltaCounter=0;

   Std_ReturnType Ret_Status = E2E_E_OK;

   if((NULL_PTR != pConfig)&&(NULL_PTR != pState)&&(NULL_PTR != pData))
   {
      if(pState->MaxDeltaCounter<15)
      {
         pState->MaxDeltaCounter++;
      }

      if(pState->NewDataAvailable == TRUE)
      {
         /* Counter is on lower nibble of byte 1 */
		 /* PRQA S 0489 ++ */
         ReceivedCounter = *(pData+1) & 0x0F;
		 /* PRQA S 0489 -- */

         Rxd_DataID = pConfig->DataIDList[ReceivedCounter];

         LenInBytes = (pConfig->DataLength/8)-1;

         /** Call to the calculate the CRC function **/
         CalculatedCRC = Crc_CalculateCRC8H2F(&(pData[1]),LenInBytes,Rxd_DataID, FALSE);

         /* Compare the Rxd CRC with the calculated CRC */
         if(pData[0] == CalculatedCRC)
         {
            if(pState->WaitForFirstData == TRUE)
            {
               /* This is first message received. Clear this flag .
                * Counter value is not checked since this is the first message
                */
               pState->WaitForFirstData = FALSE;
               /* Use the maximum value from configuration MaxDeltaCounterInit*/
               pState->MaxDeltaCounter = pConfig->MaxDeltaCounterInit;

               pState->LastValidCounter = ReceivedCounter;

               pState->Status = E2E_P02STATUS_INITAL;
            }
            else
            {
               /* Calculate the delta counter */
               DeltaCounter =(sint8) (ReceivedCounter-pState->LastValidCounter);

               if (DeltaCounter < 0)
               {
                  DeltaCounter = DeltaCounter+16;
               }
               if(DeltaCounter == 0)
               {
                  pState->Status = E2E_P02STATUS_REPEATED;
               }
               else if (DeltaCounter ==1)
               {
                  pState->MaxDeltaCounter = pConfig->MaxDeltaCounterInit;

                  pState->LastValidCounter = ReceivedCounter;

                  pState->LostData = 0;

                  pState->Status = E2E_P02STATUS_OK;
               }
               else if ((1 < DeltaCounter)&&(DeltaCounter <= pState->MaxDeltaCounter))
               {
                  pState->MaxDeltaCounter = pConfig->MaxDeltaCounterInit;

                  pState->LastValidCounter = ReceivedCounter;

                  pState->LostData = (uint8) DeltaCounter-1 ;

                  pState->Status = E2E_P02STATUS_OKSOMELOST;
               }
               else if (DeltaCounter > pState->MaxDeltaCounter)
               {
                  pState->Status = E2E_P02STATUS_WRONGSEQUENCE;
               }
               else
               {
                  /* Do nothing */
               }
            }
         }
         else
         {
            pState->Status = E2E_P02STATUS_WRONGCRC;
         }
      }
      else
      {
         pState->Status = E2E_P02STATUS_NONEWDATA;
      }
   }
   else
   {
      Ret_Status = E2E_E_INPUTERR_NULL;
   }
   return Ret_Status;
}
/**
 * Mark end of memory area for code
 */
/* PRQA S 5087 ++
 * MISRA RULE C197 VIOLATION:
 */
#define  E2E_STOP_SEC_CODE
#include "MemMap.h"
/* PRQA S 5087 -- */
/* END OF FILE -------------------------------------------------------------- */
