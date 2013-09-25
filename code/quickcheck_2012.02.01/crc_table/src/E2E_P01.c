/**\file
 *
 * \brief Implementation file for Picea E2E Module - Profile 1
 *
 *    This file contains the implementation of the AUTOSAR basic sw module
 *     End to End Software Protection Library.\n
 *
 * \b Application:        E2E Library \n
 * \b Target:             see E2E_P01.h\n
 * \b Compiler:           see E2E_P01.h\n
 * \b Autosar-Vendor-ID:  see E2E_P01.h\n
 * \n
 * \b Module:             E2E_P01.c \n
 * \b File-Revision: %version: 6 %\n
 * \b Changeable-by-user: No \n
 * \b Delivery-File:      Yes \n
 * \n
 * \b Module-Owner:       see E2E_P01.h \n
 * \b Location:           see E2E_P01.h \n
 * \b Phone:              see E2E_P01.h \n
 * \b E-Mail:             see E2E_P01.h \n
 * \n
 * \b Traceability-Info   see E2E_P01.h \n
 * \b Classification:     see E2E_P01.h \n
 * \b Deviations:         see E2E_P01.h \n
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

#define MAX_DATA_LEN_PRF1  240 /* In terms of bits,so total 30 bytes */

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

/* For detailed explanation on the exported functions see E2E_P01.h file. */

FUNC(Std_ReturnType,E2E_CODE) E2E_P01Protect( P2VAR(E2E_P01ConfigType, AUTOMATIC, E2E_APPL_DATA) pConfig,
                                              P2VAR(E2E_P01SenderStateType, AUTOMATIC, E2E_APPL_DATA) pState,
                                              P2VAR(uint8, AUTOMATIC, E2E_APPL_DATA) pData )
{
   uint8 CRC = 0,index=0,BufferIndex=0;
   uint16 LenInBytes=0,CrcOffset=0 ;
   uint8 DataBuffer[MAX_DATA_LEN_PRF1]={0};
   Std_ReturnType Ret_Status = E2E_E_OK;

   if((NULL_PTR != pConfig)&&(NULL_PTR != pState)&&(NULL_PTR != pData))
   {
      if ((pConfig->DataLength <= MAX_DATA_LEN_PRF1)&& (pState->Counter < 0xF)) /* In terms of bits,so total 30 bytes */
      {
         if((pConfig->CounterOffset%8)==0)
         {
            /* Stores the counter value in the LOWER nibble of the byte */
            pData[pConfig->CounterOffset/8] = (uint8)(((pData[pConfig->CounterOffset/8]) & 0xF0) | (pState->Counter & 0x0F));
         }
         else
         {
            /* Stores the counter value in the HIGHER nibble of the byte */
            pData[pConfig->CounterOffset/8] = (uint8)(((pData[pConfig->CounterOffset/8]) & 0x0F) | ((pState->Counter<<4) & 0xF0));
         }

         /* Send pDataBuffer with only Data , not the CRCoffset position
          * Assumed that DataLength is including the CRCoffset position */
         LenInBytes = (pConfig->DataLength/8)-1;
         CrcOffset  = pConfig->CRCOffset/8;

         for (index=0; index < MAX_DATA_LEN_PRF1 ; index++)
         {
            if ((CrcOffset!=index)&& (BufferIndex!=LenInBytes))
            {
               DataBuffer[BufferIndex]=pData[index];
               BufferIndex++;
            }
         }

         /** Call the calculate the CRC function **/
         CRC = E2E_CalculateCrc(pConfig,(uint8)(pState->Counter),DataBuffer);

         /** Place the Crc on to the frame **/
         pData[pConfig->CRCOffset/8] = CRC ;

         /** Increment the state counter ,modulo 15 is done so that value after 14 should be 0 **/
         pState->Counter++;
         pState->Counter = (pState->Counter%15);
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

/* For detailed explanation on the exported functions see E2E_P01.h file. */

FUNC(Std_ReturnType,E2E_CODE)  E2E_P01Check (P2VAR(E2E_P01ConfigType, AUTOMATIC, E2E_APPL_DATA) pConfig,
                                             P2VAR(E2E_P01ReceiverStateType, AUTOMATIC, E2E_APPL_DATA) pState,
                                             P2CONST(uint8, AUTOMATIC, E2E_APPL_DATA) pData )
{
   uint8 ReceivedCounter=0,ReceivedCRC = 0,CalculatedCRC = 0;
   sint8 DeltaCounter=0;
   uint16 CrcOffset=0;
   uint8 DataBuffer[MAX_DATA_LEN_PRF1]={0};

   /* Acts as a temporary data buffer for only data without crc byte*/
   uint16 index,BufferIndex=0,LenInBytes=0;

   Std_ReturnType Ret_Status = E2E_E_OK;

   if((NULL_PTR != pConfig)&&(NULL_PTR != pState)&&(NULL_PTR != pData))
   {
      if(pState->MaxDeltaCounter<14)
      {
         pState->MaxDeltaCounter++;
      }

      if(pState->NewDataAvailable == TRUE)
      {
         /* Read the counter by checking on the counteroffset */
         if(((pConfig->CounterOffset) % 8) == 0)
         {
            ReceivedCounter = (pData[pConfig->CounterOffset/8]) & 0x0F;
         }
         else
         {
            ReceivedCounter = ((pData[pConfig->CounterOffset/8])>>4)& 0x0F;
         }

         CrcOffset  = pConfig->CRCOffset/8;
         /* Send pDataBuffer with only Data , not the CRCoffset position
          * Assumed that DataLength is including the CRCoffset position */
         LenInBytes = (pConfig->DataLength/8)-1;
         /* Have to calculate the CRC for the pData excluding the CRC byte,so find the crc byte */

         for (index=0; index < MAX_DATA_LEN_PRF1 ; index++)
         {
            if ((CrcOffset!=index)&& (BufferIndex!=LenInBytes))
            {
               DataBuffer[BufferIndex]=pData[index];
               BufferIndex++;
            }
         }

         /* Read the CRC from pData section */
         ReceivedCRC = pData[pConfig->CRCOffset/8];

         /** Call the CRC function **/
         CalculatedCRC = E2E_CalculateCrc(pConfig,(uint8)(pState->LastValidCounter),DataBuffer);

         if(ReceivedCRC == CalculatedCRC)
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

               pState->Status = E2E_P01STATUS_INITAL;
            }
            else
            {
               if(ReceivedCounter >= pState->LastValidCounter)
               {
                  /* Difference value between previous and new counter
                   * should be one ,check on that
                   */

                  DeltaCounter = (sint8) (ReceivedCounter-pState->LastValidCounter);
               }
               else
               {
                  /* Here the exceeded value is added in order to check if the RxdCntr is lesser than
                   * LastValidcounter ,if so then some data is lost .
                   */
                  DeltaCounter = (sint8)(GREATER_MAX_COUNTER_VALUE + (ReceivedCounter-pState->LastValidCounter));
               }

               if(DeltaCounter == 0)
               {
                  pState->Status = E2E_P01STATUS_REPEATED;
               }
               else if (DeltaCounter ==1)
               {
                  pState->MaxDeltaCounter = pConfig->MaxDeltaCounterInit;

                  pState->LastValidCounter = ReceivedCounter;

                  pState->LostData = 0;

                  pState->Status = E2E_P01STATUS_OK;
               }
               else if ((1 < DeltaCounter)&&(DeltaCounter <= pState->MaxDeltaCounter))
               {
                  pState->MaxDeltaCounter = pConfig->MaxDeltaCounterInit;

                  pState->LastValidCounter = ReceivedCounter;

                  pState->LostData = (uint8) DeltaCounter-1 ;

                  pState->Status = E2E_P01STATUS_OKSOMELOST;
               }
               else if (DeltaCounter > pState->MaxDeltaCounter)
               {
                  pState->Status = E2E_P01STATUS_WRONGSEQUENCE;
               }
               else
               {
                  /* Do nothing */
               }
            }
         }
         else
         {
            pState->Status = E2E_P01STATUS_WRONGCRC;
         }
      }
      else
      {
         pState->Status = E2E_P01STATUS_NONEWDATA;
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
