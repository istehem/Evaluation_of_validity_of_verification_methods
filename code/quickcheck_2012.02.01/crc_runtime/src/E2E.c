/**\file
 *
 * \brief Implementation file for Picea E2E Module
 *
 *    This file contains the implementation of the AUTOSAR basic sw module
 *     End to End Software Protection Library.\n
 *
 * \b Application:        E2E Library \n
 * \b Target:             see E2E.h\n
 * \b Compiler:           see E2E.h\n
 * \b Autosar-Vendor-ID:  see E2E.h\n
 * \n
 * \b Module:             E2E.c \n
 * \b File-Revision: %version: 7 %\n
 * \b Changeable-by-user: No \n
 * \b Delivery-File:      Yes \n
 * \n
 * \b Module-Owner:       see E2E.h \n
 * \b Location:           see E2E.h \n
 * \b Phone:              see E2E.h \n
 * \b E-Mail:             see E2E.h \n
 * \n
 * \b Traceability-Info   see E2E.h \n
 * \b Classification:     see E2E.h \n
 * \b Deviations:         see E2E.h \n
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

/* LOCAL DEFINES FOR CONSTANTS ----------------------------------------------- */

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

/* Check for Crc.h*/
#define EXPECTED_CRC_AR_MAJOR_VERSION   (4)
#define EXPECTED_CRC_AR_MINOR_VERSION   (0)

#if ((CRC_AR_RELEASE_MAJOR_VERSION != EXPECTED_CRC_AR_MAJOR_VERSION) || \
   (CRC_AR_RELEASE_MINOR_VERSION != EXPECTED_CRC_AR_MINOR_VERSION))
#error The AUTOSAR Release version of the Crc file does not match the expected version
#endif

/* PRQA S 5087 ++
 * MISRA RULE C197 VIOLATION:
 */
#define  E2E_START_SEC_CODE
#include "MemMap.h"
/* PRQA S 5087 -- */

/*============================================================================*
* EXPORTED FUNCTIONS
*============================================================================*/

/* For detailed explanation on the exported functions see E2E.h file. */

FUNC(uint8,E2E_CODE) E2E_CalculateCrc (P2VAR(E2E_P01ConfigType, AUTOMATIC, E2E_APPL_DATA) Config,
                                       uint8 Counter,P2CONST(uint8, AUTOMATIC, E2E_APPL_DATA) pData )
{
   uint8 CRC=0,crc_data;
   uint16 TempDataId;
   E2E_P01ConfigType *pConfig = Config;

   switch (pConfig->DataIDMode)
   {
   case E2E_P01_DATAID_BOTH:
   {
      /* CRC is calculated over 2 bytes */
      /* Store the DataId ,since it will loose higher
       * byte of data when AND is done with 0xFF */
      TempDataId = pConfig->DataID & 0xFF;

      /* Calculates on the lower bits first pConfig->DataID & 0xFF) */
      crc_data = (uint8)TempDataId;
      CRC = Crc_CalculateCRC8(&crc_data,1,0x00,FALSE);

      /* Store the DataId back to pConfig->DataID */
      TempDataId =(pConfig->DataID>>8)&0xFF;
      crc_data = (uint8)TempDataId;

      /* Calculates on the higher bits
       * The XOR operation of CRC^0xFF is removed since this is done by the CRC routine*/
      CRC = Crc_CalculateCRC8(&crc_data,1,(uint8)(CRC),FALSE);
   }
   break;
   case E2E_P01_DATAID_LOW:
   {
      /* CRC is calculated over low byte */
      pConfig->DataID = pConfig->DataID & 0xFF;
      crc_data = (uint8)pConfig->DataID;

      CRC = Crc_CalculateCRC8(&crc_data,1,0x00,FALSE);
   }
   break;
   case E2E_P01_DATAID_ALT:
   {
      if((Counter % 2)== FALSE)
      {
         pConfig->DataID = pConfig->DataID & 0xFF;
         crc_data = (uint8)pConfig->DataID;

         CRC = Crc_CalculateCRC8(&crc_data,1,0x00,FALSE);
      }
      else
      {
         pConfig->DataID = (pConfig->DataID>>8)&0xFF;
         crc_data = (uint8)pConfig->DataID;

         CRC = Crc_CalculateCRC8(&crc_data,1,0x00,FALSE);
      }
   }
   break;
   default:
   {
      /* Do nothing */
   }
   break;
   }

   /* Compute CRC over the area before the CRC (if CRC is not the first byte) */
   if (pConfig->CRCOffset >= 8)
   {
      CRC = Crc_CalculateCRC8 (pData, (pConfig->CRCOffset / 8), CRC, FALSE);
   }
   else
   {
      /* Do nothing */
   }

   if(((pConfig->CRCOffset) / 8) < (((pConfig->DataLength) / 8) - 1))
   {
      CRC = Crc_CalculateCRC8 (&pData[((pConfig->CRCOffset)/8) + 1], (pConfig->DataLength / 8) - (pConfig->CRCOffset / 8) - 1, CRC, FALSE);
   }
   else
   {
      /* Do nothing */
   }

   /* To negate the last XOR 0xFF operation done on computed CRC by the last
    * Crc_CalculateCRC8(), there is a XORing done externally by the E2E Library */
   CRC = (uint8)(CRC^0x00);

   return CRC;
}

FUNC(void, E2E_CODE)   E2E_GetVersionInfo
(
   P2VAR(Std_VersionInfoType, AUTOMATIC, E2E_APPL_DATA) versioninfo
)
{
   /* simplest protection against passing null pointer */
   if (NULL_PTR != versioninfo)
   {
      versioninfo->vendorID = E2E_VENDOR_ID ;
      versioninfo->moduleID = E2E_MODULE_ID ;
      versioninfo->sw_major_version = E2E_SW_MAJOR_VERSION;
      versioninfo->sw_minor_version = E2E_SW_MINOR_VERSION;
      versioninfo->sw_patch_version = E2E_SW_PATCH_VERSION;
   }
   else
   {
      /* function returns nothing */
   }
}

/* For detailed explanation on the exported functions see E2E.h file */

FUNC(uint8, E2E_CODE) E2E_CRC8u8(VAR(uint8, AUTOMATIC) E2E_Data, VAR(uint8,
                                                                     AUTOMATIC) E2E_StartValue)
{
   VAR(uint8, AUTOMATIC) LucCRC;
   /* CRC calculation for data byte */
   LucCRC = Crc_CalculateCRC8(&E2E_Data, DATA_LENGTH_8BIT, E2E_StartValue, FALSE);

   return(LucCRC);
}

/* For detailed explanation on the exported functions see E2E.h file */

FUNC(uint8, E2E_CODE) E2E_CRC8u16(VAR(uint16, AUTOMATIC) E2E_Data, VAR(uint8,
                                                                       AUTOMATIC) E2E_StartValue)
{
   VAR(uint8, AUTOMATIC) LucCRC;
   VAR(uint8, AUTOMATIC) LucData[DATA_LENGTH_16BIT];

   /* Get the LSB byte to calculate CRC */
   LucData[0] = (uint8)((E2E_Data) & ((uint16)0x00FF));
   /* Get the MSB byte to calculate CRC */
   /* PRQA S 1281 ++ */
   LucData[1] = (uint8)((E2E_Data & (uint16)0xFF00) >> (uint8)0x08);
   /* PRQA S 1281 -- */
   /* CRC calculation for user Data */
   LucCRC = (uint8)Crc_CalculateCRC8(LucData, DATA_LENGTH_16BIT, E2E_StartValue, FALSE);

   return(LucCRC);
}

/* For detailed explanation on the exported functions see E2E.h file */

FUNC(uint8, E2E_CODE) E2E_CRC8u32(VAR(uint32, AUTOMATIC) E2E_Data, VAR(uint8,
                                                                       AUTOMATIC) E2E_StartValue)
{
   VAR(uint8, AUTOMATIC) LucData[DATA_LENGTH_32BIT];
   VAR(uint8, AUTOMATIC) LucCRC;
   VAR(uint8, E2E_FAST_VAR) LucCount;
   VAR(uint8, E2E_FAST_VAR) LucRightShift;
   VAR(uint8, E2E_FAST_VAR) LucLeftShift;

   /* Initialize the shift counters value */
   LucRightShift = (uint8)(0x00);
   LucLeftShift = (uint8)(0x00);

   for(LucCount = 0; LucCount < DATA_LENGTH_32BIT; LucCount++)
   {
      /* Get the subsequent byte value to an local uint8 array, starts from LSB*/
      LucData[LucCount] = (uint8)((E2E_Data & ((uint32)0x000000FF << LucLeftShift)) >> LucRightShift);
      /* Increment the shift counters value by 0x08 to get the subsequent byte value */
      LucRightShift += 0x08;
      LucLeftShift += 0x08;
   }

   /* CRC calculation for user data bytes */
   LucCRC = Crc_CalculateCRC8(LucData, DATA_LENGTH_32BIT, E2E_StartValue, FALSE);

   return(LucCRC);
}

/* For detailed explanation on the exported functions see E2E.h file */

FUNC(uint8, E2E_CODE) E2E_CRC8u8Array(P2CONST(uint8, AUTOMATIC, E2E_APPL_DATA)
                                      E2E_DataPtr, VAR(uint32, AUTOMATIC) E2E_ArrayLength, VAR(uint8,
                                                                                               AUTOMATIC) E2E_StartValue)
{
   VAR(uint8, AUTOMATIC) LucCRC;
   /* CRC calculation for user data bytes */
   LucCRC = Crc_CalculateCRC8(E2E_DataPtr, E2E_ArrayLength, E2E_StartValue, FALSE);

   return(LucCRC);
}

/* For detailed explanation on the exported functions see E2E.h file */

FUNC(uint8, E2E_CODE) E2E_CRC8u16Array(P2CONST(uint16, AUTOMATIC, E2E_APPL_DATA)
                                       E2E_DataPtr, VAR(uint32,AUTOMATIC) E2E_ArrayLength, VAR(uint8,
                                                                                               AUTOMATIC) E2E_StartValue)
{
   VAR(uint32, E2E_FAST_VAR) LucDataCount;
   VAR(uint8, E2E_FAST_VAR) LucData[DATA_LENGTH_16BIT];
   VAR(uint8, E2E_FAST_VAR) LucDataIndex;
   VAR(uint8, AUTOMATIC) LucCRC;

   /* Get the number of element present in an array */
   LucDataCount = E2E_ArrayLength / DATA_LENGTH_16BIT;

   /* CRC calculation for first element in array 
    * Intialize the array index value */
   LucDataIndex = 0x00;
   /* Get the LSB byte of the array element */
   LucData[0] = (uint8)((E2E_DataPtr[LucDataIndex]) & ((uint16)0x00FF));
   /* Get the MSB byte of the first element in array */
   /* PRQA S 1281 ++ */
   LucData[1] = (uint8)((E2E_DataPtr[LucDataIndex] & (uint16)0xFF00) >> (uint8)0x08);
   /* PRQA S 1281 -- */
   /* CRC calculation for first element in an arry */
   LucCRC = Crc_CalculateCRC8(LucData, DATA_LENGTH_16BIT, E2E_StartValue, FALSE);

   /* CRC calculation for subsequent elements in array */
   for(LucDataIndex = 1; LucDataIndex < LucDataCount; LucDataIndex++)
   {
      /* Get the LSB byte to calculate CRC */
      LucData[0] = (uint8)((E2E_DataPtr[LucDataIndex]) & ((uint16)0x00FF));
      /* Get the MSB byte to calculate CRC */
	  /* PRQA S 1281 ++ */
      LucData[1] = (uint8)((E2E_DataPtr[LucDataIndex] & (uint16)0xFF00) >> (uint8)0x08);
	  /* PRQA S 1281 -- */
      /* CRC calculation for subsequent byte(MSB) */
      LucCRC = Crc_CalculateCRC8(LucData, DATA_LENGTH_16BIT, LucCRC, FALSE);
   }
   return(LucCRC);
}

/* For detailed explanation on the exported functions see E2E.h file */

FUNC(uint8, E2E_CODE) E2E_CRC8u32Array(P2CONST(uint32, AUTOMATIC, E2E_APPL_DATA)
                                       E2E_DataPtr, VAR(uint32,AUTOMATIC) E2E_ArrayLength, VAR(uint8,
                                                                                               AUTOMATIC) E2E_StartValue)
{
   VAR(uint32, AUTOMATIC) LucDataCount;
   VAR(uint8, E2E_FAST_VAR) LucData[DATA_LENGTH_32BIT];
   VAR(uint8, E2E_FAST_VAR) LucDataIndex;
   VAR(uint8, E2E_FAST_VAR) LucByteCount;
   VAR(uint8, E2E_FAST_VAR) LucRightShift;
   VAR(uint8, E2E_FAST_VAR) LucLeftShift;
   VAR(uint8, AUTOMATIC) LucCRC;

   /* Get the number of element present in an array */
   LucDataCount = E2E_ArrayLength / DATA_LENGTH_32BIT;
   /*  Intialize the array index value */
   LucDataIndex = 0x00;
   /* Initialize the shift counters value */
   LucRightShift = (uint8)(0x00);
   LucLeftShift = (uint8)(0x00);
   /* Get the subsequent byte values of first element of array to an local uint8 array */
   for(LucByteCount = 0; LucByteCount < DATA_LENGTH_32BIT; LucByteCount++)
   {
      /* Get the byte value start from LSB */
      LucData[LucByteCount] = (uint8)((E2E_DataPtr[LucDataIndex] & ((uint32)0x000000FF << LucLeftShift)) >> LucRightShift);
      /* Increment the shift counters value by 0x08 to get the subsequent byte value */
      LucRightShift += 0x08;
      LucLeftShift += 0x08;
   } /* End of for loop */

   /* CRC calculation for first element in array */
   LucCRC = Crc_CalculateCRC8(LucData, DATA_LENGTH_32BIT, E2E_StartValue, FALSE);

   /* CRC calculation for subsequent element in array */
   for(LucDataIndex = 1; LucDataIndex < LucDataCount; LucDataIndex++)
   {
      /* Initialize the shift counters value */
      LucRightShift = (uint8)(0x00);
      LucLeftShift = (uint8)(0x00);
      for(LucByteCount = 0; LucByteCount < DATA_LENGTH_32BIT; LucByteCount++)
      {
         /* Get the subsequent byte value to an local uint8 array, starts from LSB*/
         LucData[LucByteCount] = (uint8)((E2E_DataPtr[LucDataIndex] & ((uint32)0x000000FF << LucLeftShift)) >> LucRightShift);
         /* Increment the shift counters value by 0x08 to get the subsequent byte value */
         LucRightShift += 0x08;
         LucLeftShift += 0x08;
      } /* End of for loop */

      /* CRC calculation for subsequent element in array */
      LucCRC = Crc_CalculateCRC8(LucData, DATA_LENGTH_32BIT, LucCRC, FALSE);
   }

   return(LucCRC);
}
/* For detailed explanation on the exported functions see E2E.h file */

FUNC(uint8, E2E_CODE) E2E_CRC8H2Fu8(VAR(uint8, AUTOMATIC) E2E_Data, VAR(uint8,
                                                                        AUTOMATIC) E2E_StartValue)
{
   VAR(uint8, AUTOMATIC) LucCRC;
   /* CRC calculation for data byte */
   LucCRC = Crc_CalculateCRC8H2F(&E2E_Data, DATA_LENGTH_8BIT, E2E_StartValue, FALSE);

   return(LucCRC);
}

/* For detailed explanation on the exported functions see E2E.h file */

FUNC(uint8, E2E_CODE) E2E_CRC8H2Fu16(VAR(uint16, AUTOMATIC) E2E_Data, VAR(uint8,
                                                                          AUTOMATIC) E2E_StartValue)
{
   VAR(uint8, AUTOMATIC) LucCRC;
   VAR(uint8, AUTOMATIC) LucData[DATA_LENGTH_16BIT];

   /* Get the LSB byte to calculate CRC */
   /* PRQA S 1281 ++ */
   LucData[0] = (uint8)((E2E_Data) & ((uint16)0x00FF));
   /* Get the MSB byte to calculate CRC */
   LucData[1] = (uint8)((E2E_Data & (uint16)0xFF00) >> (uint8)0x08);
   /* CRC calculation for user Data */
   /* PRQA S 1281 -- */
   LucCRC = Crc_CalculateCRC8H2F(LucData, DATA_LENGTH_16BIT, E2E_StartValue, FALSE);

   return(LucCRC);
}

/* For detailed explanation on the exported functions see E2E.h file */

FUNC(uint8, E2E_CODE) E2E_CRC8H2Fu32(VAR(uint32, AUTOMATIC) E2E_Data, VAR(uint8,
                                                                          AUTOMATIC) E2E_StartValue)
{
   VAR(uint8, AUTOMATIC) LucData[DATA_LENGTH_32BIT];
   VAR(uint8, AUTOMATIC) LucCRC;
   VAR(uint8, E2E_FAST_VAR) LucCount;
   VAR(uint8, E2E_FAST_VAR) LucRightShift;
   VAR(uint8, E2E_FAST_VAR) LucLeftShift;

   /* Initialize the shift counters value */
   LucRightShift = (uint8)(0x00);
   LucLeftShift = (uint8)(0x00);

   for(LucCount = 0; LucCount < DATA_LENGTH_32BIT; LucCount++)
   {
      /* Get the subsequent byte value to an local uint8 array, starts from LSB*/
      LucData[LucCount] = (uint8)((E2E_Data & ((uint32)0x000000FF << LucLeftShift)) >> LucRightShift);
      /* Increment the shift counters value by 0x08 to get the subsequent byte value */
      LucRightShift += 0x08;
      LucLeftShift += 0x08;
   }

   /* CRC calculation for user data bytes */
   LucCRC = Crc_CalculateCRC8H2F(LucData, DATA_LENGTH_32BIT, E2E_StartValue, FALSE);

   return(LucCRC);
}

/* For detailed explanation on the exported functions see E2E.h file */

FUNC(uint8, E2E_CODE) E2E_CRC8H2Fu8Array(P2CONST(uint8, AUTOMATIC, E2E_APPL_DATA)
                                         E2E_DataPtr, VAR(uint32, AUTOMATIC) E2E_ArrayLength, VAR(uint8,
                                                                                                  AUTOMATIC) E2E_StartValue)
{
   VAR(uint8, AUTOMATIC) LucCRC;
   /* CRC calculation for user data bytes */
   LucCRC = Crc_CalculateCRC8H2F(E2E_DataPtr, E2E_ArrayLength, E2E_StartValue, FALSE);

   return(LucCRC);
}

/* For detailed explanation on the exported functions see E2E.h file */

FUNC(uint8, E2E_CODE) E2E_CRC8H2Fu16Array(P2CONST(uint16, AUTOMATIC, E2E_APPL_DATA)
                                          E2E_DataPtr, VAR(uint32,AUTOMATIC) E2E_ArrayLength, VAR(uint8,
                                                                                                  AUTOMATIC) E2E_StartValue)
{
   VAR(uint32, E2E_FAST_VAR) LucDataCount;
   VAR(uint8, E2E_FAST_VAR) LucData[DATA_LENGTH_16BIT];
   VAR(uint8, E2E_FAST_VAR) LucDataIndex;
   VAR(uint8, AUTOMATIC) LucCRC;

   /* Get the number of element present in an array */
   LucDataCount = E2E_ArrayLength / DATA_LENGTH_16BIT;

   /* CRC calculation for first element in array 
    * Intialize the array index value */
   LucDataIndex = 0x00;
   /* Get the LSB byte of the array element */
   /* PRQA S 1281 ++ */
   LucData[0] = (uint8)((E2E_DataPtr[LucDataIndex]) & ((uint16)0x00FF));
   /* Get the MSB byte of the first element in array */
   LucData[1] = (uint8)((E2E_DataPtr[LucDataIndex] & (uint16)0xFF00) >> (uint8)0x08);
   /* PRQA S 1281 -- */
   /* CRC calculation for first byte */
   LucCRC = Crc_CalculateCRC8H2F(LucData, DATA_LENGTH_16BIT, E2E_StartValue, FALSE);

   /* CRC calculation for subsequent elements in array */
   for(LucDataIndex = 1; LucDataIndex < LucDataCount; LucDataIndex++)
   {
      /* Get the LSB byte to calculate CRC */
	  /* PRQA S 1281 ++ */
      LucData[0] = (uint8)((E2E_DataPtr[LucDataIndex]) & ((uint16)0x00FF));
      /* Get the MSB byte to calculate CRC */
      LucData[1] = (uint8)((E2E_DataPtr[LucDataIndex] & (uint16)0xFF00) >> (uint8)0x08);
	  /* PRQA S 1281 -- */
      /* CRC calculation for subsequent byte(MSB) */
      LucCRC = Crc_CalculateCRC8H2F(LucData, DATA_LENGTH_16BIT, LucCRC, FALSE);
   }
   return(LucCRC);
}

/* For detailed explanation on the exported functions see E2E.h file */

FUNC(uint8, E2E_CODE) E2E_CRC8H2Fu32Array(P2CONST(uint32, AUTOMATIC, E2E_APPL_DATA)
                                          E2E_DataPtr, VAR(uint32,AUTOMATIC) E2E_ArrayLength, VAR(uint8,
                                                                                                  AUTOMATIC) E2E_StartValue)
{
   VAR(uint32, AUTOMATIC) LucDataCount;
   VAR(uint8, E2E_FAST_VAR) LucData[DATA_LENGTH_32BIT];
   VAR(uint8, E2E_FAST_VAR) LucDataIndex;
   VAR(uint8, E2E_FAST_VAR) LucByteCount;
   VAR(uint8, E2E_FAST_VAR) LucRightShift;
   VAR(uint8, E2E_FAST_VAR) LucLeftShift;
   VAR(uint8, AUTOMATIC) LucCRC;

   /* Get the number of element present in an array */
   LucDataCount = E2E_ArrayLength / DATA_LENGTH_32BIT;
   /*  Intialize the array index value */
   LucDataIndex = 0x00;
   /* Initialize the shift counters value */
   LucRightShift = (uint8)(0x00);
   LucLeftShift = (uint8)(0x00);
   /* Get the subsequent byte values of first element of array to an local uint8 array */
   for(LucByteCount = 0; LucByteCount < DATA_LENGTH_32BIT; LucByteCount++)
   {
      /* Get the byte value start from LSB */
      LucData[LucByteCount] = (uint8)((E2E_DataPtr[LucDataIndex] & ((uint32)0x000000FF << LucLeftShift)) >> LucRightShift);
      /* Increment the shift counters value by 0x08 to get the subsequent byte value */
      LucRightShift += 0x08;
      LucLeftShift += 0x08;
   } /* End of for loop */

   /* CRC calculation for first element in array */
   LucCRC = Crc_CalculateCRC8H2F(LucData, DATA_LENGTH_32BIT, E2E_StartValue, FALSE);

   /* CRC calculation for subsequent element in array */
   for(LucDataIndex = 1; LucDataIndex < LucDataCount; LucDataIndex++)
   {
      /* Initialize the shift counters value */
      LucRightShift = (uint8)(0x00);
      LucLeftShift = (uint8)(0x00);
      for(LucByteCount = 0; LucByteCount < DATA_LENGTH_32BIT; LucByteCount++)
      {
         /* Get the subsequent byte value to an local uint8 array, starts from LSB*/
         LucData[LucByteCount] = (uint8)((E2E_DataPtr[LucDataIndex] & ((uint32)0x000000FF << LucLeftShift)) >> LucRightShift);
         /* Increment the shift counters value by 0x08 to get the subsequent byte value */
         LucRightShift += 0x08;
         LucLeftShift += 0x08;
      } /* End of for loop */

      /* CRC calculation for subsequent element in array */
      LucCRC = Crc_CalculateCRC8H2F(LucData, DATA_LENGTH_32BIT, LucCRC, FALSE);
   }

   return(LucCRC);
}

/* For detailed explanation on the exported functions see E2E.h file */

FUNC(uint8, E2E_CODE) E2E_UpdateCounter(VAR(uint8, AUTOMATIC) Counter)
{
   /* Increments the counter provided by the parameter */
   ++Counter;

   return (Counter % 0x0F);
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
