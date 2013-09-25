#ifndef E2E_H
#define E2E_H

/**\file
 *
 * \brief E2E Header File for Picea.
 *
 *    This file describes the End to End SW protection Library module
 *    for Picea.\n
 *
 * \b Application:        E2E \n
 * \b Target:             N.A. \n
 * \b Compiler:           N.A. \n
 * \b Autosar-Vendor-ID:  41 \n
 * \n
 * \b Module:             E2E.h \n
 * \b File-Revision: %version: 8 %\n
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
 * \reqtrace{DSAR-SDD-E2E-1-R1}{E2E0070, E2E0075, E2E0076, E2E0085, E2E0169,
 *                              E2E0163, E2E0083, E2E0190, E2E0082, E2E0227,
 *                              E2E0228, E2E0195, E2E0196, E2E0018, E2E0200,
 *                              E2E0020, E2E0021, E2E0166, E2E0158, E2E0021}
 *          {DSAR-SDD-E2E-4-R1}{E2E0092, E2E0091, E2E0094, E2E0095, E2E0096,
 *                              E2E0276, E2E0097, E2E0096, E2E0099, E2E0037}
 * \endlatexonly
 */

/*============================================================================*
*
* Copyright 2012 Mecel Technologies, Inc. All Rights Reserved
*
*============================================================================*/

/* INCLUDE DIRECTIVES FOR STANDARD HEADERS ----------------------------------- */

#include "Std_Types.h"

/* INCLUDE DIRECTIVES FOR OTHER HEADERS -------------------------------------- */
#include "Crc.h"
#include "E2E_P01.h"
#include "E2E_P02.h"

/* EXPORTED DEFINES FOR CONSTANTS -------------------------------------------- */

/**
 * Vendor ID as defined in vendor list .
 */
#define E2E_VENDOR_ID                           (41u)

/**
 * Module ID as defined in AUTOSAR_BasicSoftwareModules.pdf.
 */
#define E2E_MODULE_ID                           (207u)

/**
 * Instance ID as defined in AUTOSAR_BasicSoftwareModules.pdf.
 */
#define E2E_INSTANCE_ID                        (1)

/**
 * Major version number of AUTOSAR release which
 * the appropriate implementation is based on.
 */
#define E2E_AR_RELEASE_MAJOR_VERSION   (4)

/**
 * Minor version number of AUTOSAR release which
 * the appropriate implementation is based on.
 */
#define E2E_AR_RELEASE_MINOR_VERSION  (0)

/**
 * Patch version number of AUTOSAR release which
 * the appropriate implementation is based on.
 */
#define E2E_AR_RELEASE_PATCH_REVISION  (3)

/*
 * Major version number of the vendor specific implementation
 * of the module. The numbering is vendor specific.
 */
#define E2E_SW_MAJOR_VERSION  (2)

/*
 * Minor version number of the vendor specific implementation
 * of the module. The numbering is vendor specific.
 */
#define E2E_SW_MINOR_VERSION  (0)

/*
 * Patch version number of the vendor specific implementation
 * of the module. The numbering is vendor specific.
 */
#define E2E_SW_PATCH_VERSION  (0)

/* MACROS  ---------------------------------------------- */

#define E2E_E_OK              0x00
#define E2E_E_INPUTERR_NULL   0x13
#define E2E_E_INPUTERR_WRONG  0x17
#define E2E_E_INTERR          0x19
#define E2E_E_INVALID         0xFF

#define MAX_COUNTER_VALUE 14
#define GREATER_MAX_COUNTER_VALUE 15

#define DATA_LENGTH_8BIT                  (uint32)0x01
#define DATA_LENGTH_16BIT                 (uint32)0x02
#define DATA_LENGTH_32BIT                 (uint32)0x04

/**
 * Mark start of memory area for code
 */
#define E2E_START_SEC_CODE
#include "MemMap.h"

/**
 * \brief E2E_CalcualteCRC
 *
 *
 * This function calculates the CRC over Data ID and Data.
 * This function uses the Crc_CalculateCRC8 () function of the SWS CRC Library for
 * calculating CRC checksums.
 *
 * In this function there are three ways (inclusion modes) how the Data ID is included
 * into the CRC calculation: either both bytes are included in CRC, or alternating
 * (depending on parity of counter) either the high or the low byte is used, or only
 * the low byte is included into the CRC.
 *
 * \remark Sync/Async:      Synchronous
 * \remark Reentrancy:      Non-Reentrant
 * \remark Caveats:         -
 * \remark Configuration:   NA
 *
 * \param Counter (in)      This parameter on the sender side, for the first transmission
 *                            request of a data element it shall be initialized
 *                            with 0 and shall be incremented by 1 for every subsequent
 *                            send request.
 *                          On the receiver side, by evaluating this counter value of received
 *                          data against the counter of previously received data to know
 *                          if eg: data lost,new data or old data.
 *                          Range: 0 to 14
 * \param Config (in)       A pointer to a structure with Data Type related info:
 *                          Gives data on CRC offset,counter offset,Data Id,Data Id mode
 *                          data length and MaxDeltaCounterInit.
 * \param pData (in)        A pointer to a Data buffer on which CRC shall be calculated.
 * \retval CRC              The 8-bit value calculated by the CRC Library.
 */

FUNC(uint8,E2E_CODE) E2E_CalculateCrc (P2VAR(E2E_P01ConfigType, AUTOMATIC, E2E_APPL_DATA) Config,
                                       uint8 Counter,P2CONST(uint8, AUTOMATIC, E2E_APPL_DATA) pData);

/**
 * \brief          This service gives the version information of this module
 * \param[out]     versioninfo - pointer to standard version information
 *                               structure
 * \return         no return value
 * \ServID         0x03
 * \Reentrancy     non-reentrant
 * \Synchronism    synchronous
 * \Precondition   none
 * \Caveats        none
 */

extern FUNC(void, E2E_CODE) E2E_GetVersionInfo
(
   P2VAR(Std_VersionInfoType,AUTOMATIC,E2E_APPL_DATA) versioninfo
);

/**
 * \brief E2E_CRC8u8
 *
 *
 * This function for computing CRC over uint8 data types transmited with E2E
 * Protocol. The calculation is done in Least Significant Byte First, regardless
 * of the architecture of the microcontroller, because this is the byte order in
 * which data is transmitted over FlexRay, CAN and LIN.
 *
 * \ServID                    0x07
 * \remark Sync/Async:      Synchronous
 * \remark Reentrancy:      Reentrant
 * \remark Configuration:   NA
 *
 * \param  E2E_Data (in)        Current value over which the CRC is to be computed.
 * \param  E2E_StartValue (in)  CRC value from the previous iteration XORed with 0xFF, or (2) 0xFF if it is the first run
 * \retval 8-bit result of calculation.
 * \Precondition   none
 * \Caveats        none
 *
 */

extern FUNC(uint8, AUTOMATIC) E2E_CRC8u8(VAR(uint8, AUTOMATIC) E2E_Data,
                                         VAR(uint8, AUTOMATIC) E2E_StartValue);

/**
 * \brief E2E_CRC8u16
 *
 *
 * This function for computing CRC over uint16 data types transmited with E2E
 * Protocol. The calculation is done in Least Significant Byte First, regardless
 * of the architecture of the microcontroller, because this is the byte order in
 * which data is transmitted over FlexRay, CAN and LIN.
 *
 * \ServID                  0x08
 * \remark Sync/Async:      Synchronous
 * \remark Reentrancy:      Reentrant
 * \remark Configuration:   NA
 *
 * \param  E2E_Data (in)        Current value over which the CRC is to be computed.
 * \param  E2E_StartValue (in)  CRC value from the previous iteration XORed with 0xFF, or (2) 0xFF if it is the first run
 * \retval 8-bit result of calculation.
 * \Precondition   none
 * \Caveats        none
 *
 */

extern FUNC(uint8, AUTOMATIC) E2E_CRC8u16(VAR(uint16, AUTOMATIC) E2E_Data,
                                          VAR(uint8, AUTOMATIC) E2E_StartValue);

/**
 * \brief E2E_CRC8u32
 *
 *
 * This function for computing CRC over uint32 data types transmited with E2E
 * Protocol. The calculation is done in Least Significant Byte First, regardless
 * of the architecture of the microcontroller, because this is the byte order in
 * which data is transmitted over FlexRay, CAN and LIN.
 *
 * \ServID                  0x09
 * \remark Sync/Async:      Synchronous
 * \remark Reentrancy:      Reentrant
 * \remark Configuration:   NA
 *
 * \param  E2E_Data (in)        Current value over which the CRC is to be computed.
 * \param  E2E_StartValue (in)  CRC value from the previous iteration XORed with 0xFF, or (2) 0xFF if it is the first run
 * \retval 8-bit result of calculation.
 * \Precondition   none
 * \Caveats        none
 *
 */

extern FUNC(uint8, AUTOMATIC) E2E_CRC8u32(VAR(uint32, AUTOMATIC) E2E_Data,
                                          VAR(uint8, AUTOMATIC) E2E_StartValue);

/**
 * \brief E2E_CRC8u8Array
 *
 *
 * This function for computing CRC over uint8 array data types transmited with E2E
 * Protocol. The calculation is done in Least Significant Byte First, regardless
 * of the architecture of the microcontroller, because this is the byte order in
 * which data is transmitted over FlexRay, CAN and LIN.
 *
 * \ServID                    0x0A
 * \remark Sync/Async:      Synchronous
 * \remark Reentrancy:      Reentrant
 * \remark Configuration:   NA
 *
 * \param  E2E_DataPtr (in)        Current value over which the CRC is to be computed.
 * \param  E2E_ArrayLength (in) Length of array (data block) to be calculated in bytes.
 * \param  E2E_StartValue (in)  CRC value from the previous iteration XORed with 0xFF, or (2) 0xFF if it is the first run
 * \retval 8-bit result of calculation.
 * \Precondition   none
 * \Caveats        none
 *
 */
extern FUNC(uint8, AUTOMATIC) E2E_CRC8u8Array(P2CONST(uint8, AUTOMATIC,
                                                      E2E_APPL_DATA) E2E_DataPtr, VAR(uint32,AUTOMATIC) E2E_ArrayLength, VAR(uint8,
                                                                                                                             AUTOMATIC) E2E_StartValue);

/**
 * \brief E2E_CRC8u16Array
 *
 *
 * This function for computing CRC over uint16 array data types transmited with E2E
 * Protocol. The calculation is done in Least Significant Byte First, regardless
 * of the architecture of the microcontroller, because this is the byte order in
 * which data is transmitted over FlexRay, CAN and LIN.
 *
 * \ServID                    0x0B
 * \remark Sync/Async:      Synchronous
 * \remark Reentrancy:      Reentrant
 * \remark Configuration:   NA
 *
 * \param  E2E_DataPtr (in)        Current value over which the CRC is to be computed.
 * \param  E2E_ArrayLength (in) Length of array (data block) to be calculated in bytes.
 * \param  E2E_StartValue (in)  CRC value from the previous iteration XORed with 0xFF, or (2) 0xFF if it is the first run
 * \retval 8-bit result of calculation.
 * \Precondition   none
 * \Caveats        none
 *
 */

extern FUNC(uint8, AUTOMATIC) E2E_CRC8u16Array(P2CONST(uint16, AUTOMATIC,
                                                       E2E_APPL_DATA) E2E_DataPtr, VAR(uint32,AUTOMATIC) E2E_ArrayLength, VAR(uint8,
                                                                                                                              AUTOMATIC) E2E_StartValue);

/**
 * \brief E2E_CRC8u32Array
 *
 *
 * This function for computing CRC over uint32 array data types transmited with E2E
 * Protocol. The calculation is done in Least Significant Byte First, regardless
 * of the architecture of the microcontroller, because this is the byte order in
 * which data is transmitted over FlexRay, CAN and LIN.
 *
 * \ServID                    0x0C
 * \remark Sync/Async:      Synchronous
 * \remark Reentrancy:      Reentrant
 * \remark Configuration:   NA
 *
 * \param  E2E_DataPtr (in)        Current value over which the CRC is to be computed.
 * \param  E2E_ArrayLength (in) Length of array (data block) to be calculated in bytes.
 * \param  E2E_StartValue (in)  CRC value from the previous iteration XORed with 0xFF, or (2) 0xFF if it is the first run
 * \retval 8-bit result of calculation.
 * \Precondition   none
 * \Caveats        none
 *
 */

extern FUNC(uint8, AUTOMATIC) E2E_CRC8u32Array(P2CONST(uint32, AUTOMATIC,
                                                       E2E_APPL_DATA) E2E_DataPtr, VAR(uint32,AUTOMATIC) E2E_ArrayLength, VAR(uint8,
                                                                                                                              AUTOMATIC) E2E_StartValue);

/**
 * \brief E2E_CRC8H2Fu8
 *
 *
 * This function for computing CRC over uint8 data types transmited with E2E
 * Protocol. The calculation is done in Least Significant Byte First, regardless
 * of the architecture of the microcontroller, because this is the byte order in
 * which data is transmitted over FlexRay, CAN and LIN.
 * The function uses not the SAE polynomial, but 0x2F.
 *
 * \ServID                    0x07
 * \remark Sync/Async:      Synchronous
 * \remark Reentrancy:      Reentrant
 * \remark Configuration:   NA
 *
 * \param  E2E_Data (in)        Current value over which the CRC is to be computed.
 * \param  E2E_StartValue (in)  CRC value from the previous iteration XORed with 0xFF, or (2) 0xFF if it is the first run
 * \retval 8-bit result of calculation.
 * \Precondition   none
 * \Caveats        none
 *
 */

extern FUNC(uint8, AUTOMATIC) E2E_CRC8H2Fu8(VAR(uint8, AUTOMATIC) E2E_Data,
                                            VAR(uint8, AUTOMATIC) E2E_StartValue);

/**
 * \brief E2E_CRC8H2Fu16
 *
 *
 * This function for computing CRC over uint16 data types transmited with E2E
 * Protocol. The calculation is done in Least Significant Byte First, regardless
 * of the architecture of the microcontroller, because this is the byte order in
 * which data is transmitted over FlexRay, CAN and LIN.
 * The function uses not the SAE polynomial, but 0x2F.
 *
 * \ServID                  0x08
 * \remark Sync/Async:      Synchronous
 * \remark Reentrancy:      Reentrant
 * \remark Configuration:   NA
 *
 * \param  E2E_Data (in)        Current value over which the CRC is to be computed.
 * \param  E2E_StartValue (in)  CRC value from the previous iteration XORed with 0xFF, or (2) 0xFF if it is the first run
 * \retval 8-bit result of calculation.
 * \Precondition   none
 * \Caveats        none
 *
 */

extern FUNC(uint8, AUTOMATIC) E2E_CRC8H2Fu16(VAR(uint16, AUTOMATIC) E2E_Data,
                                             VAR(uint8, AUTOMATIC) E2E_StartValue);

/**
 * \brief E2E_CRC8H2Fu32
 *
 *
 * This function for computing CRC over uint32 data types transmited with E2E
 * Protocol. The calculation is done in Least Significant Byte First, regardless
 * of the architecture of the microcontroller, because this is the byte order in
 * which data is transmitted over FlexRay, CAN and LIN.
 * The function uses not the SAE polynomial, but 0x2F.
 *
 * \ServID                  0x09
 * \remark Sync/Async:      Synchronous
 * \remark Reentrancy:      Reentrant
 * \remark Configuration:   NA
 *
 * \param  E2E_Data (in)        Current value over which the CRC is to be computed.
 * \param  E2E_StartValue (in)  CRC value from the previous iteration XORed with 0xFF, or (2) 0xFF if it is the first run
 * \retval 8-bit result of calculation.
 * \Precondition   none
 * \Caveats        none
 *
 */

extern FUNC(uint8, AUTOMATIC) E2E_CRC8H2Fu32(VAR(uint32, AUTOMATIC) E2E_Data,
                                             VAR(uint8, AUTOMATIC) E2E_StartValue);

/**
 * \brief E2E_CRC8H2Fu8Array
 *
 *
 * This function for computing CRC over array of uint8 data types transmited with E2E
 * Protocol. The calculation is done in Least Significant Byte First, regardless
 * of the architecture of the microcontroller, because this is the byte order in
 * which data is transmitted over FlexRay, CAN and LIN.
 * The function uses not the SAE polynomial, but 0x2F.
 *
 * \ServID                    0x0A
 * \remark Sync/Async:      Synchronous
 * \remark Reentrancy:      Reentrant
 * \remark Configuration:   NA
 *
 * \param  E2E_DataPtr (in)        Current value over which the CRC is to be computed.
 * \param  E2E_ArrayLength (in) Length of array (data block) to be calculated in bytes.
 * \param  E2E_StartValue (in)  CRC value from the previous iteration XORed with 0xFF, or (2) 0xFF if it is the first run
 * \retval 8-bit result of calculation.
 * \Precondition   none
 * \Caveats        none
 *
 */
extern FUNC(uint8, AUTOMATIC) E2E_CRC8H2Fu8Array(P2CONST(uint8, AUTOMATIC,
                                                         E2E_APPL_DATA) E2E_DataPtr, VAR(uint32,AUTOMATIC) E2E_ArrayLength, VAR(uint8,
                                                                                                                                AUTOMATIC) E2E_StartValue);

/**
 * \brief E2E_CRC8H2Fu16Array
 *
 *
 * This function for computing CRC over array of uint16 data types transmited with E2E
 * Protocol. The calculation is done in Least Significant Byte First, regardless
 * of the architecture of the microcontroller, because this is the byte order in
 * which data is transmitted over FlexRay, CAN and LIN.
 * The function uses not the SAE polynomial, but 0x2F.
 *
 * \ServID                    0x0B
 * \remark Sync/Async:      Synchronous
 * \remark Reentrancy:      Reentrant
 * \remark Configuration:   NA
 *
 * \param  E2E_DataPtr (in)        Current value over which the CRC is to be computed.
 * \param  E2E_ArrayLength (in) Length of array (data block) to be calculated in bytes.
 * \param  E2E_StartValue (in)  CRC value from the previous iteration XORed with 0xFF, or (2) 0xFF if it is the first run
 * \retval 8-bit result of calculation.
 * \Precondition   none
 * \Caveats        none
 *
 */

extern FUNC(uint8, AUTOMATIC) E2E_CRC8H2Fu16Array(P2CONST(uint16, AUTOMATIC,
                                                          E2E_APPL_DATA) E2E_DataPtr, VAR(uint32,AUTOMATIC) E2E_ArrayLength, VAR(uint8,
                                                                                                                                 AUTOMATIC) E2E_StartValue);

/**
 * \brief E2E_CRC8H2Fu32Array
 *
 *
 * This function for computing CRC over array of uint32 data types transmited with E2E
 * Protocol. The calculation is done in Least Significant Byte First, regardless
 * of the architecture of the microcontroller, because this is the byte order in
 * which data is transmitted over FlexRay, CAN and LIN.
 * The function uses not the SAE polynomial, but 0x2F.
 *
 * \ServID                    0x0C
 * \remark Sync/Async:      Synchronous
 * \remark Reentrancy:      Reentrant
 * \remark Configuration:   NA
 *
 * \param  E2E_DataPtr (in)        Current value over which the CRC is to be computed.
 * \param  E2E_ArrayLength (in) Length of array (data block) to be calculated in bytes.
 * \param  E2E_StartValue (in)  CRC value from the previous iteration XORed with 0xFF, or (2) 0xFF if it is the first run
 * \retval 8-bit result of calculation.
 * \Precondition   none
 * \Caveats        none
 *
 */

extern FUNC(uint8, AUTOMATIC) E2E_CRC8H2Fu32Array(P2CONST(uint32, AUTOMATIC,
                                                          E2E_APPL_DATA) E2E_DataPtr, VAR(uint32,AUTOMATIC) E2E_ArrayLength, VAR(uint8,
                                                                                                                                 AUTOMATIC) E2E_StartValue);
/**
 * \brief E2E_UpdateCounter
 *
 *
 * This function increments the counter value provided by the parameter, and returns
 * it by return value.
 *
 * \ServID                  0x13
 * \remark Sync/Async:      Synchronous
 * \remark Reentrancy:      Reentrant
 * \remark Configuration:   NA
 *
 * \param  Counter (in)        Counter value, to be incremented.
 * \retval 8-bit result of counter value.
 * \Precondition   none
 * \Caveats        none
 *
 */

extern FUNC(uint8, AUTOMATIC) E2E_UpdateCounter(VAR(uint8, AUTOMATIC) Counter);

/**
 * Mark end of memory area for code
 */
/* PRQA S 5087 ++
 * MISRA RULE C197 VIOLATION:
 */
#define  E2E_STOP_SEC_CODE
#include "MemMap.h"
/* PRQA S 5087 -- */

#endif   /* #ifdef E2E_H */
/* END OF FILE -------------------------------------------------------------- */
