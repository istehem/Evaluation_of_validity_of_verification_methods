/**\file
*
* \brief   CRC Module Source File for Picea
*
*    This file contains the functionality of the public interface of the
*    CRC module for Picea.\n
*
* \b Application:        CRC \n
* \b Target:             see Crc.h\n
* \b Compiler:           see Crc.h\n
* \b Autosar-Vendor-ID:  see Crc.h\n
* \n
* \b Module:             %name:  Crc.c % \n
* \b File-Revision:      %version: 11 %  \n
* \b Changeable-by-user: No \n
* \b Delivery-File:      No \n
* \n
* \b Module-Owner:       see Crc.h \n
* \n
* \b Traceability-Info   see crc.h \n
* \b Classification:     Confidential \n
* \b Deviations:         None \n
* \n
* \b Requirement(s):
* \reqtrace{DSAR-SDD-CRC-17-2}{CRC022,CRC024
 *                             CRC023}
*/

/*
 *
 *      Copyright Mecel AB
 *      Box 14044
 *      400 20 GOTHENBURG
 *      SWEDEN
 *      Phone: +46 (0)31 720 44 00
 *
 *      The copyright of the computer program(s) herein is the property of
 *      Mecel AB. The programs(s) may be used and/or copied only with the
 *      written permission of Mecel AB or in accordance with the terms and
 *      conditions stipulated in the agreement/contract under which the
 *      program(s) have been supplied.
 *
 ******************************************************************************/

/*============================================================================*\
* PREPROCESSOR DIRECTIVES
\*============================================================================*/

/* INCLUDE DIRECTIVES FOR STANDARD HEADERS -----------------------------------*/

/* INCLUDE DIRECTIVES FOR OTHER HEADERS --------------------------------------*/

#include "Crc.h"


#define EXPECTED_CRC_SW_MAJOR_VERSION     (5)
#define EXPECTED_CRC_SW_MINOR_VERSION     (0)
#define EXPECTED_CRC_SW_PATCH_VERSION     (1)


#if (   (CRC_SW_MAJOR_VERSION != EXPECTED_CRC_SW_MAJOR_VERSION)         \
|| (CRC_SW_MINOR_VERSION != EXPECTED_CRC_SW_MINOR_VERSION)         \
|| (CRC_SW_PATCH_VERSION != EXPECTED_CRC_SW_PATCH_VERSION)         \
)
#error "The version of the Crc.c file does not match the version of the \
Crc.h file"
#endif


#if (   (CRC_CFG_SW_MAJOR_VERSION != EXPECTED_CRC_SW_MAJOR_VERSION)         \
|| (CRC_CFG_SW_MINOR_VERSION != EXPECTED_CRC_SW_MINOR_VERSION)         \
|| (CRC_CFG_SW_PATCH_VERSION != EXPECTED_CRC_SW_PATCH_VERSION)         \
)
#error "The version of the Crc.c file does not match the version of the \
Crc_Cfg.h file"
#endif

/* LOCAL DEFINES FOR CONSTANTS -----------------------------------------------*/

/**
* Polynomial for 8-bit calculation.
* \Requirements
* \latexonly
* \reqtrace{DSAR-SDD-CRC-44-1}{CRC030}
* \endlatexonly
*/
#define CRC_POLYNOMIAL_8    ((uint8)0x1Du)

/**
* Polynomial for 8-bit calculation.
* \Requirements
* \latexonly
* \reqtrace{DSAR-SDD-CRC-53-1}{CRC042}
* \endlatexonly
*/
#define CRC_POLYNOMIAL_8H2F    ((uint8)0x2Fu)

/**
* Polynomial for 16-bit calculation.
* \Requirements
* \latexonly
* \reqtrace{DSAR-SDD-CRC-45-1}{CRC002}
* \endlatexonly
*/

#define  CRC_POLYNOMIAL_16            ((uint16)0x1021u)
/**
* Polynomial for 32-bit calculation.
* \Requirements
* \latexonly
* \reqtrace{DSAR-SDD-CRC-46-1}{CRC003}
* \endlatexonly
*/

#define  CRC_POLYNOMIAL_32            ((uint32)0x04C11DB7u)

/**
* XOR Value for 8-bit calculation.
* \Requirements
* \latexonly
* \reqtrace{DSAR-SDD-CRC-47-1}{CRC030}
* \endlatexonly
*/
#define CRC_XORVALUE8  ((uint8)0xFFu)

/**
* XOR Value for 8-bit calculation.
* \Requirements
* \latexonly
* \reqtrace{DSAR-SDD-CRC-54-1}{CRC042}
* \endlatexonly
*/
#define CRC_XORVALUE8H2F  ((uint8)0xFFu)

/**
* XOR Value for 16-bit calculation.
* \Requirements
* \latexonly
* \reqtrace{DSAR-SDD-CRC-48-1}{CRC002}
* \endlatexonly
*/
#define  CRC_XORVALUE16      ((uint16)0x0000u)

/**
* XOR Value for 32-bit calculation.
* \Requirements
* \latexonly
* \reqtrace{DSAR-SDD-CRC-49-1}{CRC003}
* \endlatexonly
*/
#define  CRC_XORVALUE32      ((uint32)0xFFFFFFFFu)

/**
* Size of the lookup table for calculation .
* \Requirements
* \latexonly
* \reqtrace{DSAR-SDD-CRC-50-1}{CRC025,CRC026,
*                              CRC027,CRC030,
*                              CRC030,CRC31}
* \endlatexonly
*/
#define  CRC_TABLE_SIZE      ((uint16)256u)

/* auxliary defines for CRC calculation
* used for shifting variables
*/
#define  CRC_SHIFT_24        ((uint8)24u)
#define  CRC_SHIFT_8         ((uint8)8u)
#define  CRC_SHIFT_1         ((uint8)1u)

/** mask for 8-bit Crc calculation */
#define CRC8MASK   ((uint8)0x80)

/* mask for 16-bit Crc calculation */
#define  CRC16MASK       ((uint16)0x8000u)

/* mask for 32-bit Crc calculation */
#define  CRC32MASK       ((uint32)0x80000000u)

/* constant used for masking 8 bits of variable */
#define  CRC_MASK_8BITS      ((uint8)0xFFu)

/*
* defines for reflect function - describe how many bits has to be
* reflected
*/
#define  CRC_REF_8           ((uint8)8u)
#define  CRC_REF_32          ((uint8)32u)

/* zeroi length define */
#define  CRC_ZERO_LENGTH      ((uint8)0u)

/* bit indicator defines */
#define  CRC_BIT_0            ((uint8)0u)
#define  CRC_BIT_8            ((uint8)8u)

/* LOCAL DEFINE MACROS (#, ##) -----------------------------------------------*/

/*============================================================================*\
* LOCAL TYPEDEF DECLARATIONS
\*============================================================================*/

/* ENUMS ---------------------------------------------------------------------*/

/* STRUCTS -------------------------------------------------------------------*/

/* UNIONS --------------------------------------------------------------------*/

/* OTHER TYPEDEFS ------------------------------------------------------------*/

/*============================================================================*\
* OBJECT DEFINITIONS
\*============================================================================*/

/* EXPORTED OBJECTS ----------------------------------------------------------*/

/* LOCAL OBJECTS -------------------------------------------------------------*/

/* PRQA S 3218 ++
* MISRA RULE 8.7 VIOLATION:
* Tables below can not be placed in function body because of large function
* operator/statements amounts
*/

/* PRQA S 5087 ++
* MISRA RULE C197 VIOLATION:
*/
#define CRC_START_SEC_CONST_8BIT
#include "MemMap.h"
/* PRQA S 5087 -- */
/*
 * Global variable for storing Crc8 SAE J1850 result
*/

static VAR(uint8, CRC_VAR_NOINIT) Crc_Result8;

/*
* Look up  table for CRC_8 bit table driven Calculation
*/
static CONST(uint8, CRC_CONST)  CRC_Polynom8_Table[CRC_TABLE_SIZE]=
{
0x00,   0x1d,   0x3a,   0x27,   0x74,   0x69,   0x4e,   0x53,   0xe8,   0xf5,
0xd2,   0xcf,   0x9c,   0x81,   0xa6,   0xbb,   0xcd,   0xd0,   0xf7,   0xea,
0xb9,   0xa4,   0x83,   0x9e,   0x25,   0x38,   0x1f,   0x02,   0x51,   0x4c,
0x6b,   0x76,   0x87,   0x9a,   0xbd,   0xa0,   0xf3,   0xee,   0xc9,   0xd4,
0x6f,   0x72,   0x55,   0x48,   0x1b,   0x06,   0x21,   0x3c,   0x4a,   0x57,
0x70,   0x6d,   0x3e,   0x23,   0x04,   0x19,   0xa2,   0xbf,   0x98,   0x85,
0xd6,   0xcb,   0xec,   0xf1,   0x13,   0x0e,   0x29,   0x34,   0x67,   0x7a,
0x5d,   0x40,   0xfb,   0xe6,   0xc1,   0xdc,   0x8f,   0x92,   0xb5,   0xa8,
0xde,   0xc3,   0xe4,   0xf9,   0xaa,   0xb7,   0x90,   0x8d,   0x36,   0x2b,
0x0c,   0x11,   0x42,   0x5f,   0x78,   0x65,   0x94,   0x89,   0xae,   0xb3,
0xe0,   0xfd,   0xda,   0xc7,   0x7c,   0x61,   0x46,   0x5b,   0x08,   0x15,
0x32,   0x2f,   0x59,   0x44,   0x63,   0x7e,   0x2d,   0x30,   0x17,   0x0a,
0xb1,   0xac,   0x8b,   0x96,   0xc5,   0xd8,   0xff,   0xe2,   0x26,   0x3b,
0x1c,   0x01,   0x52,   0x4f,   0x68,   0x75,   0xce,   0xd3,   0xf4,   0xe9,
0xba,   0xa7,   0x80,   0x9d,   0xeb,   0xf6,   0xd1,   0xcc,   0x9f,   0x82,
0xa5,   0xb8,   0x03,   0x1e,   0x39,   0x24,   0x77,   0x6a,   0x4d,   0x50,
0xa1,   0xbc,   0x9b,   0x86,   0xd5,   0xc8,   0xef,   0xf2,   0x49,   0x54,
0x73,   0x6e,   0x3d,   0x20,   0x07,   0x1a,   0x6c,   0x71,   0x56,   0x4b,
0x18,   0x05,   0x22,   0x3f,   0x84,   0x99,   0xbe,   0xa3,   0xf0,   0xed,
0xca,   0xd7,   0x35,   0x28,   0x0f,   0x12,   0x41,   0x5c,   0x7b,   0x66,
0xdd,   0xc0,   0xe7,   0xfa,   0xa9,   0xb4,   0x93,   0x8e,   0xf8,   0xe5,
0xc2,   0xdf,   0x8c,   0x91,   0xb6,   0xab,   0x10,   0x0d,   0x2a,   0x37,
0x64,   0x79,   0x5e,   0x43,   0xb2,   0xaf,   0x88,   0x95,   0xc6,   0xdb,
0xfc,   0xe1,   0x5a,   0x47,   0x60,   0x7d,   0x2e,   0x33,   0x14,   0x09,
0x7f,   0x62,   0x45,   0x58,   0x0b,   0x16,   0x31,   0x2c,   0x97,   0x8a,
0xad,   0xb0,   0xe3,   0xfe,   0xd9,   0xc4
};

/*
 * Global variable for storing Crc8H2F result
*/

static VAR(uint8, CRC_VAR_NOINIT) Crc_Result8H2F;
/*
* Look up  table for CRC_8H2F bit table driven Calculation
*/
static CONST(uint8, CRC_CONST)  CRC_Polynom8H2F_Table[CRC_TABLE_SIZE]=
{
    0x00, 0x2f, 0x5e, 0x71, 0xbc, 0x93, 0xe2, 0xcd, 0x57, 0x78, 0x09, 0x26, 0xeb, 0xc4, 0xb5, 0x9a,
    0xae, 0x81, 0xf0, 0xdf, 0x12, 0x3d, 0x4c, 0x63, 0xf9, 0xd6, 0xa7, 0x88, 0x45, 0x6a, 0x1b, 0x34,
    0x73, 0x5c, 0x2d, 0x02, 0xcf, 0xe0, 0x91, 0xbe, 0x24, 0x0b, 0x7a, 0x55, 0x98, 0xb7, 0xc6, 0xe9,
    0xdd, 0xf2, 0x83, 0xac, 0x61, 0x4e, 0x3f, 0x10, 0x8a, 0xa5, 0xd4, 0xfb, 0x36, 0x19, 0x68, 0x47,
    0xe6, 0xc9, 0xb8, 0x97, 0x5a, 0x75, 0x04, 0x2b, 0xb1, 0x9e, 0xef, 0xc0, 0x0d, 0x22, 0x53, 0x7c,
    0x48, 0x67, 0x16, 0x39, 0xf4, 0xdb, 0xaa, 0x85, 0x1f, 0x30, 0x41, 0x6e, 0xa3, 0x8c, 0xfd, 0xd2,
    0x95, 0xba, 0xcb, 0xe4, 0x29, 0x06, 0x77, 0x58, 0xc2, 0xed, 0x9c, 0xb3, 0x7e, 0x51, 0x20, 0x0f,
    0x3b, 0x14, 0x65, 0x4a, 0x87, 0xa8, 0xd9, 0xf6, 0x6c, 0x43, 0x32, 0x1d, 0xd0, 0xff, 0x8e, 0xa1,
    0xe3, 0xcc, 0xbd, 0x92, 0x5f, 0x70, 0x01, 0x2e, 0xb4, 0x9b, 0xea, 0xc5, 0x08, 0x27, 0x56, 0x79,
    0x4d, 0x62, 0x13, 0x3c, 0xf1, 0xde, 0xaf, 0x80, 0x1a, 0x35, 0x44, 0x6b, 0xa6, 0x89, 0xf8, 0xd7,
    0x90, 0xbf, 0xce, 0xe1, 0x2c, 0x03, 0x72, 0x5d, 0xc7, 0xe8, 0x99, 0xb6, 0x7b, 0x54, 0x25, 0x0a,
    0x3e, 0x11, 0x60, 0x4f, 0x82, 0xad, 0xdc, 0xf3, 0x69, 0x46, 0x37, 0x18, 0xd5, 0xfa, 0x8b, 0xa4,
    0x05, 0x2a, 0x5b, 0x74, 0xb9, 0x96, 0xe7, 0xc8, 0x52, 0x7d, 0x0c, 0x23, 0xee, 0xc1, 0xb0, 0x9f,
    0xab, 0x84, 0xf5, 0xda, 0x17, 0x38, 0x49, 0x66, 0xfc, 0xd3, 0xa2, 0x8d, 0x40, 0x6f, 0x1e, 0x31,
    0x76, 0x59, 0x28, 0x07, 0xca, 0xe5, 0x94, 0xbb, 0x21, 0x0e, 0x7f, 0x50, 0x9d, 0xb2, 0xc3, 0xec,
    0xd8, 0xf7, 0x86, 0xa9, 0x64, 0x4b, 0x3a, 0x15, 0x8f, 0xa0, 0xd1, 0xfe, 0x33, 0x1c, 0x6d, 0x42

};

/* PRQA S 5087 ++
* MISRA RULE C197 VIOLATION:
*/
#define CRC_STOP_SEC_CONST_8BIT
#include "MemMap.h"
/* PRQA S 5087 -- */


/* PRQA S 5087 ++ */
#define CRC_START_SEC_CONST_16BIT
#include "MemMap.h"
/* PRQA S 5087 -- */

/*
 * Global variable for storing Crc16 result
*/

static VAR(uint16, CRC_VAR_NOINIT) Crc_Result16;

/*
* Look up  table for CRC_16 bit table driven Calculation
*/
/* PRQA S 1281 ++ */
static CONST(uint16, CRC_CONST)  CRC_Polynom16_Table[CRC_TABLE_SIZE]=
{
0x0000,0x1021,0x2042,0x3063,0x4084,0x50A5,0x60C6,0x70E7,
0x8108,0x9129,0xA14A,0xB16B,0xC18C,0xD1AD,0xE1CE,0xF1EF,
0x1231,0x0210,0x3273,0x2252,0x52B5,0x4294,0x72F7,0x62D6,
0x9339,0x8318,0xB37B,0xA35A,0xD3BD,0xC39C,0xF3FF,0xE3DE,
0x2462,0x3443,0x0420,0x1401,0x64E6,0x74C7,0x44A4,0x5485,
0xA56A,0xB54B,0x8528,0x9509,0xE5EE,0xF5CF,0xC5AC,0xD58D,
0x3653,0x2672,0x1611,0x0630,0x76D7,0x66F6,0x5695,0x46B4,
0xB75B,0xA77A,0x9719,0x8738,0xF7DF,0xE7FE,0xD79D,0xC7BC,
0x48C4,0x58E5,0x6886,0x78A7,0x0840,0x1861,0x2802,0x3823,
0xC9CC,0xD9ED,0xE98E,0xF9AF,0x8948,0x9969,0xA90A,0xB92B,
0x5AF5,0x4AD4,0x7AB7,0x6A96,0x1A71,0x0A50,0x3A33,0x2A12,
0xDBFD,0xCBDC,0xFBBF,0xEB9E,0x9B79,0x8B58,0xBB3B,0xAB1A,
0x6CA6,0x7C87,0x4CE4,0x5CC5,0x2C22,0x3C03,0x0C60,0x1C41,
0xEDAE,0xFD8F,0xCDEC,0xDDCD,0xAD2A,0xBD0B,0x8D68,0x9D49,
0x7E97,0x6EB6,0x5ED5,0x4EF4,0x3E13,0x2E32,0x1E51,0x0E70,
0xFF9F,0xEFBE,0xDFDD,0xCFFC,0xBF1B,0xAF3A,0x9F59,0x8F78,
0x9188,0x81A9,0xB1CA,0xA1EB,0xD10C,0xC12D,0xF14E,0xE16F,
0x1080,0x00A1,0x30C2,0x20E3,0x5004,0x4025,0x7046,0x6067,
0x83B9,0x9398,0xA3FB,0xB3DA,0xC33D,0xD31C,0xE37F,0xF35E,
0x02B1,0x1290,0x22F3,0x32D2,0x4235,0x5214,0x6277,0x7256,
0xB5EA,0xA5CB,0x95A8,0x8589,0xF56E,0xE54F,0xD52C,0xC50D,
0x34E2,0x24C3,0x14A0,0x0481,0x7466,0x6447,0x5424,0x4405,
0xA7DB,0xB7FA,0x8799,0x97B8,0xE75F,0xF77E,0xC71D,0xD73C,
0x26D3,0x36F2,0x0691,0x16B0,0x6657,0x7676,0x4615,0x5634,
0xD94C,0xC96D,0xF90E,0xE92F,0x99C8,0x89E9,0xB98A,0xA9AB,
0x5844,0x4865,0x7806,0x6827,0x18C0,0x08E1,0x3882,0x28A3,
0xCB7D,0xDB5C,0xEB3F,0xFB1E,0x8BF9,0x9BD8,0xABBB,0xBB9A,
0x4A75,0x5A54,0x6A37,0x7A16,0x0AF1,0x1AD0,0x2AB3,0x3A92,
0xFD2E,0xED0F,0xDD6C,0xCD4D,0xBDAA,0xAD8B,0x9DE8,0x8DC9,
0x7C26,0x6C07,0x5C64,0x4C45,0x3CA2,0x2C83,0x1CE0,0x0CC1,
0xEF1F,0xFF3E,0xCF5D,0xDF7C,0xAF9B,0xBFBA,0x8FD9,0x9FF8,
0x6E17,0x7E36,0x4E55,0x5E74,0x2E93,0x3EB2,0x0ED1,0x1EF0
};
/* PRQA S 1281 -- */
/* PRQA S 5087 ++
* MISRA RULE C197 VIOLATION:
*/
#define CRC_STOP_SEC_CONST_16BIT
#include "MemMap.h"
/* PRQA S 5087 -- */

/* PRQA S 5087 ++
* MISRA RULE C197 VIOLATION:
*/
#define CRC_START_SEC_CONST_32BIT
#include "MemMap.h"
/* PRQA S 5087 -- */

/*
 * Global variable for storing Crc32 result
*/
static VAR(uint32, CRC_VAR_NOINIT) Crc_Result32;

/*
* Look up  table for CRC_32 bit table driven Calculation
*/
/* PRQA S 1281 ++ */
static CONST(uint32, CRC_CONST) CRC_Polynom32_Table[CRC_TABLE_SIZE]=
{
0x00000000, 0x77073096, 0xee0e612c, 0x990951ba,0x076dc419, 0x706af48f,
0xe963a535, 0x9e6495a3, 0x0edb8832, 0x79dcb8a4,0xe0d5e91e, 0x97d2d988,
0x09b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91,0x1db71064, 0x6ab020f2,
0xf3b97148, 0x84be41de, 0x1adad47d, 0x6ddde4eb,0xf4d4b551, 0x83d385c7,
0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec,0x14015c4f, 0x63066cd9,
0xfa0f3d63, 0x8d080df5, 0x3b6e20c8, 0x4c69105e,0xd56041e4, 0xa2677172,
0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b,0x35b5a8fa, 0x42b2986c,
0xdbbbc9d6, 0xacbcf940, 0x32d86ce3, 0x45df5c75,0xdcd60dcf, 0xabd13d59,
0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116,0x21b4f4b5, 0x56b3c423,
0xcfba9599, 0xb8bda50f, 0x2802b89e, 0x5f058808,0xc60cd9b2, 0xb10be924,
0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d,0x76dc4190, 0x01db7106,
0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f,0x9fbfe4a5, 0xe8b8d433,
0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818,0x7f6a0dbb, 0x086d3d2d,
0x91646c97, 0xe6635c01, 0x6b6b51f4, 0x1c6c6162,0x856530d8, 0xf262004e,
0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457,0x65b0d9c6, 0x12b7e950,
0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49,0x8cd37cf3, 0xfbd44c65,
0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2,0x4adfa541, 0x3dd895d7,
0xa4d1c46d, 0xd3d6f4fb, 0x4369e96a, 0x346ed9fc,0xad678846, 0xda60b8d0,
0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9,0x5005713c, 0x270241aa,
0xbe0b1010, 0xc90c2086, 0x5768b525, 0x206f85b3,0xb966d409, 0xce61e49f,
0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4,0x59b33d17, 0x2eb40d81,
0xb7bd5c3b, 0xc0ba6cad, 0xedb88320, 0x9abfb3b6,0x03b6e20c, 0x74b1d29a,
0xead54739, 0x9dd277af, 0x04db2615, 0x73dc1683,0xe3630b12, 0x94643b84,
0x0d6d6a3e, 0x7a6a5aa8, 0xe40ecf0b, 0x9309ff9d,0x0a00ae27, 0x7d079eb1,
0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe,0xf762575d, 0x806567cb,
0x196c3671, 0x6e6b06e7, 0xfed41b76, 0x89d32be0,0x10da7a5a, 0x67dd4acc,
0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5,0xd6d6a3e8, 0xa1d1937e,
0x38d8c2c4, 0x4fdff252, 0xd1bb67f1, 0xa6bc5767,0x3fb506dd, 0x48b2364b,
0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60,0xdf60efc3, 0xa867df55,
0x316e8eef, 0x4669be79, 0xcb61b38c, 0xbc66831a,0x256fd2a0, 0x5268e236,
0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f,0xc5ba3bbe, 0xb2bd0b28,
0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31,0x2cd99e8b, 0x5bdeae1d,
0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a,0x9c0906a9, 0xeb0e363f,
0x72076785, 0x05005713, 0x95bf4a82, 0xe2b87a14,0x7bb12bae, 0x0cb61b38,
0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21,0x86d3d2d4, 0xf1d4e242,
0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b,0x6fb077e1, 0x18b74777,
0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c,0x8f659eff, 0xf862ae69,
0x616bffd3, 0x166ccf45, 0xa00ae278, 0xd70dd2ee,0x4e048354, 0x3903b3c2,
0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db,0xaed16a4a, 0xd9d65adc,
0x40df0b66, 0x37d83bf0, 0xa9bcae53, 0xdebb9ec5,0x47b2cf7f, 0x30b5ffe9,
0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6,0xbad03605, 0xcdd70693,
0x54de5729, 0x23d967bf, 0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94,
0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d
};
/* PRQA S 1281 -- */
/* PRQA S 5087 ++
* MISRA RULE C197 VIOLATION:
*/
#define CRC_STOP_SEC_CONST_32BIT
#include "MemMap.h"
/* PRQA S 5087 -- */

/* PRQA S 3218 -- */

/*============================================================================*\
* LOCAL FUNCTION PROTOTYPES
\*============================================================================*/

/* PRQA S 5087 ++
* MISRA RULE C197 VIOLATION:
*/
#define  CRC_START_SEC_CODE
#include "MemMap.h"
/* PRQA S 5087 -- */

/**
* \brief          Function performs 8-bit SAE J1850 CRC calculation without lookup table
* \param[in]      Crc_DataPtr - Pointer to start address of data block
*                 to be calculated
* \param[in]      Crc_Length  - Length of data blocks to be calculated in bytes
* \param[in]      Crc_StartValue8 - Initial Value for CRC algorithm
* \param[in]      Crc_IsFirstCall - Boolean value which tells is first call in as sequence of ividual CRC calculation
* \return         8-bit result of calculation
* \Reentrancy     reentrant
* \Synchronism    synchronous
* \Precondition   none
* \Caveats        none
* \Requirements
* \latexonly
* \DSARfigure{crc_8_runtime.jpg}{260pt}{fig:crc_8_runtime}{Runtime CRC8 calculation}
* \endlatexonly
*/
#if (CRC_8_MODE == CRC_8_RUNTIME)
static FUNC(uint8, CRC_CODE) CalculateCRC8_Runtime
(
P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST) Crc_DataPtr,
VAR(uint32, AUTOMATIC)                   Crc_Length,
VAR(uint8, AUTOMATIC)                   Crc_StartValue8
);
#endif

/**
* \brief          Function performs 8-bit SAE J1850 CRC calculation with lookup table
* \param[in]      Crc_DataPtr - Pointer to start address of data block to be calculated
* \param[in]      Crc_Length  - Length of data blocks to be calculated in bytes
* \param[in]      Crc_StartValue8 - Initial Value for CRC algorithm
* \param[in]      Crc_IsFirstCall - Boolean value which tells is first call in as sequence of ividual CRC calculation
* \return         8-bit result of calculation
* \Reentrancy     reentrant
* \Synchronism    synchronous
* \Precondition   none
* \Caveats        none
* \Requirements
* \latexonly
* \DSARfigure{crc_table.jpg}{260pt}{fig:crc_table}{Lookup table based CRC8 calculation}
* \endlatexonly
*/
#if (CRC_8_MODE == CRC_8_TABLE)
static FUNC(uint8, CRC_CODE) CalculateCRC8_Table
(
P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST)    Crc_DataPtr,
VAR(uint32, AUTOMATIC)                   Crc_Length,
VAR(uint8, AUTOMATIC)                   Crc_StartValue8
);
#endif


/**
* \brief          Function performs 8-bit 0x2F polynomial CRC calculation without lookup table
* \param[in]      Crc_DataPtr - Pointer to start address of data block
*                 to be calculated
* \param[in]      Crc_Length  - Length of data blocks to be calculated in bytes
* \param[in]      Crc_StartValue8 - Initial Value for CRC algorithm
* \param[in]      Crc_IsFirstCall - Boolean value which tells is first call in as sequence of ividual CRC calculation
* \return         8-bit result of calculation
* \Reentrancy     reentrant
* \Synchronism    synchronous
* \Precondition   none
* \Caveats        none
* \Requirements
* \latexonly
* \DSARfigure{crc_8H2F_runtime.jpg}{260pt}{fig:crc_8_runtime}{Runtime CRC8H2F calculation}
* \endlatexonly
*/
#if (CRC_8H2F_MODE == CRC_8H2F_RUNTIME)
static FUNC(uint8, CRC_CODE) CalculateCRC8H2F_Runtime
(
P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST) Crc_DataPtr,
VAR(uint32, AUTOMATIC)                   Crc_Length,
VAR(uint8, AUTOMATIC)                   Crc_StartValue8H2F
);
#endif

/**
* \brief          Function performs 8-bit 0x2F polynomial CRC calculation with lookup table
* \param[in]      Crc_DataPtr - Pointer to start address of data block to be calculated
* \param[in]      Crc_Length  - Length of data blocks to be calculated in bytes
* \param[in]      Crc_StartValue8 - Initial Value for CRC algorithm
* \param[in]      Crc_IsFirstCall - Boolean value which tells is first call in as sequence of ividual CRC calculation
* \return         8-bit result of calculation
* \Reentrancy     reentrant
* \Synchronism    synchronous
* \Precondition   none
* \Caveats        none
* \Requirements
* \latexonly
* \DSARfigure{crc_table.jpg}{260pt}{fig:crc_table}{Lookup table based CRC8H2F calculation}
* \endlatexonly
*/
#if (CRC_8H2F_MODE == CRC_8H2F_TABLE)
static FUNC(uint8, CRC_CODE) CalculateCRC8H2F_Table
(
P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST)    Crc_DataPtr,
VAR(uint32, AUTOMATIC)                   Crc_Length,
VAR(uint8, AUTOMATIC)                   Crc_StartValue8H2F
);
#endif
/**
* \brief          Function performs 16-bit CRC calculation without lookup table
* \param[in]      Crc_DataPtr - Pointer to start address of data block
*                 to be calculated
* \param[in]      Crc_Length  - Length of data blocks to be calculated in bytes
* \param[in]      Crc_StartValue16 - Initial Value for CRC algorithm
* \param[in]      Crc_IsFirstCall - Boolean value which tells is first call in as sequence of ividual CRC calculation
* \return         16-bit result of calculation
* \Reentrancy     reentrant
* \Synchronism    synchronous
* \Precondition   none
* \Caveats        none
* \Requirements
* \latexonly
* \DSARfigure{crc_16_runtime.jpg}{260pt}{fig:crc_16_runtime}{Runtime Crc16 calculation}
*
* \endlatexonly
*/
#if (CRC_16_MODE == CRC_16_RUNTIME)
static FUNC(uint16, CRC_CODE) CalculateCRC16_Runtime
(
P2CONST(uint8, AUTOMATIC,CRC_APPL_CONST)    Crc_DataPtr,
VAR(uint32, AUTOMATIC)                   Crc_Length,
VAR(uint16, AUTOMATIC)                   Crc_StartValue16
);
#endif

/**
* \brief          Function performs 16-bit CRC calculation with lookup table
* \param[in]      Crc_DataPtr - Pointer to start address of data block to be calculated
* \param[in]      Crc_Length  - Length of data blocks to be calculated in bytes
* \param[in]      Crc_StartValue16 - Initial Value for CRC algorithm
* \param[in]      Crc_IsFirstCall - Boolean value which tells is first call in as sequence of ividual CRC calculation
* \return         16-bit result of calculation
* \Reentrancy     reentrant
* \Synchronism    synchronous
* \Precondition   none
* \Caveats        none
* \Requirements
* \latexonly
* \DSARfigure{crc_table.jpg}{260pt}{fig:crc_16_table}{Lookup table based Crc16 calculation}
* \endlatexonly
*/
#if (CRC_16_MODE == CRC_16_TABLE)
static FUNC(uint16, CRC_CODE) CalculateCRC16_Table
(
P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST)    Crc_DataPtr,
VAR(uint32, AUTOMATIC)                   Crc_Length,
VAR(uint16, AUTOMATIC)                   Crc_StartValue16
);
#endif

/**
* \brief          Function performs 32-bit CRC calculation without lookup table
* \param[in]      Crc_DataPtr - Pointer to start address of data block to be calculated
* \param[in]      Crc_Length  - Length of data blocks to be calculated in bytes
* \param[in]      Crc_StartValue16 - Initial Value for CRC algorithm
* \param[in]      Crc_IsFirstCall - Boolean value which tells is first call in as sequence of ividual CRC calculation
* \return         32-bit result of calculation
* \Reentrancy     reentrant
* \Synchronism    synchronous
* \Precondition   none
* \Caveats        none
* \Requirements
* \latexonly
* \DSARfigure{crc_table.jpg}{260pt}{fig:crc_32_table}{Lookup table based Crc32 calculation}
*
* \endlatexonly
*/
#if (CRC_32_MODE == CRC_32_TABLE)
static FUNC(uint32, CRC_CODE) CalculateCRC32_Table
(
P2CONST(uint8, AUTOMATIC,CRC_APPL_CONST)     Crc_DataPtr,
VAR(uint32, AUTOMATIC)                    Crc_Length,
VAR(uint32, AUTOMATIC)                    Crc_StartValue32
);
#endif

/**
* \brief          Function performs 32-bit CRC calculation with lookup table
* \param[in]      Crc_DataPtr - Pointer to start address of data block to be calculated
* \param[in]      Crc_Length  - Length of data blocks to be calculated in bytes
* \param[in]      Crc_StartValue16 - Initial Value for CRC algorithm
* \param[in]      Crc_IsFirstCall - Boolean value which tells is first call in as sequence of ividual CRC calculation
* \return         32-bit result of calculation
* \Reentrancy     reentrant
* \Synchronism    synchronous
* \Precondition   none
* \Caveats        none
* \Requirements
* \latexonly
* \DSARfigure{crc_32_runtime.jpg}{260pt}{fig:crc_32_runtime}{Runtime CRC32 calculation}
* \endlatexonly
*/
#if (CRC_32_MODE == CRC_32_RUNTIME)
static FUNC(uint32, CRC_CODE) CalculateCRC32_Runtime
(
P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST)      Crc_DataPtr,
VAR(uint32, AUTOMATIC)                   Crc_Length,
VAR(uint32, AUTOMATIC)                   Crc_StartValue32
);
#endif

/**
* \brief          Function reflect selected data at selected amounts of bites
* \param[in]      data -  data to reflection
* \param[in]      bitamounts - amount of bits which has to be reflected
*                              counting from LSB
* \return         reflected data
* \Reentrancy     reentrant
* \Synchronism    synchronous
* \Precondition   none
* \Caveats        none
*/
#if (CRC_32_MODE == CRC_32_RUNTIME)
static FUNC(uint32, CRC_CODE)  ReflectValue
(
VAR(uint32, AUTOMATIC)                   data,
VAR(uint8, AUTOMATIC)                    bitamounts
);
#endif

/*============================================================================*\
* LOCAL FUNCTION-LIKE-MACROS and INLINE FUNCTIONS
\*============================================================================*/

/*============================================================================*\
* LOCAL FUNCTIONS
\*============================================================================*/

/* For detailed explanation see at function prototypes */

#if (CRC_8_MODE == CRC_8_RUNTIME)
static FUNC(uint8, CRC_CODE) CalculateCRC8_Runtime
(
P2CONST(uint8, AUTOMATIC,CRC_APPL_CONST) Crc_DataPtr,
VAR(uint32, AUTOMATIC)                Crc_Length,
VAR(uint8, AUTOMATIC)                Crc_StartValue8
)
{
VAR(uint8, AUTOMATIC) crc8;
VAR(uint8, AUTOMATIC)  index;
VAR(uint8, AUTOMATIC) dataword;

/* Initialization of CRC value */
crc8 = Crc_StartValue8;


/* checking if any data byte left for calculation */
while (Crc_Length > CRC_ZERO_LENGTH)
{
dataword = (uint8)*Crc_DataPtr;
         crc8 ^= dataword;

/* updating crc value for current data byte */
for (index = CRC_BIT_0; index<CRC_BIT_8; index++)
      {
         if (0 != (CRC8MASK & crc8))
         {
            crc8 <<= CRC_SHIFT_1;
            crc8 ^=  CRC_POLYNOMIAL_8;
         }
         else
         {
            crc8 <<= CRC_SHIFT_1;
         }
      }
      
      /* setting pointer to new data byte for calculation */
      Crc_DataPtr++;
      
      /* decrement number of data bytes left */
      Crc_Length--;
   }
   /* Crc value is XORred before is returned */
   crc8 ^= CRC_XORVALUE8;
   
   return (crc8);
}
#endif

#if (CRC_8_MODE == CRC_8_TABLE)
static FUNC(uint8, CRC_CODE) CalculateCRC8_Table
(
   P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST) Crc_DataPtr,
   VAR(uint32, AUTOMATIC)                Crc_Length,
   VAR(uint8, AUTOMATIC)                Crc_StartValue8
)
{
   VAR(uint8, AUTOMATIC) crc8;
   VAR(uint8, AUTOMATIC)  tableindex;
   /* Initialization of CRC value */
   crc8 = Crc_StartValue8 ^ CRC_XORVALUE8;
   
   /* checking if any data byte left for calculation */
   while (Crc_Length > CRC_ZERO_LENGTH)
   {
      /* calculation of the lookup table index */
      tableindex = crc8 ^ *Crc_DataPtr;
      
      /* updating crc value for current data byte */
      crc8 = CRC_Polynom8_Table[tableindex] ;
      
      /* setting data pointer to new data byte for calculation */
      Crc_DataPtr++;
      
      /* decrement number of data bytes left */
      Crc_Length--;
   }
   /* Crc value is XORred before is returned */
   crc8 ^= CRC_XORVALUE8;
   
   return(crc8);
}
#endif

#if (CRC_8H2F_MODE == CRC_8H2F_RUNTIME)
static FUNC(uint8, CRC_CODE) CalculateCRC8H2F_Runtime
(
P2CONST(uint8, AUTOMATIC,CRC_APPL_CONST) Crc_DataPtr,
VAR(uint32, AUTOMATIC)                Crc_Length,
VAR(uint8, AUTOMATIC)                Crc_StartValue8H2F
)
{
VAR(uint8, AUTOMATIC) crc8H2F;
VAR(uint8, AUTOMATIC)  index;
VAR(uint8, AUTOMATIC) dataword;

/* Initialization of CRC value */
crc8H2F = Crc_StartValue8H2F^CRC_XORVALUE8H2F;


/* checking if any data byte left for calculation */
while (Crc_Length > CRC_ZERO_LENGTH)
{
dataword = (uint8)*Crc_DataPtr;

/* updating crc value for current data byte */
for (index = CRC_BIT_0; index<CRC_BIT_8; index++)
      {
         if (0 != (CRC8MASK & (crc8H2F ^ dataword)))
         {
            crc8H2F <<= CRC_SHIFT_1;
            crc8H2F ^=  CRC_POLYNOMIAL_8H2F;
         }
         else
         {
            crc8H2F <<= CRC_SHIFT_1;
         }
         dataword <<= CRC_SHIFT_1;
      }
      
      /* setting pointer to new data byte for calculation */
      Crc_DataPtr++;
      
      /* decrement number of data bytes left */
      Crc_Length--;
   }
   /* Crc value is XORred before is returned */
   crc8H2F ^= CRC_XORVALUE8H2F;
   
   return (crc8H2F);
}
#endif

#if (CRC_8H2F_MODE == CRC_8H2F_TABLE)
static FUNC(uint8, CRC_CODE) CalculateCRC8H2F_Table
(
   P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST) Crc_DataPtr,
   VAR(uint32, AUTOMATIC)                Crc_Length,
   VAR(uint8, AUTOMATIC)                Crc_StartValue8H2F
)
{
   VAR(uint8, AUTOMATIC) crc8H2F;
   VAR(uint8, AUTOMATIC)  tableindex;
   /* Initialization of CRC value */
   crc8H2F = Crc_StartValue8H2F ^ CRC_XORVALUE8H2F;
   
   /* checking if any data byte left for calculation */
   while (Crc_Length > CRC_ZERO_LENGTH)
   {
      /* calculation of the lookup table index */
      tableindex = crc8H2F ^ *Crc_DataPtr;
      
      /* updating crc value for current data byte */
      crc8H2F = CRC_Polynom8H2F_Table[tableindex] ;
      
      /* setting data pointer to new data byte for calculation */
      Crc_DataPtr++;
      
      /* decrement number of data bytes left */
      Crc_Length--;
   }
   /* Crc value is XORred before is returned */
   crc8H2F ^= CRC_XORVALUE8H2F;
   
   return(crc8H2F);
}
#endif

#if (CRC_16_MODE == CRC_16_RUNTIME)
static FUNC(uint16, CRC_CODE) CalculateCRC16_Runtime
(
   P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST) Crc_DataPtr,
   VAR(uint32, AUTOMATIC)                Crc_Length,
   VAR(uint16, AUTOMATIC)                Crc_StartValue16
)
{
   VAR(uint16, AUTOMATIC) crc16;
   VAR(uint8, AUTOMATIC)  index;
   VAR(uint16, AUTOMATIC) dataword;
   
   /* Initialization of CRC value */
   crc16 = Crc_StartValue16 ^ CRC_XORVALUE16;
   
   /* checking if any data byte left for calculation */
   while (Crc_Length > CRC_ZERO_LENGTH)
   {
      /* preparing input data byte befere processing */
      dataword = (uint16)*Crc_DataPtr << CRC_SHIFT_8;
      
      /* updating crc value for current data byte */
      for (index = CRC_BIT_0; index<CRC_BIT_8; index++)
      {
         if (0 != (CRC16MASK & (crc16 ^ dataword)))
         {
            crc16 <<= CRC_SHIFT_1;
            crc16 ^=  CRC_POLYNOMIAL_16;
         }
         else
         {
            crc16 <<= CRC_SHIFT_1;
         }
         dataword <<= CRC_SHIFT_1;
      }
      
      /* setting pointer to new data byte for calculation */
      Crc_DataPtr++;
      
      /* decrement number of data bytes left */
      Crc_Length--;
   }
   
   /* Crc value is XORred before is returned */
   crc16 ^= CRC_XORVALUE16;
   
   return (crc16);
}
#endif

#if (CRC_16_MODE == CRC_16_TABLE)
static FUNC(uint16, CRC_CODE) CalculateCRC16_Table
(
   P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST) Crc_DataPtr,
   VAR(uint32, AUTOMATIC)                Crc_Length,
   VAR(uint16, AUTOMATIC)                Crc_StartValue16
)
{
   VAR(uint16, AUTOMATIC) crc16;
   VAR(uint8, AUTOMATIC)  tableindex;
   /* Initialization of CRC value */
   crc16 = Crc_StartValue16 ^ CRC_XORVALUE16;
   
   
   /* checking if any data byte left for calculation */
   while (Crc_Length > CRC_ZERO_LENGTH)
   {
      /* calculation of the lookup table index */
      tableindex = (uint8)(crc16 >> CRC_SHIFT_8) ^ *Crc_DataPtr;
      
      /* updating crc value for current data byte */
      crc16 = (crc16 << CRC_SHIFT_8) ^ CRC_Polynom16_Table[tableindex];
      
      /* setting data pointer to new data byte for calculation */
      Crc_DataPtr++;
      
      /* decrement number of data bytes left */
      Crc_Length--;
   }
   /* Crc value is XORred before is returned */
   crc16 ^= CRC_XORVALUE16;
   
   return(crc16);
}
#endif

#if (CRC_32_MODE == CRC_32_TABLE)
static FUNC(uint32, CRC_CODE) CalculateCRC32_Table
(
   P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST)    Crc_DataPtr,
   VAR(uint32, AUTOMATIC)                   Crc_Length,
   VAR(uint32, AUTOMATIC)                   Crc_StartValue32
)
{
   VAR(uint32, AUTOMATIC) crc32;
   VAR(uint8, AUTOMATIC)  tableindex;
   
   /* Initialization of CRC value */
   crc32 = Crc_StartValue32 ^ CRC_XORVALUE32;
   
   /* checking if any data byte left for calculation */
   while (Crc_Length > CRC_ZERO_LENGTH)
   {
      /* calculation of the lookup table index */
      tableindex = (uint8)(crc32 & CRC_MASK_8BITS) ^ *Crc_DataPtr;
      
      /* updating crc value for current data byte */
      crc32 = (crc32 >> CRC_SHIFT_8) ^ CRC_Polynom32_Table[tableindex];
      
      /* setting  data pointer to new data byte for calculation */
      Crc_DataPtr++;
      
      /* decrement number of data bytes left */
      Crc_Length--;
   }
   
   /* Crc value is XORred before is returned */
   crc32 ^= CRC_XORVALUE32;
   
   return(crc32);
}
#endif

#if (CRC_32_MODE == CRC_32_RUNTIME)
static FUNC(uint32, CRC_CODE) CalculateCRC32_Runtime
(
   P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST)    Crc_DataPtr,
   VAR(uint32, AUTOMATIC)                   Crc_Length,
   VAR(uint32, AUTOMATIC)                   Crc_StartValue32
)
{
   VAR(uint32, AUTOMATIC)  bitindex;
   VAR(uint32, AUTOMATIC)  data;
   VAR(uint32, AUTOMATIC)  bitt;
   VAR(uint32, AUTOMATIC)  crc32;
   
   /* Initialization of CRC value */
   crc32 = ReflectValue(Crc_StartValue32, CRC_REF_32);
   
   /* checking if any data byte left for calculation */
   while (Crc_Length > CRC_ZERO_LENGTH)
   {
      /* preparing input data byte before processing */
      data = (uint32)*Crc_DataPtr;
      
      /* reflection of input data byte */
      data = ReflectValue(data,CRC_REF_8);
      
      /* updating crc value for current data byte */
      for (bitindex = 0x80; bitindex > CRC_BIT_0; bitindex >>= CRC_SHIFT_1)
      {
         if (0 != (data & bitindex))
         {
            bitt = (crc32 & CRC32MASK) ^ CRC32MASK;
         }
         else
         {
            bitt = crc32 & CRC32MASK;
         }
         crc32 <<= CRC_SHIFT_1;
         if (0 != bitt)
         {
            crc32 ^= CRC_POLYNOMIAL_32;
         }
         else
         {
            /* nothing happend */
         }
      }
      
      /* setting pointer to new data byte for calculation */
      Crc_DataPtr++;
      
      /* decrement number of data bytes left */
      Crc_Length--;
   }
   
   /* reflection of output  data */
   crc32 = ReflectValue(crc32, CRC_REF_32);
   
   /* Crc value is XORred before is returned */
   crc32 ^= CRC_XORVALUE32;
   
   return(crc32);
}
#endif

#if (CRC_32_MODE == CRC_32_RUNTIME)
static FUNC(uint32, CRC_CODE)  ReflectValue
(
   VAR(uint32, AUTOMATIC)                   data,
   VAR(uint8, AUTOMATIC)                    bitamounts
)
{
   VAR(uint32, AUTOMATIC) bitnumber;
   VAR(uint32, AUTOMATIC) bitmask=1;
   VAR(uint32, AUTOMATIC) refdata=0;
   
   for (bitnumber = (uint32)1<<(bitamounts-1); bitnumber>0; bitnumber >>= CRC_SHIFT_1)
   {
      if (0 != (data & bitnumber))
      {
         refdata |= bitmask;
      }
      bitmask <<= CRC_SHIFT_1;
   }
   return (refdata);
}
#endif

/*============================================================================*\
* EXPORTED FUNCTIONS
\*============================================================================*/

/* For detailed explanation on the exported functions see Crc.h file. */

FUNC(uint8, CRC_CODE) Crc_CalculateCRC8
(
   P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST)    Crc_DataPtr,
   VAR(uint32, AUTOMATIC)                   Crc_Length,
   VAR(uint8, AUTOMATIC)                   Crc_StartValue8,
   VAR(boolean,AUTOMATIC)                  Crc_IsFirstCall

)
{
   /* mode of CRC8 calculation depends on the configuration
    * parameter CRC_8_MODE which is set at configuration
    * time.
    */
if(TRUE==Crc_IsFirstCall)
{
  #if (CRC_8_MODE == CRC_8_TABLE)
    Crc_Result8= CalculateCRC8_Table(Crc_DataPtr, Crc_Length, CRC_INITIAL_VALUE8);
  #endif
   
  #if (CRC_8_MODE == CRC_8_RUNTIME)
   Crc_Result8 = CalculateCRC8_Runtime(Crc_DataPtr, Crc_Length, CRC_INITIAL_VALUE8);
  #endif
   
  #if (CRC_8_MODE == CRC_8_HARDWARE)
   /* space for future use - hardware crc calculation */
  #endif
}
else
{
  #if (CRC_8_MODE == CRC_8_TABLE)
   Crc_Result8 = CalculateCRC8_Table(Crc_DataPtr, Crc_Length, Crc_StartValue8);
  #endif
   
  #if (CRC_8_MODE == CRC_8_RUNTIME)
   Crc_Result8 = CalculateCRC8_Runtime(Crc_DataPtr, Crc_Length, Crc_StartValue8);
  #endif
   
  #if (CRC_8_MODE == CRC_8_HARDWARE)
   /* space for future use - hardware crc calculation */
  #endif
}
   return (Crc_Result8);
}

FUNC(uint8, CRC_CODE) Crc_CalculateCRC8H2F
(
   P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST)    Crc_DataPtr,
   VAR(uint32, AUTOMATIC)                   Crc_Length,
   VAR(uint8, AUTOMATIC)                   Crc_StartValue8H2F,
   VAR(boolean,AUTOMATIC)                  Crc_IsFirstCall

)
{
   /* mode of CRC8 calculation depends on the configuration
    * parameter CRC_8_MODE which is set at configuration
    * time.
    */
  if(TRUE==Crc_IsFirstCall)
  {
   #if (CRC_8H2F_MODE == CRC_8H2F_TABLE)
   Crc_Result8H2F = CalculateCRC8H2F_Table(Crc_DataPtr, Crc_Length, CRC_INITIAL_VALUE8H2F);
   #endif
   
   #if (CRC_8H2F_MODE == CRC_8H2F_RUNTIME)
   Crc_Result8H2F = CalculateCRC8H2F_Runtime(Crc_DataPtr, Crc_Length, CRC_INITIAL_VALUE8H2F);
   #endif
   
   #if (CRC_8H2F_MODE == CRC_8H2F_HARDWARE)
   /* space for future use - hardware crc calculation */
   #endif
  }
  else
  {
    #if (CRC_8H2F_MODE == CRC_8H2F_TABLE)
   Crc_Result8H2F = CalculateCRC8H2F_Table(Crc_DataPtr, Crc_Length, Crc_StartValue8H2F);
   #endif
   
   #if (CRC_8H2F_MODE == CRC_8H2F_RUNTIME)
   Crc_Result8H2F = CalculateCRC8H2F_Runtime(Crc_DataPtr, Crc_Length, Crc_StartValue8H2F);
   #endif
   
   #if (CRC_8H2F_MODE == CRC_8H2F_HARDWARE)
   /* space for future use - hardware crc calculation */
   #endif
  }
   return (Crc_Result8H2F);
}
FUNC(uint16, CRC_CODE) Crc_CalculateCRC16
(
   P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST)    Crc_DataPtr,
   VAR(uint32, AUTOMATIC)                   Crc_Length,
   VAR(uint16, AUTOMATIC)                   Crc_StartValue16,
   VAR(boolean,AUTOMATIC)                   Crc_IsFirstCall

)
{
   /* mode of CRC16 calculation depends on the configuration
    * parameter CRC_16_MODE which is set at configuration
    * time.
    */
   if(TRUE==Crc_IsFirstCall)
   {
    #if (CRC_16_MODE == CRC_16_TABLE)
    Crc_Result16 = CalculateCRC16_Table(Crc_DataPtr, Crc_Length, CRC_INITIAL_VALUE16);
    #endif
   
    #if (CRC_16_MODE == CRC_16_RUNTIME)
    Crc_Result16 = CalculateCRC16_Runtime(Crc_DataPtr, Crc_Length, CRC_INITIAL_VALUE16);
    #endif
   
    #if (CRC_16_MODE == CRC_16_HARDWARE)
    /* space for future use - hardware crc calculation */
    #endif
   }
   else
   {
    #if (CRC_16_MODE == CRC_16_TABLE)
   Crc_Result16 = CalculateCRC16_Table(Crc_DataPtr, Crc_Length, Crc_StartValue16);
   #endif
   
   #if (CRC_16_MODE == CRC_16_RUNTIME)
   Crc_Result16 = CalculateCRC16_Runtime(Crc_DataPtr, Crc_Length, Crc_StartValue16);
   #endif
   
   #if (CRC_16_MODE == CRC_16_HARDWARE)
   /* space for future use - hardware crc calculation */
   #endif
   }
   
   return (Crc_Result16);
}

FUNC(uint32, CRC_CODE) Crc_CalculateCRC32
(
   P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST)    Crc_DataPtr,
   VAR(uint32, AUTOMATIC)                   Crc_Length,
   VAR(uint32, AUTOMATIC)                   Crc_StartValue32,
   VAR(boolean,AUTOMATIC)                   Crc_IsFirstCall
)
{
   /* mode of CRC32 calculation depends on the configuration
    * parameter CRC_32_MODE which is set at configuration
    * time.
    */
   if(TRUE==Crc_IsFirstCall)
   {
    #if (CRC_32_MODE == CRC_32_TABLE)
    Crc_Result32 = CalculateCRC32_Table(Crc_DataPtr, Crc_Length, CRC_INITIAL_VALUE32);
    #endif
   
    #if (CRC_32_MODE == CRC_32_RUNTIME)
    Crc_Result32 = CalculateCRC32_Runtime(Crc_DataPtr, Crc_Length, CRC_INITIAL_VALUE32);
    #endif
   
    #if (CRC_32_MODE == CRC_32_HARDWARE)
    /* space for future use - hardware crc calculation */
    #endif
   }
   else
   {
    #if (CRC_32_MODE == CRC_32_TABLE)
    Crc_Result32 = CalculateCRC32_Table(Crc_DataPtr, Crc_Length, Crc_StartValue32);
    #endif
   
    #if (CRC_32_MODE == CRC_32_RUNTIME)
    Crc_Result32 = CalculateCRC32_Runtime(Crc_DataPtr, Crc_Length, Crc_StartValue32);
    #endif
   
    #if (CRC_32_MODE == CRC_32_HARDWARE)
    /* space for future use - hardware crc calculation */
    #endif
   }
   
   return (Crc_Result32);
}

FUNC(void, CRC_CODE)   Crc_GetVersionInfo
(
   P2VAR(Std_VersionInfoType, AUTOMATIC, CRC_APPL_DATA) versioninfo
)
{
   /* simplest protection against passing null pointer */
   if (NULL_PTR != versioninfo)
   {
      versioninfo->vendorID = CRC_VENDOR_ID ;
      versioninfo->moduleID = CRC_MODULE_ID ;
      versioninfo->sw_major_version = CRC_SW_MAJOR_VERSION;
      versioninfo->sw_minor_version = CRC_SW_MINOR_VERSION;
      versioninfo->sw_patch_version = CRC_SW_PATCH_VERSION;
   }
   else
   {
      /* function returns nothing */
   }
}

/* PRQA S 5087 ++
 * MISRA RULE C197 VIOLATION:
 */
#define  CRC_STOP_SEC_CODE
#include "MemMap.h"
/* PRQA S 5087 -- */

/* END OF FILE -------------------------------------------------------------- */
