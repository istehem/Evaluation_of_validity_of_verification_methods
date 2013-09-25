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
* \b reqtrace{DSAR-SDD-CRC-17-2}{CRC022,CRC024
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

/** \brief Expected major version */
#define EXPECTED_CRC_SW_MAJOR_VERSION     (5)
/** \brief Expected minor version */
#define EXPECTED_CRC_SW_MINOR_VERSION     (0)
/** \brief Expected patch version */
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

/** Auxliary define used for shifting variables by 24 bits in CRC calculation */
#define  CRC_SHIFT_24        ((uint8)24u)

/** Auxliary define used for shifting variables in 8 bits in CRC calculation */
#define  CRC_SHIFT_8         ((uint8)8u)

/** Auxliary define used for shifting variables in 1 bit in CRC calculation */
#define  CRC_SHIFT_1         ((uint8)1u)

/** mask for 8-bit Crc calculation */
#define CRC8MASK   ((uint8)0x80)

/** mask for 16-bit Crc calculation */
#define  CRC16MASK       ((uint16)0x8000u)

/** mask for 32-bit Crc calculation */
#define  CRC32MASK       ((uint32)0x80000000u)

/** constant used for masking 8 bits of variable */
#define  CRC_MASK_8BITS      ((uint8)0xFFu)

/**
 * defines for reflect function - 8 bits has to be reflected
*/
#define  CRC_REF_8           ((uint8)8u)
/**
 * defines for reflect function - 32 bits has to be reflected
 */
#define  CRC_REF_32          ((uint8)32u)

/** zero length define */
#define  CRC_ZERO_LENGTH      ((uint8)0u)

/** bit indicator defines */
#define  CRC_BIT_0            ((uint8)0u)
/** bit indicator defines */
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
/**
 * Mark start of memory area for 8-bit static constants
 */
#define CRC_START_SEC_CONST_8BIT
#include "MemMap.h"
/* PRQA S 5087 -- */

/**
 * Global variable for storing Crc8 SAE J1850 result
 */
static VAR(uint8, CRC_VAR_NOINIT) Crc_Result8;

/**
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

/**
 * Global variable for storing Crc8H2F result
 */
static VAR(uint8, CRC_VAR_NOINIT) Crc_Result8H2F;

/**
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
/**
 * Mark end of memory area for 8-bit static constants
 */
#define CRC_STOP_SEC_CONST_8BIT
#include "MemMap.h"
/* PRQA S 5087 -- */


/* PRQA S 5087 ++ */
/**
 * Mark start of memory area for 16-bit static constants
 */
#define CRC_START_SEC_CONST_16BIT
#include "MemMap.h"
/* PRQA S 5087 -- */

/**
 * Global variable for storing Crc16 result
 */
static VAR(uint16, CRC_VAR_NOINIT) Crc_Result16;

/**
 * Look up  table for CRC_16 bit table driven Calculation
 */
/* PRQA S 1281 ++ */
static CONST(uint16, CRC_CONST)  CRC_Polynom16_Table[CRC_TABLE_SIZE]=
{
0x0000u,0x1021u,0x2042u,0x3063u,0x4084u,0x50A5u,0x60C6u,0x70E7u,
0x8108u,0x9129u,0xA14Au,0xB16Bu,0xC18Cu,0xD1ADu,0xE1CEu,0xF1EFu,
0x1231u,0x0210u,0x3273u,0x2252u,0x52B5u,0x4294u,0x72F7u,0x62D6u,
0x9339u,0x8318u,0xB37Bu,0xA35Au,0xD3BDu,0xC39Cu,0xF3FFu,0xE3DEu,
0x2462u,0x3443u,0x0420u,0x1401u,0x64E6u,0x74C7u,0x44A4u,0x5485u,
0xA56Au,0xB54Bu,0x8528u,0x9509u,0xE5EEu,0xF5CFu,0xC5ACu,0xD58Du,
0x3653u,0x2672u,0x1611u,0x0630u,0x76D7u,0x66F6u,0x5695u,0x46B4u,
0xB75Bu,0xA77Au,0x9719u,0x8738u,0xF7DFu,0xE7FEu,0xD79Du,0xC7BCu,
0x48C4u,0x58E5u,0x6886u,0x78A7u,0x0840u,0x1861u,0x2802u,0x3823u,
0xC9CCu,0xD9EDu,0xE98Eu,0xF9AFu,0x8948u,0x9969u,0xA90Au,0xB92Bu,
0x5AF5u,0x4AD4u,0x7AB7u,0x6A96u,0x1A71u,0x0A50u,0x3A33u,0x2A12u,
0xDBFDu,0xCBDCu,0xFBBFu,0xEB9Eu,0x9B79u,0x8B58u,0xBB3Bu,0xAB1Au,
0x6CA6u,0x7C87u,0x4CE4u,0x5CC5u,0x2C22u,0x3C03u,0x0C60u,0x1C41u,
0xEDAEu,0xFD8Fu,0xCDECu,0xDDCDu,0xAD2Au,0xBD0Bu,0x8D68u,0x9D49u,
0x7E97u,0x6EB6u,0x5ED5u,0x4EF4u,0x3E13u,0x2E32u,0x1E51u,0x0E70u,
0xFF9Fu,0xEFBEu,0xDFDDu,0xCFFCu,0xBF1Bu,0xAF3Au,0x9F59u,0x8F78u,
0x9188u,0x81A9u,0xB1CAu,0xA1EBu,0xD10Cu,0xC12Du,0xF14Eu,0xE16Fu,
0x1080u,0x00A1u,0x30C2u,0x20E3u,0x5004u,0x4025u,0x7046u,0x6067u,
0x83B9u,0x9398u,0xA3FBu,0xB3DAu,0xC33Du,0xD31Cu,0xE37Fu,0xF35Eu,
0x02B1u,0x1290u,0x22F3u,0x32D2u,0x4235u,0x5214u,0x6277u,0x7256u,
0xB5EAu,0xA5CBu,0x95A8u,0x8589u,0xF56Eu,0xE54Fu,0xD52Cu,0xC50Du,
0x34E2u,0x24C3u,0x14A0u,0x0481u,0x7466u,0x6447u,0x5424u,0x4405u,
0xA7DBu,0xB7FAu,0x8799u,0x97B8u,0xE75Fu,0xF77Eu,0xC71Du,0xD73Cu,
0x26D3u,0x36F2u,0x0691u,0x16B0u,0x6657u,0x7676u,0x4615u,0x5634u,
0xD94Cu,0xC96Du,0xF90Eu,0xE92Fu,0x99C8u,0x89E9u,0xB98Au,0xA9ABu,
0x5844u,0x4865u,0x7806u,0x6827u,0x18C0u,0x08E1u,0x3882u,0x28A3u,
0xCB7Du,0xDB5Cu,0xEB3Fu,0xFB1Eu,0x8BF9u,0x9BD8u,0xABBBu,0xBB9Au,
0x4A75u,0x5A54u,0x6A37u,0x7A16u,0x0AF1u,0x1AD0u,0x2AB3u,0x3A92u,
0xFD2Eu,0xED0Fu,0xDD6Cu,0xCD4Du,0xBDAAu,0xAD8Bu,0x9DE8u,0x8DC9u,
0x7C26u,0x6C07u,0x5C64u,0x4C45u,0x3CA2u,0x2C83u,0x1CE0u,0x0CC1u,
0xEF1Fu,0xFF3Eu,0xCF5Du,0xDF7Cu,0xAF9Bu,0xBFBAu,0x8FD9u,0x9FF8u,
0x6E17u,0x7E36u,0x4E55u,0x5E74u,0x2E93u,0x3EB2u,0x0ED1u,0x1EF0u
};
/* PRQA S 1281 -- */
/* PRQA S 5087 ++
* MISRA RULE C197 VIOLATION:
*/
/**
 * Mark end of memory area for 16-bit static constants
 */
#define CRC_STOP_SEC_CONST_16BIT
#include "MemMap.h"
/* PRQA S 5087 -- */

/* PRQA S 5087 ++
* MISRA RULE C197 VIOLATION:
*/
/**
 * Mark start of memory area for 32-bit static constants
 */
#define CRC_START_SEC_CONST_32BIT
#include "MemMap.h"
/* PRQA S 5087 -- */

/**
 * Global variable for storing Crc32 result
 */
static VAR(uint32, CRC_VAR_NOINIT) Crc_Result32;

/**
 * Look up  table for CRC_32 bit table driven Calculation
 */
/* PRQA S 1281 ++ */
static CONST(uint32, CRC_CONST) CRC_Polynom32_Table[CRC_TABLE_SIZE]=
{
0x00000000U, 0x77073096U, 0xee0e612cU, 0x990951baU,0x076dc419U, 0x706af48fU,
0xe963a535U, 0x9e6495a3U, 0x0edb8832U, 0x79dcb8a4U,0xe0d5e91eU, 0x97d2d988U,
0x09b64c2bU, 0x7eb17cbdU, 0xe7b82d07U, 0x90bf1d91U,0x1db71064U, 0x6ab020f2U,
0xf3b97148U, 0x84be41deU, 0x1adad47dU, 0x6ddde4ebU,0xf4d4b551U, 0x83d385c7U,
0x136c9856U, 0x646ba8c0U, 0xfd62f97aU, 0x8a65c9ecU,0x14015c4fU, 0x63066cd9U,
0xfa0f3d63U, 0x8d080df5U, 0x3b6e20c8U, 0x4c69105eU,0xd56041e4U, 0xa2677172U,
0x3c03e4d1U, 0x4b04d447U, 0xd20d85fdU, 0xa50ab56bU,0x35b5a8faU, 0x42b2986cU,
0xdbbbc9d6U, 0xacbcf940U, 0x32d86ce3U, 0x45df5c75U,0xdcd60dcfU, 0xabd13d59U,
0x26d930acU, 0x51de003aU, 0xc8d75180U, 0xbfd06116U,0x21b4f4b5U, 0x56b3c423U,
0xcfba9599U, 0xb8bda50fU, 0x2802b89eU, 0x5f058808U,0xc60cd9b2U, 0xb10be924U,
0x2f6f7c87U, 0x58684c11U, 0xc1611dabU, 0xb6662d3dU,0x76dc4190U, 0x01db7106U,
0x98d220bcU, 0xefd5102aU, 0x71b18589U, 0x06b6b51fU,0x9fbfe4a5U, 0xe8b8d433U,
0x7807c9a2U, 0x0f00f934U, 0x9609a88eU, 0xe10e9818U,0x7f6a0dbbU, 0x086d3d2dU,
0x91646c97U, 0xe6635c01U, 0x6b6b51f4U, 0x1c6c6162U,0x856530d8U, 0xf262004eU,
0x6c0695edU, 0x1b01a57bU, 0x8208f4c1U, 0xf50fc457U,0x65b0d9c6U, 0x12b7e950U,
0x8bbeb8eaU, 0xfcb9887cU, 0x62dd1ddfU, 0x15da2d49U,0x8cd37cf3U, 0xfbd44c65U,
0x4db26158U, 0x3ab551ceU, 0xa3bc0074U, 0xd4bb30e2U,0x4adfa541U, 0x3dd895d7U,
0xa4d1c46dU, 0xd3d6f4fbU, 0x4369e96aU, 0x346ed9fcU,0xad678846U, 0xda60b8d0U,
0x44042d73U, 0x33031de5U, 0xaa0a4c5fU, 0xdd0d7cc9U,0x5005713cU, 0x270241aaU,
0xbe0b1010U, 0xc90c2086U, 0x5768b525U, 0x206f85b3U,0xb966d409U, 0xce61e49fU,
0x5edef90eU, 0x29d9c998U, 0xb0d09822U, 0xc7d7a8b4U,0x59b33d17U, 0x2eb40d81U,
0xb7bd5c3bU, 0xc0ba6cadU, 0xedb88320U, 0x9abfb3b6U,0x03b6e20cU, 0x74b1d29aU,
0xead54739U, 0x9dd277afU, 0x04db2615U, 0x73dc1683U,0xe3630b12U, 0x94643b84U,
0x0d6d6a3eU, 0x7a6a5aa8U, 0xe40ecf0bU, 0x9309ff9dU,0x0a00ae27U, 0x7d079eb1U,
0xf00f9344U, 0x8708a3d2U, 0x1e01f268U, 0x6906c2feU,0xf762575dU, 0x806567cbU,
0x196c3671U, 0x6e6b06e7U, 0xfed41b76U, 0x89d32be0U,0x10da7a5aU, 0x67dd4accU,
0xf9b9df6fU, 0x8ebeeff9U, 0x17b7be43U, 0x60b08ed5U,0xd6d6a3e8U, 0xa1d1937eU,
0x38d8c2c4U, 0x4fdff252U, 0xd1bb67f1U, 0xa6bc5767U,0x3fb506ddU, 0x48b2364bU,
0xd80d2bdaU, 0xaf0a1b4cU, 0x36034af6U, 0x41047a60U,0xdf60efc3U, 0xa867df55U,
0x316e8eefU, 0x4669be79U, 0xcb61b38cU, 0xbc66831aU,0x256fd2a0U, 0x5268e236U,
0xcc0c7795U, 0xbb0b4703U, 0x220216b9U, 0x5505262fU,0xc5ba3bbeU, 0xb2bd0b28U,
0x2bb45a92U, 0x5cb36a04U, 0xc2d7ffa7U, 0xb5d0cf31U,0x2cd99e8bU, 0x5bdeae1dU,
0x9b64c2b0U, 0xec63f226U, 0x756aa39cU, 0x026d930aU,0x9c0906a9U, 0xeb0e363fU,
0x72076785U, 0x05005713U, 0x95bf4a82U, 0xe2b87a14U,0x7bb12baeU, 0x0cb61b38U,
0x92d28e9bU, 0xe5d5be0dU, 0x7cdcefb7U, 0x0bdbdf21U,0x86d3d2d4U, 0xf1d4e242U,
0x68ddb3f8U, 0x1fda836eU, 0x81be16cdU, 0xf6b9265bU,0x6fb077e1U, 0x18b74777U,
0x88085ae6U, 0xff0f6a70U, 0x66063bcaU, 0x11010b5cU,0x8f659effU, 0xf862ae69U,
0x616bffd3U, 0x166ccf45U, 0xa00ae278U, 0xd70dd2eeU,0x4e048354U, 0x3903b3c2U,
0xa7672661U, 0xd06016f7U, 0x4969474dU, 0x3e6e77dbU,0xaed16a4aU, 0xd9d65adcU,
0x40df0b66U, 0x37d83bf0U, 0xa9bcae53U, 0xdebb9ec5U,0x47b2cf7fU, 0x30b5ffe9U,
0xbdbdf21cU, 0xcabac28aU, 0x53b39330U, 0x24b4a3a6U,0xbad03605U, 0xcdd70693U,
0x54de5729U, 0x23d967bfU, 0xb3667a2eU, 0xc4614ab8U,0x5d681b02U, 0x2a6f2b94U,
0xb40bbe37U, 0xc30c8ea1U, 0x5a05df1bU, 0x2d02ef8dU
};
/* PRQA S 1281 -- */
/* PRQA S 5087 ++
* MISRA RULE C197 VIOLATION:
*/
/**
 * Mark end of memory area for 32-bit static constants
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
/**
 * Mark start of memory area for the executable program code
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
* \param[in]      Crc_StartValue8H2F - Initial Value for CRC algorithm
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
* \param[in]      Crc_StartValue8H2F - Initial Value for CRC algorithm
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
* \param[in]      Crc_StartValue32 - Initial Value for CRC algorithm
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
* \param[in]      Crc_StartValue32 - Initial Value for CRC algorithm
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

/* updating crc value for current data byte */
for (index = CRC_BIT_0; index<CRC_BIT_8; index++)
      {
         if (0 != (CRC8MASK & (crc8 ^ dataword)))
         {
            crc8 <<= CRC_SHIFT_1;
            crc8 ^=  CRC_POLYNOMIAL_8;
         }
         else
         {
            crc8 <<= CRC_SHIFT_1;
         }
         dataword <<= CRC_SHIFT_1;
      }
      
      /* setting pointer to new data byte for calculation */
      /* PRQA S 0489++ 
       * Pointer arithmetic needed to obtain the correct address
       */
      Crc_DataPtr++;
      
      /* decrement number of data bytes left */
      Crc_Length--;
      /* PRQA S 0489-- */
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
      /* PRQA S 0489++ 
       * Pointer arithmetic needed to obtain the correct address
       */
      Crc_DataPtr++;
      
      /* decrement number of data bytes left */
      Crc_Length--;
      /* PRQA S 0489-- */
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
      /* PRQA S 0489++ 
       * Pointer arithmetic needed to obtain the correct address
       */
      Crc_DataPtr++;
      
      /* decrement number of data bytes left */
      Crc_Length--;
      /* PRQA S 0489-- */
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
      /* PRQA S 0489++ 
       * Pointer arithmetic needed to obtain the correct address
       */
      Crc_DataPtr++;
      
      /* decrement number of data bytes left */
      Crc_Length--;
      /* PRQA S 0489-- */
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
      /* PRQA S 0489++ 
       * Pointer arithmetic needed to obtain the correct address
       */
      Crc_DataPtr++;
      
      /* decrement number of data bytes left */
      Crc_Length--;
      /* PRQA S 0489-- */
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
      /* PRQA S 0489++ 
       * Pointer arithmetic needed to obtain the correct address
       */
      Crc_DataPtr++;
      
      /* decrement number of data bytes left */
      Crc_Length--;
      /* PRQA S 0489-- */
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
      /* PRQA S 0489++ 
       * Pointer arithmetic needed to obtain the correct address
       */
      Crc_DataPtr++;
      
      /* decrement number of data bytes left */
      Crc_Length--;
      /* PRQA S 0489-- */
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
   crc32 = ReflectValue(Crc_StartValue32 ^ CRC_XORVALUE32, CRC_REF_32);
   
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
      /* PRQA S 0489++ 
       * Pointer arithmetic needed to obtain the correct address
       */
      Crc_DataPtr++;
      
      /* decrement number of data bytes left */
      Crc_Length--;
      /* PRQA S 0489-- */
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
   Crc_Result8 = CalculateCRC8_Table(Crc_DataPtr, Crc_Length, Crc_StartValue8 ^ CRC_XORVALUE8);
  #endif
   
  #if (CRC_8_MODE == CRC_8_RUNTIME)
   Crc_Result8 = CalculateCRC8_Runtime(Crc_DataPtr, Crc_Length, Crc_StartValue8 ^ CRC_XORVALUE8);
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
/**
 * Mark end of memory area for the executable program code
 */
#define  CRC_STOP_SEC_CODE
#include "MemMap.h"
/* PRQA S 5087 -- */

/* END OF FILE -------------------------------------------------------------- */

