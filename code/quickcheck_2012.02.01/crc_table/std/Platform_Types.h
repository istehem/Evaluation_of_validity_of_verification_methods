/******************************************************************************
 *
 *                    Mentor Graphics Corporation
 *                        All rights reserved
 *
 ******************************************************************************
 *
 * Module:       Common - Platform Abstraction
 *
 * File Name:    Platform_Types.h
 *
 * Description:  Platform types for WIN32/iX86
 *
 *****************************************************************************/

/* This file is AUTOSAR 4.0.2 compliant */
 
/* Each platform have to have Platform_Types.h */
/* The name of this file is Platform_Types.h */


#ifndef PLATFORM_TYPES_H
#define PLATFORM_TYPES_H

#ifdef __cplusplus
extern "C"
{
#endif

/*****************************************************************************/
/* Included standard header files                                            */
/*****************************************************************************/

/*****************************************************************************/
/* Included other header files                                               */
/*****************************************************************************/

/*****************************************************************************/
/* Public macros                                                             */
/*****************************************************************************/
/*
 * Shortname according to AR bugzilla 46541
 */
#define PLATFORM_VENDOR_ID          (31u)

#define PLATFORM_SW_MAJOR_VERSION   (1u)
#define PLATFORM_SW_MINOR_VERSION   (0u)
#define PLATFORM_SW_PATCH_VERSION   (0u)

#define PLATFORM_AR_RELEASE_MAJOR_VERSION    (4u)
#define PLATFORM_AR_RELEASE_MINOR_VERSION    (0u)
#define PLATFORM_AR_RELEASE_REVISION_VERSION (2u)

/* 
 * CPU register type width
 */
#define CPU_TYPE_8                  (8u)
#define CPU_TYPE_16                 (16u)
#define CPU_TYPE_32                 (32u)

/* 
 * Bit order definition
 */
#define MSB_FIRST                   (0u)                 /* Big endian bit ordering        */
#define LSB_FIRST                   (1u)                 /* Little endian bit ordering     */

/* 
 * Byte order definition
 */
#define HIGH_BYTE_FIRST             (0u)                /* Big endian byte ordering       */
#define LOW_BYTE_FIRST              (1u)                /* Little endian byte ordering    */

/*
 * Platform type and endianess definitions, specific for win32/ix86
 */
#define CPU_TYPE                    CPU_TYPE_32

#define CPU_BIT_ORDER       LSB_FIRST
#define CPU_BYTE_ORDER      LOW_BYTE_FIRST


#ifndef FALSE
#define FALSE       0
#endif
#ifndef TRUE
#define TRUE        1
#endif

/*****************************************************************************/
/* Public types                                                              */
/*****************************************************************************/
/*
 * AUTOSAR integer data types
 */

typedef signed char         sint8;          /*        -128 .. +127            */
typedef unsigned char       uint8;          /*           0 .. 255             */
typedef signed short        sint16;         /*      -32768 .. +32767          */
typedef unsigned short      uint16;         /*           0 .. 65535           */ 
typedef signed long         sint32;         /* -2147483648 .. +2147483647     */
typedef unsigned long       uint32;         /*           0 .. 4294967295      */
typedef float               float32;                
typedef double              float64;


typedef unsigned long       uint8_least;    /* At least 8 bit                 */
typedef unsigned long       uint16_least;   /* At least 16 bit                */
typedef unsigned long       uint32_least;   /* At least 32 bit                */
typedef signed long         sint8_least;    /* At least 7 bit + 1 bit sign    */
typedef signed long         sint16_least;   /* At least 15 bit + 1 bit sign   */
typedef signed long         sint32_least;   /* At least 31 bit + 1 bit sign   */

typedef unsigned char       boolean;        /* for use with TRUE/FALSE        */

/*****************************************************************************/
/* Public constant & variable prototypes                                     */
/*****************************************************************************/

/*****************************************************************************/
/* Public API function prototypes                                            */
/*****************************************************************************/

#ifdef __cplusplus
}
#endif

#endif  /* PLATFORM_TYPES_H */
