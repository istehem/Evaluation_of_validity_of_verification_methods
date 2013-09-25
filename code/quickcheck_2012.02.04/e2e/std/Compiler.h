/******************************************************************************
 *
 *                    Mentor Graphics Corporation
 *                        All rights reserved
 *
 ******************************************************************************
 *
 * Module:       Common - Compiler Abstraction
 *
 * File Name:    Compiler.h
 *
 * Description:  This file contains the definitions and macros specified by
 *               AUTOSAR for the abstraction of compiler specific keywords.
 *
 *
 *
 ******************************************************************************/

/*COMPILER047*/
#ifndef COMPILER_H
#define COMPILER_H

#ifdef __cplusplus
extern "C"
{
#endif

/*=============================== INCLUSIONS =================================*/

/*COMPILER052*/
#include "Compiler_Cfg.h"

/*============================== COMPILER INFO ===============================*/

/**
 * \defgroup Compiler Abstraction
 *  Describes the compiler information
 */
 
#define STATIC static

/* Compiler vendor id */
#define COMPILER_VENDOR_ID           ((uint16) 31)

/* Compiler Autosar Specification major version */
#define COMPILER_AR_RELEASE_MAJOR_VERSION     (4u)

/* Compiler Autosar Specification minor version */
#define COMPILER_AR_RELEASE_MINOR_VERSION     (0u)

/* Compiler Autosar Specification patch version */
#define COMPILER_AR_RELEASE_REVISION_VERSION  (2u)

/* Compiler Software specification major version */
#define COMPILER_SW_MAJOR_VERSION  ((uint8) 1)

/* Compiler Software specification minor version */
#define COMPILER_SW_MINOR_VERSION  ((uint8) 0)

/* Compiler Software specification patch version */
#define COMPILER_SW_PATCH_VERSION  ((uint8) 0)


/*============================ COMPILER KEYWORDS =============================*/

/**
 * \defgroup Compiler Keywords
 */

/*\{*/

#define STATIC static

/* COMPILER046 */
/* The memory class is used for the declaration of local pointers */
#define AUTOMATIC

/** COMPILER059 */
/* The memory class is used within type definitions, where no memory
   qualifier can be specified */
#define TYPEDEF

/* The memory class is used to define the abstraction of compiler keyword
   static inline*/
#define Local_INLINE     static inline

/** COMPILER051 */
/* This is used to define the void pointer to zero definition. */
#define NULL_PTR    ((void *)0)

/*COMPILER057: */
/* This is used to define the abstraction of compiler keyword inline */
#define INLINE    inline

#define _INTERRUPT_ /* PRQA S 0602*/__interrupt
/**
 * \brief  This macro is used for the declaration and definition of
 *         functions, that ensures correct syntax of function declarations as
 *         required by a specific compiler
 *
 * \param  rettype   return type of the function.
 * \param  memclass  classification of the function itself
 *
 * \retval None
 *
 */
/* PRQA S 3453 ++ */  /*not complying with MISRA Rule 19.7 to fulfill
                     the compiler abstraction SWS document */
/* PRQA S 3410 ++ */ /*not complying with MISRA Rule 19.10 to fulfill
                     the compiler abstraction SWS document */
/** COMPILER001 */
#define FUNC(rettype, memclass) rettype


/**
 * \brief  This macro is used for the declaration and definition of pointers
 *         in RAM, pointing to variables
 *
 * \param  ptrtype   type of the reference variable
 * \param  memclass  classification of the pointer's variable itself
 * \param  ptrclass  defines the classification of the pointer distance
 *
 * \retval None
 *
 */
/** COMPILER006 */
#define P2VAR(ptrtype, memclass, ptrclass) /* PRQA S 3409 */ ptrtype *


/**
 * \brief  This macro is used for the declaration and definition of pointers
 *         in RAM, pointing to constants
 *
 * \param  ptrtype   type of the reference variable.
 * \param  memclass  classification of the pointer's variable itself
 * \param  ptrclass  defines the classification of the pointer distance
 *
 * \retval None
 *
 */
/** COMPILER013 */
#define P2CONST(ptrtype, memclass, ptrclass) /* PRQA S 3409 */ const ptrtype *


/**
 * \brief  This macro is used for the declaration and definition of constant
 *         pointers accessing variables
 *
 * \param  ptrtype   type of the reference variable
 * \param  memclass  classification of the pointer's constant itself
 * \param  ptrclass  defines the classification of the pointer distance
 *
 * \retval None
 *
 */
/** COMPILER031 */
#define CONSTP2VAR(ptrtype, memclass, ptrclass) /* PRQA S 3409 */ ptrtype * const

/**
 * \brief  This macro is used for the declaration and definition of constant
 *         pointers accessing variables
 *
 * \param  ptrtype   type of the reference variable.
 * \param  memclass  classification of the pointer's constant itself
 * \param  ptrclass  defines the classification of the pointer distance
 *
 * \retval None
 *
 */
/** COMPILER032 */
#define CONSTP2CONST(ptrtype, memclass, ptrclass) /* PRQA S 3409 */ const ptrtype * const


/**
 * \brief  This macro is used for the declaration and definition of constant
 *         pointers accessing variables
 *
 * \param  rettype   return type of the function
 * \param  ptrclass  defines the classification of the pointer's distance
 * \param  fctname   classification of the pointer's variable itself
 *
 * \retval None
 *
 */
/** COMPILER039 */
#define P2FUNC(rettype, ptrclass, fctname) rettype (*fctname)

#ifdef TESSY
  /* This is used to for the declaration and definition of constants */
  #define CONST(type, memclass) type
#else
  /** COMPILER023 */
  #define CONST(type, memclass) /* PRQA S 3409 */ const type
#endif
/* This is used to for the declaration and definition of variables */
/** COMPILER026 */
#define VAR(type, memclass) type

/* PRQA S 3453 -- */
/* PRQA S 3410 -- */

/*\}*/  /* Close for the doxygen group Compiler Keywords */

#ifdef __cplusplus
}
#endif

#endif /* COMPILER_H */
