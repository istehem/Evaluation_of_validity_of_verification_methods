
#if (!defined COMPILER_CFG_H)
#define COMPILER_CFG_H

/** \file
 ** \brief AUTOSAR compiler abstraction header - XC2000
 **
 ** This file contains compiler specific declarations for the
 ** platform Infineon XC2287 using the compiler Tasking V8.6R2
 **
 ** It contains module specific memory and pointer classes
 **
 ** See "Specification of Compiler Abstraction"
 **
 ** \project AUTOSAR Standard Core
 ** \author Marko Schneider
 ** \author Elektrobit Automotive GmbH, 91058 Erlangen, Germany
 **
 ** $Id: Compiler_Cfg.h 1421 2007-03-12 10:15:03Z masc2461 $
 ** Release: $3SOFT_DELIVERY_VERSION_INFORMATION$
 **
 ** \controller Infineon XC2287
 ** \compiler Tasking for XC2000 V8.6R2
 ** \hardware independent
 **
 ** Copyright 2007 by Elektrobit Automotive GmbH
 ** All rights exclusively reserved for Elektrobit Automotive GmbH,
 ** unless expressly agreed to otherwise */

/** \addtogroup BSW Basic Software Module Definitions
 ** @{ */

/*
 * MISRA-C:2004 deviation report:
 */

/*==================[inclusions]=============================================*/


/*==================[macros]=================================================*/
/* This macro is defined to avoid the Os compilation issue */
#define VSI  	1
#define i386 	2

#if (defined CAN_AFCAN_CONST) /* to prevent double definition */
#error CAN_AFCAN_CONST already defined
#endif /* if (defined CAN_AFCAN_CONST) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define CAN_AFCAN_CONST
/*------------------[memory and pointer class of a module]-------------------*/

#if (defined ADC_CODE) /* to prevent double definition */
#error ADC_CODE already defined
#endif /* if (defined ADC_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define ADC_CODE

#if (defined ADC_CONST) /* to prevent double definition */
#error ADC_CONST already defined
#endif /* if (defined ADC_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define ADC_CONST

#if (defined ADC_APPL_DATA) /* to prevent double definition */
#error ADC_APPL_DATA already defined
#endif /* if (defined ADC_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define ADC_APPL_DATA

#if (defined ADC_APPL_CONST) /* to prevent double definition */
#error ADC_APPL_CONST already defined
#endif /* if (defined ADC_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define ADC_APPL_CONST

#if (defined ADC_APPL_CODE) /* to prevent double definition */
#error ADC_APPL_CODE already defined
#endif /* if (defined ADC_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define ADC_APPL_CODE

#if (defined ADC_VAR_NOINIT) /* to prevent double definition */
#error ADC_VAR_NOINIT already defined
#endif /* if (defined ADC_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define ADC_VAR_NOINIT

#if (defined ADC_VAR_POWER_ON_INIT) /* to prevent double definition */
#error ADC_VAR_POWER_ON_INIT already defined
#endif /* if (defined ADC_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define ADC_VAR_POWER_ON_INIT

#if (defined ADC_VAR_FAST) /* to prevent double definition */
#error ADC_VAR_FAST already defined
#endif /* if (defined ADC_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define ADC_VAR_FAST

#if (defined ADC_VAR) /* to prevent double definition */
#error ADC_VAR already defined
#endif /* if (defined ADC_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define ADC_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined CAN_CODE) /* to prevent double definition */
#error CAN_CODE already defined
#endif /* if (defined CAN_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define CAN_CODE

#if (defined CAN_CONST) /* to prevent double definition */
#error CAN_CONST already defined
#endif /* if (defined CAN_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define CAN_CONST

#if (defined CAN_APPL_DATA) /* to prevent double definition */
#error CAN_APPL_DATA already defined
#endif /* if (defined CAN_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define CAN_APPL_DATA

#if (defined CAN_APPL_CONST) /* to prevent double definition */
#error CAN_APPL_CONST already defined
#endif /* if (defined CAN_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define CAN_APPL_CONST

#if (defined CAN_APPL_CODE) /* to prevent double definition */
#error CAN_APPL_CODE already defined
#endif /* if (defined CAN_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define CAN_APPL_CODE

#if (defined CAN_VAR_NOINIT) /* to prevent double definition */
#error CAN_VAR_NOINIT already defined
#endif /* if (defined CAN_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define CAN_VAR_NOINIT

#if (defined CAN_VAR_POWER_ON_INIT) /* to prevent double definition */
#error CAN_VAR_POWER_ON_INIT already defined
#endif /* if (defined CAN_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define CAN_VAR_POWER_ON_INIT

#if (defined CAN_VAR_FAST) /* to prevent double definition */
#error CAN_VAR_FAST already defined
#endif /* if (defined CAN_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define CAN_VAR_FAST

#if (defined CAN_VAR) /* to prevent double definition */
#error CAN_VAR already defined
#endif /* if (defined CAN_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define CAN_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined CANIF_CODE) /* to prevent double definition */
#error CANIF_CODE already defined
#endif /* if (defined CANIF_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define CANIF_CODE

#if (defined CANIF_CONST) /* to prevent double definition */
#error CANIF_CONST already defined
#endif /* if (defined CANIF_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define CANIF_CONST

#if (defined CANIF_APPL_DATA) /* to prevent double definition */
#error CANIF_APPL_DATA already defined
#endif /* if (defined CANIF_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define CANIF_APPL_DATA

#if (defined CANIF_APPL_CONST) /* to prevent double definition */
#error CANIF_APPL_CONST already defined
#endif /* if (defined CANIF_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define CANIF_APPL_CONST

#if (defined CANIF_APPL_CODE) /* to prevent double definition */
#error CANIF_APPL_CODE already defined
#endif /* if (defined CANIF_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define CANIF_APPL_CODE

#if (defined CANIF_VAR_NOINIT) /* to prevent double definition */
#error CANIF_VAR_NOINIT already defined
#endif /* if (defined CANIF_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define CANIF_VAR_NOINIT

#if (defined CANIF_VAR_POWER_ON_INIT) /* to prevent double definition */
#error CANIF_VAR_POWER_ON_INIT already defined
#endif /* if (defined CANIF_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define CANIF_VAR_POWER_ON_INIT

#if (defined CANIF_VAR_FAST) /* to prevent double definition */
#error CANIF_VAR_FAST already defined
#endif /* if (defined CANIF_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define CANIF_VAR_FAST

#if (defined CANIF_VAR) /* to prevent double definition */
#error CANIF_VAR already defined
#endif /* if (defined CANIF_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define CANIF_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined CANNM_CODE) /* to prevent double definition */
#error CANNM_CODE already defined
#endif /* if (defined CANNM_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define CANNM_CODE

#if (defined CANNM_CONST) /* to prevent double definition */
#error CANNM_CONST already defined
#endif /* if (defined CANNM_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define CANNM_CONST

#if (defined CANNM_APPL_DATA) /* to prevent double definition */
#error CANNM_APPL_DATA already defined
#endif /* if (defined CANNM_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define CANNM_APPL_DATA

#if (defined CANNM_APPL_CONST) /* to prevent double definition */
#error CANNM_APPL_CONST already defined
#endif /* if (defined CANNM_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define CANNM_APPL_CONST

#if (defined CANNM_APPL_CODE) /* to prevent double definition */
#error CANNM_APPL_CODE already defined
#endif /* if (defined CANNM_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define CANNM_APPL_CODE

#if (defined CANNM_VAR_NOINIT) /* to prevent double definition */
#error CANNM_VAR_NOINIT already defined
#endif /* if (defined CANNM_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define CANNM_VAR_NOINIT

#if (defined CANNM_VAR_POWER_ON_INIT) /* to prevent double definition */
#error CANNM_VAR_POWER_ON_INIT already defined
#endif /* if (defined CANNM_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define CANNM_VAR_POWER_ON_INIT

#if (defined CANNM_VAR_FAST) /* to prevent double definition */
#error CANNM_VAR_FAST already defined
#endif /* if (defined CANNM_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define CANNM_VAR_FAST

#if (defined CANNM_VAR) /* to prevent double definition */
#error CANNM_VAR already defined
#endif /* if (defined CANNM_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define CANNM_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined CANTP_CODE) /* to prevent double definition */
#error CANTP_CODE already defined
#endif /* if (defined CANTP_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define CANTP_CODE

#if (defined CANTP_CONST) /* to prevent double definition */
#error CANTP_CONST already defined
#endif /* if (defined CANTP_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define CANTP_CONST

#if (defined CANTP_APPL_DATA) /* to prevent double definition */
#error CANTP_APPL_DATA already defined
#endif /* if (defined CANTP_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define CANTP_APPL_DATA

#if (defined CANTP_APPL_CONST) /* to prevent double definition */
#error CANTP_APPL_CONST already defined
#endif /* if (defined CANTP_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define CANTP_APPL_CONST

#if (defined CANTP_APPL_CODE) /* to prevent double definition */
#error CANTP_APPL_CODE already defined
#endif /* if (defined CANTP_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define CANTP_APPL_CODE

#if (defined CANTP_VAR_NOINIT) /* to prevent double definition */
#error CANTP_VAR_NOINIT already defined
#endif /* if (defined CANTP_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define CANTP_VAR_NOINIT

#if (defined CANTP_VAR_POWER_ON_INIT) /* to prevent double definition */
#error CANTP_VAR_POWER_ON_INIT already defined
#endif /* if (defined CANTP_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define CANTP_VAR_POWER_ON_INIT

#if (defined CANTP_VAR_FAST) /* to prevent double definition */
#error CANTP_VAR_FAST already defined
#endif /* if (defined CANTP_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define CANTP_VAR_FAST

#if (defined CANTP_VAR) /* to prevent double definition */
#error CANTP_VAR already defined
#endif /* if (defined CANTP_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define CANTP_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined CANTRCV_CODE) /* to prevent double definition */
#error CANTRCV_CODE already defined
#endif /* if (defined CANTRCV_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define CANTRCV_CODE

#if (defined CANTRCV_CONST) /* to prevent double definition */
#error CANTRCV_CONST already defined
#endif /* if (defined CANTRCV_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define CANTRCV_CONST

#if (defined CANTRCV_APPL_DATA) /* to prevent double definition */
#error CANTRCV_APPL_DATA already defined
#endif /* if (defined CANTRCV_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define CANTRCV_APPL_DATA

#if (defined CANTRCV_APPL_CONST) /* to prevent double definition */
#error CANTRCV_APPL_CONST already defined
#endif /* if (defined CANTRCV_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define CANTRCV_APPL_CONST

#if (defined CANTRCV_APPL_CODE) /* to prevent double definition */
#error CANTRCV_APPL_CODE already defined
#endif /* if (defined CANTRCV_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define CANTRCV_APPL_CODE

#if (defined CANTRCV_VAR_NOINIT) /* to prevent double definition */
#error CANTRCV_VAR_NOINIT already defined
#endif /* if (defined CANTRCV_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define CANTRCV_VAR_NOINIT

#if (defined CANTRCV_VAR_POWER_ON_INIT) /* to prevent double definition */
#error CANTRCV_VAR_POWER_ON_INIT already defined
#endif /* if (defined CANTRCV_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define CANTRCV_VAR_POWER_ON_INIT

#if (defined CANTRCV_VAR_FAST) /* to prevent double definition */
#error CANTRCV_VAR_FAST already defined
#endif /* if (defined CANTRCV_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define CANTRCV_VAR_FAST

#if (defined CANTRCV_VAR) /* to prevent double definition */
#error CANTRCV_VAR already defined
#endif /* if (defined CANTRCV_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define CANTRCV_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined COM_CODE) /* to prevent double definition */
#error COM_CODE already defined
#endif /* if (defined COM_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define COM_CODE

#if (defined COM_CONST) /* to prevent double definition */
#error COM_CONST already defined
#endif /* if (defined COM_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define COM_CONST

#if (defined COM_APPL_DATA) /* to prevent double definition */
#error COM_APPL_DATA already defined
#endif /* if (defined COM_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define COM_APPL_DATA

#if (defined COM_APPL_CONST) /* to prevent double definition */
#error COM_APPL_CONST already defined
#endif /* if (defined COM_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define COM_APPL_CONST

#if (defined COM_APPL_CODE) /* to prevent double definition */
#error COM_APPL_CODE already defined
#endif /* if (defined COM_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define COM_APPL_CODE

#if (defined COM_VAR_NOINIT) /* to prevent double definition */
#error COM_VAR_NOINIT already defined
#endif /* if (defined COM_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define COM_VAR_NOINIT

#if (defined COM_VAR_POWER_ON_INIT) /* to prevent double definition */
#error COM_VAR_POWER_ON_INIT already defined
#endif /* if (defined COM_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define COM_VAR_POWER_ON_INIT

#if (defined COM_VAR_FAST) /* to prevent double definition */
#error COM_VAR_FAST already defined
#endif /* if (defined COM_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define COM_VAR_FAST

#if (defined COM_VAR) /* to prevent double definition */
#error COM_VAR already defined
#endif /* if (defined COM_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define COM_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined COMM_CODE) /* to prevent double definition */
#error COMM_CODE already defined
#endif /* if (defined COMM_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define COMM_CODE

#if (defined COMM_CONST) /* to prevent double definition */
#error COMM_CONST already defined
#endif /* if (defined COMM_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define COMM_CONST

#if (defined COMM_APPL_DATA) /* to prevent double definition */
#error COMM_APPL_DATA already defined
#endif /* if (defined COMM_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define COMM_APPL_DATA

#if (defined COMM_APPL_CONST) /* to prevent double definition */
#error COMM_APPL_CONST already defined
#endif /* if (defined COMM_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define COMM_APPL_CONST

#if (defined COMM_APPL_CODE) /* to prevent double definition */
#error COMM_APPL_CODE already defined
#endif /* if (defined COMM_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define COMM_APPL_CODE

#if (defined COMM_VAR_NOINIT) /* to prevent double definition */
#error COMM_VAR_NOINIT already defined
#endif /* if (defined COMM_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define COMM_VAR_NOINIT

#if (defined COMM_VAR_POWER_ON_INIT) /* to prevent double definition */
#error COMM_VAR_POWER_ON_INIT already defined
#endif /* if (defined COMM_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define COMM_VAR_POWER_ON_INIT

#if (defined COMM_VAR_FAST) /* to prevent double definition */
#error COMM_VAR_FAST already defined
#endif /* if (defined COMM_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define COMM_VAR_FAST

#if (defined COMM_VAR) /* to prevent double definition */
#error COMM_VAR already defined
#endif /* if (defined COMM_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define COMM_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined CRC_CODE) /* to prevent double definition */
#error CRC_CODE already defined
#endif /* if (defined CRC_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define CRC_CODE

#if (defined CRC_CONST) /* to prevent double definition */
#error CRC_CONST already defined
#endif /* if (defined CRC_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define CRC_CONST

#if (defined CRC_APPL_DATA) /* to prevent double definition */
#error CRC_APPL_DATA already defined
#endif /* if (defined CRC_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define CRC_APPL_DATA

#if (defined CRC_APPL_CONST) /* to prevent double definition */
#error CRC_APPL_CONST already defined
#endif /* if (defined CRC_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define CRC_APPL_CONST

#if (defined CRC_APPL_CODE) /* to prevent double definition */
#error CRC_APPL_CODE already defined
#endif /* if (defined CRC_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define CRC_APPL_CODE

#if (defined CRC_VAR_NOINIT) /* to prevent double definition */
#error CRC_VAR_NOINIT already defined
#endif /* if (defined CRC_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define CRC_VAR_NOINIT

#if (defined CRC_VAR_POWER_ON_INIT) /* to prevent double definition */
#error CRC_VAR_POWER_ON_INIT already defined
#endif /* if (defined CRC_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define CRC_VAR_POWER_ON_INIT

#if (defined CRC_VAR_FAST) /* to prevent double definition */
#error CRC_VAR_FAST already defined
#endif /* if (defined CRC_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define CRC_VAR_FAST

#if (defined CRC_VAR) /* to prevent double definition */
#error CRC_VAR already defined
#endif /* if (defined CRC_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define CRC_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined DCM_CODE) /* to prevent double definition */
#error DCM_CODE already defined
#endif /* if (defined DCM_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define DCM_CODE

#if (defined DCM_CONST) /* to prevent double definition */
#error DCM_CONST already defined
#endif /* if (defined DCM_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define DCM_CONST

#if (defined DCM_APPL_DATA) /* to prevent double definition */
#error DCM_APPL_DATA already defined
#endif /* if (defined DCM_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define DCM_APPL_DATA

#if (defined DCM_APPL_CONST) /* to prevent double definition */
#error DCM_APPL_CONST already defined
#endif /* if (defined DCM_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define DCM_APPL_CONST

#if (defined DCM_APPL_CODE) /* to prevent double definition */
#error DCM_APPL_CODE already defined
#endif /* if (defined DCM_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define DCM_APPL_CODE

#if (defined DCM_VAR_NOINIT) /* to prevent double definition */
#error DCM_VAR_NOINIT already defined
#endif /* if (defined DCM_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define DCM_VAR_NOINIT

#if (defined DCM_VAR_POWER_ON_INIT) /* to prevent double definition */
#error DCM_VAR_POWER_ON_INIT already defined
#endif /* if (defined DCM_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define DCM_VAR_POWER_ON_INIT

#if (defined DCM_VAR_FAST) /* to prevent double definition */
#error DCM_VAR_FAST already defined
#endif /* if (defined DCM_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define DCM_VAR_FAST

#if (defined DCM_VAR) /* to prevent double definition */
#error DCM_VAR already defined
#endif /* if (defined DCM_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define DCM_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined DEM_CODE) /* to prevent double definition */
#error DEM_CODE already defined
#endif /* if (defined DEM_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define DEM_CODE

#if (defined DEM_CONST) /* to prevent double definition */
#error DEM_CONST already defined
#endif /* if (defined DEM_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define DEM_CONST

#if (defined DEM_APPL_DATA) /* to prevent double definition */
#error DEM_APPL_DATA already defined
#endif /* if (defined DEM_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define DEM_APPL_DATA

#if (defined DEM_APPL_CONST) /* to prevent double definition */
#error DEM_APPL_CONST already defined
#endif /* if (defined DEM_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define DEM_APPL_CONST

#if (defined DEM_APPL_CODE) /* to prevent double definition */
#error DEM_APPL_CODE already defined
#endif /* if (defined DEM_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define DEM_APPL_CODE

#if (defined DEM_VAR_NOINIT) /* to prevent double definition */
#error DEM_VAR_NOINIT already defined
#endif /* if (defined DEM_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define DEM_VAR_NOINIT

#if (defined DEM_VAR_POWER_ON_INIT) /* to prevent double definition */
#error DEM_VAR_POWER_ON_INIT already defined
#endif /* if (defined DEM_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define DEM_VAR_POWER_ON_INIT

#if (defined DEM_VAR_FAST) /* to prevent double definition */
#error DEM_VAR_FAST already defined
#endif /* if (defined DEM_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define DEM_VAR_FAST

#if (defined DEM_VAR) /* to prevent double definition */
#error DEM_VAR already defined
#endif /* if (defined DEM_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define DEM_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined DET_CODE) /* to prevent double definition */
#error DET_CODE already defined
#endif /* if (defined DET_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define DET_CODE

#if (defined DET_CONST) /* to prevent double definition */
#error DET_CONST already defined
#endif /* if (defined DET_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define DET_CONST

#if (defined DET_APPL_DATA) /* to prevent double definition */
#error DET_APPL_DATA already defined
#endif /* if (defined DET_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define DET_APPL_DATA

#if (defined DET_APPL_CONST) /* to prevent double definition */
#error DET_APPL_CONST already defined
#endif /* if (defined DET_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define DET_APPL_CONST

#if (defined DET_APPL_CODE) /* to prevent double definition */
#error DET_APPL_CODE already defined
#endif /* if (defined DET_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define DET_APPL_CODE

#if (defined DET_VAR_NOINIT) /* to prevent double definition */
#error DET_VAR_NOINIT already defined
#endif /* if (defined DET_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define DET_VAR_NOINIT

#if (defined DET_VAR_POWER_ON_INIT) /* to prevent double definition */
#error DET_VAR_POWER_ON_INIT already defined
#endif /* if (defined DET_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define DET_VAR_POWER_ON_INIT

#if (defined DET_VAR_FAST) /* to prevent double definition */
#error DET_VAR_FAST already defined
#endif /* if (defined DET_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define DET_VAR_FAST

#if (defined DET_VAR) /* to prevent double definition */
#error DET_VAR already defined
#endif /* if (defined DET_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define DET_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined DIO_CODE) /* to prevent double definition */
#error DIO_CODE already defined
#endif /* if (defined DIO_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define DIO_CODE

#if (defined DIO_CONST) /* to prevent double definition */
#error DIO_CONST already defined
#endif /* if (defined DIO_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define DIO_CONST

#if (defined DIO_APPL_DATA) /* to prevent double definition */
#error DIO_APPL_DATA already defined
#endif /* if (defined DIO_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define DIO_APPL_DATA

#if (defined DIO_APPL_CONST) /* to prevent double definition */
#error DIO_APPL_CONST already defined
#endif /* if (defined DIO_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define DIO_APPL_CONST

#if (defined DIO_APPL_CODE) /* to prevent double definition */
#error DIO_APPL_CODE already defined
#endif /* if (defined DIO_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define DIO_APPL_CODE

#if (defined DIO_VAR_NOINIT) /* to prevent double definition */
#error DIO_VAR_NOINIT already defined
#endif /* if (defined DIO_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define DIO_VAR_NOINIT

#if (defined DIO_VAR_POWER_ON_INIT) /* to prevent double definition */
#error DIO_VAR_POWER_ON_INIT already defined
#endif /* if (defined DIO_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define DIO_VAR_POWER_ON_INIT

#if (defined DIO_VAR_FAST) /* to prevent double definition */
#error DIO_VAR_FAST already defined
#endif /* if (defined DIO_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define DIO_VAR_FAST

#if (defined DIO_VAR) /* to prevent double definition */
#error DIO_VAR already defined
#endif /* if (defined DIO_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define DIO_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined EA_CODE) /* to prevent double definition */
#error EA_CODE already defined
#endif /* if (defined EA_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define EA_CODE

#if (defined EA_CONST) /* to prevent double definition */
#error EA_CONST already defined
#endif /* if (defined EA_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define EA_CONST

#if (defined EA_APPL_DATA) /* to prevent double definition */
#error EA_APPL_DATA already defined
#endif /* if (defined EA_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define EA_APPL_DATA

#if (defined EA_APPL_CONST) /* to prevent double definition */
#error EA_APPL_CONST already defined
#endif /* if (defined EA_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define EA_APPL_CONST

#if (defined EA_APPL_CODE) /* to prevent double definition */
#error EA_APPL_CODE already defined
#endif /* if (defined EA_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define EA_APPL_CODE

#if (defined EA_VAR_NOINIT) /* to prevent double definition */
#error EA_VAR_NOINIT already defined
#endif /* if (defined EA_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define EA_VAR_NOINIT

#if (defined EA_VAR_POWER_ON_INIT) /* to prevent double definition */
#error EA_VAR_POWER_ON_INIT already defined
#endif /* if (defined EA_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define EA_VAR_POWER_ON_INIT

#if (defined EA_VAR_FAST) /* to prevent double definition */
#error EA_VAR_FAST already defined
#endif /* if (defined EA_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define EA_VAR_FAST

#if (defined EA_VAR) /* to prevent double definition */
#error EA_VAR already defined
#endif /* if (defined EA_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define EA_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined ECUM_CODE) /* to prevent double definition */
#error ECUM_CODE already defined
#endif /* if (defined ECUM_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define ECUM_CODE

#if (defined ECUM_CONST) /* to prevent double definition */
#error ECUM_CONST already defined
#endif /* if (defined ECUM_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define ECUM_CONST

#if (defined ECUM_APPL_DATA) /* to prevent double definition */
#error ECUM_APPL_DATA already defined
#endif /* if (defined ECUM_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define ECUM_APPL_DATA

#if (defined ECUM_APPL_CONST) /* to prevent double definition */
#error ECUM_APPL_CONST already defined
#endif /* if (defined ECUM_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define ECUM_APPL_CONST

#if (defined ECUM_APPL_CODE) /* to prevent double definition */
#error ECUM_APPL_CODE already defined
#endif /* if (defined ECUM_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define ECUM_APPL_CODE

#if (defined ECUM_VAR_NOINIT) /* to prevent double definition */
#error ECUM_VAR_NOINIT already defined
#endif /* if (defined ECUM_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define ECUM_VAR_NOINIT

#if (defined ECUM_VAR_POWER_ON_INIT) /* to prevent double definition */
#error ECUM_VAR_POWER_ON_INIT already defined
#endif /* if (defined ECUM_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define ECUM_VAR_POWER_ON_INIT

#if (defined ECUM_VAR_FAST) /* to prevent double definition */
#error ECUM_VAR_FAST already defined
#endif /* if (defined ECUM_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define ECUM_VAR_FAST

#if (defined ECUM_VAR) /* to prevent double definition */
#error ECUM_VAR already defined
#endif /* if (defined ECUM_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define ECUM_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined EEP_CODE) /* to prevent double definition */
#error EEP_CODE already defined
#endif /* if (defined EEP_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define EEP_CODE

#if (defined EEP_CONST) /* to prevent double definition */
#error EEP_CONST already defined
#endif /* if (defined EEP_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define EEP_CONST

#if (defined EEP_APPL_DATA) /* to prevent double definition */
#error EEP_APPL_DATA already defined
#endif /* if (defined EEP_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define EEP_APPL_DATA

#if (defined EEP_APPL_CONST) /* to prevent double definition */
#error EEP_APPL_CONST already defined
#endif /* if (defined EEP_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define EEP_APPL_CONST

#if (defined EEP_APPL_CODE) /* to prevent double definition */
#error EEP_APPL_CODE already defined
#endif /* if (defined EEP_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define EEP_APPL_CODE

#if (defined EEP_VAR_NOINIT) /* to prevent double definition */
#error EEP_VAR_NOINIT already defined
#endif /* if (defined EEP_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define EEP_VAR_NOINIT

#if (defined EEP_VAR_POWER_ON_INIT) /* to prevent double definition */
#error EEP_VAR_POWER_ON_INIT already defined
#endif /* if (defined EEP_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define EEP_VAR_POWER_ON_INIT

#if (defined EEP_VAR_FAST) /* to prevent double definition */
#error EEP_VAR_FAST already defined
#endif /* if (defined EEP_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define EEP_VAR_FAST

#if (defined EEP_VAR) /* to prevent double definition */
#error EEP_VAR already defined
#endif /* if (defined EEP_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define EEP_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined EEPIF_CODE) /* to prevent double definition */
#error EEPIF_CODE already defined
#endif /* if (defined EEPIF_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define EEPIF_CODE

#if (defined EEPIF_CONST) /* to prevent double definition */
#error EEPIF_CONST already defined
#endif /* if (defined EEPIF_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define EEPIF_CONST

#if (defined EEPIF_APPL_DATA) /* to prevent double definition */
#error EEPIF_APPL_DATA already defined
#endif /* if (defined EEPIF_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define EEPIF_APPL_DATA

#if (defined EEPIF_APPL_CONST) /* to prevent double definition */
#error EEPIF_APPL_CONST already defined
#endif /* if (defined EEPIF_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define EEPIF_APPL_CONST

#if (defined EEPIF_APPL_CODE) /* to prevent double definition */
#error EEPIF_APPL_CODE already defined
#endif /* if (defined EEPIF_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define EEPIF_APPL_CODE

#if (defined EEPIF_VAR_NOINIT) /* to prevent double definition */
#error EEPIF_VAR_NOINIT already defined
#endif /* if (defined EEPIF_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define EEPIF_VAR_NOINIT

#if (defined EEPIF_VAR_POWER_ON_INIT) /* to prevent double definition */
#error EEPIF_VAR_POWER_ON_INIT already defined
#endif /* if (defined EEPIF_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define EEPIF_VAR_POWER_ON_INIT

#if (defined EEPIF_VAR_FAST) /* to prevent double definition */
#error EEPIF_VAR_FAST already defined
#endif /* if (defined EEPIF_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define EEPIF_VAR_FAST

#if (defined EEPIF_VAR) /* to prevent double definition */
#error EEPIF_VAR already defined
#endif /* if (defined EEPIF_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define EEPIF_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined EEP_MC25C160_CODE) /* to prevent double definition */
#error EEP_MC25C160_CODE already defined
#endif /* if (defined EEP_MC25C160_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define EEP_MC25C160_CODE

#if (defined EEP_MC25C160_CONST) /* to prevent double definition */
#error EEP_MC25C160_CONST already defined
#endif /* if (defined EEP_MC25C160_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define EEP_MC25C160_CONST

#if (defined EEP_MC25C160_APPL_DATA) /* to prevent double definition */
#error EEP_MC25C160_APPL_DATA already defined
#endif /* if (defined EEP_MC25C160_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define EEP_MC25C160_APPL_DATA

#if (defined EEP_MC25C160_APPL_CONST) /* to prevent double definition */
#error EEP_MC25C160_APPL_CONST already defined
#endif /* if (defined EEP_MC25C160_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define EEP_MC25C160_APPL_CONST

#if (defined EEP_MC25C160_APPL_CODE) /* to prevent double definition */
#error EEP_MC25C160_APPL_CODE already defined
#endif /* if (defined EEP_MC25C160_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define EEP_MC25C160_APPL_CODE

#if (defined EEP_MC25C160_VAR_NOINIT) /* to prevent double definition */
#error EEP_MC25C160_VAR_NOINIT already defined
#endif /* if (defined EEP_MC25C160_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define EEP_MC25C160_VAR_NOINIT

#if (defined EEP_MC25C160_VAR_POWER_ON_INIT) /* to prevent double definition */
#error EEP_MC25C160_VAR_POWER_ON_INIT already defined
#endif /* if (defined EEP_MC25C160_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define EEP_MC25C160_VAR_POWER_ON_INIT

#if (defined EEP_MC25C160_VAR_FAST) /* to prevent double definition */
#error EEP_MC25C160_VAR_FAST already defined
#endif /* if (defined EEP_MC25C160_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define EEP_MC25C160_VAR_FAST

#if (defined EEP_MC25C160_VAR) /* to prevent double definition */
#error EEP_MC25C160_VAR already defined
#endif /* if (defined EEP_MC25C160_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define EEP_MC25C160_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined FEE_CODE) /* to prevent double definition */
#error FEE_CODE already defined
#endif /* if (defined FEE_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define FEE_CODE

#if (defined FEE_CONST) /* to prevent double definition */
#error FEE_CONST already defined
#endif /* if (defined FEE_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define FEE_CONST

#if (defined FEE_APPL_DATA) /* to prevent double definition */
#error FEE_APPL_DATA already defined
#endif /* if (defined FEE_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define FEE_APPL_DATA

#if (defined FEE_APPL_CONST) /* to prevent double definition */
#error FEE_APPL_CONST already defined
#endif /* if (defined FEE_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define FEE_APPL_CONST

#if (defined FEE_APPL_CODE) /* to prevent double definition */
#error FEE_APPL_CODE already defined
#endif /* if (defined FEE_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define FEE_APPL_CODE

#if (defined FEE_VAR_NOINIT) /* to prevent double definition */
#error FEE_VAR_NOINIT already defined
#endif /* if (defined FEE_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define FEE_VAR_NOINIT

#if (defined FEE_VAR_POWER_ON_INIT) /* to prevent double definition */
#error FEE_VAR_POWER_ON_INIT already defined
#endif /* if (defined FEE_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define FEE_VAR_POWER_ON_INIT

#if (defined FEE_VAR_FAST) /* to prevent double definition */
#error FEE_VAR_FAST already defined
#endif /* if (defined FEE_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define FEE_VAR_FAST

#if (defined FEE_VAR) /* to prevent double definition */
#error FEE_VAR already defined
#endif /* if (defined FEE_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define FEE_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined FIM_CODE) /* to prevent double definition */
#error FIM_CODE already defined
#endif /* if (defined FIM_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define FIM_CODE

#if (defined FIM_CONST) /* to prevent double definition */
#error FIM_CONST already defined
#endif /* if (defined FIM_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define FIM_CONST

#if (defined FIM_APPL_DATA) /* to prevent double definition */
#error FIM_APPL_DATA already defined
#endif /* if (defined FIM_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define FIM_APPL_DATA

#if (defined FIM_APPL_CONST) /* to prevent double definition */
#error FIM_APPL_CONST already defined
#endif /* if (defined FIM_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define FIM_APPL_CONST

#if (defined FIM_APPL_CODE) /* to prevent double definition */
#error FIM_APPL_CODE already defined
#endif /* if (defined FIM_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define FIM_APPL_CODE

#if (defined FIM_VAR_NOINIT) /* to prevent double definition */
#error FIM_VAR_NOINIT already defined
#endif /* if (defined FIM_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define FIM_VAR_NOINIT

#if (defined FIM_VAR_POWER_ON_INIT) /* to prevent double definition */
#error FIM_VAR_POWER_ON_INIT already defined
#endif /* if (defined FIM_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define FIM_VAR_POWER_ON_INIT

#if (defined FIM_VAR_FAST) /* to prevent double definition */
#error FIM_VAR_FAST already defined
#endif /* if (defined FIM_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define FIM_VAR_FAST

#if (defined FIM_VAR) /* to prevent double definition */
#error FIM_VAR already defined
#endif /* if (defined FIM_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define FIM_VAR


#if (defined FIM_PBCFG_CONST) /* to prevent double definition */
#error FIM_FIM_PBCFG_CONST already defined
#endif /* if (FIM_PBCFG_CONST) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define FIM_PBCFG_CONST
/*------------------[memory and pointer class of a module]-------------------*/

#if (defined FLS_CODE) /* to prevent double definition */
#error FLS_CODE already defined
#endif /* if (defined FLS_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define FLS_CODE

#if (defined FLS_CONST) /* to prevent double definition */
#error FLS_CONST already defined
#endif /* if (defined FLS_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define FLS_CONST

#if (defined FLS_APPL_DATA) /* to prevent double definition */
#error FLS_APPL_DATA already defined
#endif /* if (defined FLS_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define FLS_APPL_DATA

#if (defined FLS_APPL_CONST) /* to prevent double definition */
#error FLS_APPL_CONST already defined
#endif /* if (defined FLS_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define FLS_APPL_CONST

#if (defined FLS_APPL_CODE) /* to prevent double definition */
#error FLS_APPL_CODE already defined
#endif /* if (defined FLS_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define FLS_APPL_CODE

#if (defined FLS_VAR_NOINIT) /* to prevent double definition */
#error FLS_VAR_NOINIT already defined
#endif /* if (defined FLS_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define FLS_VAR_NOINIT

#if (defined FLS_VAR_POWER_ON_INIT) /* to prevent double definition */
#error FLS_VAR_POWER_ON_INIT already defined
#endif /* if (defined FLS_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define FLS_VAR_POWER_ON_INIT

#if (defined FLS_VAR_FAST) /* to prevent double definition */
#error FLS_VAR_FAST already defined
#endif /* if (defined FLS_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define FLS_VAR_FAST

#if (defined FLS_VAR) /* to prevent double definition */
#error FLS_VAR already defined
#endif /* if (defined FLS_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define FLS_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined FLSIF_CODE) /* to prevent double definition */
#error FLSIF_CODE already defined
#endif /* if (defined FLSIF_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define FLSIF_CODE

#if (defined FLSIF_CONST) /* to prevent double definition */
#error FLSIF_CONST already defined
#endif /* if (defined FLSIF_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define FLSIF_CONST

#if (defined FLSIF_APPL_DATA) /* to prevent double definition */
#error FLSIF_APPL_DATA already defined
#endif /* if (defined FLSIF_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define FLSIF_APPL_DATA

#if (defined FLSIF_APPL_CONST) /* to prevent double definition */
#error FLSIF_APPL_CONST already defined
#endif /* if (defined FLSIF_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define FLSIF_APPL_CONST

#if (defined FLSIF_APPL_CODE) /* to prevent double definition */
#error FLSIF_APPL_CODE already defined
#endif /* if (defined FLSIF_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define FLSIF_APPL_CODE

#if (defined FLSIF_VAR_NOINIT) /* to prevent double definition */
#error FLSIF_VAR_NOINIT already defined
#endif /* if (defined FLSIF_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define FLSIF_VAR_NOINIT

#if (defined FLSIF_VAR_POWER_ON_INIT) /* to prevent double definition */
#error FLSIF_VAR_POWER_ON_INIT already defined
#endif /* if (defined FLSIF_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define FLSIF_VAR_POWER_ON_INIT

#if (defined FLSIF_VAR_FAST) /* to prevent double definition */
#error FLSIF_VAR_FAST already defined
#endif /* if (defined FLSIF_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define FLSIF_VAR_FAST

#if (defined FLSIF_VAR) /* to prevent double definition */
#error FLSIF_VAR already defined
#endif /* if (defined FLSIF_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define FLSIF_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined FR_CODE) /* to prevent double definition */
#error FR_CODE already defined
#endif /* if (defined FR_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define FR_CODE

#if (defined FR_CONST) /* to prevent double definition */
#error FR_CONST already defined
#endif /* if (defined FR_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define FR_CONST

#if (defined FR_APPL_DATA) /* to prevent double definition */
#error FR_APPL_DATA already defined
#endif /* if (defined FR_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define FR_APPL_DATA

#if (defined FR_APPL_CONST) /* to prevent double definition */
#error FR_APPL_CONST already defined
#endif /* if (defined FR_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define FR_APPL_CONST

#if (defined FR_APPL_CODE) /* to prevent double definition */
#error FR_APPL_CODE already defined
#endif /* if (defined FR_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define FR_APPL_CODE

#if (defined FR_VAR_NOINIT) /* to prevent double definition */
#error FR_VAR_NOINIT already defined
#endif /* if (defined FR_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define FR_VAR_NOINIT

#if (defined FR_VAR_POWER_ON_INIT) /* to prevent double definition */
#error FR_VAR_POWER_ON_INIT already defined
#endif /* if (defined FR_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define FR_VAR_POWER_ON_INIT

#if (defined FR_VAR_FAST) /* to prevent double definition */
#error FR_VAR_FAST already defined
#endif /* if (defined FR_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define FR_VAR_FAST

#if (defined FR_VAR) /* to prevent double definition */
#error FR_VAR already defined
#endif /* if (defined FR_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define FR_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined FRIF_CODE) /* to prevent double definition */
#error FRIF_CODE already defined
#endif /* if (defined FRIF_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define FRIF_CODE

#if (defined FRIF_CONST) /* to prevent double definition */
#error FRIF_CONST already defined
#endif /* if (defined FRIF_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define FRIF_CONST

#if (defined FRIF_APPL_DATA) /* to prevent double definition */
#error FRIF_APPL_DATA already defined
#endif /* if (defined FRIF_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define FRIF_APPL_DATA

#if (defined FRIF_APPL_CONST) /* to prevent double definition */
#error FRIF_APPL_CONST already defined
#endif /* if (defined FRIF_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define FRIF_APPL_CONST

#if (defined FRIF_APPL_CODE) /* to prevent double definition */
#error FRIF_APPL_CODE already defined
#endif /* if (defined FRIF_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define FRIF_APPL_CODE

#if (defined FRIF_VAR_NOINIT) /* to prevent double definition */
#error FRIF_VAR_NOINIT already defined
#endif /* if (defined FRIF_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define FRIF_VAR_NOINIT

#if (defined FRIF_VAR_POWER_ON_INIT) /* to prevent double definition */
#error FRIF_VAR_POWER_ON_INIT already defined
#endif /* if (defined FRIF_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define FRIF_VAR_POWER_ON_INIT

#if (defined FRIF_VAR_FAST) /* to prevent double definition */
#error FRIF_VAR_FAST already defined
#endif /* if (defined FRIF_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define FRIF_VAR_FAST

#if (defined FRIF_VAR) /* to prevent double definition */
#error FRIF_VAR already defined
#endif /* if (defined FRIF_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define FRIF_VAR


/*------------------[memory and pointer class of a module]-------------------*/

#if (defined FRSM_CODE) /* to prevent double definition */
#error FRSM_CODE already defined
#endif /* if (defined FRSM_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define FRSM_CODE

#if (defined FRSM_CONST) /* to prevent double definition */
#error FRSM_CONST already defined
#endif /* if (defined FRSM_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define FRSM_CONST

#if (defined FRSM_APPL_DATA) /* to prevent double definition */
#error FRSM_APPL_DATA already defined
#endif /* if (defined FRSM_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define FRSM_APPL_DATA

#if (defined FRSM_APPL_CONST) /* to prevent double definition */
#error FRSM_APPL_CONST already defined
#endif /* if (defined FRSM_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define FRSM_APPL_CONST

#if (defined FRSM_APPL_CODE) /* to prevent double definition */
#error FRSM_APPL_CODE already defined
#endif /* if (defined FRSM_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define FRSM_APPL_CODE

#if (defined FRSM_VAR_NOINIT) /* to prevent double definition */
#error FRSM_VAR_NOINIT already defined
#endif /* if (defined FRSM_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define FRSM_VAR_NOINIT

#if (defined FRSM_VAR_POWER_ON_INIT) /* to prevent double definition */
#error FRSM_VAR_POWER_ON_INIT already defined
#endif /* if (defined FRSM_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define FRSM_VAR_POWER_ON_INIT

#if (defined FRSM_VAR_FAST) /* to prevent double definition */
#error FRSM_VAR_FAST already defined
#endif /* if (defined FRSM_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define FRSM_VAR_FAST

#if (defined FRSM_VAR) /* to prevent double definition */
#error FRSM_VAR already defined
#endif /* if (defined FRSM_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define FRSM_VAR



/*------------------[memory and pointer class of a module]-------------------*/

#if (defined FRNM_CODE) /* to prevent double definition */
#error FRNM_CODE already defined
#endif /* if (defined FRNM_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define FRNM_CODE

#if (defined FRNM_CONST) /* to prevent double definition */
#error FRNM_CONST already defined
#endif /* if (defined FRNM_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define FRNM_CONST

#if (defined FRNM_APPL_DATA) /* to prevent double definition */
#error FRNM_APPL_DATA already defined
#endif /* if (defined FRNM_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define FRNM_APPL_DATA

#if (defined FRNM_APPL_CONST) /* to prevent double definition */
#error FRNM_APPL_CONST already defined
#endif /* if (defined FRNM_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define FRNM_APPL_CONST

#if (defined FRNM_APPL_CODE) /* to prevent double definition */
#error FRNM_APPL_CODE already defined
#endif /* if (defined FRNM_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define FRNM_APPL_CODE

#if (defined FRNM_VAR_NOINIT) /* to prevent double definition */
#error FRNM_VAR_NOINIT already defined
#endif /* if (defined FRNM_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define FRNM_VAR_NOINIT

#if (defined FRNM_VAR_POWER_ON_INIT) /* to prevent double definition */
#error FRNM_VAR_POWER_ON_INIT already defined
#endif /* if (defined FRNM_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define FRNM_VAR_POWER_ON_INIT

#if (defined FRNM_VAR_FAST) /* to prevent double definition */
#error FRNM_VAR_FAST already defined
#endif /* if (defined FRNM_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define FRNM_VAR_FAST

#if (defined FRNM_VAR) /* to prevent double definition */
#error FRNM_VAR already defined
#endif /* if (defined FRNM_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define FRNM_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined FRTP_CODE) /* to prevent double definition */
#error FRTP_CODE already defined
#endif /* if (defined FRTP_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define FRTP_CODE

#if (defined FRTP_CONST) /* to prevent double definition */
#error FRTP_CONST already defined
#endif /* if (defined FRTP_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define FRTP_CONST

#if (defined FRTP_APPL_DATA) /* to prevent double definition */
#error FRTP_APPL_DATA already defined
#endif /* if (defined FRTP_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define FRTP_APPL_DATA

#if (defined FRTP_APPL_CONST) /* to prevent double definition */
#error FRTP_APPL_CONST already defined
#endif /* if (defined FRTP_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define FRTP_APPL_CONST

#if (defined FRTP_APPL_CODE) /* to prevent double definition */
#error FRTP_APPL_CODE already defined
#endif /* if (defined FRTP_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define FRTP_APPL_CODE

#if (defined FRTP_VAR_NOINIT) /* to prevent double definition */
#error FRTP_VAR_NOINIT already defined
#endif /* if (defined FRTP_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define FRTP_VAR_NOINIT

#if (defined FRTP_VAR_POWER_ON_INIT) /* to prevent double definition */
#error FRTP_VAR_POWER_ON_INIT already defined
#endif /* if (defined FRTP_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define FRTP_VAR_POWER_ON_INIT

#if (defined FRTP_VAR_FAST) /* to prevent double definition */
#error FRTP_VAR_FAST already defined
#endif /* if (defined FRTP_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define FRTP_VAR_FAST

#if (defined FRTP_VAR) /* to prevent double definition */
#error FRTP_VAR already defined
#endif /* if (defined FRTP_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define FRTP_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined FRTRCV_CODE) /* to prevent double definition */
#error FRTRCV_CODE already defined
#endif /* if (defined FRTRCV_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define FRTRCV_CODE

#if (defined FRTRCV_CONST) /* to prevent double definition */
#error FRTRCV_CONST already defined
#endif /* if (defined FRTRCV_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define FRTRCV_CONST

#if (defined FRTRCV_APPL_DATA) /* to prevent double definition */
#error FRTRCV_APPL_DATA already defined
#endif /* if (defined FRTRCV_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define FRTRCV_APPL_DATA

#if (defined FRTRCV_APPL_CONST) /* to prevent double definition */
#error FRTRCV_APPL_CONST already defined
#endif /* if (defined FRTRCV_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define FRTRCV_APPL_CONST

#if (defined FRTRCV_APPL_CODE) /* to prevent double definition */
#error FRTRCV_APPL_CODE already defined
#endif /* if (defined FRTRCV_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define FRTRCV_APPL_CODE

#if (defined FRTRCV_VAR_NOINIT) /* to prevent double definition */
#error FRTRCV_VAR_NOINIT already defined
#endif /* if (defined FRTRCV_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define FRTRCV_VAR_NOINIT

#if (defined FRTRCV_VAR_POWER_ON_INIT) /* to prevent double definition */
#error FRTRCV_VAR_POWER_ON_INIT already defined
#endif /* if (defined FRTRCV_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define FRTRCV_VAR_POWER_ON_INIT

#if (defined FRTRCV_VAR_FAST) /* to prevent double definition */
#error FRTRCV_VAR_FAST already defined
#endif /* if (defined FRTRCV_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define FRTRCV_VAR_FAST

#if (defined FRTRCV_VAR) /* to prevent double definition */
#error FRTRCV_VAR already defined
#endif /* if (defined FRTRCV_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define FRTRCV_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined GPT_CODE) /* to prevent double definition */
#error GPT_CODE already defined
#endif /* if (defined GPT_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define GPT_CODE

#if (defined GPT_CONST) /* to prevent double definition */
#error GPT_CONST already defined
#endif /* if (defined GPT_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define GPT_CONST

#if (defined GPT_APPL_DATA) /* to prevent double definition */
#error GPT_APPL_DATA already defined
#endif /* if (defined GPT_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define GPT_APPL_DATA

#if (defined GPT_APPL_CONST) /* to prevent double definition */
#error GPT_APPL_CONST already defined
#endif /* if (defined GPT_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define GPT_APPL_CONST

#if (defined GPT_APPL_CODE) /* to prevent double definition */
#error GPT_APPL_CODE already defined
#endif /* if (defined GPT_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define GPT_APPL_CODE

#if (defined GPT_VAR_NOINIT) /* to prevent double definition */
#error GPT_VAR_NOINIT already defined
#endif /* if (defined GPT_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define GPT_VAR_NOINIT

#if (defined GPT_VAR_POWER_ON_INIT) /* to prevent double definition */
#error GPT_VAR_POWER_ON_INIT already defined
#endif /* if (defined GPT_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define GPT_VAR_POWER_ON_INIT

#if (defined GPT_VAR_FAST) /* to prevent double definition */
#error GPT_VAR_FAST already defined
#endif /* if (defined GPT_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define GPT_VAR_FAST

#if (defined GPT_VAR) /* to prevent double definition */
#error GPT_VAR already defined
#endif /* if (defined GPT_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define GPT_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined GW_CODE) /* to prevent double definition */
#error GW_CODE already defined
#endif /* if (defined GW_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define GW_CODE

#if (defined GW_CONST) /* to prevent double definition */
#error GW_CONST already defined
#endif /* if (defined GW_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define GW_CONST

#if (defined GW_APPL_DATA) /* to prevent double definition */
#error GW_APPL_DATA already defined
#endif /* if (defined GW_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define GW_APPL_DATA

#if (defined GW_APPL_CONST) /* to prevent double definition */
#error GW_APPL_CONST already defined
#endif /* if (defined GW_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define GW_APPL_CONST

#if (defined GW_APPL_CODE) /* to prevent double definition */
#error GW_APPL_CODE already defined
#endif /* if (defined GW_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define GW_APPL_CODE

#if (defined GW_VAR_NOINIT) /* to prevent double definition */
#error GW_VAR_NOINIT already defined
#endif /* if (defined GW_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define GW_VAR_NOINIT

#if (defined GW_VAR_POWER_ON_INIT) /* to prevent double definition */
#error GW_VAR_POWER_ON_INIT already defined
#endif /* if (defined GW_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define GW_VAR_POWER_ON_INIT

#if (defined GW_VAR_FAST) /* to prevent double definition */
#error GW_VAR_FAST already defined
#endif /* if (defined GW_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define GW_VAR_FAST

#if (defined GW_VAR) /* to prevent double definition */
#error GW_VAR already defined
#endif /* if (defined GW_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define GW_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined ICU_CODE) /* to prevent double definition */
#error ICU_CODE already defined
#endif /* if (defined ICU_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define ICU_CODE

#if (defined ICU_CONST) /* to prevent double definition */
#error ICU_CONST already defined
#endif /* if (defined ICU_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define ICU_CONST

#if (defined ICU_APPL_DATA) /* to prevent double definition */
#error ICU_APPL_DATA already defined
#endif /* if (defined ICU_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define ICU_APPL_DATA

#if (defined ICU_APPL_CONST) /* to prevent double definition */
#error ICU_APPL_CONST already defined
#endif /* if (defined ICU_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define ICU_APPL_CONST

#if (defined ICU_APPL_CODE) /* to prevent double definition */
#error ICU_APPL_CODE already defined
#endif /* if (defined ICU_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define ICU_APPL_CODE

#if (defined ICU_VAR_NOINIT) /* to prevent double definition */
#error ICU_VAR_NOINIT already defined
#endif /* if (defined ICU_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define ICU_VAR_NOINIT

#if (defined ICU_VAR_POWER_ON_INIT) /* to prevent double definition */
#error ICU_VAR_POWER_ON_INIT already defined
#endif /* if (defined ICU_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define ICU_VAR_POWER_ON_INIT

#if (defined ICU_VAR_FAST) /* to prevent double definition */
#error ICU_VAR_FAST already defined
#endif /* if (defined ICU_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define ICU_VAR_FAST

#if (defined ICU_VAR) /* to prevent double definition */
#error ICU_VAR already defined
#endif /* if (defined ICU_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define ICU_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined IOHWAB_CODE) /* to prevent double definition */
#error IOHWAB_CODE already defined
#endif /* if (defined IOHWAB_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define IOHWAB_CODE

#if (defined IOHWAB_CONST) /* to prevent double definition */
#error IOHWAB_CONST already defined
#endif /* if (defined IOHWAB_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define IOHWAB_CONST

#if (defined IOHWAB_APPL_DATA) /* to prevent double definition */
#error IOHWAB_APPL_DATA already defined
#endif /* if (defined IOHWAB_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define IOHWAB_APPL_DATA

#if (defined IOHWAB_APPL_CONST) /* to prevent double definition */
#error IOHWAB_APPL_CONST already defined
#endif /* if (defined IOHWAB_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define IOHWAB_APPL_CONST

#if (defined IOHWAB_APPL_CODE) /* to prevent double definition */
#error IOHWAB_APPL_CODE already defined
#endif /* if (defined IOHWAB_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define IOHWAB_APPL_CODE

#if (defined IOHWAB_VAR_NOINIT) /* to prevent double definition */
#error IOHWAB_VAR_NOINIT already defined
#endif /* if (defined IOHWAB_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define IOHWAB_VAR_NOINIT

#if (defined IOHWAB_VAR_POWER_ON_INIT) /* to prevent double definition */
#error IOHWAB_VAR_POWER_ON_INIT already defined
#endif /* if (defined IOHWAB_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define IOHWAB_VAR_POWER_ON_INIT

#if (defined IOHWAB_VAR_FAST) /* to prevent double definition */
#error IOHWAB_VAR_FAST already defined
#endif /* if (defined IOHWAB_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define IOHWAB_VAR_FAST

#if (defined IOHWAB_VAR) /* to prevent double definition */
#error IOHWAB_VAR already defined
#endif /* if (defined IOHWAB_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define IOHWAB_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined IPDUM_CODE) /* to prevent double definition */
#error IPDUM_CODE already defined
#endif /* if (defined IPDUM_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define IPDUM_CODE

#if (defined IPDUM_CONST) /* to prevent double definition */
#error IPDUM_CONST already defined
#endif /* if (defined IPDUM_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define IPDUM_CONST

#if (defined IPDUM_APPL_DATA) /* to prevent double definition */
#error IPDUM_APPL_DATA already defined
#endif /* if (defined IPDUM_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define IPDUM_APPL_DATA

#if (defined IPDUM_APPL_CONST) /* to prevent double definition */
#error IPDUM_APPL_CONST already defined
#endif /* if (defined IPDUM_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define IPDUM_APPL_CONST

#if (defined IPDUM_APPL_CODE) /* to prevent double definition */
#error IPDUM_APPL_CODE already defined
#endif /* if (defined IPDUM_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define IPDUM_APPL_CODE

#if (defined IPDUM_VAR_NOINIT) /* to prevent double definition */
#error IPDUM_VAR_NOINIT already defined
#endif /* if (defined IPDUM_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define IPDUM_VAR_NOINIT

#if (defined IPDUM_VAR_POWER_ON_INIT) /* to prevent double definition */
#error IPDUM_VAR_POWER_ON_INIT already defined
#endif /* if (defined IPDUM_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define IPDUM_VAR_POWER_ON_INIT

#if (defined IPDUM_VAR_FAST) /* to prevent double definition */
#error IPDUM_VAR_FAST already defined
#endif /* if (defined IPDUM_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define IPDUM_VAR_FAST

#if (defined IPDUM_VAR) /* to prevent double definition */
#error IPDUM_VAR already defined
#endif /* if (defined IPDUM_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define IPDUM_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined LIN_CODE) /* to prevent double definition */
#error LIN_CODE already defined
#endif /* if (defined LIN_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define LIN_CODE

#if (defined LIN_CONST) /* to prevent double definition */
#error LIN_CONST already defined
#endif /* if (defined LIN_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define LIN_CONST

#if (defined LIN_APPL_DATA) /* to prevent double definition */
#error LIN_APPL_DATA already defined
#endif /* if (defined LIN_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define LIN_APPL_DATA

#if (defined LIN_APPL_CONST) /* to prevent double definition */
#error LIN_APPL_CONST already defined
#endif /* if (defined LIN_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define LIN_APPL_CONST

#if (defined LIN_APPL_CODE) /* to prevent double definition */
#error LIN_APPL_CODE already defined
#endif /* if (defined LIN_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define LIN_APPL_CODE

#if (defined LIN_VAR_NOINIT) /* to prevent double definition */
#error LIN_VAR_NOINIT already defined
#endif /* if (defined LIN_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define LIN_VAR_NOINIT

#if (defined LIN_VAR_POWER_ON_INIT) /* to prevent double definition */
#error LIN_VAR_POWER_ON_INIT already defined
#endif /* if (defined LIN_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define LIN_VAR_POWER_ON_INIT

#if (defined LIN_VAR_FAST) /* to prevent double definition */
#error LIN_VAR_FAST already defined
#endif /* if (defined LIN_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define LIN_VAR_FAST

#if (defined LIN_VAR) /* to prevent double definition */
#error LIN_VAR already defined
#endif /* if (defined LIN_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define LIN_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined LINIF_CODE) /* to prevent double definition */
#error LINIF_CODE already defined
#endif /* if (defined LINIF_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define LINIF_CODE

#if (defined LINIF_CONST) /* to prevent double definition */
#error LINIF_CONST already defined
#endif /* if (defined LINIF_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define LINIF_CONST

#if (defined LINIF_APPL_DATA) /* to prevent double definition */
#error LINIF_APPL_DATA already defined
#endif /* if (defined LINIF_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define LINIF_APPL_DATA

#if (defined LINIF_APPL_CONST) /* to prevent double definition */
#error LINIF_APPL_CONST already defined
#endif /* if (defined LINIF_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define LINIF_APPL_CONST

#if (defined LINIF_APPL_CODE) /* to prevent double definition */
#error LINIF_APPL_CODE already defined
#endif /* if (defined LINIF_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define LINIF_APPL_CODE

#if (defined LINIF_VAR_NOINIT) /* to prevent double definition */
#error LINIF_VAR_NOINIT already defined
#endif /* if (defined LINIF_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define LINIF_VAR_NOINIT

#if (defined LINIF_VAR_POWER_ON_INIT) /* to prevent double definition */
#error LINIF_VAR_POWER_ON_INIT already defined
#endif /* if (defined LINIF_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define LINIF_VAR_POWER_ON_INIT

#if (defined LINIF_VAR_FAST) /* to prevent double definition */
#error LINIF_VAR_FAST already defined
#endif /* if (defined LINIF_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define LINIF_VAR_FAST

#if (defined LINIF_VAR) /* to prevent double definition */
#error LINIF_VAR already defined
#endif /* if (defined LINIF_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define LINIF_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined MCU_CODE) /* to prevent double definition */
#error MCU_CODE already defined
#endif /* if (defined MCU_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define MCU_CODE

#if (defined MCU_CONST) /* to prevent double definition */
#error MCU_CONST already defined
#endif /* if (defined MCU_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define MCU_CONST

#if (defined MCU_APPL_DATA) /* to prevent double definition */
#error MCU_APPL_DATA already defined
#endif /* if (defined MCU_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define MCU_APPL_DATA

#if (defined MCU_APPL_CONST) /* to prevent double definition */
#error MCU_APPL_CONST already defined
#endif /* if (defined MCU_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define MCU_APPL_CONST

#if (defined MCU_APPL_CODE) /* to prevent double definition */
#error MCU_APPL_CODE already defined
#endif /* if (defined MCU_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define MCU_APPL_CODE

#if (defined MCU_VAR_NOINIT) /* to prevent double definition */
#error MCU_VAR_NOINIT already defined
#endif /* if (defined MCU_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define MCU_VAR_NOINIT

#if (defined MCU_VAR_POWER_ON_INIT) /* to prevent double definition */
#error MCU_VAR_POWER_ON_INIT already defined
#endif /* if (defined MCU_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define MCU_VAR_POWER_ON_INIT

#if (defined MCU_VAR_FAST) /* to prevent double definition */
#error MCU_VAR_FAST already defined
#endif /* if (defined MCU_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define MCU_VAR_FAST

#if (defined MCU_VAR) /* to prevent double definition */
#error MCU_VAR already defined
#endif /* if (defined MCU_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define MCU_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined MEMIF_CODE) /* to prevent double definition */
#error MEMIF_CODE already defined
#endif /* if (defined MEMIF_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define MEMIF_CODE

#if (defined MEMIF_CONST) /* to prevent double definition */
#error MEMIF_CONST already defined
#endif /* if (defined MEMIF_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define MEMIF_CONST

#if (defined MEMIF_APPL_DATA) /* to prevent double definition */
#error MEMIF_APPL_DATA already defined
#endif /* if (defined MEMIF_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define MEMIF_APPL_DATA

#if (defined MEMIF_APPL_CONST) /* to prevent double definition */
#error MEMIF_APPL_CONST already defined
#endif /* if (defined MEMIF_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define MEMIF_APPL_CONST

#if (defined MEMIF_APPL_CODE) /* to prevent double definition */
#error MEMIF_APPL_CODE already defined
#endif /* if (defined MEMIF_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define MEMIF_APPL_CODE

#if (defined MEMIF_VAR_NOINIT) /* to prevent double definition */
#error MEMIF_VAR_NOINIT already defined
#endif /* if (defined MEMIF_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define MEMIF_VAR_NOINIT

#if (defined MEMIF_VAR_POWER_ON_INIT) /* to prevent double definition */
#error MEMIF_VAR_POWER_ON_INIT already defined
#endif /* if (defined MEMIF_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define MEMIF_VAR_POWER_ON_INIT

#if (defined MEMIF_VAR_FAST) /* to prevent double definition */
#error MEMIF_VAR_FAST already defined
#endif /* if (defined MEMIF_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define MEMIF_VAR_FAST

#if (defined MEMIF_VAR) /* to prevent double definition */
#error MEMIF_VAR already defined
#endif /* if (defined MEMIF_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define MEMIF_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined NM_CODE) /* to prevent double definition */
#error NM_CODE already defined
#endif /* if (defined NM_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define NM_CODE

#if (defined NM_CONST) /* to prevent double definition */
#error NM_CONST already defined
#endif /* if (defined NM_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define NM_CONST

#if (defined NM_APPL_DATA) /* to prevent double definition */
#error NM_APPL_DATA already defined
#endif /* if (defined NM_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define NM_APPL_DATA

#if (defined NM_APPL_CONST) /* to prevent double definition */
#error NM_APPL_CONST already defined
#endif /* if (defined NM_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define NM_APPL_CONST

#if (defined NM_APPL_CODE) /* to prevent double definition */
#error NM_APPL_CODE already defined
#endif /* if (defined NM_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define NM_APPL_CODE

#if (defined NM_VAR_NOINIT) /* to prevent double definition */
#error NM_VAR_NOINIT already defined
#endif /* if (defined NM_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define NM_VAR_NOINIT

#if (defined NM_VAR_POWER_ON_INIT) /* to prevent double definition */
#error NM_VAR_POWER_ON_INIT already defined
#endif /* if (defined NM_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define NM_VAR_POWER_ON_INIT

#if (defined NM_VAR_FAST) /* to prevent double definition */
#error NM_VAR_FAST already defined
#endif /* if (defined NM_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define NM_VAR_FAST

#if (defined NM_VAR) /* to prevent double definition */
#error NM_VAR already defined
#endif /* if (defined NM_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define NM_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined NVM_CODE) /* to prevent double definition */
#error NVM_CODE already defined
#endif /* if (defined NVM_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define NVM_CODE

#if (defined NVM_CONST) /* to prevent double definition */
#error NVM_CONST already defined
#endif /* if (defined NVM_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define NVM_CONST

#if (defined NVM_APPL_DATA) /* to prevent double definition */
#error NVM_APPL_DATA already defined
#endif /* if (defined NVM_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define NVM_APPL_DATA

#if (defined NVM_APPL_CONST) /* to prevent double definition */
#error NVM_APPL_CONST already defined
#endif /* if (defined NVM_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define NVM_APPL_CONST

#if (defined NVM_APPL_CODE) /* to prevent double definition */
#error NVM_APPL_CODE already defined
#endif /* if (defined NVM_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define NVM_APPL_CODE

#if (defined NVM_VAR_NOINIT) /* to prevent double definition */
#error NVM_VAR_NOINIT already defined
#endif /* if (defined NVM_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define NVM_VAR_NOINIT

#if (defined NVM_VAR_POWER_ON_INIT) /* to prevent double definition */
#error NVM_VAR_POWER_ON_INIT already defined
#endif /* if (defined NVM_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define NVM_VAR_POWER_ON_INIT

#if (defined NVM_VAR_FAST) /* to prevent double definition */
#error NVM_VAR_FAST already defined
#endif /* if (defined NVM_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define NVM_VAR_FAST

#if (defined NVM_VAR) /* to prevent double definition */
#error NVM_VAR already defined
#endif /* if (defined NVM_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define NVM_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined OS_CODE) /* to prevent double definition */
#error OS_CODE already defined
#endif /* if (defined OS_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define OS_CODE

#if (defined OS_CONST) /* to prevent double definition */
#error OS_CONST already defined
#endif /* if (defined OS_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define OS_CONST

#if (defined OS_APPL_DATA) /* to prevent double definition */
#error OS_APPL_DATA already defined
#endif /* if (defined OS_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define OS_APPL_DATA

#if (defined OS_APPL_CONST) /* to prevent double definition */
#error OS_APPL_CONST already defined
#endif /* if (defined OS_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define OS_APPL_CONST

#if (defined OS_APPL_CODE) /* to prevent double definition */
#error OS_APPL_CODE already defined
#endif /* if (defined OS_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define OS_APPL_CODE

#if (defined OS_VAR_NOINIT) /* to prevent double definition */
#error OS_VAR_NOINIT already defined
#endif /* if (defined OS_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define OS_VAR_NOINIT

#if (defined OS_VAR_POWER_ON_INIT) /* to prevent double definition */
#error OS_VAR_POWER_ON_INIT already defined
#endif /* if (defined OS_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define OS_VAR_POWER_ON_INIT

#if (defined OS_VAR_FAST) /* to prevent double definition */
#error OS_VAR_FAST already defined
#endif /* if (defined OS_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define OS_VAR_FAST

#if (defined OS_VAR) /* to prevent double definition */
#error OS_VAR already defined
#endif /* if (defined OS_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define OS_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined PDUR_CODE) /* to prevent double definition */
#error PDUR_CODE already defined
#endif /* if (defined PDUR_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define PDUR_CODE

#if (defined PDUR_CONST) /* to prevent double definition */
#error PDUR_CONST already defined
#endif /* if (defined PDUR_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define PDUR_CONST

#if (defined PDUR_APPL_DATA) /* to prevent double definition */
#error PDUR_APPL_DATA already defined
#endif /* if (defined PDUR_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define PDUR_APPL_DATA

#if (defined PDUR_APPL_CONST) /* to prevent double definition */
#error PDUR_APPL_CONST already defined
#endif /* if (defined PDUR_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define PDUR_APPL_CONST

#if (defined PDUR_APPL_CODE) /* to prevent double definition */
#error PDUR_APPL_CODE already defined
#endif /* if (defined PDUR_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define PDUR_APPL_CODE

#if (defined PDUR_VAR_NOINIT) /* to prevent double definition */
#error PDUR_VAR_NOINIT already defined
#endif /* if (defined PDUR_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define PDUR_VAR_NOINIT

#if (defined PDUR_VAR_POWER_ON_INIT) /* to prevent double definition */
#error PDUR_VAR_POWER_ON_INIT already defined
#endif /* if (defined PDUR_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define PDUR_VAR_POWER_ON_INIT

#if (defined PDUR_VAR_FAST) /* to prevent double definition */
#error PDUR_VAR_FAST already defined
#endif /* if (defined PDUR_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define PDUR_VAR_FAST

#if (defined PDUR_VAR) /* to prevent double definition */
#error PDUR_VAR already defined
#endif /* if (defined PDUR_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define PDUR_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined PORT_CODE) /* to prevent double definition */
#error PORT_CODE already defined
#endif /* if (defined PORT_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define PORT_CODE

#if (defined PORT_CONST) /* to prevent double definition */
#error PORT_CONST already defined
#endif /* if (defined PORT_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define PORT_CONST

#if (defined PORT_APPL_DATA) /* to prevent double definition */
#error PORT_APPL_DATA already defined
#endif /* if (defined PORT_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define PORT_APPL_DATA

#if (defined PORT_APPL_CONST) /* to prevent double definition */
#error PORT_APPL_CONST already defined
#endif /* if (defined PORT_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define PORT_APPL_CONST

#if (defined PORT_APPL_CODE) /* to prevent double definition */
#error PORT_APPL_CODE already defined
#endif /* if (defined PORT_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define PORT_APPL_CODE

#if (defined PORT_VAR_NOINIT) /* to prevent double definition */
#error PORT_VAR_NOINIT already defined
#endif /* if (defined PORT_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define PORT_VAR_NOINIT

#if (defined PORT_VAR_POWER_ON_INIT) /* to prevent double definition */
#error PORT_VAR_POWER_ON_INIT already defined
#endif /* if (defined PORT_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define PORT_VAR_POWER_ON_INIT

#if (defined PORT_VAR_FAST) /* to prevent double definition */
#error PORT_VAR_FAST already defined
#endif /* if (defined PORT_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define PORT_VAR_FAST

#if (defined PORT_VAR) /* to prevent double definition */
#error PORT_VAR already defined
#endif /* if (defined PORT_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define PORT_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined PWM_CODE) /* to prevent double definition */
#error PWM_CODE already defined
#endif /* if (defined PWM_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define PWM_CODE

#if (defined PWM_CONST) /* to prevent double definition */
#error PWM_CONST already defined
#endif /* if (defined PWM_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define PWM_CONST

#if (defined PWM_APPL_DATA) /* to prevent double definition */
#error PWM_APPL_DATA already defined
#endif /* if (defined PWM_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define PWM_APPL_DATA

#if (defined PWM_APPL_CONST) /* to prevent double definition */
#error PWM_APPL_CONST already defined
#endif /* if (defined PWM_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define PWM_APPL_CONST

#if (defined PWM_APPL_CODE) /* to prevent double definition */
#error PWM_APPL_CODE already defined
#endif /* if (defined PWM_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define PWM_APPL_CODE

#if (defined PWM_VAR_NOINIT) /* to prevent double definition */
#error PWM_VAR_NOINIT already defined
#endif /* if (defined PWM_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define PWM_VAR_NOINIT

#if (defined PWM_VAR_POWER_ON_INIT) /* to prevent double definition */
#error PWM_VAR_POWER_ON_INIT already defined
#endif /* if (defined PWM_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define PWM_VAR_POWER_ON_INIT

#if (defined PWM_VAR_FAST) /* to prevent double definition */
#error PWM_VAR_FAST already defined
#endif /* if (defined PWM_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define PWM_VAR_FAST

#if (defined PWM_VAR) /* to prevent double definition */
#error PWM_VAR already defined
#endif /* if (defined PWM_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define PWM_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined RAMTST_CODE) /* to prevent double definition */
#error RAMTST_CODE already defined
#endif /* if (defined RAMTST_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define RAMTST_CODE

#if (defined RAMTST_CONST) /* to prevent double definition */
#error RAMTST_CONST already defined
#endif /* if (defined RAMTST_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define RAMTST_CONST

#if (defined RAMTST_APPL_DATA) /* to prevent double definition */
#error RAMTST_APPL_DATA already defined
#endif /* if (defined RAMTST_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define RAMTST_APPL_DATA

#if (defined RAMTST_APPL_CONST) /* to prevent double definition */
#error RAMTST_APPL_CONST already defined
#endif /* if (defined RAMTST_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define RAMTST_APPL_CONST

#if (defined RAMTST_APPL_CODE) /* to prevent double definition */
#error RAMTST_APPL_CODE already defined
#endif /* if (defined RAMTST_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define RAMTST_APPL_CODE

#if (defined RAMTST_VAR_NOINIT) /* to prevent double definition */
#error RAMTST_VAR_NOINIT already defined
#endif /* if (defined RAMTST_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define RAMTST_VAR_NOINIT

#if (defined RAMTST_VAR_POWER_ON_INIT) /* to prevent double definition */
#error RAMTST_VAR_POWER_ON_INIT already defined
#endif /* if (defined RAMTST_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define RAMTST_VAR_POWER_ON_INIT

#if (defined RAMTST_VAR_FAST) /* to prevent double definition */
#error RAMTST_VAR_FAST already defined
#endif /* if (defined RAMTST_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define RAMTST_VAR_FAST

#if (defined RAMTST_VAR) /* to prevent double definition */
#error RAMTST_VAR already defined
#endif /* if (defined RAMTST_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define RAMTST_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined RTE_CODE) /* to prevent double definition */
#error RTE_CODE already defined
#endif /* if (defined RTE_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define RTE_CODE

#if (defined RTE_CONST) /* to prevent double definition */
#error RTE_CONST already defined
#endif /* if (defined RTE_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define RTE_CONST

#if (defined RTE_APPL_DATA) /* to prevent double definition */
#error RTE_APPL_DATA already defined
#endif /* if (defined RTE_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define RTE_APPL_DATA

#if (defined RTE_APPL_CONST) /* to prevent double definition */
#error RTE_APPL_CONST already defined
#endif /* if (defined RTE_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define RTE_APPL_CONST

#if (defined RTE_APPL_CODE) /* to prevent double definition */
#error RTE_APPL_CODE already defined
#endif /* if (defined RTE_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define RTE_APPL_CODE

#if (defined RTE_VAR_NOINIT) /* to prevent double definition */
#error RTE_VAR_NOINIT already defined
#endif /* if (defined RTE_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define RTE_VAR_NOINIT

#if (defined RTE_VAR_POWER_ON_INIT) /* to prevent double definition */
#error RTE_VAR_POWER_ON_INIT already defined
#endif /* if (defined RTE_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define RTE_VAR_POWER_ON_INIT

#if (defined RTE_VAR_FAST) /* to prevent double definition */
#error RTE_VAR_FAST already defined
#endif /* if (defined RTE_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define RTE_VAR_FAST

#if (defined RTE_VAR) /* to prevent double definition */
#error RTE_VAR already defined
#endif /* if (defined RTE_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define RTE_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined SPI_CODE) /* to prevent double definition */
#error SPI_CODE already defined
#endif /* if (defined SPI_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define SPI_CODE

#if (defined SPI_CONST) /* to prevent double definition */
#error SPI_CONST already defined
#endif /* if (defined SPI_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define SPI_CONST

#if (defined SPI_APPL_DATA) /* to prevent double definition */
#error SPI_APPL_DATA already defined
#endif /* if (defined SPI_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define SPI_APPL_DATA

#if (defined SPI_APPL_CONST) /* to prevent double definition */
#error SPI_APPL_CONST already defined
#endif /* if (defined SPI_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define SPI_APPL_CONST

#if (defined SPI_APPL_CODE) /* to prevent double definition */
#error SPI_APPL_CODE already defined
#endif /* if (defined SPI_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define SPI_APPL_CODE

#if (defined SPI_VAR_NOINIT) /* to prevent double definition */
#error SPI_VAR_NOINIT already defined
#endif /* if (defined SPI_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define SPI_VAR_NOINIT

#if (defined SPI_VAR_POWER_ON_INIT) /* to prevent double definition */
#error SPI_VAR_POWER_ON_INIT already defined
#endif /* if (defined SPI_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define SPI_VAR_POWER_ON_INIT

#if (defined SPI_VAR_FAST) /* to prevent double definition */
#error SPI_VAR_FAST already defined
#endif /* if (defined SPI_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define SPI_VAR_FAST

#if (defined SPI_VAR) /* to prevent double definition */
#error SPI_VAR already defined
#endif /* if (defined SPI_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define SPI_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined WDG_CODE) /* to prevent double definition */
#error WDG_CODE already defined
#endif /* if (defined WDG_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define WDG_CODE

#if (defined WDG_CONST) /* to prevent double definition */
#error WDG_CONST already defined
#endif /* if (defined WDG_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define WDG_CONST

#if (defined WDG_APPL_DATA) /* to prevent double definition */
#error WDG_APPL_DATA already defined
#endif /* if (defined WDG_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define WDG_APPL_DATA

#if (defined WDG_APPL_CONST) /* to prevent double definition */
#error WDG_APPL_CONST already defined
#endif /* if (defined WDG_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define WDG_APPL_CONST

#if (defined WDG_APPL_CODE) /* to prevent double definition */
#error WDG_APPL_CODE already defined
#endif /* if (defined WDG_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define WDG_APPL_CODE

#if (defined WDG_VAR_NOINIT) /* to prevent double definition */
#error WDG_VAR_NOINIT already defined
#endif /* if (defined WDG_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define WDG_VAR_NOINIT

#if (defined WDG_VAR_POWER_ON_INIT) /* to prevent double definition */
#error WDG_VAR_POWER_ON_INIT already defined
#endif /* if (defined WDG_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define WDG_VAR_POWER_ON_INIT

#if (defined WDG_VAR_FAST) /* to prevent double definition */
#error WDG_VAR_FAST already defined
#endif /* if (defined WDG_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define WDG_VAR_FAST

#if (defined WDG_VAR) /* to prevent double definition */
#error WDG_VAR already defined
#endif /* if (defined WDG_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define WDG_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined WDGIF_CODE) /* to prevent double definition */
#error WDGIF_CODE already defined
#endif /* if (defined WDGIF_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define WDGIF_CODE

#if (defined WDGIF_CONST) /* to prevent double definition */
#error WDGIF_CONST already defined
#endif /* if (defined WDGIF_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define WDGIF_CONST

#if (defined WDGIF_APPL_DATA) /* to prevent double definition */
#error WDGIF_APPL_DATA already defined
#endif /* if (defined WDGIF_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define WDGIF_APPL_DATA

#if (defined WDGIF_APPL_CONST) /* to prevent double definition */
#error WDGIF_APPL_CONST already defined
#endif /* if (defined WDGIF_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define WDGIF_APPL_CONST

#if (defined WDGIF_APPL_CODE) /* to prevent double definition */
#error WDGIF_APPL_CODE already defined
#endif /* if (defined WDGIF_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define WDGIF_APPL_CODE

#if (defined WDGIF_VAR_NOINIT) /* to prevent double definition */
#error WDGIF_VAR_NOINIT already defined
#endif /* if (defined WDGIF_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define WDGIF_VAR_NOINIT

#if (defined WDGIF_VAR_POWER_ON_INIT) /* to prevent double definition */
#error WDGIF_VAR_POWER_ON_INIT already defined
#endif /* if (defined WDGIF_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define WDGIF_VAR_POWER_ON_INIT

#if (defined WDGIF_VAR_FAST) /* to prevent double definition */
#error WDGIF_VAR_FAST already defined
#endif /* if (defined WDGIF_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define WDGIF_VAR_FAST

#if (defined WDGIF_VAR) /* to prevent double definition */
#error WDGIF_VAR already defined
#endif /* if (defined WDGIF_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define WDGIF_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined WDGM_CODE) /* to prevent double definition */
#error WDGM_CODE already defined
#endif /* if (defined WDGM_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define WDGM_CODE

#if (defined WDGM_CONST) /* to prevent double definition */
#error WDGM_CONST already defined
#endif /* if (defined WDGM_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define WDGM_CONST

#if (defined WDGM_APPL_DATA) /* to prevent double definition */
#error WDGM_APPL_DATA already defined
#endif /* if (defined WDGM_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define WDGM_APPL_DATA

#if (defined WDGM_APPL_CONST) /* to prevent double definition */
#error WDGM_APPL_CONST already defined
#endif /* if (defined WDGM_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define WDGM_APPL_CONST

#if (defined WDGM_APPL_CODE) /* to prevent double definition */
#error WDGM_APPL_CODE already defined
#endif /* if (defined WDGM_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define WDGM_APPL_CODE

#if (defined WDGM_VAR_NOINIT) /* to prevent double definition */
#error WDGM_VAR_NOINIT already defined
#endif /* if (defined WDGM_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define WDGM_VAR_NOINIT

#if (defined WDGM_VAR_POWER_ON_INIT) /* to prevent double definition */
#error WDGM_VAR_POWER_ON_INIT already defined
#endif /* if (defined WDGM_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define WDGM_VAR_POWER_ON_INIT

#if (defined WDGM_VAR_FAST) /* to prevent double definition */
#error WDGM_VAR_FAST already defined
#endif /* if (defined WDGM_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define WDGM_VAR_FAST

#if (defined WDGM_VAR) /* to prevent double definition */
#error WDGM_VAR already defined
#endif /* if (defined WDGM_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define WDGM_VAR

/*------------------[memory and pointer class of a module]-------------------*/

#if (defined XCP_CODE) /* to prevent double definition */
#error XCP_CODE already defined
#endif /* if (defined XCP_CODE) */

/** \brief definition of the code memory class
 **
 ** To be used for code. */
#define XCP_CODE

#if (defined XCP_CONST) /* to prevent double definition */
#error XCP_CONST already defined
#endif /* if (defined XCP_CONST) */

/** \brief definition of the constant memory class
 **
 ** To be used for global or static constants. */
#define XCP_CONST

#if (defined XCP_APPL_DATA) /* to prevent double definition */
#error XCP_APPL_DATA already defined
#endif /* if (defined XCP_APPL_DATA) */

/** \brief definition of the application data pointer class
 **
 ** To be used for references on application data (expected to
 ** be in RAM or ROM) passed via API. */
#define XCP_APPL_DATA

#if (defined XCP_APPL_CONST) /* to prevent double definition */
#error XCP_APPL_CONST already defined
#endif /* if (defined XCP_APPL_CONST) */

/** \brief definition of the constant pointer class
 **
 ** To be used for references on application constants (expected to
 ** be certainly in ROM, for instance pointer of Init() function)
 ** passed via API. */
#define XCP_APPL_CONST

#if (defined XCP_APPL_CODE) /* to prevent double definition */
#error XCP_APPL_CODE already defined
#endif /* if (defined XCP_APPL_CODE) */

/** \brief definition of a code pointer class
 **
 ** To be used for references on application functions
 ** (e.g. call back function pointers). */
#define XCP_APPL_CODE

#if (defined XCP_VAR_NOINIT) /* to prevent double definition */
#error XCP_VAR_NOINIT already defined
#endif /* if (defined XCP_VAR_NOINIT) */

/** \brief definition of the noinit variable memory class
 **
 ** To be used for all global or static variables that are
 ** never initialized. */
#define XCP_VAR_NOINIT

#if (defined XCP_VAR_POWER_ON_INIT) /* to prevent double definition */
#error XCP_VAR_POWER_ON_INIT already defined
#endif /* if (defined XCP_VAR_POWER_ON_INIT) */

/** \brief definition of a power on init variable memory class
 **
 ** To be used for all global or static variables that are initialized
 ** only after power on reset. */
#define XCP_VAR_POWER_ON_INIT

#if (defined XCP_VAR_FAST) /* to prevent double definition */
#error XCP_VAR_FAST already defined
#endif /* if (defined XCP_VAR_FAST) */

/** \brief definition of a fast bariable memory class
 **
 ** To be used for all global or static variables that have at least one
 ** of the following properties:
 ** - accessed bitwise
 ** - frequently used
 ** - high number of accesses in source code */
#define XCP_VAR_FAST

#if (defined XCP_VAR) /* to prevent double definition */
#error XCP_VAR already defined
#endif /* if (defined XCP_VAR) */

/** \brief definition of a variable memory class
 **
 ** To be used for global or static variables that are initialized
 ** after every reset. */
#define XCP_VAR

/*==================[type definitions]=======================================*/

/*==================[external function declarations]=========================*/

/*==================[internal function declarations]=========================*/

/*==================[external constants]=====================================*/

/*==================[internal constants]=====================================*/

/*==================[external data]==========================================*/

/*==================[internal data]==========================================*/

/*==================[external function definitions]==========================*/

/*==================[internal function definitions]==========================*/

/** @} doxygen end group definition */
#endif /* if !defined( COMPILER_CFG_H ) */
/*==================[end of file]============================================*/
