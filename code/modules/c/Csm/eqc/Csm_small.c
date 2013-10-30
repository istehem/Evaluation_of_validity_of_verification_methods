#define FUNC(rettype, memclass) rettype
#define P2CONST(ptrtype, memclass, ptrclass) const ptrtype *
#define P2FUNC(rettype, ptrclass, fctname) rettype (*fctname)
#define VAR(type, memclass) type
#define P2VAR(ptrtype, memclass, ptrclass) /* PRQA S 3409 */ ptrtype *
#define CONST(type, memclass) /* PRQA S 3409 */ const type
#define MAX_NO_HASH_CONFIGS   2
#define E_OK     0x00

typedef unsigned char uint8;
typedef unsigned int uint32;
typedef unsigned char boolean;

typedef unsigned char Std_ReturnType;

Std_ReturnType Det_ReportError(unsigned short int, unsigned char, unsigned char, unsigned char);

Std_ReturnType Det_ReportError(unsigned short int a, unsigned char b, unsigned char c, unsigned char d)
{
  return E_OK;
}
//extern FUNC (void, CSM_CODE) Csm_HashServiceFinishNotification (void);


typedef enum
{
   CSM_E_OK = 0,
   CSM_E_NOT_OK,
   CSM_E_BUSY,
   CSM_E_SMALL_BUFFER,
   CSM_E_ENTROPY_EXHAUSTION
} Csm_ReturnType;

typedef P2FUNC (Std_ReturnType, CSM_APPL_CODE, Csm_ServiceFinishNotificationType)(char retVal);
typedef P2FUNC (Csm_ReturnType, CSM_APPL_CODE, Csm_ServiceStartFctType)(const void *cfgPtr);

typedef short int Csm_ConfigIdType;

typedef P2FUNC (Csm_ReturnType, CSM_APPL_CODE, Hash_PrimitiveStartFctType)(P2CONST(void,CSM_CONST,CSM_APPL_CONST) cfgPtr);
typedef P2FUNC (Csm_ReturnType, CSM_APPL_CODE, Hash_PrimitiveUpdateFctType)(P2CONST(void, CSM_CONST, CSM_APPL_CONST)cfgPtr,P2CONST(unsigned char,CSM_CONST,CSM_APPL_CONST) dataPtr ,
                                                                            VAR(unsigned int, CSM_VAR)dataLength);
typedef P2FUNC (Csm_ReturnType, CSM_APPL_CODE, Hash_PrimitiveFinishFctType)(P2CONST(void, CSM_CONST, CSM_APPL_CONST)cfgPtr,P2VAR(unsigned char,CSM_VAR,CSM_APPL_DATA) ResultPtr,P2VAR (unsigned int, CSM_VAR, CSM_APPL_DATA) resultLengthPtr);
typedef P2FUNC (Csm_ReturnType, CSM_APPL_CODE, Hash_ServiceFinishFctType)(P2CONST(void, CSM_CONST, CSM_APPL_CONST)cfgPtr,P2VAR(unsigned char,CSM_VAR,CSM_APPL_DATA) ResultPtr,
                                                                          P2VAR (unsigned int, CSM_VAR, CSM_APPL_DATA) resultLengthPtr,
                                                                          VAR(unsigned char, CSM_VAR) TruncationIsAllowed);

typedef P2FUNC (Csm_ReturnType, CSM_APPL_CODE, Csm_ServiceUpdateFctType)(const void *cfgPtr, const unsigned char * dataPtr, VAR (unsigned int, CSM_VAR)dataLength);

typedef P2FUNC (void, CSM_APPL_CODE, Csm_ServiceMainFctType)(void);

typedef struct
{
   Hash_PrimitiveStartFctType Primitive_HashStartFct;
   Hash_PrimitiveUpdateFctType Primitive_HashUpdateFct;
   Hash_PrimitiveFinishFctType Primitive_HashFinishFct;
 }HashConfigType;


typedef struct Csm_HashConfigType_Tag
{
   Csm_ConfigIdType ConfigId;
   Csm_ServiceStartFctType HashStartFct;
   Csm_ServiceUpdateFctType HashUpdateFct;
   Hash_ServiceFinishFctType HashFinishFct;
   /*Csm_ServiceFctType HashFct;*/
   Csm_ServiceMainFctType HashMainFct;
   P2CONST (HashConfigType, CSM_VAR, CSM_APPL_CONST) HashConfig;
}Csm_HashConfigType;

typedef struct CsmHashConfig_Tag
{
   Csm_ServiceFinishNotificationType CsmCallbackHash;
   P2CONST (Csm_HashConfigType, CSM_VAR, CSM_APPL_CONST) CsmHashInitConfiguration;
}Csm_HashConfig;

//Original defination
//extern CONST(Csm_HashConfig, CSM_CONST) CsmHashConfig[MAX_NO_HASH_CONFIGS];
CONST(Csm_HashConfig, CSM_CONST) CsmHashConfig[MAX_NO_HASH_CONFIGS];


//void Csm_MainFunction();
//void Csm_Interruption();

typedef enum Driver_Status_Tag
{
   CSM_NOT_INITIALIZED,
   CSM_INITIALIZED
} Csm_StatusType;
typedef enum Service_Type_Tag
{
   HASH,
   MAC_GENERATE,
   MAC_VERIFY,
   RANDOM_SEED,
   RANDOM_GENERATE,
   SYM_BLOCK_ENCRYPT,
   SYM_BLOCK_DECRYPT,
   SYM_ENCRYPT,
   SYM_DECRYPT,
   ASYM_ENCRYPT,
   ASYM_DECRYPT,
   SIGNATURE_GENERATE,
   SIGNATURE_VERIFY,
   CHECKSUM,
   KEY_DERIVE,
   KEY_DERIVE_SYM_KEY,
   KEY_EXCHANGE_CALC_PUBVAL,
   KEY_EXCHG_CALC_SECRET,
   KEY_EXCHG_CALC_SYM_KEY,
   SYM_KEY_EXTRACT,
   SYM_KEY_WRAP_SYM,
   SYM_KEY_WRAP_ASYM,
   ASYM_PUBLIC_KEY_EXTRACT,
   ASYM_PRIVATE_KEY_EXTRACT,
   ASYM_PRIVATE_KEY_WRAP_SYM,
   ASYM_PRIVATE_KEY_WRAP_ASYM,
   UNUSED /* This has been added to remove warning for the check (ASYM_PRIVATE_KEY_WRAP_ASYM+1) */
} Csm_ServiceType;

Csm_ServiceType Csm_Service;
Csm_StatusType Csm_Init_Status;

/*
FUNC(Csm_ReturnType,CSM_CODE)SHAHashStart1 (P2CONST(void,CSM_CONST,CSM_APPL_CONST) cfgPtr);
FUNC(Csm_ReturnType,CSM_CODE)SHAHashUpdate1(P2CONST(void,CSM_CONST,CSM_APPL_CONST) cfgPtr,
                                                    P2CONST(uint8,CSM_CONST,CSM_APPL_CONST) dataPtr,
                                                    VAR(uint32, CSM_VAR)dataLength);
FUNC(Csm_ReturnType,CSM_CODE) SHAHashFinish1(P2CONST(void,CSM_CONST,CSM_APPL_CONST) cfgPtr,
                                                    P2VAR(uint8,CSM_VAR,CSM_APPL_DATA) ResultPtr,
                                                    P2VAR (uint32, CSM_VAR, CSM_APPL_DATA) resultLengthPtr);

static CONST (HashConfigType,CSM_CONST) Hash_Prim1_configPtr =
{
   &SHAHashStart1,
   &SHAHashUpdate1,
   &SHAHashFinish1
};
*/
void Csm_Init();


Csm_ReturnType Csm_Hashstart(Csm_ConfigIdType);
Csm_ReturnType Csm_HashUpdate(
   Csm_ConfigIdType cfgId,
   const uint8* dataPtr,
   uint32 dataLength
   );
Csm_ReturnType Csm_HashFinish(
   Csm_ConfigIdType cfgId,
   uint8* resultPtr,
   uint32* resultLengthPtr,
   boolean TruncationIsAllowed
);

//eqc_c:start(csm,[{c_src, "eqc/Csm_small.c"},{additional_files, ["out/Csm.o","out/Csm_Hash.o"]}]) .

