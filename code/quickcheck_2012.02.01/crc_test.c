//#include "stdio.h"
#include "Crc.h"

FUNC(uint8, CRC_CODE) Crc_CalculateCRC8
(
   P2CONST(uint8, AUTOMATIC, CRC_APPL_CONST)    Crc_DataPtr,
   VAR(uint32, AUTOMATIC)                   Crc_Length,
   VAR(uint8, AUTOMATIC)                   Crc_StartValue8,
   VAR(boolean,AUTOMATIC)                  Crc_IsFirstCall

);
/*
int main(void)
{
   uint8 str[] = "123456789";
   uint8 crcval = Crc_CalculateCRC8(str, 9, 0xFF, 1);
   printf("%d\n", crcval);
   return 0;
}
*/
