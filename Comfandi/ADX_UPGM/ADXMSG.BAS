!************************************************** 
!Empresa       : Casa Informatica Unidad  Retail  *
!Programa      : ADXMSG.BAS                       *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Enero 12 del 2000                *
!Observaciones : Adicion de mensajes ADXCSOZF.DAT *
!						  *
!**************************************************
!
!

STRING A$,B$,C$,MSG$
INTEGER I%

ON ERROR GOTO E0101
OPEN "ADX_IPGM:ADXCSOZF.DAT" AS 1 NOREAD NODEL APPEND
 
 

 A$ = "X001 "
!        1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456
 MSG$ = "EVENTO MODULO MERCADEO                  "
 MSG$ = LEFT$(MSG$+STRING$(106," "),106)
 WRITE FORM "C5,C106"; #1; A$,MSG$


 Close 1
 STOP

 
E0101:
         IF ERRF% = 1 AND                                \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            PRINT "ERROR DE APERTURA ADXCSOZF.DAT"
            STOP
         ENDIF
