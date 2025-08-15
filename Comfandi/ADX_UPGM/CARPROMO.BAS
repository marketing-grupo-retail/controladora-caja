!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : FINPROMO.011                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Finalizacion de una promocion    *
!**************************************************
!*

Integer*4 Global TS.ERROR%
String    Global A$, B$, C$, D$, PROMO$, Ffin$, Fini$

Sub PANTALLA.PRINCIPAL
   CLEARS
   Locate 2, 4: Print CHR$(218)+String$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "**** SIMULADOR CARGA DE PROMOCIONES MODULO POS      ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Mayo 12 de 2.010   G.R. ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+String$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
End Sub 

On Error GoTo Errores
Open "MKTP:PRMADMON.DAT" KEYED RECL  9 AS 33 ! Apertura Control de promociones
Call Pantalla.Principal
Locate 7, 4 : Print "Codigo de la Promocion: "
Locate 7,30 : Input Promo$

Promo$ = Pack$(Right$("0000"+promo$,4))
TS.ERROR% = -1 
Read Form "C2 2C3 C1";#33 KEY Promo$;                   \! Lee registro
  A$, B$, C$, D$
If TS.ERROR% = -1 Then Begin 
 Ffin$  = pack$(Right$("000000"+DATE$,6))
 Write Form "C2 2C3 C1";#33; promo$, B$, Ffin$, "1"
 Locate 24,1 : Print "Codigo de Promocion Ya Existe"
 Input "Presione Enter Para Continuar....";Promo$
EndIf Else Begin 
 Input "Fecha Inicial aammdd : ";Fini$
 Input "Fecha Final   aammdd : ";Ffin$
 Fini$  = pack$(Right$("000000"+Fini$,6))
 Ffin$  = pack$(Right$("000000"+Ffin$,6))
 Write Form "C2 2C3 C1";#33; promo$, Fini$, Ffin$, "0"
 Print "Promocion Publicada" 
 Input "Presione Enter Para Continuar....";Promo$
EndIf 
Close 33
Stop


Errores:

IF ERRF% = 33 AND                                    \! Validacion si existe 
   (ERR = "OE" OR ERR = "FU") THEN BEGIN              ! el archivo de control
   Locate 24,1 : Print "Error Apertura Archivo Control Promociones"
   Stop 
ENDIF                                                 ! del programa

If ERRF% = 33 And ERR = "EF"  THEN BEGIN              ! el archivo parametros TEF 
   TS.ERROR% = 0  																	  ! Reporta Error
   Resume 																						! Regresa control programa
EndIf 

Print "Error no definido"
Stop 