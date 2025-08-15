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
   Locate 3,12: Print "**** SIMULADOR CARGA DE PROMOCIONES MODULO POS V2   ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Enero 30   2.019   G.R. ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+String$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
End Sub 

On Error GoTo Errores
Open "TF:LSTCAR" KEYED RECL  13  AS 34   						 				 ! Apertura lista items promociones
Call Pantalla.Principal
Locate 7, 4 : Print "Archivo de Carga : "
Locate 7,30 : Input Promo$

If Len(Promo$) <> 12 Then Begin
   Print "Nombre de arhivo invalido "
EndIf Else Begin
	 Write Form "C12 C1";#34; Promo$, "C" 
   Print "Codigo Promocion Cargado"
EndIf
Input "Presione Enter Para Continuar....";Promo$ 
Close 34
Stop

Errores:

IF ERRF% = 34 AND                                    \! Validacion si existe 
   (ERR = "OE" OR ERR = "FU") THEN BEGIN              ! el archivo de control
   Locate 24,1 : Print "Error Apertura Archivo Control Promociones"
   Stop 
ENDIF                                                 ! del programa

If ERRF% = 34 And ERR = "EF"  THEN BEGIN              ! el archivo parametros TEF 
   TS.ERROR% = 0  																	  ! Reporta Error
   Resume 																						! Regresa control programa
EndIf 

Print "Error no definido"
Stop 