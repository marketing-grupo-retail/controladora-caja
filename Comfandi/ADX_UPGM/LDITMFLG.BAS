!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LDITMFLG.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Activacion user Option 3 para    *
!                registro producto escalonado     *
!**************************************************
! Modificaciones:
!

%Environ C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$, Xfile$, XAlm$, IR.USERDATA$, Xcon$
Integer*1  Global U.Error%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty%
%INCLUDE EAMITEMR.J86
%INCLUDE BASROUT.J86

Sub MSG.LOG
    Locate 23,1 : Print String$(78," ") 
    Locate 23,1 : Print "Msg: "+UE.MSG$
End Sub 

Function TRADUCE.ERROR                                       !
Integer*4 HX%, S%, SX%, SUM%
String    Z$
    HX% = ERRN                                               ! Traduccion de los
    ERRFX$=""                                                ! codigos de error
    FOR S% = 28 TO 0 STEP -4                                 ! del sistema operativo
        SX% = SHIFT(HX%,S%)                                  !
        SUM% = SX% AND 000FH                                 !
        IF SUM% > 9 THEN SUM% = SUM% + 55                   \!
        ELSE SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                      !
        ERRFX$ = ERRFX$ + Z$                                 !
    NEXT S%                                                  !
END Function


Sub Apertura.Archivos
    Open "EAMITEMR" KEYED RECL  77 AS 4				  					   	! Apertura Maestro de articulos
    Create "ADX_UDT1:LDITMFLG.ERR" AS 2                      	! Errores proceso de carga
End Sub

Sub Carga.Masiva.Datos
String U.buffer$, U.Iva$, U.Dsct$
String X.Itm$, X.Ctrl$, X.Status$, X.Und$
  While(1)																																	! Barre archivo de novedades
      U.Error% = -1																													! Control de Error
      Read #1; X.Itm$ 																											! Lee archivo
      If U.Error% = 0 Then Exit Sub 																				! Si Error de lectura, termina proceso
      U.Qty% = U.Qty% + 1																										! Numero de registros leidos
      Locate 12,27 : Print Str$(U.Qty%)																			!
      U.Error% = -1 																												! Control de errores
      IR.ITEMCODE$ = Pack$(Right$("000000000000"+X.ITM$,12))								! Llave de busqueda
       %INCLUDE EAMIRRD1.J86																								! Lectura Eamitemr
       If U.Error% = -1 Then Begin																					! Lo encontro
         U.QtyOk% = U.QtyOk% + 1 																						!
         Locate 13,27 : Print Str$(U.QtyOk%)																!
         IR.INDICAT1A = IR.INDICAT1A Or 02H   															! Activa Flag
         %INCLUDE EAMIRWR1.J86																							! Almacena registro
       EndIf Else Begin																											! No encontro Item
         U.QtyNok% = U.QtyNok% + 1																					! Reporta Error
         Locate 14,27 : Print Str$(U.QtyNok%)																! de actualizacion
         Write #2; Unpack$(IR.ITEMCODE$),UE.MSG$,FINR$											! y deja rastro
       EndIf    																														!
  Wend																																			!
  
End Sub

Sub Fin.Proceso
    Close 4 : Delete 1: Close 2
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****     ACTUALIZACION MASIVA ARTICULOS ESCALONADOS  ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Diciembre 06 2011  G.R. ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : "+Xfile$
   Locate 9, 1: Print "Actualizando Archivo   : EAMITEMR"
   Locate 12,1: Print "Registros Procesados   : "
   Locate 13,1: Print "Registros Actualizados : " 
   Locate 14,1: Print "Registros No Procesados: " 
   Locate 23,1: Print "Msg : "
   FINR$ = CHR$(13) + CHR$(10)   ! Marca de fin de registro
End Sub


!---
!------- Bloque Principal
!---

On Error GoTo Errores.Aplicativo																					! Control de proceso de errores
Xfile$ = "C:\DATOS\ITMESCAL.TXT"           																! Archivo de carga
Call Pantalla.Principal																									  ! Pantalla Principal
Open Xfile$ AS 1								       																		! Plano maestro de carga
Call Apertura.Archivos																										! Apertura de archivos
Call Carga.Masiva.Datos																										! Carga Masiva de Informacion
UE.MSG$ = "Fin Carga Masiva Notas, Verifique Errores en ADX_UDT1:LDITMFLG.ERR"
Call Msg.Log
Stop 

Errores.Aplicativo:
  U.Error% = 0
  CALL TRADUCE.ERROR
  UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  
  If ERR = "IH" Then Resume 
  
  If ERRF% = 1 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO "+Xfile$+" NO EXISTE... "
     Call Msg.Log
     Stop 
  EndIf 
  If ERRF% = 4 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 

  If ERRF% = 4 AND ERR = "EF" THEN Begin
     Resume 
  EndIf 

  If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO MAESTRO DE ARTICULOS NO EXISTE... "
     Call Msg.Log
     Stop
  EndIf 
  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
     Resume 
  EndIf 
  
  CALL MSG.LOG
  STOP

