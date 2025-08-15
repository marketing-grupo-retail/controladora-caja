!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LDECFOSF.BAS                     *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Alimentacion masiva estructuras  *
!                comerciales excludidas Fosfec    *
!**************************************************
! Modificaciones:
! Ver 1.0 3/Abr/2019
! Mod 26Nov2019
! Se modifica carga de excluidos, se cargan ahora 
! los plu que no son autorizados, se crea una 
! estructura para 50.000 registros
! solicitado por Comfandi.
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%Environ C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$
Integer*1  Global U.Error%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty%

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
    Open "ADX_UDT1:PFOSFECF.TXT"   AS 1								       								! Plano maestro de carga
    Open "TF:ECFOSF" KEYED RECL  8 AS 4				  					   								! Apertura Articulos controlados
    Create "ADX_UDT1:ERRFOSFE.TXT" As 2                      								! Errores proceso de carga
End Sub

Sub Carga.Masiva.Datos																											! Carga masiva plano
String U.buffer$, U.Iva$, U.Dsct$
String X.Sku$, X.Ean$, X.Status$, XReg$, Xprov$
  While(1)																																	! Recorre archivo plano 
      Read #1; XReg$																												! Lectura registro 
      U.Qty% = U.Qty% + 1																										! nro de registros leidos
      Locate 12,27 : Print Str$(U.Qty%)																			!
      U.Error% = -1 																												!
      X.Sku$   = Mid$(Xreg$,1,15)    																				! Estructura comercial excluida
      X.Status$ = Right$(Xreg$,1)    																				! Acciona a realizar 1. Add/Mod 3.Erase
      X.Sku$   = Str$(Val(X.Sku$))																					! Ajusta dato
      X.Sku$   = Pack$("1" + Right$("000000000000000"+X.Sku$,15))						! Dato para grabacion / borrado 
      If Val(X.Status$) <> 3 Then Begin 																		! Adicion de registro
         Write Form "C8" ; #4;                                             \! Graba el registro en archivo de Fosfec
           X.Sku$                                                          \! Estructura comercial excluida PD 8
      EndIf																																	!
      If Val(X.Status$) = 3 Then Begin 																			! Elimina registro
     	   DelRec 4 ; X.Sku$																									!
      EndIf 																																!
       If U.Error% = -1 Then Begin
         U.QtyOk% = U.QtyOk% + 1 
         Locate 13,27 : Print Str$(U.QtyOk%)
       EndIf Else Begin
         U.QtyNok% = U.QtyNok% + 1
         Locate 14,27 : Print Str$(U.QtyNok%)
         Write #2; "Linea :"+Str$(U.Qty%),Unpack$(X.Sku$),UE.MSG$,FINR$
       EndIf    
  Wend
End Sub

Sub Fin.Proceso
    UE.MSG$ = "Fin Carga Masiva Fosfec, Verifique Errores en ADX_UDT1:ERRFOSF.TXT"
    Call Msg.Log
    Close 4 : Delete 1: Close 2
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****   CARGA MASIVA ESTRUCTURAS FOSFEC     ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Nov. 26 de 2019 G.R.***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : ADX_UDT1:PFOSFECF.TXT"
   Locate 9, 1: Print "Actualizando Archivos  : TF:ECFOSF"
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
Call Pantalla.Principal																									  ! Pantalla Principal
Call Apertura.Archivos																										! Apertura de archivos
Call Carga.Masiva.Datos																										! Carga Masiva de Informacion
Call Fin.Proceso
Stop 

Errores.Aplicativo:
  U.Error% = 0
  CALL TRADUCE.ERROR
  UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  
  If ERR = "IH" Then Resume 
  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
  EndIf 

  If ERR = "EF" Then Resume 
  
  If ERRF% = 1 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO ADX_UDT1:PFOSFECF.TXT NO EXISTE... "
     Call Msg.Log
     Stop
  EndIf 
  If ERRF% = 4 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
  If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO ESTRUCTURAS EXCLUIDAS FOSFEC NO EXISTE.. CREANDO..."
     CREATE POSFILE "TF:ECFOSF" KEYED 8,,,50000 RECL 8 AS 4 COMPOUND PERUPDATE
     Resume 
  EndIf 

  CALL MSG.LOG
  Stop 
