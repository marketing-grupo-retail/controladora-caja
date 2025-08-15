!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LDPRMLST.BAS                    *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Alimentacion masiva Listas de    *
!                Promociones                      *
!**************************************************
! Modificaciones:

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
    Open "C:/PROMODAT/LDPRMLST.TXT"  AS 1								       ! Plano maestro de carga
    Open "TF:LISTAS" KEYED RECL 6    AS 4				  					   ! Apertura Articulos controlados
    Create "ADX_UDT1:ERRPRM.TXT"     AS 2                      ! Errores proceso de carga
End Sub

Sub Carga.Masiva.Datos
String U.buffer$, U.Iva$, U.Dsct$
String X.Sku$, X.Ean$, X.Status$, XReg$
  While(1)

      Read #1; X.Sku$, X.Status$
      U.Qty% = U.Qty% + 1
      Locate 12,27 : Print Str$(U.Qty%)
      U.Error% = -1 
      
      X.Sku$   = Str$(Val(X.Sku$))
      X.Sku$   = Pack$(Right$("000000000000"+X.Sku$,12))

      If Val(X.Status$) <> 3 Then Begin 
        WRITE FORM "C06" ; #4;               \! Graba el registro en archivo de listas
          X.Sku$                             \! Articulo Promocionado      UPD 6
      EndIf
       
       If Val(X.Status$) = 3 Then Begin 
       	  DelRec 4 ; X.Sku$
       EndIf 
       
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
    UE.MSG$ = "Fin Carga Masiva Listas, Verifique Errores en ADX_UDT1:ERRPRM.TXT"
    Call Msg.Log
    Close 4 : Close 1: Close 2
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****  CARGA MASIVA LISTAS PROMOCION Ver.1.0****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Febrero 02 2018 G.R.***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : /PROMODAT/LDITMPRM.TXT"
   Locate 9, 1: Print "Actualizando Archivo   : TF:LISTAS   "
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
     UE.MSG$ = "ARCHIVO /PROMODAT/LDITMLST.TXT NO EXISTE... "
     Call Msg.Log
     Stop
  EndIf 
  If ERRF% = 4 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
  If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO MAESTRO DE LISTAS PROMOCION NO EXISTE.. CREANDO..."
     Call MSG.LOG
     CREATE POSFILE "TF:LISTAS" KEYED 6,,,50000 RECL 6 AS 4 COMPOUND PERUPDATE
     Resume 
  EndIf 
  CALL MSG.LOG
  Stop 
