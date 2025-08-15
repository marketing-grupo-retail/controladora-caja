!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LDITMAB2.BAS                     *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Alimentacion masiva Articulos    *
!                Abbot Json                       *
!**************************************************
! Modificaciones:
! Mod 11 Nov 2020
!-------------------------------------------------

%Environ C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$
Integer*1  Global U.Error%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty%


Sub ADXCOPYF(RETC,INFILE,OUTFILE,OPT0,OPT1,OPT2) EXTERNAL
 INTEGER*1 ADXCOPYF
 INTEGER*4 RETC
 INTEGER*2 OPT0,OPT1,OPT2
 STRING INFILE, OUTFILE
End Sub 


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
Integer*4 RETC
String Xin$, Xout$
    Xin$ = "ADX_UDT1:LDITMAB2.TXT"
    Xout$ = "ADX_UDT1:LDITMAB2.COP"
    Call ADXCOPYF(RETC,Xin$,Xout$,0,0,0)
    If RETC <> 0 THEN BEGIN                        ! if bad rc
       Ue.Msg$ = "ADXCOPYF FALLIDO "
       Call Msg.Log
    ENDIF
    wait ; 1000
    Open "ADX_UDT1:LDITMAB2.TXT"   AS 1								       ! Plano maestro de carga
    Open "TF:ITMAB2" KEYED RECL 13 AS 4				  					   ! Apertura Articulos controlados
    Open "TF:PRVAB2" KEYED RECL 10 AS 5				  					   ! Apertura proveedor Abbot
    Create "ADX_UDT1:ERRAB2.TXT"   AS 2                      ! Errores proceso de carga
End Sub

Sub Carga.Masiva.Datos
String U.buffer$, U.Iva$, U.Dsct$
String X.Sku$, X.Ean$, X.Status$, XReg$, Xprov$
  While(1)

      !Read #1; X.Sku$, X.Ean$, X.Status$
      Read #1; XReg$
      U.Qty% = U.Qty% + 1
      Locate 12,27 : Print Str$(U.Qty%)
      U.Error% = -1 
      X.Sku$   = Mid$(Xreg$,1,7)     ! SKU Comfandi
      X.Ean$   = Mid$(Xreg$,8,13)    ! EAN ABBOT
      Xprov$   = Mid$(Xreg$,21,6)    ! Proveedor
      X.Status$ = Right$(Xreg$,1)    ! Acciona a realizar 1. Add/Mod 3.Erase
      
      X.Sku$   = Str$(Val(X.Sku$))
      X.Sku$   = Pack$(Right$("000000000000"+X.Sku$,12))
      X.Ean$   = Str$(Val(X.Ean$))
      X.Ean$   = Pack$(Right$("00000000000000"+X.Ean$,14))
      Xprov$   = Pack$(Right$("000000"+Xprov$,6))

      If Val(X.Status$) <> 3 Then Begin 
        WRITE FORM "C6 C7" ; #4;               \! Graba el registro en archivo de Pfizer
          X.Sku$ ,                             \! Articulo controlado        UPD 6
          X.Ean$                                ! Codigo EAN articulo        UPD 7
        Write Form "C7 C3" ; #5;               \! Graba proveedor por ean reportado
          X.Ean$,                              \! Codigo EAN articulo        UPD 7
          Xprov$                                ! Codigo del proveedor       UPD 3
       EndIf
       
       If Val(X.Status$) = 3 Then Begin 
       	  DelRec 4 ; X.Sku$
       	  DelRec 5 ; X.Ean$
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
    UE.MSG$ = "Fin Carga Masiva Abbot, Verifique Errores en ADX_UDT1:ERRAB2.TXT"
    Call Msg.Log
    Close 4 : Close 2
    Delete 1             ! Add 14dic2021
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****   CARGA MASIVA CODIGOS ABBOT 2 Ver.1.1****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Diciembre 14 2021 GR***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : ADX_UDT1:LDITMAB2.TXT"
   Locate 9, 1: Print "Actualizando Archivos  : TF:ITMAB2 TF:PRVAB2"
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
     UE.MSG$ = "ARCHIVO ADX_UDT1:LDITMAB2.TXT NO EXISTE... "
     Call Msg.Log
     Stop
  EndIf 
  If ERRF% = 4 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
  If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO MAESTRO DE ARTICULOS ABBOT NO EXISTE.. CREANDO..."
     CREATE POSFILE "TF:ITMAB2" KEYED 6,,,10000 RECL 13 AS 4 COMPOUND PERUPDATE
     Resume 
  EndIf 
  If ERRF% = 5 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO MAESTRO DE PROVEEDOR ABBOT NO EXISTE.. CREANDO..."
     CREATE POSFILE "TF:PRVAB2" KEYED 7,,,10000 RECL 10 AS 5 COMPOUND PERUPDATE
     Resume 
  EndIf 


  CALL MSG.LOG
  Stop 
