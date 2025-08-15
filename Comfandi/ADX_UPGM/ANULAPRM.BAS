!************************************************** 
!Empresa       : COMFANDI                         *
!Programa      : ANULAPRM.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Carga de Articulo Promociones    *
!**************************************************
! Modificaciones:
!

%ENVIRON C						   																		 ! Ambiente de controlador

String    Global UE.MSG$, ERRFX$, Finr$, Dm.Almacen$, IR.USERDATA$
Integer*1  Global U.Error%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty%

%INCLUDE EAMITEMR.J86

Sub MSG.LOG
    Locate 23,1 : Print String$(78," ") 
    Locate 23,1 : Print "Msg: "+UE.MSG$
End Sub 

FUNCTION TRADUCE.ERROR                                       !
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
END FUNCTION

SUB ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  ! Msg background
   INTEGER*4 RET
   INTEGER*2 FUNC,PARM1
   STRING PARM2
END SUB


Function LEER.CABECERA
 String PARM2$
 INTEGER*2 PARM1,RET
 Call ADXSERVE(RET,4,PARM1,PARM2$)                 ! 
 DM.ALMACEN$ = RIGHT$("000"+STR$(VAL(MID$(PARM2$,1,4))),3)
End Function 
!--- Fin de la funcion de lectura


Sub Apertura.Archivos
    Open "ADX_UDT1:ARTPROMO.TXT"    AS 1				! Plano maestro de carga
    Open "EAMITEMR" KEYED RECL 77   AS 4			  ! Apertura Eamitemr
    Create "ADX_UDT1:CARGAERR.TXT"  As 2        ! Log de errores
End Sub

Sub Carga.Masiva.Datos
String Registro$, Cupon$, Family$, Ind$
  While(1)
      Read #1; Registro$
      U.Qty% = U.Qty% + 1
      Locate 12,27 : Print Str$(U.Qty%)
      U.Error% = -1 
      IR.ITEMCODE$  = Left$(Registro$,7)
      IR.ITEMCODE$  = Pack$(Right$("000000000000"+IR.ITEMCODE$,12))
      Cupon$        = Pack$("0000")
      Family$       = Pack$("000000")
      Ind$          = Mid$(Registro$,12,1)
      %INCLUDE EAMIRRD1.J86          ! Lectira del Eamitemr
      If U.Error% = -1 Then Begin 
        Write Form "C6,3I1,C1,C2,C3,2C1,C5,C2,C18,2I2 C31" ;          \
            #4; IR.ITEMCODE$,        \ ITEM CODE                  UPD 6
               IR.INDICAT0,          \ INDICATOR FLAG BYTE 0      INT 1
               IR.INDICAT1,          \ INDICATOR FLAG BYTE 1      INT 1
               IR.INDICAT1A,         \ INDICATOR FLAG BYTE 2      INT 1
               IR.INDICAT2$,         \ INDICATOR BYTE 3           UPD 1
               IR.DEPARTME$,         \ DEPARTMENT NUMBER          UPD 2
               Family$,              \ FAMILY COUPON NUMBERS      UPD 3
               IR.MPGROUP$,          \ MULTIPRICING GROUP NUMBER  UPD 1
               IR.SALEQUAN$,         \ DEAL QUANTITY              UPD 1
               IR.SALEPRIC$,         \ PRICE                      UPD 5
               Cupon$,               \ LINKED ITEM                UPD 2
               IR.ITEMNAME$,         \ ITEM DESCRIPTION           ASC 18
               IR.USEREXIT1,         \ RESERVED FOR USER DATA     INT 2
               IR.USEREXIT2,         \ RESERVED FOR USER DATA     INT 2
               IR.USERDATA$          ! Datos Adicionales Expan.   Asc 31
       EndIf    
       If U.Error% = -1 Then Begin
         U.QtyOk% = U.QtyOk% + 1 
         Locate 13,27 : Print Str$(U.QtyOk%)
       EndIf Else Begin
         U.QtyNok% = U.QtyNok% + 1
         Locate 14,27 : Print Str$(U.QtyNok%)
         Write #2; Registro$,UE.MSG$,FINR$
       EndIf    
  Wend
End Sub

Sub Fin.Proceso
    UE.MSG$ = "Fin Carga Masiva Articulos, Verifique Errores en ADX_UDT1:CARGAERR.TXT"
    Call Msg.Log
    Close 4 : Close 1: Close 2: Close 5
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****  REVERSO MASIVO PROMOCION ARTICULOS     V.1.1   ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software 31/Mzo/2010 Grupo Retail **"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : ADX_UDT1:ARTPROMO.TXT"
   Locate 9, 1: Print "Actualizando Archivo   : EAMITEMR.DAT"
   Locate 12,1: Print "Registros Procesados   : "
   Locate 13,1: Print "Registros Actualizados : " 
   Locate 14,1: Print "Registros No Procesados: " 
   Locate 15,1: Print "Procesando Tienda Nro. : "+DM.ALMACEN$
   Locate 23,1: Print "Msg : "
   FINR$ = CHR$(13) + CHR$(10)   ! Marca de fin de registro
End Sub


!---
!------- Bloque Principal
!---

On Error GoTo Errores.Aplicativo	! Control de proceso de errores
Call LEER.CABECERA                      ! Lectura codigo de la tienda
Call Pantalla.Principal			! Pantalla Principal
Call Apertura.Archivos			! Apertura de archivos
Call Carga.Masiva.Datos			! Carga Masiva de Informacion
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
     UE.MSG$ = "ARCHIVO ADX_UDT1:ARTPROMO.TXT NO EXISTE... "
     Call Msg.Log
     Stop
  EndIf 
  If ERRF% = 4 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
  If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO MAESTRO DE ARTICULOS NO EXISTE... "
     Call Msg.Log
     Stop        
  EndIf 
  CALL MSG.LOG
  STOP

