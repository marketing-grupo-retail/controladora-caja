!************************************************** 
!Empresa       : GRUPO RETAIL LTDA                *
!Programa      : GRLDPTOS.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Alimentacion masiva EAMITEMR     *
!                manejo puntajes                  *
!**************************************************
! Modificaciones:

%ENVIRON C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$, Store$, Cfile$, GR.IRDATA$
Integer*1  Global U.Error%, Tmp.Indicat1A
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty2%

%INCLUDE EAMITEMR.J86																				 ! Variables maestra de articulos

 Function GETN4(P1$,P2) EXTERNAL
     INTEGER*4 GETN4
     STRING P1$
     INTEGER*2 P2
 End Function

Function PUTN4(P1$,P2,P3) EXTERNAL
  String P1$
  Integer*2 P2
  Integer*4 P3
End Function


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

Sub Fin.Proceso
    UE.MSG$ = "Fin Carga Masiva Articulos, Verifique Errores en ADX_UDT1:CARGAPTO.TXT"
    Call Msg.Log
    Close 4 : Delete 1: Close 2
    Wait ; 2500
    Stop
End Sub 

Sub Apertura.Archivos
    Open "C:\DATOS\EAMITEMR.DAT" KEYED RECL 254 AS 4																		! Apertura Eamitemr
    Create "ADX_UDT1:CARGAPTO.TXT" AS 2                                   ! Errores proceso de carga
End Sub


Sub Carga.Masiva.Datos
String Id$, Ean$, Secc$, Rubro$, Subr$, grupo$, Acum$, Ptos$, Redim$, QtyRed$, VlrRed$, Pago$, \
       Vigencia$, Fini$, Hini$, Mini$, Ffin$, Hfin$, Mfin$, Estado$, Ue1$, Ue2$, Obs$
Integer*1 U.Ind%
Integer*4 U.QtyCaja%, U.Pric2%, U.Qty%, GREX1%
    Open Cfile$ AS 1					              															! Plano maestro de carga
    If End # 1 Then Final.Proceso
While (1)
      Read #1; Id$, Ean$, Secc$, Rubro$, Subr$, grupo$, Acum$, Ptos$, Redim$, QtyRed$, VlrRed$, Pago$, \
               Vigencia$, Fini$, Hini$, Mini$, Ffin$, Hfin$, Mfin$, Estado$, Ue1$, Ue2$, Obs$
      U.Qty2% = U.Qty2% + 1
      Locate 12,27 : Print Str$(U.Qty2%)
      U.Error% = -1 
      IR.ITEMCODE$ = Pack$(Right$("000000000000"+Ean$,12))
      %INCLUDE EAMIREAS.J86       
      If U.Error% = -1 Then Begin 
         TMP.INDICAT1A = IR.INDICAT1A                                              ! Salva dato de variable
         IR.INDICAT1A = 0       																									 ! Init estado de la variable
         IF  TMP.INDICAT1A And 80H    THEN \
             IR.INDICAT1A = IR.INDICAT1A OR 80H            !
         IF  TMP.INDICAT1A And 40H    THEN \
             IR.INDICAT1A = IR.INDICAT1A OR 40H            !
         IF  TMP.INDICAT1A And 20H    THEN \
             IR.INDICAT1A = IR.INDICAT1A OR 20H            !
         IF  TMP.INDICAT1A And 10H    THEN \
             IR.INDICAT1A = IR.INDICAT1A OR 10H            !
         IF  TMP.INDICAT1A And 08H    THEN \
             IR.INDICAT1A = IR.INDICAT1A OR 08H            !
         IF  TMP.INDICAT1A And 04H    THEN \
             IR.INDICAT1A = IR.INDICAT1A OR 04H            !
         IF  Acum$ = "S"              THEN \
             IR.INDICAT1A = IR.INDICAT1A Or 02H            !
         IF  Redim$ = "S"             THEN \
             IR.INDICAT1A = IR.INDICAT1A Or 01H            !
        
         Ptos$ = Pack$(Right$("00000000"+Ptos$,8))                                  ! Ptos adicionales  Campo 21 MSI Number
         GR.IRDATA$ = Mid$(GR.IRDATA$,1,8) + Ptos$ + Mid$(GR.IRDATA$,13,196)        ! Arma String dato

         QtyRed$ = Pack$(Right$("000000"+QtyRed$,6))                                ! Ptos a redimir Campo 20 SUBCOMODITY
         GR.IRDATA$ = Mid$(GR.IRDATA$,1,5) + QtyRed$ + Mid$(GR.IRDATA$,9,200)       ! Arma String dato

         IR.USEREXIT1 = Val(Ue1$)																										! Mensaje 1
         IR.USEREXIT2 = Val(Ue2$)																										! Mensaje 2

         Id$ = Pack$(Right$("000000"+Id$,6))                                        ! ID Promocion Campo 44 SecondaryPointsClub
         GR.IRDATA$ = Mid$(GR.IRDATA$,1,54) + Id$ + Mid$(GR.IRDATA$,58,151)         ! Arma String dato
         
         VlrRed$ = Pack$(Right$("0000000000"+VlrRed$,10))                           ! Valor redencion Campo 35 CompPricePD
         GR.IRDATA$ = Mid$(GR.IRDATA$,1,30) + VlrRed$ + Mid$(GR.IRDATA$,36,173)     ! Arma String dato
         
         Pago$ = Pack$(Right$("00"+Pago$,2))                                        ! Tipo Variedad Pago Campo 47 CouponLevel
         GR.IRDATA$ = Mid$(GR.IRDATA$,1,115) + Pago$ + Mid$(GR.IRDATA$,117,92)      ! Arma String dato

        WRITE FORM "C6,3I1,C1,C2,C3,2C1,C5,C2,C18,2I2,C208" ; #4;                  \! Graba el registro en la maestra de articulos
          IR.ITEMCODE$,         \ ITEM CODE                  UPD 6
          IR.INDICAT0,          \ INDICATOR FLAG BYTE 0      INT 1
          IR.INDICAT1,          \ INDICATOR FLAG BYTE 1      INT 1
          IR.INDICAT1A,         \ INDICATOR FLAG BYTE 2      INT 1
          IR.INDICAT2$,         \ INDICATOR BYTE 3           UPD 1
          IR.DEPARTME$,         \ DEPARTMENT NUMBER          UPD 2
          IR.FAMILYNU$,         \ COUPON FAMILY NUMBERS      UPD 3
          IR.MPGROUP$,          \ MULTIPRICING GROUP NUMBER  UPD 1
          IR.SALEQUAN$,         \ DEAL QUANTITY              UPD 1
          IR.SALEPRIC$,         \ PRICE                      UPD 5
          IR.LINKEDTO$,         \ LINKED ITEM                UPD 2
          IR.ITEMNAME$,         \ ITEM DESCRIPTION           ASC 18
          IR.USEREXIT1,         \ RESERVED FOR USER DATA     INT 2
          IR.USEREXIT2,         \ RESERVED FOR USER DATA     INT 2
          GR.IRDATA$            ! ADITIONAL DATA IR          Asc 208
      EndIf    

       If U.Error% = -1 Then Begin
         U.QtyOk% = U.QtyOk% + 1 
         Locate 13,27 : Print Str$(U.QtyOk%)
       EndIf Else Begin
         U.QtyNok% = U.QtyNok% + 1
         Locate 14,27 : Print Str$(U.QtyNok%)
         Write #2; Unpack$(ir.itemcode$),Str$(U.Qty2%),UE.MSG$,FINR$
       EndIf    

Wend
Final.Proceso:
   Call Fin.Proceso
End Sub


Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****   CARGA MASIVA ACTIVACION FLAG PUNTAJE  V.1.0   ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Octubre 30   2013  G.R  ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : "+Cfile$
   Locate 9, 1: Print "Actualizando Archivo   : EAMITEMR.DAT"
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

Cfile$ = "ADX_UDT1:LDITMPTO.TXT"																					! Archivo de carga puntaje
Call Pantalla.Principal																									  ! Pantalla Principal
Call Apertura.Archivos																										! Apertura de archivos
Call Carga.Masiva.Datos																										! Carga Masiva de Informacion


Errores.Aplicativo:
  U.Error% = 0
  CALL TRADUCE.ERROR
  UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  
  If ERR = "IH" Then Resume 
  If ERR = "SS" Then Resume 
  If ERR = "IO" Then Resume
  
  If ERRF% = 1 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO "+Cfile$+" NO EXISTE... "
     Call Msg.Log
     Wait ; 2500
     Stop
  EndIf 
  If ERRF% = 4 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
  If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO MAESTRO DE ARTICULOS NO EXISTE... "
     Call Msg.Log
     Wait ; 2500
     Stop        
  EndIf 
  If ERRF% = 4 AND ERR = "EF" THEN Begin
     Resume
  EndIf   
  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
  EndIf 
  Call MSG.LOG
  Stop 

