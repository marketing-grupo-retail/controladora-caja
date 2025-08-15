!************************************************** 
!Empresa       : ASIC S.A. UNIDAD DE RETAIL       *
!Programa      : CGIRPTOS.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Julio 12 de 2.004                *
!Observaciones : Alimentacion masiva IRPUNTOS     *
!**************************************************
! Modificaciones:


%ENVIRON C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$
Integer*1  Global U.Error%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty2%

%INCLUDE adx_upgm:EAMITEMR.J86																				 ! Variables maestra de articulos

!------------------------------------------------------------------------------
!
!                            PUTN1/PUTN2/PUTN4
!
!       These routines insert a one/two/four byte integer into a string.
!       P2 is the offset within the string and P3 is the source integer
!
!------------------------------------------------------------------------------
  
  FUNCTION PUTN1(P1$,P2,P3) EXTERNAL
  STRING P1$
  INTEGER*2 P2
  INTEGER*1 P3
  END FUNCTION
  
  FUNCTION PUTN2(P1$,P2,P3) EXTERNAL
  STRING P1$
  INTEGER*2 P2,P3
  END FUNCTION
  
  FUNCTION PUTN4(P1$,P2,P3) EXTERNAL
  STRING P1$
  INTEGER*2 P2
  INTEGER*4 P3
  END FUNCTION





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


Sub Apertura.Archivos
    Open "ADX_UDT1:IRPUNTOS.TXT" AS 1																				! Plano maestro de carga
    Open "IRPUNTOS" KEYED RECL 7 AS 4			  															! Apertura Ctrl Puntos 
    Create "ADX_UDT1:CARGAERR.TXT" AS 2                                   ! Errores proceso de carga
End Sub

Sub Carga.Masiva.Datos
String U.PLU$, U.PTO$, U.IND$

  While(1)
      Read #1; U.Plu$, U.PTO$, U.Ind$
      U.Qty2% = U.Qty2% + 1
      Locate 12,27 : Print Str$(U.Qty2%)
      U.Error% = -1 
      IR.ITEMCODE$ = Str$(Val(U.Plu$))
      IR.ITEMCODE$ = Pack$(Right$("000000000000"+IR.ITEMCODE$,12))
      If U.Error% = -1 Then Begin 
       If Val(U.Ind$) <> 3 Then Begin 
        Write Form "C6 C1" ; #4;        \! Graba el registro en la maestra de articulos
          IR.ITEMCODE$,         \ ITEM CODE                  UPD 6
          U.PTO$                ! NUMERO DE PUNTOS A OTORGAR Asc 1
       EndIf Else Begin
          DELREC 4; IR.ITEMCODE$
       EndIf
      EndIf    

      If U.Error% = -1 Then Begin
         U.QtyOk% = U.QtyOk% + 1 
         Locate 13,27 : Print Str$(U.QtyOk%)
      EndIf Else Begin
         U.QtyNok% = U.QtyNok% + 1
         Locate 14,27 : Print Str$(U.QtyNok%)
         Write #2; Unpack$(ir.itemcode$),IR.ITEMNAME$,Str$(U.Qty2%),UE.MSG$,FINR$
      EndIf    
  Wend
  
End Sub

Sub Fin.Proceso
    UE.MSG$ = "Fin Carga Masiva Articulos, Verifique Errores en ADX_UDT1:CARGAERR.TXT"
    Call Msg.Log
    Close 4 : Close 1: Close 2
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****   CARGA MASIVA MAESTRO PUNTOS ADICIONALES       ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Abril 25    2.006  ASIC ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : ADX_UDT1:IRPUNTOS.TXT"
   Locate 9, 1: Print "Actualizando Archivo   : IRPUNTOS.DAT"
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


Errores.Aplicativo:
  U.Error% = 0
  CALL TRADUCE.ERROR
  UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  
  If ERR = "IH" Then Resume 
  If ERR = "IO" Then Resume
  
  If ERRF% = 1 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO ADX_UDT1:IRPUNTOS.TXT NO EXISTE... "
     Call Msg.Log
     Stop
  EndIf 
  If ERRF% = 4 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
  If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO MAESTRO DE IRPUNTOS NO EXISTE... "
     Call Msg.Log
     Stop        
  EndIf 
  If ERRF% = 4 AND ERR = "EF" THEN Begin
     Resume
  EndIf   
  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
  EndIf 
  CALL MSG.LOG
  STOP

