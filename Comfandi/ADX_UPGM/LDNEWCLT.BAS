!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LDNEWCLT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Alimentacion masiva Clientes     *
!                Vecino Fiel Contingencia         *
!**************************************************
! Modificaciones:

%Environ C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$, Clte$
Integer*1  Global U.Error%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty%, PP

%INCLUDE BASROUT.J86

Sub ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  ! Msg background
   INTEGER*4 RET
   INTEGER*2 FUNC,PARM1
   String PARM2
End Sub


Sub MSG.LOG
    Locate 23,1 : Print String$(78," ") 
    Locate 23,1 : Print "Msg: "+UE.MSG$
    Call ADXSERVE(PP,26,1,UE.MSG$)
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
    Open "C:\DATOS\CLIENTE.TXT"    AS 1	BUFFSIZE 2048 LOCKED ! Plano maestro de carga
    Open "TF:NEWCLT" KEYED RECL 85 AS 5	                	   ! Apertura Articulos controlados
    
    !CREATE POSFILE "Q:\TFNEWCLT" KEYED 9,,,1000000 RECL 85   \! Crea Cliente Temporal
    !                    AS 4          	     ! 

    Create "ADX_UDT1:CARGACLT.TXT" AS 2                      ! Errores proceso de carga
End Sub

Sub Carga.Masiva.Datos
String U.buffer$, U.Iva$, U.Dsct$
String X.Itm$, X.Ctrl$, X.Status$, Xkey$, XApell$, Xname$, Xpto$, XTipo$, Xtipifica$

  While(1)
      Read #1; Xkey$, XApell$, Xname$, XTipo$, Xpto$, Xtipifica$
      U.Qty% = U.Qty% + 1
      Locate 12,27 : Print Str$(U.Qty%)
      U.Error% = -1 

      XKey$   = Str$(Val(XKEY$))
      XKey$   = Pack$(Right$("000000000000000000"+XKey$,18))
      Xpto$   = Str$(Val(Xpto$))
      Xpto$   = Pack$(Right$("0000000000"+XPto$,10))
      Xname$  = Ucase$(Xname$)
      XApell$ = Ucase$(XApell$)
      Xtipifica$ = Left$(Xtipifica$+String$(30," "),30)
      Clte$ = Xkey$
      
      Write FORM "C9 2C20 C5 C1 C30" ; #5;    \ Graba el registro en archivo de notas
          XKey$ ,                             \ Identificacion del cliente UPD 9
          XApell$,                            \ Apellidos del cliente
          XName$,                             \ Nombre del cliente
          Xpto$,                              \ Puntos Totales del cliente
          Xtipo$,                             \ Tipo de cliente
          Xtipifica$
       If U.Error% = -1 Then Begin
         U.QtyOk% = U.QtyOk% + 1 
         Locate 13,27 : Print Str$(U.QtyOk%)
       EndIf Else Begin
         U.QtyNok% = U.QtyNok% + 1
         Locate 14,27 : Print Str$(U.QtyNok%)
         Write #2; Unpack$(X.Itm$),UE.MSG$,FINR$
       EndIf    
       UE.MSG$ = "ACT: "+STR$(U.QtyOk%)+" Fail:"+Str$(U.QtyNok%)
       Call ADXSERVE(PP,26,1,UE.MSG$)
  Wend
End Sub

Sub Fin.Proceso
    UE.MSG$ = "TOTAL ACT:"+STR$(U.QtyOk%)+" Fail:"+Str$(U.QtyNok%)
    Call Msg.Log

    !WAIT ; 100
    !Call OSSHELL("COPY Q:\TFNEWCLT ADX_IDT1:")
    
    Delete 1  
    Close 2 : Close 5
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****   CARGA MASIVA VECINO FIEL CONTINGENCIA V.1.0   ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Junio     25 2012  G.R. ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : ADX_UDT1:CLIENTE.TXT"
   Locate 9, 1: Print "Actualizando Archivo   : TF:NEWCLT   "
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
  If ERRF% = 1 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO C:\DATOS\CLIENTE.TXT NO EXISTE... "
     Call Msg.Log
     Stop
  EndIf 
  If ERRF% = 4 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
  If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO MAESTRO DE CLIENTES FIDELIZADOS NO EXISTE... "
     Call Msg.Log
     Stop        
  EndIf 
  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
  EndIf 
  CALL MSG.LOG
  STOP

