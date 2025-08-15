!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LDCLSLTE.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Alimentacion masiva Clientes     *
!                Salitre                          *
!**************************************************
! Modificaciones:

%Environ C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$
Integer*1  Global U.Error%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty%

function osshell(cmd.line$) external			! routine to start
							! another program.
           integer*4    osshell				! Upon completion of
           string       cmd.line$			! program, control is
							! returned to calling
end function						! program.



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
    Open "ADX_UDT1:SALITRE.TXT"   AS 1								       ! Plano maestro de carga
    Open "ADX_UDT1:SALITRE.DAT" KEYED RECL 12 AS 4				   ! Apertura Articulos controlados
    Create "ADX_UDT1:ERRSLTE.TXT" AS 2                       ! Errores proceso de carga
End Sub

Sub Carga.Masiva.Datos
String U.buffer$, U.Iva$, U.Dsct$
String X.Itm$, X.Ctrl$, X.Status$, X.Und$, X1$, X2$
  While(1)
      Read #1; X.Itm$, X.Ctrl$, X.Status$,X1$, X2$
      U.Qty% = U.Qty% + 1
      Locate 12,27 : Print Str$(U.Qty%)
      U.Error% = -1 
      X.Und$   =  X.itm$
      X.Itm$   = Str$(Val(X.Itm$))
      X.Itm$   = Pack$(Right$("000000000000"+X.Itm$,12))
      X.Ctrl$  = Str$(Val(X.Ctrl$))
      X.Ctrl$  = Pack$(Right$("000000"+X.Ctrl$,6))
      X1$      = Pack$(Right$("00"+X1$,2))
      X2$      = Pack$(Right$("00"+X2$,2))
      WRITE FORM "C6 C3 3C1" ; #4;     \! Graba el registro en archivo de notas
          X.Itm$ ,                     \! Codigo del cliente    PD 6
          X.Ctrl$,                     \! Fecha actualizacion   PD 3
          X.Status$,                   \! Estado cliente        C  1
          X1$,                         \! Grupo cliente         PD 1
          X2$                           ! Tipo  cliente         PD 1

       If U.Error% = -1 Then Begin
         U.QtyOk% = U.QtyOk% + 1 
         Locate 13,27 : Print Str$(U.QtyOk%)
       EndIf Else Begin
         U.QtyNok% = U.QtyNok% + 1
         Locate 14,27 : Print Str$(U.QtyNok%)
         Write #2; (X.Und$),UE.MSG$,FINR$
       EndIf    
  Wend
End Sub

Sub Fin.Proceso
String Ffinal$
    UE.MSG$ = "Fin Carga Masiva Clientes, Verifique Errores en ADX_UDT1:ERRSLTE.TXT"
    Call Msg.Log
    Close 4 : Close 1: Close 2
    Ffinal$ = "COPY ADX_UDT1:SALITRE.TXT ADX_UDT1:SL"+DATE$+".COP"
    Call Osshell(fFINAL$)
    Open "ADX_UDT1:SALITRE.TXT"   AS 1								       ! Plano maestro de carga
    Delete 1
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****   CARGA MASIVA CLIENTES SALITRE         V.1.0   ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Noviembre 20 2014  G.R. ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : ADX_UDT1:SALITRE.TXT "
   Locate 9, 1: Print "Actualizando Archivo   : SALITRE     "
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
     UE.MSG$ = "ARCHIVO ADX_UDT1:LDCLCLTE.TXT NO EXISTE... "
     Call Msg.Log
     Stop
  EndIf 
  If ERRF% = 4 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
  If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO MAESTRO DE CLIENTES SALITRE NO EXISTE... "
     CREATE POSFILE "SALITRE" KEYED 6,,,200000 RECL 12 AS 4 COMPOUND PERUPDATE
     Resume 
  EndIf 
  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
  EndIf 
  CALL MSG.LOG
  Stop 
