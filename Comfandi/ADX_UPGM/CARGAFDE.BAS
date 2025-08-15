!************************************************** 
!Empresa       : COMFANDI                         *
!Programa      : CARGAFDE.BAS                     *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Carga Asociados Fondo Empleados  *
!**************************************************

%ENVIRON C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$ 
Integer*1  Global U.Error%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty%

%INCLUDE EAMITEMR.J86																				 ! Variables maestra de articulos

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
    Open "c:\datos\FONDO.TXT" AS 1																				! Plano maestro de carga
 !   Open "EAMFDEMP" KEYED RECL 29 AS 4																		! Apertura Fondo de Empleados
    Create "ADX_UDT1:CARGAERR.TXT" AS 2                                   ! Errores proceso de carga    
End Sub

Sub Carga.Masiva.Datos
String U.buffer$, U.Iva$, U.Data$
Integer*4 X.Vlr%

  While(1)
      Read #1; U.Data$
      U.Qty% = U.Qty% + 1
      Locate 12,27 : Print Str$(U.Qty%)
      U.Error% = -1 
      IR.ITEMCODE$ = Mid$(U.DATA$,1,10)
      IR.ITEMCODE$ = Pack$(Right$("0000000000"+IR.ITEMCODE$,10))
      IR.ITEMNAME$ = Mid$(U.Data$,11,20)
      IR.SALEQUAN$ = Mid$(U.Data$,31,10)
      X.Vlr% = Val(ir.salequan$)
      If U.Error% = -1 Then Begin 
        WRITE FORM "C5 C20 I4" ; #4;        \! Graba el registro en la maestra de articulos
          IR.ITEMCODE$,         \ Codigo del asociado        UPD 5
          IR.ITEMNAME$,         \ Nombre del asociado        ASC 20
          x.vlr%                ! Cupo de Credito            Int 4

       EndIf    
       If U.Error% = -1 Then Begin
         U.QtyOk% = U.QtyOk% + 1 
         Locate 13,27 : Print Str$(U.QtyOk%)
       EndIf Else Begin
         U.QtyNok% = U.QtyNok% + 1
         Locate 14,27 : Print Str$(U.QtyNok%)
         Write #2; Unpack$(ir.itemcode$),IR.ITEMNAME$,IR.SALEQUAN$,FINR$
       EndIf    
  Wend
End Sub

Sub Fin.Proceso
    UE.MSG$ = "Fin Carga Fondo Empleados, Verifique Errores en ADX_UDT1:CARGAERR.TXT"
    Call Msg.Log
    Close 4 : Close 1: Close 2
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****   CARGA MASIVA MAESTRO FONDO EMPLEADOS  V.1.0   ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Julio 19 de 2.004       ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : C:\DATOS\FONDO.TXT"
   Locate 9, 1: Print "Actualizando Archivo   : EAMFDEMP.DAT"
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
Create posfile "EAMFDEMP" keyed 5 ,,, 10000 recl 29 as 4
Call Pantalla.Principal																									  ! Pantalla Principal
Call Apertura.Archivos																										! Apertura de archivos
Call Carga.Masiva.Datos																										! Carga Masiva de Informacion


Errores.Aplicativo:
  U.Error% = 0
  
  If ERR = "IH" Then Resume 
  
  If ERRF% = 1 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO c:\datos\FONDO.TXT NO EXISTE... "
     Call Msg.Log
     Stop        

  EndIf 
  If ERRF% = 4 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
  If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO MAESTRO DE FONDO DE EMPLEADOS ... "
     Call Msg.Log
     Stop        
  EndIf 
  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
  EndIf 
  CALL TRADUCE.ERROR
  UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  CALL MSG.LOG
  STOP

