!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LDINOTAX.BAS                     *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Alimentacion masiva Articulos    *
!                Dia Sin iva                      *
!**************************************************
! Modificaciones:


%Environ C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$, Ue.Data2$,        \!
                  FileOut$, Param2$, XParam$, Ifile$
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
    Ifile$ = "ADX_UDT1:LDINOTAX.TXT"
    Open Ifile$ As 1																				 ! Carga archivos parámetros bono recompra
    Create "ADX_UDT1:LDINOTAX.ERR" As 2                      ! Errores proceso de carga
End Sub

Sub Carga.Masiva.Datos
String Xstr$, Xplu$, Xmonto$, Xqty$, Xfec$, Xdpto$, XReg$
  U.Qty% = 0																																! Init total registros
  While(1)																																	! Recorre archivo
      Read #1; XReg$																												! Lectura registro
      U.Qty% = U.Qty% + 1																										!
      Locate 12,27 : Print Str$(U.Qty%)																			!
      If U.Qty% = 1 Then Begin           																		! control de registros 
         Create Posfile "TF:DNOTAX" KEYED  8,,,20000 RECL 22 AS 3 COMPOUND PERUPDATE  ! Control estructura comercial
         Create Posfile "TF:CNOTAX" KEYED 17,,,50000 RECL 19 AS 5 COMPOUND PERUPDATE  ! Control compra clientes
      EndIf 
      U.Error% = -1 																												! Control errores
      Xstr$   = "1" + Left$(Xreg$,15)         															! Toma estructura comercial 
      Xplu$   = Mid$(Xreg$,16,7)																						! Plu 
      Xmonto$ = Str$(Val(Mid$(Xreg$,23,11)))										            ! Monto maximo precio
      Xqty$   = Mid$(Xreg$,34,04)																						! Qty maxima venta
      Xfec$   = Mid$(Xreg$,38,08)																						! Fecha aplicacion evento
      Xdpto$  = Mid$(Xreg$,46,03)																						! Departamento Evento
      Xmonto$ = Pack$(Right$("0000000000"+Xmonto$,10))
      Xstr$   = Pack$(Xstr$)                                                !
      Xqty$   = Pack$(Right$("0000"+Xqty$,4))                               !
      Xfec$   = Pack$(Xfec$)                                                !
      U.Error% = -1																													! Control errores
      Write Form "C8 C5 C2 C4 C3" ; #3;                                    \! Graba el registro
       Xstr$, Xmonto$, Xqty$, Xfec$, Xdpto$       													!
       If U.Error% = -1 Then Begin
         U.QtyOk% = U.QtyOk% + 1 
         Locate 13,27 : Print Str$(U.QtyOk%)
       EndIf Else Begin
         U.QtyNok% = U.QtyNok% + 1
         Locate 14,27 : Print Str$(U.QtyNok%)
         Write #2; "Linea :"+Str$(U.Qty%),Xreg$,UE.MSG$,FINR$
       EndIf    
  Wend
End Sub

Sub Fin.Proceso
    UE.MSG$ = "Fin Carga Dia sin IVA, Verifique Errores en ADX_UDT1:LDINOTAX.ERR"
    Call Msg.Log
    Close 3 : Close 2 : Close 5
    Delete 1
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "**** CARGA MASIVA PRODUCTOS DIA SIN IVA V.1.0**"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Junio     16 2020 GR.**"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : LDINOTAX.TXT"
   Locate 9, 1: Print "Actualizando Archivo   : TF:DNOTAX   "
   Locate 10,1: Print "Actualizando Parametros: TF:PBONOS   "
   Locate 11,1: Print "Creando Contingencia   : TF:CNOTAX   "
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
Ifile$ = command$
If Ucase$(Ifile$) = "VERSION" Then Begin
  Print "Grupo Retail Ltda"
  Print "CARGA MASIVA PRODUCTOS DIA SIN IVA Ver 1.0"
  Print "Ultima Revision Software 16/Jun/2020"
  Stop 
EndIf
Call Pantalla.Principal																									  ! Pantalla Principal
Call Apertura.Archivos																										! Apertura de archivos
Call Carga.Masiva.Datos																										! Carga Masiva de Informacion
Call Fin.Proceso
Stop 

Errores.Aplicativo:
  U.Error% = 0
  Call TRADUCE.ERROR
  UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  
  If ERR = "IH" Then Resume 
  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
  EndIf 

  If ERR = "EF" Then Resume 
 
  If ERRF% = 1 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO ADX_UDT1:LDINOTAX.TXT NO EXISTE... "
  EndIf 

  If ERRF% = 3 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "FALLA EN CREACION ARCHIVO PARAMETROS DNOTAX."
  EndIf 

  Call MSG.LOG
  Stop 
