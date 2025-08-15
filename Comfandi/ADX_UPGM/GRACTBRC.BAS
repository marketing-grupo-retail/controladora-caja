!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRACTBRG.BAS                     *
!Autor         : Oscar Valencia                   *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Actualizacion Bonos recompra     *
!                contingencia                     *
!**************************************************
! Observaciones
!

%ENVIRON C

String    GLOBAL MSG$, ERRFX$, PROFREE(1), TMP$, IFILE$, COSTO$, \
                 JOURNAL$, TLOG$, LOG$, UE.IP$, UE.USER$, UE.PASSWD$, UE.RUTA$, \
                 AA$, BB$, CC$, DD$, LLAVE$
Integer   GLOBAL I%, CTRL%, BAN.PRG%, True, False 
Integer*2 GLOBAL  WI2%, RTA.PROMO%
Integer*4 Global Apuntador%

%INCLUDE ADX_UPGM:BASROUT.J86

Sub Adxserve(RET,FUNC,PARM1,PARM2) EXTERNAL
Integer*4 RET
Integer*2 FUNC,PARM1
String PARM2
End Sub

Function ADXERROR(TERM,MSGGRP,MSGNUM,SEVERITY,EVENT,UNIQUE$) EXTERNAL
Integer*2 	TERM,MSGNUM
Integer*1	SEVERITY,MSGGRP,EVENT
String		UNIQUE$
End Function

Sub ADXCOPYF(RETC,INFILE,OUTFILE,OPT0,OPT1) EXTERNAL
   Integer*4 RETC
   String INFILE, OUTFILE
   Integer*2 OPT0, OPT1
End Sub

Function TRADUCE.ERROR                                       !
Integer*4 HX%, S%, SX%, SUM%
String    Z$
    HX% = ERRN                                               ! Traduccion de los
    ERRFX$=""                                                ! codigos de error
    FOR S% = 28 TO 0 STEP -4                                 ! del sistema operativo
        SX% = SHIfT(HX%,S%)                                  !
        SUM% = SX% AND 000FH                                 !
        If SUM% > 9 Then SUM% = SUM% + 55                   \!
        Else SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                      !
        ERRFX$ = ERRFX$ + Z$                                 !
    NEXT S%                                                  !
End Function

Sub Mensaje(Xmsg$)
String XMSG$
 If IFILE$ = "BACKGRND" Then Call ADXSERVE(0,26,1,XMSG$) \
  Else Print XMSG$
 
End Sub 

SUB JavaCall.Initialize.Request(ClassName,MethodName,TheRequest) EXTERNAL
  STRING ClassName
  STRING MethodName
  STRING TheRequest
END SUB

SUB JavaCall.AddParameter.String(TheRequest,TheParameter) EXTERNAL
  STRING TheRequest
  STRING TheParameter
END SUB

SUB JavaCall.InvokeMethod.ReturnString(TheRequest,ReturnValue, Exception) EXTERNAL
  STRING TheRequest
  STRING ReturnValue
  STRING Exception
END SUB

Function Rutina.Java(Java.CLASS$, Java.METHOD$, Java.MESSAGE$) Public 					! Inicializacion Clases de Java
String Java.METHOD$, Java.REQUEST$, Java.EXCEPTION$, Java.CLASS$,             			       \! Def. Variables Tmp
       Java.RETURNVALUE$, Java.MESSAGE$, Rutina.Java            					!

Java.REQUEST$     = "C$"										! Init de variables
Java.EXCEPTION$   = ""											! para la ejecucion
Java.RETURNVALUE$ = ""											! del modulo
Java.EXCEPTION$   = "" 											! Init variable
Call Javacall.Initialize.Request(Java.CLASS$,Java.METHOD$,Java.REQUEST$)  				! Inicializacion de clases
Call Javacall.AddParameter.String(Java.REQUEST$,Java.MESSAGE$)						! Adicion de parametros
Call Javacall.InvokeMethod.ReturnString(Java.REQUEST$,Java.RETURNVALUE$,                               \! Ejecucion de la clase
        				Java.EXCEPTION$)						!

If LEN(Java.EXCEPTION$) > 0 THEN BEGIN									! Si hay Exception
   Locate 23,1 : Print string$(78," ")
   Locate 23,1 : Print "Error Java "+Java.EXCEPTION$
   Java.RETURNVALUE$ = ""										
ENDIF Else Begin											! Ejecucion OK

!   CALL U.Imprime("Respuesta Entregada",4100H)								! Imprime Error
!   CALL U.Imprime(LEFT$(Java.RETURNVALUE$,38),4100H)							! en SJ

Endif													!

Rutina.Java = Java.RETURNVALUE$										! Retorna Respuesta
   
End Function
!--- Fin invocacion rutinas de Java


Sub CALCULO.HORA
String Xh$, Xa$, X.Men$
Integer*4 Xa%, Xb%
Xh$ = Left$(TIME$,4)             ! Toma hora y minutos
Xa% = Val(Xh$)                   ! Toma Valor Numerico
Xa% = Round(Xa%,-2,1)            ! Calcula Proxima Hora de ejecucion
Xa$ = Right$("0000"+Str$(Xa%),4) ! Ajusta Formato
Xa$ = Left$(Xa$,2) + ":" + Right$(Xa$,2)
Xh$ = Mid$(TIME$,3,2)            ! Toma Minutos
Xa% = val(Xh$)                   ! Convierte valor numerico
Xa% = 60 - Xa%                   ! Calcula resto de tiempo para proxima hora
Xb% = (Xa% * 60) * 1000          ! Calcula tiempo en tiempos de Relog

Xa% = Val(Left$(time$,2))        ! Toma Hora 
If Xa% + 1 > 23 Then Xa% = 00 Else \
   Xa% = Xa% + 1 
X.Men$ = "Proximo Proceso "+Right$("00"+Str$(Xa%),2)+":00"
Call Mensaje(X.Men$)
Print "TIEMPO DE ESPERA "+STR$(XB%)
xb% = 1200 
Wait ; Xb%
End Sub
!--- Fin calculo hora

Sub Busca.Apuntador
    Ban.Prg% = True
    Open "TF:APUNTA" As 12 Append 
    If Ban.Prg% = False Then Begin
    	 Create "TF:APUNTA" As 12 
    EndIf
    Close 12 
    Apuntador% = 1 
End Sub 

Sub Actualiza.Apuntador

End Sub 

Sub Procesa.Bonos
String Trama$, RtaProc$
    Call Busca.Apuntador
    Ban.Prg% = True
    Open "BRENCOLA" RECL 126 AS 11 NOWRITE NODEL    
    If Ban.Prg% = False Then Begin 
    	 Call Mensaje("ARCHIVO ENCOLAMIENTOS BONOS NO EXISTE")
    	 Exit Sub 
    EndIf
    While (1)
      Ban.Prg% = True
      Read #11,Apuntador%;Trama$
      If Ban.Prg% = False Then Begin 
      	 Call Actualiza.Apuntador
      	 Close 11 
      	 Exit Sub 
      EndIf
      Print "Procesa "+Trama$
      RtaProc$ = Rutina.Java("com.appl.ApplKernel","threader", Trama$)         ! Ejecuta Requerimiento
      Print "Rta Proceso "+RtaProc$
      Apuntador% = Apuntador% + 1 
    Wend 
End Sub 

!---
! Bloque Principal
!---

On Error GoTo ERROR.PRG
IFILE$ = Command$                						 ! Como esta ejecutando
IF IFILE$ = "VERSION" THEN BEGIN 
	 PRINT "MODULO ACTUALIZACION BONO RECOMP Ver 1.0 08/Oct/2015 11:36 am"
	 Stop
ENDIF
True = -1
False = 0

While (1)                                    ! Ciclo infinito
 Call Calculo.Hora               						 ! Cada Hora Verifica si encuentra archivos para cargar
 Call Procesa.Bonos             				     ! Procesar bonos encolados
Wend
Stop

ERROR.PRG:
     Ban.Prg% = False 
     Call TRADUCE.ERROR
     If ERR = "EF" Then Resume
     If ERR = "IH" Then Resume 
     If ERR = "*I" Then Resume 
     If (ERR = "OE" OR ERR = "FU") Then Resume

     MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
     Call Mensaje(MSG$)
		 Stop
