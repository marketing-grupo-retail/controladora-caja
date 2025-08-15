!************************************************** 
!Empresa       : ASIC S.A  UNIDAD DE RETAIL       *
!Programa      : CNVCARGA.BAS                     *
!Autor         : Oscar Valencia                   *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Alimentacion Carga Convenios     *
!**************************************************

%ENVIRON C

String    Global MSG$, ERRFX$, PROFREE(1), TMP$, Dato.So$
Integer   Global TS.ERROR, CTRL%
Integer*2 Global  WI2%, Ejecuta%
Integer*4 Global NR%, I%
String    Global AA$, BB$, CC$, DD$, LLAVE$, ARCHIVOS$(2)

%INCLUDE ADX_UPGM:BASROUT.J86

Sub Adxserve(RET,FUNC,PARM1,PARM2)  External
Integer*4 RET
Integer*2 FUNC,PARM1
String PARM2
End Sub

Function ADXSTART(NOMBRE,PARAMETROS,MENSAJE) External 
 Integer*2 ADXSTART
 String NOMBRE,PARAMETROS,MENSAJE
End Function


Sub TRADUCE.ERROR                                       !
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
End Sub

Sub CALCULO.HORA
String Xh$, Xa$
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
Call Adxserve(0,26,1,"Convenios Proxima Verificacion "+Xa$)
Wait ; Xb%

End Sub
!--- Fin calculo hora

Sub Mensaje(X.Men$)
String X.Men$
    If Ejecuta% = -1 Then \ 
       Call Adxserve(0,26,1,X.Men$) Else \ ! Msg Alerta
       Print X.Men$
End Sub

Sub RECORRE.FILE																! Recorre lista de programas a ejecutar
String TS.TEMP1$, TS.TEMP2$, X.Ejecuta$, X.men$
Integer*4 TS.TEMP1I4
  Call Calculo.Hora                             ! Valida hora exacta para ejecutar
  If NR% <> 0 Then Begin                        ! Recorre vector para verificar procesos de carga
     For I% = 1 TO NR%
       TS.TEMP1$ = ARCHIVOS$(I%,1)              ! Ruta del archivo
       TS.ERROR = -1                            ! Ctrl de errores
       Open TS.TEMP1$ AS 2                      ! Apertura del plano
       If TS.ERROR = -1 Then Begin              ! Si apertura correcta
          Close 2                               ! Cierra archivo
          X.Ejecuta$ = ARCHIVOS$(I%,2) + " " + ARCHIVOS$(I%,1)
          X.Men$ = "Ejecutando "+ARCHIVOS$(I%,2)
          Call Mensaje(X.Men$)
          TS.TEMP1I4 = OSSHELL(X.Ejecuta$) ! Ejecuta programa de carga
       EndIf Else Begin       
          X.Men$ = "No Existe "+ARCHIVOS$(I%,1)   !
          Call Mensaje(X.Men$)
          Wait ; 1500
       EndIf 
     Next I%
     X.Men$ = "Terminando Verificacion, Espere ..."
     Call Mensaje(X.Men$)
     Wait ; 60000
  EndIf 
  
End Sub
!--- Fin del recorrido del archivo

Sub Carga.Archivos
String TS.TEMP1$, TS.TEMP2$, X.men$
  NR% = 0 
  Dim ARCHIVOS$(500,2)                          ! Vector con informacion de la interfaces
  TS.ERROR = -1 
  Open "ADX_UDT1:CARGAS.DAT" AS 1               ! Abre archivo de interfaces
  If TS.ERROR = -1 Then Begin                   ! Apertura satisfactoria
   IF END #1 THEN UE.FINAL	    							  ! Si es EOF
   While (1)																		! Recorre archivo
    Read #1; TS.TEMP1$,TS.TEMP2$  							! Lectura registro
    NR% = NR% + 1 															! Mueve apuntador
    ARCHIVOS$(NR%,1) = TS.TEMP1$                ! Almacena ruta del archivo
    ARCHIVOS$(NR%,2) = TS.TEMP2$                ! Programa de Carga
   Wend
   UE.FINAL:
     Close 1
  EndIf Else Begin                              ! Error en la apertura
    X.Men$ = "ADX_UDT1:CARGAS.DAT NO EXISTE ..."
    Call Mensaje(X.Men$)
    Wait ; 5800 
  EndIf 
  
End Sub 

!---
! Bloque Principal
!---

ON ERROR GOTO ERROR.PRG
DATO.SO$ = COMMAND$						! Dato S.O
Ejecuta% = 0 
If DATO.SO$ = "BACKGRND" Then Ejecuta% = -1

While (1) 
 Call Carga.Archivos
 Call RECORRE.FILE
Wend
Stop

ERROR.PRG:
         If ERR = "OE" OR ERR = "FU" Then Begin         ! Error de apertura
            TS.ERROR = 0
            Resume 
         EndIf 
         
         If ERR = "EF" Then TS.ERROR = 0 : Resume 

	       If ERRF% = 2  Then Begin
            TS.ERROR = 0 
	          Resume 
	       EndIf
         
         Call TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
         Call Adxserve(0,26,1,MSG$)
		     Stop
