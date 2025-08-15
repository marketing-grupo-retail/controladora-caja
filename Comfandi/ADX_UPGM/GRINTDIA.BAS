!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRINTDIA.BAS                     *
!Autor         : Oscar Valencia                   *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Generacion Interfaces Diarias    *
!                Cierre Automatico                *
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
Xa% = 30                         	! Calcula resto de tiempo para proxima hora
Xb% = 150000          						! Calcula tiempo en tiempos de Relog 
Call Adxserve(0,26,1,"Wait for Process 15 Min... "+Left$(time$,2)+":"+mid$(time$,3,2))
Wait ; Xb%
End Sub
!--- Fin calculo hora

Sub Mensaje(X.Men$)
String X.Men$
    If Ejecuta% = -1 Then \
     Call Adxserve(0,26,1,X.Men$) Else \
     Print X.Men$
End Sub

Sub RECORRE.FILE																! Recorre lista de programas a ejecutar
String TS.TEMP1$, TS.TEMP2$, X.Ejecuta$, X.men$
Integer*4 TS.TEMP1I4
  If Ejecuta% = -1 Then \
     Call Calculo.Hora                             ! Valida hora exacta para ejecutar
     
  If NR% <> 0 Then Begin                           ! Recorre vector para verificar procesos de carga
     For I% = 1 TO NR%
          X.Ejecuta$ = ARCHIVOS$(I%,1)
          X.Men$ = "Proc:"+ARCHIVOS$(I%,2)
          Call Mensaje(X.Men$)
          TS.TEMP1I4 = OSSHELL(X.Ejecuta$)       ! Ejecuta programa de carga
     Next I%
     X.Men$ = "Proceso Interfaces Finalizado..."
     Call Mensaje(X.Men$)
  EndIf 
  
End Sub
!--- Fin del recorrido del archivo

Sub Carga.Archivos
String TS.TEMP1$, TS.TEMP2$, X.men$
  NR% = 0 
  Dim ARCHIVOS$(500,2)                          ! Vector con informacion de la interfaces
  TS.ERROR = -1 
  Open "TF:CIERRE" AS 1                         ! Abre archivo definicion interfaces
  If TS.ERROR = -1 Then Begin                   ! Apertura satisfactoria
   IF END #1 THEN UE.FINAL	    							  ! Si es EOF
   While (1)																		! Recorre archivo
    Read #1; TS.TEMP1$,TS.TEMP2$  							! Lectura registro
    NR% = NR% + 1 															! Mueve apuntador
    ARCHIVOS$(NR%,1)   = TS.TEMP1$              ! Interface a Ejecutar
    ARCHIVOS$(NR%,2)   = TS.TEMP2$              ! Descriptor del proceso
   Wend
   UE.FINAL:
     Close 1
  EndIf Else Begin                              ! Error en la apertura
    X.Men$ = "TF:CIERRE NO EXISTE ..."
    Call Mensaje(X.Men$)
    Stop 
  EndIf 
  
End Sub 

!---
! Bloque Principal
!---

ON ERROR GOTO ERROR.PRG
DATO.SO$ = COMMAND$						! Dato S.O
Ejecuta% = 0 

If DATO.SO$ = "VERSION" Then BEGIN 
   Print "Proceso Interfaces Diarias Ver. 15/Jul/2011  10:12 am"
   Stop
ENDIF


If DATO.SO$ = "BACKGRND" Then Ejecuta% = -1
 Call Carga.Archivos
 Call RECORRE.FILE
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
