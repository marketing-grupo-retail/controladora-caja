!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : BKFILIZO.BAS                     *
!Lenguaje      : Basic 4690 IBM                   *
!Observaciones : Modulo Carga variaiones          *
!               balanzas                          *
!**************************************************

%ENVIRON C                            ! Ambiente de controlador

Integer*1 SESION1%, SESION2%,GRABA%,A%,I%, TS.ERROR%

Integer*4 CONT.READ%, CONT.WRITE%,P, CONTADOR%,CANTIDAD.REG%

String GLOBAL 	CAD1$		,\
		DATA$		,\
		LLAVE$		,\
		PRECIO$		,\
		PRODUCTO$	,\
    CANTIDAD$	,\
		ID$		,\
		TIPO.TRX$	,\
		FECHA.V$	,\
		FILLER$		,\
		A$		,\
		FINR$		,\
    FECHA.G$	,\
		CLIENTE$	,\
		ESTADO$		,\
		STORE$		,\
		ARCHIVO$	,\
		REGISTRO$

SESION1% = 1
SESION2% = 2
CONT.READ% = 0

%INCLUDE ADX_UPGM:BASROUT.J86

Sub ADXSERVE(RET,FUNC,PARM1,PARM2$) EXTERNAL         !
     STRING       PARM2$                             !
     INTEGER*4    RET                                !
     INTEGER*2    FUNC,   PARM1                      !
End SUB                                              !

Function LEA.STORE
 STRING PARM2$
 INTEGER*2 PARM1,RET
 Call ADXSERVE(RET,4,PARM1,PARM2$)                 ! 
 STORE$ = RIGHT$("000"+STR$(VAL(Mid$(PARM2$,1,4))),3)
End Function

Function PROCESA.ARCHIVO(FILE$)
STRING FILE$, FILEOUT$, PRG$
FILEOUT$ = LEFT$(FILE$,6)
A% = OSSHELL("C:\FILIZOLA\FILINEW2.286")                         ! Ejecuta aplicacion balanza
End Function

! ******** INCIALIZACION DE ARCHIVOS

On Error GoTo ERRORES
Call LEA.STORE
INICIO:
DATA$ = "["+DATE$+"-"+TIME$+"]"+"Buscando Novedades."
P=0
If Val(date$) > 100617 Then Begin 
 DATA$ = "Tiempo Licencia Expirado"
 Print DATA$
 Call ADXSERVE(P,26,1,DATA$)
 Stop 
EndIf 
Print DATA$
Call ADXSERVE(P,26,1,DATA$)
WAIT;10000

A% = OSSHELL("DIR ADX_UDT1:BAL"+STORE$+".TXT > ADX_UDT1:BALTRX.TXT")
TS.ERROR% = -1
Open "ADX_UDT1:BALTRX.TXT" AS 50
If TS.ERROR% = -1 Then BEGIN 
 While(1)
  CLEARS
  Read #50;LINE REGISTRO$
  If Mid$(REGISTRO$,1,6) = "BAL"+STORE$ Then Begin
   ARCHIVO$ = Mid$(REGISTRO$,1,6)+".TXT"
   Print "PROCESANDO ...."+ARCHIVO$
   Call PROCESA.ARCHIVO(ARCHIVO$)
  EndIf
 Wend
 Close 50
EndIf 
GoTo INICIO
Stop


CLEARS
FINR$ = CHR$(13)+CHR$(10)

ERRORES:
TS.ERROR% = 0 
If  ERRF% = 50 AND ERR = "EF" Then Begin
   Close 50
   Resume INICIO
EndIf

CLEARS
DATA$ = "["+DATE$+"-"+TIME$+"]"+"UN ERROR FILE "+STR$(ERRF%)+" TIPO "+ERR
Print #52;DATA$
P=0
Print DATA$
Call ADXSERVE(P,26,1,DATA$)
STOP
