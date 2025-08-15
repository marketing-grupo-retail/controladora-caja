!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRDELLOG.BAS                     *
!Autor         : Oscar Valencia                   *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Eliminacion Logs comunicaciones  *
!                Punto de Venta.
!                Directorios ADX_UDT1: y JAVABIN: *
!**************************************************
! Version 1.0 11/Jun/2019
!--------------------------------------------------


%ENVIRON C

String    GLOBAL MSG$, ERRFX$, TMP$, IFILE$, LISTA$(2)
Integer   GLOBAL I%, TS.ERROR, CTRL%
Integer*4 Global Nlogs%
INTEGER*2 GLOBAL    TS.DATEFORM                 !* Date Format

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

!*****************************************************************************!
!*                                                                           *!
!*  Function    :   DaysIn                                                   *!
!*                                                                           *!
!*  Description :   Calculate the maximum days in a given month/year         *!
!*                                                                           *!
!*  Parameters  :   month                                                    *!
!*              :   year                                                     *!
!*                                                                           *!
!*  Returns     :   DaysIn                                                   *!
!*                                                                           *!
!*  Notes       :   Returns 29 for leap years even is divibles by 100.       *!
!*              :   30 days hath September, April, June & November. All      *!
!*                  the rest have 31 except for February which has 28 and    *!
!*                  29 each leap year.                                       *!
!*                                                                           *!
!*****************************************************************************!
FUNCTION DaysIn(month,year)
  INTEGER*1 DaysIn
  INTEGER*1 month
  INTEGER*1 year
  IF (month = 2) THEN BEGIN
    IF (mod(year, 4)) THEN BEGIN
      DaysIn = 28
    ENDIF ELSE BEGIN
      DaysIn = 29
    ENDIF
  ENDIF ELSE BEGIN
    IF (month = 4) OR \
       (month = 6) OR \
       (month = 9) OR \
       (month = 11) THEN BEGIN
      DaysIn = 30
    ENDIF ELSE BEGIN
      IF (month < 1) OR \
         (month > 12) THEN BEGIN
        DaysIn = 0
      ENDIF ELSE BEGIN
        DaysIn = 31
      ENDIF
    ENDIF
  ENDIF
END FUNCTION


Sub Carga.Tipo.Logs																													! Carga lista de logs a depurar
String XD1$, XD2$																														! variables temporales
Dim Lista$(100,2)																														! dimensión vector proceso
TS.ERROR = -1
Nlogs% = 0																																! Control errores
Open "TF:DELLOG" As 1																												! Apertura lista logs
If TS.ERROR <> -1 Then Begin																								! Falla proceso apertura
	 Call Mensaje("ERROR APERTURA LISTA LOGS")																! Mensaje Alerta
	 Stop 																																		! detiene ejecucion programa
EndIf																																				!
If End #1 Then Fin.Logs																											! Si EOF 
While (1) 																																	! Recorre archivo
  READ #1 ; XD1$, XD2$																											! Lectura registro
  Nlogs% = Nlogs% + 1																												! Aumento apuntador
  Lista$(Nlogs%,0) = Xd1$																										! Prefijo log
  Lista$(Nlogs%,1) = Xd2$																										! Ruta Busqueda
Wend 																																				!
Fin.Logs:																																		!
 Close 1																																		! cierre archivo
 
End Sub 																																		! Fin carga lista logs

Sub Retiro.Logs																															! Retiro Logs punto de venta
Integer*1 Proc%, Xt%, Xa%, Xd%, Xmes%, Xyear%
Integer*4 Xi%, Xj%, RETCODE, Tf%
String    Xfile$, AAMMDD$, Xfec$
Xfec$ = date$																																! Fecha del sistema
XT% = Val(mid$(Xfec$,3,2))    																							! Toma Mes 
XA% = Val(mid$(Xfec$,1,2))    																							! Toma año proceso    
XD% = Val(mid$(Xfec$,5,2))    																							! Toma dia

If IFILE$ = "BACKGRND" Then Begin
   Proc% = DaysIn(Xt%,Xa%)																									! Retorna dias del mes en proceso
   If (Proc% <> Xd%) Then Begin 																						! Si no es fin de mes
	    Call Mensaje("NO ES FIN DE MES, PROCESO CANCELADO")										! 
	    Exit Sub 																															! 
   EndIf																																		!
   XT% = Val(mid$(Xfec$,3,2)) - 1																						! Calcula Mes Anterior
   XA% = Val(mid$(Xfec$,1,2))    																						! Toma año proceso    
   If Xt% <= 0 Then Begin 																									! Si fin mes enero
	    Xt% = 12																															! Asigna mes diciembre
	    Xa% = Xa% - 1																													! Toma Año anterior
   EndIf																																		!
EndIf Else Begin																														! Proceso Interactivo
	Input "Ingrese Año: ";Xyear%
	Input "Ingrese Mes: ";Xmes%
	
	If Xyear% <= Xa% Then Begin																								! Año correcto
		 Xt% = Xmes%
		 Xa% = Xyear%
  EndIf Else Begin
  	Call Mensaje("AÑO NO VALIDO PARA PROCESO")
  	Exit Sub 
  EndIf
EndIf

TF% = 0
For Xi% = 1 To Nlogs%																												! Recorre lista de logs
 For Xj% = 1 To 25  																												! Lista 25 terminales
     Xfile$ = Lista$(Xi%,1)+Lista$(Xi%,0)+Right$("00"+Str$(Xa%),2) +    \! Arma nombre archivo
              Right$("00"+Str$(Xt%),2) + "??." + Right$("000"+Str$(Xj%),3)   ! a eliminar

     RETCODE = OSSHELL("DEL "+XFILE$)
     Call Mensaje("Procesando: "+XFILE$)
 Next Xj%																																		!
Next Xi%																																		! Fin lista logs

End Sub 																																		! Fin carga lista logs

!---
! Bloque Principal
!---

On Error GoTo ERROR.PRG																											! Control errores
IFILE$ = Command$                						 																! Como esta ejecutando
If Ucase$(IFILE$) = "VERSION" Then Begin 																		!
	 Print "ELIMINACION LOGS COMUNICACIONES POS Ver 1.0 14/Jun/2019 09:45 am"	!
	 Stop																																			!
EndIf 																																			!
Nlogs% = 0
Call Carga.Tipo.Logs           				     																  ! Carga Archivos 
Call Retiro.Logs
Call Mensaje("Proceso Terminado.")
Stop																																			!

ERROR.PRG:
     TS.ERROR = 0
     Call TRADUCE.ERROR
     If ERR = "IH" Then Resume 
     If ERR = "CU" Then Resume 
     If ERR = "*I" Then Resume 
     If ERR = "EF" Then Resume 
     If (ERR = "OE" OR ERR = "FU") Then Resume
     
     MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
     Call Mensaje(MSG$)
		 Stop
