!-- programa creacion pipe

String global Pipe$
Integer*1 Global Cerror%
Integer*2 Global RtaPipe%, RtaProc%
String    Global Npipe$, RtaPipe$

%INCLUDE EAMTSWKG.J86
%INCLUDE EAMTRANS.J86


\ REM  *****************************************************************
\ REM   PIPE FUNCTION DEFINITIONS
\ REM  *****************************************************************

  FUNCTION PRSCCRT(IONUM,PIPESIZE,PIPEID) EXTERNAL
  INTEGER*2 PRSCCRT                               !
  INTEGER*2 IONUM,PIPESIZE                        !
  STRING PIPEID                                   !
  END FUNCTION                                    !

  FUNCTION PRSCINT EXTERNAL                       !
  INTEGER*4 PRSCINT                               !
  END FUNCTION                                    !

  FUNCTION PRSCWRT(PRSNUM,DEST,BUFFER) EXTERNAL   !
  INTEGER*4 PRSCWRT                               !
  INTEGER*4 PRSNUM                                !
  STRING    DEST, BUFFER                          !
  END FUNCTION                                    !

  FUNCTION PRSCCLS EXTERNAL
  INTEGER*1 PRSCCLS
  END FUNCTION


Sub VISOR.AND.BORRAR(X.MSG$) Public               !
  STRING X.MSG$, A$																!
  locate 22, 1 : Print X.msg$
  locate 22,77 : Input A$
  locate 22, 1 : Print String$(78," ")
End Sub																						!

On Error GoTo pipe1

Clears 
Print "Grupo Retail Ltda "
Print "Simulador Respuesta PIPE"
Print "-----------------------------------------------------"
Input "Nombre del Pipe ";pipe$
Pipe$ = "PI:"+UCASE$(PIPE$)


!--- Creacion Pipe Request
Npipe$ = "INPIPE"          																											! Letra creacion Pipe
RtaPipe% = Gr.Create.Pipe ( 12, 120, Npipe$ )															! Creacion del pipe
If RtaPipe% <> -1 Then Begin																								! Si creacion Fallida
	 Print "Error Creacion Pipe T "
	 Stop 
EndIf
!Print "Pipe Creado Exitosamente "+Npipe$+" "+"ADX0CC"+NPIPE$+"P"					  ! Presenta nombre pipe creado

Print "Pipe Creado Exitosamente "+Npipe$+" "+"ADX0CC"+NPIPE$+"P"					  ! Presenta nombre pipe creado
Input "Nombre del Pipe ";pipe$
Pipe$ = "PI:"+UCASE$(PIPE$)



pipe1:
Print "ERROR DE APLICACION "+ERR+" SESS:"+STR$(ERRF%)
Stop 
