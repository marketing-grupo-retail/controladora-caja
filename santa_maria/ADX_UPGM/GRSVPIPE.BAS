!************************************************** 
!Empresa       : Grupo Retail Ltda -  Retail      *
!Programa      : GrSvPipe.Bas                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 Ibm                   * 
!Fecha         : Junio de 2.013                   *
!Observaciones : Simulador Servidor proceso de    *
!                requerimientos pipes             *
!**************************************************

%Environ C

Integer*1 Global Cerror%
Integer*2 Global RtaPipe%, RtaProc%, T%
String    Global Npipe$, RtaPipe$, S$, M$

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

\ REM  *****************************************************************
\ REM   PIPE FUNCTION GRETAIL
\ REM  *****************************************************************

Function Gr.Create.Pipe ( Gses%, Gsize%, Gname$ ) Public										! Proceso Creacion de Pipes
Integer*2 Gses%, Gsize%																											! Variables de trabajo
String    Gname$																														!
Integer*4 Gstat%																														!
Integer*1 Gr.Create.Pipe																										!
  If Gsize% <= 0 or Gsize% > 120 Then Begin															    ! Longitud Invalida
 	   Call Visor.and.borrar("INVALID LONG PIPE" ) 											      ! Msg Alerta
     Gr.Create.Pipe = 0																								      ! Reporta Falla creacion
     Exit Function																											    ! Sale del proceso
  EndIf																																	    !
      Gstat% = PRSCCRT( Gses%, Gsize%, Gname$ )															! Realiza Creacion Pipe
      If ( Gstat% <> 0 ) Then Begin																					! Falla Creacion Pipe
      	  Call Visor.and.borrar("FAIL PIPE CREATION :" + "Error :"+str$( Gstat% ) ) ! Msg Alerta
          Gr.Create.Pipe = 0																								! Reporta Falla creacion
          Exit Function																											! Sale del proceso
      EndIf Else Begin                                                      ! Creacion satisfactoria
         TS.TEMP5I4 = PRSCINT																								! Direccion en Memoria
         Print "Offset Memory Pipe "+Str$(TS.TEMP5I4)
         
         If ( TS.TEMP5I4 = 0 ) Then Begin																		! Falla Proceso
            Call Visor.and.borrar("FAIL PIPE MEMORY   :" + "Error :"+str$( TS.TEMP5I4 ) ) ! Msg Alerta
            Gr.Create.Pipe = 0																							! Reporta Falla creacion
            Exit Function																											! Sale del proceso
         EndIf            																									!
      EndIf																																	!
      Gr.Create.Pipe = -1																								    ! Reporta Proceso satisfactorio

End Function

Function Gr.Write.Pipe ( GMem%, Gname$, Gdata$, GSize% ) Public		   				! Proceso Grabacion de Pipes
Integer*4 Gstat%, Gsize%, Gmem%																							!
String    Gname$, Gdata$																										!
Integer*1 Gr.Write.Pipe                                                     !
!      If Gsize% <= 0 or Gsize% > 120 Then Begin															! Longitud Invalida
!      	  Call Visor.and.borrar("INVALID LONG PIPE" ) 											! Msg Alerta
!          Gr.Write.Pipe = 0				  																				! Reporta Falla grabacion
!          Exit Function																											! Sale del proceso
!      EndIf																																	!
!      Gdata$ = LEFT$(Gdata$+STRING$(Gsize%," "),Gsize%)                     ! Ajuste dato para grabacion
!      Print "Almacena en "+Str$(Gmem%)+" "+Gname$
!      Gstat% = PRSCWRT(Gmem%, Gname$, Gdata$)	 															! Almacena dato en Pipe
      WAIT ; 500																														! Time to wait
      If Gstat% <> 0 Then Begin																							! Error Grabacion
      	 Print "Respuesta Pipe grabacion :"+Str$(Gstat%)
      	 Gr.Write.Pipe = 0																									!
      	 Exit Function																											!
      EndIf																																	!
      Gr.Write.Pipe = -1																										! Proceso Correcto

End Function 

Function Gr.Answer.Pipe( GrPipeIn, GrWait%, Gsize% ) Public                 ! Respuesta Pipe
Integer*2 GrPipeIn																													! Variables temporales
Integer*1 Gr.Answer.Pipe, Xtimes%																						! 
Integer*4 GrWait%, Gsize%						  																			!
String    Gr.Rta$, GrLen$																										!
  Xtimes% = 0
  TS.TEMP1$ = ""																														! Init Variable
  If Gsize% <= 0 or Gsize% > 120 Then Begin															    ! Longitud Invalida
 	   Call Visor.and.borrar("INVALID LONG PIPE" ) 											      ! Msg Alerta
     Gr.Answer.Pipe = 0																								      ! Reporta Falla creacion
     Exit Function																											    ! Sale del proceso
  EndIf																																	    !
  GrLen$ = "C" + Str$(Gsize%)                                               ! Formato lectura pipe
  While (Xtimes% < 5)
    Print "Esperando Rta Pipe "+Str$(Xtimes%)
    Wait GrPipeIn; GrWait%																									! Espera Rta del Pipe
     Cerror% = -1																											! Control de errores
     Read Form GrLen$; #GrPipeIn; Gr.Rta$                                   ! Rta del pipe
     Gr.Answer.Pipe = Cerror%																					! Reporta proceso de lectura
     If Cerror% = -1 Then Begin
        TS.TEMP1$ = Gr.Rta$																										! Retorna Rta del Pipe
        Xtimes% = 10
        Gr.Answer.Pipe = -1
     EndIf Else Begin
     	Gr.Answer.Pipe = 0
     	TS.TEMP1$ = "TO"
     EndIf
    Xtimes% = Xtimes% + 1
  Wend 
End Function

Sub Gr.Clean.Pipe ( GrSes%, Grsize% )	Public																! Proceso limpieza pipe
String    GrData$, GrLen$																										!
Integer*2 GrSes%, GrOk%, Grsize%																						!
GrOk% = -1																																	! Control del proceso
While ( GrOk% )																															! Procesa limpieza del pipe
   Wait GrSes%; 800																													! Espera rta del pipe
   If ( event% = GrSes% ) Then Begin																				! Hay algo en el pipe
   	  GrLen$ = "C" + Str$(Grsize%)                                         	! Formato lectura pipe
      Read Form GrLen$ ; #GrSes%; GrData$  																	! Realiza lectura
   EndIf Else Begin																													! No hay actividad en pipe
      GrOk% = 0																															! Termina Proceso
   EndIf																																		!
Wend   																																			!
End Sub

Sub Crea.H

!!--- Creacion Pipe Request
Npipe$ = "H"          																											! Letra creacion Pipe
RtaPipe% = Gr.Create.Pipe ( 11, 120, Npipe$ )															! Creacion del pipe
If RtaPipe% <> -1 Then Begin																								! Si creacion Fallida
	 Print "Error Creacion Pipe K "
	 Stop 
EndIf
Print "Pipe Creado Exitosamente "+Npipe$+" "+"ADX0CC"+NPIPE$+"P"					  ! Presenta nombre pipe creado

End Sub 

Sub Crea.T
String Pipe$
!!--- Creacion Pipe Request
!Npipe$ = "T"          																											! Letra creacion Pipe
!RtaPipe% = Gr.Create.Pipe ( 12, 120, Npipe$ )															! Creacion del pipe
!If RtaPipe% <> -1 Then Begin																								! Si creacion Fallida
!	 Print "Error Creacion Pipe T "
!	 Stop 
!EndIf
!Print "Pipe Creado Exitosamente "+Npipe$+" "+"ADX0CC"+NPIPE$+"P"					  ! Presenta nombre pipe creado

Input "Nombre del Pipe ";pipe$
Pipe$ = "PI:"+UCASE$(PIPE$)

Print "Procediendo Apertura Pipe "+Pipe$

!Open PIPE$ As 12 BUFFSIZE 37000 noread nodel

Create Pipe$ As 12 buffsize 37000
Open "PI:PROUT045" As 13 buffsize 37000


Print "Pipe Abierto Exitosamente "+pipe$
wait ; 2500

End Sub 

!--- Bloque Principal

On Error GoTo RUN.ERROR
Clears 
Print "Grupo Retail Ltda "
Print "Simulador Server requerimientos PIPES          "
Print "-----------------------------------------------------"

While (1)																															    	! Ciclo Infinito
   Call PRSCCLS
   TS.TEMP5I4 = PRSCINT																								      ! Direccion en Memoria
   Print "Offset Memory Pipe "+Str$(TS.TEMP5I4)
   If ( TS.TEMP5I4 = 0 ) Then Begin																		      ! Falla Proceso
    Call Visor.and.borrar("FAIL PIPE MEMORY   :" + "Error :"+str$( TS.TEMP5I4 ) ) ! Msg Alerta
    Stop 
   EndIf            																								        !
   
   Call Crea.T
   
   While T% > 0
     Print "Esperando Requerimiento al pipe "+time$
     Wait ; 1000
     Cerror% = -1
     read form "C5"; #13; s$
     If Cerror% = -1 Then Begin 
     	  Print "Longitud trama "+S$ 
     	  m$ = "C" + s$
     	  read form m$; #13 ; s$
     	  Print "Rta:"+s$
     	  Print "TERMINO "+TIME$
     endif
   Wend 
   TERMINA:
   Input "Otro Proceso ENTER o CTRL-C Cancelar ";TS.TEMP1$
   Close 12
   Close 13
      
Wend    																																	  ! Fin Ciclo

Stop 


RUN.ERROR:
  Cerror% = 0
  If ERR = "EF" Then Resume 
  	
  Call VISOR.AND.BORRAR("ERROR DE APLICACION "+ERR+" SESS:"+STR$(ERRF%))
Stop 
