!************************************************** 
!Empresa       : Grupo Retail Ltda -  Retail      *
!Programa      : GrmPipes.Bas                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 Ibm                   * 
!Fecha         : Junio de 2.013                   *
!Observaciones : Rutinas Genericas Manejo de Pipes*
!                Routing Services                 *
!**************************************************

%Environ T

%INCLUDE EAMTSWKG.J86
%INCLUDE EAMTRANS.J86


\ REM  *****************************************************************
\ REM   PIPE FUNCTION DEFINITIONS
\ REM  *****************************************************************

  FUNCTION PRSTCRT(IONUM,PIPESIZE,PIPEID) EXTERNAL
  INTEGER*2 PRSTCRT                               !
  INTEGER*2 IONUM,PIPESIZE                        !
  STRING PIPEID                                   !
  END FUNCTION                                    !

  FUNCTION PRSTINT EXTERNAL                       !
  INTEGER*4 PRSTINT                               !
  END FUNCTION                                    !

  FUNCTION PRSTWRT(PRSNUM,DEST,BUFFER) EXTERNAL   !
  INTEGER*4 PRSTWRT                               !
  INTEGER*4 PRSNUM                                !
  STRING    DEST, BUFFER                          !
  END FUNCTION                                    !

Sub VISOR.AND.BORRAR(X.MSG$) External             !
  STRING X.MSG$																		!
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
      Gstat% = PRSTCRT( Gses%, Gsize%, Gname$ )															! Realiza Creacion Pipe
      If ( Gstat% <> 0 ) Then Begin																					! Falla Creacion Pipe
      	  Call Visor.and.borrar("FAIL PIPE CREATION :" + "Error :"+str$( Gstat% ) ) ! Msg Alerta
          Gr.Create.Pipe = 0																								! Reporta Falla creacion
          Exit Function																											! Sale del proceso
      EndIf Else Begin                                                      ! Creacion satisfactoria
         TS.TEMP5I4 = PRSTINT																								! Direccion en Memoria
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
      If Gsize% <= 0 or Gsize% > 120 Then Begin															! Longitud Invalida
      	  Call Visor.and.borrar("INVALID LONG PIPE" ) 											! Msg Alerta
          Gr.Write.Pipe = 0				  																				! Reporta Falla grabacion
          Exit Function																											! Sale del proceso
      EndIf																																	!

      Gname$ = Right$("000"+Str$(SL.HD.TERMINAL),3)     +     			       \! Numero de terminal
               Right$(Gname$,1)                                             ! Nombre del Pipe
      Gdata$ = LEFT$(Gdata$+STRING$(Gsize%," "),Gsize%)                     ! Ajuste dato para grabacion
      Gstat% = PRSTWRT(Gmem%, Gname$, Gdata$)	 															! Almacena dato en Pipe
      WAIT ; 500																														! Time to wait
      If Gstat% <> 0 Then Begin																							! Error Grabacion
      	 Gr.Write.Pipe = 0																									!
      	 Exit Function																											!
      EndIf																																	!
      Gr.Write.Pipe = -1																										! Proceso Correcto

End Function 

Function Gr.Answer.Pipe( GrPipeIn, GrWait%, Gsize% ) Public                 ! Respuesta Pipe
Integer*2 GrPipeIn																													! Variables temporales
Integer*1 Gr.Answer.Pipe																										! 
Integer*4 GrWait%, Gsize%						  																			!
String    Gr.Rta$, GrLen$																										!
  TS.TEMP1$ = ""																														! Init Variable
  If Gsize% <= 0 or Gsize% > 120 Then Begin															    ! Longitud Invalida
 	   Call Visor.and.borrar("INVALID LONG PIPE" ) 											      ! Msg Alerta
     Gr.Answer.Pipe = 0																								      ! Reporta Falla creacion
     Exit Function																											    ! Sale del proceso
  EndIf																																	    !
  GrLen$ = "C" + Str$(Gsize%)                                               ! Formato lectura pipe
  Wait GrPipeIn; GrWait%																										! Espera Rta del Pipe
  If ( event% = GrPipeIn ) Then Begin																			  ! Encuentra Actividad
   TS.ER.RETURN = -1																												! Control de errores
   Read Form GrLen$; #GrPipeIn; Gr.Rta$                                     ! Rta del pipe
   Gr.Answer.Pipe = TS.ER.RETURN																						! Reporta proceso de lectura
   TS.TEMP1$ = Gr.Rta$																											! Retorna Rta del Pipe
  EndIf Else Begin                                                          ! No hay actividad en el pipe
   Gr.Answer.Pipe = 0																												! Reporta Error Pipe
   TS.TEMP1$ = "XX"																													! Reporta salida por Time Out
  EndIf																																			! 

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
