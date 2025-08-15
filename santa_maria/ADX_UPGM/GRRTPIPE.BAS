!************************************************** 
!Empresa       : Grupo Retail Ltda -  Retail      *
!Programa      : GrRtPipe.Bas                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 Ibm                   * 
!Fecha         : Junio de 2.013                   *
!Observaciones : Simulador Respuesta Pipes        *
!                Routing Services                 *
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

Open PIPE$ As 12 BUFFSIZE 37000 noread nodel
Create "PI:PROUT045" As 13 buffsize 37000

Print "Pipe Abierto Exitosamente "+pipe$
wait ; 2500

End Sub 

!--- Bloque Principal

On Error GoTo RUN.ERROR
Clears 
Print "Grupo Retail Ltda "
Print "Simulador Respuesta PIPE MOTOR PROMOCIONES NUO "
Print "-----------------------------------------------------"

TS.TEMP5I4 = PRSCINT																								! Direccion en Memoria
Print "Offset Memory Pipe "+Str$(TS.TEMP5I4)


While (1)																															    	! Ciclo Infinito
   Call PRSCCLS
   TS.TEMP5I4 = PRSCINT																								      ! Direccion en Memoria
   Print "Offset Memory Pipe "+Str$(TS.TEMP5I4)
   If ( TS.TEMP5I4 = 0 ) Then Begin																		      ! Falla Proceso
    Call Visor.and.borrar("FAIL PIPE MEMORY   :" + "Error :"+str$( TS.TEMP5I4 ) ) ! Msg Alerta
    Stop 
   EndIf            																								        !
   
   !Call Crea.H
   Call Crea.T
   
   Npipe$ = "H"
   TS.TEMP1$ = "0216900450011230923131533000001012441USD99999999999999999990000000010000000200050000000001000000000047001001111050001000304003790208440001000010000000000003691002424020002000208410435"
   TS.TEMP1$ = TS.TEMP1$ + "030010002000010000000000001401002424060001000107861048632140003000010000000000004761002424010002000407861048604060004000010000000000005981002424010002000207862104121590005"
   TS.TEMP1$ = TS.TEMP1$ + "00001000000000000170100111109000600040786210412390000600001000000000000149100111109000600010786210412257000700001000000000000289100111109000600040786210412361"
   TS.TEMP1$ = TS.TEMP1$ + "000800001000000000000280100111109000600010786210412426000900001000000000000286100111109000600040786210412331001000001000000000000149100111109000600010786210412268001100001"
   TS.TEMP1$ = TS.TEMP1$ + "000000000000156100111109000600030786212119063001200001000000000000229100111109000600040786212119051001300001000000000000229100111109000600040786210412333001400001200000000"
   TS.TEMP1$ = TS.TEMP1$ + "000149100111109000600030786212119055001500001000000000000229100111109000600010786210412332001600001000000000000149100111109000600040786212119059001700001000000"
   TS.TEMP1$ = TS.TEMP1$ + "000000229100111109000600040786109113149001800001000000000000285100111121000300010786102559234001900001000000000000105100111104000100010770210300111002000001000000000000682"
   TS.TEMP1$ = TS.TEMP1$ + "100111109000600030770210301502002100001000000000000437100111109000600040786100520083002200001000000000000450100111108000300010786100520061002300001000000000000090100111108000"
   TS.TEMP1$ = TS.TEMP1$ + "400030786100520113002400001000000000000256100111108000400040786100520003002500001000000000000116100111108000300010786211229004002600001000000000000143100242"
   TS.TEMP1$ = TS.TEMP1$ + "402000500020779325347160002700001000000000000464100242402000300030786211229118002800001000000000000213100242401000500010770221304087002900001000000000000079100242406000100030"
   TS.TEMP1$ = TS.TEMP1$ + "786211229007003000001200000000000149100242402000500010786211229006003100001200000000000150100242402000500010786104860373003200001200000000000404100242401000600070786104863921"
   TS.TEMP1$ = TS.TEMP1$ + "003300001200000000001035100242401000200030786104860407003400001200000000000598100242401000200020786104860271003500001200000000001439100242401000200020786"
   TS.TEMP1$ = TS.TEMP1$ + "104860242003600001200000000000404100242401000600060786104860402003700001200000000000476100242401000200040786104860148003800001200000000000476100242401000200040"
   Print "Grabando requerimiento en INPIPE "+TIME$
   
   !write #12; TS.TEMP1$
   
   Print "QUIETO AHI.... "
   Input "Otro Proceso ENTER o CTRL-C Cancelar ";TS.TEMP1$
   
   t% = 10
   While T% > 0
     Print "Esperando Respuesta..."
     Wait ; 1000
     Cerror% = -1
     read form "C5"; #13; s$
     If Cerror% = -1 Then Begin 
     	  Print "Longitud trama "+S$ 
     	  m$ = "C" + s$
     	  read form m$; #13 ; s$
     	  Print "Rta:"+s$
     	  Print "TERMINO "+TIME$
     	  GoTo termina
     endif Else T% = T% -1 
     	
   Wend 
   TERMINA:
   
   Close 12
   Close 13
      
Wend    																																	  ! Fin Ciclo

Stop 


RUN.ERROR:
  Cerror% = 0
  If ERR = "EF" Then Resume 
  	
  Call VISOR.AND.BORRAR("ERROR DE APLICACION "+ERR+" SESS:"+STR$(ERRF%))
Stop 
