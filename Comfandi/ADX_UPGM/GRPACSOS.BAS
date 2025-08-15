!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRPACSOS.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Aplicacion beneficios afiliados  *
!                plan complementario sos-comfandi *
!**************************************************
!Observaciones:
! Version 1.0 - 21/Jul/2017

%ENVIRON T		                          																			! Ambiente de terminal

Integer*1 Global Gr.PacSos.Ok%, Gr.PacSos.Ctl% 																!
Integer*2 Global Gr.PacSos.Key%																								!
String    Global Gr.PacSos.Tip$, Gr.PacSos.Tip2$, Gr.Lcl.TipClte$,           \!
                 Gr.Lcl.Tipifica$																							!

%INCLUDE EAMTSWKG.J86          																								! working storage
%INCLUDE EAMTSEVA.J86          																								! Variables Cliente Frecuente
%INCLUDE RECATSSU.011        																									! Rutinas Genericas Grupo Retail

Sub GRPACSOS(XOPT%) Public																							    	! Captura PAC SOS
Integer*4 Xopt%, GrG%                                                         !

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                       ! Carga de parametros
  Gr.PacSos.Ok%  = 0                                                          ! Proyecto Apagado
  Gr.PacSos.Tip$ = ""																													!
  Gr.PacSos.Key% = 0 																													!
  TS.ER.RETURN = -1																													  ! Ctrl Errores                     
  OPEN "R::$SCNTRL" AS 94					  		  																	  ! Apertura archivo parametrizacion 
  If TS.ER.RETURN <> -1 Then BEGIN                                            !
  	 Call VISOR.AND.BORRAR("ERROR OPEN ARCHIVO PROYECTOS COMFANDI  ")
  	 Exit Sub 
  ENDIF 
  IF END #94 THEN UE.FIN.PACSOS         																	    ! Si es EOF                        
  While (1)															  																    ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																				! Lectura registro                 
        IF TS.TEMP1$ = "[PAC SOS]" Then Begin		   		   											! Promo Plan Complementario
         Read #94; TS.TEMP1$																									! Lectura registro                 
         Gr.PacSos.Ok%   = Val(Mid$(TS.TEMP1$,30,2))   				    					  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						  ! Lectura registro                 
         Gr.PacSos.Tip$  = Mid$(TS.TEMP1$,30,20)           				            ! Lista tipificaciones
         Read #94; TS.TEMP1$     																						  ! Lectura registro                 
         Gr.PacSos.Key%  = Val(Mid$(TS.TEMP1$,30,03))      				            ! Tecla motora secuencia
         TS.TEMP1$ = ""																												!
         TS.TEMP2$ = ""
         For GrG% = 1 To Len(Gr.PacSos.Tip$) STEP 2			    			  					! Hasta longitud cadena
             TS.TEMP1$ = TS.TEMP1$ + "TC"+MID$(Gr.PacSos.Tip$,GrG%,2)         ! Lista tipificaciones
             TS.TEMP2$ = TS.TEMP2$ + "/"+MID$(Gr.PacSos.Tip$,GrG%,2)          ! Lista tipificaciones
         Next GrG%																														!
         Gr.PacSos.Tip$ = TS.TEMP1$			  																		! Entrega tipificaciones
         Gr.PacSos.Tip2$ = TS.TEMP2$		  																		! Entrega tipificaciones
         GoTo UE.FIN.PACSOS 																								  ! Sale del ciclo de carga          
       Endif                                                                  !
   Wend                                                                       !
   UE.FIN.PACSOS:                                                             !
     Close 94																																  ! Cierra archivo
   If Gr.PacSos.Ok% = -1 Then                                                \! Proyecto Activo
      Call U.IMPRIME("MODULO DSCT PAC SOS    ON 21/Jun/2017",6100H) Else     \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO DSCT PAC SOS   OFF 21/Jun/2017",6100H)           ! Msg Proyecto Cargado
EndIf 

If Gr.PacSos.Ok% <> -1 Then Exit Sub                                          ! Si proyecto apagado

!--- EAMTSU02.J86
If Xopt% = 2 Then Begin                                                       ! Inicializacion de trx
	 Gr.PacSos.Ctl% = 0 																												! Control registro
EndIf 																																				! Fin EAMTSU02

!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                      ! Secuencias de tecleo
 If TS.IO.MOTORKEY = Gr.PacSos.Key% Then Begin 																! Captura tecla Pac SOS
 	If Gr.PacSos.Ctl%  = -1 Then Begin																					! Si ya capturado proceso
 		 Call VISOR.AND.BORRAR("PROCESO YA CAPTURADO ANTERIOMENTE /Borrar")				! Msg alerta cajero
 		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																	! Anula secuncia registro
 		 TS.IO.MOTORKEY = 73																											! Simula tecla borrar
 		 Exit Sub 
 	EndIf
 	If USER.FBACT.READ = 0 Then Begin 																		      ! Cliente no capturado
 		 Call VISOR.AND.BORRAR("DEBE INGRESAR ANTES  VECINO FIEL /Borrar")				! Msg alerta cajero
 		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																	! Anula secuncia registro
 		 TS.IO.MOTORKEY = 73																											! Simula tecla borrar
 		 Exit Sub 
 	EndIf																																				! Fin cliente no capturado
 	Gr.PacSos.Ctl%  = -1																												! Proceso ya capturado
 	Gr.Lcl.TipClte$ = Gr.Lcl.TipClte$ + Gr.PacSos.Tip$											    ! Adiciona Lista tipificaciones
 	Gr.Lcl.Tipifica$ = Gr.Lcl.Tipifica$ + Gr.PacSos.Tip2$												! para registro promociones PAC SOS
  Call VISOR.AND.BORRAR("PROCEDA AL REGISTRO  DE ITEMS    /Borrar")						! Msg alerta cajero
  Call VISOR.AND.BORRAR(Gr.Lcl.Tipifica$)						                          ! Msg alerta cajero
  Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																		! Anula secuncia registro
  TS.IO.MOTORKEY = 73																													! Simula tecla borrar
  Exit Sub 
 	
 EndIf 																																				! Fin aplicacion motorkey
  	
EndIf 																																				! Fin EAMTSU14

End Sub 
