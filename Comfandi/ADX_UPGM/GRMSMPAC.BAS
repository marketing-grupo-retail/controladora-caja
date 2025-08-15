!**************************************************
!Empresa       : Grupo Retail Ltda - Colombia     *
!Programa      : GRMSMPAC.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Subsidio M.P.A.C          *
!**************************************************

%ENVIRON T

Integer*1 Global Gr.Mpac.Act%,  		   \! Si proyecto activo
                 Gr.Mpac.Trx%    		 		! Si remesa en registro 
                 
Integer*2 Global Gr.Mpac.Sec%    		 		! Tecla para Secuencia

String    Global Gr.Mpac.Benef$,       \! ID del beneficiario
                 Gr.Mpac.TipVar$,      \! Tipo y variedad definida
                 Gr.Mpac.Naut$,        \! Numero autorizacion
                 Gr.Mpac.Trama$,       \! Trama original mensaje
                 Hora.Mundial$,        \! Fecha y hora de trx
                 Gr.Mpac.Vlr$           ! Valor del subsidio
                 
%INCLUDE ADX_UPGM:EAMTSCVA.J86
%INCLUDE ADX_UPGM:EAMTSWKG.J86          ! working storage
%INCLUDE ADX_UPGM:EAMTRANS.j86          ! 
%INCLUDE ADX_UPGM:EAMTERMS.J86          ! 
%INCLUDE ADX_UPGM:EAMTOPTS.J86	        ! Variables de la terminal options
%INCLUDE ADX_UPGM:EAMTSXHC.J86          ! Rutinas de Assembler
%INCLUDE ADX_UPGM:EAMASMRT.J86          ! Rutinas de Assembler

%INCLUDE RECATSSU.011          					! RUTINAS GENERICAS APLICACION

SUB TSTCEC01 EXTERNAL          ! tender cashing
END SUB                        !

Sub TSCSEC10 EXTERNAL          ! non sales initialization
END SUB                        !

Function FORMAT.AMOUNT(AMT1) EXTERNAL
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub TSHIECET External 
End Sub 

Sub MPAC.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:MP" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
			Open X.FILE$ AS 56 Append
			If TS.ER.RETURN <> -1 Then Begin    ! Si no existe
			   TS.ER.RETURN = -1
				 CREATE X.FILE$ AS 56
         If TS.ER.RETURN <> -1 Then Begin 
            Call VISORES4690(1,"ERROR EN CREACION","DE AUDITORIA ",1500,"L")
            Exit Sub 
         EndIf 
			EndIf 
			X.Finr$ = Chr$(13) + Chr$(10)
			X.BUFF$ = "["+X.SALE$+"]"+"MSG:"+X.ENVIA$
			X.Len% = Len(X.BUFF$)						  				  								          ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						          ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					        ! Graba registro
			X.BUFF$ = "["+X.RTA$+"]"+"RTA:"+X.LLEGA$                              !
			X.Len% = Len(X.BUFF$)						  				  								          ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						          ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					        ! Graba registro      
			Close 56
End Sub 

Function COBRO.SUBSIDIO.MPAC
Integer*1 COBRO.SUBSIDIO.MPAC																								!
String XR$, Xtemp4$, Xtrama$,Xsnd$, Xfin$, Xcedulas$												!
String Xbuffer$
   Call VISORES4690(1,"VALIDANDO SUBSIDIO","ESPERE POR FAVOR ...",0,"L")					! Msg Operador
   COBRO.SUBSIDIO.MPAC = 0  																				        !
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora del requerimiento
   Xbuffer$ = Right$("                 "+Gr.Mpac.Benef$,18)                 ! ID del beneficiario
   XTemp4$ = Armar.Trama.Msg("17","01",Xbuffer$,"00","0001","123456")       ! Armar trama MSG

   Gr.Mpac.TRama$ = Right$("0000"+TS.STORE$,4)                  + 	       \! Numero del almacen
                    Right$("000000"+Str$(SL.HD.TERMINAL),6)     + 	       \! Numero de terminal
                    Unpack$(TS.OPER$)                           +          \! Numero de operador
                    Right$("000000"+Str$(SL.HD.TRANSNUM + 1),6) + 	       \! Numero de transaccion
                    Hora.Mundial$                               +          \! Fecha y Hora de la operacion AAMMDDHHMMSS
                    Xbuffer$ 								                                ! ID del beneficiario

   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)         ! Ejecuta Requerimiento
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call MPAC.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                       ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   	  COBRO.SUBSIDIO.MPAC = 0 		 																	        ! Error del proceso
   	  Exit Function
   EndIf
   XTEMP4$ = Mid$(XTrama$,12,2)	           																  ! Valida rta entregada
   If XTemp4$ = "00" Then Begin 																					  ! Rta Satisfactoria
       Gr.Mpac.Vlr$ = Str$(Val(Mid$(XTrama$,54,12)))                        ! Valor del subsidio
       If Val(Gr.Mpac.Vlr$) <= 0 Then Begin																	! Error en Valor Pago
   		    Call VISOR.AND.BORRAR("VALOR DEL SUBSIDIO  NO VALIDO")		        ! Presenta Msg Error
   		    COBRO.SUBSIDIO.MPAC = 0																				    !
          Exit Function 
       EndIf
       Gr.Mpac.Naut$ = Mid$(XTrama$,66,6)                                   ! Numero de autorizacion asignado
       Xbuffer$  = Pack$(Gr.Mpac.Benef$)                          +        \! ID beneficiario
                   ":"+Pack$(Gr.Mpac.Vlr$)                        +        \! Valor subsidio entregado
                   ":"+(Gr.Mpac.Naut$)                            +        \! Numero autorizacion
                   ":"+Gr.Mpac.TRama$                             +        \! Trama para conciliacion
                   ":"+Pack$("00")                                          ! Filler 
       Call Grabacion.String.Usuario2("20140529",Xbuffer$)                  ! Graba String usuario  
   	   COBRO.SUBSIDIO.MPAC = -1		 																	        !
   EndIf Else Begin 																												!
   		 Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   		 COBRO.SUBSIDIO.MPAC = 0																					    !
   EndIf 																																		!
End Function

Function CONFIRMAR.MPAC
Integer*1 CONFIRMAR.MPAC																								    !
String XR$, Xtemp4$, Xtrama$,Xsnd$, Xfin$, Xcedulas$												!
String Xbuffer$
   CONFIRMAR.MPAC = 0  																				              !
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora del requerimiento
   Xbuffer$ = Gr.Mpac.TRama$															                  ! Trama del cobro
   XTemp4$ = Armar.Trama.Msg("17","02",Xbuffer$,"00","0001","123456")       ! Armar trama MSG
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)         ! Ejecuta Requerimiento
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call MPAC.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                       ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   CONFIRMAR.MPAC = -1 																				              !
   Exit Function 
   
   If Xtemp4$ <> "00" Then Begin 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   	  CONFIRMAR.MPAC = 0 		 																	              ! Error del proceso
   	  Exit Function
   EndIf
   XTEMP4$ = Mid$(XTrama$,12,2)	           																  ! Valida rta entregada
   If XTemp4$ = "00" Then Begin 																					  ! Rta Satisfactoria
   	   CONFIRMAR.MPAC = -1		 																	            !
   EndIf Else Begin 																												!
   		 Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   		 CONFIRMAR.MPAC = 0																					          !
   EndIf 																																		!

End Function 


Sub Reclamo.Mpac  																													! Cobro subsidio
Integer*2 Xcont%, XK%, XCant%
String Xmsg1$, Xmsg2$, Xdata$, XvlrTrx$
                                                                                                    
         TS.TEMP5I2 = 0                    													        ! Si proceso correcto
         TS.TEMP5I2 = COBRO.SUBSIDIO.MPAC																  	! Confirma movimiento
      	 IF TS.TEMP5I2 <> 0 Then Begin																			! Si proceso correcto
      	 	  Call Format.Amount( Val(Gr.Mpac.Vlr$) )                         ! Formatea valor a entregar
      	    Call TSHIECET
      	    Call VISORES4690(2,"VALOR A ENTREGAR","$"+TS.TEMP1$,0,"L")
      	    Call VISORES4690(1,"VALOR A ENTREGAR","$"+TS.TEMP1$,1500,"L")
      	    
            TS.TEMP5I2 = CONFIRMAR.MPAC                                     ! Confirma movimiento pago subsidio
            Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 												! Init vectores de carga
            TS.IO.DATA$(3) = Right$(Gr.Mpac.TipVar$,1)                      ! Variedad de Pago
            TS.IO.KEYS(3)  = 78                                             ! Tecla Slash
            TS.IO.DATA$(7) = Gr.Mpac.Vlr$																		! Valor a entregar
            TS.IO.DATA$(9) = Gr.Mpac.Benef$																	! ID del Beneficiario
            TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.Mpac.TipVar$,1))             ! Tipo de Pago
            TS.IO.MOTORKEY = TS.IO.KEYS(7)																	!
         EndIf ELSE Begin 																									! Falla Confirmacion
            Call Gr.Init.Trx																								! Init Trx
            TS.IO.STATE    = 10            																  ! PREPARE FOR NORMAL SALES STATE
         EndIf																															!
      
End Sub 



Sub SUBSIDIO.MPAC(Xopt%) Public
Integer*4 Xopt%

!--- EAMTSU07.J86
If XOPT% = 07 Then Begin																										! Carga de opciones
  Gr.Mpac.Act% = 0 																											    ! Init proyecto activo
  TS.TS11WERR$ = ""																													! Control errores
  OPEN "R::$SCNTRL" AS 94 UNLOCKED NOWRITE NODEL												    ! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR APERTURA PARAMETROS MPAC   ")             !
	   Gr.Mpac.Act% = 0																											  ! Init proyecto activo  	 
  	 Exit Sub																																!
  EndIf 
  If End #94 Then UE.FIN.MPAC           																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[SUBSIDIO MPAC]" Then Begin		      	 					    ! Parametros MPAC
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Mpac.Act% = Val(Mid$(TS.TEMP1$,30,2))      			    					  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Mpac.Sec% = Val(Mid$(TS.TEMP1$,30,3))      			    					  ! Tecla Motora Asignada
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Mpac.TipVar$ = Mid$(TS.TEMP1$,30,2)       			    					  ! Tipo y variedad definida
         GoTo UE.FIN.MPAC                                                   ! Sale del proceso
        EndIf																																!
  Wend 																																		  ! Fin recorrido archivo
  UE.FIN.MPAC:
    Close 94 
    If Gr.Mpac.Act% = -1 Then                                              \! Proyecto Activo
      Call U.IMPRIME("MODULO SUBSIDIO MPAC   ON 29/May/2014",2100H) Else   \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO SUBSIDIO MPAC  OFF 29/May/2014",2100H)         ! Msg Proyecto Cargado
    
EndIf

If Gr.Mpac.Act% = 0 Then Exit Sub 																					! Si proyecto apagado

!--- EAMTSU02.J86
If XOPT% = 02 Then Begin																										! Inicio de transaccion
   Gr.Mpac.Vlr$	= ""																												! Valor a entregar
   Gr.Mpac.Benef$ = ""																											! ID del Beneficiario
   Gr.Mpac.TRama$ = ""                                                      ! Trama del mensaje
EndIf


!--- EAMTSU14.J86
If XOPT% = 14 Then Begin																										! En secuencia de tecleo
	 
	 If TS.IO.MOTORKEY = Gr.Mpac.Sec% Then Begin														! Si tecla de secuencia

	 	  If TS.INTRX Then Begin  																							! Esta en plena trx de venta
	 	  	 Call VISOR.AND.BORRAR("NO SE PERMITE PAGO SUBSIDIO EN UNA TRX")           ! Msg Alerta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
         TS.IO.MOTORKEY = 73																									!
         Exit Sub 																											    ! Termina proceso
	 	  EndIf
	 	  If TS.TRAINING Then Begin																							! Si en entrenamiento
	 	  	 Call VISOR.AND.BORRAR("PROCESO NO PERMITIDO EN ENTRENAMIENTO")            ! Msg Alerta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
         TS.IO.MOTORKEY = 73																									!
         Exit Sub 																											    ! Termina proceso
	 	  EndIf																																	! Fin Salida
      If TS.STANDALONE Then Begin 						 	                            ! Proceso fuera de linea
	 	  	 Call VISOR.AND.BORRAR("PROCESO NO PERMITIDO FUERA DE LINEA  ")            ! Msg Alerta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
         TS.IO.MOTORKEY = 73																									!
         Exit Sub 																											    ! Termina proceso
      EndIf
      If (TE.TR.AMT(1) + 50000) >= To.CDLIMIT2 Then Begin                   ! PROCESO CERCA A RECOGIDA
   	      Call VISOR.AND.BORRAR("REALICE PROCESO DE RECOGIDA CAJERO AHORA") ! Msg de alerta
   	      Call VISOR.AND.BORRAR("PAGO SUBSIDIO   CANCELADO")								! Msg de alerta
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													!
          TS.IO.MOTORKEY = 73																								!
          Exit Sub 																											    ! Termina proceso
      EndIf 	

      If (TS.IO.KEYS(1) = 70) Then Begin                                    ! Si secuencia anulacion
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													!
          TS.IO.MOTORKEY = 0 																								!
          TS.GUIDANCE    = 1003                                             ! mensaje para revisar secuencia
          Exit Sub 																											    ! Termina proceso
      EndIf

          TS.TEMP5$ = ASIC.DATOS$("INGRESE CEDULA DEL","BENEFICIARIO..")    !                
          If TS.TEMP5$ = "E" Then BEGIN 																		! Proceso cancelado   
          	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								!                     			
             Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!                     			
             TS.IO.MOTORKEY = 73																						!                     			
             Exit Sub 																											!                     			
          EndIf 																														!                     			

          TS.TEMP4$ = ASIC.DATOS$("REINGRESE CEDULA DEL","BENEFICIARIO..")  !                
          If TS.TEMP4$ = "E" Then BEGIN 																		! Proceso cancelado   
          	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								!                     			
             Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!                     			
             TS.IO.MOTORKEY = 73																						!                     			
             Exit Sub 																											!                     			
          EndIf 																														!                     			
                                                                                                        

      If Val(TS.TEMP4$) <> Val(TS.TEMP5$) Then Begin												! Datos no validos
       	 Call VISOR.AND.BORRAR("DATOS CAPTURADOS NO SON IGUALES")				    !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Exit Sub 																											    !
      EndIf

      Gr.Mpac.Benef$ = TS.TEMP5$																					  ! ID capturado
      Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 									  					! Init vectores de carga        
      TS.IO.DATA$(5) = "1"																									! Secuencia cambio a efectivo          
      TS.IO.KEYS(5)  = 61																										!                                        
      TS.IO.MOTORKEY = 61																										!                                        
      Call TSTCEC01                                                         !                        	     
      Call Reclamo.Mpac                                            					! Reclamo subsidio
      Exit Sub                                                              !                         	     

   EndIf																																		! Fin secuencia remesas
   
EndIf																																				! Fin Eamtsu14



End Sub 
