!**************************************************
!Empresa       : Grupo Retail Ltda - Colombia     *
!Programa      : GRMBNELF.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Control Bonos Electrofiesta
!**************************************************
! Modificaciones:

%ENVIRON T

Integer*1 Global Gr.BnElec.Act%,																					 \! Control proyecto
                 Gr.BnElec.Apl%,                                           \! Pago Bono Aplicado
                 Gr.Bnsyd.Apl%,                                            \! Bonos electro SYD
                 Asc.Pay.Impr%                                              ! Control nro trx 
Integer*4 Global Asc.Tmp.Apun%        																			! Apuntador Lectura Strings                 
String    Global Gr.Lcl.Clte$																								! ID cliente


%INCLUDE ADX_UPGM:EAMTSEVA.J86   																					  ! Variables EMS
%INCLUDE ADX_UPGM:EAMTSWKG.J86                                              ! working storage                 
%INCLUDE ADX_UPGM:EAMTRANS.j86                                              !                                 
%INCLUDE ADX_UPGM:EAMTERMS.J86                                              ! 
%INCLUDE ADX_UPGM:EAMTSXHC.J86                                              ! Rutinas de Assembler            
%INCLUDE ADX_UPGM:EAMASMRT.J86                                              ! Rutinas de Assembler            
                                                                                                              
%INCLUDE RECATSSU.011          					                                    ! RUTINAS GENERICAS APLICACION    

Sub  TSCSEC03 External
End Sub 

Sub BONOELECTRO.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$) Public 
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:BE" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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

Sub CONFIRMAR.BONOELECTRO
String Xtv$, Xtemp4$, Xtrama$,Xsnd$, Xfin$, Xcedulas$												!
String Xbuffer$, Xpago$
Integer*4 XPago%
  Call Visores4690(1,"ACTUALIZA BONO ELECTRO","ESPERE POR FAVOR..",0,"L")   ! 
  Xpago% = 0
  For I% = 1 TO SL.END                                                      ! FOR ALL StringS                   
    Asc.Tmp.Apun% = 3
    H$ = READ.SL.STR$(I%)                                                   ! GET String                        
    If LEN(H$) > 5 Then Begin                                              \! ASSURE GOOD String                
     If ASC(H$) = 5 Then Begin                                              ! FORMA PAGO String                 
    	 Xtv$   = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                            ! Tipo y variedad de pago registrado
  	 	 Xpago$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                          	! Valor forma de pago               
  	 	 If Xtv$ = "34" Then Begin																						!
          Xpago% = XPago% + Val(Xpago$)                                     ! Monto otros pagos
       EndIf																																!
     EndIf																							                    ! FIN  FORMA PAGO String            
     If ASC(H$) = 6 Then Begin                                              ! FORMA PAGO String                 
    	 Xtv$   = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                            ! Tipo y variedad de pago registrado
  	 	 Xpago$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                          	! Valor forma de pago               
  	 	 If Xtv$ = "34" Then Begin																						!
          Xpago% = XPago% - Val(Xpago$)                                     ! Monto otros pagos
       EndIf																																!
     EndIf																							                    ! FIN  FORMA PAGO String            
    EndIf																																		! 
  Next I%                                                                   ! NEXT String                       
  If XPago% <= 0 Then Exit Sub 																							! No hay pagos para reportar
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora del requerimiento
   Xbuffer$ = Right$("               "+Gr.Lcl.Clte$,15) +                  \! Id del cliente
              Right$("           "+Str$(Xpago%),11)													! Monto del bono
   Asc.Pay.Impr% = 0                                                        ! Ajusta numero trx para seguimiento
   XTemp4$ = Armar.Trama.Msg("20","51",Xbuffer$,"00","0001","123456")       ! Armar trama act bono electro
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)         ! Ejecuta Requerimiento
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call BONOELECTRO.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                    ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   EndIf
End Sub  

Sub Validando.BonoElectro
String Xsnd$, Xfin$, Xbuffer$, XTemp4$, Xtrama$
  If TS.INTRX Then Begin                                                    ! Si esta dentro de una trx venta
  	 If USER.FBACT.READ = 0 Then Begin																		  ! Cliente no capturado
  	    Call VISOR.AND.BORRAR("NO SE HA CAPTURADO  VECINO FIEL...")         !
  	    TS.TEMP1I1 = 0																											! Reporta falla en el proceso
  	 	  Exit Sub 
  	 EndIf
     If Val(Gr.Lcl.Clte$) <> Val(TS.IO.DATA$(9)) Then Begin                 ! Dato CC diferente a vecino fiel
     	  Call VISOR.AND.BORRAR("DATO NO ES SIMILAR A VECINO FIEL..")         !
  	    TS.TEMP1I1 = 0																											! Reporta falla en el proceso
  	 	  Exit Sub 
     EndIf																																	! 
     If TS.IO.KEYS(1) = 70 Then Begin																				!
     	  TS.IO.DATA$(9) = Gr.Lcl.Clte$
     	  Gr.BnElec.Apl% = 0
     	  Exit Sub 
     EndIf
     Call VISORES4690(1,"VALIDA CLTE BONOS ","ESPERE POR FAVOR ...",0,"L")	! Msg Operador
     Asc.Pay.Impr% = 0																											! Aumenta contador de trx
     Xsnd$ = DATE$ +":"+ Time$                                              ! Fecha y hora del requerimiento
     Xbuffer$ = Right$("               "+Gr.Lcl.Clte$,15)                   ! ID del cliente 
     XTemp4$ = Armar.Trama.Msg("20","50",Xbuffer$,"00","0001","123456")     ! Armar trama MSG
     XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)       ! Ejecuta Requerimiento
     Xfin$ = DATE$ +":"+ Time$                                              ! Fecha y hora rta del requerimiento
     Call BONOELECTRO.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)              ! Rastreo movimiento
     XTEMP4$ = Valida.Rta(XTrama$)																		      ! Valida rta entregada
     If Xtemp4$ <> "00" Then Begin 																					! Si falla en proceso comunicacion
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
  	  TS.TEMP1I1 = 0																											  ! Reporta falla en el proceso      
   	  Exit Sub 																															!
     EndIf																																	!
     XTEMP4$ = Mid$(XTrama$,12,2)	         																  ! Valida rta entregada
     If XTemp4$ <> "00" Then Begin 																				  ! Rta No Satisfactoria
     	  Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									        ! Presenta Msg Error
  	    TS.TEMP1I1 = 0																										  ! Reporta falla en el proceso      
   	    Exit Sub 																														!
     EndIf																																	!
     TS.TEMP1I1 = -1																										    ! Reporta proceso exitoso
     Gr.BnElec.Apl% = -1																										! Pago aplicado
  EndIf Else Begin																													! Fuera de una operacion de venta
  	 Call VISOR.AND.BORRAR("FORMA PAGO NO VALIDA PARA NO VENTA")            !
  	 TS.TEMP1I1 = 0																													! Reporta falla en el proceso
  EndIf																																			! fin operacion de venta
End Sub 																																		! Fin validación bonos 

Sub BONOS.ELECTRO(Xopt%) Public																						  ! Modulo bonos electrofiesta
Integer*4 Xopt%, Xj%

!--- EAMTSU07.J86
If XOPT% = 07 Then Begin																										! Carga de opciones
  Gr.BnElec.Act% = 0 																										    ! Init proyecto activo
  TS.TS11WERR$ = ""																													! Control errores
  OPEN "R::$SCNTRL" AS 94 UNLOCKED NOWRITE NODEL												    ! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR PARAMTROS BONOS ELECTRO")                 !
	   Gr.BnElec.Act% = 0																										  ! Init proyecto activo  	 
  	 Exit Sub																																!
  EndIf 
  If End #94 Then UE.FIN.BNELECTRO      																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[BONOS ELECTROFIESTA]" Then Begin		 	  		        ! Parametros bonos electrofiesta
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.BnElec.Act% = Val(Mid$(TS.TEMP1$,30,2))      			    					! Proyecto Activo 0. No -1 Si
         GoTo UE.FIN.BNELECTRO                                              ! Sale del proceso
        EndIf																																!
  Wend 																																		  ! Fin recorrido archivo
  UE.FIN.BNELECTRO:
    Close 94 
    If Gr.BnElec.Act% = -1 Then                                            \! Proyecto Activo
      Call U.IMPRIME("MODULO BONOS ELECTR    ON 13/Sep/2021",2100H) Else   \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO BONOS ELECTR   OFF 13/Sep/2021",2100H)         ! Msg Proyecto Cargado
    
EndIf

If Gr.BnElec.Act% = 0 Then Exit Sub 																				! Si proyecto apagado

!--- EAMTSU02.J86
If XOPT% = 02 Then Begin																										! Al inicio de una trx
   Gr.BnElec.Apl% = 0
EndIf																																				! Fin eamtsu02


!--- EAMTSU20.J86
If XOPT% = 20 Then Begin																										! En impresion de tiquete
 If Not TS.TRAINING Then Begin                       												! Si no en entrenamiento
  If TS.INTRX Then                                  											 \! Si esta en una compra
    If TS.LINETYPE = 6 And                                                 \! Store Data y Transacc
       TS.LINEDATA = 1 Then Begin                    											  ! Fecha, Hora, etc
        If TS.TENDERED (0) <> 0 Or                                         \! Si hay pagos
           TS.TRX.STATUS <> 100 Then Begin                                  ! y NO hay VOID en curso
           If Gr.BnElec.Apl% = -1	Then Begin																! Init variable
           	   Call CONFIRMAR.BONOELECTRO                                   ! Almacena bono electro
           	   Gr.BnElec.Apl% = 0                                           ! proceso confirmado
           EndIf																														!
        EndIf
     EndIf
  EndIf
EndIf 																																			! Fin Eamtsu02

!--- EAMTSU32.J86
If XOPT% = 32 Then Begin																										! Validacion forma de pago
 If (TS.IO.DATA$(3) = "4" And TS.IO.KEYS(7)  = 93 ) Then Begin              ! Si pago bono electrofiesta
 	  TS.TEMP1I1 = 0																													! Control de errores
 	  If Gr.Bnsyd.Apl% = -1 Then Exit Sub                                     ! Es un bono SYD
    Call Validando.BonoElectro																							! Validando forma de pago
    If TS.IO.KEYS(1) = 70 Then Exit Sub 
   	If TS.TEMP1I1 = 0 Then Begin 																		        ! Si falla el proceso
       TS.USER.RETURN = 99																									!
       TS.LINEDATA = 51                 																    ! "TARJETA INVALIDA"            
       TS.STACKERR(0) = 0                                                   ! manager's override required   
       TS.STACKERR(3) = 0                                                   ! item descriptor               
       TS.STACKERR(6) = 0                                                   ! put "M" on last display       
       TS.STACKERR(7) = -1                                                  ! indicate no printing required 
       TS.MO.REASON = 0                                                     ! invalid key with department   
       Call TSCSEC03       																							    !                               
       TS.IO.MOTORKEY = 0 																									!
       Exit Sub                                                             !
   	EndIf																																		! 
 EndIf																																			! fin pago
EndIf																																				! Fin Eamtsu32

End Sub																																			! Fin Modulo control cambio precio
