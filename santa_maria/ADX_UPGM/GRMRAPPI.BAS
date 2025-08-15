!**************************************************
!Empresa       : Grupo Retail Ltda - Colombia     *
!Programa      : GRMRAPPI.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Forma Pago Rappi          *
!**************************************************
! Modificaciones:

%ENVIRON T

String    Global Gr.Rappi.Tvp$,                                            \! Tipo y variedad de pago 
                 Gr.Rappi.Pin$,                                            \! Tipo y variedad de pago 
                 Gr.Rappi.PinDeUna$,                                       \! Pin reportado al cliente
                 Gr.Rappi.Motor$,                                          \! Tecla motora
                 Gr.Rappi.Motor2$,                                         \! Tecla motora
                 Gr.Rappi.Tmp1$,                                           \! Temporal 1
                 Gr.Rappi.Tmp2$,                                           \! Temporal 2
                 Gr.Rappi.Tmp3$,                                           \! Temporal 3
                 Gr.Rappi.Tmp4$,                                           \! Temporal 4
                 Gr.Rappi.AnulPin$,                                        \! Pin anular
                 Gr.Rappi.PtVta$                                            ! numero de caja asignado por deuna
String    Global Gr.Rappi.Pagos$(2)                                         ! Vector pagos capturados
Integer*1 Global Gr.Rappi.Act%,																					   \! Control proyecto
                 Gr.Rappi.AplAn%,                                          \! Anulacion pago Kupi
                 Gr.Rappi.Apl%,                                            \! Pago Bono Aplicado
                 Asc.Pay.Impr%                                              ! Control nro trx 
Integer*2 Global GR.BKRPGCNT(1),                                           \!
                 Gr.Rappi.Numtrx%                                           ! Nro de pagos recibidos
Integer*4 Global Gr.Rappi.Motora%,                                         \! Tecla procedimiento
                 Gr.Rappi.TVPOS%,                                          \!
                 Gr.Rappi.MaxTv%,                                          \! Maximo pago   tipo y variedad
                 Gr.Rappi.ChgTv%,                                          \! Maximo cambio tipo y variedad
                 GR.BKRPGAMT(1),                                           \!
                 Asc.Tmp.Apun%                                              ! Apuntador String
Integer*4 Global GR.TOTAL.DSC%																							!
Integer*1 Global Gr.Domi.Intrx%, 																					 \! Operacion domicilios
                 Gr.Domi.Delivery%                                          ! Codigo domiciliario
String    Global Gr.Domi.Pedido$                                            ! Numero del pedido
String    Global Gr.Rappi.Compra$,																				 \!
                 Gr.Rappi.Imptos$,               												   \!
                 Gr.Rappi.Dsctos$,																				 \!
                 Gr.Rappi.CajOri$,																				 \! Nro caja
                 Gr.Rappi.TrxOri$																						! Nro Trx 
                 
%INCLUDE EAMTOPTS.J86          																			        ! working storage
%INCLUDE EAMTSWKG.J86                                              					! working storage                 
%INCLUDE EAMTRANS.j86                                              					!
%INCLUDE JAVAGUIV.J86          																							! Variables java
%INCLUDE EAMTERMS.J86                                              					!
%INCLUDE EAMTSXHC.J86                                              					! Rutinas de Assembler            
%INCLUDE EAMASMRT.J86                                              					! Rutinas de Assembler            
                                                                                                              
%INCLUDE RECATSSU.011          					                                    ! RUTINAS GENERICAS APLICACION    
%INCLUDE JAVAGUIC.J86																												! RUTINAS AMBIENTE GRAFICO

Sub TSTCEC01 EXTERNAL                         ! tender cashing
End Sub                                       !

Sub  TSCSEC03 External
End Sub 

Sub TSBDEC01 EXTERNAL          ! balance due (normal/foodstamp)
End Sub                        !

Sub TSHIECET external					 ! Tono de alerta
End Sub

Function FORMAT.AMOUNT(AMT1) External 																			! Formateo de valores
 Integer*1 FORMAT.AMOUNT
 Integer*4 AMT1
End Function

!------------------------------------------------------------------------------
! Recibe datos desde desde teclado en modo alfanumerico
! msg1$, msg2$ mensaje para linea 1 y linea 2 del visor
! retorna una cadena donde el primer caracter indica
! "E" en caso de error, "P" dato ingresado (tecla PLU),
! "A" aborta el ingreso (tecla BORRAR o ANULAR)
! el segundo caracter "G" si la llave esta girada
! del tercer en adelante el valor ingresado
! si esta activo el indicador teclaAnular
! la tecla BORRAR limpia el texto ingresado y la tecla
! ANULAR aborta el ingreso
!------------------------------------------------------------------------------
Function ingresoTexto$( msg1$, msg2$ ) External
   String ingresoTexto$, msg1$, msg2$
End Function 

!-----------------------------------------------------------------------
! formatea un entero considerando 2 decimales, sin separador de miles
! separador decimal "."
! ejemplos 3 -> 0.03  123456789 -> 1234567.89
!-----------------------------------------------------------------------
Function formateaMonto( numero ) External
   String    formateaMonto
   Integer*4 numero
End Function 

Function LISTA.TRX.RAPPI External																					! Datos para envio de trx rappi
  String LISTA.TRX.RAPPI
End Function 

Sub RAPPI.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::GRLOG:RP" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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

Sub RAPPI.BUSCA.LIMITE.TV(UE.TIPO.VAR$)
  Integer*2 UE.I%
  Integer*1 UE.FOUND%
  String UE.TIPO.VAR$
  UE.I% = 1
  UE.FOUND% = 0 
  Gr.Rappi.MaxTv% = 0
  Gr.Rappi.ChgTv% = 0 
  While UE.I% <= TO.NUMTNDR AND NOT UE.FOUND%
    If TO.TENDOPTS(UE.I%,0) = VAL(LEFT$(UE.TIPO.VAR$,1)) AND  \
       TO.TENDOPTS(UE.I%,1) = VAL(RIGHT$(UE.TIPO.VAR$,1)) Then Begin 
       Gr.Rappi.MaxTv% = TO.TENDLIMITS(UE.I%,0)
       Gr.Rappi.ChgTv% = TO.TENDLIMITS(UE.I%,1)
       UE.FOUND%  = -1
       Gr.Rappi.TVPOS% = UE.I%
    EndIf Else UE.I% = UE.I% + 1
  Wend    
  UE.I% = 0
  For UE.I% = 0 To 7
      GR.BKRPGCNT(UE.I%) = To.NEGCNT(UE.I%)    
      GR.BKRPGAMT(UE.I%) = To.NEGAMT(UE.I%)    
  Next UE.I%
End Sub

Sub Validando.PagoRappi
String Xsnd$, Xfin$, Xbuffer$, XTemp4$, Xtrama$, Xfunc$, d$, Xtmp$,        \!
       XtrxAnul$	
Integer*1 XT%, Xfnd%, Xrpt%, Xnvc%
Integer*4 Xvlr%
     If Gr.Rappi.AplAn% = 0 Then Xfunc$ = "01" Else Xfunc$ = "03"           ! 
     Xrpt% = 4 : Xnvc% = 0
     Call VISORES4690(1,"VALIDA "+Gr.Rappi.Tmp3$,"ESPERE POR FAVOR ...",0,"L")	! Msg Operador
     Asc.Pay.Impr% = 0																											! Aumenta contador de trx
     Xsnd$ = DATE$ +":"+ Time$                                              ! Fecha y hora del requerimiento
     Xbuffer$ = ":" + LISTA.TRX.RAPPI																				! Lista de productos 
     Xbuffer$ = Gr.Rappi.Tmp3$ + Xbuffer$																		! Trama de la compra
     XTemp4$ = Armar.Trama.Msg("45",Xfunc$,Xbuffer$,"00","0001","123456")   ! Armar trama MSG
     XTrama$ = Rutina.Java("com.grpretail.mega.ecuador.sigui.actions.Core","execute", XTemp4$)       ! Ejecuta Requerimiento
     Xfin$ = DATE$ +":"+ Time$                                              ! Fecha y hora rta del requerimiento
     Call RAPPI.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                    ! Rastreo movimiento
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
     If Xfunc$ = "03" Then Begin																						! Proceso anulacion
     	  Gr.Rappi.PinDeUna$ = Mid$(Xtrama$,55,40)														! Trama autorizacion anulacion
     EndIf
     Gr.Rappi.Pin$ = Mid$(Xtrama$,54,19)                                    ! Nro ID trx Deuna
     TS.TEMP1I1 = -1																										    ! Reporta proceso exitoso
     Gr.Rappi.Apl% = -1																										  ! Pago aplicado
End Sub 																																		! Fin validación bonos 

Sub RECUPERA.TRX.RAPPI
String Xtmp$
Integer*2 XS%
 If Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "99" Then BEGIN   	   	              ! Si user data
  If Unpack$(MID$(SL.STR.ENTRY$,3,4)) = "2011" Then Begin                   ! Proyecto Rappi
    Asc.Tmp.Apun% = 3																												!
    Gr.Rappi.Numtrx% = Gr.Rappi.Numtrx% + 1																	! Total operaciones Deuna
    Xtmp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                   		! Numero de proyecto
    Xtmp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)           						! Punto de venta
    Xtmp$ = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)           						! Autorizacion
    Gr.Rappi.Pagos$(Gr.Rappi.Numtrx%,0) = Xtmp$                         		! Nro de autorizacion
    Xtmp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)           						! Valor compra
    Gr.Rappi.Pagos$(Gr.Rappi.Numtrx%,1) = Xtmp$                         		! Nro de autorizacion
    Xtmp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)           						! Signo operacion 
    Gr.Rappi.Pagos$(Gr.Rappi.Numtrx%,2) = Xtmp$                         		! Nro de autorizacion
    Gr.Rappi.Apl%   = -1																										! Pagos aplicados
    If Xtmp$ = "01" Then                                                   \!
     	  Xtmp$ = "PROCESO OPERACION RAPPI! VENTA" Else                      \!
     	  Xtmp$ = "PROCESO OPERACION RAPPI! ANULACION"												!
    Write #34 ; Xtmp$ + CHR$(10)																						!
    Write #34 ; "AUT: "+Gr.Rappi.Pagos$(Gr.Rappi.Numtrx%,0) + CHR$(10)			!
    TS.TEMP2I4 = Val(Gr.Rappi.Pagos$(Gr.Rappi.Numtrx%,1))										!
    Xtmp$ = formateaMonto(TS.TEMP2I4)																				!
    Write #34 ; "VALOR: "+Xtmp$ + CHR$(10)																	!
    XS% = 0																																	!
    For XS% = 1 To 9																												!
      Write #34 ; " " + CHR$(10)																						!
    Next XS%																																!
    Call U.CORTACR																													! Guillotina papel

  EndIf																																			! 
 EndIf 

End Sub 

Sub Almacena.Pedido.Rappi																										! Almacena pedido
String Xkey$																																!
  TS.ER.RETURN = -1																													! Control errores
  Open "R::TF:RPORDN" Keyed Recl 37 As 94																		! Archivo control pedido
  If TS.ER.RETURN <> -1 Then Begin																					! Si Error en proceso
  	 Call Visor.And.Borrar("ERROR EN GRABACION  PEDIDO RAPPI /Borrar")      !
  	 Call U.Imprime("ERROR GRAB. PEDIDO " + Gr.Rappi.Tmp3$,2100H)           ! Rastro auditoria 
  	 Exit Sub 																															! Sale del proceso
  EndIf																																			!
  TS.ER.RETURN = -1																													! Control Errores
  Xkey$ = Right$("                  "+Gr.Rappi.Tmp3$,18)                    ! Numero de pedido
  Gr.Rappi.Compra$ = Pack$(Right$("0000000000"+Gr.Rappi.Compra$,10))				!
  Gr.Rappi.Imptos$ = Pack$(Right$("0000000000"+Gr.Rappi.Imptos$,10))				!
  Gr.Rappi.Dsctos$ = Pack$(Right$("0000000000"+Gr.Rappi.Dsctos$,10))				!
  Gr.Rappi.CajOri$ = Pack$(Right$("0000"+TS.TERMINAL$,4))										! Nro caja
  Gr.Rappi.TrxOri$ = Pack$(Right$("0000"+Str$(Sl.Hd.Transnum + 1),4))				! Nro trx 
  Write Form "C18 3C5 2C2 ";#94;           								                 \! Graba numero de pedido
        Xkey$,                  																					 \! Numero de pedido
        Gr.Rappi.Compra$,																									 \! Valor compra
        Gr.Rappi.Imptos$,																									 \! Valor Imptos
        Gr.Rappi.Dsctos$, 																								 \! Valor dsctos
        Gr.Rappi.CajOri$,																									 \! Nro caja
        Gr.Rappi.TrxOri$																										! Nro Trx 
  Close 94																																	! Cierre archivo
End Sub 																																		!

Sub Anul.Pedido.Rappi     																									! Anulación pedido
String Xkey$, Xbuff$, Xsnd$, XTemp4$, Xtrama$, Xfin$, Xtotc$, XtotD$,      \!
       XtotT$

  TS.TEMP5I2 = 1                    													        			! Control del proceso
  TS.ER.RETURN = -1																													! Control errores
  Open "R::TF:RPORDN" Keyed Recl 37 As 94																		! Archivo control pedido
  If TS.ER.RETURN <> -1 Then Begin																					! Si Error en proceso
  	 Call Visor.And.Borrar("ERROR EN ANULACION  PEDIDO RAPPI /Borrar")      !
  	 Call U.Imprime("ERROR ANUL. PEDIDO " + Gr.Rappi.Tmp3$,2100H)           ! Rastro auditoria 
     TS.TEMP5I2 = 0                  													        			! Control del proceso  	 
  	 Exit Sub 																															! Sale del proceso
  EndIf																																			!
  TS.ER.RETURN = -1																													! Control Errores
  Xkey$ = Right$("                  "+Gr.Rappi.Tmp3$,18)                    ! Numero de pedido
  Read FORM "C18 3C5 2C2";#94 KEY Xkey$;         				                   \! Busca pedido
        Xkey$,                  																					 \! Numero de pedido
        Gr.Rappi.Compra$,																									 \! Valor compra
        Gr.Rappi.Imptos$,																									 \! Valor Imptos
        Gr.Rappi.Dsctos$,																									 \! Valor dsctos     
        Gr.Rappi.CajOri$,																									 \! Nro caja
        Gr.Rappi.TrxOri$																										! Nro Trx 
  If TS.ER.RETURN = -1 Then Begin																						! Pedido encontrado
     Xtotc$ = formateaMonto(Val(Unpack$(Gr.Rappi.Compra$)))									! Total de la compra
     XtotD$ = formateaMonto(Val(Unpack$(Gr.Rappi.Dsctos$)))									! Total de los descuentos 
     XtotT$ = formateaMonto(Val(Unpack$(Gr.Rappi.Imptos$)))									! Total de los impuestos 
     Xbuff$ = Gr.Rappi.Tmp3$ + ";" +                                       \! Nro Pedido
              XtotC$ + ";" +														                   \! Monto compra
              TS.STORE$ + ";" + 																				   \! Tienda
              Right$("000000"+UnPack$(Gr.Rappi.CajOri$),6) + ";" + 				 \! Terminal
              Right$("000000"+UnPack$(Gr.Rappi.TrxOri$),6) + ";" + 				 \! Transaccion
              XtotD$ + ";" +														                   \! Descuentos
              XtotT$        														  									! Impuestos 
     Call VISORES4690(1,"ANULA  "+Gr.Rappi.Tmp3$,"ESPERE POR FAVOR ...",0,"L")	! Msg Operador
     Asc.Pay.Impr% = 0																											! Aumenta contador de trx
     Xsnd$ = DATE$ +":"+ Time$                                              ! Fecha y hora del requerimiento
     XTemp4$ = Armar.Trama.Msg("45","02",Xbuff$,"00","0001","123456")   		! Armar trama MSG
     XTrama$ = Rutina.Java("com.grpretail.mega.ecuador.sigui.actions.Core","execute", XTemp4$)       ! Ejecuta Requerimiento
     Xfin$ = DATE$ +":"+ Time$                                              ! Fecha y hora rta del requerimiento
     Call RAPPI.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                    ! Rastreo movimiento
     XTEMP4$ = Valida.Rta(XTrama$)																		      ! Valida rta entregada
     If Xtemp4$ <> "00" Then Begin 																					! Si falla en proceso comunicacion
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
  	  TS.TEMP5I2 = 0                    													        	! Control del proceso
     EndIf																																	!
     XTEMP4$ = Mid$(XTrama$,12,2)	         																  ! Valida rta entregada
     If XTemp4$ <> "00" Then Begin 																				  ! Rta No Satisfactoria
     	  Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									        ! Presenta Msg Error
        TS.TEMP5I2 = 0                    													       	! Control del proceso
     EndIf																																	!
  	 If TS.TEMP5I2 = 1 Then Begin      													        		! Si proceso correcto
  	 	  Delrec 94 ;  Xkey$																									! Elimina registro keyed
  	 EndIf																																	!
  EndIf Else Begin																													! Pedido no encontrado
  	 Call Visor.And.Borrar("PEDIDO A ANULAR NO  ENCONTRADO   /Borrar")      !
  	 TS.TEMP5I2 = 0                    													        		! Si proceso correcto
  EndIf      																																! Fin valida pedido
  Close 94
End Sub 																																		! Fin anul pedido

Sub Anula.Pago.Rappi																												! Anulacion pago
Integer*2 Xcont%, XK%, XCant%
String Xmsg1$, Xmsg2$, Xdata$, XvlrTrx$, Llave$
         TS.TEMP5I2 = 0                    													        ! Si proceso correcto
         Call Anul.Pedido.Rappi                						  	      				! Anulación movimiento
      	 IF TS.TEMP5I2 <> 0 Then Begin																			! Si proceso correcto
            Gr.Rappi.Compra$ = UnPack$(Gr.Rappi.Compra$)
            Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 												! Init vectores de carga
            TS.IO.DATA$(3) = Right$(Gr.Rappi.Tvp$,1)                   			! Variedad de Pago
            TS.IO.KEYS(3)  = 78                                             ! Tecla Slash
            TS.IO.DATA$(7) = Gr.Rappi.Compra$																! Valor a entregar
            TS.IO.DATA$(9) = Gr.Rappi.Tmp3$																	! ID del Beneficiario
            TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.Rappi.Tvp$,1))          			! Tipo de Pago
            TS.IO.MOTORKEY = TS.IO.KEYS(7)																	!
            TS.TEMP1$ = "1"                     + ":" +                    \! ID domiciliario
                        Gr.Rappi.Tmp3$          + ":" +                    \! Numero del pedido
                        TS.IO.DATA$(7)          + ":" +                    \! Valor pago
                        "01"                                                ! Signo Operacion  00.Pago 01. Anulacion
            Call Grabacion.Cadena.Usuario("2011",TS.TEMP1$)                 ! Almacena UD operaciones DEUNA
            TS.TEMP1I1 = 0
            For TS.TEMP1I1 = 0 To 7
                TO.NEGAMT(TS.TEMP1I1) = 9999999
                TO.NEGCNT(TS.TEMP1I1) = 999
            Next TS.TEMP1I1           
         EndIf Else Begin 																									! Falla Confirmacion
            Call Gr.Init.Trx																								! Init Trx
            TS.IO.STATE    = 10            																  ! PREPARE FOR NORMAL SALES STATE
         EndIf																															!
End Sub																																			!

Sub PAGO.RAPPI(Xopt%) Public																					  		! Modulo Pago De Una
Integer*4 Xopt%, Xj%
Integer*1 Xm%, Xfnd%

!--- EAMTSU07.J86
If XOPT% = 07 Then Begin																										! Carga de opciones
  Gr.Rappi.Act% = 0 																										    ! Init proyecto activo
  Dim GR.BKRPGCNT(8)
  Dim GR.BKRPGAMT(8)
  Gr.Rappi.Numtrx% = 0																											! Nro de pagos
  Dim Gr.Rappi.Pagos$(20,2)																								  ! 20 pagos 
  TS.TS11WERR$ = ""																													! Control errores
  Open "R::$ARGENER" As 94 UNLOCKED NOWRITE NODEL 												  ! Apertura archivo parametrizacion para TCxSky
  If TS.TS11WERR$ <> "" Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR PARAMETROSPAGO RAPPI!  ")                 !
	   Gr.Rappi.Act% = 0																										  ! Init proyecto activo  	 
  	 Exit Sub																																!
  EndIf 
  If End #94 Then UE.FIN.RAPPI         																	    ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[PAGO RAPPI]" Then Begin		 	  		        				! Parametros pago Rappi
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Rappi.Act% = Val(Mid$(TS.TEMP1$,30,2))      			    					! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Rappi.Tvp$ = Mid$(TS.TEMP1$,30,2)           			    					! Tipo y variedad de pago 
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Rappi.Motor$ = Mid$(TS.TEMP1$,30,3)           			    			  ! Tecla motora
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Rappi.Motor2$ = Mid$(TS.TEMP1$,30,3)           			    			  ! Tecla motora
         
         Call RAPPI.BUSCA.LIMITE.TV(Gr.Rappi.Tvp$)													! Ajusta controles pago
         GoTo UE.FIN.RAPPI                                         					! Sale del proceso
        EndIf																																!
  Wend 																																		  ! Fin recorrido archivo
  UE.FIN.RAPPI: 
    Close 94 
    If Gr.Rappi.Act% = -1 Then                                             \! Proyecto Activo
      Call U.IMPRIME("MODULO PAGO RAPPI      ON 05/Ago/2024",2100H) Else   \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO PAGO RAPPI     OFF 05/Ago/2024",2100H)         ! Msg Proyecto Cargado
EndIf

If Gr.Rappi.Act% = 0 Then Exit Sub 																				  ! Si proyecto apagado

!--- EAMTSU02.J86
If XOPT% = 02 Then Begin																										! Al inicio de una trx
   Gr.Rappi.PinDeUna$ = ""
   Gr.Rappi.Pin$  = ""
   Gr.Rappi.Tmp1$ = ""
   Gr.Rappi.Tmp2$ = ""
   Gr.Rappi.Tmp3$ = ""
   Gr.Rappi.AplAn% = 0
   Gr.Rappi.Apl%   = 0
   Gr.Rappi.AnulPin$ = ""
   To.TENDLIMITS(Gr.Rappi.TVPOS%,0) = Gr.Rappi.MaxTv%												! Retorna valores default
   To.TENDLIMITS(Gr.Rappi.TVPOS%,1) = Gr.Rappi.ChgTv%												! Retorna valores default
   TS.TEMP1I1 = 0
   For TS.TEMP1I1 = 0 To 7
       TO.NEGAMT(TS.TEMP1I1) = GR.BKRPGAMT(TS.TEMP1I1)
       TO.NEGCNT(TS.TEMP1I1) = GR.BKRPGCNT(TS.TEMP1I1)
   Next TS.TEMP1I1
   Dim Gr.Rappi.Pagos$(20,2)																								! 20 pagos 
   Gr.Rappi.Numtrx% = 0																											! Nro de pagos
   Gr.Rappi.Compra$ = ""
   Gr.Rappi.Imptos$ = ""
   Gr.Rappi.Dsctos$ = ""
   Gr.Rappi.CajOri$ = ""
   Gr.Rappi.TrxOri$	= ""
   
EndIf																																				! Fin eamtsu02

!--- EAMTSU14.J86
If XOPT% = 14 Then Begin																										! En entrada de datos
 
 If TS.IO.MOTORKEY = Val(Gr.Rappi.Motor2$) Then Begin												! Anul Rappi
   If java.init = -1 Then Begin																							! Habilita estado 101
      jGuiSubState = 80670																								  ! para entrada de datos
      Call javaEvent(terminalSubStateMsg)																    ! alfanumericos
   EndIf 																																	  !
   jGuiSubState = 80508																								  		! Teclado qwerty
   Call javaEvent(terminalSubStateMsg)																		  !
   Gr.Rappi.Tmp3$ = ingresoTexto$("INGRESE # PEDIDO A","ANULAR /Borrar")    ! 
   If Left$( Gr.Rappi.Tmp3$, 1 ) = "A"  Then Gr.Rappi.Tmp3$ = "E"			      !
   If Left$( Gr.Rappi.Tmp3$, 1 ) <> "P" Then Gr.Rappi.Tmp3$ = "E"		 	      !
   If Gr.Rappi.Tmp3$ = "E" Then BEGIN														     		    ! Proceso cancelado
      Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								      !
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      !
      TS.IO.MOTORKEY = 73																						   	    !
      Exit Sub 																															!
   EndIf 																													     	    !
   Gr.Rappi.Tmp3$ = Right$( Gr.Rappi.Tmp3$, len( Gr.Rappi.Tmp3$ ) - 2 )     ! toma dato capturado
   Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 									  						! Init vectores de carga
   To.TENDLIMITS(Gr.Rappi.TVPOS%,0) = 9999999   														! Modifica controles entrega efectivo
   To.TENDLIMITS(Gr.Rappi.TVPOS%,1) = 9999999										    			  ! Modifica controles entrega efectivo
   TS.IO.DATA$(5) = "1"																										  ! Secuencia cambio a efectivo          
   TS.IO.KEYS(5)  = 61																											!
   TS.IO.MOTORKEY = 61																											!
   Gr.Rappi.AplAn% = -1
   Call TSTCEC01                                                      			!
   Call Anula.Pago.Rappi
   Exit Sub                                                           			!
 EndIf																																			! Fin Anul Rappi
 
 If TS.IO.MOTORKEY = Val(Gr.Rappi.Motor$) Then Begin												!
   If Not TS.BAL.TAKEN Then Begin																						!
      TS.GUIDANCE = 1020																										!
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      !
      TS.IO.MOTORKEY = 73																						      	!
      Exit Sub																															!
   EndIf																																		!
   
   If Gr.Domi.Intrx% <> -1 Then Begin                                       ! No en trx de domicilios
      Call VISOR.AND.BORRAR("FORMA PAGO NO VALIDAFUERA DE DELIVERY")			  !
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      !
      TS.IO.MOTORKEY = 73																						      	!
      Exit Sub 																											      	!
   EndIf
   
   If Gr.Domi.Delivery% <> 1 Then Begin 																		! Delivery diferente rappi
      Call VISOR.AND.BORRAR("FORMA PAGO SOLO VALIDA PARA RAPPI.   ")			  !
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      !
      TS.IO.MOTORKEY = 73																						      	!
      Exit Sub 																											      	!
   EndIf																																		!
   
   Gr.Rappi.AplAn% = 0                         														  ! Init Variable
   If TS.IO.KEYS(1) = 70 Then Gr.Rappi.AplAn% = -1                          ! Anulacion pago
   If java.init = -1 Then Begin																							! Habilita estado 101
      jGuiSubState = 80670																								  ! para entrada de datos
      Call javaEvent(terminalSubStateMsg)																    ! alfanumericos
   EndIf 																																	  !
   jGuiSubState = 80508																								  		! Teclado qwerty
   Call javaEvent(terminalSubStateMsg)																		  !
   Gr.Rappi.Tmp3$ = ingresoTexto$("INGRESE NUMERO DEL","PEDIDO /Borrar")    ! 
   If Left$( Gr.Rappi.Tmp3$, 1 ) = "A"  Then Gr.Rappi.Tmp3$ = "E"			      !
   If Left$( Gr.Rappi.Tmp3$, 1 ) <> "P" Then Gr.Rappi.Tmp3$ = "E"		 	      !
   If Gr.Rappi.Tmp3$ = "E" Then BEGIN														     		    ! Proceso cancelado
      Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								      !
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      !
      TS.IO.MOTORKEY = 73																						   	    !
      Exit Sub 																															!
   EndIf 																													     	    !
   Gr.Rappi.Tmp3$ = Right$( Gr.Rappi.Tmp3$, len( Gr.Rappi.Tmp3$ ) - 2 )     ! toma dato capturado
   TS.TEMP5I4 = TS.TOTALS(0,0,0) - GR.TOTAL.DSC%														! Total de la trx
   
   TS.TEMP5I4 = TS.BALDUE(0)																								! Saldo de la compra
   TS.BD.DSPPARM = -1                     																	! SET FOR NORMAL BD                    
   Call TSBDEC01                          																	! FIRST TAKE BAL DUE                         
   TS.IO.STATE = 10                                                         !
   Dim TS.IO.PREV.KEYS(10)                                                  !
   Dim TS.IO.PREV.DATA$(10)                                                 !
   TS.IO.PREV.MOTORKEY = 0                                                  !
   Dim Ts.Io.Keys(10)																									      ! Inicializa Vectores
   Dim Ts.Io.Data$(10)																								      !
   If Gr.Rappi.AplAn% = -1 Then TS.IO.KEYS(1) = 70                   	      ! Anulacion pago
   TS.IO.DATA$(3) = Right$(Gr.Rappi.Tvp$,1)              		        	      ! Variedad de Pago
   TS.IO.KEYS(3)  = 78                                     		              ! Tecla Slash
   TS.IO.DATA$(7) = Str$(TS.TEMP5I4)					        								      ! Valor a entregar
   TS.IO.DATA$(9) = Gr.Domi.Pedido$										        				      ! Nro del pedido
   TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.Rappi.Tvp$,1))   		          	      ! Tipo de Pago
   TS.IO.MOTORKEY = TS.IO.KEYS(7)															              !
   To.TENDLIMITS(Gr.Rappi.TVPOS%,0) = 9999999   										        ! Modifica controles entrega efectivo
   To.TENDLIMITS(Gr.Rappi.TVPOS%,1) = 9999999									              ! Modifica controles entrega efectivo  
   TS.TEMP1I1 = 0                                                           !  
   For TS.TEMP1I1 = 0 To 7                                                  !
       TO.NEGAMT(TS.TEMP1I1) = 9999999                                      !
       TO.NEGCNT(TS.TEMP1I1) = 999                                          !
   Next TS.TEMP1I1                                                          !
   Exit Sub                                                                 !
 EndIf																																			! Fin secuencia
EndIf																																				! Fin Eamtsu14

!--- EAMTSU32.J86
If XOPT% = 32 Then Begin																										! Validacion forma de pago
	If Gr.Rappi.AplAn% = -1 Then Exit Sub 
  If (TS.IO.DATA$(3) = Right$(Gr.Rappi.Tvp$,1)) And                        \!
    (TS.IO.KEYS(7)  = Val("9"+Left$(Gr.Rappi.Tvp$,1))) Then Begin           ! Si pago bono kupi
 	  TS.TEMP1I1 = 0																													! Control de errores
    Call Validando.PagoRappi   																							! Validando forma de pago
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
       Gr.Rappi.PinDeUna$ = ""
       Gr.Rappi.Pin$  = ""
       Gr.Rappi.Tmp1$ = ""
       Gr.Rappi.Tmp2$ = ""
       Gr.Rappi.Tmp3$ = ""
       Gr.Rappi.AplAn% = 0
       Gr.Rappi.Apl%   = 0
       Exit Sub                                                             !
   	EndIf Else Begin																							          !
   		 TS.TEMP2$ = "00"																											! Default venta
   		 If TS.IO.KEYS(1) = 70 Then TS.TEMP2$ = "01"                          ! Si una anulacion
       TS.TEMP1$ = Str$(Gr.Domi.Delivery%) + ":" +                         \! ID domiciliario
                   Gr.Rappi.Tmp3$          + ":" +                         \! Numero del pedido
                   TS.IO.DATA$(7)          + ":" +                         \! Valor pago
                   TS.TEMP2$                                                ! Signo Operacion  00.Pago 01. Anulacion
      Call Grabacion.Cadena.Usuario("2011",TS.TEMP1$)                       ! Almacena UD operaciones DEUNA
      Call Almacena.Pedido.Rappi																						! Almacenamiento pedido
      Gr.Rappi.Apl%   = -1																									! Pago aplicado
    EndIf																																		!
 EndIf																																			! fin pago
EndIf																																				! Fin Eamtsu32

!--- EAMTSU53.J86
If XOPT% = 53 Then Begin																										! Recuperación de trx
   Call RECUPERA.TRX.RAPPI																									! Valida si operaciones deuna
EndIf																																				! Fin EAMTSU53

End Sub																																			! Fin Modulo control cambio precio
