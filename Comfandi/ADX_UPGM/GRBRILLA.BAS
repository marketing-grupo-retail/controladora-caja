!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRBRILLA.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : FORMA PAGO CREDITO BRILLA        *
!**************************************************
!Observaciones:
! Version 1.0 - Noviembre 25 2022
!--------------------------------------------------

%ENVIRON T		                         																			! Ambiente de terminal

String    Global Gr.Fis.BrillaDoc$,                                        \!
                 Gr.Fis.BrillaDate$,                                       \!
                 Gr.Fis.BrillaPre$,                                        \!
                 Gr.Fis.BrillaPgo$,                                        \!
                 Gr.Fis.BrillaValr$                                         !
Integer*1 Global Gr.Fis.BrillaTrx%                                          !

%INCLUDE EAMTSEVA.J86          																							! Variables Cliente Frecuente
%INCLUDE EAMTSWKG.J86          																							! working storage
%INCLUDE EAMTOPTS.J86          																							! Terminal Options
%INCLUDE EAMTRANS.J86          																							! Variables de transancion
%INCLUDE EAMITEMR.J86          																							! Variables EamItemr
%INCLUDE LCLTSUVA.011	                                                      ! Variables desarrollo              
%INCLUDE EAMTSXHC.J86          																							! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          																							! Rutinas de Assembler

%INCLUDE RECATSSU.011        																								! Rutinas Genericas Grupo Retail

Sub TSHIECET External 
End Sub 

Function VALIDA.COMPRA.UVT External 
Integer*1 VALIDA.COMPRA.UVT
End Function 

Sub BRILLA.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:PB" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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

Sub VALIDA.PAGO.BRILLA
String Xlen$, Xsnd$, Xrta$, Xfin$, Xtrama$, Xtemp4$, Xmsg$
   TS.TEMP1I1 = 0
   Call VISORES4690(1,"VALIDANDO # CREDITO","ESPERE POR FAVOR",1000,"L")
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Xmsg$ = Gr.Fis.BrillaDoc$                           											! Numero de documento
   Xrta$ = Armar.Trama.Msg("21","30",Xmsg$,"00","0001","123456")            ! Armar trama MSG
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", Xrta$)         	! Ejecuta Requerimiento
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call BRILLA.AUDITORIA(Xrta$,Xtrama$, Xsnd$, Xfin$)                    	  ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   	  Exit Sub  
   EndIf
   XTEMP4$ = Mid$(XTrama$,12,2)	           																  ! Valida rta entregada
   If XTemp4$ <> "00" Then Begin 																					  ! Rta Satisfactoria
   	  Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   	  Exit Sub 
   EndIf Else Begin
   	  Call VISOR.AND.BORRAR("NUMERO DE CREDITO     ACEPTADO /Borrar")       ! Msg Alerta
   EndIf
   TS.TEMP1I1 = -1
End Sub 

Sub Actualiza.Brilla																												! Grabacion recibo
String Xlen$, Xsnd$, Xrta$, Xfin$, Xtrama$, Xtemp4$, Xmsg$
   Call VISORES4690(1,"ALMACENANDO CREDITO","ESPERE POR FAVOR",1000,"L")
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Xmsg$ = Gr.Fis.BrillaDoc$ + ("0" + Gr.Fis.BrillaValr$)                   ! documento mas valor
   Xrta$ = Armar.Trama.Msg("21","31",Xmsg$,"00","0001","123456")            ! Armar trama MSG
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", Xrta$)         	! Ejecuta Requerimiento
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call BRILLA.AUDITORIA(Xrta$,Xtrama$, Xsnd$, Xfin$)                    	  ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   	  Exit Sub  
   EndIf
   XTEMP4$ = Mid$(XTrama$,12,2)	           																  ! Valida rta entregada
   If XTemp4$ <> "00" Then Begin 																					  ! Rta Satisfactoria
   	  Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   	  Exit Sub 
   EndIf

End Sub 																																		! Fin grabación recibo

Sub Aplica.Pago.Brilla Public 																							! Aplica pago brilla

If LEN(TS.IO.DATA$(2)) = 45 And \												  								  ! Si barra convenio brilla
   Left$(TS.IO.DATA$(2),3) = Gr.Fis.BrillaPre$ Then Begin                   ! si prefijo convenio

 	 If TS.TRX.STATUS <> 1 And TS.PROCEDURE = -1 THEN BEGIN				  	        ! No se ha ingresado total en venta
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 0 																									! 
       TS.GUIDANCE = 1020																										! Solicita total a la trx
       Exit Sub 																														! Sale del proceso
   EndIf 																																	  !
   If (Gr.Fis.BrillaTrx% = -1 And TS.IO.KEYS(1) <> 70) Then Begin           ! Ya brilla registrado pero no anulacion
   	  Call Visor.And.Borrar("PAGO BRILLA YA ESTA APLICADO /Borrar")
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    	! Inicializa secuencia de teclado
      TS.IO.MOTORKEY = 73																							  		! 
      Exit Sub 																															! Sale del procedimiento
   EndIf																																		!
   TS.TEMP1I1 = VALIDA.COMPRA.UVT																					  ! Si cumple parametros compra minima UVT
   If TS.TEMP1I1 <> -1 Then Begin 																				  ! Si no pasa validacion Feco
   	  Gr.Fis.BrillaDoc$  = ""
      Gr.Fis.BrillaDate$ = ""
      Gr.Fis.BrillaValr$ = ""
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    	! Inicializa secuencia de teclado
      TS.IO.MOTORKEY = 73																							  		! 
      Exit Sub 																															! Sale del procedimiento
   EndIf
   Gr.Fis.BrillaDoc$  = Mid$(TS.IO.DATA$(2),4,24)                           ! Numero de documento
   Gr.Fis.BrillaDate$ = Mid$(TS.IO.DATA$(2),28,8)                           ! fecha del documento
   Gr.Fis.BrillaValr$ = Mid$(TS.IO.DATA$(2),36,10)                          ! monto del crédito
   TS.TEMP4$ = "20" + DATE$																									! Fecha del sistema
   If TS.TEMP4$ <> Gr.Fis.BrillaDate$ Then Begin														! Si no fecha del sistema
   	  Call Visor.And.Borrar("DOCUMENTO NO VIGENTE /Borrar")
   	  Gr.Fis.BrillaDoc$  = ""
      Gr.Fis.BrillaDate$ = ""
      Gr.Fis.BrillaValr$ = ""
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    	! Inicializa secuencia de teclado
      TS.IO.MOTORKEY = 73																							  		! 
      Exit Sub 																															! Sale del procedimiento
   EndIf																																		! fin validacion fecha
      TS.TEMP1$ = Gr.Fis.BrillaDoc$																					! Numero de referencia
      TS.TEMP2$ = Str$(Val(Gr.Fis.BrillaValr$))															! Valor recibo
      If TS.IO.KEYS(1) <> 70 Then                                          \! No es un reintegro 
      If Val(TS.TEMP2$) > TS.BALDUE(0) Then Begin														! Si recibo mayor que saldo de la compra
   	     Call Visor.And.Borrar("FORMA DE PAGO NO    PERMITE CAMBIO/Borra")
   	     Gr.Fis.BrillaDoc$  = ""
         Gr.Fis.BrillaDate$ = ""
         Gr.Fis.BrillaValr$ = ""
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    ! Inicializa secuencia de teclado
         TS.IO.MOTORKEY = 73																							  ! 
         Exit Sub 																													! Sale del procedimiento
      EndIf																																	! fin valida valor recibo
      TS.TEMP1I1 = 0																												! Control proceso
      If TS.IO.KEYS(1) = 70  Then TS.TEMP1I1 = -1														! Registra anulación
      If TS.IO.KEYS(1) <> 70 Then                                          \!
      	 Call VALIDA.PAGO.BRILLA																						! Valida nro documento
      If TS.TEMP1I1 <> -1 Then Begin																				! Falla proceso
   	     Gr.Fis.BrillaDoc$  = ""
         Gr.Fis.BrillaDate$ = ""
         Gr.Fis.BrillaValr$ = ""
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    ! Inicializa secuencia de teclado
         TS.IO.MOTORKEY = 73																							  ! 
         Exit Sub 																													! Sale del procedimiento
      EndIf
      TS.TEMP1I1 = 0																												! Control anulacion
      If TS.IO.KEYS(1) = 70 Then TS.TEMP1I1 = -1														! Registra anulación
      Dim TS.IO.PREV.DATA$(10) : Dim TS.IO.PREV.KEYS(10)
      TS.IO.PREV.MOTORKEY = 80
      TS.IO.DEVICE = 1 
      Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 												      ! Init vectores de carga
      If TS.TEMP1I1 = -1 Then TS.IO.KEYS(1) = 70                            ! Si anulacion de pago 
      TS.IO.DATA$(3) = Right$(Gr.Fis.BrillaPgo$,1) 													! Variedad de pago
      TS.IO.KEYS(3)  = 78                                                   ! Tecla Slash
      TS.IO.DATA$(7) = Str$(Val(Gr.Fis.BrillaValr$))												! Valor recibo
      TS.IO.DATA$(9) = Gr.Fis.BrillaDoc$ 													          ! Numero de referencia
      TS.IO.KEYS(7)  = Val("9"+Left$(Gr.Fis.BrillaPgo$,1))                  ! Forma de pago 
      TS.IO.MOTORKEY = TS.IO.KEYS(7)																	      ! Reporta a la tecla motora
      If TS.IO.KEYS(1) = 70 Then                                           \!
      	 Gr.Fis.BrillaTrx% = 0  Else 																			 \! No hay brilla en trx
      	 Gr.Fis.BrillaTrx% = -1                                             ! Brilla en trx 
EndIf																																				! fin registro convenio brilla

End Sub 

Sub Confirma.Pago.Brilla Public 																						! Grabación documento brilla
 If TS.INTRX Then                                                          \! Si esta en una compra
  If (TS.LINETYPE = 6 And TS.LINEDATA = 1) And                             \! Store Data y Transacc
        Gr.Fis.BrillaTrx% = -1 Then Begin                                   ! Fecha, Hora, etc 
        Call Actualiza.Brilla                                               ! Actualizacion recibos brilla
        Gr.Fis.BrillaTrx% = 0 																							!
  EndIf 																																	  !

End Sub 																																		! Fin grabacion
