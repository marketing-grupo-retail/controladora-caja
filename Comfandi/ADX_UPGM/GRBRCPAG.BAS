!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRBRCPAG.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Pago Bonos Recompra              *
!**************************************************
!Observaciones:

%ENVIRON T		                          																		! Ambiente de terminal

Integer*1 Global Gr.BrcPag.Ok%           																		! Control del proyecto
Integer*1 Global Gr.BrcPag.dat%
Integer*1 Global Gr.BrcPag.Dup%                                             ! Genera duplicado tiquete
Integer*1 Global Gr.BrcPag.Capt%                                            ! Credito Social capturado
Integer*2 Global SL.INDEX
Integer*2 Global Gr.BrcPag.Key%          																	  ! Tecla motora
Integer*4 Global Asc.Tmp.Apun%                                              ! 
Integer*1 Global Gr.Fis.Sesion%                                             ! Sesion numeracion fiscal
Integer*1 Global Asic.Detalle%, Asic.Device%                                ! 
Integer*1 Global TS.RECOVERY                    														! terminal recovery flag
Integer*1 Global Gr.REC.TipID%																							! Reservas Hoteleras
Integer*1 Global Gr.RecSrv.NewCol%                                          ! Recaudo colegios
Integer*2 Global UE.RECASRV.QTY%																						! Proceso recaudos terceros 
Integer*1 Global Gr.RecSrv.RecComfa%               													! Indicador recaudo comfandi
String    Global HORA.MUNDIAL$																							! Fecha y hora trx 

String    Global Gr.BrcPag.VlrCompra$,  																	 \! Valor compra
                 Gr.BrcPag.IdClte$,     																	 \! Id del cliente
                 Gr.BrcPag.TipVar$                                          ! Tipo y variedad de pago
               
%INCLUDE EAMTSWKG.J86          																			        ! working storage
%INCLUDE EAMTRANS.J86          																			        ! working storage
%INCLUDE EAMITEMR.J86          																			        ! Maestra productos
%INCLUDE EAMTOPTS.J86          																			        ! working storage
%INCLUDE RECATSSU.011          																			        ! Rutinas Genericas Grupo Retail

Function FORMAT.AMOUNT(AMT1) External   																		! Formateo de valores
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub TSPREC01 EXTERNAL          ! print routine
End Sub

Sub TSHIECET EXTERNAL
End Sub

Sub CRDSOC.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:BR" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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


Function BUSCA.AUTO.CS(Xnaut$)
Integer*1 BUSCA.AUTO.CS, XM%, Xfnd%
String    Xnaut$
  Xfnd% = 0
  For XM% = 1 To Gr.BrcPag.Qty%																						  ! Total Voucher recaudados
      If Val(Gr.BrcPag.RtaLinix$(XM%,1)) = Val(Xnaut$) And                 \! Si encuentra autorizacion
         (Gr.BrcPag.RtaLinix$(XM%,3)) = "85" Then Begin                     ! y es una venta
         	If Val(TS.TEMP4$) <> Val(Gr.BrcPag.RtaLinix$(XM%,2)) Then Begin   ! Montos no coinciden
         		 Call VISOR.AND.BORRAR("VALOR ORIGINAL PAGO NO COINCIDE")				! Msg Alerta 
         	   BUSCA.AUTO.CS = 0																							! Reporta falla 
        	   Exit Function 																									!
         	EndIf
         	Xfnd% = XM%																												! 
         	BUSCA.AUTO.CS = Xfnd%
         	Exit Function 
      EndIf																																	!
      If (Gr.BrcPag.RtaLinix$(XM%,1) = Xnaut$) And                         \! Si encuentra autorizacion
         (Gr.BrcPag.RtaLinix$(XM%,3)) = "86" Then Begin                     ! y es una venta
         	Xfnd% = 0  																												! 
         	BUSCA.AUTO.CS = Xfnd%
         	Exit Function 
      EndIf																																	!
  Next XM%																																	!
  
End Function 																																! fin busqeda autorizaciones


Sub COMPRACRDSOCIAL(Xfunc$, Xaut$)
String XRtrama$, Xenvia$, Xllega$, XSALE$, XRTA$, Xfunc$, Xalm$, Xaut$
String Xtmp$, Xcampo8$
Integer*1 XPOS%

  TS.ER.RETURN = -1																													! Control del proceso
  Call Echo.CreditoSocial																										! Prueba comunicacion autorizador
  If TS.ER.RETURN <> -1 Then Begin																					! Falla proceso comunicacion
     Call VISOR.AND.BORRAR("ERROR AUTORIZADOR   CREDITO SOCIAL")						!
	   Gr.BrcPag.Capt% = 0																						      	! Proceso No satisfactorio
	   Exit Sub 																													  	! Sale del proceso
  EndIf																																			!

 Call VISORES4690(1,"PROCESANDO COMPRA","ESPERE POR FAVOR",0,"L")						!
 Gr.BrcPag.PosId$ = " 1" 																										! Compra Mercadeo
 If UE.RECASRV.QTY% > 0 Then Begin																					! En recaudos
 	  Gr.BrcPag.PosId$ = "  "
 	  If Gr.RecSrv.RecComfa% = -1 Then Gr.BrcPag.PosId$ = " 3" 							  ! Recaudo colegios 
    If Gr.REC.TipID% = 2 Then Gr.BrcPag.PosId$ = " 2" 											! Recaudo reservas hoteleras
 EndIf																																			! Fin recaudos	
 If Gr.BrcPag.PosId$ = "  " Then Begin																			! No autorizado pago
    Call VISOR.And.BORRAR("FORMA DE PAGO       NO AUTORIZADA")              ! Msg de Error	
		Gr.BrcPag.Capt% = 0																									    ! Proceso No satisfactorio
		Exit Sub 																														    ! Sale del proceso
 EndIf
 Xcampo8$ = Gr.BrcPag.Fwdid$ + Gr.BrcPag.TipCrd$                            ! Id cliente + tipo de credito Add 14Nov2018
 If Xfunc$ = "85" Then Begin																								! Proceso de compra
 TS.TEMP1$ = Right$("                         "+TS.TEMP5$,19)      +       \! Numero de la credencial   C 19
             Right$("   "+Gr.BrcPag.Prcode$,3)                     +       \! Processing Code           C  3
             Right$("            "+TS.TEMP4$+"00",12)              +       \! Valor de la compra        C 12
             Right$("    "+Gr.BrcPag.MerTyp$,4)                    +       \! Merchant Type             C  4
             "  0"                                                 +       \! Entry Mode 0=Scanner      C  3
             Gr.BrcPag.PosId$                                      +       \! Pos conditions ID         C  2
             Left$(Gr.BrcPag.Aquid$+String$(20," "),20)            +       \! Acquirind Id Code         C 20
             Left$(Xcampo8$+String$(20," "),20)                    +       \! Cedula tarjetahabiente    C 20
             Left$(Gr.BrcPag.NomAlm$+String$(25," "),25)           +       \! Nombre del almacen        C 25
             Left$(Gr.BrcPag.CodCiud$+String$(08," "),08)          +       \! Codigo de la ciudad       C  8
             Left$(Gr.BrcPag.CodDpto$+String$(03," "),03)          +       \! Codigo departamento       C  3
             Left$(Gr.BrcPag.CodPais$+" ",02)                      +       \! Codigo del pais           C  2
             Right$("   "+Gr.BrcPag.CurCod$,3)                     +       \! Currency Code             C  3
             "00000000"                                            +       \! Pin Code                  C  8
             Left$(Gr.Oper.Name$+String$(20," "),20)               +       \! Nombre del cajero         C 20
             Right$("   "+Gr.BrcPag.Prcdrv$,3)                              ! Processing code reverso   C  3
 EndIf
 If Xfunc$ = "86" Then Begin																								! Proceso de Anulacion
  XPOS% = BUSCA.AUTO.CS(Xaut$)
  If Xpos% = 0 Then Begin																									  ! Falla en busqueda
    Call VISOR.And.BORRAR("NRO AUTORIZACION NO EXISTE O ANULADO")           ! Msg de Error	
		Gr.BrcPag.Capt% = 0																									    ! Proceso No satisfactorio
		Exit Sub 																														    ! Sale del proceso
  EndIf
 Xtmp$ = Right$(Gr.BrcPag.RtaLinix$(Xpos%,5),10)                            ! mmddhhmmss trx
 TS.TEMP1$ = Right$("                         "+TS.TEMP5$,19)      +       \! Numero de la credencial   C 19
             Right$("   "+Gr.BrcPag.PrcdAn$,3)                     +       \! Processing Code           C  3 
             Right$("            "+TS.TEMP4$+"00",12)              +       \! Valor de la compra        C 12
             Right$("    "+Gr.BrcPag.MerTyp$,4)                    +       \! Merchant Type             C  4
             "  0"                                                 +       \! Entry Mode 0=Scanner      C  3
             Gr.BrcPag.PosId$                                      +       \! Pos conditions ID         C  2
             Left$(Gr.BrcPag.Aquid$+String$(20," "),20)            +       \! Acquirind Id Code         C 20
             Left$(Xcampo8$+String$(20," "),20)                    +       \! Acquirind Id Code         C 20
             Left$(Gr.BrcPag.NomAlm$+String$(25," "),25)           +       \! Nombre del almacen        C 25
             Left$(Gr.BrcPag.CodCiud$+String$(08," "),08)          +       \! Codigo de la ciudad       C  8
             Left$(Gr.BrcPag.CodDpto$+String$(03," "),03)          +       \! Codigo departamento       C  3
             Left$(Gr.BrcPag.CodPais$+" ",02)                      +       \! Codigo del pais           C  2
             Right$("   "+Gr.BrcPag.CurCod$,3)                     +       \! Currency Code             C  3
             "00000000"                                            +       \! Pin Code                  C  8
             Left$(Gr.Oper.Name$+String$(20," "),20)               +       \! Nombre del cajero         C 20
             Right$("          "+Xtmp$,10)                         +       \! mmddhhmmss trx original   C 10
             Right$("      "+Gr.BrcPag.RtaLinix$(Xpos%,1),6)                ! Nro Autorizacion          C  6                       
 EndIf

 TS.TEMP2$ = Armar.Trama.Msg("17",Xfunc$,TS.TEMP1$,"00","0001","123456") 		! Armar trama MSG                	
 Xenvia$   = TS.TEMP2$																											!
 Xsale$    = Date$ + ":" + TIME$																						! Hora entrada requerimiento
 XRtrama$  = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)  			! Ejecuta Requerimiento
 Xrta$     = Date$ + ":" + TIME$																						! Hora salida requerimiento
 Xllega$   = XRtrama$																												!
 Call CRDSOC.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                      ! Rastro auditoria
 TS.TEMP1$ = Valida.Rta(XRtrama$)																					  ! Valida rta entregada
 If TS.TEMP1$ = "00" Then Begin																							! Proceso satisfactorio
	  TS.TEMP1$ = MID$(XRtrama$,12,2)                                         ! Toma rta de la trama
		If TS.TEMP1$ <> "00" Then Begin								   									    	! Si presenta un Error
			 Call VISOR.And.BORRAR(Mid$(XRtrama$,14,40))                          ! Msg de Error	
			 Gr.BrcPag.Capt% = 0																									! Proceso No satisfactorio
			 Exit Sub 																														! Sale del proceso
    EndIf																																		! Proceso satistactorio
    Gr.BrcPag.Capt% = -1																										! Proceso satisfactorio
    Gr.BrcPag.Nro$  = MID$(XRtrama$,54,6)                                   ! Nro de autorizacion
    Call TSHIECET																														! Tono Alerta
    Call VISORES4690(1,"AUTORIZADO",Gr.BrcPag.Nro$,1200,"L")                ! Msg al cajero
    Gr.BrcPag.Qty% = Gr.BrcPag.Qty% + 1 																		! Aumenta apuntador
    Gr.BrcPag.RtaLinix$(Gr.BrcPag.Qty%,0) = Gr.BrcPag.IdClte$               ! Id del cliente
    Gr.BrcPag.RtaLinix$(Gr.BrcPag.Qty%,1) = Gr.BrcPag.Nro$                  ! Nro de autorizacion
    Gr.BrcPag.RtaLinix$(Gr.BrcPag.Qty%,2) = TS.TEMP4$                       ! Valor de la compra
    Gr.BrcPag.RtaLinix$(Gr.BrcPag.Qty%,3) = Xfunc$                          ! Compra o Anul
    Gr.BrcPag.RtaLinix$(Gr.BrcPag.Qty%,4) = Gr.BrcPag.Fwdid$                ! Cedula tarjeta habiente
    Gr.BrcPag.RtaLinix$(Gr.BrcPag.Qty%,5) = HORA.MUNDIAL$                   ! FECHA Y HORA TRX
    Exit Sub 																																!
 EndIf Else Begin 
    If Len(Xrtrama$) > 9 Then \
	     Call VISOR.And.BORRAR(Mid$(XRtrama$,14,40)) Else                    \! Msg de Error	
	     Call VISOR.And.BORRAR("ERROR EN COMUNICACIONES")                     ! Msg de Error	
 EndIf
 Gr.BrcPag.Capt% = 0																									      ! Proceso No satisfactorio
End Sub																																			!

Sub CREDITOSOCIAL(XOPT%) Public			 																				! Credito Social 
Integer*4 Xopt%, Xloc%                                                      !
String    Xtmp$

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.BrcPag.Ok%  = 0                                                        ! Proyecto Apagado
  Gr.BrcPag.dat% = 0
  TS.ER.RETURN = -1																													! Ctrl Errores
  Dim Gr.BrcPag.RtaLinix$(35,5)																							!
  Gr.BrcPag.Qty% = 0
  Gr.BrcPag.Ncopia% = 0																											!
  Open "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL               							! Apertura archivo parametrizacion
  If TS.ER.RETURN <> -1 Then BEGIN                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA CUPO EMPLEADO")									! MSg alerta
  	 Exit Sub 																															! Sale del proceso
  ENDIF 																																		!
  IF END #94 THEN UE.FIN.CRDEMP         																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        IF TS.TEMP1$ = "[RECOMPRA PAGO]" Then Begin		  	        				  ! Bono recompra pago
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.BrcPag.Ok%   = Val(Mid$(TS.TEMP1$,30,2))   				    					! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.BrcPag.Key%  = Val(Mid$(TS.TEMP1$,30,03))      				          ! Tecla motora secuencia
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.BrcPag.TipVar$ = Mid$(TS.TEMP1$,30,02)          				        ! Tipo y variedad pago registro
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         GoTo UE.FIN.BRCPAG  																								! Sale del ciclo de carga          
       Endif                                                                !
   Wend                                                                     !
   UE.FIN.BRCPAG:                                                           !
     Close 94																																! Cierra archivo
   If Gr.BrcPag.Ok% = -1 Then                                              \! Proyecto Activo
      Call U.IMPRIME("MODULO BRC PAGOS     ON  27-Nov-2019",6100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO BRC PAGOS     OFF 27-Nov-2019",6100H)          ! Msg Proyecto Cargado
EndIf 																																			! Fin carga opciones

If Gr.BrcPag.Ok% <> -1 Then Exit Sub                                        ! Si proyecto apagado

!--- EAMTSU02.J86
If Xopt% = 02 Then Begin                                                    ! Recuperacion trx
	 Gr.BrcPag.Capt% = 0																											! Init variables
   Gr.BrcPag.VlrCompra$ = ""																								!
   Gr.BrcPag.IdClte$ = ""																										!
   Gr.BrcPag.dat% = 0
   Gr.BrcPag.Nro$ = ""
   Dim Gr.BrcPag.RtaLinix$(35,5)
   Gr.BrcPag.Qty% = 0 
EndIf																																				! Fin recuperacion trx

!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                    ! Secuencias de tecleo
 
 If	TS.IO.KEYS(1) = 70 And TS.IO.MOTORKEY = 81 Then Begin                   ! Si anulacion total
 	If (TS.TOTALS(0,0,0) <> TS.BALDUE(0)) Then Begin													! Si pagos capturados
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
   	   TS.GUIDANCE = 1069																										! Msg Error appl nativa
	EndIf
 EndIf

 If TS.IO.MOTORKEY = Gr.BrcPag.Key% Then Begin 														  ! Si tecla motora
 	If TS.INTRX Then BEGIN 																										! En una trx 
  	If TS.TRX.STATUS <> 1 And TS.PROCEDURE = -1 THEN BEGIN				  	      ! No se ha ingresado total en venta
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 0 																									! 
       TS.GUIDANCE = 1020																										! Solicita total a la trx
       Exit Sub 																														! Sale del proceso
    EndIf 																																	!
 		If TS.TRAINING Then Begin                    														! Si en entrenamiento
 			 Call Visor.And.Borrar("PROCESO NO VALIDO EN ENTRENAMIENTO")          !
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
 	  EndIf																																		!
    TS.TEMP4$ = TS.IO.DATA$(5)																					    ! Reporta valor a pagar
    TS.TEMP5$ = ASIC.DATOS$("FAVOR ESCANEE EL   ","NUMERO DE CREDENCIAL")	  ! Captura credencial
    If TS.TEMP5$ = "E" Then BEGIN 																					! Proceso cancelado
    	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")											!
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
    EndIf 																																	!

    If Asic.Device% <> 3 Then Begin 																				! Dato no escaneado
       Call VISOR.AND.BORRAR("SOLO SE PERMITE DATO POR ESCANER")						! Msg de alerta
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														  ! Init vectores de aplicacion
       TS.IO.MOTORKEY = 73																									! Inicia tecla motora
       Exit Sub 																													  ! Sale de la rutina
    EndIf 																															    ! 

    Call TSHIECET																														! Tono Alerta
    Gr.BrcPag.Fwdid$ = ASIC.DATOS$("INGRESE CEDULA DEL","AFILIADO...")      ! Captura credencial
    If Gr.BrcPag.Fwdid$ = "E" Then BEGIN 																		! Proceso cancelado
    	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")											!
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
    EndIf 																																	!
!-- Add 14Nov2018    
    Gr.BrcPag.TipCrd$ = ASIC.DATOS$("INGRESE TIPO CRED.","1.ROTATIVO 2.ELECTRO")      ! Captura credencial
    If Val(Gr.BrcPag.TipCrd$) < 1 Or Val(Gr.BrcPag.TipCrd$) > 2  Then BEGIN	! Proceso cancelado
    	 Call VISOR.AND.BORRAR("TIPO DE CREDITO NO  VALIDO  /Borrar")					!
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
    EndIf 																																	!
    
    If TS.TEMP4$ = "" Then 																					       \! Valor de la compra
       TS.TEMP4$ = STR$(TS.BALDUE(0))																	      ! 
    If Val(TS.TEMP4$) <= 0 Then Begin																				! 
    	 Call VISOR.AND.BORRAR("VLR CERO EN PAGO NO VALIDO  Borrar=Salir")    ! Msg alerta
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencias 
       TS.IO.MOTORKEY = 73																									! de tecleo 
       Exit Sub 																														!
    EndIf																																		!
    If TS.IO.KEYS(1) <> 70 Then                                            \! No es una anulacion pago
     If Val(TS.TEMP4$) > ( TS.BALDUE(0) ) Then Begin                        ! Si dato supera total compra
    	 Call VISOR.AND.BORRAR("PAGO SUPERIOR AL    VALOR DE LA COMPRA")      !
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
     EndIf																																	!
    Gr.BrcPag.IdClte$ = TS.TEMP5$																						! Reporta numero de credencial
    If (TS.IO.KEYS(1) = 70) Then Begin 																			! En anulacion
       Gr.BrcPag.AutAnul$ = ASIC.DATOS$("INGRESE AUTORIZACION","A ANULAR...") ! Captura Autorizacion
       If Gr.BrcPag.AutAnul$ = "E" Then BEGIN 														  ! Proceso cancelado
    	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")									!
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													!
          TS.IO.MOTORKEY = 73																							  !
          Exit Sub 																													!
       EndIf 						
    EndIf
    TS.TEMP5I1 = 0																													! Init control del proceso
    TS.TEMP6$ = "85" 																												! Funcion de consulta
    If (TS.IO.KEYS(1) = 70) Then Begin 																			!
     TS.TEMP6$ = "86" 													                            ! Funcion de anulacion
    EndIf
    Gr.BrcPag.Capt% = 0																											! Control de proceso
    If TS.TEMP6$ = "85" Then Begin
       Call COMPRACRDSOCIAL(TS.TEMP6$,"0")																	! Procesa requerimiento Compra
    EndIf
    If TS.TEMP6$ = "86" Then Begin
       Call COMPRACRDSOCIAL(TS.TEMP6$,Gr.BrcPag.AutAnul$)		 							  ! Procesa requerimiento Anulacion
    EndIf

    If Gr.BrcPag.Capt% = -1 Then Begin 
      Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 															! Init vectores de carga
      If TS.TEMP6$ = "86" Then TS.IO.KEYS(1) = 70    												! Funcion de anulacion
      TS.IO.DATA$(3) = Right$(Gr.BrcPag.TipVar$,1)                    			! Variedad de Pago
      TS.IO.KEYS(3)  = 78                                             			! Tecla Slash
      TS.IO.DATA$(7) = TS.TEMP4$                              							! Valor a entregar
      TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.BrcPag.TipVar$,1))           			! Tipo de Pago
      TS.IO.DATA$(9) = TS.TEMP5$																						! Nit asociado
      TS.IO.MOTORKEY = TS.IO.KEYS(7)																				!
      Gr.BrcPag.VlrCompra$ = TS.TEMP4$                                      ! Valor de la comnpra
      Gr.BrcPag.IdClte$ = TS.TEMP5$     																	  ! Id del cliente
      Exit Sub 
    EndIf Else Begin
      Gr.BrcPag.VlrCompra$ = ""                                             ! Valor de la comnpra
      Gr.BrcPag.IdClte$ = ""              															    ! Id del cliente
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Init secuencia de tecleo
      TS.IO.MOTORKEY = 73																										!
      Exit Sub 
    EndIf	  
  EndIf Else Begin																													! Si fuera de venta
    Call VISOR.AND.BORRAR("PROCESO NO PERMITIDO FUERA DE VENTA")						! Msg de alerta
    Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																! Init secuencia de tecleo
    TS.IO.MOTORKEY = 73																											!
  EndIf 																																		! 
 EndIf 																																			!
EndIf 																																			! Fin secuencias de tecleo

!--- EAMTSU53.J86
If Xopt% = 53 Then Begin                                                    ! Recuperacion trx

If Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "99" THEN BEGIN   	    	              ! Si DATA/ENTRY
 If Unpack$(MID$(SL.STR.ENTRY$,3,4)) = "20150917" Then Begin                ! Credito Social
    Asc.Tmp.Apun% = 3
    Gr.BrcPag.Qty% = Gr.BrcPag.Qty% + 1 																		! Numero de vouchers
    Xloc% = Gr.BrcPag.Qty%
    TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                   ! Numero de proyecto
    Gr.BrcPag.RtaLinix$(Xloc%,0) = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)! Credencial beneficiario
    Gr.BrcPag.RtaLinix$(Xloc%,1) = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)! Nro Autorizacion
    Gr.BrcPag.RtaLinix$(Xloc%,2) = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)! Valor de la compra
    Gr.BrcPag.RtaLinix$(Xloc%,3) = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)! Compra / Anulacion      
    Gr.BrcPag.RtaLinix$(Xloc%,4) = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)! Cedula tarjeta habiente 
    Gr.BrcPag.RtaLinix$(Xloc%,5) = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)! Fecha y hora de la trx
    Gr.BrcPag.TipCrd$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)           ! tipo de credito
    Gr.BrcPag.IdClte$ = Gr.BrcPag.RtaLinix$(Xloc%,0)                        !
    Gr.BrcPag.Nro$ = Gr.BrcPag.RtaLinix$(Xloc%,1)                           !
    Gr.BrcPag.Capt% = -1																										! Capturado credito social
 Endif
EndIf	

EndIf																																				! Fin recuperacion trx

!-- EAMTSU60
If Xopt% = 60 Then Begin																										! En la impresion del CR
	
  If TS.LINETYPE = 2 And  Gr.BrcPag.Capt% = -1 THEN BEGIN										! Impresion pago y credito social
      TS.TEMP1$ = Left$("AFIL:"+Gr.BrcPag.IdClte$+" AUT:" + Gr.BrcPag.Nro$+String$(37," "),37)
      Call U.IMPRIME(TS.TEMP1$,6100H)
      If Gr.BrcPag.TipCrd$ = "1" Then TS.TEMP1$ = "TIPO CREDITO: ROTATIVO     "
      If Gr.BrcPag.TipCrd$ = "2" Then TS.TEMP1$ = "TIPO CREDITO: ELECTRO HOGAR"
      Call U.IMPRIME(TS.TEMP1$,6100H)
      Gr.BrcPag.Capt% = 0
      If Not(TS.RECOVERY) Then Begin

          TS.TEMP1$ = Pack$(Gr.BrcPag.RtaLinix$(Gr.BrcPag.Qty%,0)) + ":" + \! Id del cliente
                           (Gr.BrcPag.RtaLinix$(Gr.BrcPag.Qty%,1)) + ":" + \! Nro de autorizacion
                      Pack$(Gr.BrcPag.RtaLinix$(Gr.BrcPag.Qty%,2)) + ":" + \! Valor de la compra
                      Pack$(Gr.BrcPag.RtaLinix$(Gr.BrcPag.Qty%,3)) + ":" + \! Compra / Anulacion
                      Pack$(Gr.BrcPag.RtaLinix$(Gr.BrcPag.Qty%,4)) + ":" + \! Cedula tarjeta habiente
                      Pack$(Gr.BrcPag.RtaLinix$(Gr.BrcPag.Qty%,5)) + ":" + \! Fecha y hora trx 
                      Pack$(Gr.BrcPag.TipCrd$)															! Tipo de credito
          Call Grabacion.Cadena.Usuario("20150917",TS.TEMP1$)               ! Almacena UD Credito social

       EndIf
   EndIf

EndIf																																				! Fin EAMTSU60


End Sub 
