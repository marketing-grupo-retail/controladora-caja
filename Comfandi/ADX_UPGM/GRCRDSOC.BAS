!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRCRDSOC.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Credito Social                   *
!**************************************************
!Observaciones:
!17 85 --> Trama de Compra
!17 86 --> Trama de anulacion
!17 87 --> Trama de Echo
!-------------------------------------------------
! Mod 14 Nov 2018
! Se adiciona el tipo de crédito que desea el cliente
! utilizar 1. Rotativo 2. Electro Hogar
! Desarrollo Grupo Retail - OVS
!-------------------------------------------------
! Mod 09/Sep/2022
! Se permite el pago de recaudos de SIGA con la forma de 
! pago de credito social solicitado por Comfandi
! Desarrollo Grupo Retail - OVS
!-------------------------------------------------------

%ENVIRON T		                          																		! Ambiente de terminal

Integer*1 Global Gr.CrdSoc.Ok%           																		! Control del proyecto
Integer*1 Global Gr.CrdSoc.dat%
Integer*1 Global Gr.CrdSoc.Dup%                                             ! Genera duplicado tiquete
Integer*1 Global Gr.CrdSoc.Capt%                                            ! Credito Social capturado
Integer*2 Global SL.INDEX
Integer*2 Global Gr.CrdSoc.Key%          																	  ! Tecla motora
Integer*4 Global Asc.Tmp.Apun%                                              ! 
Integer*1 Global Gr.Fis.Sesion%                                             ! Sesion numeracion fiscal
Integer*1 Global Asic.Detalle%, Asic.Device%                                ! 
Integer*1 Global TS.RECOVERY                    														! terminal recovery flag
Integer*4 Global Gr.CrdSoc.Qty%             														    ! Nro de trx Credito social 
Integer*1 Global Gr.CrdSoc.Ncopia%																					! Nro copias voucher
Integer*1 Global Gr.REC.TipID%																							! Reservas Hoteleras
Integer*1 Global Gr.RecSrv.NewCol%                                          ! Recaudo colegios
Integer*2 Global UE.RECASRV.QTY%																						! Proceso recaudos terceros 
Integer*1 Global Gr.RecSrv.RecComfa%               													! Indicador recaudo comfandi
String    Global Gr.Oper.Name$                                              ! Nombre Cajero
String    Global HORA.MUNDIAL$																							! Fecha y hora trx 

Integer*1 Global Gr.RecSrv.Siga%                                            ! Recaudo SIGA
String    Global Gr.CrdSoc.VlrCompra$,  																	 \! Valor compra
                 Gr.CrdSoc.IdClte$,     																	 \! Id del cliente
                 Gr.CrdSoc.Nro$,                                           \! Nro Autorizacion
                 Gr.CrdSoc.Str$,                                           \! 
                 Gr.CrdSoc.Voucher$(2),                                    \! Voucher impresion
                 Gr.CrdSoc.RtaLinix$(2),                                   \! Rta Trama Tef
                 Gr.CrdSoc.MerTyp$,                                        \! Merchant Type
                 Gr.CrdSoc.Prcode$,                                        \! Processing Code Venta
                 Gr.CrdSoc.Prcdrv$,                                        \! Processing Code reverso
                 Gr.CrdSoc.PrcdAn$,                                        \! Processing Code anulacion
                 Gr.CrdSoc.PosId$,                                         \! Pos Condition ID 
                 Gr.CrdSoc.Aquid$,                                         \! Acquiring Institution Id Code
                 Gr.CrdSoc.Fwdid$,                                         \! Forward Instituion Id  ???? Cedula 
                 Gr.CrdSoc.NomAlm$,                                        \! Nombre Almacen
                 Gr.CrdSoc.CurCod$,                                        \! Currency Code 
                 Gr.CrdSoc.CodPais$,                                       \! Codigo de pais
                 Gr.CrdSoc.CodDpto$,                                       \! Codigo departament
                 Gr.CrdSoc.CodCiud$,                                       \! Codigo ciudad
                 Gr.CrdSoc.AutAnul$,                                       \! Nro Autor. Anulaciones
                 Gr.CrdSoc.TipVar$,                                        \! Tipo y variedad de pago
                 Gr.CrdSoc.TipCrd$                                          ! Tipo de credito Add 14Nov2018
               
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
			X.FILE$ = "R::ADX_UDT1:CS" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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

Sub GR.DISENO.VOUCHERCS																											! Diseño Impresion Voucher
String XL$																																	!
     Dim Gr.CrdSoc.Voucher$(6,50)                                  			    ! Maximo 5 autorizadores 50 lineas de mensaje
     TS.ER.RETURN = -1 																											!
     OPEN "R::$RDVOUCH" AS 94	NoWrite NoDel																	! Apertura archivo formato mensajes
     If TS.ER.RETURN <> -1 Then BEGIN                                       ! Error apertura
  	    Call VISOR.AND.BORRAR("ERROR ARCHIVO DISENO VOUCHER  ")             ! Msg Alerta
  	    Call U.IMPRIME("ERROR APERTURA ARCHIVO VOUCHER",2100H)              ! 
    	  Gr.CrdSoc.Ok% = 0 																									  ! Inactiva proyecto
  	    Exit Sub 																														!
     ENDIF 																																	!
     K% = 0																																	!
     IF END #94 THEN UE.CRDSOC.MSGVOUCHER    												        ! Si es EOF
     While (1)																												      !
      Read #94; A$
      If Left$(A$,1) = "<" Then Begin																				! Carga los mensajes 
	       I% = Match(">",A$,1)																				  			! que van a utilizar 
	       J% = Val(MID$(A$,2,I%-2))																					! c/u de los operadores
	       K% = 1																															! bancarios
	    Endif Else Begin																											!
	     If J% < 6 Then                                                      \! Maximo 5 autorizadores
        If K% <= 50 Then Begin																						  ! Maximo 50 lineas de mensaje
           Gr.CrdSoc.Voucher$ (J%,K%) = A$															    ! Almacena linea de mensaje
           Gr.CrdSoc.Voucher$ (J%,0)  = STR$(K%)										 	      ! Numero de lineas total a imprimir
        EndIf																															  !
       K% = K% + 1																									  		  !
      EndIf
     Wend																																		!         
     UE.CRDSOC.MSGVOUCHER:																									!
       Close 94																															!
End Sub 																																		! Fin carga diseno voucher

Function Voucher.Str.CrdSoc(Ypos%,X.A$)            													! Coloca datos por parametro
String Voucher.Str.CrdSoc, X.A$, X.B$, X.C$, X.D$, XM$
Integer*4 X.A%, X.B%, Ypos%
Integer*1 Xpaso%, Xsgn%
Xpaso% = 0

PASO.RTA.TRAMA:
X.A% = MATCH("&",X.A$,1)						            														! Busca dato a imprimir
If X.A% > 0 Then Begin                          														! Si lo encotro
   X.B% = LEN(X.A$)                             														! Arma mensaje con el dato 
   X.B$ = Left$(X.A$,X.A%-1)                    														! solicitado, entregando 
   X.C$ = Mid$(X.A$,X.A%,2)                     														! 38 caracteres
   X.D$ = Mid$(X.A$,(X.A%+2),X.B%)

   If UCASE$(X.C$) = "&A" Then Begin            														! Valor de la compra
      X.C$ = Gr.CrdSoc.RtaLinix$(Ypos%,2)						  	                    ! Formatea dato
      If Gr.CrdSoc.RtaLinix$(Ypos%,3) = "86" Then X.C$ = "-"+X.C$           ! Es una anulacion 
      Call Format.Amount(Val(X.C$))         																! 
      X.C$ = TS.TEMP1$																											! Retorna dato
   EndIf

   If UCASE$(X.C$) = "&B" Then Begin            														! Numero de autorizacion
      X.C$ = Gr.CrdSoc.RtaLinix$(Ypos%,1)  						  	                  ! 
   EndIf

   If UCASE$(X.C$) = "&C" Then Begin            														! Numero de credencial
      X.C$ = Gr.CrdSoc.RtaLinix$(Ypos%,0)  						  	                  ! 
   EndIf

   If UCASE$(X.C$) = "&D" Then Begin            														! Numero de cedula 
      X.C$ = Gr.CrdSoc.RtaLinix$(Ypos%,4)  						  	                  ! 
   EndIf

   If UCASE$(X.C$) = "&E" Then Begin            														! Tipo de credito
   	  X.C$ = " "
   	  If Gr.CrdSoc.TipCrd$ = "1" Then X.C$ = "ROTATIVO"
   	  If Gr.CrdSoc.TipCrd$ = "2" Then X.C$ = "ELECTRO HOGAR"
   EndIf

   Xsgn% = 1

   X.B$ = X.B$ + X.C$ + X.D$
   X.B$ = Left$(X.B$+String$(37," "),37)
   X.A$ = X.B$
   If Xpaso% = 0 Then Begin
   	  Xpaso% = 1 
   	  GoTo PASO.RTA.TRAMA																										! Analiza nuevamente la trama
   EndIf
EndIf    
Voucher.Str.CrdSoc = X.A$

End Function
!--- Fin funcion data String

Sub IMPRESION.VOUCHER.CRDSOC																							  ! Impresion Voucher credito social
Integer*4 Xl%, Xfin%, Xj%, Xpos%, XM%
String    Xbuff$
  If Gr.CrdSoc.Qty% > 0 Then Begin					     													  ! Hay Vouchers para impresion
   For XM% = 1 To Gr.CrdSoc.Qty%																						! Total Voucher para impresion
  	 Xpos% = 1                                														  ! autorizador bancario
  	 Xl% = 0 : Xfin% = Val(Gr.CrdSoc.Voucher$(XPOS%,0)) 									  ! Init Variables
     To.USEREXIT(20) = 0																									  ! Cancela lineas impresion
     To.USEREXIT(60) = 0																									  !
     For Xj% = 1 To (Gr.CrdSoc.Ncopia% + 1) 																!
      For XL% = 1 To Xfin%																									! Recorre diseño del voucher
         Xbuff$ = Voucher.Str.CrdSoc(Xm%,Gr.CrdSoc.Voucher$(XPOS%,Xl%))     ! Analiza String
         If Xbuff$ <> "" Then Begin																			    ! Hay Info para imprimir
            Call U.Imprime(Xbuff$,4100H)	                                  ! Impresion voucher
         EndIf																															!
      Next Xl%																															! Fin impresion
      If Xj% = 1 Then Begin 																								! Si Voucher para cliente
      	TS.TEMP1$ = "         -- ORIGINAL --              "									!
      	Call U.IMPRIME(TS.TEMP1$,4101H)                                     !
      EndIf Else Begin																											! Si copias para tarjeta habiente
      	TS.TEMP1$ = "         --  COPIA   --              "									!
      	Call U.IMPRIME(TS.TEMP1$,4101H)                                     !
      EndIf																																	!
      Asic.Detalle% = 1
      TS.TEMP1$ = Linea.Detalle(38)  																				! Trailer del movimiento
      Call U.IMPRIME(TS.TEMP1$,4200H)                                       !
      TS.TEMP1$ = String$(37,"-")																						! Linea cierre voucher
      Call U.IMPRIME(TS.TEMP1$,4900H)																			  ! avance de papel
      Call U.CORTACR																												! Corta Papel
     Next Xj%																																! Fin numero copias
     To.USEREXIT(20) = -1																									  ! Activa  lineas impresion
     To.USEREXIT(60) = -1																									  !
     TS.LINETYPE = 18																											  ! Genera cabecera 
     TS.LINEDATA = 1																											  ! en proximo tiquete
     Call TSPREC01																												  !
   Next XM%
  EndIf																																			! Fin impresion del bono

End Sub 																					

Function BUSCA.AUTO.CS(Xnaut$)
Integer*1 BUSCA.AUTO.CS, XM%, Xfnd%
String    Xnaut$
  Xfnd% = 0
  For XM% = 1 To Gr.CrdSoc.Qty%																						  ! Total Voucher recaudados
      If Val(Gr.CrdSoc.RtaLinix$(XM%,1)) = Val(Xnaut$) And                 \! Si encuentra autorizacion
         (Gr.CrdSoc.RtaLinix$(XM%,3)) = "85" Then Begin                     ! y es una venta
         	If Val(TS.TEMP4$) <> Val(Gr.CrdSoc.RtaLinix$(XM%,2)) Then Begin   ! Montos no coinciden
         		 Call VISOR.AND.BORRAR("VALOR ORIGINAL PAGO NO COINCIDE")				! Msg Alerta 
         	   BUSCA.AUTO.CS = 0																							! Reporta falla 
        	   Exit Function 																									!
         	EndIf
         	Xfnd% = XM%																												! 
         	BUSCA.AUTO.CS = Xfnd%
         	Exit Function 
      EndIf																																	!
      If (Gr.CrdSoc.RtaLinix$(XM%,1) = Xnaut$) And                         \! Si encuentra autorizacion
         (Gr.CrdSoc.RtaLinix$(XM%,3)) = "86" Then Begin                     ! y es una venta
         	Xfnd% = 0  																												! 
         	BUSCA.AUTO.CS = Xfnd%
         	Exit Function 
      EndIf																																	!
  Next XM%																																	!
  
End Function 																																! fin busqeda autorizaciones

Sub Echo.CreditoSocial																											! Prueba de comunicacion servicios crd soc
String XRtrama$, Xenvia$, Xllega$, XSALE$, XRTA$, Xfunc$, Xalm$, Xaut$
String Xtmp$
 Call visores4690(1,"VALIDANDO LINEA","CON AUTORIZADOR...",0,"L")
 TS.ER.RETURN = 0   																										  	! Control del proceso
 TS.TEMP1$ = "   980"                                            +         \! Processing Code                     C 6
             "301"                                                          ! Network Management Information Code C 3
 TS.TEMP2$ = Armar.Trama.Msg("17","87",TS.TEMP1$,"00","0001","123456") 	  	! Armar trama MSG                	
 Xenvia$   = TS.TEMP2$																											!
 Xsale$    = Date$ + ":" + TIME$																						! Hora salida requerimiento
 XRtrama$  = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)  			! Ejecuta Requerimiento
 Xrta$     = Date$ + ":" + TIME$																						! Hora llegada requerimiento
 Xllega$   = XRtrama$																												!
 Call CRDSOC.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                      ! Rastro auditoria
 TS.TEMP1$ = Valida.Rta(XRtrama$)																					  ! Valida rta entregada
 If Len(XRtrama$) > 12 Then Begin 																				  ! Proceso ejecutado
    If Mid$(XRtrama$,12,2) <> "00" Then Begin															  ! Fallo el proceso
    	 Call Visor.And.Borrar(Mid$(XRtrama$,14,40))													! Presenta mensaje Error
    	 TS.ER.RETURN = 0 																										! Reporta Error
    EndIf
 EndIf Else Begin																														! Falla de proceso
   TS.ER.RETURN = 0 																										    ! Reporta Error
 EndIf																																			
End Sub 																																		! Fin prueba de comuniacion


Sub COMPRACRDSOCIAL(Xfunc$, Xaut$)
String XRtrama$, Xenvia$, Xllega$, XSALE$, XRTA$, Xfunc$, Xalm$, Xaut$
String Xtmp$, Xcampo8$
Integer*1 XPOS%

  TS.ER.RETURN = -1																													! Control del proceso
  Call Echo.CreditoSocial																										! Prueba comunicacion autorizador
  If TS.ER.RETURN <> -1 Then Begin																					! Falla proceso comunicacion
     Call VISOR.AND.BORRAR("ERROR AUTORIZADOR   CREDITO SOCIAL")						!
	   Gr.CrdSoc.Capt% = 0																						      	! Proceso No satisfactorio
	   Exit Sub 																													  	! Sale del proceso
  EndIf																																			!

 Call VISORES4690(1,"PROCESANDO COMPRA","ESPERE POR FAVOR",0,"L")						!
 Gr.CrdSoc.PosId$ = " 1" 																										! Compra Mercadeo
 If UE.RECASRV.QTY% > 0 Then Begin																					! En recaudos
    Gr.CrdSoc.PosId$ = "  "
    If Gr.RecSrv.RecComfa% = -1 Then Gr.CrdSoc.PosId$ = " 3" 							  ! Recaudo colegios 
    If Gr.REC.TipID% = 2 Then Gr.CrdSoc.PosId$ = " 2"								! Recaudo reservas hoteleras
    If Gr.RecSrv.Siga% = -1 Then Gr.CrdSoc.PosId$ = " 2"                                                      ! Recaudo SIGA
 EndIf																																			! Fin recaudos	
 If Gr.CrdSoc.PosId$ = "  " Then Begin																			! No autorizado pago
    Call VISOR.And.BORRAR("FORMA DE PAGO       NO AUTORIZADA")              ! Msg de Error	
		Gr.CrdSoc.Capt% = 0																									    ! Proceso No satisfactorio
		Exit Sub 																														    ! Sale del proceso
 EndIf
 Xcampo8$ = Gr.CrdSoc.Fwdid$ + Gr.CrdSoc.TipCrd$                            ! Id cliente + tipo de credito Add 14Nov2018
 If Xfunc$ = "85" Then Begin																								! Proceso de compra
 TS.TEMP1$ = Right$("                         "+TS.TEMP5$,19)      +       \! Numero de la credencial   C 19
             Right$("   "+Gr.CrdSoc.Prcode$,3)                     +       \! Processing Code           C  3
             Right$("            "+TS.TEMP4$+"00",12)              +       \! Valor de la compra        C 12
             Right$("    "+Gr.CrdSoc.MerTyp$,4)                    +       \! Merchant Type             C  4
             "  0"                                                 +       \! Entry Mode 0=Scanner      C  3
             Gr.CrdSoc.PosId$                                      +       \! Pos conditions ID         C  2
             Left$(Gr.CrdSoc.Aquid$+String$(20," "),20)            +       \! Acquirind Id Code         C 20
             Left$(Xcampo8$+String$(20," "),20)                    +       \! Cedula tarjetahabiente    C 20
             Left$(Gr.CrdSoc.NomAlm$+String$(25," "),25)           +       \! Nombre del almacen        C 25
             Left$(Gr.CrdSoc.CodCiud$+String$(08," "),08)          +       \! Codigo de la ciudad       C  8
             Left$(Gr.CrdSoc.CodDpto$+String$(03," "),03)          +       \! Codigo departamento       C  3
             Left$(Gr.CrdSoc.CodPais$+" ",02)                      +       \! Codigo del pais           C  2
             Right$("   "+Gr.CrdSoc.CurCod$,3)                     +       \! Currency Code             C  3
             "00000000"                                            +       \! Pin Code                  C  8
             Left$(Gr.Oper.Name$+String$(20," "),20)               +       \! Nombre del cajero         C 20
             Right$("   "+Gr.CrdSoc.Prcdrv$,3)                              ! Processing code reverso   C  3
 EndIf
 If Xfunc$ = "86" Then Begin																								! Proceso de Anulacion
  XPOS% = BUSCA.AUTO.CS(Xaut$)
  If Xpos% = 0 Then Begin																									  ! Falla en busqueda
    Call VISOR.And.BORRAR("NRO AUTORIZACION NO EXISTE O ANULADO")           ! Msg de Error	
		Gr.CrdSoc.Capt% = 0																									    ! Proceso No satisfactorio
		Exit Sub 																														    ! Sale del proceso
  EndIf
 Xtmp$ = Right$(Gr.CrdSoc.RtaLinix$(Xpos%,5),10)                            ! mmddhhmmss trx
 TS.TEMP1$ = Right$("                         "+TS.TEMP5$,19)      +       \! Numero de la credencial   C 19
             Right$("   "+Gr.CrdSoc.PrcdAn$,3)                     +       \! Processing Code           C  3 
             Right$("            "+TS.TEMP4$+"00",12)              +       \! Valor de la compra        C 12
             Right$("    "+Gr.CrdSoc.MerTyp$,4)                    +       \! Merchant Type             C  4
             "  0"                                                 +       \! Entry Mode 0=Scanner      C  3
             Gr.CrdSoc.PosId$                                      +       \! Pos conditions ID         C  2
             Left$(Gr.CrdSoc.Aquid$+String$(20," "),20)            +       \! Acquirind Id Code         C 20
             Left$(Xcampo8$+String$(20," "),20)                    +       \! Acquirind Id Code         C 20
             Left$(Gr.CrdSoc.NomAlm$+String$(25," "),25)           +       \! Nombre del almacen        C 25
             Left$(Gr.CrdSoc.CodCiud$+String$(08," "),08)          +       \! Codigo de la ciudad       C  8
             Left$(Gr.CrdSoc.CodDpto$+String$(03," "),03)          +       \! Codigo departamento       C  3
             Left$(Gr.CrdSoc.CodPais$+" ",02)                      +       \! Codigo del pais           C  2
             Right$("   "+Gr.CrdSoc.CurCod$,3)                     +       \! Currency Code             C  3
             "00000000"                                            +       \! Pin Code                  C  8
             Left$(Gr.Oper.Name$+String$(20," "),20)               +       \! Nombre del cajero         C 20
             Right$("          "+Xtmp$,10)                         +       \! mmddhhmmss trx original   C 10
             Right$("      "+Gr.CrdSoc.RtaLinix$(Xpos%,1),6)                ! Nro Autorizacion          C  6                       
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
			 Gr.CrdSoc.Capt% = 0																									! Proceso No satisfactorio
			 Exit Sub 																														! Sale del proceso
    EndIf																																		! Proceso satistactorio
    Gr.CrdSoc.Capt% = -1																										! Proceso satisfactorio
    Gr.CrdSoc.Nro$  = MID$(XRtrama$,54,6)                                   ! Nro de autorizacion
    Call TSHIECET																														! Tono Alerta
    Call VISORES4690(1,"AUTORIZADO",Gr.CrdSoc.Nro$,1200,"L")                ! Msg al cajero
    Gr.CrdSoc.Qty% = Gr.CrdSoc.Qty% + 1 																		! Aumenta apuntador
    Gr.CrdSoc.RtaLinix$(Gr.CrdSoc.Qty%,0) = Gr.CrdSoc.IdClte$               ! Id del cliente
    Gr.CrdSoc.RtaLinix$(Gr.CrdSoc.Qty%,1) = Gr.CrdSoc.Nro$                  ! Nro de autorizacion
    Gr.CrdSoc.RtaLinix$(Gr.CrdSoc.Qty%,2) = TS.TEMP4$                       ! Valor de la compra
    Gr.CrdSoc.RtaLinix$(Gr.CrdSoc.Qty%,3) = Xfunc$                          ! Compra o Anul
    Gr.CrdSoc.RtaLinix$(Gr.CrdSoc.Qty%,4) = Gr.CrdSoc.Fwdid$                ! Cedula tarjeta habiente
    Gr.CrdSoc.RtaLinix$(Gr.CrdSoc.Qty%,5) = HORA.MUNDIAL$                   ! FECHA Y HORA TRX
    Exit Sub 																																!
 EndIf Else Begin 
    If Len(Xrtrama$) > 9 Then \
	     Call VISOR.And.BORRAR(Mid$(XRtrama$,14,40)) Else                    \! Msg de Error	
	     Call VISOR.And.BORRAR("ERROR EN COMUNICACIONES")                     ! Msg de Error	
 EndIf
 Gr.CrdSoc.Capt% = 0																									      ! Proceso No satisfactorio
End Sub																																			!

Sub Gr.Carga.Almacen																												! Carga nombre almacen
  TS.ER.RETURN = -1																													! Control de errores
  Open "R::C:\ADXALMA.DAT" AS 94	UNLOCKED NOWRITE NODEL       							! Apertura archivo parametrizacion
  If TS.ER.RETURN <> -1 Then BEGIN                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA ADXALMA      ")									! MSg alerta
  	 Gr.CrdSoc.NomAlm$ = "NOMBRE ALMACEN NO CARGADO"
  	 Exit Sub 																															! Sale del proceso
  ENDIF 																																		!
  IF END #94 THEN UE.FIN.ADXALMA        																	  ! Si es EOF                        
  Read #94; TS.TEMP1$     																			       			! Lectura registro                 
  Gr.CrdSoc.NomAlm$ = Mid$(TS.TEMP1$,14,25)          				        				! Nombre almacen
  
  UE.FIN.ADXALMA:
    Close 94

End Sub 																																		! Fin carga nombre almacen


Sub CREDITOSOCIAL(XOPT%) Public			 																				! Credito Social 
Integer*4 Xopt%, Xloc%                                                      !
String    Xtmp$

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.CrdSoc.Ok%  = 0                                                        ! Proyecto Apagado
  Gr.CrdSoc.dat% = 0
  TS.ER.RETURN = -1																													! Ctrl Errores
  Dim Gr.CrdSoc.RtaLinix$(35,5)																							!
  Gr.CrdSoc.Qty% = 0
  Gr.CrdSoc.Ncopia% = 0																											!
  Open "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL               							! Apertura archivo parametrizacion
  If TS.ER.RETURN <> -1 Then BEGIN                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA CUPO EMPLEADO")									! MSg alerta
  	 Exit Sub 																															! Sale del proceso
  ENDIF 																																		!
  IF END #94 THEN UE.FIN.CRDEMP         																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        IF TS.TEMP1$ = "[CREDITO SOCIAL]" Then Begin		  	        				! Credito Social
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.CrdSoc.Ok%   = Val(Mid$(TS.TEMP1$,30,2))   				    					! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdSoc.Key%  = Val(Mid$(TS.TEMP1$,30,03))      				          ! Tecla motora secuencia
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdSoc.TipVar$ = Mid$(TS.TEMP1$,30,02)          				        ! Tipo y variedad pago registro
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdSoc.Prcode$ = Mid$(TS.TEMP1$,30,03)          				        ! Processing Code venta
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdSoc.Prcdrv$ = Mid$(TS.TEMP1$,30,03)          				        ! Processing Code reverso
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdSoc.PrcdAn$ = Mid$(TS.TEMP1$,30,03)          				        ! Processing Code anulacion
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdSoc.MerTyp$ = Mid$(TS.TEMP1$,30,04)          				        ! Merchant Type
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdSoc.Aquid$  = Mid$(TS.TEMP1$,30,20)          				        ! Acquiring Id Code
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdSoc.CurCod$ = Mid$(TS.TEMP1$,30,03)          				        ! Currency Code
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdSoc.Ncopia% = Val(Mid$(TS.TEMP1$,30,02))      				        ! Numero de copias voucher
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdSoc.NomAlm$ = Mid$(TS.TEMP1$,30,25)          				        ! Nombre almacen
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdSoc.CodPais$ = Mid$(TS.TEMP1$,30,02)          				        ! Codigo del Pais
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdSoc.CodDpto$ = Mid$(TS.TEMP1$,30,03)          				        ! Codigo del Departmento
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdSoc.CodCiud$ = Mid$(TS.TEMP1$,30,08)          				        ! Codigo de la ciudad
         
         GoTo UE.FIN.CRDEMP  																								! Sale del ciclo de carga          
       Endif                                                                !
   Wend                                                                     !
   UE.FIN.CRDEMP:                                                           !
     Close 94																																! Cierra archivo
   Call Gr.Carga.Almacen
   Call GR.DISENO.VOUCHERCS																									! Carga diseño vocuher
   If Gr.CrdSoc.Ok% = -1 Then                                              \! Proyecto Activo
      Call U.IMPRIME("MODULO CRD SOCIAL    ON  09-Sep-2022",2100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO CRD SOCIAL    OFF 09-Sep-2022",2100H)          ! Msg Proyecto Cargado
EndIf 																																			! Fin carga opciones

If Gr.CrdSoc.Ok% <> -1 Then Exit Sub                                        ! Si proyecto apagado

!--- EAMTSU02.J86
If Xopt% = 02 Then Begin                                                    ! Recuperacion trx
	 Call IMPRESION.VOUCHER.CRDSOC																					  ! Generacion Voucher
	 Gr.CrdSoc.Capt% = 0																											! Init variables
   Gr.CrdSoc.VlrCompra$ = ""																								!
   Gr.CrdSoc.IdClte$ = ""																										!
   Gr.CrdSoc.dat% = 0
   Gr.CrdSoc.Nro$ = ""
   Dim Gr.CrdSoc.RtaLinix$(35,5)
   Gr.CrdSoc.Qty% = 0 
EndIf																																				! Fin recuperacion trx

!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                    ! Secuencias de tecleo
 If Gr.CrdSoc.Qty% <> 0 Then                                               \!
 If	TS.IO.KEYS(1) = 70 And TS.IO.MOTORKEY = 81 Then Begin                   ! Si anulacion total
 	If (TS.TOTALS(0,0,0) <> TS.BALDUE(0)) Then Begin													! Si pagos capturados
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
   	   TS.GUIDANCE = 1069																										! Msg Error appl nativa
	EndIf
 EndIf

 If TS.IO.MOTORKEY = Gr.CrdSoc.Key% Then Begin 														  ! Si tecla motora
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
    Gr.CrdSoc.Fwdid$ = ASIC.DATOS$("INGRESE CEDULA DEL","AFILIADO...")      ! Captura credencial
    If Gr.CrdSoc.Fwdid$ = "E" Then BEGIN 																		! Proceso cancelado
    	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")											!
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
    EndIf 																																	!
!-- Add 14Nov2018    
    Gr.CrdSoc.TipCrd$ = ASIC.DATOS$("INGRESE TIPO CRED.","1.ROTATIVO 2.ELECTRO")      ! Captura credencial
    If Val(Gr.CrdSoc.TipCrd$) < 1 Or Val(Gr.CrdSoc.TipCrd$) > 2  Then BEGIN	! Proceso cancelado
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
    Gr.CrdSoc.IdClte$ = TS.TEMP5$																						! Reporta numero de credencial
    If (TS.IO.KEYS(1) = 70) Then Begin 																			! En anulacion
       Gr.CrdSoc.AutAnul$ = ASIC.DATOS$("INGRESE AUTORIZACION","A ANULAR...") ! Captura Autorizacion
       If Gr.CrdSoc.AutAnul$ = "E" Then BEGIN 														  ! Proceso cancelado
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
    Gr.CrdSoc.Capt% = 0																											! Control de proceso
    If TS.TEMP6$ = "85" Then Begin
       Call COMPRACRDSOCIAL(TS.TEMP6$,"0")																	! Procesa requerimiento Compra
    EndIf
    If TS.TEMP6$ = "86" Then Begin
       Call COMPRACRDSOCIAL(TS.TEMP6$,Gr.CrdSoc.AutAnul$)		 							  ! Procesa requerimiento Anulacion
    EndIf

    If Gr.CrdSoc.Capt% = -1 Then Begin 
      Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 															! Init vectores de carga
      If TS.TEMP6$ = "86" Then TS.IO.KEYS(1) = 70    												! Funcion de anulacion
      TS.IO.DATA$(3) = Right$(Gr.CrdSoc.TipVar$,1)                    			! Variedad de Pago
      TS.IO.KEYS(3)  = 78                                             			! Tecla Slash
      TS.IO.DATA$(7) = TS.TEMP4$                              							! Valor a entregar
      TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.CrdSoc.TipVar$,1))           			! Tipo de Pago
      TS.IO.DATA$(9) = TS.TEMP5$																						! Nit asociado
      TS.IO.MOTORKEY = TS.IO.KEYS(7)																				!
      Gr.CrdSoc.VlrCompra$ = TS.TEMP4$                                      ! Valor de la comnpra
      Gr.CrdSoc.IdClte$ = TS.TEMP5$     																	  ! Id del cliente
      Exit Sub 
    EndIf Else Begin
      Gr.CrdSoc.VlrCompra$ = ""                                             ! Valor de la comnpra
      Gr.CrdSoc.IdClte$ = ""              															    ! Id del cliente
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
    Gr.CrdSoc.Qty% = Gr.CrdSoc.Qty% + 1 																		! Numero de vouchers
    Xloc% = Gr.CrdSoc.Qty%
    TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                   ! Numero de proyecto
    Gr.CrdSoc.RtaLinix$(Xloc%,0) = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)! Credencial beneficiario
    Gr.CrdSoc.RtaLinix$(Xloc%,1) = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)! Nro Autorizacion
    Gr.CrdSoc.RtaLinix$(Xloc%,2) = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)! Valor de la compra
    Gr.CrdSoc.RtaLinix$(Xloc%,3) = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)! Compra / Anulacion      
    Gr.CrdSoc.RtaLinix$(Xloc%,4) = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)! Cedula tarjeta habiente 
    Gr.CrdSoc.RtaLinix$(Xloc%,5) = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)! Fecha y hora de la trx
    Gr.CrdSoc.TipCrd$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)           ! tipo de credito
    Gr.CrdSoc.IdClte$ = Gr.CrdSoc.RtaLinix$(Xloc%,0)                        !
    Gr.CrdSoc.Nro$ = Gr.CrdSoc.RtaLinix$(Xloc%,1)                           !
    Gr.CrdSoc.Capt% = -1																										! Capturado credito social
 Endif
EndIf	

EndIf																																				! Fin recuperacion trx

!-- EAMTSU60
If Xopt% = 60 Then Begin																										! En la impresion del CR
	
  If TS.LINETYPE = 2 And  Gr.CrdSoc.Capt% = -1 THEN BEGIN										! Impresion pago y credito social
      To.USEREXIT(20) = 0
      To.USEREXIT(60) = 0
      TS.TEMP1$ = Left$("AFIL:"+Gr.CrdSoc.IdClte$+" AUT:" + Gr.CrdSoc.Nro$+String$(37," "),37)
      Call U.IMPRIME(TS.TEMP1$,6100H)
      If Gr.CrdSoc.TipCrd$ = "1" Then TS.TEMP1$ = "TIPO CREDITO: ROTATIVO     "
      If Gr.CrdSoc.TipCrd$ = "2" Then TS.TEMP1$ = "TIPO CREDITO: ELECTRO HOGAR"
      Call U.IMPRIME(TS.TEMP1$,6100H)
      Gr.CrdSoc.Capt% = 0
      If Not(TS.RECOVERY) Then Begin

          TS.TEMP1$ = Pack$(Gr.CrdSoc.RtaLinix$(Gr.CrdSoc.Qty%,0)) + ":" + \! Id del cliente
                           (Gr.CrdSoc.RtaLinix$(Gr.CrdSoc.Qty%,1)) + ":" + \! Nro de autorizacion
                      Pack$(Gr.CrdSoc.RtaLinix$(Gr.CrdSoc.Qty%,2)) + ":" + \! Valor de la compra
                      Pack$(Gr.CrdSoc.RtaLinix$(Gr.CrdSoc.Qty%,3)) + ":" + \! Compra / Anulacion
                      Pack$(Gr.CrdSoc.RtaLinix$(Gr.CrdSoc.Qty%,4)) + ":" + \! Cedula tarjeta habiente
                      Pack$(Gr.CrdSoc.RtaLinix$(Gr.CrdSoc.Qty%,5)) + ":" + \! Fecha y hora trx 
                      Pack$(Gr.CrdSoc.TipCrd$)															! Tipo de credito
          Call Grabacion.Cadena.Usuario("20150917",TS.TEMP1$)               ! Almacena UD Credito social

       EndIf
       To.USEREXIT(20) = -1
       To.USEREXIT(60) = -1
       
   EndIf

EndIf																																				! Fin EAMTSU60


End Sub 
