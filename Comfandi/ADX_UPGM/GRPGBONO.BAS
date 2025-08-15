!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRPGBONO.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Pago Bonos Recompra              *
!**************************************************
!Observaciones:
! Version 1.0 03 Diciembre 2019
!--------------------------------------------------
! Mod 10ENE2019
! Se ajusta la generación del numero del bono para
! evitar duplicidad de movimientos.
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 27Ene2020
! Se controla el proceso de suspensión de transaccion
! y retorno de las variables para los controles de 
! autorización de supervisor en los reintegros dentro
! de la compra.
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 06Ago2020
! Se adiciona proceso para captura manual del bono
! recompra
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 10Ago2020
! Se ajusta proceso de captura de bono de formato     
! pdf-417 a EAN-128 para optimizar proceso.
! desarrollado Grupo Retail - OVS
!--------------------------------------------------
! Mod 14Dic2021
! Se controla el proceso de registro del bono recompra
! si al ingresar el numero del bono no se ha ingresado 
! la tecla total anteriormente para evitar que se 
! descuadre el total de la compra.
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 14Feb2022
! Se ajusta registro pago bono recompra por problemas
! reportados en la grabación del pago con reintegro 
! de productos dentro de la compra
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 14Dic2023
! Se ajusta aplicación forma de pago con procesos de
! reintegro y corrección descuadres de caja
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%ENVIRON T		                          																		! Ambiente de terminal

Integer*1 Global Gr.BrcPag.Ok%,          																	 \! Control del proyecto
                 Gr.BrcPag.TrxBon%,                                        \! Proceso pago bono
                 Gr.BrcPag.InTrx%,                                         \! Bono en la trx
                 Gr.BrcPag.AplAn%,                                         \! Proceso de anulacion
                 Gr.BrcPag.Captura%,                                       \! Forma captura bono
                 Gr.BrcPag.Capt%                                            ! Bono capturado
Integer*1 Global TS.RECOVERY           																			! terminal recovery flag                 
Integer*2 Global GR.BRNEGCNT(1)                                             !
Integer*4 Global Gr.BrcPag.Motora%,                                        \! Tecla procedimiento
                 Gr.BrcPag.Slend%,                                         \!
                 Asc.Pay.Impr%,                                            \!
                 Gr.BrcPag.TVPOS%,                                         \!
                 Gr.BrcPag.MaxTv%,                                         \! Maximo pago   tipo y variedad
                 Gr.BrcPag.ChgTv%,                                         \! Maximo cambio tipo y variedad
                 GR.BRNEGAMT(1),                                           \!
                 Asc.Tmp.Apun%                                              ! Apuntador String
String    Global Gr.BrcPag.TipVar$,                                        \! Tipo y variedad de pago
                 Gr.BonPag.Prefijo$,                                       \! Prefijo bonos recompra
                 Gr.BrcPag.Captura$,                                       \!
                 Gr.BrcPag.Valor$,                                         \! Valor del bono
                 Gr.BrcPag.DatTmp$,                                        \! Temporal bono
                 Gr.Lcl.Clte$,                                             \! Id del cliente
                 Hora.Mundial$,                                            \! Fecha y hora para confirmacion
                 Gr.BrcPag.Nro$                                             ! Numero bono capturado
Integer  Global  USER.FBACT.READ                                            ! SUCCESSFUL READ OF FILE
Integer  Global  SL.END                                                     ! NUMBER TOTAL STRINGS
Integer*1 Global Gr.BrcPag.ValTotp%
String    Global Gr.BrcPag.Token$,                                         \! Nro token 
                 Gr.BrcPag.Celular$																					! Nro Celular 

%INCLUDE EAMTSWKG.J86          																			        ! working storage
%INCLUDE EAMTRANS.J86          																			        ! working storage
%INCLUDE EAMITEMR.J86          																			        ! Maestra productos
%INCLUDE EAMTOPTS.J86          																			        ! working storage
%INCLUDE EAMTSXHC.J86                                              					! Rutinas de Assembler            
%INCLUDE EAMASMRT.J86                                              					! Rutinas de Assembler            

%INCLUDE RECATSSU.011          																			        ! Rutinas Genericas Grupo Retail

Function FORMAT.AMOUNT(AMT1) External   																		! Formateo de valores
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub TSHIECET EXTERNAL
End Sub

Sub  TSCSEC03 External
End Sub 

Sub TSTDEC01 EXTERNAL          ! tendering
End Sub 

Sub TSBDEC01 EXTERNAL          ! balance due (normal/foodstamp)
End Sub                        !

Function VALIDA.COMPRA.UVT External 
Integer*1 VALIDA.COMPRA.UVT
End Function 

Sub CREAR.PIPE.PDF External																									! Control lectura pipes
End Sub																																			!

Sub PGBONO.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
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

Function BARRA417.TOTP.BONOS
String Xlen$, BARRA417.TOTP.BONOS, Xsnd$, Xrta$, Xfin$, Xtrama$, Xtemp4$
String Xid$
   Xlen$ = Right$("0000"+TS.TEMP1$,4)
   Call VISORES4690(1,"TOMANDO DATOS  PIPE","ESPERE POR FAVOR",0,"L")
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Asc.Pay.Impr% = 2
   Xrta$ = Armar.Trama.Msg("10","41",Xlen$,"00","0001","123456")  					! Armar trama MSG
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", Xrta$)         	! Ejecuta Requerimiento
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call PGBONO.AUDITORIA(Xrta$,Xtrama$, Xsnd$, Xfin$)                    		! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
      BARRA417.TOTP.BONOS = ""
   	  Exit Function 
   EndIf
   Xid$ = Mid$(XTrama$,54,170)

   Xtemp4$ = Str$(Val(mid$(Xid$,15,12))) 																		! Numero cedula persona
   Xfin$ = Asic.Datos$(Xtemp4$,"CONFIRMA?  1.Si 0.No")
   If Xfin$ = "E" Then BEGIN 																		    				! Proceso cancelado   
   	  Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  		!
      BARRA417.TOTP.BONOS = ""
      Exit Function 																								    		!
   EndIf 																														    		!
   If Xfin$ <> "1" Then Begin
      Call VISOR.AND.BORRAR("DATO CAPTURADO NO ESVALIDO /Borrar")			  		!
      BARRA417.TOTP.BONOS = ""
      Exit Function 
  EndIf
  If Val(Xtemp4$) <> Val(Gr.Lcl.Clte$) Then Begin															! Si cliente capturado no es igual
     Call VISOR.AND.BORRAR("CLIENTE DIFERENTE ALCAPTURADO /Borrar ")        !  	
      BARRA417.TOTP.BONOS = ""
      Exit Function 
  EndIf
  BARRA417.TOTP.BONOS = Xid$																								! Retorna la trama

End Function 

Function Validacion.Token.Bonos Public 																			! Validacion token
Integer*1 Validacion.Token.Bonos
String    Xsnd$, Xbuffer$, XTemp4$, Xtrama$, Xfin$                          !
 Validacion.Token.Bonos = 0																									! Proceso fallido
 Gr.BrcPag.Token$ = ASIC.DATOS$("INGRESE NUMERO TOKEN","PARA VALIDACION....")!
 If Gr.BrcPag.Token$ = "E" Then BEGIN 													    					! Proceso cancelado   
 	  Gr.BrcPag.Token$ = ""
    Validacion.Token.Bonos = 0																							! Proceso fallido
    Exit Function 																											   	!
 EndIf 																														    			!

  XTemp4$ = Gr.BrcPag.Captura$
  Gr.BrcPag.Celular$ = Left$(XTemp4$,10)																	  ! Numero de celular capturado
  Gr.BrcPag.Captura$ = Str$(Val(Mid$(Xtemp4$,15,12)))                       ! Numero de cedula beneficiario
  
 Call TSHIECET
 Call VISORES4690(1,"VALIDANDO TOKEN ","ESPERE POR FAVOR..",0,"L")
 Asc.Pay.Impr% = 0																													! Numero actual de trx
 Xsnd$ = DATE$ +":"+ Time$                                                  ! Fecha y hora del requerimiento
 Xbuffer$ = Right$("          "+Gr.BrcPag.Token$,10) +                     \! Numero de token ajustado
            Right$("                "+Gr.BrcPag.Celular$,16)  +            \! Numero de celular ajustado
            Right$("            "+Gr.BrcPag.Captura$,12)                    ! Numero de cedula del beneficiario
            
 XTemp4$ = Armar.Trama.Msg("10","53",Xbuffer$,"00","0001","123456")         ! Armar trama MSG CC 
 XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)           ! Ejecuta Requerimiento
 
 !Xtrama$ = "1052046200000CODIGO TOTP CORRECTO                    "
 
 Xfin$ = DATE$ +":"+ Time$                                                  ! Fecha y hora rta del requerimiento
 Call PGBONO.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                       ! Rastreo movimiento
 XTEMP4$ = Valida.Rta(XTrama$)																			        ! Valida rta entregada
 If Xtemp4$ <> "00" Then Begin 
    Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									            ! Presenta Msg Error
    Exit Function 
 EndIf																																			!
 XTEMP4$ = Mid$(XTrama$,12,2)	           																    ! Valida rta entregada
 If XTemp4$ <> "00" Then Begin 																					    ! Rta Satisfactoria
    Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									            ! Presenta Msg Error
    Exit Function 
 EndIf
 Validacion.Token.Bonos = -1																									! Proceso exitoso
End Function 																																! fin validacion token

Sub BONOS.BUSCA.LIMITE.TV(UE.TIPO.VAR$)
  Integer*2 UE.I%
  Integer*1 UE.FOUND%
  String UE.TIPO.VAR$
  UE.I% = 1
  UE.FOUND% = 0 
  Gr.BrcPag.MaxTv% = 0
  Gr.BrcPag.ChgTv% = 0 
  While UE.I% <= TO.NUMTNDR AND NOT UE.FOUND%
    If TO.TENDOPTS(UE.I%,0) = VAL(LEFT$(UE.TIPO.VAR$,1)) AND  \
       TO.TENDOPTS(UE.I%,1) = VAL(RIGHT$(UE.TIPO.VAR$,1)) Then Begin 
       Gr.BrcPag.MaxTv% = TO.TENDLIMITS(UE.I%,0)
       Gr.BrcPag.ChgTv% = TO.TENDLIMITS(UE.I%,1)
       UE.FOUND%  = -1
       Gr.BrcPag.TVPOS% = UE.I%
    EndIf Else UE.I% = UE.I% + 1
  Wend    
  UE.I% = 0
  For UE.I% = 0 To 7
      GR.BRNEGCNT(UE.I%) = To.NEGCNT(UE.I%)    
      GR.BRNEGAMT(UE.I%) = To.NEGAMT(UE.I%)    
  Next UE.I%
End Sub

Function BONOS.PDF417 
String Xlen$, BONOS.PDF417, Xsnd$, Xrta$, Xfin$, Xtrama$, Xtemp4$
String Xid$
   Xlen$ = Right$("0000"+TS.TEMP1$,4)
   Call VISORES4690(1,"TOMANDO BONOS  PIPE","ESPERE POR FAVOR",0,"L")
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Asc.Pay.Impr% = 1
   Xrta$ = Armar.Trama.Msg("10","41",Xlen$,"00","0001","123456")  					! Armar trama MSG
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", Xrta$)         	! Ejecuta Requerimiento
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call PGBONO.AUDITORIA(Xrta$,Xtrama$, Xsnd$, Xfin$)                    	  ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
      BONOS.PDF417 = ""
   	  Exit Function 
   EndIf
   BONOS.PDF417 = Mid$(Xtrama$,54,45) 																			! Retorna dato escaneado
End Function 

Function BUSQUEDA.PAGO.BONOS(Xfnd$)
Integer*1 BUSQUEDA.PAGO.BONOS
String XR$, Xtemp4$, Xtrama$,Xsnd$, Xfin$, Xfnd$									!
String Xbuffer$, Xbonos$(2), Xtmp$, xbon$, Xvlr$, Xid$, Xsgn$								!
String xenvia$, xsale$, xrta$, xllega$, xmov$														    !
Integer*4 Xpos%, Xloc%, Xj%, Xi%
   Xpos% = 0
   Xloc% = 0
   For XI% = 1 TO SL.END                                  									! FOR ALL StringS
       H$ = READ.SL.STR$(XI%)                              									! GET String
       If LEN(H$) > 5 Then Begin                          									! ASSURE GOOD String
        If ASC(H$) = 153 Then Begin                        									! User Data String
     	     Asc.Tmp.Apun% = 3 																							  ! Init apuntador String 
     	     Xtmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													  ! numero del proyecto
     	     If Xtmp$ = "20191128" Then Begin																  ! Pago bono recompra
     	     	  Xbon$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! numero del bono
     	     	  Xvlr$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! valor del bono
     	     	  Xid$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! cliente
              xmov$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! aammddhhmmss movimiento
              Xsgn$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! signo operacion
              If (Xfnd$ = XBON$) And (Xsgn$ = "00") Then Begin              ! Encuentra y positivo
    	     	    	Xloc% = -1 																								! Lo encontro
    	     	    	TS.TEMP3$ = XMOV$                                         ! Retorna datos de uso
    	     	  EndIf																													! Fin encontrado
        	 EndIf																														! fin String pago bono
        EndIf																																! Fin User data
       EndIf																																! Fin String tlog
   Next XI%																																	! Fin barrido tlog
   If Xsgn$ = "00" Then                                                    \! Si encuentra bono en pago 
      BUSQUEDA.PAGO.BONOS = Xloc% Else                                     \!
      BUSQUEDA.PAGO.BONOS = 0
End Function 

Sub VALIDA.PAGO.BONO(Xbono$,Xfunc$)																					! Validacion bono
String Xbono$, Xfunc$, Xbuffer$, XTemp4$, xenvia$, xsale$, xrta$, xllega$   !
String XTrama$, XMonto$
Integer*1 Xrta%
 
 TS.TEMP1I1 = 0																															! Control proceso
 If Gr.BrcPag.AplAn% = -1 Then Begin																				! Si es anulacion bono
    Call VISORES4690(1,"ANULANDO  BONO","ESPERE POR FAVOR..",0,"L")					! Msg Operador
 	  Xrta% = BUSQUEDA.PAGO.BONOS(Left$(XBONO$,29))                           ! Busca 
    If Xrta% = 0 Then Begin 																								! No encontrado en trx
       Call VISOR.AND.BORRAR("BONO A ANULAR NO    EXITE EN LA COMPRA")	
       Exit Sub
    EndIf
    Xbuffer$ = Left$(Xbono$,29) +                                          \!
               Right$("0000"+TS.STORE$,4)              +                   \! Numero del almacen
               Right$("000000"+Str$(SL.HD.TERMINAL),6) +                   \! Numero de terminal
               Right$("000000"+Str$(SL.HD.TRANSNUM+1),6) +                 \! Numero de transaccion
               TS.TEMP3$    																				        ! Trama de anulacion
 EndIf Else Begin																														! Fin anulacion bono
 	  Call VISORES4690(1,"VALIDANDO BONO","ESPERE POR FAVOR..",0,"L")				  ! Msg Operador
    Xbuffer$ = Right$("000000000000000"+Gr.Lcl.Clte$,15) +                 \! ID del cliente
               Left$(Xbono$,29)                          +                 \! Nro del bono
               Right$("0000000000"+Gr.BrcPag.Valor$,10)                     ! Valor del bono
 EndIf
 Asc.Pay.Impr% = 1
 XTemp4$ = Armar.Trama.Msg("16",Xfunc$,(Xbuffer$),"00","0001","123456")     ! Armar trama MSG
 Xenvia$ = XTemp4$																											    !
 Xsale$  = DATE$ +":"+ Time$																							  ! Hora salida requerimiento
 XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)           ! Ejecuta Requerimiento

!Xtrama$ = "1666072200000CONSULTA SATISFACTORIA                  202312142023121583040     "
 
 XTEMP4$   = Valida.Rta(XTrama$)																			      ! Valida rta entregada
 Xrta$     = DATE$ +":"+ Time$																						  ! Hora llegada requerimiento
 Xllega$   = Xtrama$																										  	!
 Call  PGBONO.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                     ! Rastro auditoria
 If Xtemp4$ = "00" Then Begin 																						  ! Proceso satisfactorio
    If Mid$(XTrama$,12,2) <> "00" Then Begin 																! Con Error de ejecucion 
    	 Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))														! Presenta mensaje de Error 
    	 TS.TEMP1I1 = 0																												! Control proceso
   	   Exit Sub      																												! Sale del proceso
    EndIf	  																																!
 EndIf Else Begin																														! Falla proceso
 	  Call VISOR.AND.BORRAR("ERROR VALIDACION BONO")													!
 	  TS.TEMP1I1 = 0																													! Control proceso
 	  Exit Sub 																																!
 EndIf																																	    !
 If Gr.BrcPag.AplAn% = 0 Then Begin																				  ! Si es consumo bono
    TS.TEMP1$ = MID$(XTrama$,56,6)                                          ! Fecha Inicial validez
    TS.TEMP2$ = MID$(XTrama$,64,6)                                          ! Fecha final validez
    If Val(DATE$) >= Val(TS.TEMP1$) And                                    \! Si bono en el periodo valido
       Val(DATE$) <= Val(TS.TEMP2$) Then Begin												      ! 
       
    EndIf	Else Begin
       Call VISOR.AND.BORRAR("FECHA VIGENCIA DEL  BONO NO AUTORIZADA")      !
       TS.TEMP1I1 = 0																												! Control proceso
       Exit Sub 																														!
    EndIf																																	  !
 EndIf
 Gr.BrcPag.Nro$ = Left$(Xbono$,29)
 TS.TEMP1I1 = -1																														! Proceso exitoso
End Sub 																																		! Fin validacion bono

Function CONFIRMA.CONSUMO.BONO																							! Confirma consumo Bono
String Xbono$, Xfunc$, Xbuffer$, XTemp4$, xenvia$, xsale$, xrta$, xllega$   !
String XTrama$
Integer*1 CONFIRMA.CONSUMO.BONO
 CONFIRMA.CONSUMO.BONO = 0
 If Gr.BrcPag.AplAn% = -1 Then Begin
 	  CONFIRMA.CONSUMO.BONO = -1  																						! Control proceso
 	  Exit Function 
 EndIf
 Call VISORES4690(1,"CONFIRMANDO CONSUMO","ESPERE POR FAVOR",0,"L")         ! 
 Xbuffer$ = Gr.BrcPag.Nro$                                                  ! Nro del bono
 Asc.Pay.Impr% = 1
 XTemp4$ = Armar.Trama.Msg("16","67",(Xbuffer$),"00","0001","123456")       ! Armar trama MSG
 Xenvia$ = XTemp4$																											    !
 Xsale$  = DATE$ +":"+ Time$																							  ! Hora salida requerimiento
 XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)           ! Ejecuta Requerimiento
 
 !Xtrama$ = "1667046200000BONO ACTUALIZADO SATISFACTORIAMENTE     "
 
 XTEMP4$   = Valida.Rta(XTrama$)																			      ! Valida rta entregada
 Xrta$     = DATE$ +":"+ Time$																						  ! Hora llegada requerimiento
 Xllega$   = Xtrama$																										  	!
 Call  PGBONO.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                     ! Rastro auditoria
 If Xtemp4$ = "00" Then Begin 																						  ! Proceso satisfactorio
    If Mid$(XTrama$,12,2) <> "00" Then Begin 																! Con Error de ejecucion 
    	 Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))														! Presenta mensaje de Error 
    	 CONFIRMA.CONSUMO.BONO = 0																						! Control proceso
   	   Exit Function 																												! Sale del proceso
    EndIf	  																																!
 EndIf Else Begin																														! Falla proceso
 	  Call VISOR.AND.BORRAR("ERROR CONFIRMANDO BONO")												  !
 	  CONFIRMA.CONSUMO.BONO = 0																								! Control proceso
 	  Exit Function																														!
 EndIf																																	    !
 CONFIRMA.CONSUMO.BONO = -1  																								! Control proceso
End Function																																! Fin confirma consumo bono

Sub CONFIRMAR.PAGO.BONOS
String XR$, Xtemp4$, Xtrama$,Xsnd$, Xfin$												            !
String Xbuffer$, Xbonos$(2), Xtmp$, xbon$, Xvlr$, Xid$, Xsgn$								!
String xenvia$, xsale$, xrta$, xllega$, xmov$														    !
Integer*4 Xpos%, Xloc%, Xj%, Xi%
   Dim Xbonos$(100,2)                                                       !
   Xpos% = 0
   For XI% = 1 TO Gr.BrcPag.Slend%                        									! FOR ALL StringS
       H$ = READ.SL.STR$(XI%)                              									! GET String
       If LEN(H$) > 5 Then Begin                          									! ASSURE GOOD String
        If ASC(H$) = 153 Then Begin                        									! User Data String
     	     Asc.Tmp.Apun% = 3 																							  ! Init apuntador String 
     	     Xtmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													  ! numero del proyecto
     	     If Xtmp$ = "20191128" Then Begin																  ! Pago bono recompra
     	     	  Xbon$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! numero del bono
     	     	  Xvlr$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! valor del bono
     	     	  Xid$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! cliente
              xmov$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! aammddhhmmss movimiento
              Xsgn$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! signo operacion
     	     	  Xloc% = 0
     	     	  For Xj% = 1 To Xpos%																					! Busqueda de bono
     	     	    If Xbonos$(Xj%,0) = Xbon$ Then                             \! Bono encontrado
     	     	    	 Xloc% = Xj%																							!
     	     	  Next Xj%																											!
              If Xloc% = 0 Then Begin                                       ! No Existe
              	 Xpos% = Xpos% + 1
              	 Xbonos$(Xpos%,0) = Xbon$
              	 If Xsgn$ = "01" Then Xvlr$ = Str$(Val(Xvlr$) * -1)         ! Si anulacion
              	 Xbonos$(Xpos%,1) = Xvlr$                                   !
                 Xbonos$(Xpos%,2) = Right$("0000"+TS.STORE$,4)  +          \! Numero del almacen
                                    Right$("000000"+Str$(SL.HD.TERMINAL),6) + \! Numero de terminal
                                    Right$("000000"+Str$(SL.HD.TRANSNUM),6) + \! Numero de transaccion
                                    Xmov$                                   ! Fecha y Hora de la operacion AAMMDDHHMMSS
              EndIf Else Begin																							! Bono ya existe
              	 If Xsgn$ = "01" Then Xvlr$ = Str$(Val(Xvlr$) * -1)         ! Si anulacion
              	 Xbonos$(Xloc%,1) = Str$(Val(Xbonos$(Xloc%,1)) + Val(Xvlr$))!
                 Xbonos$(Xloc%,2) = Right$("0000"+TS.STORE$,4)  +          \! Numero del almacen
                                    Right$("000000"+Str$(SL.HD.TERMINAL),6) + \! Numero de terminal
                                    Right$("000000"+Str$(SL.HD.TRANSNUM),6) + \! Numero de transaccion
                                    Xmov$                                   ! Fecha y Hora de la operacion AAMMDDHHMMSS
              EndIf																													!
        	 EndIf																														! fin String pago bono
        EndIf																																! Fin User data
       EndIf																																! Fin String tlog
   Next XI%																																	! Fin barrido tlog
   Xj% = 0
   If Xpos% > 0 Then \
   	  Call Visores4690(1,"ACTUALIZANDO BONOS    ","ESPERE POR FAVOR..",0,"L")
   For Xj% = 1 To Xpos%																											! Total bonos para actualizar
    If Val(Xbonos$(XJ%,1)) > 0 Then Begin																	  ! Bono para actualizar
     Xsnd$ = DATE$ +":"+ Time$                                              ! Fecha y hora del requerimiento     
     Xbuffer$ = Xbonos$(XJ%,0) + Xbonos$(XJ%,2)                             ! numero del bono + datos origen
     Asc.Pay.Impr% = 2                                                      ! Ajusta numero trx para seguimiento 
     XTemp4$ = Armar.Trama.Msg("16","68",Xbuffer$,"00","0001","123456")     ! Armar trama MSG CC              
     XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)       ! Ejecuta Requerimiento              

!     Xtrama$ = "1668046100000PROCESO SATISFACTORIO                   "
     
     Xfin$ = DATE$ +":"+ Time$                                              ! Fecha y hora rta del requerimiento 
     Call PGBONO.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                   ! Rastreo movimiento
     XTEMP4$ = Valida.Rta(XTrama$)																			    ! Valida rta entregada
     If Xtemp4$ <> "00" Then Begin																					!
        Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									        ! Presenta Msg Error                 
     EndIf                                                                  !
    EndIf																																		! Fin Bono actualizar                 
   Next Xj%
End Sub 

Sub CAPTURA.DATO.BONO																												!
String Xopt$     
! Xopt$ = ASIC.DATOS$("FORMA CAPTURA BONO  ","1.MANUAL 0.ESCANEADO")        ! Captura tipo proceso
! If Val(Xopt$) < 0 Or Val(Xopt$) > 1 Then BEGIN 					  						    ! Proceso cancelado   
! 	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")									  		!
!   Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																!
!   TS.IO.MOTORKEY = 73																											!
!   Exit Sub 																																!
! EndIf 																																    !

Xopt$ = "1"

If Xopt$ = "1" Then Begin																										! Ingreso Manual
   Gr.BrcPag.Captura$ = ASIC.DATOS$("INGRESE O ESCANEE ","BONO BENEFICIARIO.") !
   If Gr.BrcPag.Captura$ = "E" Then BEGIN															      ! Proceso cancelado   
 	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								      !
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      !
      TS.IO.MOTORKEY = 73																							      !
      Exit Sub 																												      !
   EndIf 																															      !
   Gr.BrcPag.Captura$ = Str$(Val(Gr.BrcPag.Captura$))

	 If Len(Gr.BrcPag.Captura$) <> 35 Then Begin
 	    Call VISOR.AND.BORRAR("NUMERO BONO NO VALIDO")								        !
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      !
      TS.IO.MOTORKEY = 73																							      !
      Exit Sub 																												      !
	 EndIf

   Gr.BrcPag.Valor$ = ASIC.DATOS$("INGRESE VLR. BONO.","DEL BENEFICIARIO..")!
   If Gr.BrcPag.Valor$ = "E" Then BEGIN															        ! Proceso cancelado
 	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								      !
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      !
      TS.IO.MOTORKEY = 73																							      !
      Exit Sub 																												      !
   EndIf 																															      !
	 If Val(Gr.BrcPag.Valor$) <= 0 Then Begin
 	    Call VISOR.AND.BORRAR("VALOR INGRESADO NO VALIDO")			              !
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      !
      TS.IO.MOTORKEY = 73																							      !
      Exit Sub 																												      !
	 EndIf
	 Gr.BrcPag.Valor$ = Right$("0000000000"+Gr.BrcPag.Valor$,10)							! 
   Gr.BrcPag.Captura$ = Gr.BrcPag.Captura$ + Gr.BrcPag.Valor$								! Ajusta datos mensajeria
	 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												          !
   TS.IO.MOTORKEY = 69 																											! Simula captura PDF
   Gr.BrcPag.TrxBon% = -1	 																									!
   Gr.BrcPag.Captura% = 2
EndIf
If Xopt$ = "0" Then Begin																										! 
 TS.ER.RETURN = -1																											    ! Ctrl errores
 Call CREAR.PIPE.PDF																										    ! Apertura pipe
 If TS.ER.RETURN = -1 Then Begin 																			      ! Proceso OK
    Call VISORES4690(1,"ESCANEE BONO  ","DEL BENEFICIARIO..",0,"L")         ! 
    If (TS.IO.KEYS(1) = 70) Then Begin 															        ! Anulando movimiento
    	  Gr.BrcPag.AplAn% = -1
    EndIf
    Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												        !
    TS.IO.MOTORKEY = 0 																						          !
    Gr.BrcPag.TrxBon% = -1
    Gr.BrcPag.Captura% = 1
    Exit Sub 																											          !
 EndIf
EndIf

End Sub 

Function DIALOGO.TOTP.BONOS
Integer*1 DIALOGO.TOTP.BONOS
String Xtmp$
      Entrada.Dato.Totp:                          
      Xtmp$ = ASIC.DATOS$("VALIDACION TOT 1.CEL","2.MANUAL 3.OTRO   ")      ! Tipo de validacion 
      If Xtmp$ = "E" Then BEGIN 																		        ! Proceso cancelado   
       	  Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  !
          DIALOGO.TOTP.BONOS = 0																				    !
          Exit Function 																								    !
      EndIf 																														    !
      If Match(Xtmp$," 1 2 3 ",1) <= 0 Then Begin
      	 Call VISOR.AND.BORRAR("DATO CAPTURADO NO ESVALIDO /Borrar")			  !
      	 GoTo Entrada.Dato.Totp
      EndIf
      DIALOGO.TOTP.BONOS = Val(Xtmp$)
End Function 

Sub BRCOMPRAPAGO(XOPT%) Public			 																				! Bono recompra pago
Integer*4 Xopt%                                                             !
Integer*1 Xrta%
String    Xtmp$

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.BrcPag.Ok%  = 0                                                        ! Proyecto Apagado
  TS.ER.RETURN = -1																													! Ctrl Errores
  Dim GR.BRNEGCNT(8)
  Dim GR.BRNEGAMT(8)
  Open "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL               							! Apertura archivo parametrizacion
  If TS.ER.RETURN <> -1 Then BEGIN                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA PROYECTO BONO")									! MSg alerta
  	 Exit Sub 																															! Sale del proceso
  EndIf 																																		!
  IF END #94 THEN UE.FIN.BRCPAG         																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        IF TS.TEMP1$ = "[RECOMPRA PAGO]" Then Begin		  	        				  ! Bono recompra pago
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.BrcPag.Ok%   = Val(Mid$(TS.TEMP1$,30,2))   				    					! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.BrcPag.TipVar$ = Mid$(TS.TEMP1$,30,02)          				        ! Tipo y variedad pago registro
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.BrcPag.Motora% = Val(Mid$(TS.TEMP1$,30,03))      				        ! Tipo y variedad pago registro
         Call BONOS.BUSCA.LIMITE.TV(Gr.BrcPag.TipVar$)
         GoTo UE.FIN.BRCPAG  																								! Sale del ciclo de carga          
       EndIf                                                                !
   Wend                                                                     !
   UE.FIN.BRCPAG:                                                           !
     Close 94																																! Cierra archivo
   If Gr.BrcPag.Ok% = -1 Then                                              \! Proyecto Activo
      Call U.IMPRIME("MODULO BRC PAGOS     ON  14-Dic-2023",2100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO BRC PAGOS     OFF 14-Dic-2023",2100H)          ! Msg Proyecto Cargado
EndIf 																																			! Fin carga opciones

If Gr.BrcPag.Ok% <> -1 Then Exit Sub                                        ! Si proyecto apagado

!--- EAMTSU02.J86
If Xopt% = 02 Then Begin                                                    ! Inicio de trx
	 Call CONFIRMAR.PAGO.BONOS
   Gr.BrcPag.Nro$ = ""
   Gr.BrcPag.Capt% = 0
   Gr.BrcPag.TrxBon% = 0
   Gr.BrcPag.Captura$ = ""
   Gr.BrcPag.AplAn% = 0
   Gr.BrcPag.Valor$ = ""
   Gr.BrcPag.Slend% = 0
   Gr.BrcPag.InTrx% = 0
   Gr.BrcPag.Captura% = 0
   Gr.BrcPag.DatTmp$ = ""
   To.TENDLIMITS(Gr.BrcPag.TVPOS%,0) = Gr.BrcPag.MaxTv%											! Retorna valores default
   To.TENDLIMITS(Gr.BrcPag.TVPOS%,1) = Gr.BrcPag.ChgTv%											! Retorna valores default
   TS.TEMP1I1 = 0
   For TS.TEMP1I1 = 0 To 7
       TO.NEGAMT(TS.TEMP1I1) = GR.BRNEGAMT(TS.TEMP1I1)
       TO.NEGCNT(TS.TEMP1I1) = GR.BRNEGCNT(TS.TEMP1I1)
   Next TS.TEMP1I1
EndIf																																				! Inicio de trx 

!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                    ! Secuencias de tecleo

	 If TS.IO.MOTORKEY = 69 And Gr.BrcPag.ValTotp% = -1 Then Begin 						! Dato escaneado
	 	  TS.TEMP1$ = TS.IO.DATA$(10)																						! Longitd dato
      If Val(TS.TEMP1$) = 26 Then Begin																		  ! Si dato TOTP barra
         Gr.BrcPag.Captura$ = BARRA417.TOTP.BONOS                           ! Toma dato pdf
         If Gr.BrcPag.Captura$ = "" Then Begin															! Proceso fallido
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													!
          TS.IO.MOTORKEY = 73																								!
         	Exit Sub 																													! 
         EndIf																															!
         Gr.BrcPag.ValTotp% = 0 																						! 
         GoTo New.Proc.PagBon																								! Validacion bono recompra
      EndIf																																  ! Fin TOT barra
   EndIf

	If Gr.BrcPag.InTrx% = -1 Then                                            \! Si bono en proceso
	 If	TS.IO.KEYS(1) = 82 And TS.IO.MOTORKEY = 81 Then Begin                 ! Si Suspension de trx
 			 Call Visor.And.Borrar("SUSPENSION NO AUTOR.CON BONO RECOMPRA  ")     ! Msg de alerta
       Call Visor.And.Borrar("ANULE BONO RECOMPRA PARA SUSPENDER TRX ")     ! Msg de alerta
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
   EndIf
	
	If Gr.BrcPag.InTrx% = -1 Then                                            \! Si bono en proceso
    If	TS.IO.KEYS(1) = 70 And TS.IO.MOTORKEY = 81 Then Begin               ! Si anulacion total
 	   If (TS.TOTALS(0,0,0) <> TS.BALDUE(0)) Then Begin												! Si pagos capturados
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
   	   TS.GUIDANCE = 1069																										! Msg Error appl nativa
	     Exit Sub 
	   EndIf
    EndIf
	
   If LEN(TS.IO.DATA$(2)) > 14 And                                         \! Escaneo un bono 
      Left$(TS.IO.DATA$(2),3) = "023" Then Begin														! recompra
      If Not(TS.INTRX) Then BEGIN 																					! No esta en trx de venta
	 	  	 Call VISOR.AND.BORRAR("PROCEDIMIENTO NO PERMITIDO FUERA DE VENTA") ! Msg Alerta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Exit Sub 																											    ! Termina proceso
      EndIf

  	If TS.TRX.STATUS <> 1 And TS.PROCEDURE = -1 THEN BEGIN				  	      ! No se ha ingresado total en venta
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 0 																									! 
       TS.GUIDANCE = 1020																										! Solicita total a la trx
       Exit Sub 																														! Sale del proceso
    EndIf 																																	!

      TS.TEMP1I1 = VALIDA.COMPRA.UVT																			  ! Si cumple parametros compra minima UVT
      If TS.TEMP1I1 <> -1 Then Begin 																			  ! Si no pasa validacion Feco
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 0 																									! 
       TS.GUIDANCE = 1020																										! Solicita total a la trx
       Exit Sub 																														! Sale del proceso
      EndIf

	 	  If Gr.Lcl.Clte$ = "" Then Begin						    												! Si no hay cliente capturado
	 	  	 Call VISOR.AND.BORRAR("DEBE CAPTURAR INFORMACION DE CLIENTE ")     ! Msg Alerta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Exit Sub 																											    ! Termina proceso
	 	  EndIf
	 	  If TS.TRAINING Then Begin																							! Si en entrenamiento
	 	  	 Call VISOR.AND.BORRAR("PROCESO NO PERMITIDO EN ENTRENAMIENTO")     ! Msg Alerta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Exit Sub 																											    ! Termina proceso
	 	  EndIf																																	! Fin Salida
      If TS.STANDALONE Then Begin 						 	                            ! Proceso fuera de linea
	 	  	 Call VISOR.AND.BORRAR("PROCESO NO PERMITIDO FUERA DE LINEA  ")     ! Msg Alerta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Exit Sub 																											    ! Termina proceso
      EndIf
      If (TS.TRX.STATUS <> 1 And TS.PROCEDURE = -1) Then Begin	  	        ! No se ha ingresado total en venta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    ! Inicializa secuencia de teclado
         TS.IO.MOTORKEY = 0 																							  ! 
         TS.GUIDANCE = 1020																							    ! Solicita total a la trx
         Exit Sub 																												  ! Sale del proceso
      EndIf 																															  !
      Gr.BrcPag.DatTmp$ = TS.IO.DATA$(2)      															! Dato bono capturado
      TS.ER.RETURN = DIALOGO.TOTP.BONOS      																! Tipo captura datos
      If TS.ER.RETURN = 0 Then Begin																				! Proceso cancelado
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Exit Sub 																											    ! Termina proceso
      EndIf																																	!
      If TS.ER.RETURN = 2 Then Begin																				! Captura Manual
         TS.TEMP5$ = ASIC.DATOS$("INGRESE NUMERO DE   ","CELULAR      ")    ! Numero de celular
         If TS.TEMP5$ = "E" Then BEGIN 																      ! Proceso cancelado   
       	  Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  !
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													!
          TS.IO.MOTORKEY = 73																								!
          Exit Sub 																											    ! Termina proceso
         EndIf 																														  !
         Gr.BrcPag.Captura$ = Left$(TS.TEMP5$+String$(14," "),14)           !
         Gr.BrcPag.Captura$ = Gr.BrcPag.Captura$  +                        \!
                              Left$(Gr.Lcl.Clte$+String$(12," "),12)        !
         GoTo New.Proc.PagBon
      EndIf																																	!
      If TS.ER.RETURN = 3 Then Begin																				! Ignora TOTP
      	 GoTo Pago.Bono.Recompra
      EndIf     																														!
!--- Captura TOTP para validacion       
      TS.ER.RETURN = -1																										  ! Control errores
      Call CREAR.PIPE.PDF																									  ! Apertura pipe
      TS.ER.RETURN = -1																										  ! Control errores
      If TS.ER.RETURN = -1 Then Begin 
           Call VISORES4690(1,"ESCANEE CEDULA DEL  ","TELEF. BENEFICIARIO.",0,"L")  ! 
           Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  !
           TS.IO.MOTORKEY = 0 																						  !
           Gr.BrcPag.ValTotp% = -1
           Exit Sub 																											  !
      EndIf Else Begin																											!
           Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  !
           TS.IO.MOTORKEY = 73 																						  !
           Gr.BrcPag.ValTotp% = 0																						!
           Exit Sub 																											  !
      EndIf
      
!--- Fin validacion TOTP      	
      New.Proc.PagBon:
      TS.TEMP5I1 = Validacion.Token.Bonos																		! Valida captura token
      If TS.TEMP5I1 = 0 Then Begin																					! Falla validacion token
       	  Call VISOR.AND.BORRAR("ERROR EN VALIDACION DEL TOKEN /Borrar")  	!
       	  Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 									  			! Init vectores de carga
       	  TS.IO.MOTORKEY = 73
          Gr.BrcPag.TrxBon% = 0 																						! Cancela proceso
          Gr.BrcPag.Captura$ = ""
          Gr.BrcPag.AplAn% = 0
          Exit Sub 
      EndIf																																  !
      
      Pago.Bono.Recompra:
      TS.IO.DATA$(2) = Gr.BrcPag.DatTmp$																		! Retorna bono capturado
  	  Gr.BrcPag.Captura$ = Right$(TS.IO.DATA$(2),35)                        ! Ajuste dato capturado
  	  If Len(Gr.BrcPag.Captura$) <> 35 Then Begin														! Error longitud bono
	 	     Call VISOR.AND.BORRAR("DATO ESCANEADO NO ES UN BONO VALIDO")		    !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Gr.BrcPag.TrxBon% = 0 																						  ! Cancela proceso
         Gr.BrcPag.Captura$ = ""
         Gr.BrcPag.AplAn% = 0
         Exit Sub 																											    ! Termina proceso
  	  EndIf																																	! 
  	  
      Gr.BrcPag.Valor$ = ASIC.DATOS$("INGRESE VLR. BONO.","DEL BENEFICIARIO..") !
      If Gr.BrcPag.Valor$ = "E" Then BEGIN															    ! Proceso cancelado
 	       Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								    !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Exit Sub 																											    !
      EndIf 																														    !
	    If Val(Gr.BrcPag.Valor$) <= 0 Then Begin
 	       Call VISOR.AND.BORRAR("VALOR INGRESADO NO VALIDO")			            !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Exit Sub 																											    !
	    EndIf
	    Gr.BrcPag.Valor$ = Right$("0000000000"+Gr.BrcPag.Valor$,10)						! 
      If Gr.BrcPag.AplAn% = 0 Then                                         \!
          If Val(Gr.BrcPag.Valor$) > ( TS.BALDUE(0) ) Then Begin            ! Si dato supera total compra
    	       Call VISOR.AND.BORRAR("MONTO BONO SUPERA ELVALOR DE LA COMPRA")!
             Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!
             TS.IO.MOTORKEY = 73																					  !
             Exit Sub 																											!
          EndIf																															!
      Gr.BrcPag.Captura$ = Gr.BrcPag.Captura$ + Gr.BrcPag.Valor$            ! Arma mensaje consulta   
	 	  	 If Gr.BrcPag.AplAn% = 0 Then                                      \!
            Call VALIDA.PAGO.BONO(Gr.BrcPag.Captura$,"66") Else            \! Proceso de pago
            Call VALIDA.PAGO.BONO(Gr.BrcPag.Captura$,"70")                  ! Proceso de anulacion pago
         If TS.TEMP1I1 = 0 Then Begin																				! Proceso fallido
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!
            TS.IO.MOTORKEY = 73																							!
            Gr.BrcPag.TrxBon% = 0
            Gr.BrcPag.Captura$ = ""
            Gr.BrcPag.AplAn% = 0
            Gr.BrcPag.Valor$ = ""
            Exit Sub 
         EndIf 
         Xrta% = CONFIRMA.CONSUMO.BONO
   	     If Xrta% = 0 Then Begin 																		       	! Si falla el proceso
   	     	  Call VISOR.AND.BORRAR("FORMA DE PAGO NO   AUTORIZADA /Borrar ") !
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!
            TS.IO.MOTORKEY = 73																							!
            Gr.BrcPag.TrxBon% = 0
            Gr.BrcPag.Captura$ = ""
            Gr.BrcPag.AplAn% = 0
            Gr.BrcPag.Valor$ = ""
           Exit Sub 
         EndIf																															! fin falla proceso
         
         TS.BD.DSPPARM = -1                     														! SET FOR NORMAL BD
         Call TSBDEC01                          														! FIRST TAKE BAL DUE
         TS.IO.STATE = 10
         Dim TS.IO.PREV.KEYS(10)
         Dim TS.IO.PREV.DATA$(10)
         TS.IO.PREV.MOTORKEY = 0
         Dim Ts.Io.Keys(10)																									! Inicializa Vectores
         Dim Ts.Io.Data$(10)																								! 
         If Gr.BrcPag.AplAn% = -1 Then TS.IO.KEYS(1) = 70                   ! Anulacion pago
         TS.IO.DATA$(3) = Right$(Gr.BrcPag.TipVar$,1)           		        ! Variedad de Pago
         TS.IO.KEYS(3)  = 78                                     		        ! Tecla Slash
         TS.IO.DATA$(7) = Gr.BrcPag.Valor$													        ! Valor a entregar
         !TS.IO.DATA$(9) = Gr.BrcPag.Nro$														        ! Nro bono capturado
         TS.IO.DATA$(9) = Left$(Gr.BrcPag.Nro$,23)
         TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.BrcPag.TipVar$,1))   		        ! Tipo de Pago
         TS.IO.MOTORKEY = TS.IO.KEYS(7)															        !
         To.TENDLIMITS(Gr.BrcPag.TVPOS%,0) = 9999999   									    ! Modifica controles entrega efectivo
         To.TENDLIMITS(Gr.BrcPag.TVPOS%,1) = 9999999									      ! Modifica controles entrega efectivo
         TS.TEMP1I1 = 0
         For TS.TEMP1I1 = 0 To 7
             TO.NEGAMT(TS.TEMP1I1) = 9999999																!
             TO.NEGCNT(TS.TEMP1I1) = 999																		!
         Next TS.TEMP1I1           																				  !
         Gr.BrcPag.TrxBon% = -1
         Gr.BrcPag.Capt% = -1

         
 !        Call TSTDEC01																										  ! Ejecuta secuencia pago
 !        Dim Ts.Io.Keys(10)																								  ! Inicializa Vectores
 !        Dim Ts.Io.Data$(10)																							  ! 
 !        TS.IO.MOTORKEY = 73

	 EndIf																																		! Fin escaneo Bono
EndIf 																																			! Fin secuencias de tecleo

!--- EAMTSU32.J86
If XOPT% = 32 Then Begin																										! En validacion de pago

 If (TS.IO.DATA$(3) = Right$(Gr.BrcPag.TipVar$,1)) And                     \! Si usa forma de pago 
    (TS.IO.KEYS(7)  = Val("9"+Left$(Gr.BrcPag.TipVar$,1))) And             \!
    Gr.BrcPag.TrxBon% <> -1 Then Begin 
      Call VISOR.AND.BORRAR("ERROR SECUENCIA DE  TECLEO / BORRAR")
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)                              ! Init vectores entrada
      TS.IO.MOTORKEY = 73                                         
       Gr.BrcPag.TrxBon% = 0
       Gr.BrcPag.Captura$ = ""
       Gr.BrcPag.AplAn% = 0
       Gr.BrcPag.Valor$ = ""
       Gr.BrcPag.Capt% = 0
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
 EndIf

! If (TS.IO.DATA$(3) = Right$(Gr.BrcPag.TipVar$,1)) And                     \! Si usa forma de pago 
!    (TS.IO.KEYS(7)  = Val("9"+Left$(Gr.BrcPag.TipVar$,1))) And             \!
!    Gr.BrcPag.TrxBon% = -1 Then Begin       																!	bono recompra
!    Xrta% = CONFIRMA.CONSUMO.BONO
!   	If Xrta% = 0 Then Begin 																		        		! Si falla el proceso
!       Gr.BrcPag.TrxBon% = 0
!       Gr.BrcPag.Captura$ = ""
!       Gr.BrcPag.AplAn% = 0
!       Gr.BrcPag.Valor$ = ""
!       Gr.BrcPag.Capt% = 0
!       TS.USER.RETURN = 99																									!
!       TS.LINEDATA = 51                 																    ! "TARJETA INVALIDA"            
!       TS.STACKERR(0) = 0                                                   ! manager's override required   
!       TS.STACKERR(3) = 0                                                   ! item descriptor               
!       TS.STACKERR(6) = 0                                                   ! put "M" on last display       
!       TS.STACKERR(7) = -1                                                  ! indicate no printing required 
!       TS.MO.REASON = 0                                                     ! invalid key with department   
!       Call TSCSEC03       																							    !                               
!       TS.IO.MOTORKEY = 0 																									!
!       Exit Sub                                                             !
!   	EndIf																																		! 
!   	TS.ACNUM$ = Gr.BrcPag.Nro$                                              ! Asigna # bono
!   	Gr.BrcPag.Capt% = -1																								    ! Proceso correcto
! EndIf           																														! Fin validacion forma de pago pes  	

EndIf																																				! Fin validacion forma de pago

!-- EAMTSU20
If Xopt% = 20 Then Begin																										! En la impresion del CR
   
   If (TS.INTRX) AND (TS.LINETYPE = 6 AND TS.LINEDATA = 1) And  					 \!
      (TS.TENDERED (0) <> 0 Or TS.TRX.STATUS <> 100) Then Begin							!
    	If Gr.BrcPag.InTrx% = -1 Then Begin
    		 Gr.BrcPag.Slend% = SL.END																					! Total de strings
    		 Gr.BrcPag.InTrx% = 0
    	EndIf
   EndIf
   
EndIf 

!-- EAMTSU60
If Xopt% = 60 Then Begin																										! En la impresion del CR
	
  If TS.LINETYPE = 2 And Gr.BrcPag.Capt% = -1 THEN BEGIN										! Impresion pago y credito social
      TS.TEMP1$ = Left$("BONO:"+Left$(Gr.BrcPag.Nro$,23)+String$(37," "),37)
      Call U.IMPRIME(TS.TEMP1$,6100H)
      Gr.BrcPag.Capt% = 0
      Gr.BrcPag.InTrx% = -1
      If Not(TS.RECOVERY) Then Begin
      	  TS.TEMP2$ = "00"
      	  If Gr.BrcPag.AplAn% = -1 Then Begin 
      	  	 TS.TEMP2$ = "01"
      	  	 Gr.BrcPag.InTrx% = 0
      	  Endif
          TS.TEMP1$ = Pack$(Gr.BrcPag.Nro$)   + ":" +                      \! Numero bono
                      Pack$(Gr.BrcPag.Valor$) + ":" +                      \! Valor del bono
                      Pack$(Gr.Lcl.Clte$)     + ":" +                      \! Cliente asociado
                      Pack$(Hora.Mundial$)    + ":" +                      \! ammddhhmmss para confirmar
                      Pack$(TS.TEMP2$)        + ":" +                      \! Signo Operacion  00.Pago 01. Anulacion
                      Pack$(Gr.BrcPag.Token$) + ":" +                      \! Nro token
                      Pack$(Gr.BrcPag.Celular$)                             ! Nro Celular
          Call Grabacion.Cadena.Usuario("20191128",TS.TEMP1$)               ! Almacena UD bono recompra pago
       EndIf
       Gr.BrcPag.AplAn% = 0

       To.TENDLIMITS(Gr.BrcPag.TVPOS%,0) = Gr.BrcPag.MaxTv%									! Retorna valores default
       To.TENDLIMITS(Gr.BrcPag.TVPOS%,1) = Gr.BrcPag.ChgTv%									! Retorna valores default
       TS.TEMP1I1 = 0
       For TS.TEMP1I1 = 0 To 7
          TO.NEGAMT(TS.TEMP1I1) = GR.BRNEGAMT(TS.TEMP1I1)
          TO.NEGCNT(TS.TEMP1I1) = GR.BRNEGCNT(TS.TEMP1I1)
       Next TS.TEMP1I1
   EndIf
EndIf																																				! Fin EAMTSU60

End Sub 
