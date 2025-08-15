!**************************************************
!Empresa       : Grupo Retail Ltda - Colombia     *
!Programa      : GRSAPCOR.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Pago Electronico Subsidio *
!                SAP-CORE                         *
!**************************************************
! Mod 21 Jun 2018
! Se modifica modulo para manejo del pago del 
! subsidio por cc o credencial, capturando el 
! número de documento del PDF417 de las cedulas
! de Colombia.
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 12Mzo2019
! Se genera por solicitud de comfandio la 
! impresión de voucher de constancia que se pago
! el auxilio monetario, el cliente debe firmar el 
! voucher y el cajero guardarlo,
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 06May2018
! Se ajusta Formato impresion voucher para soporte
! de pago solicitado por Comfandi
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 05/Jul/2019
! Se adiciona funcionalidad para verificacion de 
! carga de servicios java en procesos de inactividad
! de la terminal, se adiciona ejecución en las user
! Exit 1 y 10.
! Se modifica el numero de referencia almacenado en la 
! forma de pago de pes para evitar el aumento de registros
! en los pagos de subsidio en la tabla EAMCUSTA
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 18Jun2021
! Se ajusta grabacion de UD en operaciones con 
! venta y procesos de autorizacion para que aplique
! de manera correcta el pago.
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 13Ene2023
! Se inicializan las variables de token y numero
! de celular cuando se presentan fallas de consumo 
! al servicio de TOTP para evitar grabar mal la
! información de registro.
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 18May2023
! Se adiciona funcionalidad al modulo de TOTP para 
! la captura del número de celular y cedula del 
! beneficiario se tome de un PDF-417 que presenta el 
! usuario en su celular y proceso el pago.
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%Environ T

Integer*1 Global Gr.SapCore.Act%,  		   		 \! Si proyecto activo
                 Gr.SapCore.Trx%,   		 		 \! Si remesa en registro 
                 Gr.SapCore.InTrx%,          \! 
                 Gr.SapCore.Apl%,            \! Aplicacion de pago 
                 Gr.SapCore.AplAn%,          \! AN Aplicacion de pago 
                 Gr.SapCore.CtlKy%,          \! Control dato escaneado
                 Gr.SapCore.TipEnt%,         \! Tipo entrada 1.Cedula 2.Credencial
                 Gr.SapCore.TrxCed%,         \!
                 Gr.SapCore.MetPag%,         \! Metodo pago token - Manual
                 Gr.SapCore.TotBarra%,       \! TOT barra
                 Asic.Detalle%,              \! 
                 Asic.Device%                 !                  
Integer*2 Global Gr.SapCore.Sec%    		 			! Tecla para Secuencia
Integer*1 Global TS.RECOVERY           				! terminal recovery flag
Integer*1 Global Asc.Pay.Impr%                ! 
Integer*4 Global Asc.Tmp.Apun%,              \! Apuntador para manejo strings 
                 Gr.Sapcore.TVPOS%,          \! Apuntador para control cambio efectivo
                 Gr.Sapcore.MaxTv%,          \! Maximo pago   tipo y variedad
                 Gr.Sapcore.ChgTv%,          \! Maximo cambio tipo y variedad
                 Gr.Sapcore.Turno%            !
String    Global Gr.SapCore.Benef$,       	 \! ID del beneficiario
                 Gr.SapCore.Captura$,      	 \! ID del beneficiario captura
                 Gr.SapCore.TipVar$,      	 \! Tipo y variedad definida
                 Gr.SapCore.Trama$,          \! Trama original mensaje
                 Hora.Mundial$,              \! Fecha y hora de trx
                 Gr.SapCore.Name$,           \! Nombre beneficiario
                 Gr.SapCore.Token$,          \! Nro Token TOTP
                 Gr.SapCore.Celular$,        \! Celular pago PES
                 Gr.SapCore.Vlr$              ! Valor del subsidio
Integer*2 Global GR.SCNEGCNT(1)               !
Integer*4 Global GR.SCNEGAMT(1)							  !
                 
%INCLUDE ADX_UPGM:EAMTSCVA.J86                !
%INCLUDE ADX_UPGM:EAMTSWKG.J86                ! working storage                  
%INCLUDE ADX_UPGM:EAMTRANS.j86                !                                  
%INCLUDE ADX_UPGM:EAMTERMS.J86                !                                  
%INCLUDE ADX_UPGM:EAMTOPTS.J86	              ! Variables de la terminal options 
%INCLUDE ADX_UPGM:EAMTSXHC.J86                ! Rutinas de Assembler             
%INCLUDE ADX_UPGM:EAMASMRT.J86                ! Rutinas de Assembler             
                                                                                 
%INCLUDE RECATSSU.011          					      ! RUTINAS GENERICAS APLICACION     

SUB TSTCEC01 EXTERNAL                         ! tender cashing
END SUB                                       !

Sub TSCSEC10 EXTERNAL                         ! non sales initialization
END SUB                                       !

Sub  TSCSEC03 External
End Sub 

Sub TSTDEC03 EXTERNAL            							! tendering
End Sub 

Sub TSBDEC01 EXTERNAL            							! take a balance due
End Sub

Sub TSPREC01 EXTERNAL          								! print routine
End Sub

Sub TSDSEC01 External					        				! Rutina display
End Sub

Function FORMAT.AMOUNT(AMT1) EXTERNAL
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub TSHIECET External 
End Sub 

Sub SAPCORE.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:SC" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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

Sub SAP.BUSCA.LIMITE.TV(UE.TIPO.VAR$)
  Integer*2 UE.I%
  Integer*1 UE.FOUND%
  String UE.TIPO.VAR$
  UE.I% = 1
  UE.FOUND% = 0 
  Gr.Sapcore.MaxTv% = 0
  Gr.SapCore.ChgTv% = 0 
  While UE.I% <= TO.NUMTNDR AND NOT UE.FOUND%
    If TO.TENDOPTS(UE.I%,0) = VAL(LEFT$(UE.TIPO.VAR$,1)) AND  \
       TO.TENDOPTS(UE.I%,1) = VAL(RIGHT$(UE.TIPO.VAR$,1)) Then Begin 
       Gr.Sapcore.MaxTv% = TO.TENDLIMITS(UE.I%,0)
       Gr.SapCore.ChgTv% = TO.TENDLIMITS(UE.I%,1)
       UE.FOUND%  = -1
       Gr.Sapcore.TVPOS% = UE.I%
    EndIf Else UE.I% = UE.I% + 1
  Wend    
  UE.I% = 0
  For UE.I% = 0 To 7
      GR.SCNEGCNT(UE.I%) = To.NEGCNT(UE.I%)    
      GR.SCNEGAMT(UE.I%) = To.NEGAMT(UE.I%)    
  Next UE.I%

End Sub

Function CEDULA.PDF417 Public 
String Xlen$, CEDULA.PDF417, Xsnd$, Xrta$, Xfin$, Xtrama$, Xtemp4$
String Xid$
   Xlen$ = Right$("0000"+TS.TEMP1$,4)
   Call VISORES4690(1,"TOMANDO CEDULA PIPE","ESPERE POR FAVOR",0,"L")
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Asc.Pay.Impr% = 2
   Xrta$ = Armar.Trama.Msg("10","41",Xlen$,"00","0001","123456")  					! Armar trama MSG
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", Xrta$)         	! Ejecuta Requerimiento
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call SAPCORE.AUDITORIA(Xrta$,Xtrama$, Xsnd$, Xfin$)                    	! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
      CEDULA.PDF417 = ""
   	  Exit Function 
   EndIf
   Xid$ = Mid$(XTrama$,54,170)

   Xtemp4$ = Left$(mid$(Xid$,59,10),20) + " " + Left$(mid$(Xid$,105,09),20) ! Nombre y apellido de la persona
   Gr.SapCore.Name$ = Xtemp4$
   Xfin$ = Asic.Datos$(Xtemp4$,"CONFIRMA?  1.Si 0.No")
   If Xfin$ = "E" Then BEGIN 																		    				! Proceso cancelado   
   	  Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  		!
      CEDULA.PDF417 = ""
      Exit Function 																								    		!
   EndIf 																														    		!
   If Xfin$ <> "1" Then Begin
      Call VISOR.AND.BORRAR("DATO CAPTURADO NO ESVALIDO /Borrar")			  		!
      CEDULA.PDF417 = ""
      Exit Function 
  EndIf

   CEDULA.PDF417 = Str$(Val(mid$(Xid$,49,10)))															! Cedula retornada
    
End Function 

Function BARRA417.TOTP
String Xlen$, BARRA417.TOTP, Xsnd$, Xrta$, Xfin$, Xtrama$, Xtemp4$
String Xid$
   Xlen$ = Right$("0000"+TS.TEMP1$,4)
   Call VISORES4690(1,"TOMANDO DATOS  PIPE","ESPERE POR FAVOR",0,"L")
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Asc.Pay.Impr% = 2
   Xrta$ = Armar.Trama.Msg("10","41",Xlen$,"00","0001","123456")  					! Armar trama MSG
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", Xrta$)         	! Ejecuta Requerimiento
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call SAPCORE.AUDITORIA(Xrta$,Xtrama$, Xsnd$, Xfin$)                    	! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
      BARRA417.TOTP = ""
   	  Exit Function 
   EndIf
   Xid$ = Mid$(XTrama$,54,170)

   Xtemp4$ = Str$(Val(mid$(Xid$,15,12))) 																		! Numero cedula persona
   Xfin$ = Asic.Datos$(Xtemp4$,"CONFIRMA?  1.Si 0.No")
   If Xfin$ = "E" Then BEGIN 																		    				! Proceso cancelado   
   	  Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  		!
      BARRA417.TOTP = ""
      Exit Function 																								    		!
   EndIf 																														    		!
   If Xfin$ <> "1" Then Begin
      Call VISOR.AND.BORRAR("DATO CAPTURADO NO ESVALIDO /Borrar")			  		!
      BARRA417.TOTP = ""
      Exit Function 
  EndIf

  BARRA417.TOTP = Xid$																											! Retorna la trama

End Function 


Sub CREAR.PIPE.PDF Public 
String Xrta$, XTrama$, Xfin$, Xtemp4$, Xsnd$
   Call VISORES4690(1,"CREANDO PIPE PDF","ESPERE POR FAVOR",0,"L")
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Asc.Pay.Impr% = 0
   Xrta$ = Armar.Trama.Msg("10","40","","00","0001","123456")   						! Armar trama MSG
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", Xrta$)         	! Ejecuta Requerimiento
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call SAPCORE.AUDITORIA(Xrta$,Xtrama$, Xsnd$, Xfin$)                    ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
      TS.ER.RETURN = 0
   	  Exit Sub 
   EndIf
    
End Sub 


Function COBRO.SUBSIDIO.SAPCORE(Xfunc$)
Integer*1 COBRO.SUBSIDIO.SAPCORE																						!
String XR$, Xtemp4$, Xtrama$,Xsnd$, Xfin$, Xcedulas$, xmsg$  								!
String Xbuffer$, Xfunc$


If Gr.SapCore.TipEnt% = 1 Or Gr.SapCore.TipEnt% = 3 Then Begin							! Pago con cedula
   Xmsg$ = "*******"+Right$(gr.sapcore.captura$,3)
   Call VISORES4690(1,"VALIDANDO CEDULA",Xmsg$,0,"L")	    									! Msg Operador
   COBRO.SUBSIDIO.SAPCORE = 0  																			        !
   Gr.SapCore.Apl% = 0
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora del requerimiento
   Xbuffer$ = Right$("                 "+Gr.SapCore.Captura$,18) +         \! ID del beneficiario
              "1"                                                +         \! Via Pago
              "1"                                                           ! Autorizador 
   
   Asc.Pay.Impr% = 0

   XTemp4$ = Armar.Trama.Msg("10","43",(Xbuffer$),"00","0001","123456")   ! Armar trama MSG

   Gr.SapCore.TRama$ = Right$("0000"+TS.STORE$,4)               + 	       \! Numero del almacen
                    Right$("000000"+Str$(SL.HD.TERMINAL),6)     + 	       \! Numero de terminal
                    Unpack$(TS.OPER$)                           +          \! Numero de operador
                    Right$("000000"+Str$(SL.HD.TRANSNUM + 1),6) + 	       \! Numero de transaccion
                    Mid$(XTemp4$,44,12)                         +          \! Fecha y Hora de la operacion AAMMDDHHMMSS
                    Right$("                 "+Gr.SapCore.Captura$,18) +   \! ID del beneficiario
                    "1"                                         +          \! Via Pago
                    "1"                                                     ! Autorizador 
                    
! Hora.Mundial$                               +          ! Fecha y Hora de la operacion AAMMDDHHMMSS

   
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)         ! Ejecuta Requerimiento
   
   !Xtrama$ = "1043058200000ACTUALIZACION SE REALIZO DE MANERA SATIF31963       "
   
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call SAPCORE.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                    ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   	  COBRO.SUBSIDIO.SAPCORE = 0 		 																        ! Error del proceso
   	  Exit Function
   EndIf
   XTEMP4$ = Mid$(XTrama$,12,2)	           																  ! Valida rta entregada
   If XTemp4$ = "00" Then Begin 																					  ! Rta Satisfactoria
       Gr.SapCore.Vlr$ = Str$(Val(Mid$(XTrama$,54,12)))                     ! Valor del subsidio
       Gr.SapCore.Apl% = -1
       If Val(Gr.SapCore.Vlr$) <= 0 Then Begin															! Error en Valor Pago
   		    Call VISOR.AND.BORRAR("VALOR DEL SUBSIDIO  NO VALIDO")		        ! Presenta Msg Error
   		    COBRO.SUBSIDIO.SAPCORE = 0																		    !
   		    Gr.SapCore.Apl% = 0
          Exit Function 
       EndIf
   	   COBRO.SUBSIDIO.SAPCORE = -1		 															        !
   EndIf Else Begin 																												!
   		 Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   		 COBRO.SUBSIDIO.SAPCORE = 0																				    !
   EndIf 																																		!
   
EndIf

If Gr.SapCore.TipEnt% = 2  Then Begin						  ! Pago con credencial
   Xmsg$ = "*******"+Right$(gr.sapcore.captura$,3)
   Call VISORES4690(1,"VALIDANDO CREDENCIAL",Xmsg$,0,"L")       					! Msg Operador
   COBRO.SUBSIDIO.SAPCORE = 0  																			        !
   Gr.SapCore.Apl% = 0
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora del requerimiento
   Xbuffer$ = Right$("                 "+Gr.SapCore.Captura$,18) +         \! ID del beneficiario
              "1"                                                +         \! Via Pago
              "1"                                                           ! Autorizador 
   
   Asc.Pay.Impr% = 0                                                        ! Ajusta numero trx para seguimiento
   XTemp4$ = Armar.Trama.Msg("10",Xfunc$,(Xbuffer$),"00","0001","123456")   ! Armar trama MSG

   Gr.SapCore.TRama$ = Right$("0000"+TS.STORE$,4)               + 	       \! Numero del almacen
                    Right$("000000"+Str$(SL.HD.TERMINAL),6)     + 	       \! Numero de terminal
                    Unpack$(TS.OPER$)                           +          \! Numero de operador
                    Right$("000000"+Str$(SL.HD.TRANSNUM + 1),6) + 	       \! Numero de transaccion
                    Mid$(XTemp4$,44,12)                         +          \! Fecha y Hora de la operacion AAMMDDHHMMSS
                    Right$("                 "+Gr.SapCore.Captura$,18) +   \! ID del beneficiario
                    "1"                                         +          \! Via Pago
                    "1"                                                     ! Autorizador 
   
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)         ! Ejecuta Requerimiento
   
   !Xtrama$ = "1032058200000ACTUALIZACION SE REALIZO DE MANERA SATIF189422      "
   
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call SAPCORE.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                    ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   	  COBRO.SUBSIDIO.SAPCORE = 0 		 																        ! Error del proceso
   	  Exit Function
   EndIf
   XTEMP4$ = Mid$(XTrama$,12,2)	           																  ! Valida rta entregada
   If XTemp4$ = "00" Then Begin 																					  ! Rta Satisfactoria
       Gr.SapCore.Vlr$ = Str$(Val(Mid$(XTrama$,54,12)))                     ! Valor del subsidio
       Gr.SapCore.Apl% = -1
       If Val(Gr.SapCore.Vlr$) <= 0 Then Begin															! Error en Valor Pago
   		    Call VISOR.AND.BORRAR("VALOR DEL SUBSIDIO  NO VALIDO")		        ! Presenta Msg Error
   		    COBRO.SUBSIDIO.SAPCORE = 0																		    !
   		    Gr.SapCore.Apl% = 0
          Exit Function 
       EndIf
   	   COBRO.SUBSIDIO.SAPCORE = -1		 															        !
   EndIf Else Begin 																												!
   		 Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   		 COBRO.SUBSIDIO.SAPCORE = 0																				    !
   EndIf 																																		!
   
EndIf

End Function

Sub Voucher.Sapcore																													! Generacion vouchers pago pes
    To.USEREXIT(20) = 0																									   	! Activa  lineas impresion
    To.USEREXIT(60) = 0																									   	!

    Call U.IMPRIME(TO.HEADERLINE1$,6100H)																		!
    Call U.IMPRIME(TO.HEADERLINE2$,6100H)																		!
    Call Format.Amount(Val(Gr.SapCore.Vlr$))																!
    Call U.IMPRIME("VALOR A ENTREGADO :$"+TS.TEMP1$,6100H)									!
    Call U.Imprime("ID BENEFICIARIO   :"+Gr.SapCore.Benef$,6100H)						!
    CALL U.Imprime(Gr.SapCore.Name$,6100H)

    CALL U.Imprime(" ",6200h)
    CALL U.Imprime("Firma:______________________________",6100H)
    Asic.Detalle% = 1																												!
    TS.TEMP1$ = Linea.Detalle(38)  																					! Trailer del movimiento
    Call U.IMPRIME(TS.TEMP1$,6200H)                                       	!
    TS.TEMP1$ = String$(37,"-")																							! Linea cierre voucher
    Call U.IMPRIME(TS.TEMP1$,2100H)																			  	! avance de papel
    Call U.IMPRIME(TS.TEMP1$,4900H)																			  	! avance de papel
    Call U.CORTACR																													! Corta Papel
    To.USEREXIT(20) = -1																								  	! Activa  lineas impresion
    To.USEREXIT(60) = -1																								  	!
    TS.LINETYPE = 18																										  	! Genera cabecera 
    TS.LINEDATA = 1																											  	! en proximo tiquete
    Call TSPREC01																												  	!

End Sub 

Function CONFIRMAR.SAPCORE
Integer*1 CONFIRMAR.SAPCORE																							    !
String XR$, Xtemp4$, Xtrama$,Xsnd$, Xfin$, Xcedulas$												!
String Xbuffer$
   Call Voucher.Sapcore																											! Generación voucher pes
   Call Visores4690(1,"CONFIRMANDO MOVIMIENTO","ESPERE POR FAVOR..",0,"L")
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora del requerimiento
   Xbuffer$ = Gr.SapCore.TRama$															                ! Trama del cobro
   Asc.Pay.Impr% = 2                                                        ! Ajusta numero trx para seguimiento
   !Call visores4690(1,"tipo entrada",str$(Gr.SapCore.TipEnt%),1200,"l")
   If Gr.SapCore.TipEnt% = 1 Then                                          \!
      XTemp4$ = Armar.Trama.Msg("10","50",Xbuffer$,"00","0001","123456")    ! Armar trama MSG CC 
   If Gr.SapCore.TipEnt% = 2 Then                                          \!
      XTemp4$ = Armar.Trama.Msg("10","38",Xbuffer$,"00","0001","123456")    ! Armar trama MSG credencial
   If Gr.SapCore.TipEnt% = 3 Then                                          \!
      XTemp4$ = Armar.Trama.Msg("10","50",Xbuffer$,"00","0001","123456")    ! Armar trama MSG CC DIGITAL
      
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)         ! Ejecuta Requerimiento
   
   !Xtrama$ = "1038046200000ACTUALIZACION SE REALIZO DE MANERA SATIF"
   
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call SAPCORE.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                    ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 
      !Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   EndIf
   CONFIRMAR.SAPCORE = -1 																		              !
End Function 

Function ANULACION.SAPCORE
Integer*1 ANULACION.SAPCORE																							    !
String XR$, Xtemp4$, Xtrama$,Xsnd$, Xfin$, Xcedulas$												!
String Xbuffer$
   Call Visores4690(1,"ANULANDO MOVIMIENTO   ","ESPERE POR FAVOR..",0,"L")
   ANULACION.SAPCORE = 0  																		              !
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora del requerimiento
   Xbuffer$ = Gr.SapCore.TRama$															                ! Trama del cobro
   Asc.Pay.Impr% = 0                                                        ! Ajusta numero trx para seguimiento
   If Gr.SapCore.TrxCed% = -1 Then                                         \!
   XTemp4$ = Armar.Trama.Msg("10","45",Xbuffer$,"00","0001","123456")       ! Armar trama MSG anul cc 
   
   If Gr.SapCore.TipEnt% = 2 Then                                          \!
   XTemp4$ = Armar.Trama.Msg("10","33",Xbuffer$,"00","0001","123456")       ! Armar trama MSG anul crede

   If Gr.SapCore.TipEnt% = 3 Then                                          \!
   XTemp4$ = Armar.Trama.Msg("10","45",Xbuffer$,"00","0001","123456")       ! Armar trama MSG anul cc digital


   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)         ! Ejecuta Requerimiento
   
   !Xtrama$ = "1045046200000ACTUALIZACION SE REALIZO DE MANERA SATIF"
   
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call SAPCORE.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                    ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   	  ANULACION.SAPCORE = 0 		 																            ! Error del proceso
   	  Exit Function
   EndIf
   XTEMP4$ = Mid$(XTrama$,12,2)	           																  ! Valida rta entregada
   If XTemp4$ = "00" Then Begin 																					  ! Rta Satisfactoria
   	  Gr.SapCore.Apl%   = -1
      ANULACION.SAPCORE = -1 																	              !
      Gr.SapCore.InTrx% = 2
   ENDIF Else BEGIN 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   	  ANULACION.SAPCORE = 0 		 																            ! Error del proceso
   	  Gr.SapCore.InTrx% = 0
   	  Exit Function
   ENDIF    	
   
End Function 

Function Validacion.Token.Pes Public 																				! Validacion token
Integer*1 Validacion.Token.Pes
String    Xsnd$, Xbuffer$, XTemp4$, Xtrama$, Xfin$                          !
 Validacion.Token.Pes = 0																										! Proceso fallido
  Gr.SapCore.Token$ = ASIC.DATOS$("INGRESE NUMERO TOKEN","PARA VALIDACION....")!
  If Gr.SapCore.Token$ = "E" Then BEGIN 													    			! Proceso cancelado   
 	  Gr.SapCore.Token$ = ""
    Validacion.Token.Pes = 0																								! Proceso fallido
    Exit Function 																											   	!
  EndIf 																														    		!

 If Gr.SapCore.TotBarra% = 0 Then Begin
  Gr.SapCore.Celular$ = ASIC.DATOS$("INGRESE NUMERO DE   ","CELULAR          ")!
  If Gr.SapCore.Celular$ = "E" Then BEGIN 												    			! Proceso cancelado   
 	  Gr.SapCore.Celular$ = ""
 	  Gr.SapCore.Token$   = ""
    Validacion.Token.Pes = 0																								! Proceso fallido
    Exit Function 																								    			!
  EndIf 																														    		!
 EndIf Else Begin
 	  XTemp4$ = Gr.SapCore.Captura$
 	  Gr.SapCore.Celular$ = Left$(XTemp4$,10)																	! Numero de celular capturado
 	  Gr.SapCore.Captura$ = Str$(Val(Mid$(Xtemp4$,15,12)))                    ! Numero de cedula beneficiario
 EndIf
  
 Call TSHIECET
 Call VISORES4690(1,"VALIDANDO TOKEN ","ESPERE POR FAVOR..",0,"L")
 Asc.Pay.Impr% = 0																													! Numero actual de trx
 Xsnd$ = DATE$ +":"+ Time$                                                  ! Fecha y hora del requerimiento
 Xbuffer$ = Right$("          "+Gr.SapCore.Token$,10) +                    \! Numero de token ajustado
            Right$("                "+Gr.SapCore.Celular$,16)  +           \! Numero de celular ajustado
            Right$("            "+Gr.SapCore.Captura$,12)                   ! Numero de cedula del beneficiario
            
 XTemp4$ = Armar.Trama.Msg("10","53",Xbuffer$,"00","0001","123456")         ! Armar trama MSG CC 
 XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)           ! Ejecuta Requerimiento
 
 !Xtrama$ = "1052046200000CODIGO TOTP CORRECTO                    "
 
 Xfin$ = DATE$ +":"+ Time$                                                  ! Fecha y hora rta del requerimiento
 Call SAPCORE.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                      ! Rastreo movimiento
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
 Validacion.Token.Pes = -1																									! Proceso exitoso
End Function 																																! fin validacion token

Sub Reclamo.SapCore  																												! Cobro subsidio
Integer*2 Xcont%, XK%, XCant%
String Xmsg1$, Xmsg2$, Xdata$, XvlrTrx$, Llave$
                                                                                                    
         TS.TEMP5I2 = 0                    													        ! Si proceso correcto
         TS.TEMP5I2 = COBRO.SUBSIDIO.SAPCORE("32")											  	! Confirma movimiento
      	 IF TS.TEMP5I2 <> 0 Then Begin																			! Si proceso correcto
      	 	  Call Format.Amount( Val(Gr.SapCore.Vlr$) )                      ! Formatea valor a entregar
      	    Call TSHIECET
      	    Call VISORES4690(2,"VALOR A ENTREGAR","$"+TS.TEMP1$,0,"L")
      	    Call VISORES4690(1,"VALOR A ENTREGAR","$"+TS.TEMP1$,1500,"L")

            Llave$ = Pack$("43000000000000000890303208")                    ! Dato EamCusta
            TS.ER.RETURN = -1
            Open "R::EAMCUSTA" KEYED RECL 28 AS 94                             ! Abre EAMCUSTA
            If TS.ER.RETURN = -1 Then BEGIN
               Write FORM "C13 2I4 I2 I4 C1";#94 ;                            \! Reescribe el EAMCUSTA 
               Llave$, 0,0,0,0," "																							!
               Close 94
            ENDIF Else BEGIN
            	Call VISORES4690(1,"EAMCUSTA NO FUE ","ACTUALIZADO...",1000,"L")
            ENDIF
            
            !TS.TEMP5I2 = CONFIRMAR.SAPCORE                                  ! Confirma movimiento pago subsidio
            Gr.SapCore.Benef$ = Gr.SapCore.Captura$
            Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 												! Init vectores de carga
            TS.IO.DATA$(3) = Right$(Gr.SapCore.TipVar$,1)                   ! Variedad de Pago
            TS.IO.KEYS(3)  = 78                                             ! Tecla Slash
            TS.IO.DATA$(7) = Gr.SapCore.Vlr$																! Valor a entregar
            !TS.IO.DATA$(9) = Gr.SapCore.Benef$															! ID del Beneficiario
            
            TS.IO.DATA$(9) = "890303208"      															! ID del Beneficiario Mod 5/Jul/2019 GR-OVS
            TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.SapCore.TipVar$,1))          ! Tipo de Pago
            TS.IO.MOTORKEY = TS.IO.KEYS(7)																	!
            Gr.SapCore.InTrx% = 1																						! Cobro Subsidio 
!-- Adicion signo operacion
            TS.TEMP2$ = "00"   																							! Operacion Venta
            If Gr.SapCore.AplAn% = -1 Then Begin                            !
      	       TS.TEMP2$ = "01"                       										  ! Operacion Anulacion
            EndIf
               
            TS.TEMP1$  = Pack$(Gr.SapCore.Benef$)                  +       \! ID beneficiario
               ":"+Pack$(Gr.SapCore.Vlr$)                          +       \! Valor subsidio entregado
               ":"+Pack$(Right$("00"+Str$(Gr.SapCore.InTrx%),2))   +       \! Tipo de opearacion 1. Cobro 2. Compra
               ":"+Pack$(TS.TEMP2$)                                +       \! Signo Operacion 
               ":"+Gr.SapCore.TRama$                               +       \! Trama para reversos
               ":"+Pack$(Gr.SapCore.Token$)                        +       \! Nro Token operacion
               ":"+Pack$(Gr.SapCore.Celular$)                      +       \! Nro celular operacion
               ":"+Pack$("00")                                              ! Filler 
            Call Grabacion.String.Usuario2("20151109",TS.TEMP1$)            ! Graba String usuario  

            TS.TEMP1I1 = 0
            For TS.TEMP1I1 = 0 To 7
                TO.NEGAMT(TS.TEMP1I1) = 9999999
                TO.NEGCNT(TS.TEMP1I1) = 999
            Next TS.TEMP1I1           

         EndIf ELSE Begin 																									! Falla Confirmacion
            Call Gr.Init.Trx																								! Init Trx
            TS.IO.STATE    = 10            																  ! PREPARE FOR NORMAL SALES STATE
         EndIf																															!
      
End Sub 

Sub CARGA.SERVICIOS.PES
   CALL VISORES4690(1,"SUBIENDO SERVICIOS  ","COMUNICACION V.3.00",0,"L")
   TS.TEMP1$ = RIGHT$("                  "+"1",18)                        !
   TS.TEMP2$ = Armar.Trama.Msg("99","90",TS.TEMP1$,"00","0001","123456")
   TS.TEMP2$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)
End Sub 


Sub Aplica.Pago.Pes
String Xpago$
Integer*4 Xvalor%

      Xvalor% = Val(Gr.SapCore.Vlr$)
		  If Gr.SapCore.AplAn% = -1 Then                   				             \! Formatea valor 
		     Call Format.Amount(Xvalor% * -1) Else                             \!
		     Call Format.Amount(Xvalor%) 															          !
		  Call TSHIECET
      Call VISORES4690(1,"VALOR SUBSIDIO  ","$"+TS.TEMP1$,1500,"L")
      TS.BD.DSPPARM = -1                     																	! SET FOR NORMAL BD
      Call TSBDEC01                          																	! FIRST TAKE BAL DUE
      TS.IO.STATE = 10
      Dim TS.IO.PREV.KEYS(10)
      Dim TS.IO.PREV.DATA$(10)
      TS.IO.PREV.MOTORKEY = 0
      Dim Ts.Io.Keys(10)																											! Inicializa Vectores
      Dim Ts.Io.Data$(10)																											! 
      If Gr.SapCore.AplAn% = -1 Then TS.IO.KEYS(1) = 70												! Anulacion pago
      TS.IO.DATA$(3) = Right$(Gr.SapCore.TipVar$,1)                           ! Toma la variedad de pago
      TS.IO.KEYS(3)  = 78																											! Simula la tecla slash
      TS.IO.DATA$(7) = Gr.SapCore.Vlr$																				! Valor del subsidio
      TS.IO.KEYS(7)  = 90 + Val(Left$(Gr.SapCore.TipVar$,1))                  ! Arma secuencia tecleo motor
   	  !TS.IO.DATA$(9) = Gr.SapCore.Benef$                                      ! Beneficiario
   	  
   	  TS.IO.DATA$(9) = "890303208"      																			! ID del Beneficiario Mod 5/Jul/2019 GR-OVS
      TS.IO.MOTORKEY = 90 + Val(Left$(Gr.SapCore.TipVar$,1))                  ! Arma secuencia tecleo motor
      Gr.SapCore.TrxCed% = 0 																						      ! Cancela proceso
      Gr.SapCore.InTrx% = 2
      TS.TEMP1I1 = 0
      For TS.TEMP1I1 = 0 To 7
          TO.NEGAMT(TS.TEMP1I1) = 9999999
          TO.NEGCNT(TS.TEMP1I1) = 999
      Next TS.TEMP1I1           

            TS.TEMP2$ = "00"   																							! Operacion Venta
            If Gr.SapCore.AplAn% = -1 Then Begin                            !
      	       TS.TEMP2$ = "01"                       										  ! Operacion Anulacion
            EndIf
            TS.TEMP1$  = Pack$(Gr.SapCore.Benef$)                  +       \! ID beneficiario
               ":"+Pack$(Gr.SapCore.Vlr$)                          +       \! Valor subsidio entregado
               ":"+Pack$(Right$("00"+Str$(Gr.SapCore.InTrx%),2))   +       \! Tipo de opearacion 1. Cobro 2. Compra
               ":"+Pack$(TS.TEMP2$)                                +       \! Signo Operacion 
               ":"+Gr.SapCore.TRama$                               +       \! Trama para reversos
               ":"+Pack$(Gr.SapCore.Token$)                        +       \! Nro Token operacion
               ":"+Pack$(Gr.SapCore.Celular$)                      +       \! Nro celular operacion
               ":"+Pack$("00")                                              ! Filler 
            Call Grabacion.String.Usuario2("20151109",TS.TEMP1$)            ! Graba String usuario  
      
!      If Gr.SapCore.AplAn% = -1 Then                   				               \! Si anulacion
!         Gr.SapCore.InTrx% = 0 Else 																			   \!
!         Gr.SapCore.InTrx% = 2

End Sub 



Sub SUBSIDIO.SAPCORE(Xopt%) Public
Integer*4 Xopt%
Integer*1 Xtoken%
String Sctmp$

!--- EAMTSU07.J86
If XOPT% = 07 Then Begin																										! Carga de opciones
  Gr.SapCore.Act% = 0 																									    ! Init proyecto activo
  Dim GR.SCNEGCNT(8)
  Dim GR.SCNEGAMT(8)
  TS.TS11WERR$ = ""																													! Control errores
  OPEN "R::$SCNTRL" AS 94 UNLOCKED NOWRITE NODEL												    ! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR APERTURA PARAMETROS SAPCORE")             !
	   Gr.SapCore.Act% = 0																									  ! Init proyecto activo  	 
  	 Exit Sub																																!
  EndIf 
  If End #94 Then UE.FIN.SAPCORE         																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[SUBSIDIO SAP CORE]" Then Begin	     	 					    ! Parametros SAP CORE
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.SapCore.Act% = Val(Mid$(TS.TEMP1$,30,2))      			    			  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.SapCore.Sec% = Val(Mid$(TS.TEMP1$,30,3))      			    			  ! Tecla Motora Asignada
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.SapCore.TipVar$ = Mid$(TS.TEMP1$,30,2)       			    	 		    ! Tipo y variedad definida
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.SapCore.CtlKy% = Val(Mid$(TS.TEMP1$,30,2))   			    	 		    ! Si permite dato no escaneado
         Call SAP.BUSCA.LIMITE.TV("11")																			! Limite de cambio al efectivo
         GoTo UE.FIN.SAPCORE                                                ! Sale del proceso
        EndIf																																!
  Wend 																																		  ! Fin recorrido archivo
  UE.FIN.SAPCORE:
    Close 94 
    Call CARGA.SERVICIOS.PES                                                ! Subir servicios java
    If Gr.SapCore.Act% = -1 Then                                           \! Proyecto Activo
      Call U.IMPRIME("MODULO SUBSID.SAP CORE ON 18/May/2023",2100H) Else   \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO SUBSID.SAP CORE OFF18/May/2023",2100H)         ! Msg Proyecto Cargado
    
EndIf

If Gr.SapCore.Act% = 0 Then Exit Sub 																				! Si proyecto apagado

!--- EAMTSU02.J86
If XOPT% = 02 Then Begin																										! Inicio de transaccion
 If Gr.SapCore.InTrx% = 2 Then Begin																				! Aplicando pago
    TS.TEMP5I2 = CONFIRMAR.SAPCORE                                          ! Confirma movimiento pago subsidio
 EndIf
   Gr.SapCore.Vlr$	= ""																										! Valor a entregar
   Gr.SapCore.Benef$ = ""																										! ID del Beneficiario
   Gr.SapCore.TRama$ = ""                                                   ! Trama del mensaje
   Gr.SapCore.Apl% = 0
   Gr.SapCore.InTrx% = 0
   Gr.SapCore.TrxCed% = 0
   Gr.SapCore.TipEnt% = 0
   Gr.SapCore.Name$ = ""
   Gr.SapCore.MetPag% = 0
   Gr.SapCore.TotBarra% = 0
   Gr.SapCore.Token$ = ""
   Gr.SapCore.Celular$ = ""
   To.TENDLIMITS(Gr.Sapcore.TVPOS%,0) = Gr.Sapcore.MaxTv%										! Retorna valores default
   To.TENDLIMITS(Gr.Sapcore.TVPOS%,1) = Gr.SapCore.ChgTv%										! Retorna valores default
   TS.TEMP1I1 = 0
   For TS.TEMP1I1 = 0 To 7
       TO.NEGAMT(TS.TEMP1I1) = GR.SCNEGAMT(TS.TEMP1I1)
       TO.NEGCNT(TS.TEMP1I1) = GR.SCNEGCNT(TS.TEMP1I1)
   Next TS.TEMP1I1

EndIf

!--- EAMTSU05.J86
If XOPT% = 05 Then Begin																										! Inicio de transaccion
 If Gr.SapCore.InTrx% = 1 Then Begin																				! Aplicando pago
    TS.TEMP5I2 = CONFIRMAR.SAPCORE                                          ! Confirma movimiento pago subsidio
 EndIf
   Gr.SapCore.Vlr$	= ""																										! Valor a entregar
   Gr.SapCore.Benef$ = ""																										! ID del Beneficiario
   Gr.SapCore.TRama$ = ""                                                   ! Trama del mensaje
   Gr.SapCore.Apl% = 0
   Gr.SapCore.InTrx% = 0
   Gr.SapCore.TipEnt% = 0
   Gr.SapCore.TrxCed% = 0
   Gr.SapCore.Name$ = ""
   Gr.SapCore.MetPag% = 0
   Gr.SapCore.TotBarra% = 0
   Gr.SapCore.Token$ = ""
   Gr.SapCore.Celular$ = ""
   To.TENDLIMITS(Gr.Sapcore.TVPOS%,0) = Gr.Sapcore.MaxTv%										! Retorna valores default
   To.TENDLIMITS(Gr.Sapcore.TVPOS%,1) = Gr.SapCore.ChgTv%										! Retorna valores default
EndIf


!--- EAMTSU14.J86
If XOPT% = 14 Then Begin																										! En secuencia de tecleo
	
   If	TS.IO.KEYS(1) = 82 And TS.IO.MOTORKEY = 81 Then Begin                 ! Si Suspension de trx
 		If (Gr.SapCore.InTrx% <> 0) Then Begin                                  ! Si subsidio capturado en trx
 			 Call Visor.And.Borrar("SUSPENSION NO AUTOR.CON PAGO SUBSIDIO  ")     ! Msg de alerta
       Call Visor.And.Borrar("ANULE PAGO SUBSIDIO PARA SUSPENDER TRX ")     ! Msg de alerta
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
 	  EndIf																																		!
   EndIf
	 If TS.IO.MOTORKEY = 69 And Gr.SapCore.TrxCed% = -1 Then Begin 
	 	  TS.TEMP1$ = TS.IO.DATA$(10)																						! Longitd dato

      If Val(TS.TEMP1$) = 26 Then Begin																		  ! Si dato TOTP barra
         Gr.Sapcore.Captura$ = BARRA417.TOTP
         Gr.SapCore.MetPag% = 1																							! Cobro totp
         Gr.SapCore.TotBarra% = 1
         GoTo PROCESA.PES																										! Cobro PES
      EndIf																																  ! Fin TOT barra
      
	 	  If Val(TS.TEMP1$) >= 531 Then Begin																		! Si cc Colombia
	 	     Gr.SapCore.Captura$ = CEDULA.PDF417
	 	  EndIf Else Begin
	 	     Call VISOR.AND.BORRAR("DATO ESCANEADO NO ES UNA CEDULA VALIDA")		!
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Gr.SapCore.TrxCed% = 0 																						! Cancela proceso
         Exit Sub 																											    ! Termina proceso
	 	  EndIf
	 	  If Gr.SapCore.Captura$ <> "" Then Begin                               !
	 	     If Gr.SapCore.AplAn% = -1 Then TS.IO.KEYS(1) = 70
	 	     GoTo Procesa.Pes                                                   ! Procesa pago de subsidio
	 	     
	 	  EndIf Else Begin																											!
	 	     Call VISOR.AND.BORRAR("FALLA LECTURA CEDULA PROCESO CANCELADO")		!
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Gr.SapCore.TrxCed% = 0 																						! Cancela proceso
         Gr.SapCore.AplAn% = 0
         Exit Sub 																											    ! Termina proceso
	 	  EndIf																																	!
	 EndIf 
	 If TS.IO.MOTORKEY <> 69 And Gr.SapCore.TrxCed% = -1 Then Begin 
  	  Call VISOR.AND.BORRAR("SECUENCIA REGISTRO   NO AUTORIZADA...")     		! Msg Alerta
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
      TS.IO.MOTORKEY = 73																										!
      Gr.SapCore.TrxCed% = 0 																								! Cancela proceso
      Exit Sub 																											    		! Termina proceso
 	 EndIf
 	 
 	 
	 If TS.IO.MOTORKEY = Gr.SapCore.Sec% Then Begin														! Si tecla de secuencia

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
      If (TE.TR.AMT(1) + 50000) >= To.CDLIMIT2 Then Begin                   ! PROCESO CERCA A RECOGIDA
   	      Call VISOR.AND.BORRAR("REALICE PROCESO DE RECOGIDA CAJERO AHORA") ! Msg de alerta
   	      Call VISOR.AND.BORRAR("PAGO SUBSIDIO   CANCELADO")								! Msg de alerta
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													!
          TS.IO.MOTORKEY = 73																								!
          Exit Sub 																											    ! Termina proceso
      EndIf 	

      Entrada.Pago.Pes:
      Sctmp$ = ASIC.DATOS$("SELECCIONE TIPO PAGO","1. TOTP 2.MANUAL")       ! Mod 27jUL2021 GR      
      If Sctmp$ = "E" Then BEGIN 																		        ! Proceso cancelado   
       	  Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  !
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  !
          TS.IO.MOTORKEY = 73																						    !
          Exit Sub 																											    !
      EndIf 																														    !
      If Match(Sctmp$," 1 2 ",1) <= 0 Then Begin
      	 Call VISOR.AND.BORRAR("DATO CAPTURADO NO ESVALIDO /Borrar")			  !
      	 GoTo Entrada.Pago.Pes
      EndIf
      Gr.SapCore.MetPag% = Val(Sctmp$)
      If Sctmp$ = "1" Then GoTo Entra.Totp
      If Sctmp$ = "2" Then GoTo Entra.Sapc
      
      Entra.Totp:
      !Sctmp$ = ASIC.DATOS$("METODO DE CAPTURA","1.CEDULA 2.CREDENC.")      !
      Sctmp$ = ASIC.DATOS$("INGRESE 1.CEDULA    ","2.CREDENC. 3.BARRA. ")   !
      If Sctmp$ = "E" Then BEGIN 																		        ! Proceso cancelado   
       	  Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  !
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  !
          TS.IO.MOTORKEY = 73																						    !
          Exit Sub 																											    !
      EndIf 																														    !
      If Match(Sctmp$," 1 2 3 ",1) <= 0 Then Begin
      	 Call VISOR.AND.BORRAR("DATO CAPTURADO NO ESVALIDO /Borrar")			  !
      	 GoTo Entra.Totp
      EndIf

      If Sctmp$ = "1" Then Begin																						! Ingreso con cedula
       TS.TEMP5$ = ASIC.DATOS$("INGRESE NRO CEDULA","DEL BENEFICIARIO..")   !
       If TS.TEMP5$ = "E" Then BEGIN 																		    ! Proceso cancelado   
       	  Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  !
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  !
          TS.IO.MOTORKEY = 73																						    !
          Exit Sub 																											    !
       EndIf 																														    !
       Gr.SapCore.TipEnt% = 1																								!
       Gr.SapCore.Captura$ = TS.TEMP5$																      ! ID capturado
       GoTo PROCESA.PES																											! Ejecuta proceso cobro
      EndIf       																													! Fin ingreso por cedula
      If Sctmp$ = "2" Then Begin																						! 
      	 GoTo Procesa.Credencial																						! Procesa credencial
      EndIf																																	!
      If Sctmp$ = "3" Then Begin																						! Barra PDF 18Nay2023
        TS.ER.RETURN = -1																										! Control errores
        Call CREAR.PIPE.PDF																									! Apertura pipe
        TS.ER.RETURN = -1																										! Control errores
        If TS.ER.RETURN = -1 Then Begin 
           Call VISORES4690(1,"ESCANEE CEDULA DEL  ","TELEF. BENEFICIARIO.",0,"L")  ! 
           If (TS.IO.KEYS(1) = 70) Then Begin 															! Anulando movimiento
           	  Gr.SapCore.AplAn% = -1
           EndIf
           Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  !
           TS.IO.MOTORKEY = 0 																						  !
           Gr.SapCore.TrxCed% = -1
           Gr.SapCore.TipEnt% = 1
           Exit Sub 																											  !
        EndIf Else Begin
           Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  !
           TS.IO.MOTORKEY = 73 																						  !
           Gr.SapCore.TrxCed% = 0
           Exit Sub 																											  !
        EndIf
      	
      EndIf																																	! Fin barra PDF
!-- Proceso de pago metodo anterior
      Entra.Sapc:
      !Sctmp$ = ASIC.DATOS$("METODO DE CAPTURA","1.CEDULA 2.CREDENC.")      !
      Sctmp$ = ASIC.DATOS$("DOC 1.CEDULA 2.CRED.","3.DIGITAL")              ! Mod 27jUL2021 GR
      
      If Sctmp$ = "E" Then BEGIN 																		        ! Proceso cancelado   
       	  Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  !
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  !
          TS.IO.MOTORKEY = 73																						    !
          Exit Sub 																											    !
      EndIf 																														    !
      If Match(Sctmp$," 1 2 3 ",1) <= 0 Then Begin
      	 Call VISOR.AND.BORRAR("DATO CAPTURADO NO ESVALIDO /Borrar")			  !
      	 GoTo Entra.Sapc
      EndIf
    
      If Sctmp$ = "1" Then Begin
        TS.ER.RETURN = -1
        Call CREAR.PIPE.PDF
        If TS.ER.RETURN = -1 Then Begin 
           Call VISORES4690(1,"ESCANEE CEDULA","DEL BENEFICIARIO..",0,"L")  ! 
           If (TS.IO.KEYS(1) = 70) Then Begin 															! Anulando movimiento
           	  Gr.SapCore.AplAn% = -1
           EndIf
           Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  !
           TS.IO.MOTORKEY = 0 																						  !
           Gr.SapCore.TrxCed% = -1
           Gr.SapCore.TipEnt% = 1
           Exit Sub 																											  !
        EndIf Else Begin
           Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  !
           TS.IO.MOTORKEY = 73 																						  !
           Gr.SapCore.TrxCed% = 0
           Exit Sub 																											  !
        EndIf
      EndIf

      Procesa.Credencial: 
      If Sctmp$ = "2" Or Sctmp$ = "3" Then Begin
       TS.TEMP2I4  = 0																											! Control proceso
       Call SALVAR.SECUENCIA
       Call Entrada.Autorizacion																						! Entrada autorizacion al proceso	
       If TS.TEMP2I4  = 0 Then Begin																				!
       	  Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  !
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  !
          TS.IO.MOTORKEY = 73																						    !
          Exit Sub 																											    !
       EndIf																																!
       Call RESTAURAR.SECUENCIA
       If Sctmp$ = "2" Then \
          TS.TEMP5$ = ASIC.DATOS$("ESCANEE CREDENCIAL","DEL BENEFICIARIO..") Else \  !
          TS.TEMP5$ = ASIC.DATOS$("INGRESE CEDULA    ","DEL BENEFICIARIO..") 
       If TS.TEMP5$ = "E" Then BEGIN 																		    ! Proceso cancelado   
       	  Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  !
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  !
          TS.IO.MOTORKEY = 73																						    !
          Exit Sub 																											    !
       EndIf 																														    !
       If Sctmp$ = "2" Then \
       Gr.SapCore.TipEnt% = 2 Else \
       Gr.SapCore.TipEnt% = 3
      EndIf
       Gr.SapCore.Captura$ = TS.TEMP5$																	    ! ID capturado
       If Gr.SapCore.TipEnt% = 2 Then BEGIN
       If Gr.SapCore.CtlKy% = 0 Then                                       \! Valida dato capturado
        If Asic.Device% <> 3 Then Begin 																		! Dato no escaneado
          Call VISOR.AND.BORRAR("SOLO SE PERMITE DATO POR ESCANER")					! Msg de alerta
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  ! Init vectores de aplicacion
          TS.IO.MOTORKEY = 73																								! Inicia tecla motora
         Exit Sub 																												  ! Sale de la rutina
        EndIf 																														  ! 
       ENDIF 

      PROCESA.PES:
	 	  If Not(TS.INTRX) Then Begin  																					! No Esta en plena trx de venta
	 	   If Gr.SapCore.MetPag% = 1 Then Begin	
        Xtoken% = Validacion.Token.Pes																			! Valida captura token
        If Xtoken% = 0 Then Begin																						! Falla validacion token
       	  Call VISOR.AND.BORRAR("ERROR EN VALIDACION DEL TOKEN /Borrar")  	!
       	  Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 									  			! Init vectores de carga
       	  TS.IO.MOTORKEY = 73
 		      Gr.SapCore.InTrx% = 0																							!
 		      Gr.SapCore.TrxCed% = 0
 		      Gr.SapCore.AplAn% = 0
          Gr.SapCore.Captura$ = ""																        	! ID capturado
          Gr.SapCore.Token$ = ""
          Gr.SapCore.TotBarra% = 0
          Gr.SapCore.Celular$ = ""
          Exit Sub 
        EndIf																																!
	 	   EndIf	
	 	  	
         Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 									  			! Init vectores de carga        
         To.TENDLIMITS(Gr.Sapcore.TVPOS%,0) = 9999999   										! Modifica controles entrega efectivo
         To.TENDLIMITS(Gr.Sapcore.TVPOS%,1) = 9999999										    ! Modifica controles entrega efectivo
         TS.IO.DATA$(5) = "1"																								! Secuencia cambio a efectivo          
         TS.IO.KEYS(5)  = 61																								!
         TS.IO.MOTORKEY = 61																								!
         Call TSTCEC01                                                      !
         Call Reclamo.SapCore                                          			! Reclamo subsidio
         Exit Sub                                                           !
      EndIf Else Begin																											! Es una trx de compra
  	     If (TS.TRX.STATUS <> 1 And TS.PROCEDURE = -1) Then Begin	  	      ! No se ha ingresado total en venta
   		      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												! Inicializa secuencia de teclado
   		      TS.IO.MOTORKEY = 0 																							! 
            TS.GUIDANCE = 1020																							! Solicita total a la trx
            Exit Sub 																												! Sale del proceso
         EndIf 																															!
         Gr.SapCore.AplAn% = 0
         If (TS.IO.KEYS(1) = 70) Then Begin 																! Anulando movimiento
         	  If Gr.SapCore.Benef$ <> Gr.SapCore.Captura$ Then Begin          ! ID Invalido 
         	  	 Call Visor.And.Borrar("CREDENCIAL CAPTURADA INVALIDA")       ! 
   		         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)											! Inicializa secuencia de teclado
   		         TS.IO.MOTORKEY = 73																					!
   		         Exit Sub 																										! 
            EndIf	
         	  Gr.SapCore.AplAn% = -1                												  ! En anulacion
         EndIf
!--- Validacion de token
       If Gr.SapCore.MetPag% = 1 Then Begin
         Xtoken% = Validacion.Token.Pes																				! Valida captura token
         If Xtoken% = 0 Then Begin																						! Falla validacion token
       	  Call VISOR.AND.BORRAR("ERROR EN VALIDACION DEL TOKEN /Borrar")  	!
       	  Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 									  			! Init vectores de carga
       	  TS.IO.MOTORKEY = 73
 		      Gr.SapCore.InTrx% = 0																							!
 		      Gr.SapCore.TrxCed% = 0
 		      Gr.SapCore.AplAn% = 0
          Gr.SapCore.Captura$ = ""																        	! ID capturado
          Gr.SapCore.Token$ = ""
          Gr.SapCore.TotBarra% = 0
          Gr.SapCore.Celular$ = ""
          Exit Sub 
         EndIf																																!
       EndIf
         
         TS.TEMP5I2 = 0                    													        ! Si proceso correcto
         If Gr.SapCore.AplAn% <> -1 Then                                   \!
            TS.TEMP5I2 = COBRO.SUBSIDIO.SAPCORE("31")	Else                 \! Cobro subsidio
            TS.TEMP5I2 = ANULACION.SAPCORE          	     							  	! Anulacion Subsidio
      	 If TS.TEMP5I2 <> 0 Then Begin																			! Si proceso correcto
      	 	  Gr.SapCore.Benef$ = Gr.SapCore.Captura$                         ! Asigna ID beneficiario
            Call APLICA.PAGO.PES
            Exit Sub																												! Sale del proceso
         EndIf Else Begin																										! Fin cobro subsidio
   		      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												! Inicializa secuencia de teclado
   		      TS.IO.MOTORKEY = 73																							! 
   		      Gr.SapCore.InTrx% = 0																						!
   		      Gr.SapCore.TrxCed% = 0
   		      Gr.SapCore.AplAn% = 0
         	  Exit Sub 																												! Sale del proceso
         EndIf																															! Fin falla cobro subsidio

      EndIf																																	! Fin trx Compra
   EndIf																																		! Fin secuencia 
   
EndIf																																				! Fin Eamtsu14

!--- EAMTSU60.J86
If Xopt% = 60 Then Begin
 If TS.LINETYPE = 2 And Gr.SapCore.Apl% = -1 Then Begin											! Aplicando pago
    If Not(TS.RECOVERY) Then Begin       
      TS.TEMP2$ = "00"   																										! Operacion Venta
      If Gr.SapCore.AplAn% = -1 Then Begin                                  !
      	 TS.TEMP2$ = "01"                       														! Operacion Anulacion
      EndIf
!-- Mod 27/May/2011
!     TS.TEMP1$  = Pack$(Gr.SapCore.Benef$)                        +       \! ID beneficiario
!              ":"+Pack$(Gr.SapCore.Vlr$)                          +       \! Valor subsidio entregado
!              ":"+Pack$(Right$("00"+Str$(Gr.SapCore.InTrx%),2))   +       \! Tipo de opearacion 1. Cobro 2. Compra
!              ":"+Pack$(TS.TEMP2$)                                +       \! Signo Operacion 
!              ":"+Gr.SapCore.TRama$                               +       \! Trama para reversos
!              ":"+Pack$("00")                                              ! Filler 
!     Call Grabacion.String.Usuario2("20151109",TS.TEMP1$)                  ! Graba String usuario  

      If Gr.SapCore.AplAn% = -1 Then                                       \!
      	 Gr.SapCore.InTrx% = 0
    EndIf
    TS.TEMP1$ = Left$("CREDENCIAL :"+Gr.SapCore.Benef$+String$(38," "),38)  ! Arma mensaje 
    Call U.IMPRIME("CREDENCIAL :"+Gr.SapCore.Benef$,6100H)         				  ! Imprime credencial
    Gr.SapCore.Apl% = 0																											!
    TS.TEMP1I1 = 0
    For TS.TEMP1I1 = 0 To 7
       TO.NEGAMT(TS.TEMP1I1) = GR.SCNEGAMT(TS.TEMP1I1)
       TO.NEGCNT(TS.TEMP1I1) = GR.SCNEGCNT(TS.TEMP1I1)
    Next TS.TEMP1I1
    
 EndIf																																			!
EndIf																																				! Fin eamtsu60

!--- EAMTSU53.J86
If Xopt% = 53 Then Begin																										! Prroceso rec

If Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "99" THEN BEGIN   	    	              ! Si DATA/ENTRY
 If Unpack$(MID$(SL.STR.ENTRY$,3,4)) = "20151109" Then Begin                ! PES Sap Core
    Asc.Tmp.Apun% = 3
    Gr.SapCore.Apl% = -1																										! Aplicacion de pago
    TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                   ! Numero de proyecto
    Gr.SapCore.Benef$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)           ! Credencial beneficiario
    Gr.SapCore.Vlr$   = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)           ! Valor del subsidio
    Gr.SapCore.InTrx% = Val(ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%))      ! Tipo de operacion
    TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                   ! Signo de la operacion
    Gr.SapCore.TRama$ = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)          ! Trama para manejo reversos
    If TS.TEMP1$ = "01" Then Gr.SapCore.InTrx% = 0                          ! Es una anulacion de pago
  EndIf
 EndIf 

EndIf

!--- EAMTSU32.J86
If XOPT% = 32 Then Begin																										! En validacion de pago
 If (TS.IO.DATA$(3) = Right$(Gr.SapCore.TipVar$,1)) And                    \! Si usa forma de pago 
   (TS.IO.KEYS(7)  = Val("9"+Left$(Gr.SapCore.TipVar$,1))) Then Begin       !	pes 
   	If Gr.SapCore.InTrx% = 0 Then Begin 																		! Si no esta en una operacion con PES
       	  Call VISOR.AND.BORRAR("FORMA DE PAGO NO AUTORIZADA      /Borrar")	!
           TS.USER.RETURN = 99
           TS.LINEDATA = 51                 																! "TARJETA INVALIDA"            
           TS.STACKERR(0) = 0                                               ! manager's override required   
           TS.STACKERR(3) = 0                                               ! item descriptor               
           TS.STACKERR(6) = 0                                               ! put "M" on last display       
           TS.STACKERR(7) = -1                                              ! indicate no printing required 
           TS.MO.REASON = 0                                                 ! invalid key with department   
           Call TSCSEC03       																							!
           TS.IO.MOTORKEY = 0 							
   	EndIf																																		! 
 EndIf																																			! Fin validacion forma de pago pes  	
EndIf																																				! Fin validacion forma de pago

!--- EAMTSU10.J86
If XOPT% = 10 Then Begin																										! En reposo la maquina
 If (Match(TS.SDESC$(43),TS.DISP1$,1) > 0) Or                              \!
    (Match(Left$(TS.SDESC$(42),20),TS.DISP1$,1) > 0) Then Begin             ! Con terminal asegurada
   If Gr.Sapcore.Turno% >= 5  Then Begin 
      Call CARGA.SERVICIOS.PES                                              ! Subir servicios java   	   
      Gr.Sapcore.Turno% = 0
      TS.DISP1$  = TS.SDESC$(43)
      TS.TEMP1$  = TS.SDESC$(43)
      TS.TEMP2$  = MID$(DATE$,1,2) + "/" + MID$(DATE$,3,2) + "/" +         \!
                   MID$(DATE$,5,2) + " " + MID$(TIME$,1,2) + ":" +         \!
                   MID$(TIME$,3,2) + " " + Str$(Gr.Sapcore.Turno%)
                    
      Call Visores4690(1,TS.TEMP1$,TS.TEMP2$,0,"L")
   EndIf Else Begin 
   	  TS.TEMP1$ = TS.SDESC$(43)
   	  TS.DISP1$ = TS.SDESC$(43)
   	  Gr.Sapcore.Turno% = Gr.Sapcore.Turno% + 1
      TS.TEMP2$  = MID$(DATE$,1,2) + "/" + MID$(DATE$,3,2) + "/" +         \!
                   MID$(DATE$,5,2) + " " + MID$(TIME$,1,2) + ":" +         \!
                   MID$(TIME$,3,2) + " " + Str$(Gr.Sapcore.Turno%)
      Call Visores4690(1,TS.TEMP1$,TS.TEMP2$,0,"L")
   EndIf	 
 EndIf 
EndIf 																																			!

!--- EAMTSU01.J86
If XOPT% = 01 Then Begin																										! Al ingreso primer Item 
   Gr.Sapcore.Turno% = 0 
EndIf 																																			!

End Sub 
