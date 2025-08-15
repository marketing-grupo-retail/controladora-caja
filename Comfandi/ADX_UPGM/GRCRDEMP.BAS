!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRCRDEMP.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Credito Empresarial              *
!**************************************************
!Observaciones:
!Mod 20Sep2013
! Se asignaun consecutivo para el manejo del credito
! asignado por ma HHMMmmss del movimento, desarrollado
! por Grupo Retail - ovs
!--------------------------------------------------
! Mod 30Sep2013
! Se controla el proceso de suspension de transaccion
! para que no permita realizarlo si tiene un credito
! o formas de pago capturadas.
!---------------------------------------------------
! Mod 3 Oct 2013
! Se adiciona impresion del numero del credito en el
! tiquete de venta, desarrollado GR - ovs
!--------------------------------------------------
! Mod 30 Oct 2013
! Se maneja un consecutivo de numero de credito por 
! tienda, este consecutivo se trabaja con el archivo
! de numeración fiscal definiendo la terminal 99
! para ese manejo, el consecutivo es de 8 digitos
! desarrollado por Grupo Retail - OVS 
!--------------------------------------------------
! Mod 19 Dic 2013
! Se adiciona dialogo para captura de tipo de credito
! definido por Comfandi para las compras de cafeteria
! 1. Credito Cafeteria
! 2. Otros Creditos 
! Desarrollado por Grupo Retail - OVS
!----------------------------------------------------
! Mod 2 ene 2017
! Se adiciona funcionalidad de consulta de afiliado
! para el proceso de cuenta transitoria
! Desarrollado por Grupo Retail - OVS
!----------------------------------------------------
! Mod 18/Feb/2020
! Se ajusta aplicación de pago para evitar errores
! en procesos de autorización de supervisor.
!Desarrollado por Grupo Retail - OVS 
!--------------------------------------------------
! Mod 26/May/2021
! Se adiciona la funcionalidad de la impresión de 
! una copia de tiquete cuando dentro de una trx sea 
! utilizado la forma de pago Bono Emergencia, este
! requerimineto es solicitado por Freddy Guerrero y 
! Andrea Guzman.
! Desarrollado por Grupo Retail - OVS 
!--------------------------------------------------
! Mod 05Abr2024
! Se modifica llamado modulo fiscal por manejo de 
! facturación electronica
! Desarrollado por Grupo Retail - OVS 
!--------------------------------------------------

%ENVIRON T		                          																		! Ambiente de terminal

Integer*1 Global Gr.CrdEmp.Ok%           																		! Control del proyecto
Integer*1 Global Gr.CrdEmp.dat%
Integer*1 Global Gr.CrdEmp.Dup%                                             ! Genera duplicado tiquete
Integer*1 Global Gr.CrdEmp.Capt%                                            ! Credito empleado capturado
Integer*2 Global SL.INDEX
Integer*2 Global Gr.CrdEmp.Key%          																	  ! Tecla motora
Integer*4 Global Asc.Tmp.Apun%                                              ! 
Integer*1 Global Gr.Fis.Sesion%                                             ! Sesion numeracion fiscal

String    Global Gr.CrdEmp.Nit$,																					 \! Nit empresa
                 Gr.CrdEmp.Name$, 																				 \! Nombre empleado
                 Gr.CrdEmp.Empr$, 																				 \! Nombre empresa
                 Gr.CrdEmp.VlrAut$,																				 \! Valor Autorizado
                 Gr.CrdEmp.Saldo$, 																				 \! Saldo
                 Gr.CrdEmp.VlrCompra$,  																	 \! Valor compra
                 Gr.CrdEmp.IdClte$,     																	 \! Id del cliente
                 Gr.CrdEmp.Trama$,                                         \! Trama original
                 HORA.MUNDIAL$,                                            \! Hora trx original
                 Gr.CrdEmp.Nro$,                                           \! Nro Credito asignado
                 Gr.CrdEmp.Dlg1$,                                          \! Dialogo 1
                 Gr.CrdEmp.Dlg2$,                                          \! Dialogo 2
                 Gr.CrdEmp.Dlgx$,                                          \! Dialogo seleccionado
                 Gr.CrdEmp.TipVar$                                          ! Tipo y variedad de pago

String    GLOBAL GrCr$(1)
Integer   Global Gr.CrdEmp.Slend%
Integer*2 GLOBAL GRindex%
Integer*2 Global GR.CENEGCNT(1)                                             !
Integer*4 Global Gr.CrdEmp.TVPOS%,                                         \!
                 Gr.CrdEmp.MaxTv%,                                         \! Maximo pago   tipo y variedad
                 Gr.CrdEmp.ChgTv%,                                         \! Maximo cambio tipo y variedad
                 GR.CENEGAMT(1) 																						!
                 
%INCLUDE EAMTSWKG.J86          																			        ! working storage
%INCLUDE EAMTRANS.J86          																			        ! working storage
%INCLUDE EAMITEMR.J86          																			        ! Maestra productos
%INCLUDE EAMTOPTS.J86          																			        ! working storage
%INCLUDE EAMTSEVA.J86          																							! Variables EMS
%INCLUDE EAMTSXHC.J86          																							! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          																							! Rutinas de Assembler

%INCLUDE RECATSSU.011          																			        ! Rutinas Genericas Grupo Retail

Function FORMAT.AMOUNT(AMT1) External   																		! Formateo de valores
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub TSPREC01 EXTERNAL          ! print routine
End Sub

Sub TSHIECET EXTERNAL
End Sub

Sub TSBDEC01 EXTERNAL          ! balance due (normal/foodstamp)
End Sub                        !


Sub Valida.Consecutivo.Creditos																							! Validacion control creditos
String X.Key$, X.LEC$, X.Tmp$																								!
 X.KEY$ = PACK$("0099")         										                        ! consecutivo creditos    
 X.Lec$="C2 C8 C7 C4 2I4 C6 2C3 I4"	                                        ! Nuevo Formato 29Oct2018 
 X.Lec$ = "C2 C96"                                                          ! Nuevo Formato 05Abr2024 
 TS.ER.RETURN = -1                                                          ! Control de errores                    
 Read FORM X.LEC$;#Gr.Fis.Sesion% KEY X.KEY$;                              \! Busca Terminal                           
      X.KEY$, 																		                         \! Numero de la terminal 
      X.Tmp$      																   												! Total del registro
End Sub 																																		!

Function Asigna.Consecutivo.Creditos																				! Asignacion nro credito
String X.Key$, X.LEC$, Gr.Crd.Numer$(1), X.Tmp$, Xdata$											!
Integer*2  Xtry%																														!
Integer*4  X.ACT%																														!
String Asigna.Consecutivo.Creditos																					!
 Dim Gr.Crd.Numer$(8)																												!
 Xtry% = 0                                                                  !                                            
 Proc.Act.Crd:                                                              !                                            
 X.KEY$ = Pack$("0099")        											 												! Terminal para control consecutivo creditos       
 X.Lec$="C2 C8 C7 C4 2I4 C6 2C3 I4"	                 												! Nuevo Formato 29Oct2018                          
 X.Lec$ = "C2 C4 3C5 C7 2C3 C64"            																! Nuevo Formato 05Abr2024 FECO                     
 TS.ER.RETURN = -1                                                   				! Control errores                                                    
 Read FORM X.LEC$;#Gr.Fis.Sesion% AUTOLOCK KEY X.KEY$;                     \! Busca Terminal                                                 
      X.KEY$, 																		                         \! Numero de la terminal                                       
      Gr.Crd.Numer$(1),                                                    \! Prefijo factura fiscal                                                            
      Gr.Crd.Numer$(2),                                                    \! Rango factura inicial                    
      Gr.Crd.Numer$(3),                                                    \! Rango factura final                      
      Gr.Crd.Numer$(4),                                                    \! Numero actual factura                    
      Gr.Crd.Numer$(5),                                                    \! Numero resolucion                        
      Gr.Crd.Numer$(6),                                                    \! Fecha inicial resolucion                 
      Gr.Crd.Numer$(7),                                                    \! Fecha final resolucion                   
      X.Tmp$                                                                ! Resto del registro       
            
 If TS.ER.RETURN = -1 Then Begin                											      ! Si lectura satisfactoria  
       	X.ACT% = Val(Unpack$(Gr.Crd.Numer$(4)))  											      ! Consecutivo actual        
       	X.ACT% = X.ACT% + 1																						      ! Aumenta consecutivo       
       	Gr.Crd.Numer$(4) = Pack$(Right$("0000000000"+Str$(X.ACT%),10))      ! Actualiza consecutivo     
       	Xtry% = 0																											      !                           
       	Prc.write.crd:
        Write Form X.LEC$;#Gr.Fis.Sesion% AUTOUNLOCK ;                     \! Graba numero de terminal 
          X.KEY$, 																		                     \! Numero de la terminal                                       
          Gr.Crd.Numer$(1),                                                \! Prefijo factura fiscal                                                        
          Gr.Crd.Numer$(2),                                                \! Rango factura inicial                    
          Gr.Crd.Numer$(3),                                                \! Rango factura final                      
          Gr.Crd.Numer$(4),                                                \! Numero actual factura                    
          Gr.Crd.Numer$(5),                                                \! Numero resolucion                        
          Gr.Crd.Numer$(6),                                                \! Fecha inicial resolucion                 
          Gr.Crd.Numer$(7),                                                \! Fecha final resolucion                   
          X.Tmp$                                                            ! Resto del registro       
        If TS.ER.RETURN = -1 Then BEGIN     													      ! Proceso correcto     
       	   Xdata$ = Right$("000000"+Str$(X.ACT%),6)        						      ! Consecutivo asignado 
       	EndIf Else Begin																	 						      !                      
       		 Xtry% = Xtry% + 1 																					      !                      
       	   If Xtry% <= 3 Then Begin												 						      ! Maximo 3 reintentos  
       	   	  Wait ; 500																							      !                      
       	 	    GoTo Prc.write.crd																			      !                      
       	   EndIf																											      !                      
       	   Call VISOR.AND.BORRAR("ERROR DE GRABAC. EN  CONSECUTIVO CREDITOS")
           Call U.IMPRIME("ERROR GRABACION  EN CONSECUTIVO CRD",2100H)      !                                 
       	   Xdata$ = Right$("000000"+time$,6)                 					      ! Consecutivo trx                 
       	EndIf																							 						      !                                 
       EndIf Else Begin																		 						      ! Si consecutivo no parametrizado 
       	 Xtry% = Xtry% + 1 																						      !                                 
       	 If Xtry% <= 3 Then Begin													 						      ! Maximo 3 reintentos             
       	 	  wait ; 500																								      !                                 
       	 	  GoTo Proc.Act.Crd																					      !                                 
       	 EndIf																												      !                                 
       	 Call VISOR.AND.BORRAR("ERROR DE LECTURA EN  CONSECUTIVO CREDITOS") !
         Call U.IMPRIME("ERROR DE LECTURA EN CONSECUTIVO CRD",2100H)  			!
       	 Xdata$ = Right$("000000"+time$,6)                 									! Consecutivo trx
       EndIf																																!
       Asigna.Consecutivo.Creditos = Xdata$                									! Retorna consecutivo
End Function 

Sub CRDEMP.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:CE" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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

Sub CONSUMO.CUPO(Xfunc$)
String XRtrama$, Xenvia$, Xllega$, XSALE$, XRTA$, Xfunc$
 TS.TEMP1$ = Right$("                  "+TS.TEMP5$,12) + Right$("          "+TS.TEMP4$,10)
 TS.TEMP2$ = Armar.Trama.Msg("16",Xfunc$,TS.TEMP1$,"00","0001","123456") 		! Armar trama MSG                	
 Xenvia$   = TS.TEMP2$																											!
 Xsale$    = Date$																													! Hora salida requerimiento
 XRtrama$  = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)  			! Ejecuta Requerimiento
 
 !Xrtrama$ = "1601153200000ACTUALIZACION SE REALIZO DE MANERA SATIF890303208   MARIA  DEL PILAR  ACEVEDO  BEJ          207009    161509    COMFANDI                           "
 
 
 Xrta$     = Date$																													! Hora llegada requerimiento
 Xllega$   = XRtrama$																												!
 Call CRDEMP.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                      ! Rastro auditoria
 TS.TEMP1$ = Valida.Rta(XRtrama$)																					  ! Valida rta entregada
 If TS.TEMP1$ = "00" Then Begin																							! Proceso satisfactorio
	  TS.TEMP1$ = MID$(XRtrama$,12,2)                                         ! Toma rta de la BD
		If TS.TEMP1$ <> "00" Then Begin								   									    	! Si presenta un Error
			 Call VISOR.And.BORRAR(Mid$(XRtrama$,14,40))                          ! Msg de Error	
       TS.TEMP1$ = MID$(XRtrama$,12,2)                                      ! Toma rta de la BD
       If TS.TEMP1$ = "03" Then Begin																				! Msg de cupo insuficiente
          Gr.CrdEmp.Saldo$  = Mid$(XRtrama$,116,10)  								  		  ! Saldo 
          Call Format.Amount(Val(Gr.CrdEmp.Saldo$))													! formatea valor
          Call VISOR.AND.BORRAR("CUPO DISPONIBLE    :$"+TS.TEMP1$)	    	  ! Presenta Saldo disponible
          Gr.CrdEmp.Saldo$ = ""																							! Init variable
       EndIf
			 TS.TEMP5I1 = 0																											  ! Proceso no satisfactorio
			 Exit Sub 																														! Sale del proceso
    EndIf																																		! Proceso satistactorio
    Gr.CrdEmp.Nit$    = Str$(Val(Mid$(XRtrama$,54,12)))				  						! Nit empresa
    Gr.CrdEmp.Name$   = Mid$(XRtrama$,66,40)  								  						! Nombre empleado
    Gr.CrdEmp.VlrAut$	= Mid$(XRtrama$,106,10) 									  					! Valor autorizado
    Gr.CrdEmp.Saldo$  = Mid$(XRtrama$,116,10)  								  					  ! Saldo 
    Gr.CrdEmp.Empr$   = Mid$(XRtrama$,126,35)  								  					  ! Nombre Empresa
    Gr.CrdEmp.VlrAut$	= Str$(Val(Gr.CrdEmp.VlrAut$))												! Ajusta dato
    Gr.CrdEmp.Saldo$  = Str$(Val(Gr.CrdEmp.Saldo$))													! Ajusta dato
    TS.TEMP5I1 = -1																												  ! Proceso no satisfactorio
    
    Gr.CrdEmp.Trama$ = Right$("0000"+TS.STORE$,4)                     +    \! Numero del almacen
					             Right$("000000"+Str$(SL.HD.TERMINAL),6)        +    \! Numero de terminal
					             UnPack$(TS.OPER$)                              +    \! Codigo del cajero
					             Right$("000000"+Str$(SL.HD.TRANSNUM+1),6)      + 	 \! Numero de transaccion
					             HORA.MUNDIAL$                                  +    \! Fecha y Hora de la operacion                      
					             Right$("                  "+TS.TEMP5$,12)      +    \! Numero cedula 
					             Right$("          "+TS.TEMP4$,10)                    ! Valor Compra
    Exit Sub 
 EndIf																																			!
 TS.TEMP5I1 = 0																														  ! Proceso no satisfactorio
End Sub

Sub Actualiza.Cupo(Xfunc$)																									! Proceso Actualiza Cupo
String XRtrama$, Xenvia$, Xllega$, XSALE$, XRTA$, Xfunc$										!
 TS.TEMP1$ = Gr.CrdEmp.Trama$                                               ! Trama original
 TS.TEMP2$ = Armar.Trama.Msg("16",Xfunc$,TS.TEMP1$,"00","0001","123456") 		! Armar trama MSG                	
 Xenvia$   = TS.TEMP2$																											!
 Xsale$    = Date$																													! Hora salida requerimiento
 XRtrama$  = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)  			! Ejecuta Requerimiento
 
 !Xrtrama$ = "1602046200000ACTUALIZACION SE REALIZO DE MANERA SATIF"
 
 Xrta$     = Date$																													! Hora llegada requerimiento
 Xllega$   = XRtrama$																												!
 Call CRDEMP.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                      ! Rastro auditoria
 TS.TEMP1$ = Valida.Rta(XRtrama$)																					  ! Valida rta entregada
 If TS.TEMP1$ = "00" Then Begin																							! Proceso satisfactorio
	  TS.TEMP1$ = MID$(XRtrama$,12,2)                                         ! Toma rta de la BD
		If TS.TEMP1$ <> "00" Then Begin								   									    	! Si presenta un Error
			 Call VISOR.And.BORRAR(Mid$(XRtrama$,14,40))                          ! Msg de Error
			 TS.TEMP5I1 = 0																											  ! Proceso no satisfactorio
			 Exit Sub 																														! Sale del proceso
   EndIf																																		! Proceso satistactorio
   TS.TEMP5I1 = -1																												  ! Proceso no satisfactorio
   Exit Sub 																																!
 EndIf																																			!
 TS.TEMP5I1 = 0																														  ! Proceso no satisfactorio
End Sub																																			!

Sub COPIA.TIQUETE.CREDITO                                                   ! Copia tiquete
Integer*4 Cri%	 
	 If Gr.CrdEmp.Dup% = -1 Then Begin																				! Genera duplicado tiquete
	 	  To.UserExit(20) = 0																										! Apaga controls UE 20
	 	  To.UserExit(60) = 0																										! Apaga controls UE 60
      Write #34 ; " *** C O P I A   T I Q U E T E ***" + Chr$(10)          	! Copia tiquete	    	 	  
	    For CrI% = 1 To GrIndex%																							!
	       Write #34 ;GrCr$(Cri%) + Chr$(10)																	! Copia del tiquete
	    Next Cri%		
	    Write #34 ; " " + Chr$(10) : Write #34 ; " " + Chr$(10)								! Saltos de linea
	    Write #34 ; " " + Chr$(10) : Write #34 ; " " + Chr$(10)
	    Write #34 ; " " + Chr$(10) : Write #34 ; " " + Chr$(10)
	    Write #34 ; " " + Chr$(10) : Write #34 ; " " + Chr$(10)
      Call U.CORTACR																												! Gillotina Papel
	    To.UserExit(20) = -1																									! Activa control UE 20
	    To.UserExit(60) = -1																									! Activa control UE 60
      TS.LINETYPE = 18																										  ! Genera cabecera 
      TS.LINEDATA = 1																											  ! en proximo tiquete
      Call TSPREC01
	 EndIf
End Sub 

Function CONSULTA.AFILIADO(XID$) Public
String XRtrama$, Xenvia$, Xllega$, XSALE$, XRTA$, XID$
String Xt1$, Xt2$
Integer*1 CONSULTA.AFILIADO

 CONSULTA.AFILIADO = 0																											! Inicializa proceso
 Call VISORES4690(1,"VALIDANDO EMPLEADO",XID$,500,"L")
 Xt1$ = Right$("                  "+XID$,12)																! Consulta afiliado
 Xt2$ = Armar.Trama.Msg("18","02",Xt1$,"00","0001","123456") 		  					! Armar trama MSG                	
 Xenvia$   = Xt2$																											      !
 Xsale$    = Date$																													! Hora salida requerimiento
 XRtrama$  = Rutina.Java("com.appl.ApplKernel","threader", Xt2$)  			    ! Ejecuta Requerimiento
 Xrta$     = Date$																													! Hora llegada requerimiento
 Xllega$   = XRtrama$																												!
 Call CRDEMP.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                      ! Rastro auditoria
 Xt1$ = Valida.Rta(XRtrama$)																					  		! Valida rta entregada
 If Xt1$ = "00" Then Begin																									! Proceso satisfactorio
	  Xt1$ = MID$(XRtrama$,12,2)                                         			! Toma rta de la BD
		If Xt1$ <> "00" Then Begin								   									    			! Si presenta un Error
			 Call VISOR.And.BORRAR(Mid$(XRtrama$,14,40))                          ! Msg de Error	
			 CONSULTA.AFILIADO = 0																							  ! Proceso no satisfactorio
			 Exit Function 																												! Sale del proceso
    EndIf																																		! Proceso satistactorio
    CONSULTA.AFILIADO = -1																								  ! Proceso satisfactorio
 EndIf																																			!

End Function 

Sub CE.BUSCA.LIMITE.TV(UE.TIPO.VAR$)
  Integer*2 UE.I%
  Integer*1 UE.FOUND%
  String UE.TIPO.VAR$
  UE.I% = 1
  UE.FOUND% = 0 
  Gr.CrdEmp.MaxTv% = 0
  Gr.CrdEmp.ChgTv% = 0 
  While UE.I% <= TO.NUMTNDR AND NOT UE.FOUND%
    If TO.TENDOPTS(UE.I%,0) = VAL(LEFT$(UE.TIPO.VAR$,1)) AND  \
       TO.TENDOPTS(UE.I%,1) = VAL(RIGHT$(UE.TIPO.VAR$,1)) Then Begin 
       Gr.CrdEmp.MaxTv% = TO.TENDLIMITS(UE.I%,0)
       Gr.CrdEmp.ChgTv% = TO.TENDLIMITS(UE.I%,1)
       UE.FOUND%  = -1
       Gr.CrdEmp.TVPOS% = UE.I%
    EndIf Else UE.I% = UE.I% + 1
  Wend    
  UE.I% = 0
  For UE.I% = 0 To 7
      GR.CENEGCNT(UE.I%) = To.NEGCNT(UE.I%)    
      GR.CENEGAMT(UE.I%) = To.NEGAMT(UE.I%)    
  Next UE.I%
End Sub


Sub Calculo.Bono.Emergencia
Integer*4 XI%, Xpag%
String    Xtv$, Xtmp$
  Xpag% = 0
  For XI% = 1 TO Gr.CrdEmp.Slend%                      											! FOR ALL StringS
    H$ = READ.SL.STR$(XI%)                             											! GET String
    If LEN(H$) > 5 Then Begin                          										  ! ASSURE GOOD String
     If ASC(H$) = 5 Then Begin                          										! Forma de pago
     	  Asc.Tmp.Apun% = 3 																								  ! Init apuntador String 
     	  Xtv$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												    ! Tipo y variedad
     	  If Xtv$ = "62" Then Begin																						! Pago con bono emergencia
     	     Xtmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%) 									        ! Valor Pagado
     	     Xpag% = Xpag% + Val(XTmp$)																				! Acumula valor
     	  EndIf
     EndIf     																															! fin forma de pago
     If ASC(H$) = 6 Then Begin                          										! Anulacion Forma de pago
     	  Asc.Tmp.Apun% = 3 																								  ! Init apuntador String 
     	  Xtv$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												    ! Tipo y variedad
     	  If Xtv$ = "62" Then Begin																						! Pago con bono emergencia
     	     Xtmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%) 									        ! Valor Pagado
     	     Xpag% = Xpag% + Val(XTmp$)																				! Acumula valor
     	  EndIf
     EndIf     																															! fin Anulacion forma de pago
    EndIf
   Next XI%
   If Xpag% > 0 Then Gr.CrdEmp.Dup% = -1																		! Genera duplicado tiquete
End Sub

Sub CRDEMPRESAS(XOPT%) Public			  																				! Credito empresas
Integer*4 Xopt%                                                             !
String    Xtmp$
!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.CrdEmp.Ok%  = 0                                                        ! Proyecto Apagado
  Dim GrCr$(2000)																													  ! Copia tiquete
  GrIndex% = 1																															! Init apuntador copia
  Gr.CrdEmp.dat% = 0
  Dim GR.CENEGCNT(8)
  Dim GR.CENEGAMT(8)

  TS.ER.RETURN = -1																													! Ctrl Errores                     
  Open "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL               							! Apertura archivo parametrizacion
  If TS.ER.RETURN <> -1 Then BEGIN                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA CUPO EMPLEADO")									! MSg alerta
  	 Exit Sub 																															! Sale del proceso
  ENDIF 																																		!
  IF END #94 THEN UE.FIN.CRDEMP         																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        IF TS.TEMP1$ = "[CREDITO EMPRESARIAL]" Then Begin		  	     				! Credito empresarial
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.CrdEmp.Ok%   = Val(Mid$(TS.TEMP1$,30,2))   				    					! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdEmp.Key%  = Val(Mid$(TS.TEMP1$,30,03))      				          ! Tecla motora secuencia
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdEmp.TipVar$ = Mid$(TS.TEMP1$,30,02)          				        ! Tipo y variedad pago registro
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdEmp.Dlg1$ = Mid$(TS.TEMP1$,30,20)          				          ! Dialogo 1
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.CrdEmp.Dlg2$ = Mid$(TS.TEMP1$,30,20)          				          ! Dialogo 2
         Call CE.BUSCA.LIMITE.TV(Gr.CrdEmp.TipVar$)                         ! 
         GoTo UE.FIN.CRDEMP  																								! Sale del ciclo de carga          
       Endif                                                                !
   Wend                                                                     !
   UE.FIN.CRDEMP:                                                           !
     Close 94																																! Cierra archivo
   Call Valida.Consecutivo.Creditos                                         ! Valida si ctrl consecutivos existe
   If TS.ER.RETURN <> -1 Then BEGIN 																				! Si no encontrado control en numeracion fiscal
   	  Call VISOR.AND.BORRAR("CONSECUTIVO CREDITOSEMPRESAS NO DEFINIDO")	  	! MSg alerta
   	  Call VISOR.AND.BORRAR("PROCESO NO FUNCIONA PARA ESTA TIENDA..  ")	  	! MSg alerta
   	  Call U.IMPRIME("CONSECUTIVO CREDITO EMPRESA NO DEFIN.",2100H)	      	! MSg alerta
   	  Call U.IMPRIME("PROCESO NO FUNCIONA PARA ESTA TIENDA ",2100H)	  	    ! MSg alerta
      Gr.CrdEmp.Ok%  = 0                                                    ! Proyecto Apagado
   EndIf																																		! 
   If Gr.CrdEmp.Ok% = -1 Then                                              \! Proyecto Activo
      Call U.IMPRIME("MODULO CRD EMPRESAS  ON  05-Abr-2024",2100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO CRD EMPRESAS  OFF 05-Abr-2024",2100H)          ! Msg Proyecto Cargado
EndIf 																																			! Fin carga opciones

If Gr.CrdEmp.Ok% <> -1 Then Exit Sub                                        ! Si proyecto apagado

!--- EAMTSU02.J86
If Xopt% = 01 Then Begin                                                    ! Recuperacion trx
	 Dim GrCr$(2000)																													! Copia tiquete
	 GrIndex% = 1																															! Init apuntador copia
EndIf


!--- EAMTSU02.J86
If Xopt% = 02 Then Begin                                                    ! Recuperacion trx
	 Call Calculo.Bono.Emergencia                                             ! Add 26May2021
	 Call COPIA.TIQUETE.CREDITO																								! Copia tiquete de credito
	 Dim GrCr$(2000)																													! Copia tiquete
	 GrIndex% = 1																															! Init apuntador copia
	 Gr.CrdEmp.Capt% = 0																											! Init variables
   Gr.CrdEmp.Nit$ = "" 																						          ! Nit empresa
   Gr.CrdEmp.Name$ = ""  																					          ! Nombre empleado
   Gr.CrdEmp.VlrAut$ = "" 																					        ! Valor Autorizado
   Gr.CrdEmp.Saldo$ = ""  																					        ! Saldo
   Gr.CrdEmp.VlrCompra$ = ""																								!
   Gr.CrdEmp.IdClte$ = ""																										!
   Gr.CrdEmp.Trama$ = ""																										!
   Gr.CrdEmp.Dup% = 0
   Gr.CrdEmp.dat% = 0
   Gr.CrdEmp.Nro$ = ""
   Gr.CrdEmp.Dlgx$ = ""
   
   To.TENDLIMITS(Gr.CrdEmp.TVPOS%,0) = Gr.CrdEmp.MaxTv%										! Retorna valores default
   To.TENDLIMITS(Gr.CrdEmp.TVPOS%,1) = Gr.CrdEmp.ChgTv%										! Retorna valores default
   TS.TEMP1I1 = 0
   For TS.TEMP1I1 = 0 To 7
       TO.NEGAMT(TS.TEMP1I1) = GR.CENEGAMT(TS.TEMP1I1)
       TO.NEGCNT(TS.TEMP1I1) = GR.CENEGCNT(TS.TEMP1I1)
   Next TS.TEMP1I1
   
   
EndIf																																				! Fin recuperacion trx

!--- EAMTSU08.J86
If Xopt% = 08 Then Begin                                                    ! Registro de producto
	 If (Gr.CrdEmp.Capt% = -1) Then Begin                                     ! Si credito ya capturado
     IR.INDICAT0 = IR.INDICAT0 OR 04H					                              ! No permite venta
     Exit Sub																									  					  ! Sale del Procedimiento     
   EndIf
EndIf																																				! Fin registro producto


!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                    ! Secuencias de tecleo
 If Gr.CrdEmp.Capt% <> 0 Then                                              \!
 If	TS.IO.KEYS(1) = 70 And TS.IO.MOTORKEY = 81 Then Begin                   ! Si anulacion total
 	If (TS.TOTALS(0,0,0) <> TS.BALDUE(0)) Then Begin													! Si pagos capturados
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
   	   TS.GUIDANCE = 1069																										! Msg Error appl nativa
	EndIf
 EndIf


!--- Mod 30 Sep 2012 - GR ovs
!--- Adicion control de tecla suspension con credito empresas               !  

 If	TS.IO.KEYS(1) = 82 And TS.IO.MOTORKEY = 81 Then Begin                   ! Si Suspension de trx
 		If (Gr.CrdEmp.Capt% = -1) Then Begin                                    ! Si Crd ya capturado
 			 Call Visor.And.Borrar("SUSPENSION NO AUTOR.CON MANEJO CREDITOS")     ! Msg de alerta
       Call Visor.And.Borrar("ANULE PAGO CREDITO  PARA SUSPENDER TRX ")     ! Msg de alerta
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
 	  EndIf																																		!
 EndIf

 If TS.IO.MOTORKEY = Gr.CrdEmp.Key% Then Begin 														  ! Si tecla motora
 	If TS.INTRX Then BEGIN 																										! En una trx 
 		
    If Not (TS.BAL.TAKEN) Then Begin																				! No ha entrado total
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
   	   TS.GUIDANCE = 1020																										! Msg Error appl nativa
	     TS.IO.MOTORKEY = 0																										!
	     Exit Sub 																														!
    EndIf																																		!
    
 		If TS.TRAINING Then Begin                    														! Si en entrenamiento
 			 Call Visor.And.Borrar("PROCESO NO VALIDO EN ENTRENAMIENTO")          !
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
 	  EndIf																																		!
 		If (Gr.CrdEmp.Capt% = -1) And (TS.IO.KEYS(1) <> 70) Then Begin          ! Si ya capturado dato
 			 Call Visor.And.Borrar("CREDITO YA APLICADO EN COMPRA")								!
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
 	  EndIf																																		!
 		If (Gr.CrdEmp.Capt% = 0) And (TS.IO.KEYS(1) = 70) Then Begin            ! Anulando crd Sin pago anterior
 			 Call Visor.And.Borrar("CREDITO NO SE APLICADO EN TRX")								!
       Call Visor.And.Borrar("ANULACION NO PERMITIDA       ")								!
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
 	  EndIf																																		!
    TS.TEMP5$ = ASIC.DATOS$("INGRESE CEDULA DEL","EMPLEADO...")             !
    If TS.TEMP5$ = "E" Then BEGIN 																					! Proceso cancelado
    	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")											!
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
    EndIf 																																	!

    TS.TEMP3$ = ASIC.DATOS$("REINGRESE CEDULA DEL","EMPLEADO...")           !
    If TS.TEMP3$ = "E" Then BEGIN 																					! Proceso cancelado
    	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")											!
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
    EndIf 																																	!

    If TS.TEMP5$ <> TS.TEMP3$ Then Begin																		! 
    	 Call VISOR.AND.BORRAR("DATOS INGRESADOS NO COINCIDEN")								!
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
    EndIf

    TS.TEMP4$ = ASIC.DATOS$("VALOR A CONSUMIR  ","DEL CUPO...")             !
    If TS.TEMP4$ = "E" Then BEGIN 																					! Proceso cancelado
    	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")											!
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
    EndIf 																																	!
    If TS.IO.KEYS(1) <> 70 Then                                            \! No es una anulacion pago
     If Val(TS.TEMP4$) > ( TS.BALDUE(0) ) Then Begin                        ! Si dato supera total compra
    	 Call VISOR.AND.BORRAR("MONTO EXCEDE TOTAL  DE LA COMPRA")						!
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
     EndIf

!--- Se adiciona dialogo tipo de credito GR 19Dic 2013
   DLGO.CRD:
    TS.TEMP3$ = ASIC.DATOS$(Gr.CrdEmp.Dlg1$,Gr.CrdEmp.Dlg2$)                ! Captura tipo de credito
    If TS.TEMP3$ = "E" Then BEGIN 																					! Proceso cancelado
    	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")											!
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
    EndIf 																																	!
    If Match(TS.TEMP3$,"12",1) <= 0 Then Begin
    	 Call VISOR.AND.BORRAR("OPCION CRD NO VALIDA PRESIONE BORRAR")				!
    	 GoTo DLGO.CRD
    EndIf

    
!-- Fin de lineas adicionadas
    
    Call VISORES4690(1,"VALIDANDO CUPO",TS.TEMP5$,0,"L")										!
    TS.TEMP5I1 = 0																													! Init control del proceso
    TS.TEMP6$ = "01" 																												! Funcion de consulta
    If (TS.IO.KEYS(1) = 70) Then Begin 																			!
     If (Val(TS.TEMP5$) <> Val(Gr.CrdEmp.IdClte$)) Or                      \! Si datos de anulacion
        (Val(TS.TEMP4$) <> Val(Gr.CrdEmp.VlrCompra$)) Or                   \! 
        (Val(TS.TEMP3$) <> Val(Gr.CrdEmp.Dlgx$)) Then Begin                 ! no son iguales a la compra
    	  Call VISOR.AND.BORRAR("VALORES NO COINCIDEN CON DATO COMPRA")       !
        Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
        TS.IO.MOTORKEY = 73																									!
        Exit Sub 																														!
     EndIf   	
     TS.TEMP6$ = "03" 													                            ! Funcion de anulacion
    EndIf Else Begin
    	Gr.CrdEmp.Dlgx$ = TS.TEMP3$	  																				! Retorna dato capturado tipo de credito
    EndIf

    If TS.TEMP6$ = "01" Then                                               \! Es un consumo
       Call CONSUMO.CUPO(TS.TEMP6$)																					! Valida cupo del empleado
    If TS.TEMP6$ = "03" Then                                               \! Es una anulación consumo
       Call Actualiza.Cupo(TS.TEMP6$)	  																		! Valida cupo del empleado
       
    If TS.TEMP5I1 <> -1 Then Begin																					! Proceso no satisfactorio
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Init secuencia de tecleo
       TS.IO.MOTORKEY = 73																									!
    	 Exit Sub 																														! Salir del proceso
    EndIf																																		! 
    TS.BD.DSPPARM = -1                     																	! SET FOR NORMAL BD
    Call TSBDEC01                          																	! FIRST TAKE BAL DUE
    TS.IO.STATE = 10
    
    Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 																! Init vectores de carga
    If TS.TEMP6$ = "03" Then TS.IO.KEYS(1) = 70    													! Funcion de anulacion
    TS.IO.DATA$(3) = Right$(Gr.CrdEmp.TipVar$,1)                    				! Variedad de Pago
    TS.IO.KEYS(3)  = 78                                             				! Tecla Slash
    TS.IO.DATA$(7) = TS.TEMP4$                              								! Valor a entregar
    TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.CrdEmp.TipVar$,1))           				! Tipo de Pago
    TS.IO.DATA$(9) = TS.TEMP5$																							! Nit asociado
    TS.IO.MOTORKEY = TS.IO.KEYS(7)																					!
    
    To.TENDLIMITS(Gr.CrdEmp.TVPOS%,0) = 9999999   										      ! Modifica controles forma de pago
    To.TENDLIMITS(Gr.CrdEmp.TVPOS%,1) = 9999999									            ! Modifica controles forma de pago
    TS.TEMP1I1 = 0
    For TS.TEMP1I1 = 0 To 7
        TO.NEGAMT(TS.TEMP1I1) = 9999999
        TO.NEGCNT(TS.TEMP1I1) = 999
    Next TS.TEMP1I1           
    
    
    Xtmp$ = Mid$(Hora.Mundial$,3,4) + Mid$(Hora.Mundial$,9,4)               ! MMDDmmss de la trx de mensajeria
    Xtmp$ = Asigna.Consecutivo.Creditos                                     ! Asignacion Consecutivo Credito
    TS.TEMP1$ = PACK$(TS.TEMP5$)        +                                  \! Cedula beneficiario
                ":" + PACK$(TS.TEMP4$)  +                                  \! Valor consumo
                ":" + PACK$(Gr.CrdEmp.Nit$)  +                             \! Nit Empresa
                ":" + Gr.CrdEmp.Name$        +                             \! Nombre afiliado
                ":" + PACK$(Gr.CrdEmp.VlrAut$)  +                          \! Cupo total
                ":" + PACK$(Gr.CrdEmp.Saldo$)   +                          \! Saldo cupo
                ":" + PACK$(TS.TEMP6$)          +                          \! Tipo Movimiento 01-03
                ":" + Gr.CrdEmp.Trama$          +                          \! Trama original
                ":" + Gr.CrdEmp.Empr$           +                          \! Nombre empresa
                ":" + Pack$(Xtmp$)              +                          \! Numero de documento TTcttxxxx
                ":" + Gr.CrdEmp.Dlgx$           +                          \! Tipo de Credito 
                ":" + PACK$("00")                                           ! Filler
    Call Grabacion.String.Usuario2("20130829",TS.TEMP1$)                    ! Almacena UD
    Gr.CrdEmp.VlrCompra$ = TS.TEMP4$                                        ! Valor de la comnpra
    Gr.CrdEmp.IdClte$ = TS.TEMP5$     																		  ! Id del cliente
    Gr.CrdEmp.Nro$ = Xtmp$                                                  ! Nro del credito
    If TS.TEMP6$ = "03" Then Gr.CrdEmp.Capt% = 0 Else 									   \!
       Gr.CrdEmp.Capt% = -1																					        !
    Gr.CrdEmp.dat% = -1
  EndIf Else Begin																													! Si fuera de venta
    Call VISOR.AND.BORRAR("PROCESO NO PERMITIDO FUERA DE VENTA")						! Msg de alerta
    Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																! Init secuencia de tecleo
    TS.IO.MOTORKEY = 73																											!
  EndIf 																																		! 
 EndIf 																																			!
EndIf 																																			! Fin secuencias de tecleo

!--- EAMTSU20.J86
If Xopt% = 20 Then Begin                                                    ! Impresion en CR
 If (TS.LINETYPE = 18) And (TS.LINEDATA = 99) Then BEGIN
  If (TS.TOTALS(0,0,0) > 0) Then BEGIN
     Gr.CrdEmp.Slend% = SL.END								! Total de strings
 EndIf
EndIf

 If Not TS.TRAINING Then Begin                       												! Si no en entrenamiento
  If TS.INTRX Then                                  											 \! Si esta en una compra
    If TS.LINETYPE = 6 And                                                 \! Store Data y Transacc
       TS.LINEDATA = 1 Then Begin                    											  ! Fecha, Hora, etc
        If TS.TENDERED (0) <> 0 Or                                         \! Si hay pagos
           TS.TRX.STATUS <> 100 Then Begin                                  ! y NO hay VOID en curso
            If Gr.CrdEmp.Capt% = 0 Then Begin	                              ! No Hay credito capturado
               Exit Sub                                                     ! Sale del proceso
            EndIf																														!
            Call U.Imprime("*----   INFORMACION DE CREDITO  ----*",2100H)		! Impresion del credito
            Call U.Imprime("*----   INFORMACION DE CREDITO  ----*",4100H)		! Impresion del credito
            TS.TEMP1$ = "NIT    :" + Gr.CrdEmp.Nit$   
            TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
            Call U.Imprime(TS.TEMP1$,6100H)
            TS.TEMP1$ = "Razon  :" + Gr.CrdEmp.Empr$  
            TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
            Call U.Imprime(TS.TEMP1$,6100H)
            TS.TEMP1$ = "CEDULA :" + Gr.CrdEmp.IdClte$
            TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
            Call U.Imprime(TS.TEMP1$,6100H)
            TS.TEMP1$ = "NOMBRE :" + Gr.CrdEmp.Name$
            TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
            Call U.Imprime(TS.TEMP1$,6100H)
            TS.TEMP1$ = "CREDITO:" + Gr.CrdEmp.Nro$
            TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
            Call U.Imprime(TS.TEMP1$,6400H)
            TS.TEMP1$ = String$(27,"-")
            TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
            Call U.Imprime(TS.TEMP1$,6100H)
            TS.TEMP1$ = "FIRMA CREDITO" 
            TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
            Call U.Imprime(TS.TEMP1$,4100H)
            Call U.Imprime(TS.TEMP1$,2100H)
            Call U.Imprime(String$(37,"-"),6100H)														!
            TS.TEMP5I1 = 0
            Call TSHIECET																										! Tono de alerta
            Call VISORES4690(1,"ACTUALIZANDO CUPO","CREDITO, ESPERE..",0,"L") ! Msg alerta
            Call Actualiza.Cupo("02")  																			! Confirmacion uso del cupo
            If TS.TEMP5I1 <> -1 Then Begin 																  ! Proceso no satisfactorio
            	 Call Visor.And.Borrar("CUPO NO ACTUALIZADO CORRECTAMENTE")   ! Msg de alerta cajero
            	 Call Visor.And.Borrar("PROCESO ENCOLADO    PARA ACTUALIZAR") ! 
            EndIf
            Gr.CrdEmp.Dup% = -1
        EndIf
     EndIf
  EndIf
  
EndIf																																				! Fin impresion CR

!--- EAMTSU53.J86
If Xopt% = 53 Then Begin                                                    ! Recuperacion trx
IF Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "99" THEN BEGIN   	    	              ! Si DATA/ENTRY
 If Unpack$(MID$(SL.STR.ENTRY$,3,4)) = "20130829" Then Begin                ! Credito Empresas
    Asc.Tmp.Apun% = 3
    TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                   ! Numero de proyecto
    Gr.CrdEmp.IdClte$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)           ! Cedula Beneficiario
    Gr.CrdEmp.VlrCompra$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)        ! valor consumo
    Gr.CrdEmp.Nit$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)              ! nit empresa
    Gr.CrdEmp.Name$ = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)            ! nombre afiliado
    Gr.CrdEmp.VlrAut$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)           ! cupo de credito
    Gr.CrdEmp.Saldo$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)            ! saldo disponible
    TS.TEMP1$      = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)              ! tipo movimiento
    If TS.TEMP1$ = "01" Then Gr.CrdEmp.Capt% = -1 Else                     \!
       Gr.CrdEmp.Capt% = 0
    Gr.CrdEmp.Trama$ = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)           ! Trama original
    Gr.CrdEmp.Empr$  = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)           ! Nombre empresa
    Gr.CrdEmp.Nro$   = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)            ! Numero del credito
    Gr.CrdEmp.Dlgx$  = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)            ! Tipo de credito
    Gr.CrdEmp.dat% = -1 																										! Init dato impresion
 Endif
EndIf	
EndIf																																				! Fin recuperacion trx


!--- EAMTSU60.J86
If Xopt% = 60 Then Begin                                                    ! Impresion CR
	
	 If Gr.CrdEmp.dat% = -1 Then  																					 \! Si Pago Credito
	 If TS.LINETYPE = 2 And TS.LINEDATA = 60 And TS.PROCEDURE = -1 Then Begin ! 
      To.USEREXIT(20) = 0
      To.USEREXIT(60) = 0
	 	  Call VISOR.AND.BORRAR("NOMBRE EMPLEADO CRD:"+Gr.CrdEmp.Name$)					! Nombre del afiliado
      TS.TEMP1$ = "CEDULA :" + Gr.CrdEmp.IdClte$														! Imprime nro cedula credito
      TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												!
      Call U.Imprime(TS.TEMP1$,6100H)																				!
	 	  Gr.CrdEmp.dat% = 0																										! Init dato impresion
      To.TENDLIMITS(Gr.CrdEmp.TVPOS%,0) = Gr.CrdEmp.MaxTv%								  ! Retorna valores default
      To.TENDLIMITS(Gr.CrdEmp.TVPOS%,1) = Gr.CrdEmp.ChgTv%									! Retorna valores default
      TS.TEMP1I1 = 0
      For TS.TEMP1I1 = 0 To 7
         TO.NEGAMT(TS.TEMP1I1) = GR.CENEGAMT(TS.TEMP1I1)
         TO.NEGCNT(TS.TEMP1I1) = GR.CENEGCNT(TS.TEMP1I1)
      Next TS.TEMP1I1
      To.USEREXIT(20) = -1
      To.USEREXIT(60) = -1
	 EndIf
	 If GrIndex% < 2000 Then Begin																						! Maximo 2000 lineas
      GrCr$(Grindex%) = TS.PRTBUF$																					! Almacena buffer
      Grindex% = GrIndex% + 1 																							! Aumenta apuntador
   EndIf
EndIf

End Sub 
