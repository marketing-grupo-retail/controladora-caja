!************************************************** 
!Empresa       : Grupo Retail S.A                 *
!Programa      : RECMEXIT.BAS                     *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Noviembre de 2007                *
!Observaciones : Modulo Recaudo Serv. Publicos    *
!**************************************************
! Mod17 Ene 2008
! Se modifica la fecha de validacion del recaudo simple
! para que solo se permita recaudar recibos del dia 
! activo.
! Solicitado por ComfAndi y Simple, ajuste realizado
! por Oscar Valencia
!---------------------------------------------------
! Mod21May2008
! Se adiciona controles de fecha de vencimiento, ingreso
! manual del EAN y captura del valor dependiendo del
! tipo de recaudo.
! Desarrollado por OVS 21 de Mayo 2008.
!----------------------------------------------------
! Mod15Oct2008
! Se modifica la validacion de la fecha de vencimiento para
! los recaudos de simple permitiendo no solo la fecha limite
! si no fechas anteriores al vencimiento de la factura
! Desarrollado por Grupo Retail - OVS 
!-------------------------------------------------------------
! Mod 05Ene2009
! Se adiciona la funcionalidad para el recaudo de reservas 
! Hoteleras, desarrollado por Grupo Retail - OVS
!----------------------------------------------------------------
! Mod 16Feb2009
! Se adiciona el control para los recaudos de TelePalmira y
! de AcuaViva para captura de numero de recibo especiales.
! Desarrollador por Grupo Retail - OVS
!----------------------------------------------------------------
! Mod 08Abr2009
! Se adiciona el proceDimiento de recaudo de colegios, y educacion
! adultos  para ser validado en linea.
! Desarrollado por Grupo Retail - OVS
!----------------------------------------------------------------
! Mod 15Jul2009
! Se modifica la lectura del recibo de recaudo de simple, tomAndo 
! el formato de fecha de 6 digitos segun requerimiento entregado 
! por ComfAndi, ahora el formato es:
! 88PPPPPPPPPPVVVVVVVAAMMDD
! Modificado 15 jul 2009 Grupo Retail - OVS
!-----------------------------------------------------------------
! Mod 02Nov2009
! Se controla el proceso de recaudo de reservas que sea transaccion unica
! desarrollado por Grupo Retail - OVS
!------------------------------------------------------------------
! Mod 19Ene2010
! Se controla el proceso de recaudo de Simple sea transaccion unica
! desarrollado por Grupo Retail - OVS
!------------------------------------------------------------------
! Mod 12Abr2010
! Se controla en el proceso de pago si esta cerca 
! una recogida total de caja, no permita el proceso
! de pago y solicite al cajero realizar la recogida
! de dinero.
! desarrollado por Grupo Retail - OVS
!---------------------------------------------------
! Mod 15 Feb 2011
! Se controla en la impresion de franqueos de Emcali
! que solo se franque un documento, requerimiento
! solicitado por Freddy Guerrero.
! Desarrollado por Grupo Retail - OVS
!---------------------------------------------------
! Mod 15 Feb 2011
! Se controla que el ingreso de los recibos de EMCALI
! no se permita ingresar de manera digitada o simulada
! solicitado por Freddy Guerrero
! Desarrollado por Grupo Retail - OVS
!---------------------------------------------------
! Mod 15 Feb 2011
! Se controla que el registro manual del codigo de 
! recaudo de Simple no se permita de manera digitada
! unicamente escaneado, solicitado por Charles Gomez
! Desarrollado por Grupo Retail - OVS 
!---------------------------------------------------
! Mod 15 Jul 2011
! Se controla que no permita el funcionamiento
! de la tecla de consulta de precio para recaudos
! desarrollado por Grupo Retail - OVS
!-----------------------------------------------
! Mod 23 Nov 2011
! Se ajusta lectura codigo de barras 415, parametrizando
! en el archivo de control TFP0005, colocando como longitud
! del dato 99, esto hace que el aplicativo calcule la longitud 
! del valor, ajuste para recaudo de Mega Obras - Cali.
! Desarrollado por Grupo Retail - OVS 
!------------------------------------------------------------
! Mod 30 Nov 2011
! Se ajusta el procedimiento de recaudo para que no permita
! cerrar la transacción con un pago de PES
! Desarrollado por Grupo Retail - OVS
!------------------------------------------------------------
! Mod 01 Mzo 2012
! Se controla el registro de articulos de recaudo, colocando
! el flag de no venta y que este se encienda en modalidad
! de venta, para evitar el registro manual de estos por parte
! de los cajeros.
! desarrollado por Grupo Retail - OVS
!------------------------------------------------------------
! Mod 27 Jun 2012
! Se adiciona mensaje solicitador por Freddy Guerrero para ser
! impreso en los tiquetes de venta, cuando se realiza el recaudo
! de una reserva hotelera, el mensaje es el siguiente:
! ESTIMADO HUESPED TENGA EN CUENTA 
! ESTA INFORMACION:
! TEMPORADA BAJA
! CHECK IN      4:00 PM
! CHECK OUT     2:00 PM
!
! TEMPORADA ALTA
! CHECK IN      5:00 PM
! CHECK OUT     2:00 PM
!
! Desarrollado por Grupo Retail - OVS
!-------------------------------------------------------------
! Mod 26Oct2012
! Se ajusta bug de proceso que cancelaba registro de articulos 
! pesables despues de que algun recaudo presentara falla por 
! validacion de datos.
! Desarrollado por Grupo Retail - OVS
!-------------------------------------------------------------
! Mod 22Abr2013
! Se controla en la captura manual de numero de referencia
! que dicho numero no sea cero o menor de cero.
! Desarrollado por Grupo Retail - OVS
!------------------------------------------------------------
! Mod Dic2012
! Se modifica el modulo de reservas hoteleras para que al final
! de la transacción, se realice la actualización del movimiento
! en linea, si este falla, utilice el proceso de encolamiento en 
! linea.
! desarrollado por Grupo Retail - OVS/CAC 
!--------------------------------------------------------------
! Mod 05/Mar/2012
! Se desarrolla el proceso de validación de los recaudos de simple
! para validación y actualización en linea
! desarrollado por Grupo Retail - OVS/CAC 
!--------------------------------------------------------------
! Mod 24Abr2013
! Se adiciona manejo de archivo de mensajes en tiquete a la 
! parametrización del modulo de recaudos, para informacion
! al usuario de donde realizar algún tipo de recaudo, se crea
! el archivo TF:MSGREC para la definición de dichos mensajes
! tienen maximo 38 caracteres por 10 lineas de impresion.
! el formato del archivo es el siguiente:
! <1>   --> Numero del mensaje
! "12345678901234567890123456789012345678" maximo 10 lineas
! "12345678901234567890123456789012345678"
! Se adicionan dos (2) lineas a los parametros de Acuaviva y 
! reservas hoteleras para el manejo del mensaje
! GENERA_MENSAJE               1
! NUMERO_MENSAJE               03
! Desarrollado por Grupo Retail - OVS
!------------------------------------------------------------
! MOD 29MAY2013
! Se adiciona un archivo de parametro, para definir por tipo de 
! recaudo si genera o no genera numeracion fiscal, en el archivo
! de parametros TFP0005 se adiciona la linea NUMFIS y se coloca
! un parametro 1.si genera 0.No genera (NUMFIS1)
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------------------
! Mod 8 ene 2014
! Se realiza la inicializacion de la variable SL.CH.AMTCHANGE
! para evitar datos negativos al reportar el pago 
! desarrollado por Grupo Retail - OVS 
!--------------------------------------------------------------
! Mod 31 jul 2014
! Se controla que todos los tipos de recaudos capturados sean en
! transacción exclusiva y no permita su mezcla con venta de productos
! desarrollado por Grupo Retail - OVS 
!--------------------------------------------------------------
! Mod 14 Ago 2014
! Se modifica modulo recaudo simple, para que no valide fecha de
! vencimiento y si falla la comunicación, no permita el recaudo
! desarrollado por Grupo Retail - OVS 
!--------------------------------------------------------------
! Mod 25 Ago 2014
! Se modifica el proceso de franqueo para que solo se genere un
! franqueo en los recaudos de Simple 
! desarrollado por Grupo Retail - OVS 
!--------------------------------------------------------------
! Mod 14Abr2015
! Se controla manejo del modulo para procesos en entrenamiento
! no se permite la actualización de procesos a los sistemas 
! centrales de Comfandi.
! Se define el parametro TRXEXC con valor 1 o 0 para manejo
! de recaudo unico por transaccion
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------------------
! Mod 31Jul2015
! Se ajusta mensajeria modulo de recaudo Click, enviando el valor
! del recaudo para validación por parte del web services si se esta
! escanenado en dato correcto para el valor del recaudo dentro
! de las fechas establecidas.
! Se implementa el nuevo jar poseduw2.jar y se anexan llamados en 
! los archivos krn.ini y was.ini
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------------------
! Mod 23Sep2015
! Se modifica mensajería modulo simple y manejo de recaudo para la 
! presentación en tiquete visor y almacenamiento de user data 
! del periodo de cotización, segun requerimientos de Simple.
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------------------
! Mod 5Feb2015
! Se maneja un flag de indicador de recaudos comfandi para 
! que se permitan se pagados con credito social
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------------------
! Mod 10mzo2016
! Se modifica respuesta de validacion de simple
! no tiene encuenta validaciones fallidas para el cierre de la trx
! Solicitado por Comfandi 10-mzo-2016
!--------------------------------------------------------------
! Mod 7 jun 2016
! Se modifica orden de presentacion de pin, planilla en recaudos
! de simple.
! Solicitado por Comfandi 10-mzo-2016
!--------------------------------------------------------------
! Mod 12 Jun 2019
! Se ajusta proceso de captura de recuados para trx unica evitando
! inicialización de variables al tratar de capturar otro tipo de 
! recaudo cuando ya hay uno en proceso
! Solicitado por Comfandi 12-Jun-2019
!--------------------------------------------------------------
! Mod 28 Ago 2019
! Implementación recaudo siga en linea
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------------------
! Mod 11 May 2021
! Ajsute para manejo nuevos recibos de recaudo Celsia y Celsa
! para manejo de recibos viejos y nuevos de 12 y 14 caracteres
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------------------
! Mod 29Nov2021
! Activacion nuevo servicio resrvas hoteleras integrados a 
! Zeus, modulo desarrollado en junio del 2021.
! Las remisiones siguen integradas a Saleforce
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------------------
! Mod 28Mzo2022
! Se desarrolla funcionalidad de recaudo de credito social
! con validación en línea a la plataforma de Linix
! desarrollado por Grupo Retail - OVS / CAC
!--------------------------------------------------------------
! Mod 01FEb2023
! Se activa nuevamente servicio de reservas hoteleras
! para funcionamiento con Zeus.
! Solicitado por Comfandi
! desarrollado por Grupo Retail - OVS / CAC
!--------------------------------------------------------------
! Mod 05Feb2024
! Se ajusta nombre recaudo simple descriptor Item para 
! presentación con ajuste ICUI
!--------------------------------------------------------------
! Mod 21Mzo2025
! Se controla el proceso de recuperacion de recaudos por caida 
! o suspension de trx, obligando en el proceso de recuperacion
! a anular la transaccion para volver a capturar datos y evitar 
! problemas con los recaudos.
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------------------

%ENVIRON T																																	! Ambiente de terminal

Integer*1 Global Ue.RecSrv.Reserva%, UE.RECASRV.UNICA%, Ue.RecSrv.InTrx%,  \!
                 Ue.RecSim.InTrx%, Ue.RecSrv.Emcali%, Ue.RecSrv.Device%,   \!
                 Ue.RecSrv.PagReserva%, USER.VERIFY.ITEM, Ue.RecSrv.OnTrx%,\!
                 Gr.Mrm.Cap%, Gr.RecSrv.Siga%, Gr.RecSrv.CrdSoc%            !
Integer*4 Global GR.TEMP1I4, Asc.Pay.Motora%                                !
Integer*4 GLOBAL TE.TR.AMT(1)                      													! ENTERED TENDER 1-CASH         INT 4
Integer*4 Global Gr.RecSrv.Txn%
Integer*1 Global Ue.Cnv.Fiscal%                                             ! Control impresion numeracion fiscal           
Integer*1 Global Gr.RecSrv.Msg%                                             ! Numero de mensaje a ser impreso Add 24Abr2013 
Integer*1 Global Gr.RecSrv.Fiscal%                                          ! Si recaudo genera numeracion fiscal           
Integer*1 Global Gr.RecSrv.SndSmp%                                          ! Numero intentos actual. simpe                 
Integer*1 Global Asc.Pay.Impr%                                              ! Control nro trx en mensajeria java            
Integer*1 Global Gr.RecSrv.RecComfa%, Gr.Fis.Doc%                           ! Indicador recaudo comfandi                    
String    GLOBAL Gr.RecSrv.Rqst$, Gr.RecSrv.IdRef$, Gr.Rec.Tipo$ 						!
Integer*1 Global Gr.RecSrv.Simple%, Gr.RecSrv.Proc%, Gr.RecSrv.NewCol%			!
Integer*1 Global UE.RECASRV.TipoRecSales%																		!
Integer*2 Global Ue.RecSrv.LenCelsia%
String    GLOBAL USER.SAVE.ITEM$,                                          \!                                               
                 Gr.RecSrv.Msg$,                                           \! Numero mensaje aplicar                        
                 Ue.RecSrv.Msg$(2),                                        \! Mensajes recaudos                             
                 Ue.RecSrv.AplMsg$(1),                                     \! Control de aplicacion de mensajes             
                 Ue.RecSrv.AplMsgRsv$,                                     \! Aplica Mensaje reservas                       
                 Ue.RecSrv.NumMsgRsv$,                                     \! Numero Mensaje reservas                       
                 Ue.RecSrv.AplMsgAcv$,                                     \! Aplica Mensaje reservas                       
                 Ue.RecSrv.NumMsgAcv$,                                     \! Numero Mensaje reservas                       
                 Ue.RecSrv.FiscalAcv$,                                     \! Si aplica numeracion fiscal Acuaviva          
                 Ue.RecSrv.FiscalRsv$,                                     \! Si aplica numeracion fiscal Reservas Hoteleras
                 Ue.RecSrv.Periodo$,                                       \! Periodo Cotizacion Simple                     
                 Ue.RecSrv.Planilla$,                                      \! Numero planilla Simple
                 Ue.RecSrv.CelsiaPlu$,                                     \! Codigo recaudo celsia
                 Ue.Recasrv.Fomento$																				!

%INCLUDE RECTSUVA.011        							                                  !Variables Desarrollo    
%INCLUDE EAMTOPTS.J86        							                                  !Working Storage         
%INCLUDE EAMTSWKG.J86        							                                  !Working Storage         
%INCLUDE EAMITEMR.J86        							                                  !Item record file        
%INCLUDE GRITEMR.J86        				                                        !Variables Itemr         
%INCLUDE EAMTRANS.J86        							                                  !Transaction summary log 
%INCLUDE EAMTSEVA.J86        							                                  !4610 Printer            
%INCLUDE EAMTSXHC.J86                                                                                
%INCLUDE RECATSSU.011        							                                  !Rutinas Genericas       

Sub TSHIECET EXTERNAL
End Sub 

Sub TSPREC01 EXTERNAL
End Sub 

!SUB TSU08.CPN EXTERNAL
!End SUB

Function FORMAT.AMOUNT(AMT1) External
  INTEGER*1 FORMAT.AMOUNT
  INTEGER*4 AMT1
End Function

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                      *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                         *!
  STRING    DATA$      !* String to hold the data read from file.            *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                               *!
End Sub 

Sub SPLIT.ITEMFILE (RECORD$) External
  STRING    RECORD$    !* The record (including the key) read from file      *!
End Sub 

!--- Fin definicion de rutinas externas

Sub RECTER.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%

			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:RT" + DATE$ + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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
			X.Len% = Len(X.BUFF$)						  				  								            ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						            ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					          ! Graba registro
			X.BUFF$ = "["+X.RTA$+"]"+"RTA:"+X.LLEGA$                                !
			X.Len% = Len(X.BUFF$)						  				  								            ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						            ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					          ! Graba registro      
			Close 56
End Sub 


Sub FRANQUEA.DOCUMENTO.PUBLICO(XPROY$)
String UE.RECASRV.MSGD$, Xtipo$, XPlu$, Xvlr$, ZFEC$, XDoc$, Xnit$, XPROY$
String Xenvia$, Xsale$, Xrta$, Xllega$, Xtrama$
Integer*2 UE.RECASRV.I%, XI%, XX%
 Xplu$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)
 XTipo$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	 

 If Val(XPROY$) <> 20090226 Then Begin 
    Xdoc$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)   ! Dato Empaquetado
 EndIf Else Begin 
    Xdoc$  = ASIC.GETUNPK4(H$,Asc.Tmp.Apun%)  ! Dato No Empaquetado
 EndIf 

 !Xdoc$  = ASIC.GETUNPK4(H$,Asc.Tmp.Apun%)  ! Dato No Empaquetado
 
 XVlr$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	 
 ZFEC$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	 
 XNit$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	 
 TS.ER.RETURN = -1
 TS.TEMP2$ = Pack$(Right$("000000000000"+Xplu$,12)) 
 Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)                  					! Lectura Datos Itemr
 Call SPLIT.ITEMFILE( TS.TEMP1$ ) 			                          					! Entrega datos al eamitemr.j86
 If TS.ER.RETURN <> -1 Then GR.ITEMNAME$ = "RECAUDO S.P.      "
 XX% = 2	 																																	! NUMERO DE FRANQUEOS DEFUALT
 If Ue.RecSrv.Emcali% = -1 Then XX% = 1                                     ! Si Emcali no genera franqueo
 If Gr.RecSrv.Simple% = -1 Then XX% = 1                                     ! Un solo franqueo para Simple


 If Gr.RecSrv.Simple% = -1 Then Begin
    If TS.TRAINING Then Begin																											! En entrenamiento
    	 Call VISORES4690(1,"SUBIENDO MOVIMIENT0","SIMPLE *ENTRENA* ",1000,"L") 	  !	
    	 Exit Sub 
    EndIf
    Call VISORES4690(1,"SUBIENDO MOVIMIENT0","RECAUDO SIMPLE...",0,"L") 	  !	
    TS.TEMP1$ =  Right$("000000000000"+UE.RECASRV.PLU$,12)  			    +		 \! EAN recaudo        Asc 12
                 Right$("                  "+UE.RECASRV.DOC$(1),18)		+		 \! Nro referencia     Asc 18
                 Right$("0000000000"+UE.RECASRV.VLR$(1),10)						+		 \! Vlr recaudo        Asc 10
                 Right$("00000000"+UE.RECASRV.FECHA$(1),8)						+		 \! Fecha vencimiento  Asc 8
                 Right$("00000000000000"+UE.RECASRV.NIT$(1),14)				+		 \! Nit empresa rec.   Asc 14
                 Ue.RecSrv.Periodo$																					! Periodo            Asc 6 
    Asc.Pay.Impr% = 2																												! Numero consecutivo trx
    TS.TEMP2$ = Armar.Trama.Msg("15","62",TS.TEMP1$,"00","0001","123456")  	! Armar trama MSG                	
    Xenvia$   = TS.TEMP2$																										!
    Xsale$    = Date$ + " : " + Time$																				! Hora salida requerimiento
    Xtrama$   = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)    ! Ejecuta Requerimiento
    Xrta$     = Date$ + " : " + Time$																				! Hora llegada requerimiento
    Xllega$   = Xtrama$   																							    ! Trama de respuesta
    Call RECTER.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                   ! Rastro auditoria
 EndIf

 	
 For XI% = 1 To XX%   
     Call TSHIECET
     Call VISORES4690(1,"FRANQUEO NRO "+Str$(XI%),"RECIBO:"+Left$(Xdoc$,13),1200,"L")		! Franquea el documento     
	   TS.PRT.PARM = 1100H																										!DOC INSERT
     TS.LINETYPE = 9																												!
     TS.LINEDATA = 12      																		      				! Formato de franqueo   
     If TS.TRAINING Then Begin
     
     EndIf
     If Gr.RecSrv.Simple% <> -1 Then Begin
	     TO.FRANK.TXT$(12,01) = "*  RECAUDO SERVICIOS PUBLICOS *"
	     TO.FRANK.TXT$(12,02) = "CONCEPTO : "+GR.ITEMNAME$
	     TO.FRANK.TXT$(12,03) = "RECIBO   : "+XDOC$
	     Call Format.Amount(Val(Xvlr$))
	     Ts.Temp1$ = Xvlr$
	     TO.FRANK.TXT$(12,04) = "VALOR    : "+Ts.Temp1$
	     TO.FRANK.TXT$(12,05) = "FECHA    : "+ZFEC$
       To.FRANK.TXT$(12,06) = STRING$(37,"-")
       Asic.Detalle% = -1
       XTipo$ = Linea.Detalle(94)                                   ! Store Line Trx      
       To.FRANK.TXT$(12,07) = XTipo$																!
     EndIf Else Begin
	     TO.FRANK.TXT$(12,01) = "*  RECAUDO SERVICIOS PUBLICOS *"
	     TO.FRANK.TXT$(12,02) = "CONCEPTO : "+GR.ITEMNAME$
	     TO.FRANK.TXT$(12,03) = "RECIBO   : "+XDOC$
	     Call Format.Amount(Val(Xvlr$))
	     Ts.Temp1$ = Xvlr$
	     TO.FRANK.TXT$(12,04) = "VALOR    : "+Ts.Temp1$
	     TO.FRANK.TXT$(12,05) = "FECHA    : "+ZFEC$
	     TO.FRANK.TXT$(12,06) = "PERIODO  : "+Ue.RecSrv.Periodo$
       To.FRANK.TXT$(12,07) = STRING$(37,"-")
       Asic.Detalle% = -1
       XTipo$ = Linea.Detalle(94)                                   ! Store Line Trx      
       To.FRANK.TXT$(12,08) = XTipo$																!
     EndIf 
     If TS.TRAINING Then Begin
        To.FRANK.TXT$(12,10) = "*** ENTRENAMIENTO ***"            ! Msg entrenamiento
        To.FRANK.TXT$(12,11) = "** RECAUDO NO VALIDO **"          ! Msg entrenamiento
     EndIf Else Begin
        To.FRANK.TXT$(12,10) = ""            											! Msg entrenamiento
        To.FRANK.TXT$(12,11) = ""          												! Msg entrenamiento
     EndIf
     Call TSPREC01 
     Call U.EXPLUSADI                                             ! Expulda Documento
 Next XI%
 UE.RECASRV.QTY% = 0
  
End Sub

Sub PROCESO.RECIBOS.PUBLICOS Public             			!
 Integer*4 PRIC%, TOT4%, X.I%, XI%,					\!
           UE.RECASRV.POS%, UE.RECASRV.LENCLAVE%
 String X.TMP$, XCLAVE$							!

  For X.I% = 1 TO UE.RECASRV.Slend%                			             ! Recorre todos los String

    H$ = READ.SL.STR$(X.I%)                 				                 ! Toma el String
    If LEN(H$) > 5 Then                    			                    \! Si el String es OK
     If ASC(H$)  = 153 Then Begin            				                 ! Si es un Data Entry
        Asc.Tmp.Apun% = 3						                                 ! Asigna apuntador
        X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  			             ! Verifica el numero de proyecto
        If (X.TMP$ = UE.RECASRV.SIMPLEPRO$) OR                      \!
           (X.TMP$ = UE.RECASRV.ACUAVIPRO$) Then Begin               ! Si recaudo Simple o Acuaviva
           Call FRANQUEA.DOCUMENTO.PUBLICO(X.TMP$)                   ! Impresion del franqueo del documento 
        EndIf Else Begin 																						 ! Demas formatos de recaudo
         For XI% = 1 To UE.RECASRV.CONT%				                     ! Total de recaudos definidos
            UE.RECASRV.POS% = MATCH("CLAVE",UE.RECASRV.MASK$(XI%),1)
            If UE.RECASRV.POS% > 0 Then UE.RECASRV.LENCLAVE% = 8
            If UE.RECASRV.LENCLAVE% > 0 Then \
               XCLAVE$ = Mid$(UE.RECASRV.MASK$(XI%),(UE.RECASRV.POS%+5),UE.RECASRV.LENCLAVE%) Else \
               XCLAVE$ = "11111111"
            If X.TMP$ = XCLAVE$ Then Begin                              ! Proyecto recaudos
               Call FRANQUEA.DOCUMENTO.PUBLICO(X.TMP$)                  ! Impresion del franqueo del documento 
               XI% = UE.RECASRV.CONT% + 10
            EndIf 																	  										!
         Next XI%
        EndIf 
     EndIf                                  															!
  Next X.I%                                 															!
End Sub

Sub Calculo.Request
Integer*4 Xdata%
      Xdata% = (Val(Mid$(Date$,3,2)) * 2678400)            +                    \! Dato Calculo del mes
               (Val(Mid$(Date$,5,2)) * 86400)              +                    \! Dato Calculo del dia
               (Val(Mid$(Time$,1,2)) * 3600)               +                    \! Dato Calculo del hora
               (Val(Mid$(Time$,3,2)) * 60)                 +                    \! Dato Calculo del Minuto
               (Val(Mid$(Time$,5,2)) * 01)                                       ! Dato Calculo del Segundo
      Gr.RecSrv.Rqst$ = "       " + Left$(Date$,2) + Right$("00000000"+Str$(Xdata%),8) +   \! Año AA, dato calculado, 
               Right$("000"+TS.STORE$,3) + Right$("000"+Str$(SL.HD.TERMINAL),3) + \! tienda, Numero de terminal
 	             Right$("00"+Str$(SL.HD.TRANSNUM + 1),2)                             ! Numero de la trx                 
End Sub 

Sub Validacion.Recibo.Publico
String X.Opt$, X.Trama$, Xtmp1$, Xtmp2$
String Host.Message$, X.Lec$, X.Finr$, Rbuffer$, X.ENVIA$, X.LLEGA$, X.SALE$, X.RTA$, Xrqst$
Integer*4 X.Len%, X.LONG%, Xdata%
String    Xenvia$, Xsale$, Xrta$, Xllega$
    !Xtmp1$ = Str$(Val(UE.RECASRV.DOC$(UE.RECASRV.QTY%)))
    
    Xtmp1$ = UE.RECASRV.DOC$(UE.RECASRV.QTY%)																		 ! Numero el recaudo
    Xtmp2$ = UE.RECASRV.VLR$(UE.RECASRV.QTY%)																		 ! Valor del recaudo
    
    Gr.RecSrv.Simple% = 0																												 ! Inicializacion proceso
    Gr.RecSrv.Proc%   = 1    																									   ! Validacion en linea
    Gr.RecSrv.NewCol% = 0																												 ! Recaudo colegios ws
    Gr.RecSrv.Siga% = 0																													 ! Recaudo Siga
    Call TSHIECET                                                                ! Tono de alerta                 		
    Call VISORES4690(1,"PROCESANDO RECIBO:",XTMP1$,0,"L")       						     !	
    If Gr.REC.TipID% = 1 Then \
       TS.TEMP1$ = RIGHT$("                  "+XTMP1$,18  ) Else  \! ajusta dato capturado          	
       TS.TEMP1$ = RIGHT$("                   "+XTMP1$,19  ) 
    If UE.RECASRV.APLICACION$ = "15" And UE.RECASRV.FUNCION$ = "60" Then Begin   ! Planilla Simple
      Call VISORES4690(1,"CONSULTANDO A SIMPLE",XTMP1$,0,"L")
   	  Gr.RecSrv.Simple% = -1                                                     ! Marca proceso para simple online
   	  Ue.RecSim.InTrx%  = -1                                                     ! Recaudo simple en proceso
      Call Calculo.Request
   	  TS.TEMP1$ = Gr.RecSrv.Rqst$ + RIGHT$("                    "+XTMP1$,20)     ! Numero planilla
    EndIf

    If UE.RECASRV.APLICACION$ = "17" And UE.RECASRV.FUNCION$ = "71" Then Begin   ! Recaudo Colegios New
    	 Gr.RecSrv.NewCol% = -1                                                    ! Recaudo Colegios New
    	 TS.TEMP1$ = TS.TEMP1$ + Right$("0000000000"+Xtmp2$,10)
    EndIf

    If UE.RECASRV.APLICACION$ = "18" And UE.RECASRV.FUNCION$ = "91" Then Begin   ! Recaudo SIGA
    	 Gr.RecSrv.Siga% = -1                                                      ! Recaudo SIGA
    	 XTMP1$ = Str$(Val(Xtmp1$))
    	 XTMP2$ = Str$(Val(Xtmp2$))
       TS.TEMP1$ = RIGHT$("                  "+XTMP1$,18 ) 
    	 TS.TEMP1$ = TS.TEMP1$ + Right$("          "+Xtmp2$,10)
    EndIf
!--- Add 28Mzo2022 - Recaudo credito social    
    If UE.RECASRV.APLICACION$ = "21" And UE.RECASRV.FUNCION$ = "01" Then Begin   ! Recaudo Credito Social
      TS.TEMP1$ = TS.TEMP1$ + Right$(String$(25," ")+Xtmp1$,25)
      Gr.RecSrv.CrdSoc% = -1
    EndIf
    
    Asc.Pay.Impr% = 0																														! Asignacion nro trx 
    TS.TEMP2$ = Armar.Trama.Msg(UE.RECASRV.APLICACION$,UE.RECASRV.FUNCION$,TS.TEMP1$,"00","0001","123456") ! Armar trama MSG
    Xenvia$   = TS.TEMP2$																												!
    Xsale$    = Date$ + " : " + Time$																						! Hora salida requerimiento
    Gr.Rec.RtaMsg$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$) 	! Ejecuta Requerimiento          	
    
    !Gr.Rec.RtaMsg$ = "1891064200000RECIBO ES APTO PARA      PAGO                             "
    
    Xrta$     = Date$ + " : " + Time$																						! Hora llegada requerimiento
    Xllega$   = Gr.Rec.RtaMsg$																									!
    Call RECTER.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                     	! Rastro auditoria
    TS.TEMP1$ = Valida.Rta(Gr.Rec.RtaMsg$)																		  ! Valida rta entregada 
     If TS.TEMP1$ <> "00" Then Begin	  							   										    ! Si presenta un error, verif. local
        GR.TEMP1I4 = 0 																												  ! Reporta Error en la consulta
        Gr.RecSrv.Simple% = 00
        Ue.RecSim.InTrx% = 0 
        Gr.RecSrv.NewCol% = 00                                                     ! Recaudo Colegios New
        Gr.RecSrv.Siga% = 0																												 ! Recaudo Siga
        Gr.RecSrv.CrdSoc% = 0
        Exit Sub
     EndIf 
     
     TS.TEMP1$ = MID$(Gr.Rec.RtaMsg$,12,2)                                         ! Toma status de la Operacion
     If TS.TEMP1$ <> "00" Then Begin								   														 ! Si presenta un error, verif. local
        TS.TEMP1$ = MID$(Gr.Rec.RtaMsg$,14,40)                                     ! Toma mensaje del Error 
        Call VISOR.AND.BORRAR(Left$(TS.TEMP1$,20)+Right$(TS.TEMP1$,20))            !
        GR.TEMP1I4 = 0 																														 ! Reporta Error en la consulta			
        Gr.RecSrv.Simple% = 00                                                     ! Marca proceso para simple online
        Ue.RecSim.InTrx% = 0 
        Gr.RecSrv.NewCol% = 00                                                     ! Recaudo Colegios New
        Gr.RecSrv.Siga% = 0																												 ! Recaudo Siga
        Gr.RecSrv.CrdSoc% = 0
        Exit Sub     																														   ! 
      EndIf Else Begin					                                                   ! TRX AUTORIZADA
        GR.TEMP1I4 = -1
      EndIf 
End Sub 
!--- fin validacion compra

Function ASIGNA.DATOS.RESERVA
Integer*4 ASIGNA.DATOS.RESERVA
STRING    UE.RECASRV.DATO$, Xdoc$, ZFEC$, Xvlr$, Xdata$
Integer*4 XI%, XJ%, XK%
Integer*1 Xfound%
     If UE.RECASRV.UNICA% = -1 And UE.RECASRV.QTY% > 0 Then Begin 
        Call VISOR.AND.BORRAR("SOLO UN RECIBO POR  TRANSACCION")
        ASIGNA.DATOS.RESERVA = 0
        UE.RECASRV.FLAG% = 0
        UE.RECASRV.ITM% = 0          
        Exit Function 
     EndIf 
     
     If UE.RECASRV.UNICA% = -1 And TS.INTRX Then Begin 
        Call VISOR.AND.BORRAR("NO SE PERMITE MEZCLADE RECAUDO Y ARTIC5")
        ASIGNA.DATOS.RESERVA = 0
        UE.RECASRV.FLAG% = 0
        UE.RECASRV.ITM% = 0          
        Exit Function 
     EndIf 
     
     Gr.Rec.NitRsv$ = "0"
     Gr.Rec.NameRsv$ = ""
     Xdoc$ = UE.RECASRV.CODE$																				! Toma numero de referencia
     For XK% = 1 To UE.RECASRV.QTY%																	! BUSCA SI RECIBO YA CAPTURADO
         If Xdoc$ = UE.RECASRV.DOC$(XK%) Then \
            Xfound% = -1 
     Next XK%
     If Xfound% = -1 Then Begin 																	  ! Recibo ya ingresado
        Call VISOR.AND.BORRAR("RESERVA YA ESTA CAPTURADA")
        ASIGNA.DATOS.RESERVA = 0
        UE.RECASRV.FLAG% = 0
        UE.RECASRV.ITM% = 0          
        Exit Function 
     EndIf 
     
     UE.RECASRV.QTY% = UE.RECASRV.QTY% + 1																		! Aumenta contador
     UE.RECASRV.DOC$(UE.RECASRV.QTY%)   = Xdoc$																! Numero del documento
     GR.TEMP1I4 = -1
     UE.RECASRV.APLICACION$ = "14"																						! Recaudo Reservas Hoteleras
     UE.RECASRV.FUNCION$ = "01"
!-- Nuevo modulo de reservas, retirar comentarios       28 jul 2021
     If UE.RECASRV.TipoRecSales% = 1 Then UE.RECASRV.FUNCION$ = "11"          ! Recaudo reservas Zeus Mod 25/Jun/2021
     If UE.RECASRV.TipoRecSales% = 2 Then UE.RECASRV.FUNCION$ = "01"          ! Validación fomento Saleforce Mod 25/Jun/2021
     Gr.REC.TipID% = 2
     Call Validacion.Recibo.Publico																						! Valida el numero del recibo
     If GR.TEMP1I4 <> -1 Then Begin 																					! Si proceso Fallido
        ASIGNA.DATOS.RESERVA = 0
        GR.TEMP1I4 = 0 
        UE.RECASRV.FLAG% = 0
        UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1
        UE.RECASRV.ITM% = 0           
        Gr.REC.TipID% = 0
        Exit Function 
     EndIf 

     UE.RECASRV.VLR$(UE.RECASRV.QTY%)   = Str$(Val(Mid$(Gr.Rec.RtaMsg$,54,15)))   ! Toma Valor de la respuesta
     UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = DATE$                                   ! Toma la fecha de Vencimiento
     UE.RECASRV.RTA$ = Mid$(Gr.Rec.RtaMsg$,70,29)                                 ! Trama de respuesta
     
     Gr.Rec.NitRsv$ = Mid$(Gr.Rec.RtaMsg$,70,19)				  ! Nit del cliente
     Gr.Rec.NameRsv$ = Mid$(Gr.Rec.RtaMsg$,89,40)				  ! Nombre del cliente
!--- Por Error definicion, se define solo para reservas 23 Sept 2016     
     Gr.Rec.Tipo$    = Mid$(Gr.Rec.RtaMsg$,129,1)				  ! Tipo de recaudo

     Gr.Rec.Tipo$ = "R"            ! Se comenta 11 ene 2017 
     
!-- Fin modificacion 
     
     Gr.Rec.CtroCosto$ = "00000"
     
!     If UE.RECASRV.TipoRecSales% = 1 And Ucase$(Gr.Rec.Tipo$) <> "R" Then Begin		! Si recaudo no es reserva hotelera
!     	  Call VISOR.AND.BORRAR("RECAUDO NO ES UNA   RESERVA HOTELERA")
!        ASIGNA.DATOS.RESERVA = 0
!        UE.RECASRV.FLAG% = 0
!        UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1
!        UE.RECASRV.ITM% = 0           
!        Gr.REC.TipID% = 0
!        Exit Function 
!     EndIf

!     If UE.RECASRV.TipoRecSales% = 2 And Ucase$(Gr.Rec.Tipo$) <> "F" Then Begin		! Si recaudo no es fomento
!     	  Call VISOR.AND.BORRAR("RECAUDO NO ES DE    FOMENTO         ")
!        ASIGNA.DATOS.RESERVA = 0
!        UE.RECASRV.FLAG% = 0
!        UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1
!        UE.RECASRV.ITM% = 0           
!        Gr.REC.TipID% = 0
!        Exit Function 
!     EndIf

     UE.RECASRV.CCOSTO$(UE.RECASRV.QTY%) = Gr.Rec.CtroCosto$
     TS.TEMP3$ = ASIC.DATOS$(LEFT$(Gr.Rec.NameRsv$,20),"PLU/CONFIR BORR/ANUL")
     If (TS.TEMP3$ = "E") Then Begin		! No se capturo dato
        ASIGNA.DATOS.RESERVA = 0
        UE.RECASRV.FLAG% = 0
        UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1
        UE.RECASRV.ITM% = 0           
        Gr.REC.TipID% = 0
        Exit Function 
     EndIf					     

!-- Add 24Abr2013
         If Ue.RecSrv.AplMsgRsv$ = "1" Then Begin                      ! Si genera mensaje
         	  Ue.RecSrv.AplMsg$(Val(Ue.RecSrv.NumMsgRsv$)) = "1"         ! Indica a la aplicacion que genere el mensaje
         EndIf
         Gr.RecSrv.Fiscal% = Val(Ue.RecSrv.FiscalRsv$)                 ! Si aplica numeracion fiscal            
!--- End Add 24Abr2013
    
     UE.RECASRV.NIT$(UE.RECASRV.QTY%)   = String$(10,"0")                   ! Nit del recaudador
     UE.RECASRV.CLAVE$(UE.RECASRV.QTY%) = UE.RECASRV.RESERVPRO$             ! Codigo del proyecto
     UE.RECASRV.TIPO$(UE.RECASRV.QTY%)  = "1" 
     ZFEC$ = Right$(UE.RECASRV.FECHA$(UE.RECASRV.QTY%),6)

     UE.RECASRV.DC$ = UE.RECASRV.DOC$(UE.RECASRV.QTY%)
     UE.RECASRV.VL$ = UE.RECASRV.VLR$(UE.RECASRV.QTY%)
     UE.RECASRV.FC$ = UE.RECASRV.FECHA$(UE.RECASRV.QTY%)
     If Len(UE.RECASRV.FC$) = 6 Then UE.RECASRV.FC$ = "20" + UE.RECASRV.FC$
     UE.RECASRV.NT$ = UE.RECASRV.NIT$(UE.RECASRV.QTY%)
     UE.RECASRV.TP$ = UE.RECASRV.TIPO$(UE.RECASRV.QTY%)
     UE.RECASRV.CLV$= UE.RECASRV.CLAVE$(UE.RECASRV.QTY%)
     UE.RECASRV.RTA$ = String$(12,"0")
                      
     ASIGNA.DATOS.RESERVA = -1
     UE.RECASRV.FLAG% = -1
     UE.RECASRV.ITM% = -3
       
End Function 

Function ASIGNA.DATOS.SIMPLE
Integer*4 ASIGNA.DATOS.SIMPLE
STRING    UE.RECASRV.DATO$, Xdoc$, ZFEC$, Xvlr$, Xdata$
Integer*4 XI%, XJ%, XK%
Integer*1 Xfound%

!--- Lineas Adicionadas 19Ene2010
     If UE.RECASRV.UNICA% = -1 And UE.RECASRV.QTY% > 0 Then Begin 
        Call VISOR.AND.BORRAR("SOLO UN RECIBO POR  TRANSACCION")
        ASIGNA.DATOS.SIMPLE = 0
        UE.RECASRV.FLAG% = 0
        UE.RECASRV.ITM% = 0          
        Exit Function 
     EndIf 
     
     If UE.RECASRV.UNICA% = -1 And TS.INTRX Then Begin 
        Call VISOR.AND.BORRAR("NO SE PERMITE MEZCLADE RECAUDO Y ARTIC6")
        ASIGNA.DATOS.SIMPLE = 0
        UE.RECASRV.FLAG% = 0
        UE.RECASRV.ITM% = 0          
        Exit Function 
     EndIf 

     Xdoc$ = Mid$(UE.RECASRV.CODE$,3,10) 														! Toma numero de referencia
     For XK% = 1 To UE.RECASRV.QTY%																	! BUSCA SI RECIBO YA CAPTURADO
           If Xdoc$ = UE.RECASRV.DOC$(XK%) Then \
              Xfound% = -1 
     Next XK%
     If Xfound% = -1 Then Begin 																	  ! Recibo ya ingresado
        Call VISOR.AND.BORRAR("RECIBO YA ESTA CAPTURADO")
        ASIGNA.DATOS.SIMPLE = 0
        UE.RECASRV.FLAG% = 0
        UE.RECASRV.ITM% = 0          
        Exit Function 
     EndIf 

     UE.RECASRV.QTY% = UE.RECASRV.QTY% + 1												        	! Aumenta contador
     UE.RECASRV.DOC$(UE.RECASRV.QTY%)   = Xdoc$															! Numero del documento
     UE.RECASRV.VLR$(UE.RECASRV.QTY%)   = Str$(Val(Mid$(UE.RECASRV.CODE$,13,7)))  ! Toma Valor del recibo
     UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = Right$(UE.RECASRV.CODE$,6)              ! Fecha Vencimimiento
     UE.RECASRV.NIT$(UE.RECASRV.QTY%)   = String$(10,"0")                         ! Nit del recaudador
     UE.RECASRV.CLAVE$(UE.RECASRV.QTY%) = UE.RECASRV.SIMPLEPRO$                   ! Codigo del proyecto
     UE.RECASRV.TIPO$(UE.RECASRV.QTY%)  = "1" 
     ZFEC$ = Right$(UE.RECASRV.CODE$,6)     					  ! Fecha Vencimimiento
     
     If Val(Date$) > Val(ZFEC$) Then Begin 			                  ! Si recibo no es del dia de recaudo
        Call VISOR.AND.BORRAR("RECIBO FUERA DE LA  FECHA DE PAG1 - BORRAR")                                           
        ASIGNA.DATOS.SIMPLE = 0                                                                                       
        UE.RECASRV.FLAG% = 0                                                                                          
        UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1                                                                         
        UE.RECASRV.ITM% = 0                                                                                           
        UE.RECASRV.CODE$ = ""
        TS.IO.STATE    = 10            ! PREPARE FOR NORMAL SALES STATE
        Exit Function                                                                                                 
     EndIf                                                                                                           

       UE.RECASRV.DC$ = UE.RECASRV.DOC$(UE.RECASRV.QTY%)
       UE.RECASRV.VL$ = UE.RECASRV.VLR$(UE.RECASRV.QTY%)
       UE.RECASRV.FC$ = UE.RECASRV.FECHA$(UE.RECASRV.QTY%)
       If Len(UE.RECASRV.FC$) = 6 Then UE.RECASRV.FC$ = "20" + UE.RECASRV.FC$
       UE.RECASRV.NT$ = UE.RECASRV.NIT$(UE.RECASRV.QTY%)
       UE.RECASRV.TP$ = UE.RECASRV.TIPO$(UE.RECASRV.QTY%)
       UE.RECASRV.CLV$= UE.RECASRV.CLAVE$(UE.RECASRV.QTY%)
       UE.RECASRV.RTA$ = String$(12,"0")
       Gr.Rec.CtroCosto$ = "00000"
       UE.RECASRV.CCOSTO$(UE.RECASRV.QTY%) = Gr.Rec.CtroCosto$
       ASIGNA.DATOS.SIMPLE = -1
       UE.RECASRV.FLAG% = -1
       UE.RECASRV.ITM% = -3
       Ue.RecSim.InTrx% = -1             ! Add 19Ene2010
       
End Function 

Function Consulta.CedulaCrdSoc																							! Consulta por CC credito social
String Consulta.CedulaCrdSoc, XCED$																					! 
String Xenvia$, Xsale$, Xrta$, Xllega$ 
    XCED$ = ASIC.DATOS$("CEDULA A VERIFICAR","")	  												! Ingresa numero de referencia
    If (XCED$ = "") OR (XCED$ = "E") Then Begin															! No se capturo dato
    	  Consulta.CedulaCrdSoc = "0"																					!
        Exit Function 																					    				! Sale de la rutina
    EndIf					     																											!
    Call TSHIECET
    Call VISORES4690(1,"CONSULTANDO CEDULA  ","ESPERE POR FAVOR... ",1500,"L")
    Asc.Pay.Impr% = 0																												! Asignacion nro trx 
    XCED$ = Right$("            "+XCED$,12)
    TS.TEMP2$ = Armar.Trama.Msg("21","02",XCED$,"00","0001","123456") 			! Armar trama MSG
    Xenvia$   = TS.TEMP2$																										!
    Xsale$    = Date$ + " : " + Time$																				! Hora salida requerimiento
    !Gr.Rec.RtaMsg$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$) 	! Ejecuta Requerimiento          	
    Gr.Rec.RtaMsg$ = "2502064200000PROCESO EXITOSO                         COP000000050000       124221122100127735"    
    Xrta$     = Date$ + " : " + Time$																				! Hora llegada requerimiento
    Xllega$   = Gr.Rec.RtaMsg$																							!
    Call RECTER.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                  	! Rastro auditoria
    TS.TEMP1$ = Valida.Rta(Gr.Rec.RtaMsg$)																  ! Valida rta entregada 
     If TS.TEMP1$ <> "00" Then Begin	  							   									  ! Si presenta un error, verif. local
        Consulta.CedulaCrdSoc = "0"																					!
        Exit Function 																													!
     EndIf 
     
     TS.TEMP1$ = MID$(Gr.Rec.RtaMsg$,12,2)                                  ! Toma status de la Operacion
     If TS.TEMP1$ <> "00" Then Begin								   										  ! Si presenta un error, verif. local
        TS.TEMP1$ = MID$(Gr.Rec.RtaMsg$,14,40)                              ! Toma mensaje del Error 
        Call VISOR.AND.BORRAR(Left$(TS.TEMP1$,20)+Right$(TS.TEMP1$,20))     !
        Consulta.CedulaCrdSoc = "0"																					!        
        Exit Function 																											! 
     EndIf Else Begin					                                              ! TRX AUTORIZADA
        Consulta.CedulaCrdSoc = MID$(Gr.Rec.RtaMsg$,69,25)                  ! Retorna numero referencia
     EndIf 

End Function 																																! Fin consulta CC crd social

Function ASIGNA.DATOS.MANUAL
STRING UE.RECASRV.DATO$, Xdoc$, ZFEC$, Xvlr$, Xdata$
INTEGER*2 UE.RECASRV.POS%,UE.RECASRV.LENDOC%,UE.RECASRV.LENFECHA%,UE.RECASRV.LENVLR%,UE.RECASRV.LENCLAVE%
Integer*4 XXI%, XJ%, ASIGNA.DATOS.MANUAL, XK%, XPOS%
Integer*1 Xfound%
For XXI% = 1 TO UE.RECASRV.CONT%
    If VAL(UE.RECASRV.PLU$) = VAL(MID$(UE.RECASRV.MASK$(XXI%),1,12)) Then Begin

      XPOS% = MATCH("TRXEXC",UE.RECASRV.MASK$(XXI%),1)								! Si trx exclusiva
      If MID$(UE.RECASRV.MASK$(XXI%),XPOS%+6,1) = "1" Then Begin      ! Si trx exclusiva
         If TS.INTRX Then Begin																				! Hay una trx en curso
          Call VISOR.AND.BORRAR("SOLO UN RECAUDO POR TRANSACCION  Borrar ")
          ASIGNA.DATOS.MANUAL = 0
          UE.RECASRV.FLAG% = 0
          UE.RECASRV.ITM% = 0          
          Exit Function 
         EndIf
      EndIf
      
!-- Se valida si se permite la captura manual del recaudo
        XPOS% = MATCH("CAPDIG",UE.RECASRV.MASK$(XXI%),1)
        If MID$(UE.RECASRV.MASK$(XXI%),XPOS%+6,1) = "0" Then Begin 
          Call VISOR.AND.BORRAR("NO SE PERMITE RECAUDO DIGITADO")
          ASIGNA.DATOS.MANUAL = 0
          UE.RECASRV.FLAG% = 0
          UE.RECASRV.ITM% = 0          
          Exit Function 
        EndIf					     

       Xfound% = 0 
!-- Add 28Mzo2022 Recaudo Credito Social 
     If VAL(UE.RECASRV.PLU$) <> 770101599993 Then Begin 										! Si recaudo diferente a credito social
       Xdoc$ = ASIC.DATOS$("NUMERO REFERENCIA?","")	  											! Ingresa numero de referencia
       If (Xdoc$ = "") OR (Xdoc$ = "E") Then Begin													! No se capturo dato
          ASIGNA.DATOS.MANUAL = 0
          UE.RECASRV.FLAG% = 0
          UE.RECASRV.ITM% = 0          
          Exit Function 																					    			! Sale de la rutina
       EndIf					     
       UE.RECASRV.POS% = MATCH("DOCUMENTO",UE.RECASRV.MASK$(XXI%),1)				  ! Valida si captura documento
       If UE.RECASRV.POS% > 0 Then                                           \! Captura de documento
      	  UE.RECASRV.LENDOC% = VAL(MID$(UE.RECASRV.MASK$(XXI%),UE.RECASRV.POS%+9,2)) ! Toma la longitud 
       If (Val(Xdoc$) <= 0) Or (Len(Xdoc$) <> UE.RECASRV.LENDOC%) Then Begin  ! Longitud Erronea 
          Call VISOR.AND.BORRAR("NUMERO DE REFERENCIA INVALIDO")						!
          ASIGNA.DATOS.MANUAL = 0																						!
          UE.RECASRV.FLAG% = 0																							!
          UE.RECASRV.ITM% = 0          																			!
          Exit Function 																										!
       EndIf																																	!
     EndIf Else Begin 																											! Recaudo Crd Social x Cedula
     	Xdoc$ = Consulta.CedulaCrdSoc																					! Consulta cedula credito Social
     	If (Val(Xdoc$)) > 0 Then Begin																				! Si retorna numero documento
     		Call Format.Amount(Val(Mid$(Gr.Rec.RtaMsg$,57,12))/100)	  					! Valor del recaudo
     		Xvlr$ = Str$(Val(Mid$(Gr.Rec.RtaMsg$,57,12))/100)
     		Gr.RecSrv.CrdSoc% = -1
     	EndIf Else Begin																											! Error en consulta
          ASIGNA.DATOS.MANUAL = 0
          UE.RECASRV.FLAG% = 0
          UE.RECASRV.ITM% = 0 
          Gr.RecSrv.CrdSoc% = 0         
          Exit Function 
     	EndIf
     EndIf																																	! Fin recaudo Crd Social
     
       For XK% = 1 To UE.RECASRV.QTY%																				! BUSCA SI RECIBO YA CAPTURADO
           If Xdoc$ = UE.RECASRV.DOC$(XK%) Then \
              Xfound% = -1 
       Next XK%
       If Xfound% = -1 Then Begin 																	  			! Recibo ya ingresado
          Call VISOR.AND.BORRAR("RECIBO YA ESTA CAPTURADO")
          ASIGNA.DATOS.MANUAL = 0
          UE.RECASRV.FLAG% = 0
          UE.RECASRV.ITM% = 0          
          Exit Function 
       EndIf 
       
       UE.RECASRV.QTY% = UE.RECASRV.QTY% + 1																! Aumenta contador
       UE.RECASRV.DOC$(UE.RECASRV.QTY%) = Xdoc$    													! Asigna documento
       Gr.Rec.CtroCosto$ = "00000"
       UE.RECASRV.CCOSTO$(UE.RECASRV.QTY%) = Gr.Rec.CtroCosto$
       If Right$(UE.RECASRV.MASK$(XXI%),1) = "1" Then Begin    							! Si proceso en linea
          GR.TEMP1I4 = -1
          
!--- Lineas adicionadas para parametros de aplicacion funcion 08Abrl2009         
         UE.RECASRV.POS% = MATCH("CODAPL",UE.RECASRV.MASK$(XXI%),1)
         If UE.RECASRV.POS% > 0 Then Begin                              ! PARAMETRO DE CAPTURA DE VALOR
           UE.RECASRV.APLICACION$ = MID$(UE.RECASRV.MASK$(XXI%),UE.RECASRV.POS%+6,2)
         EndIf
         UE.RECASRV.POS% = MATCH("CODFUN",UE.RECASRV.MASK$(XXI%),1)
         If UE.RECASRV.POS% > 0 Then Begin                              ! PARAMETRO DE CAPTURA DE VALOR
           UE.RECASRV.FUNCION$ = MID$(UE.RECASRV.MASK$(XXI%),UE.RECASRV.POS%+6,2)
         EndIf
         If VAL(UE.RECASRV.PLU$) = 770101599993 Then GoTo CONFIRMA.RECIBO
          Gr.REC.TipID% = 1
          Call Validacion.Recibo.Publico					        							! Valida el numero del recibo
          If GR.TEMP1I4 <> -1 Then Begin 																! Si proceso Fallido
             ASIGNA.DATOS.MANUAL = 0
             UE.RECASRV.FLAG% = 0
             UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1
             UE.RECASRV.ITM% = 0             
             Gr.REC.TipID% = 0
             Gr.RecSrv.Simple% = 0
             Ue.RecSrv.InTrx% = 0
             Ue.RecSim.InTrx% = 0
             Gr.RecSrv.CrdSoc% = 0
             Exit Function 
          EndIf 
          If Gr.RecSrv.Simple% = 0 Then Begin
          	 Xvlr$ = Mid$(Gr.Rec.RtaMsg$,54,16)
             Call Format.Amount(Val(Mid$(Gr.Rec.RtaMsg$,54,16)))
          EndIf Else Begin
          	 Xvlr$ = Mid$(Gr.Rec.RtaMsg$,56,09)
          	 Call Format.Amount(Val(Mid$(Gr.Rec.RtaMsg$,56,09)))
          EndIf
          
          CONFIRMA.RECIBO:
          Xdata$ = Asic.Datos$("Vlr: $"+TS.TEMP1$,"CONFIRMA? 1.SI 9.NO")
          If MATCH(XDATA$,"19",1) <= 0 Then Begin				           				        ! Dato no valido
              GoTo CONFIRMA.RECIBO
          EndIf					     
          If Val(Xdata$) = 9 Then Begin 																					! No confirma operacion 
             Xvlr$ = Asic.Datos$("Valor del Recibo?","")
             If Val(Xvlr$) <= 0 Then Begin				     				      							! No se capturo dato
                ASIGNA.DATOS.MANUAL = 0
                UE.RECASRV.FLAG% = 0
                UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1
                UE.RECASRV.ITM% = 0              
                Gr.REC.TipID% = 0
                Gr.RecSrv.Simple% = 0
                Ue.RecSrv.InTrx% = 0
                Ue.RecSim.InTrx% = 0
                Gr.RecSrv.CrdSoc% = 0
                Exit Function 																					          ! Sale de la rutina
             EndIf					     
          EndIf 	

          If Gr.RecSrv.Simple% = 0 Then Begin
             UE.RECASRV.VLR$(UE.RECASRV.QTY%)   = Str$(Val(Xvlr$))   											! Toma Valor del recaudo
             UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = Mid$(Gr.Rec.RtaMsg$,70,8)               ! Toma la fecha de Vencimiento
             UE.RECASRV.RTA$ = Mid$(Gr.Rec.RtaMsg$,70,29)                                 ! Trama de respuesta
             Gr.Rec.CtroCosto$ = Mid$(Gr.Rec.RtaMsg$,086,5)                               ! Numero centro de costo para educacion
          	 If VAL(UE.RECASRV.PLU$) = 770101599993 Then Begin														! Recaudo Crd Soc
          	   UE.RECASRV.VLR$(UE.RECASRV.QTY%)   = Str$(Val(Xvlr$))   								    ! Toma Valor del recaudo
               UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = "20" + DATE$													! Fecha vencimiento recaudo
               UE.RECASRV.RTA$ = Mid$(Gr.Rec.RtaMsg$,54,40)                               ! Trama de respuesta
               Gr.Rec.CtroCosto$ = "00000"																								! Centro de costo
          	 EndIf
             ZFEC$ = Right$(UE.RECASRV.FECHA$(UE.RECASRV.QTY%),6)
             Gr.Rec.CtroCosto$ = Str$(Val(Gr.Rec.CtroCosto$))
             Gr.Rec.CtroCosto$ = Right$("00000"+Gr.Rec.CtroCosto$,5)
          EndIf Else Begin																																! Recaudo Simple Online
             UE.RECASRV.VLR$(UE.RECASRV.QTY%)   = Str$(Val(Mid$(Gr.Rec.RtaMsg$,56,09)))   ! Toma Valor de la respuesta
             TS.TEMP5$ = Mid$(Gr.Rec.RtaMsg$,109,10)                                      ! Toma la fecha de Vencimiento
             TS.TEMP5$ = Mid$(TS.TEMP5$,3,2) + Mid$(TS.TEMP5$,6,2) + Right$(TS.TEMP5$,2)  ! Arma fecha reportada por WS AAMMDD
             UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = TS.TEMP5$                               ! Toma la fecha de Vencimiento
             Gr.RecSrv.IdRef$ = Mid$(Gr.Rec.RtaMsg$,65,20)                                ! Reference ID 
             Ue.RecSrv.Periodo$ = Mid$(Gr.Rec.RtaMsg$,85,06)															! Periodo Cotizacion
             UE.RECASRV.RTA$ = String$(12,"0")
             ZFEC$ = Right$(UE.RECASRV.FECHA$(UE.RECASRV.QTY%),6)                         ! 
             ZFEC$ = DATE$																																! Toma fecha del sistema 14 Ago 2014
             Gr.Rec.CtroCosto$ = "00000"
          EndIf
          
          UE.RECASRV.CCOSTO$(UE.RECASRV.QTY%) = Gr.Rec.CtroCosto$
          
        EndIf Else Begin 																							            ! Captura datos en linea
           Xvlr$ = Asic.Datos$("Valor del Recibo?","")
           If (Xvlr$ = "") OR (Xvlr$ = "E") Then Begin				     				        ! No se capturo dato
              ASIGNA.DATOS.MANUAL = 0
              UE.RECASRV.FLAG% = 0
              UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1
              UE.RECASRV.ITM% = 0              
              Exit Function 																					            ! Sale de la rutina
          EndIf					     

!--- Parametro para validar si se captura o no fecha de vencimiento validada
        XPOS% = MATCH("FECVEN",UE.RECASRV.MASK$(XXI%),1)				  ! Si se valida la fecha de vencimiento
              
        If MID$(UE.RECASRV.MASK$(XXI%),XPOS%+6,1) = "1" Then Begin                    
           ZFEC$ = Asic.Datos$("Fecha Recibo AAMMDD?","")
           If (ZFEC$ = "") OR (ZFEC$ = "E") Then Begin				     				        ! No se capturo dato
              ASIGNA.DATOS.MANUAL = 0
              UE.RECASRV.FLAG% = 0
              UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1
              UE.RECASRV.ITM% = 0              
              Gr.REC.TipID% = 0
              Exit Function 																					            ! Sale de la rutina
          EndIf					     
        EndIf Else ZFEC$ = DATE$
        UE.RECASRV.VLR$(UE.RECASRV.QTY%)  = Xvlr$
        UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = ZFEC$
   EndIf 
         
         If Val(Date$) > Val(ZFEC$) Then Begin 																	! Si recibo fuera de la fecha de pago
           Call VISOR.AND.BORRAR("RECIBO FUERA DE LA  FECHA DE PAG2 - BORRAR")
           ASIGNA.DATOS.MANUAL = 0
           UE.RECASRV.FLAG% = 0
           UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1
           UE.RECASRV.ITM% = 0
           TS.IO.STATE    = 10            ! PREPARE FOR NORMAL SALES STATE
           UE.RECASRV.CODE$ = ""
           Gr.REC.TipID% = 0
           Exit Function 
         EndIf 
        
       !BUSCA NIT   
       UE.RECASRV.POS% = MATCH("NIT",UE.RECASRV.MASK$(XXI%),1)
       UE.RECASRV.NIT$(UE.RECASRV.QTY%) = MID$(UE.RECASRV.MASK$(XXI%),(UE.RECASRV.POS%+3),14)

      !BUSCA CLAVE
       UE.RECASRV.POS% = MATCH("CLAVE",UE.RECASRV.MASK$(XXI%),1)
       If UE.RECASRV.POS% > 0 Then UE.RECASRV.LENCLAVE% = 8
       If UE.RECASRV.LENCLAVE% > 0 Then \
          UE.RECASRV.CLAVE$(UE.RECASRV.QTY%) = MID$(UE.RECASRV.MASK$(XXI%),(UE.RECASRV.POS%+5),UE.RECASRV.LENCLAVE%) Else \
          UE.RECASRV.CLAVE$(UE.RECASRV.QTY%) = "11111111"          

       If (SL.IT.INDICAT2 And 80H) OR (SL.IT.INDICAT2 And 40H) Then \
          UE.RECASRV.TIPO$(UE.RECASRV.QTY%) = "1"  Else UE.RECASRV.TIPO$(UE.RECASRV.QTY%) = "0"
          	
!-- Add 24Abr2013
         Gr.RecSrv.Msg% = 0        ! Numero de mensaje aplicar
         Gr.RecSrv.Msg$ = "0"      ! Si aplica o no mensaje default no (0)
         Gr.RecSrv.Fiscal% = 1     ! Default Genera numeracion fiscal
         
         UE.RECASRV.POS% = MATCH("APLMSG",UE.RECASRV.MASK$(XXI%),1)
         If UE.RECASRV.POS% > 0 Then Begin                              ! Parametro de si aplica mensaje
           Gr.RecSrv.Msg$ = MID$(UE.RECASRV.MASK$(XXI%),UE.RECASRV.POS%+6,1)
         EndIf

         UE.RECASRV.POS% = MATCH("NUMMSG",UE.RECASRV.MASK$(XXI%),1)
         If UE.RECASRV.POS% > 0 Then Begin                              ! Parametro de si aplica mensaje
           Gr.RecSrv.Msg% = Val(MID$(UE.RECASRV.MASK$(XXI%),UE.RECASRV.POS%+6,2))
         EndIf
         
         If Gr.RecSrv.Msg$ = "1" Then Begin                      ! Si genera mensaje
         	  Ue.RecSrv.AplMsg$(Gr.RecSrv.Msg%) = "1"              ! Indica a la aplicacion que genere el mensaje
         EndIf
!--- End Add 24Abr2013
!--- ADD 29 Mayo 2013

         UE.RECASRV.POS% = MATCH("NUMFIS",UE.RECASRV.MASK$(XXI%),1)
         If UE.RECASRV.POS% > 0 Then Begin                              ! Parametro de si aplica mensaje
           Gr.RecSrv.Fiscal% = Val(MID$(UE.RECASRV.MASK$(XXI%),UE.RECASRV.POS%+6,1))
         EndIf

!--- End 29 Mayo 2013          	
       UE.RECASRV.DC$ = UE.RECASRV.DOC$(UE.RECASRV.QTY%)
       UE.RECASRV.VL$ = UE.RECASRV.VLR$(UE.RECASRV.QTY%)
       UE.RECASRV.FC$ = UE.RECASRV.FECHA$(UE.RECASRV.QTY%)
       If Len(UE.RECASRV.FC$) = 6 Then UE.RECASRV.FC$ = "20" + UE.RECASRV.FC$
       UE.RECASRV.NT$ = UE.RECASRV.NIT$(UE.RECASRV.QTY%)
       UE.RECASRV.TP$ = UE.RECASRV.TIPO$(UE.RECASRV.QTY%)
       UE.RECASRV.CLV$= UE.RECASRV.CLAVE$(UE.RECASRV.QTY%)
       ASIGNA.DATOS.MANUAL = -1
       UE.RECASRV.FLAG% = -1
       UE.RECASRV.ITM% = -3
       
     If Left$(UE.RECASRV.PLU$,6) = "770101" Then Begin 				 			! Si es un recaudo comfandi
        Gr.RecSrv.RecComfa% = -1     	  														! En un recaudo comfandi
     EndIf																													!

     If Left$(UE.RECASRV.PLU$,12) = "770101599993" Then Begin       ! Recaudo credito social
     	  Gr.RecSrv.RecComfa% = 0     	  														! cancela poderse pagar asi mismo
     EndIf

     Exit Function 

   EndIf 
Next XXI%
ASIGNA.DATOS.MANUAL = 0
UE.RECASRV.FLAG% = 0
UE.RECASRV.ITM% = 0

End Function

Function ASIGNA.DATOS.DOCUMENTO(UE.RECASRV.DATO$)
STRING UE.RECASRV.DATO$, Xdoc$, ZFEC$, Xvlr$, Xdata$
INTEGER*2 UE.RECASRV.POS%,UE.RECASRV.LENDOC%,UE.RECASRV.LENFECHA%,UE.RECASRV.LENVLR%,UE.RECASRV.LENCLAVE%
Integer*4 ZI%, XJ%, ASIGNA.DATOS.DOCUMENTO, XK%, XPOS%
Integer*1 Xfound%

ASIGNA.DATOS.DOCUMENTO = 0
UE.RECASRV.FLAG% = 0
UE.RECASRV.ITM% = 0          


For ZI% = 1 TO UE.RECASRV.CONT%
    
    If VAL(UE.RECASRV.PLU$) = VAL(MID$(UE.RECASRV.MASK$(ZI%),1,12)) Then Begin
       Xfound% = 0 
       !BUSCA DOC
      
      XPOS% = MATCH("TRXEXC",UE.RECASRV.MASK$(ZI%),1)									! Si trx exclusiva
      If MID$(UE.RECASRV.MASK$(ZI%),XPOS%+6,1) = "1" Then Begin       ! Si trx exclusiva
         If TS.INTRX Then Begin																				! Hay una trx en curso
          Call VISOR.AND.BORRAR("SOLO UN RECAUDO POR TRANSACCION  Borrar ")
          ASIGNA.DATOS.DOCUMENTO = 0
          UE.RECASRV.FLAG% = 0
          UE.RECASRV.ITM% = 0          
          Exit Function 
         EndIf
      EndIf
      UE.RECASRV.POS% = MATCH("DOCUMENTO",UE.RECASRV.MASK$(ZI%),1)
      If UE.RECASRV.POS% > 0 Then UE.RECASRV.LENDOC% = VAL(MID$(UE.RECASRV.MASK$(ZI%),UE.RECASRV.POS%+9,2))

!--- Add 11May2021      
       If UE.RECASRV.PLU$ = "770999800597" And 										     \! Recaudo Celsia
          Ue.RecSrv.LenCelsia% <> 58 Then Begin                         ! Nuevo recaudo 12 caracteres
          UE.RECASRV.LENDOC% = UE.RECASRV.LENDOC% - 2
       EndIf																														!

       If UE.RECASRV.PLU$ = "770720821001" And 										     \! Recaudo Celsia
          Ue.RecSrv.LenCelsia% <> 58 Then Begin                         ! Nuevo recaudo 12 caracteres
          UE.RECASRV.LENDOC% = UE.RECASRV.LENDOC% - 2
       EndIf																														!

       If UE.RECASRV.LENDOC% > 0 Then \
          Xdoc$ = MID$(UE.RECASRV.DATO$,21,UE.RECASRV.LENDOC%) Else \
          Xdoc$ = "0"

       If UE.RECASRV.QTY% < 0 Then UE.RECASRV.QTY% = 0
       For XK% = 1 To UE.RECASRV.QTY%																	! BUSCA SI RECIBO YA CAPTURADO
           If Xdoc$ = UE.RECASRV.DOC$(XK%) Then \
              Xfound% = -1 
       Next XK%
       
       If Xdoc$ = "0" Then Begin
          Call VISOR.AND.BORRAR("ERROR CAPTURA NUMERO DOCUMENTO")
          ASIGNA.DATOS.DOCUMENTO = 0
          UE.RECASRV.FLAG% = 0
          UE.RECASRV.ITM% = 0          
          Exit Function 
       EndIf
       
       If Xfound% = -1 Then Begin 
          Call VISOR.AND.BORRAR("RECIBO YA ESTA CAPTURADO")
          ASIGNA.DATOS.DOCUMENTO = 0
          UE.RECASRV.FLAG% = 0
          UE.RECASRV.ITM% = 0          
          Exit Function 
       EndIf 

    If Ue.RecSrv.Device% <> 3 Then Begin 
      XPOS% = MATCH("CAPDIG",UE.RECASRV.MASK$(ZI%),1)
      If MID$(UE.RECASRV.MASK$(ZI%),XPOS%+6,1) = "0" Then Begin 
         Call VISOR.AND.BORRAR("NO SE PERMITE RECAUDO DIGITADO")
         ASIGNA.DATOS.DOCUMENTO = 0
         UE.RECASRV.FLAG% = 0
         UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1
         UE.RECASRV.ITM% = 0           
         Exit Function 
      EndIf					     
    EndIf

    If UE.RECASRV.PLU$ = "770724718015" Then Begin                  ! Recaudo Emcali
       Ue.RecSrv.Emcali% = -1
    EndIf 

!--- Toma valor del recibo
         !BUSCA VALOR                                                                                              
         UE.RECASRV.POS% = MATCH("VALOR",UE.RECASRV.MASK$(ZI%),1)                                                  
         If UE.RECASRV.POS% > 0 Then UE.RECASRV.LENVLR% = VAL(MID$(UE.RECASRV.MASK$(ZI%),(UE.RECASRV.POS%+5),2))   
         If UE.RECASRV.LENVLR% > 0 Then Begin   
          If UE.RECASRV.LENVLR% = 99 Then Begin								! Si longitud variable
          	 UE.RECASRV.LENVLR% = Len(UE.RECASRV.DATO$) - (21 + UE.RECASRV.LENDOC% + 4) - 9 
          EndIf
          If UE.RECASRV.LENVLR% > 0 Then \
           UE.RECASRV.VLR$(UE.RECASRV.QTY%) = MID$(UE.RECASRV.DATO$,21+UE.RECASRV.LENDOC%+4,UE.RECASRV.LENVLR%) Else \
           UE.RECASRV.VLR$(UE.RECASRV.QTY%) = "0"
         EndIf Else UE.RECASRV.VLR$(UE.RECASRV.QTY%) = "0"                                                         
      
!--- Toma fecha del recuado  
    If Ue.RecSrv.Emcali% <> -1 Then Begin                            ! No es un recaudo emcali
       !BUSCA FECHA
       UE.RECASRV.POS% = MATCH("FECHA",UE.RECASRV.MASK$(ZI%),1)
       If UE.RECASRV.POS% > 0 Then UE.RECASRV.LENFECHA% = VAL(MID$(UE.RECASRV.MASK$(ZI%),(UE.RECASRV.POS%+5),1))
       If UE.RECASRV.LENFECHA% > 0 Then \
          UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = MID$(UE.RECASRV.DATO$,21+UE.RECASRV.LENDOC%+4+UE.RECASRV.LENVLR%+2,UE.RECASRV.LENFECHA%) Else \
          UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = "0"
       
       XPOS% = MATCH("FECVEN",UE.RECASRV.MASK$(ZI%),1)				          ! Si se valida la fecha de vencimiento

        If MID$(UE.RECASRV.MASK$(ZI%),Xpos%+6,1) = "1" Then Begin 		  ! Si se valida la fecha de vencimiento
           ZFEC$ = Right$(UE.RECASRV.FECHA$(UE.RECASRV.QTY%),6) 
        EndIf Else Begin 
           ZFEC$ = DATE$
           UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = "20"+Date$                        ! Mod 08Abr2009 - OVS
        EndIf 

        If Val(Date$) > Val(ZFEC$) Then Begin 																	! Si recibo fuera de la fecha de pago
           Call VISOR.AND.BORRAR("RECIBO FUERA DE LA  FECHA DE PAG3 * BORRAR")  
           ASIGNA.DATOS.DOCUMENTO = 0                                           
           UE.RECASRV.FLAG% = 0                                                 
           UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1                                
           UE.RECASRV.ITM% = 0                                                  
           UE.RECASRV.CODE$ = ""
           Gr.RecSrv.Simple% = 0
           TS.IO.STATE    = 10            ! PREPARE FOR NORMAL SALES STATE
           Exit Function                                                        
        EndIf                             
     
    EndIf
     
       UE.RECASRV.QTY% = UE.RECASRV.QTY% + 1
       !UE.RECASRV.DOC$(UE.RECASRV.QTY%) = Str$(Val(Xdoc$))    ! Asigna documento
       
       UE.RECASRV.DOC$(UE.RECASRV.QTY%) = Xdoc$    ! Asigna documento
       Gr.Rec.CtroCosto$ = "00000"
       If Right$(UE.RECASRV.MASK$(ZI%),1) = "1" Then Begin    				! Si proceso en linea       
         GR.TEMP1I4 = -1
         UE.RECASRV.APLICACION$ = "00"
         UE.RECASRV.FUNCION$    = "00"

         UE.RECASRV.POS% = MATCH("VALOR",UE.RECASRV.MASK$(ZI%),1)                                                  
         If UE.RECASRV.POS% > 0 Then UE.RECASRV.LENVLR% = VAL(MID$(UE.RECASRV.MASK$(ZI%),(UE.RECASRV.POS%+5),2))   
         If UE.RECASRV.LENVLR% > 0 Then Begin   
          If UE.RECASRV.LENVLR% = 99 Then Begin								! Si longitud variable
          	 UE.RECASRV.LENVLR% = Len(UE.RECASRV.DATO$) - (21 + UE.RECASRV.LENDOC% + 4) - 9 
          EndIf
          If UE.RECASRV.LENVLR% > 0 Then \
           UE.RECASRV.VLR$(UE.RECASRV.QTY%) = MID$(UE.RECASRV.DATO$,21+UE.RECASRV.LENDOC%+4,UE.RECASRV.LENVLR%) Else \
           UE.RECASRV.VLR$(UE.RECASRV.QTY%) = "0"
         EndIf Else UE.RECASRV.VLR$(UE.RECASRV.QTY%) = "0"                                                         

!--- Lineas adicionadas para parametros de aplicacion funcion 08Abrl2009         
         UE.RECASRV.POS% = MATCH("CODAPL",UE.RECASRV.MASK$(ZI%),1)
         If UE.RECASRV.POS% > 0 Then Begin                              ! PARAMETRO DE CAPTURA DE VALOR
           UE.RECASRV.APLICACION$ = MID$(UE.RECASRV.MASK$(ZI%),UE.RECASRV.POS%+6,2)
         EndIf
         UE.RECASRV.POS% = MATCH("CODFUN",UE.RECASRV.MASK$(ZI%),1)
         If UE.RECASRV.POS% > 0 Then Begin                              ! PARAMETRO DE CAPTURA DE VALOR
           UE.RECASRV.FUNCION$ = MID$(UE.RECASRV.MASK$(ZI%),UE.RECASRV.POS%+6,2)
         EndIf

         Gr.REC.TipID% = 1 
         Call Validacion.Recibo.Publico							! Valida el numero del recibo
        If GR.TEMP1I4 <> -1 Then Begin 							! Si proceso Fallido
        	
!-- Se elimina la validación manual del recibo
!-- Segun requerimiento Simple
!-- Mod 14 Ago 2014 
!        	 If Gr.RecSrv.Simple% = -1 Then BEGIN
!        	 	  Gr.RecSrv.Simple% = 1 
!        	 	  CALL TSHIECET
!        	 	  Call Visores4690(1,"PROCESO MANUAL DE LA","PLANILLA SIMPLE...",1000,"L")
!        	 	  GoTo PROCESO.MANUAL
!        	 ENDIF 

           ASIGNA.DATOS.DOCUMENTO = 0
           Gr.RecSrv.Simple% = 0
           Ue.RecSim.InTrx% = 0
           UE.RECASRV.FLAG% = 0
           UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1
           UE.RECASRV.ITM% = 0 
           Gr.REC.TipID% = 0          
           Exit Function 
        EndIf 
        
        If TS.INTRX AND Gr.RecSrv.Simple% = -1 Then BEGIN
           Call VISOR.AND.BORRAR("NO SE PERMITE RECAUDSIMPLE CON PRODUCTOS..")  
           ASIGNA.DATOS.DOCUMENTO = 0                                           
           UE.RECASRV.FLAG% = 0                                                 
           UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1                                
           UE.RECASRV.ITM% = 0                                                  
           UE.RECASRV.CODE$ = ""
           Gr.RecSrv.Simple% = 0
           Gr.REC.TipID% = 0
           TS.IO.STATE    = 10            ! PREPARE FOR NORMAL SALES STATE
           Exit Function                                            	
        EndIf

         If Gr.RecSrv.NewCol% = -1 Or Gr.RecSrv.SIGA% = -1 Then Begin				! Recaudo nuevo Colegio o SIGA
         	  GoTo Proceso.Manual																							! Valida datos del recibo
         EndIf

         If UE.RECASRV.PLU$ = "770724718015" Then Begin             				! Recaudo Emcali
            Ue.RecSrv.Emcali% = -1
         EndIf 

         If UE.RECASRV.PLU$ = "770101599993" Then Begin											! Recaudo Credito Social
         	  GoTo Proceso.Manual																							! Valida datos del recibo
         EndIf

         
          If Gr.RecSrv.Simple% = 0 Then Begin
             UE.RECASRV.VLR$(UE.RECASRV.QTY%)   = Str$(Val(Mid$(Gr.Rec.RtaMsg$,54,16)))   ! Toma Valor de la respuesta
             UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = Mid$(Gr.Rec.RtaMsg$,70,8)               ! Toma la fecha de Vencimiento
             UE.RECASRV.RTA$ = Mid$(Gr.Rec.RtaMsg$,70,29)                                 ! Trama de respuesta
             ZFEC$ = Right$(UE.RECASRV.FECHA$(UE.RECASRV.QTY%),6)
             Gr.Rec.CtroCosto$ = Mid$(Gr.Rec.RtaMsg$,086,5)                               ! Numero centro de costo para educacion
             Gr.Rec.CtroCosto$ = Str$(Val(Gr.Rec.CtroCosto$))
             Gr.Rec.CtroCosto$ = Right$("00000"+Gr.Rec.CtroCosto$,5)
          EndIf Else Begin																																! Recaudo Simple Online
             UE.RECASRV.VLR$(UE.RECASRV.QTY%)   = Str$(Val(Mid$(Gr.Rec.RtaMsg$,56,09)))   ! Toma Valor de la respuesta
             TS.TEMP5$ = Mid$(Gr.Rec.RtaMsg$,109,10)                                      ! Toma la fecha de Vencimiento
             TS.TEMP5$ = Mid$(TS.TEMP5$,3,2) + Mid$(TS.TEMP5$,6,2) + Right$(TS.TEMP5$,2)  ! Arma fecha reportada por WS AAMMDD
             UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = TS.TEMP5$                               ! Toma la fecha de Vencimiento
             Gr.RecSrv.IdRef$ = Mid$(Gr.Rec.RtaMsg$,65,20)                                ! Reference ID 
             UE.RECASRV.RTA$ = String$(12,"0")
             Ue.RecSrv.Periodo$ = Mid$(Gr.Rec.RtaMsg$,85,06)															! Periodo Cotizacion
             ZFEC$ = Right$(UE.RECASRV.FECHA$(UE.RECASRV.QTY%),6)                         ! 
             Gr.Rec.CtroCosto$ = "00000"
          EndIf

       EndIf Else Begin                                                              ! PROCESA LOCALMENTE RECIBO
       
       PROCESO.MANUAL:
         !BUSCA VALOR                                                                                              
         UE.RECASRV.POS% = MATCH("VALOR",UE.RECASRV.MASK$(ZI%),1)                                                  
         If UE.RECASRV.POS% > 0 Then UE.RECASRV.LENVLR% = VAL(MID$(UE.RECASRV.MASK$(ZI%),(UE.RECASRV.POS%+5),2))   
         If UE.RECASRV.LENVLR% > 0 Then Begin   
          If UE.RECASRV.LENVLR% = 99 Then Begin								! Si longitud variable
          	 UE.RECASRV.LENVLR% = Len(UE.RECASRV.DATO$) - (21 + UE.RECASRV.LENDOC% + 4) - 9 
          EndIf
          If UE.RECASRV.LENVLR% > 0 Then \
           UE.RECASRV.VLR$(UE.RECASRV.QTY%) = MID$(UE.RECASRV.DATO$,21+UE.RECASRV.LENDOC%+4,UE.RECASRV.LENVLR%) Else \
           UE.RECASRV.VLR$(UE.RECASRV.QTY%) = "0"
         EndIf Else UE.RECASRV.VLR$(UE.RECASRV.QTY%) = "0"                                                         

       UE.RECASRV.POS% = MATCH("CAPVAL",UE.RECASRV.MASK$(ZI%),1)
       If UE.RECASRV.POS% > 0 Then Begin                              ! PARAMETRO DE CAPTURA DE VALOR
          If MID$(UE.RECASRV.MASK$(ZI%),UE.RECASRV.POS%+6,1) = "1" Then Begin 
             Entra.Valor.Recaudo:
             If UE.RECASRV.PLU$ <> "770101599993" Then \
                Xvlr$ = UE.RECASRV.VLR$(UE.RECASRV.QTY%) Else \
                Xvlr$ = Str$(Val(Mid$(Gr.Rec.RtaMsg$,57,12))/100) 
              Call Format.Amount(Val(Xvlr$))
              Xdata$ = Asic.Datos$("Vlr: $"+TS.TEMP1$,"CONFIRMA? 1.Si 9.Otro")
              If MATCH(XDATA$,"19",1) <= 0 Then Begin				           			! Dato no valido
                 GoTo Entra.Valor.Recaudo
              EndIf 
              If Val(Xdata$) = 9 Then Begin 																! Captura valor 
                 Xvlr$ = Asic.Datos$("Valor a Pagar?","")										! Msg al cajero
                 If Val(Xvlr$) <= 0 Or (Xvlr$ = "E") Then Begin	        		! No se capturo dato
                     ASIGNA.DATOS.DOCUMENTO = 0
                     UE.RECASRV.FLAG% = 0
                     UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1
                     UE.RECASRV.ITM% = 0           
                     Gr.RecSrv.CrdSoc% = 0
                     Exit Function 																					! Sale de la rutina
                 EndIf 
              EndIf 
              UE.RECASRV.VLR$(UE.RECASRV.QTY%) = Xvlr$               				! Asigna valor pagado
          EndIf					     
       EndIf 
  EndIf 


  If Right$(UE.RECASRV.MASK$(ZI%),1) <> "1" Then Begin    									! Si proceso no es en linea       
       !BUSCA FECHA
       UE.RECASRV.POS% = MATCH("FECHA",UE.RECASRV.MASK$(ZI%),1)
       If UE.RECASRV.POS% > 0 Then UE.RECASRV.LENFECHA% = VAL(MID$(UE.RECASRV.MASK$(ZI%),(UE.RECASRV.POS%+5),1))
       If UE.RECASRV.LENFECHA% > 0 Then \
          UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = MID$(UE.RECASRV.DATO$,21+UE.RECASRV.LENDOC%+4+UE.RECASRV.LENVLR%+2,UE.RECASRV.LENFECHA%) Else \
          UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = "0"
  EndIf 

  If (Right$(UE.RECASRV.MASK$(ZI%),1) = "1" And Gr.RecSrv.Simple% = 1) Then Begin ! Si proceso en linea y recaudo simple
       !BUSCA FECHA
       UE.RECASRV.POS% = MATCH("FECHA",UE.RECASRV.MASK$(ZI%),1)
       If UE.RECASRV.POS% > 0 Then UE.RECASRV.LENFECHA% = VAL(MID$(UE.RECASRV.MASK$(ZI%),(UE.RECASRV.POS%+5),1))
       If UE.RECASRV.LENFECHA% > 0 Then \
          UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = MID$(UE.RECASRV.DATO$,21+UE.RECASRV.LENDOC%+4+UE.RECASRV.LENVLR%+2,UE.RECASRV.LENFECHA%) Else \
          UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = "0"
  EndIf 

  If (Right$(UE.RECASRV.MASK$(ZI%),1) = "1" And Gr.RecSrv.NewCol% = -1) Then Begin ! Si proceso en linea y recaudo New colegio
       !BUSCA FECHA
       UE.RECASRV.POS% = MATCH("FECHA",UE.RECASRV.MASK$(ZI%),1)
       If UE.RECASRV.POS% > 0 Then UE.RECASRV.LENFECHA% = VAL(MID$(UE.RECASRV.MASK$(ZI%),(UE.RECASRV.POS%+5),1))
       If UE.RECASRV.LENFECHA% > 0 Then \
          UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = MID$(UE.RECASRV.DATO$,21+UE.RECASRV.LENDOC%+4+UE.RECASRV.LENVLR%+2,UE.RECASRV.LENFECHA%) Else \
          UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = "0"
  EndIf 

  If (Right$(UE.RECASRV.MASK$(ZI%),1) = "1" And Gr.RecSrv.Siga% = -1) Then Begin ! Si proceso en linea y recaudo SIGA
       !BUSCA FECHA
       UE.RECASRV.POS% = MATCH("FECHA",UE.RECASRV.MASK$(ZI%),1)
       If UE.RECASRV.POS% > 0 Then UE.RECASRV.LENFECHA% = VAL(MID$(UE.RECASRV.MASK$(ZI%),(UE.RECASRV.POS%+5),1))
       If UE.RECASRV.LENFECHA% > 0 Then \
          UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = MID$(UE.RECASRV.DATO$,21+UE.RECASRV.LENDOC%+4+UE.RECASRV.LENVLR%+2,UE.RECASRV.LENFECHA%) Else \
          UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = "0"
  EndIf 
  
!--- Parametro para validar si se captura o no fecha de vencimiento validada

        XPOS% = MATCH("FECVEN",UE.RECASRV.MASK$(ZI%),1)				  ! Si se valida la fecha de vencimiento

        If MID$(UE.RECASRV.MASK$(ZI%),Xpos%+6,1) = "1" Then Begin 		  ! Si se valida la fecha de vencimiento
           ZFEC$ = Right$(UE.RECASRV.FECHA$(UE.RECASRV.QTY%),6) 
        EndIf Else Begin 
           ZFEC$ = DATE$
           UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = "20"+Date$                        ! Mod 08Abr2009 - OVS
        EndIf 
        If Val(Date$) > Val(ZFEC$) Then Begin 																	! Si recibo fuera de la fecha de pago
           Call VISOR.AND.BORRAR("RECIBO FUERA DE LA  FECHA DE PAG4 * BORRAR")  
           ASIGNA.DATOS.DOCUMENTO = 0                                           
           UE.RECASRV.FLAG% = 0                                                 
           UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1                                
           UE.RECASRV.ITM% = 0                                                  
           UE.RECASRV.CODE$ = ""
           Gr.RecSrv.Simple% = 0
           TS.IO.STATE    = 10            ! PREPARE FOR NORMAL SALES STATE
           Exit Function                                                        
        EndIf                                                                   

      !BUSCA CLAVE
       UE.RECASRV.POS% = MATCH("CLAVE",UE.RECASRV.MASK$(ZI%),1)
       If UE.RECASRV.POS% > 0 Then UE.RECASRV.LENCLAVE% = 8
       If UE.RECASRV.LENCLAVE% > 0 Then \
          UE.RECASRV.CLAVE$(UE.RECASRV.QTY%) = MID$(UE.RECASRV.MASK$(ZI%),(UE.RECASRV.POS%+5),UE.RECASRV.LENCLAVE%) Else \
          UE.RECASRV.CLAVE$(UE.RECASRV.QTY%) = "11111111"          
        
       !BUSCA NIT   
       UE.RECASRV.POS% = MATCH("NIT",UE.RECASRV.MASK$(ZI%),1)
       UE.RECASRV.NIT$(UE.RECASRV.QTY%) = MID$(UE.RECASRV.MASK$(ZI%),(UE.RECASRV.POS%+3),14)
      
       If (SL.IT.INDICAT2 And 80H) OR (SL.IT.INDICAT2 And 40H) Then \
          UE.RECASRV.TIPO$(UE.RECASRV.QTY%) = "1"  Else UE.RECASRV.TIPO$(UE.RECASRV.QTY%) = "0"
     
!-- Add 24Abr2013
         Gr.RecSrv.Msg% = 0        ! Numero de mensaje aplicar
         Gr.RecSrv.Msg$ = "0"      ! Si aplica o no mensaje default no (0)
         Gr.RecSrv.Fiscal% = 1     ! Default Genera numeracion fiscal
         
         UE.RECASRV.POS% = MATCH("APLMSG",UE.RECASRV.MASK$(ZI%),1)
         If UE.RECASRV.POS% > 0 Then Begin                              ! Parametro de si aplica mensaje
           Gr.RecSrv.Msg$ = MID$(UE.RECASRV.MASK$(ZI%),UE.RECASRV.POS%+6,1)
         EndIf

         UE.RECASRV.POS% = MATCH("NUMMSG",UE.RECASRV.MASK$(ZI%),1)
         If UE.RECASRV.POS% > 0 Then Begin                              ! Parametro de si aplica mensaje
           Gr.RecSrv.Msg% = Val(MID$(UE.RECASRV.MASK$(ZI%),UE.RECASRV.POS%+6,2))
         EndIf
         
         If Gr.RecSrv.Msg$ = "1" Then Begin                      ! Si genera mensaje
         	  Ue.RecSrv.AplMsg$(Gr.RecSrv.Msg%) = "1"              ! Indica a la aplicacion que genere el mensaje
         EndIf
!--- End Add 24Abr2013

!--- ADD 29 Mayo 2013

         UE.RECASRV.POS% = MATCH("NUMFIS",UE.RECASRV.MASK$(ZI%),1)
         If UE.RECASRV.POS% > 0 Then Begin                              ! Parametro de si aplica mensaje
           Gr.RecSrv.Fiscal% = Val(MID$(UE.RECASRV.MASK$(ZI%),UE.RECASRV.POS%+6,1))
         EndIf

!--- End 29 Mayo 2013          	
     
       UE.RECASRV.DC$ = UE.RECASRV.DOC$(UE.RECASRV.QTY%)
       UE.RECASRV.VL$ = UE.RECASRV.VLR$(UE.RECASRV.QTY%)
       UE.RECASRV.FC$ = UE.RECASRV.FECHA$(UE.RECASRV.QTY%)
       If Len(UE.RECASRV.FC$) = 6 Then UE.RECASRV.FC$ = "20" + UE.RECASRV.FC$
       UE.RECASRV.NT$ = UE.RECASRV.NIT$(UE.RECASRV.QTY%)
       UE.RECASRV.TP$ = UE.RECASRV.TIPO$(UE.RECASRV.QTY%)
       UE.RECASRV.CLV$= UE.RECASRV.CLAVE$(UE.RECASRV.QTY%)
       UE.RECASRV.CCOSTO$(UE.RECASRV.QTY%) = Gr.Rec.CtroCosto$
       ASIGNA.DATOS.DOCUMENTO = -1
       UE.RECASRV.FLAG% = -1
       UE.RECASRV.ITM% = -3
       
     If Left$(UE.RECASRV.PLU$,6) = "770101" Then Begin 				 			! Si es un recaudo comfandi
        Gr.RecSrv.RecComfa% = -1     	  														! En un recaudo comfandi
     EndIf																													!

     If Left$(UE.RECASRV.PLU$,12) = "770101599993" Then Begin       ! Recaudo credito social
     	  Gr.RecSrv.RecComfa% = 0     	  														! cancela poderse pagar asi mismo
     EndIf
       
       Exit Function  
    EndIf
NEXT ZI%

End Function


Function ASIGNA.DATOS.ACUAVIVA
Integer*4 ASIGNA.DATOS.ACUAVIVA
String    UE.RECASRV.DATO$, Xdoc$, ZFEC$, Xvlr$, Xdata$, XCuota$
Integer*4 XI%, XJ%, XK%, Xpos%
Integer*1 Xfound%
     Xdoc$ = "" : Xvlr$ = "" : XCuota$ = ""
     Xpos% = Match("/",UE.RECASRV.CODE$,1)          ! Recibo con separador de Campo
     If Xpos% <= 0 Then Begin 
       XCuota$ = ASIC.DATOS$("NUMERO CUOTA?","")	  	! Ingresa numero recibo
       If (XCuota$ = "") OR (XCuota$ = "E") Then Begin	! No se capturo dato
          Call VISOR.AND.BORRAR("PROCESO CANCELADO POR USUARIO")
          ASIGNA.DATOS.ACUAVIVA = 0
          UE.RECASRV.FLAG% = 0
          UE.RECASRV.ITM% = 0          
          Exit Function 
       EndIf		

       XVlr$ = ASIC.DATOS$("VALOR RECAUDO?","")	  	! Ingresa numero recibo
       If (XVlr$ = "") OR (XVlr$ = "E") Then Begin	! No se capturo dato
          Call VISOR.AND.BORRAR("PROCESO CANCELADO POR USUARIO")
          ASIGNA.DATOS.ACUAVIVA = 0
          UE.RECASRV.FLAG% = 0
          UE.RECASRV.ITM% = 0          
          Exit Function 
       EndIf	
       Xdoc$ = UE.RECASRV.CODE$                         ! Numero de referencia capturado
       
       If Val(XCuota$) > 0 Then \                       ! Recaudo con cuotas
          Xdoc$ = Xdoc$ + "-" + XCuota$

     EndIf Else Begin 
      Xdoc$ = Mid$(UE.RECASRV.CODE$,1,(XPOS%-1))				! Toma numero de referencia
      Xvlr$  = Mid$(UE.RECASRV.CODE$,(Xpos%+1),(Len(UE.RECASRV.CODE$)-Xpos%))   ! Toma Valor del recibo
      
     EndIf 
     Xfound% = 0 
     For XK% = 1 To UE.RECASRV.QTY%					! BUSCA SI RECIBO YA CAPTURADO
           If Match(Xdoc$,UE.RECASRV.DOC$(XK%),1) > 0  Then \
              Xfound% = -1 
     Next XK%
     If Xfound% = -1 Then Begin 																	  ! Recibo ya ingresado
        Call VISOR.AND.BORRAR("RECIBO YA ESTA CAPTURADO")
        ASIGNA.DATOS.ACUAVIVA = 0
        UE.RECASRV.FLAG% = 0
        UE.RECASRV.ITM% = 0          
        Exit Function 
     EndIf 

!-- Add 24Abr2013
         If Ue.RecSrv.AplMsgAcv$ = "1" Then Begin                      ! Si genera mensaje
         	  Ue.RecSrv.AplMsg$(Val(Ue.RecSrv.NumMsgAcv$)) = "1"         ! Indica a la aplicacion que genere el mensaje
         EndIf
         Gr.RecSrv.Fiscal% = Val(Ue.RecSrv.FiscalAcv$)                 ! Si aplica numeracion fiscal   
!--- End Add 24Abr2013

     UE.RECASRV.QTY% = UE.RECASRV.QTY% + 1												        	! Aumenta contador
     UE.RECASRV.DOC$(UE.RECASRV.QTY%)   = Xdoc$															! Numero del documento
     UE.RECASRV.VLR$(UE.RECASRV.QTY%)   = Xvlr$ 
     UE.RECASRV.FECHA$(UE.RECASRV.QTY%) = Date$                             ! Fecha Vencimimiento
     UE.RECASRV.NIT$(UE.RECASRV.QTY%)   = UE.RECASRV.ACUAVINIT$             ! Nit del recaudador
     UE.RECASRV.CLAVE$(UE.RECASRV.QTY%) = UE.RECASRV.ACUAVIPRO$             ! Codigo del proyecto
     UE.RECASRV.TIPO$(UE.RECASRV.QTY%)  = "1" 

     UE.RECASRV.DC$ = UE.RECASRV.DOC$(UE.RECASRV.QTY%)
     UE.RECASRV.VL$ = UE.RECASRV.VLR$(UE.RECASRV.QTY%)
     UE.RECASRV.FC$ = UE.RECASRV.FECHA$(UE.RECASRV.QTY%)
     If Len(UE.RECASRV.FC$) = 6 Then UE.RECASRV.FC$ = "20" + UE.RECASRV.FC$
     UE.RECASRV.NT$ = UE.RECASRV.NIT$(UE.RECASRV.QTY%)
     UE.RECASRV.TP$ = UE.RECASRV.TIPO$(UE.RECASRV.QTY%)
     UE.RECASRV.CLV$= UE.RECASRV.CLAVE$(UE.RECASRV.QTY%)
     UE.RECASRV.RTA$ = String$(12,"0")
     Gr.Rec.CtroCosto$ = "00000"
     UE.RECASRV.CCOSTO$(UE.RECASRV.QTY%) = Gr.Rec.CtroCosto$
     ASIGNA.DATOS.ACUAVIVA = -1
     UE.RECASRV.FLAG% = -1
     UE.RECASRV.ITM% = -3
     Gr.REC.Acuaviva% = -1

End Function 

Sub PAGOS.RSV.HOTEL																																! Recuperacion pagos Rsv Hoteleras
  Integer*4 PRIC%, TOT4%, X.I%, Rpago%(1), X.J%																		!
  String    X.LISTA$, XTV$, XPG$           																				!
  Dim Rpago%(70)                                                                  !
  X.LISTA$ = ""				    																								  			! Init variable
  FOR X.I% = 1 TO SL.END                    																			! FOR ALL StringS
    H$ = READ.SL.STR$(X.I%)                 																			! GET String
    If LEN(H$) > 5 Then Begin                                                     ! ASSURE GOOD String
     Asc.Tmp.Apun% = 3																													  ! Posicion inicial strings
     If ASC(H$)  = 5 Then Begin             																			! TENDER String
        XTV$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	 														  			! Tipo y Variedad de pago
        XPG$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	 														  			! Valor del pago
        Rpago%(Val(XTV$)) = Rpago%(Val(XTV$)) + Val(XPG$)                         ! Acumula pagos por TV
     EndIf                                  																			! Tender String
     If ASC(H$)  = 6 Then Begin             																			! TENDER String Anul Pago
        XTV$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	 														  			! Tipo y Variedad de pago
        XPG$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	 														  			! Valor del pago
        Rpago%(Val(XTV$)) = Rpago%(Val(XTV$)) - Val(XPG$)                         ! Acumula pagos por TV
     EndIf                                  																			! Tender String
    EndIf
  NEXT X.I%   																																		!
  X.I% = 0
  Wait ; 100
  If SL.CH.AMTCHANGE <> 0 Then Begin 
  	 Rpago%(11) = Rpago%(11) - SL.CH.AMTCHANGE                                    ! Disminuye vueltas del efectivo
  EndIf
  X.LISTA$ = ""																																		! Init variable lista pagos
  For X.I% = 11 To 70 																														! Reporta formas de pago
   If Rpago%(X.I%) > 0 Then Begin																									! Cambia condicion <> por > 
      X.LISTA$ = X.LISTA$ +  Right$("00"+Str$(X.I%),2) + Right$("00000000"+Str$(Rpago%(X.I%)),8) ! Lista de pagos
   EndIf
  Next X.I%
  TS.TEMP3$ = Left$(X.LISTA$+String$(100," "),100)																! Retorna lista de pagos ajusta a 100
End Sub 

Sub ACT.RSV.HOTEL                                      											     	! Actualizacion reserva hotelera
String Xtmp1$																																	   	! 
String    Xenvia$, Xsale$, Xrta$, Xllega$, Xfunc$
    Xtmp1$ = UE.RECASRV.DOC$(UE.RECASRV.QTY%)																		 	! Numero de la reserva
    Xtmp1$ = UE.RECASRV.DOC$(1)																		 								! Numero de la reserva Add 2 abr 2018
    Call TSHIECET                                                                	! Tono de alerta                 		
    
    If TS.TRAINING Then Begin																											! En entrenamiento
    	 Call VISORES4690(1,"ACTUALIZA RESERVA:","***ENTRENAMIENTO****",1000,"L")  	!	
    	 Exit Sub 
    EndIf
    Xfunc$ = "08"																																	! conexion a saleforce
!-- Nuevo modulo de reservas, retirar comentarios       28 jul 2021    
    If UE.RECASRV.TipoRecSales% = 1 Then Xfunc$ = "18"          									! Recaudo reservas Zeus Mod 25/Jun/2021
    If UE.RECASRV.TipoRecSales% = 2 Then Xfunc$ = "08"          									! Validación fomento Saleforce Mod 25/Jun/2021
    
    Asc.Pay.Impr% = 0
    Call VISORES4690(1,"ACTUALIZA RESERVA:",XTMP1$,0,"L")       						     	!	
    TS.TEMP1$ = RIGHT$("                   "+XTMP1$,19  )                        	! Ajusta numero de reserva
    Call PAGOS.RSV.HOTEL                                                         	! Retorna lista de las formas de pago
    TS.TEMP1$ = TS.TEMP1$ + TS.TEMP3$																						 	! Numero de reserva + formas de pago
    TS.TEMP2$ = Armar.Trama.Msg("14",Xfunc$,TS.TEMP1$,"00","0001","123456")      	! Armar trama MSG                	
    Xenvia$   = TS.TEMP2$																										    	!
    Xsale$    = Date$ + " : " + Time$																					    ! Hora salida requerimiento
    TS.TEMP1$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)         	! Ejecuta Requerimiento
    Xrta$     = Date$ + " : " + Time$																					    ! Hora llegada requerimiento
    Xllega$   = TS.TEMP1$																								          !
    Call RECTER.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                         ! Rastro auditoria
    
End Sub 
!----- Definicion de User Exits

!-- Se modifica respuesta de validacion de simple
!-- no tiene encuenta validaciones fallidas para el cierre de la trx
!-- Solicitado por Comfandi 10-mzo-2016
!---
Sub ACT.REC.SIMPLE
String Xtmp1$
String    Xenvia$, Xsale$, Xrta$, Xllega$, Xtrama$
 	  Call Calculo.Request                                                          ! Numero requerimiento al WS 
    Xtmp1$ = Gr.RecSrv.IdRef$               																		 	! Reference ID de la trx
    Call TSHIECET                                                                	! Tono de alerta  
    
    If TS.TRAINING Then Begin																											! En entrenamiento
    	 Call VISORES4690(1,"ACTUALIZA RECAUDO:","***ENTRENAMIENTO****",1000,"L") 	!	
    	 TS.ER.RETURN = -1
    	 Exit Sub 
    EndIf
    Asc.Pay.Impr% = 0
    Call VISORES4690(1,"ACTUALIZA RECAUDO:",XTMP1$,500,"L")       						   	!	
    TS.TEMP1$ = Left$(Xtmp1$,18)                             											! Reference ID 18 posiciones
    TS.TEMP2$ = RIGHT$("         "+UE.RECASRV.VLR$(UE.RECASRV.QTY%),9)            !
    TS.TEMP2$ = RIGHT$("         "+UE.RECASRV.VLR$(1),9)            							! Valor del recaudo 2 Abr 2018
    TS.TEMP3$ = RIGHT$("                    "+Ue.RecSrv.Planilla$,20)             ! Numero de planilla
    TS.TEMP1$ = Gr.RecSrv.Rqst$ + TS.TEMP1$ + TS.TEMP2$ + TS.TEMP3$ +            \!
                Ue.RecSrv.Periodo$ + Str$(Gr.RecSrv.SndSmp%)                      ! Arma trama mensaje 
    TS.TEMP2$ = Armar.Trama.Msg("15","61",TS.TEMP1$,"00","0001","123456")        	! Armar trama MSG                	
    Xenvia$   = TS.TEMP2$																										    	!
    Xsale$    = Date$ + " : " + Time$																				      ! Hora salida requerimiento
    Xtrama$   = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)         	! Ejecuta Requerimiento
    Xrta$     = Date$ + " : " + Time$																					    ! Hora llegada requerimiento
    Xllega$   = Xtrama$   																							          ! Trama de respuesta
    Call RECTER.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                         ! Rastro auditoria
    TS.TEMP1$ = Valida.Rta(Xtrama$)																			          ! Valida rta entregada     
    
    If TS.TEMP1$ = "00" Then Begin	  							   										        ! Si proceso java OK
       Xrta$ = MID$(Xtrama$,12,2)																					  		  ! Mensaje proceso 
       If Xrta$ = "00" Then Begin 
          TS.ER.RETURN = -1 																											! Proceso OK

!       	  Xrta$ = MID$(Xtrama$,14,40)																			  		  ! Mensaje proceso 
!          Call Visor.and.Borrar(Xrta$)																	    			! Mensaje todo OK

          Exit Sub 
       EndIf Else Begin 																													!
     	    If Len(Xtrama$) > 9 Then                                               \!
   	         Xrta$ = Mid$(Xtrama$,14,40) Else  																	 \! Mensaje Error proceso 
   	         Xrta$ = "FALLA EN COMUNICACION"
          Call Visor.and.Borrar(Xrta$)																	    			! Mensaje todo OK
       EndIf 
       TS.ER.RETURN = -1																													! Retorna falla 
    EndIf Else Begin
    	 If Len(Xtrama$) > 9 Then                                                  \!
   	      Xrta$ = Mid$(Xtrama$,14,40) Else  																		 \! Mensaje Error proceso 
   	      Xrta$ = "FALLA EN COMUNICACION"
       Call Visor.and.Borrar(Xrta$)																	    			    ! 
       Gr.RecSrv.SndSmp% = Gr.RecSrv.SndSmp% + 1																	! Nro de re intentos
       If Gr.RecSrv.SndSmp% > 10 Then Gr.RecSrv.SndSmp% = 1 											! Se ha intentado varias veces
    	 TS.ER.RETURN = -1																													! Error en el proceso
    EndIf

End Sub 


Sub ACT.REC.CRDSOC 																																! Confirmación recaudo credito social
String Xtmp1$
String    Xenvia$, Xsale$, Xrta$, Xllega$, Xtrama$
    Call TSHIECET                                                                	! Tono de alerta  
    If TS.TRAINING Then Begin																											! En entrenamiento
    	 Call VISORES4690(1,"ACTUALIZA RECAUDO:","***ENTRENAMIENTO****",1000,"L") 	!	
    	 TS.ER.RETURN = -1
    	 Exit Sub 
    EndIf
    Asc.Pay.Impr% = 0
    XTMP1$ = UE.RECASRV.DOC$(1)
    Call VISORES4690(1,"ACTUALIZA RECAUDO:",XTMP1$,500,"L")       						   	!	
    TS.TEMP1$ = Right$(String$(25," ")+Xtmp1$,25)            											! Reference ID 25 posiciones
    TS.TEMP2$ = RIGHT$("            "+(UE.RECASRV.VLR$(1)+"00"),12)  							! Valor del recaudo
    TS.TEMP1$ = TS.TEMP1$ + TS.TEMP2$																							! Arma trama mensaje
    TS.TEMP2$ = Armar.Trama.Msg("21","03",TS.TEMP1$,"00","0001","123456")        	! Armar trama MSG                	
    Xenvia$   = TS.TEMP2$																										    	!
    Xsale$    = Date$ + " : " + Time$																				      ! Hora salida requerimiento
    Xtrama$   = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)         	! Ejecuta Requerimiento
    Xrta$     = Date$ + " : " + Time$																					    ! Hora llegada requerimiento
    Xllega$   = Xtrama$   																							          ! Trama de respuesta
    Call RECTER.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                         ! Rastro auditoria
    TS.TEMP1$ = Valida.Rta(Xtrama$)																			          ! Valida rta entregada     
    
    If TS.TEMP1$ = "00" Then Begin	  							   										        ! Si proceso java OK
       Xrta$ = MID$(Xtrama$,12,2)																					  		  ! Mensaje proceso 
       If Xrta$ = "00" Then Begin 
          TS.ER.RETURN = -1 																											! Proceso OK
          Exit Sub 
       EndIf 
       TS.ER.RETURN = -1																													! Retorna falla 
    EndIf 

End Sub 

Sub ACT.REC.NEWCOL																																! Actualizacion nuevo recaudo colegio
Integer*4 PRIC%, TOT4%, X.I%, XI%,					                                     \!
           UE.RECASRV.POS%, UE.RECASRV.LENCLAVE%																	!
String X.TMP$, XCLAVE$, Xenvia$, Xsale$, Xrta$, Xllega$, Xvlr$										!

    If TS.TRAINING Then Begin																											! En entrenamiento
    	 Call VISORES4690(1,"ACTUAL. REC. COL.:","***ENTRENAMIENTO****",1000,"L")  	!	
    	 Exit Sub 
    EndIf
  Asc.Pay.Impr% = 0
  For X.I% = 1 TO UE.RECASRV.Slend%                			             							! Recorre todos los String
    H$ = READ.SL.STR$(X.I%)                 				                 							! Toma el String
    If LEN(H$) > 5 Then                    			                                 \! Si el String es OK
     If ASC(H$)  = 153 Then Begin            				                 							! Si es un Data Entry
        Asc.Tmp.Apun% = 3						                                 							! Asigna apuntador
        X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  			             							! Verifica el numero de proyecto
        If X.Tmp$ = "20140930" Then Begin																					! Recaudo nuevo Colegio
        	 X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  			             				  ! Ean del recaudo
        	 X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  			             				  ! Tipo de proceso
        	 X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  			             				  ! Numero del documento
        	 Xvlr$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  			             				  ! Valor del documento
           Call VISORES4690(1,"ACTUAL. REC. COL.:",X.TMP$,0,"L")       					 	!	                            
           TS.TEMP1$ = Right$("                   "+X.TMP$,18  )   +             \! Ajusta numero de documento  Asc 18
                       Right$("          "+Str$(Val(Xvlr$)),10)                   ! Valor del recaudo           Asc 10
         	 TS.TEMP2$ = Armar.Trama.Msg("17","72",TS.TEMP1$,"00","0001","123456") 	! Armar trama MSG           
           Xenvia$   = TS.TEMP2$																								  !                           
           Xsale$    = Date$ + " : " + Time$																		  ! Hora salida requerimiento 
           TS.TEMP1$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)  	! Ejecuta Requerimiento     
           Xrta$     = Date$ + " : " + Time$																		  ! Hora llegada requerimiento
           Xllega$   = TS.TEMP1$																								  !                           
           Call RECTER.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                  ! Rastro auditoria           
        EndIf
     EndIf                                  																			!
  Next X.I%                                 																			!

End Sub																																						! Fin actualizacion nuevo recaudo colegio

Sub ACT.REC.SIGA  																													! Actualizacion recaudo SIGA
Integer*4 PRIC%, TOT4%, X.I%, XI%,			                                   \!
           UE.RECASRV.POS%, UE.RECASRV.LENCLAVE%														!
String X.TMP$, XCLAVE$, Xenvia$, Xsale$, Xrta$, Xllega$, Xvlr$							!

    If TS.TRAINING Then Begin																								! En entrenamiento
    	 Call VISORES4690(1,"ACTUAL. REC.SIGA.:","***ENTRENAMIENTO****",1000,"L")  	!	
    	 Exit Sub 
    EndIf
  Asc.Pay.Impr% = 0
  For X.I% = 1 TO UE.RECASRV.Slend%                			        						! Recorre todos los String
    H$ = READ.SL.STR$(X.I%)                 				            						! Toma el String
    If LEN(H$) > 5 Then                    			                           \! Si el String es OK
     If ASC(H$)  = 153 Then Begin            				            						! Si es un Data Entry
        Asc.Tmp.Apun% = 3						                            						! Asigna apuntador
        X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  			        						! Verifica el numero de proyecto
        If X.Tmp$ = "20080520" Then Begin																		! Recaudo nuevo Colegio
        	 X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  			       				  ! Ean del recaudo
        	 X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  			       				  ! Tipo de proceso
        	 X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  			       				  ! Numero del documento
        	 Xvlr$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  			       				  ! Valor del documento
           X.TMP$ = Str$(Val(X.TMP$))
           Call VISORES4690(1,"ACTUAL. REC.SIGA.:",X.TMP$,0,"L") 					 	!
           TS.TEMP1$ = Right$("                   "+X.TMP$,18  )   +       \! Ajusta numero de documento  Asc 18
                       Right$("          "+Str$(Val(Xvlr$)),10)             ! Valor del recaudo           Asc 10
         	 TS.TEMP2$ = Armar.Trama.Msg("18","92",TS.TEMP1$,"00","0001","123456") 	! Armar trama MSG
           Xenvia$   = TS.TEMP2$																				    !
           Xsale$    = Date$ + " : " + Time$																! Hora salida requerimiento 
           TS.TEMP1$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)  	! Ejecuta Requerimiento
           Xrta$     = Date$ + " : " + Time$																! Hora llegada requerimiento
           Xllega$   = TS.TEMP1$																						!
           Call RECTER.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)            ! Rastro auditoria
        EndIf
     EndIf                                  															  !
  Next X.I%                                 															  !

End Sub																																			! Fin actualizacion SIGA

Sub Init.Proc.Recaudos
   UE.RECASRV.FLAG% = 0
   UE.RECASRV.QTY%  = 0
   UE.RECASRV.DC$ = ""
   UE.RECASRV.VL$ = ""
   UE.RECASRV.FC$ = ""
   UE.RECASRV.NT$ = ""
   UE.RECASRV.TP$ = ""
   UE.RECASRV.CODE$ = ""
   Gr.Rec.CtroCosto$ = "00000"
   UE.RECASRV.ANUL% = 0 
   UE.RECASRV.Recup% = 0 
   UE.RECASRV.ITM% = 0
   Ue.RecSrv.Emcali% = 0 
   Dim UE.RECASRV.DOC$(100)
   Dim UE.RECASRV.VLR$(100)
   Dim UE.RECASRV.NIT$(100)
   Dim UE.RECASRV.FECHA$(100)
   Dim UE.RECASRV.TIPO$(100)
   Dim UE.RECASRV.CLAVE$(100)
   Dim UE.RECASRV.CCOSTO$(100)
   Dim UE.RECASRV.TRX$(100)
   Dim Ue.RecSrv.AplMsg$(60)
  Ue.RecSrv.OnTrx% = 0
  Gr.REC.Acuaviva% = 0 
  Gr.Rec.NitRsv$ = ""
  Gr.Rec.NameRsv$ = ""
  Ue.RecSrv.Reserva% = 0
  Ue.RecSrv.InTrx% = 0 
  Ue.RecSim.InTrx% = 0 
  Ue.RecSrv.Device% = 0 
  Ue.RecSrv.PagReserva% = 0
  Gr.RecSrv.Txn% = 0 
  Gr.REC.TipID% = 0
  Gr.RecSrv.NewCol% = 0
  SL.CH.AMTCHANGE = 0 			! Add 8 ene 2014
  Ue.RecSrv.Periodo$ = ""
  Ue.RecSrv.Planilla$ = ""
  Gr.RecSrv.Simple% = 0 
  Gr.RecSrv.SndSmp% = 0
  Gr.RecSrv.RecComfa% = 0
  Gr.RecSrv.Siga% = 0
  Ue.RecSrv.CelsiaPlu$ = ""
  Gr.RecSrv.CrdSoc% = 0
  UE.RECASRV.TipoRecSales% = 0
End Sub 


!--- Rutina General Aplicacion
Function RECASRV(UE.RECASRV.PARAMETRO%,Xdata%) PUBLIC
INTEGER*1 UE.RECASRV.PARAMETRO%,UE.RECASRV.POS%,UE.RECASRV.I%, Xdata%
STRING UE.RECASRV.CADENA$, XTMP$, XCLAVE$
Integer*4 XI%, UE.RECASRV.LENCLAVE%

!--- Ejecucion en EAMTSU01
  If UE.RECASRV.PARAMETRO% = 1 Then Begin
  EndIf
  
!--- Ejecucion en EAMTSU02
  If UE.RECASRV.PARAMETRO% = 2 Then Begin
   If UE.RECASRV.QTY% > 0 Then \
      Call PROCESO.RECIBOS.PUBLICOS
   Call Init.Proc.Recaudos
  EndIf

!--- Ejecucion en EAMTSU23
  If UE.RECASRV.PARAMETRO% = 23 Then Begin
!  	 If Mid$(TS.DISP1$,1,4) = "B026" And UE.RECASRV.QTY% > 0 Then Begin   ! Item Not found
  	 	
  	 	If Mid$(TS.DISP1$,1,4) = "B026" And Gr.RecSrv.Txn%  = -1 Then Begin   ! Item Not found Add 11May2021
  	 	   Call Init.Proc.Recaudos
  	 EndIf
  EndIf
  
!--- Ejecucion en EAMTSU07
  If UE.RECASRV.PARAMETRO% = 7 Then Begin
    UE.RECASRV.DC$ = ""
    UE.RECASRV.VL$ = ""
    UE.RECASRV.FC$ = ""
    UE.RECASRV.NT$ = ""
    UE.RECASRV.TP$ = ""
    UE.RECASRV.LISTA$ = ""
    UE.RECASRV.FLAG% = 0
    UE.RECASRV.ANUL% = 0 
    UE.RECASRV.ITM% = 0
    Ue.RecSrv.PagReserva% = 0 
    Gr.Rec.CtroCosto$ = "00000"
    TS.ER.RETURN = -1
    Open "R::$F:P0005" AS 94 UNLOCKED NOWRITE NODEL
    Dim UE.RECASRV.MASK$(100)
    Dim UE.RECASRV.DOC$(100)
    Dim UE.RECASRV.VLR$(100)
    Dim UE.RECASRV.NIT$(100)
    Dim UE.RECASRV.FECHA$(100)
    Dim UE.RECASRV.TIPO$(100)
    Dim UE.RECASRV.CLAVE$(100)
    Dim UE.RECASRV.CCOSTO$(100)
    Dim UE.RECASRV.TRX$(100)
    Dim Ue.RecSrv.AplMsg$(60)
    UE.RECASRV.CONT% = 1
    UE.RECASRV.QTY%  = 0
    Gr.REC.Acuaviva% = 0 
    Ue.RecSrv.Reserva% = 0
    Ue.RecSrv.InTrx% = 0 
    Ue.RecSim.InTrx% = 0 
    Ue.RecSrv.Emcali% = 0
    Ue.RecSrv.Device% = 0
    Gr.RecSrv.Txn%    = 0
    Ue.RecSrv.OnTrx%  = 0  
    Gr.RecSrv.NewCol% = 0
    Ue.RecSrv.Periodo$ = ""
    Ue.RecSrv.Planilla$ = ""
    Gr.RecSrv.Simple% = 0
    Gr.RecSrv.SndSmp% = 0
    Gr.RecSrv.Siga% = 0
    While TS.ER.RETURN
       SIGUIENTE.LINEA:
       Read #94;LINE UE.RECASRV.CADENA$
       If MID$(UE.RECASRV.CADENA$,1,1) = "#" Then GoTo SIGUIENTE.LINEA
       UE.RECASRV.MASK$(UE.RECASRV.CONT%) = UE.RECASRV.CADENA$
       UE.RECASRV.CONT% =  UE.RECASRV.CONT% + 1
    Wend
    Close 94
    For XI% = 1 To UE.RECASRV.CONT%				! Lista de EAN de recaudos
        UE.RECASRV.LISTA$ = UE.RECASRV.LISTA$ + "," + MID$(UE.RECASRV.MASK$(XI%),1,12) + ","
    Next XI%
    
    Dim Ue.RecSrv.Msg$(50,15)                                             ! Maximo 50 mensajes 15 lineas
    
    TS.ER.RETURN = -1 
    Open "R::$F:MSGREC" AS 94 UNLOCKED NOWRITE NODEL									    ! Apertura archivo formato mensajes
    If TS.ER.RETURN = -1 THEN BEGIN 
        K% = 0																														!
        If End #94 Then UE.RECSRV.MSGLINEA	     												  ! Si es EOF
        While (1)																													!
	        Read #94; A$ 				      																			! Lectura registro hasta eof()
	        If Left$(A$,1) = "<" Then Begin																	! Carga los mensajes 
		         I% = Match(">",A$,1)																				  ! que van a utilizar 
		         J% = Val(MID$(A$,2,I%-2))																		! c/u de los recaudos de
		         K% = 1																												! terceros
	        Endif Else Begin																								! Contenido del mensaje
           If J% <= 50 Then Begin                                         ! Maximo 50 mensajes
		         Ue.RecSrv.Msg$ (J%,K%) = A$																	! Almacena linea de mensaje
		         If K% < 11 Then                                             \! Hasta un maximo de 10 lineas por mensaje
		            Ue.RecSrv.Msg$ (J%,0) = STR$(K%)													! Almacena numero de lineas que tiene el mensaje
		         K% = K% + 1																									!
           EndIf
	        EndIf																														!
        Wend																															!         
    EndIf Else Begin
      Call U.Imprime("MENSAJES RECAUDOS NO CARGADOS",6100H)
    EndIF
    UE.RECSRV.MSGLINEA:
       Close 94
    
    TS.ER.RETURN = -1																													! Ctrl Errores                     
    OPEN "R::$SCNTRL" AS 94 UNLOCKED NOWRITE NODEL														! Apertura archivo parametrizacion 
    IF END #94 THEN UE.FIN.SIMPLE		       																	  ! Si es EOF                        
    While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																				! Lectura registro                 
        IF TS.TEMP1$ = "[RECAUDO SIMPLE]" Then Begin			   			   					! Recaudo Simple
         Read #94; TS.TEMP1$																									! Lectura registro                 
         UE.RECASRV.SIMPLE$ = Mid$(TS.TEMP1$,30,12)   				    						! PLU de registro
         Read #94; TS.TEMP1$     																						  ! Lectura registro                 
         UE.RECASRV.SIMPLEPRO$ = Mid$(TS.TEMP1$,30,8)              				    ! Numero de proyecto
         GoTo UE.FIN.SIMPLE																									  ! Sale del ciclo de carga          
       Endif                                                                                                     
     Wend                                                                                                        
     UE.FIN.SIMPLE:                                                                                               
       Close 94																																! Cierra archivo                   
   
    TS.ER.RETURN = -1																													! Ctrl Errores                     
    OPEN "R::$SCNTRL" AS 94 UNLOCKED NOWRITE NODEL														! Apertura archivo parametrizacion 
    IF END #94 THEN UE.FIN.RESERVA		       																  ! Si es EOF                        
    While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																				! Lectura registro                 
        IF TS.TEMP1$ = "[RECAUDO RESERVAS]" Then Begin												! Recaudo reservas hoteleras
         Read #94; TS.TEMP1$																									! Lectura registro                 
         UE.RECASRV.RESERV$ = Mid$(TS.TEMP1$,30,12)   	        							! PLU de registro
         Read #94; TS.TEMP1$     																							! Lectura registro                 
         UE.RECASRV.RESERVPRO$ = Mid$(TS.TEMP1$,30,8)           							! Numero de proyecto
         Read #94; TS.TEMP1$     																							! Lectura registro                 
         UE.RECASRV.RESERVMOT$ = Mid$(TS.TEMP1$,30,3)           							! Numero de proyecto
         Read #94; TS.TEMP1$     																							! Lectura registro                 
         UE.RECASRV.RESERVPAG$ = Mid$(TS.TEMP1$,30,50)           							! Formas de pago validas
         Read #94; TS.TEMP1$     																							! Lectura registro                 
         Ue.RecSrv.AplMsgRsv$  = Mid$(TS.TEMP1$,30,01)           							! Aplica Mensaje 1.Si 0. No
         Read #94; TS.TEMP1$     																							! Lectura registro                 
         Ue.RecSrv.NumMsgRsv$  = Mid$(TS.TEMP1$,30,02)           							! Numero mensaje aplicar
         Read #94; TS.TEMP1$     																							! Lectura registro                 
         Ue.RecSrv.FiscalRsv$  = Mid$(TS.TEMP1$,30,01)           							! Si aplica numeracion fiscal
         Read #94; TS.TEMP1$     																							! Genera tiquete recaudo
         Read #94; TS.TEMP1$     																							! Lectura registro                 
         UE.RECASRV.FOMENTO$   = Mid$(TS.TEMP1$,30,12)           							! PLU registro fomento

         UE.RECASRV.UNICA%     = -1                             							! Transaccion Exclusiva
         GoTo UE.FIN.RESERVA																									! Sale del ciclo de carga          
       Endif                                                                                                     
     Wend                                                                                                        
     UE.FIN.RESERVA:                                                                                               
       Close 94																																! Cierra archivo                   

    TS.ER.RETURN = -1																													! Ctrl Errores                     
    OPEN "R::$SCNTRL" AS 94 UNLOCKED NOWRITE NODEL														! Apertura archivo parametrizacion 
    IF END #94 THEN UE.FIN.ACUAVI		       																	  ! Si es EOF                        
    While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																				! Lectura registro                 
        IF TS.TEMP1$ = "[RECAUDO ACUAVIVA]" Then Begin		                    ! Recaudo Simple
         Read #94; TS.TEMP1$					                                        ! Lectura registro                 
         UE.RECASRV.ACUAVI$ = Mid$(TS.TEMP1$,30,12)   	                      ! PLU de registro
         Read #94; TS.TEMP1$     				                                      ! Lectura registro                 
         UE.RECASRV.ACUAVIPRO$ = Mid$(TS.TEMP1$,30,8)                         ! Numero de proyecto
         Read #94; TS.TEMP1$     				                                      ! Lectura registro                 
         UE.RECASRV.ACUAVINIT$ = Mid$(TS.TEMP1$,30,10)                        ! Numero de NIT
         Read #94; TS.TEMP1$     																							! Lectura registro                 
         Ue.RecSrv.AplMsgAcv$  = Mid$(TS.TEMP1$,30,01)           							! Aplica Mensaje 1.Si 0. No
         Read #94; TS.TEMP1$     																							! Lectura registro                 
         Ue.RecSrv.NumMsgAcv$  = Mid$(TS.TEMP1$,30,02)           							! Numero mensaje aplicar
         Read #94; TS.TEMP1$     																							! Lectura registro                 
         Ue.RecSrv.FiscalAcv$  = Mid$(TS.TEMP1$,30,01)           							! Si aplica numeracion fiscal
         GoTo UE.FIN.ACUAVI																									  ! Sale del ciclo de carga          
       Endif                                                                                                     
     Wend                                                                                                        
     UE.FIN.ACUAVI:                                                                                               
       Close 94																																! Cierra archivo                   

    !Call U.IMPRIME("MODULO RECAUDO TERCERO ON 29/Nov/2021",2100H)
    !Call U.IMPRIME("MODULO RECAUDO TERCERO ON 05/May/2023",2100H)
     Call U.IMPRIME("MODULO RECAUDO TERCERO ON 21/Mar/2025",2100H)
  EndIf
  
!--- Ejecucion en EAMTSU08
  If UE.RECASRV.PARAMETRO% = 8 Then Begin

!-- Validar si esta en proceso de recaudo cambiar flag de permitir venta producto

      If UE.RECASRV.QTY% > 0 Then Begin													! Ya hay recaudos registrado
      	 Call VISOR.AND.BORRAR("EXISTE UN RECAUDO EN PROCESO    /Borrar")
         IR.INDICAT0 = IR.INDICAT0 OR 04H			                	! No permite venta
         Gr.RecSrv.Txn% = 0                                    	! Cancela proceso de recaudo
         UE.RECASRV.ITM% = 0
         Exit Function 
      EndIf
   
      If TS.INTRX Then Begin 																		! Dentro de una trx    
       If Gr.RecSrv.Txn% = -1 Then Begin 												! Registrando un recaudo
       	If Ue.RecSrv.OnTrx% = 0 Then Begin                      ! No hay recaudos antes registrados
          Call VISOR.AND.BORRAR("NO SE PERMITE MEZCLADE RECAUDO Y ARTIC3")
          IR.INDICAT0 = IR.INDICAT0 OR 04H			                ! No permite venta
          Gr.RecSrv.Txn% = 0                                    ! Cancela proceso de recaudo
          UE.RECASRV.ITM% = 0
          Exit Function 
       	EndIf
       EndIf	
      EndIf																											!

      If TS.INTRX Then Begin 																		! Dentro de una trx    
       If Gr.RecSrv.Txn% = 0 Then Begin 												! Registrando un no recaudo
       	If UE.RECASRV.QTY% > 0 Then Begin                       ! Si hay recaudos antes registrados Mod 11nov2015
          Call VISOR.AND.BORRAR("NO SE PERMITE MEZCLADE RECAUDO Y ARTIC7")
          IR.INDICAT0 = IR.INDICAT0 OR 04H			                ! No permite venta
          Gr.RecSrv.Txn% = 0                                    ! Cancela proceso de recaudo
          UE.RECASRV.ITM% = 0
          Exit Function 
       	EndIf
       EndIf	
      EndIf																											!

      If Gr.RecSrv.Txn% = -1 Then Begin 												! Registrando un recaudo
      	 IR.INDICAT0 = 0                       									! Permite la venta del producto
      EndIf																											! 
     
      IF (IR.INDICAT0 AND 04H) NE 0   THEN          	         \! item not for sale
          Exit Function 																				! Sale el proceso	

     If (Val(SL.IT.ITEMCODE$) = Val(UE.RECASRV.SIMPLE$)) And   \! Si registra PLU recaudo simple
        (UE.RECASRV.ITM% <> -5) Then Begin                      ! Pero no por modulo recaudo
          IR.INDICAT0 = IR.INDICAT0 OR 04H			                ! No permite venta
          Gr.RecSrv.Txn% = 0                                    ! Cancela proceso de recaudo
          Exit Function 
     EndIf
 
     If Ue.RecSrv.InTrx% = -1 Then Begin                        ! Si reserva capturada
          Call VISOR.AND.BORRAR("NO SE PERMITE MEZCLADE RECAUDO Y ARTIC1")
          IR.INDICAT0 = IR.INDICAT0 OR 04H			                ! No permite venta
          Gr.RecSrv.Txn% = 0                                    ! Cancela proceso de recaudo
          UE.RECASRV.ITM% = 0
          Exit Function 
     EndIf  

     If Ue.RecSim.InTrx% = -1 Then Begin                        ! Si recaudo simple capturado
     	    Call VISOR.AND.BORRAR("NO SE PERMITE MEZCLARECAUDOS Y/O ARTIC.")
          Call VISOR.AND.BORRAR("SOLO UN RECAUDO POR TRANSACCION")
          IR.INDICAT0 = IR.INDICAT0 OR 04H			                ! No permite venta
          Gr.RecSrv.Txn% = 0                                    ! Cancela proceso de recaudo
          UE.RECASRV.ITM% = 0
          Exit Function 
     EndIf  

     If UE.RECASRV.ITM% = -1 Then Begin 
       GR.TEMP1I4 = ASIGNA.DATOS.DOCUMENTO(UE.RECASRV.CODE$)
       If GR.TEMP1I4 <> -1 Then Begin 
           UE.RECASRV.FLAG% = 0                                                 
           UE.RECASRV.ITM% = 0                                                  
           UE.RECASRV.CODE$ = ""
           Gr.RecSrv.Simple% = 0
           TS.IO.STATE    = 10            
           IR.INDICAT0 = IR.INDICAT0 OR 04H			                 ! No permite venta
           Gr.RecSrv.Txn% = 0                                    ! Cancela proceso de recaudo
           UE.RECASRV.ITM% = 0
          Exit Function 
       EndIf Else Begin
       	If Val(UE.RECASRV.VL$) < 50 Then Begin                  ! Error valor recaudo
       		 Call VISOR.AND.BORRAR("ERROR EN VALOR DEL RECAUDO")
           UE.RECASRV.FLAG% = 0                                                 
           UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1                                
           UE.RECASRV.ITM% = 0                                                  
           UE.RECASRV.CODE$ = ""
           Gr.RecSrv.Simple% = 0
           TS.IO.STATE    = 10            
           IR.INDICAT0 = IR.INDICAT0 OR 04H			                 ! No permite venta
           Gr.RecSrv.Txn% = 0                                    ! Cancela proceso de recaudo
           UE.RECASRV.ITM% = 0
           Exit Function 
       	EndIf
       EndIf
     EndIf 
     
     If UE.RECASRV.ITM% = -2 Then Begin 
        GR.TEMP1I4 = ASIGNA.DATOS.MANUAL
        If GR.TEMP1I4 <> -1 Then Begin                            
           UE.RECASRV.FLAG% = 0                                                 
           UE.RECASRV.ITM% = 0                                                  
           UE.RECASRV.CODE$ = ""
           Gr.RecSrv.Simple% = 0
           TS.IO.STATE    = 10            
           IR.INDICAT0 = IR.INDICAT0 OR 04H			                 ! No permite venta
           Gr.RecSrv.Txn% = 0                                    ! Cancela proceso de recaudo
           UE.RECASRV.ITM% = 0
          Exit Function 
        EndIf Else Begin
        	If Val(UE.RECASRV.VL$) < 50 Then Begin                  ! Error valor recaudo
       		 Call VISOR.AND.BORRAR("ERROR EN VALOR DEL RECAUDO")
           UE.RECASRV.FLAG% = 0                                                 
           UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1                                
           UE.RECASRV.ITM% = 0                                                  
           UE.RECASRV.CODE$ = ""
           Gr.RecSrv.Simple% = 0
           TS.IO.STATE    = 10            
           IR.INDICAT0 = IR.INDICAT0 OR 04H			                 ! No permite venta
           Gr.RecSrv.Txn% = 0                                    ! Cancela proceso de recaudo
           UE.RECASRV.ITM% = 0
           Exit Function 
        	EndIf
        EndIf
     EndIf 

     If UE.RECASRV.ITM% = -5 Then Begin 
        GR.TEMP1I4 = ASIGNA.DATOS.SIMPLE
       If GR.TEMP1I4 <> -1 Then Begin 
           UE.RECASRV.FLAG% = 0                                                 
           UE.RECASRV.ITM% = 0                                                  
           UE.RECASRV.CODE$ = ""
           Gr.RecSrv.Simple% = 0
           TS.IO.STATE    = 10            
           IR.INDICAT0 = IR.INDICAT0 OR 04H			                 ! No permite venta
           Gr.RecSrv.Txn% = 0                                    ! Cancela proceso de recaudo
           UE.RECASRV.ITM% = 0
          Exit Function 
       EndIf Else Begin 
       	If Val(UE.RECASRV.VL$) < 50 Then Begin                  ! Error valor recaudo
       		 Call VISOR.AND.BORRAR("ERROR EN VALOR DEL RECAUDO")
           UE.RECASRV.FLAG% = 0                                                 
           UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1                                
           UE.RECASRV.ITM% = 0                                                  
           UE.RECASRV.CODE$ = ""
           Gr.RecSrv.Simple% = 0
           TS.IO.STATE    = 10            
           IR.INDICAT0 = IR.INDICAT0 OR 04H			                 ! No permite venta
           Gr.RecSrv.Txn% = 0                                    ! Cancela proceso de recaudo
           UE.RECASRV.ITM% = 0
           Exit Function 
       	EndIf
       EndIf
     EndIf 


!--- MOD 05ENE2009
     If UE.RECASRV.ITM% = -6 Then Begin
       GR.TEMP1I4 = ASIGNA.DATOS.RESERVA
       If GR.TEMP1I4 <> -1 Then Begin 
           UE.RECASRV.FLAG% = 0                                                 
           UE.RECASRV.ITM% = 0                                                  
           UE.RECASRV.CODE$ = ""
           Gr.RecSrv.Simple% = 0
           TS.IO.STATE    = 10            
           IR.INDICAT0 = IR.INDICAT0 OR 04H			                 ! No permite venta
           Gr.RecSrv.Txn% = 0                                    ! Cancela proceso de recaudo
           UE.RECASRV.ITM% = 0
          Exit Function 
       EndIf Else Begin 
       	If Val(UE.RECASRV.VL$) < 50 Then Begin                  ! Error valor recaudo
       		 Call VISOR.AND.BORRAR("ERROR EN VALOR DEL RECAUDO")
           UE.RECASRV.FLAG% = 0                                                 
           UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1                                
           UE.RECASRV.ITM% = 0                                                  
           UE.RECASRV.CODE$ = ""
           Gr.RecSrv.Simple% = 0
           TS.IO.STATE    = 10            
           IR.INDICAT0 = IR.INDICAT0 OR 04H			                 ! No permite venta
           Gr.RecSrv.Txn% = 0                                    ! Cancela proceso de recaudo
           UE.RECASRV.ITM% = 0
           Exit Function 
       	EndIf
       EndIf
     EndIf 

!--- MOD 16FEB2009
     If UE.RECASRV.ITM% = -7 Then Begin 
       GR.TEMP1I4 = ASIGNA.DATOS.ACUAVIVA
       If GR.TEMP1I4 <> -1 Then Begin 
           UE.RECASRV.FLAG% = 0                                                 
           UE.RECASRV.ITM% = 0                                                  
           UE.RECASRV.CODE$ = ""
           Gr.RecSrv.Simple% = 0
           TS.IO.STATE    = 10            
           IR.INDICAT0 = IR.INDICAT0 OR 04H			                 ! No permite venta
           Gr.RecSrv.Txn% = 0                                    ! Cancela proceso de recaudo
           UE.RECASRV.ITM% = 0
          Exit Function 
       EndIf Else Begin
       	If Val(UE.RECASRV.VL$) < 50 Then Begin                  ! Error valor recaudo
       		 Call VISOR.AND.BORRAR("ERROR EN VALOR DEL RECAUDO")
           UE.RECASRV.FLAG% = 0                                                 
           UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1                                
           UE.RECASRV.ITM% = 0                                                  
           UE.RECASRV.CODE$ = ""
           Gr.RecSrv.Simple% = 0
           TS.IO.STATE    = 10            
           IR.INDICAT0 = IR.INDICAT0 OR 04H			                 ! No permite venta
           Gr.RecSrv.Txn% = 0                                    ! Cancela proceso de recaudo
           UE.RECASRV.ITM% = 0
           Exit Function 
       	EndIf
       EndIf
     EndIf 
     If Gr.RecSrv.Simple% = -1 Then Begin									       ! Es un recaudo simple - GR 22 Sept 2015
     	  IR.ITEMNAME$ = Left$(IR.ITEMNAME$,09)+"-"+Ue.RecSrv.Periodo$
     	  IR.ITEMNAME$ = Left$(IR.ITEMNAME$ + String$(18," "),18)
     EndIf 
  EndIf

!--- Ejecucion en EAMTSU14
  If UE.RECASRV.PARAMETRO% = 14 Then Begin
     
!--- Add 21Mzo2025     
    If (UE.RECASRV.Recup% = -1 And TS.IO.MOTORKEY > 90) Then Begin          !
        TS.GUIDANCE = 1020																									! Obliga totalizar
        TS.IO.MOTORKEY = 0																									!
        Exit Function 																											!
     EndIf																																	!

     If TS.IO.MOTORKEY = 81 And UE.RECASRV.Recup% = -1 Then Begin						! Total y en recuperacion
     	  Call TSHIECET
     	  Call VISOR.AND.BORRAR("EXISTE UN RECAUDO EN PROCESO    /Borrar")		! Msg de alerta
     	  Call TSHIECET
     	  Call VISOR.AND.BORRAR("ANULANDO MOVIMENTO  REGISTRE RECAUDO   ")		! Msg de alerta
     	  Call TSHIECET
        Call VISOR.AND.BORRAR("NUEVAMENTE TRANSAC  NO EXITOSA  /Borrar")		! Msg de alerta
        Call Gr.Init.Trx																										! Init Trx
        TS.IO.STATE    = 10            																  		! PREPARE FOR NORMAL SALES STATE
        Exit Function 																											! Sale del procedimiento
     EndIf																																	! Fin proceso recuperacion
     
     If TS.IO.MOTORKEY = 248 Then Begin																			! Recaudo credito social
       If UE.RECASRV.QTY% > 0 Then Begin																		! Ya hay recaudos registrado
      	 Call VISOR.AND.BORRAR("EXISTE UN RECAUDO EN PROCESO    /Borrar")		! Msg de alerta
         Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)                						!
         TS.IO.MOTORKEY = 73 																								!
         Exit Function 																											!
       EndIf																																!
       TS.IO.DATA$(2) = "7701015999930"																			! Registra EAN credito social
       TS.IO.KEYS(2)  = 80																									!
       TS.IO.MOTORKEY = 80																									!
     EndIf																																	! Fin recaudo manual credito social
    
     If (UE.RECASRV.QTY% > 0) Then 											                   \! Si recaudo en tramite
     	If (TS.IO.MOTORKEY = Asc.Pay.Motora%) Then Begin                      ! Si tecla pago PES
     		 Call VISOR.AND.BORRAR("NO SE PERMITE PAGO  CON SUBSIDIO")          !
         Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)                						!
         TS.IO.MOTORKEY = 73 																								!
         Exit Function 																											!
     	EndIf																																	!
	   
	   If (UE.RECASRV.QTY% > 0) Then 																			 	 \! Si recaudo en tramite
      If TS.IO.KEYS(1) = 82 AND \                                           ! Proceso de suspension de
         TS.IO.KEYS(6) = 81 Then Begin                                      ! Capturada
	 	  	 Call VISOR.AND.BORRAR("PROCESO NO PERMITIDO PARA RECAUDOS")        ! Msg Alerta
         Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)                						!
         TS.IO.MOTORKEY = 73 																								!
         Exit Function 																											!
      EndIf																																	! Fin suspension
      
     If Gr.RecSrv.Simple% = -1 Then Begin																    ! Es un recaudo simple - GR 22 Sept 2015
       If TS.IO.KEYS(7) > 90 AND TS.IO.KEYS(7) < 97 Then Begin              ! Si forma de pago 
       	  If (TS.IO.KEYS(1) = 70) Then Exit Function                        ! Si es anulacion del pago
       		If TS.IO.DATA$(7) = "" Then Begin 																! Si no entra valor recaudado
       			 Call VISOR.AND.BORRAR("INGRESE MONTO DE LA FORMA DE PAGO  ")   ! Msg alerta cajero
                Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)                     !
                TS.IO.MOTORKEY = 73 																				!
                Exit Function 																							!
       	  EndIf																															!
       	  TS.TEMP4I4 = Val(TS.IO.DATA$(7))                                  ! Toma valor capturado
          TS.TEMP5I4 = TS.BALDUE(0) - TS.TEMP4I4
     		  If (TS.TEMP5I4) <= 0 Then Begin											              ! Si cerrando trx venta
     		  	 If (TS.TEMP5I4 * -1) > 50000 Then Begin                        !
     		  	 	  Call VISOR.AND.BORRAR("SOBREPASA EL LIMITE CAMBIO AUTOR. /Borra")
                Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)                     !
                TS.IO.MOTORKEY = 73 																				!
                Exit Function 																							!
     		  	 EndIf
     			   TS.ER.RETURN = -1 																							! Control del proceso
     			   Call ACT.REC.SIMPLE																					  ! Actualizacion de la planilla
     			   If TS.ER.RETURN <> -1 Then Begin 															! Sin Error 
                Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)                     !
                TS.IO.MOTORKEY = 73 																				!
                Exit Function 																							!
     			   EndIf
     	    EndIf 																														! Fin si cerrando trx venta
       EndIf  		 																													! Fin de un pago
     EndIf																																	! fin validacion recaudo simple

!--- Captura recaudo AcuaViva
    If Val(TS.IO.DATA$(2)) = Val(UE.RECASRV.ACUAVI$) Then Begin

       If UE.RECASRV.QTY% > 0 Then Begin													! Ya hay recaudos registrado
      	  Call VISOR.AND.BORRAR("EXISTE UN RECAUDO EN PROCESO    /Borrar")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf

    	 If Gr.Mrm.Cap% = -1 Then Begin
   	      Call VISOR.AND.BORRAR("PROCESO DE RECAUDO  NO VALIDO EN REMISION")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          Dim TS.IO.PREV.DATA$(10) : Dim TS.IO.PREV.KEYS(10)
          USER.VERIFY.ITEM = 0
          UE.RECASRV.FLAG% = 0
          TS.ENTRY.POS = 0
          USER.SAVE.ITEM$ = ""
          TS.IO.MOTORKEY = 73
          Exit Function 
    	 EndIf

       If (TS.IO.KEYS(6) = 75) Then Begin																		! Si tecla consulta precio
   	      Call VISOR.AND.BORRAR("PROCESO DE CONSULTA NO VALIDO EN RECAUDO")
   	      Call VISOR.AND.BORRAR("PROCESO RECAUDO CANCELADO")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          Dim TS.IO.PREV.DATA$(10) : Dim TS.IO.PREV.KEYS(10)
          USER.VERIFY.ITEM = 0
          UE.RECASRV.FLAG% = 0
          TS.ENTRY.POS = 0
          USER.SAVE.ITEM$ = ""
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf
    	  
       If (TE.TR.AMT(1) + 50000) >= To.CDLIMIT2 Then Begin    ! SOBREPASA LIMITE CAJON MONEDERO
   	      Call VISOR.AND.BORRAR("REALICE PROCESO DE RECOGIDA CAJERO AHORA")
   	      Call VISOR.AND.BORRAR("PROCESO RECAUDO CANCELADO")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf 	
    	
       If ((TS.IO.KEYS(1) = 70) OR (TS.IO.KEYS(8) = 67)) Then Begin 			! Si anulacion del moviento
          Call VISOR.AND.BORRAR("NO SE PERMITE ANUL. RECIBO, ANULE TRX")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf 
       If TS.STANDALONE Then Begin 					
          Call VISOR.AND.BORRAR("PROCESO NO PERMITIDO  FUERA DE LINEA")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf 

       TS.TEMP1$ = ASIC.DATOS$("NUMERO RECIBO?","")	  	! Ingresa numero recibo
       If (TS.TEMP1$ = "") OR (TS.TEMP1$ = "E") Then Begin	! No se capturo dato
          Call VISOR.AND.BORRAR("PROCESO CANCELADO POR USUARIO")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf		
       UE.RECASRV.CODE$ = TS.TEMP1$
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
       TS.IO.DATA$(2) = Right$("000000000000"+Str$(Val(UE.RECASRV.ACUAVI$)),12)
       TS.IO.KEYS(2)  = 80
       TS.IO.MOTORKEY = 80
       TS.IO.DEVICE   = 1
       UE.RECASRV.PLU$ = Str$(Val(UE.RECASRV.ACUAVI$))
       UE.RECASRV.ITM% = -7																								! Reporta recaudo Simple
       Gr.RecSrv.Txn% = -1
       Exit Function 
    EndIf 
!--- Fin recaudo AcuaViva


!--- Captura recaudo Reservas Hoteleras 
    If TS.IO.MOTORKEY = Val(UE.RECASRV.RESERVMOT$) Then Begin

       If UE.RECASRV.QTY% > 0 Then Begin													! Ya hay recaudos registrado
      	  Call VISOR.AND.BORRAR("EXISTE UN RECAUDO EN PROCESO    /Borrar")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf

    	 If Gr.Mrm.Cap% = -1 Then Begin
   	      Call VISOR.AND.BORRAR("PROCESO DE RECAUDO  NO VALIDO EN REMISION")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          Dim TS.IO.PREV.DATA$(10) : Dim TS.IO.PREV.KEYS(10)
          USER.VERIFY.ITEM = 0
          UE.RECASRV.FLAG% = 0
          TS.ENTRY.POS = 0
          USER.SAVE.ITEM$ = ""
          TS.IO.MOTORKEY = 73
          Exit Function 
    	 EndIf

       If (TS.IO.KEYS(6) = 75) Then Begin																		! Si tecla consulta precio
   	      Call VISOR.AND.BORRAR("PROCESO DE CONSULTA NO VALIDO EN RECAUDO")
   	      Call VISOR.AND.BORRAR("PROCESO RECAUDO CANCELADO")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          Dim TS.IO.PREV.DATA$(10) : Dim TS.IO.PREV.KEYS(10)
          USER.VERIFY.ITEM = 0
          UE.RECASRV.FLAG% = 0
          TS.ENTRY.POS = 0
          USER.SAVE.ITEM$ = ""
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf

       If (TE.TR.AMT(1) + 50000) >= To.CDLIMIT2 Then Begin    ! SOBREPASA LIMITE CAJON MONEDERO
   	      Call VISOR.AND.BORRAR("REALICE PROCESO DE RECOGIDA CAJERO AHORA")
   	      Call VISOR.AND.BORRAR("PROCESO RECAUDO CANCELADO")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf 	
    	
       If ((TS.IO.KEYS(1) = 70) OR (TS.IO.KEYS(8) = 67)) Then Begin 			! Si anulacion del moviento
          Call VISOR.AND.BORRAR("NO SE PERMITE ANUL. RECIBO, ANULE TRX")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf 

       If TS.STANDALONE Then Begin 					
          Call VISOR.AND.BORRAR("PROCESO NO PERMITIDO  FUERA DE LINEA")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf 

       If UE.RECASRV.UNICA% = -1 And TS.INTRX Then Begin 
          Call VISOR.AND.BORRAR("NO SE PERMITE MEZCLADE RECAUDO Y ARTIC2")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf 
       TIPO.REC.RESERVA:
!-- Lineas comentareadas 23 Sept 2016, no esta claro requerimiento
!-- Dialogo liberado 11 ene 2017 - solicitud comfandi

       !TS.TEMP1$ = ASIC.DATOS$("1.RESERVA HOTELERA","2.RECAUDO FOMENTO")	  	! Ingresa numero de reserva
       TS.TEMP1$ = ASIC.DATOS$("1.RESERVA ZEUS    ","2.RESERVA SALEFORCE")	 	! Ingresa numero de reserva
       If (TS.TEMP1$ = "") OR (TS.TEMP1$ = "E") Then Begin										! No se capturo dato
          Call VISOR.AND.BORRAR("PROCESO CANCELADO POR USUARIO")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf					     
       If Match(TS.TEMP1$," 1 2 ",1) <= 0 Then Begin												! 
       	  Call VISOR.AND.BORRAR("OPCION NO VALIDA             ")
          GoTo TIPO.REC.RESERVA
       EndIf

!-- Lineas adicionadas       
       
!       TS.TEMP1$ = "1"																											! Reporta defualt reserva hotelera commnet 11 ene 2017

       UE.RECASRV.TipoRecSales% = Val(TS.TEMP1$)														! Tipo de recaudo
       If  UE.RECASRV.TipoRecSales% = 1 Then 													     \!
           TS.TEMP1$ = ASIC.DATOS$("#   RESERVA ZEUS?   ","")	Else         \! Ingresa numero de reserva
           TS.TEMP1$ = ASIC.DATOS$("# RESERVA SALEFORCE?","")	              ! Numero de fomento
       If (TS.TEMP1$ = "") OR (TS.TEMP1$ = "E") Then Begin	! No se capturo dato
          Call VISOR.AND.BORRAR("PROCESO CANCELADO POR USUARIO")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf					     

       UE.RECASRV.CODE$ = TS.TEMP1$
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)

!       If  UE.RECASRV.TipoRecSales% = 1 Then 													     \!
!           TS.IO.DATA$(2) = Right$("000000000000"+Str$(Val(UE.RECASRV.RESERV$)),12)
!       If  UE.RECASRV.TipoRecSales% = 2 Then 													     \!
!           TS.IO.DATA$(2) = Right$("000000000000"+Str$(Val(UE.RECASRV.FOMENTO$)),12)

       TS.IO.DATA$(2) = Right$("000000000000"+Str$(Val(UE.RECASRV.RESERV$)),12)
       TS.IO.KEYS(2)  = 80
       TS.IO.MOTORKEY = 80
       TS.IO.DEVICE   = 1
       !UE.RECASRV.PLU$ = Str$(Val(UE.RECASRV.RESERV$))
       
       UE.RECASRV.PLU$ = Str$(Val(TS.IO.DATA$(2)))
       UE.RECASRV.ITM% = -6								! Reporta recaudo reservas
       Gr.RecSrv.Txn% = -1
       Exit Function 
    EndIf 
!--- Fin recaudo reservas hoteleras

!--- Captura recaudo Simple    
    If LEN(TS.IO.DATA$(2)) = 25 And \																			! Recaudo Simple
       Left$(TS.IO.DATA$(2),2) = "88" Then Begin

   	   Call VISOR.AND.BORRAR("FORMATO DE RECIBO  NO VALIDO PARA SIMPLE")
       Call VISOR.AND.BORRAR("FAVOR ESCANEAR NUEVO CODIGO DE BARRAS  ")
       Call VISOR.AND.BORRAR("PROCESO RECAUDO CANCELADO")
       Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
       TS.IO.MOTORKEY = 73
       Exit Function 

       If (TS.IO.KEYS(6) = 75) Then Begin																		! Si tecla consulta precio
   	      Call VISOR.AND.BORRAR("PROCESO DE CONSULTA NO VALIDO EN RECAUDO")
   	      Call VISOR.AND.BORRAR("PROCESO RECAUDO CANCELADO")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          Dim TS.IO.PREV.DATA$(10) : Dim TS.IO.PREV.KEYS(10)
          USER.VERIFY.ITEM = 0
          UE.RECASRV.FLAG% = 0
          TS.ENTRY.POS = 0
          USER.SAVE.ITEM$ = ""
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf

       If (TE.TR.AMT(1) + 50000) >= To.CDLIMIT2 Then Begin    ! SOBREPASA LIMITE CAJON MONEDERO
   	      Call VISOR.AND.BORRAR("REALICE PROCESO DE RECOGIDA CAJERO AHORA")
   	      Call VISOR.AND.BORRAR("PROCESO RECAUDO CANCELADO")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf 	

       	
       If ((TS.IO.KEYS(1) = 70) OR (TS.IO.KEYS(8) = 67)) Then Begin 			! Si anulacion del moviento
          Call VISOR.AND.BORRAR("NO SE PERMITE ANUL. RECIBO, ANULE TRX")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf 
       If TS.STANDALONE Then Begin 					
          Call VISOR.AND.BORRAR("PROCESO NO PERMITIDO  FUERA DE LINEA")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf 
    
     If UE.RECASRV.UNICA% = -1 And TS.INTRX Then Begin 
        Call VISOR.AND.BORRAR("NO SE PERMITE MEZCLADE RECAUDO Y ARTIC4")
        Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
        TS.IO.MOTORKEY = 73
        Exit Function 
     EndIf 
    
!-- Mod 15Feb2011       

       If TS.IO.DEVICE <> 3 Then Begin																		! Si no Escaneado
          Call VISOR.AND.BORRAR("NO SE PERMITE CAPTURA DIGITADA RECAUDO.")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf
       
       UE.RECASRV.CODE$ = TS.IO.DATA$(2)
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
       TS.IO.DATA$(2) = Right$("000000000000"+Str$(Val(UE.RECASRV.SIMPLE$)),12)
       TS.IO.KEYS(2)  = 80
       TS.IO.MOTORKEY = 80
       TS.IO.DEVICE   = 1
       UE.RECASRV.PLU$ = Str$(Val(UE.RECASRV.SIMPLE$))
       UE.RECASRV.ITM% = -5																								! Reporta recaudo Simple
       Gr.RecSrv.Txn%  = -1
       Exit Function 
    EndIf 
    
!--- Captura recaudo escaneado    
    If LEN(TS.IO.DATA$(2)) > 14 And \
       Left$(TS.IO.DATA$(2),3) = "415" Then Begin

       If UE.RECASRV.QTY% > 0 Then Begin													! Ya hay recaudos registrado
      	  Call VISOR.AND.BORRAR("EXISTE UN RECAUDO EN PROCESO    /Borrar")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf

    	 If Gr.Mrm.Cap% = -1 Then Begin
   	      Call VISOR.AND.BORRAR("PROCESO DE RECAUDO  NO VALIDO EN REMISION")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          Dim TS.IO.PREV.DATA$(10) : Dim TS.IO.PREV.KEYS(10)
          USER.VERIFY.ITEM = 0
          UE.RECASRV.FLAG% = 0
          TS.ENTRY.POS = 0
          USER.SAVE.ITEM$ = ""
          TS.IO.MOTORKEY = 73
          Exit Function 
    	 EndIf

       If (TS.IO.KEYS(6) = 75) Then Begin																		! Si tecla consulta precio
   	      Call VISOR.AND.BORRAR("PROCESO DE CONSULTA NO VALIDO EN RECAUDO")
   	      Call VISOR.AND.BORRAR("PROCESO RECAUDO CANCELADO")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          Dim TS.IO.PREV.DATA$(10) : Dim TS.IO.PREV.KEYS(10)
          USER.VERIFY.ITEM = 0
          UE.RECASRV.FLAG% = 0
          TS.ENTRY.POS = 0
          USER.SAVE.ITEM$ = ""
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf
       
       If (TE.TR.AMT(1) + 50000) >= To.CDLIMIT2 Then Begin    ! SOBREPASA LIMITE CAJON MONEDERO
   	      Call VISOR.AND.BORRAR("REALICE PROCESO DE RECOGIDA CAJERO AHORA")
   	      Call VISOR.AND.BORRAR("PROCESO RECAUDO CANCELADO")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf 	
       	
       If ((TS.IO.KEYS(1) = 70) OR (TS.IO.KEYS(8) = 67)) Then Begin 			! Si anulacion del moviento
          Call VISOR.AND.BORRAR("NO SE PERMITE ANUL. RECIBO, ANULE TRX")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf 
       If TS.STANDALONE Then Begin 					
          Call VISOR.AND.BORRAR("PROCESO NO PERMITIDO  FUERA DE LINEA")
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
          TS.IO.MOTORKEY = 73
          Exit Function 
       EndIf 
       A$ = CHR$(13)+CHR$(10)
       C$ = "":B$ = ""
       UE.RECASRV.CODE$ = ""
       TS.ER.RETURN = -1 
       
       !TS.IO.DATA$(2) = "4157709998005976802026306012000110390000000661809620210416"
       !TS.IO.DATA$(2) = "41577099980059768020741038694868390000000535509620201230"

       For I% = 1 TO LEN(TS.IO.DATA$(2))
           B$ = MID$(TS.IO.DATA$(2),I%,1)
           If ASC(B$) <> 29 Then C$ = C$ + B$
       Next I%
       UE.RECASRV.CODE$ = C$
!--- Add 11May2021
       Ue.RecSrv.LenCelsia% = 0
       If MID$(UE.RECASRV.CODE$,4,12) = "770720821001" Then Begin						! Recaudo Celsia
       	  Ue.RecSrv.LenCelsia% = Len(UE.RECASRV.CODE$)                      ! Longitud de la cadena
       	  Ue.RecSrv.CelsiaPlu$ = MID$(UE.RECASRV.CODE$,4,13)                ! Ean de recaudo
       EndIf																														    !
       If MID$(UE.RECASRV.CODE$,4,12) = "770999800597" Then Begin						! Recaudo Celsa
       	  Ue.RecSrv.LenCelsia% = Len(UE.RECASRV.CODE$)                      ! Longitud de la cadena
       	  Ue.RecSrv.CelsiaPlu$ = MID$(UE.RECASRV.CODE$,4,13)                ! EAN de recaudo
       EndIf																														    !
       
       TS.IO.DATA$(2) = Mid$(UE.RECASRV.CODE$,4,13) 
       TS.IO.KEYS(2)  = 80
       TS.IO.MOTORKEY = 80
       Ue.RecSrv.Device% = TS.IO.DEVICE
       TS.IO.DEVICE   = 3
       UE.RECASRV.PLU$ = MID$(UE.RECASRV.CODE$,4,12)
       UE.RECASRV.ITM% = -1
       Gr.RecSrv.Txn%  = -1

     EndIf

!--- Captura recaudo digitado     
     If (Len(TS.IO.DATA$(2)) > 11 And Len(TS.IO.DATA$(2)) < 14) And        \! 
        UE.RECASRV.CODE$ = ""   Then Begin 																	! Entrada de un EAN13
        TS.TEMP1$ = "," + Mid$(TS.IO.DATA$(2),1,12) + ","                   !
        If MATCH(TS.TEMP1$,UE.RECASRV.LISTA$,1) > 0 Then Begin							! Ingreso manual
           If ((TS.IO.KEYS(1) = 70) OR (TS.IO.KEYS(8) = 67)) Then Begin 	  ! Si anulacion del moviento 
              Call VISOR.AND.BORRAR("NO SE PERMITE ANUL. RECIBO ANULE TRX") !                               
              Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)                       !                               
              TS.IO.MOTORKEY = 73                                           !                             
              Exit Function                                                 !                             
           EndIf                                                            !                             
           UE.RECASRV.PLU$ = Mid$(TS.IO.DATA$(2),1,12)                      !
           UE.RECASRV.ITM% = -2																							!
           Gr.RecSrv.Txn%  = -1																							!
        EndIf 
     EndIf
     
     If (TS.IO.KEYS(1) = 70 And TS.IO.MOTORKEY = 81) Then Begin             ! Si  anulacion total
        UE.RECASRV.ANUL% = -1
     EndIf 
     
     If UE.RECASRV.QTY% > 0 Then \ 																					! Si recibos recaudados
      If TS.IO.MOTORKEY > 91 And TS.IO.MOTORKEY <= 96 Then Begin 						! Si formas de pago
       If Val(TS.IO.DATA$(7)) > TS.TOTALS(0,0,0) Then Begin 								! Valor mayor al recaudo
          Call VISOR.AND.BORRAR("MONTO INGRESADO SUPERA VALOR DEL RECIBO")  ! Msg de alerta
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)                
          TS.IO.MOTORKEY = 73      
          Exit Function 
       EndIf 
      EndIf 

!-- Control formas de pago reservas Hoteleras      

    If TS.IO.KEYS(7) > 90 AND TS.IO.KEYS(7) < 97 Then Begin                 ! Si forma de pago                       
 	   If (Ue.RecSrv.PagReserva% = -1) Then Begin                             ! Si en recaudo reservas                 
        TS.TEMP3$ = Str$(TS.IO.KEYS(7) - 90)                                ! Toma forma de pago                     
        If TS.IO.DATA$(3) = "" Then TS.TEMP3$ = TS.TEMP3$ + "1" Else       \! Toma variedad de pago                  
           TS.TEMP3$ = TS.TEMP3$ + TS.IO.DATA$(3)                           !                                        
 	   	  TS.TEMP3$ = ";"+TS.TEMP3$+";"																				! Arma dato de comparacion      					
 	   	  TS.TEMP1I4 = MATCH(TS.TEMP3$,UE.RECASRV.RESERVPAG$,1)			 					! Valida forma de pago          					
 	   	  If TS.TEMP1I4 <= 0 Then Begin																				! Si forma de pago no aceptada  					
          Call VISOR.AND.BORRAR("FORMA DE PAGO NO VALIDA PARA RESERVAS")    ! Msg de alerta                          
          Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)                
          TS.IO.MOTORKEY = 73      
          Exit Function 
 	   	  EndIf
     EndIf
    EndIf      

  EndIf																																			! Final TSU14
  
!--- Ejecucion en EAMTSU20
  If UE.RECASRV.PARAMETRO% = 20 Then Begin
    
    If (TS.LINETYPE = 18) And (TS.LINEDATA = 99) Then Begin
     If UE.RECASRV.QTY% > 0 THEN Begin
        UE.RECASRV.Slend% = SL.END								! Total de strings
     EndIf
    EndIf
    
    If MATCH(TS.SDESC$(59),TS.PRTBUF$,1) > 0 Then Begin					! Si anulacion Total
       UE.RECASRV.FLAG% = 0       
       UE.RECASRV.QTY%  = 0       
       UE.RECASRV.DC$ = ""        
       UE.RECASRV.VL$ = ""        
       UE.RECASRV.FC$ = ""        
       UE.RECASRV.NT$ = ""        
       UE.RECASRV.TP$ = ""        
       UE.RECASRV.CODE$ = ""      
       Gr.Rec.CtroCosto$ = "00000"
       UE.RECASRV.ANUL% = 0       
       UE.RECASRV.Recup% = 0      
       UE.RECASRV.ITM% = 0        
       Dim UE.RECASRV.DOC$(100)   
       Dim UE.RECASRV.VLR$(100)   
       Dim UE.RECASRV.NIT$(100)   
       Dim UE.RECASRV.FECHA$(100) 
       Dim UE.RECASRV.TIPO$(100)  
       Dim UE.RECASRV.CLAVE$(100) 
       Dim UE.RECASRV.CCOSTO$(100)
       Dim UE.RECASRV.TRX$(100)
       Dim Ue.RecSrv.AplMsg$(60)
       Gr.Rec.NitRsv$ = ""        
       Gr.Rec.NameRsv$ = ""  
       Gr.REC.Acuaviva% = 0   
       Ue.RecSrv.Reserva% = 0 
       Ue.RecSrv.Emcali% = 0 
       Ue.RecSrv.Device% = 0
       Gr.RecSrv.Txn% = 0
       Gr.RecSrv.Simple% = 0
       Ue.RecSrv.OnTrx% = 0
       Gr.RecSrv.NewCol% = 0
       Ue.RecSrv.Periodo$ = ""
       Ue.RecSrv.Planilla$ = ""
       Gr.RecSrv.Simple% = 0
       Gr.RecSrv.SndSmp% = 0
       Gr.RecSrv.RecComfa% = 0
       Gr.RecSrv.Siga% = 0
       Gr.RecSrv.CrdSoc% = 0 
       
    EndIf

    If TS.INTRX Then                                                       \! Si esta en una compra
     If (TS.LINETYPE = 6 And TS.LINEDATA = 1) And                          \! Store Data y Transacc
        Ue.RecSrv.PagReserva% = -1 And                                     \!
        UE.RECASRV.TipoRecSales% <> 0 Then Begin                            ! Fecha, Hora, etc 
        Call U.IMPRIME("ACEPTO LAS NORMAS ESTIPULADAS EN EL  ",4101H)       !
        Call U.IMPRIME("REGLAMENTO INTERNO DE LOS CENTROS    ",4101H)       !
        Call U.IMPRIME("VACACIONALES Y RECREATIVOS           ",4101H)       !
        Call ACT.RSV.HOTEL                                                  ! Actualizacion reserva hotelera
        Ue.RecSrv.Reserva% = 0 
     EndIf 

!    If TS.INTRX Then                                                       \! Si esta en una compra
!     If (TS.LINETYPE = 6 And TS.LINEDATA = 1) And                          \! Store Data y Transacc
!        Ue.RecSrv.PagReserva% = -1 And                                     \!
!        UE.RECASRV.TipoRecSales% = 2 Then Begin                             ! Fecha, Hora, etc 
!        Call ACT.RSV.HOTEL                                                  ! Actualizacion reserva hotelera
!        Ue.RecSrv.Reserva% = 0 																							!
!     EndIf 																																	!
     
    If TS.INTRX Then                                                       \! Si esta en una compra
     If (TS.LINETYPE = 6 And TS.LINEDATA = 1) And                          \! Store Data y Transacc
        Gr.RecSrv.Simple% = -1 Then Begin                                   ! Si recaudo simple online
        UE.RECASRV.Slend% = SL.END								                          ! Total de strings
        
        !Call ACT.REC.SIMPLE	                           ! Confirma recaudo
        
     EndIf

    If TS.INTRX Then Begin                                                  ! Si esta en una compra
     If (TS.LINETYPE = 6 And TS.LINEDATA = 1) And                          \! Store Data y Transacc
        Gr.RecSrv.NewCol% = -1 Then Begin              											! Si recaudo colegios ws
        UE.RECASRV.Slend% = SL.END								     											! Total de strings
        Call ACT.REC.NEWCOL	                           											! Confirma recaudo
     EndIf																																	!
     If (TS.LINETYPE = 6 And TS.LINEDATA = 1) And     								     \! Store Data y Transacc
        Gr.RecSrv.Siga% = -1 Then Begin                											! Si recaudo Siga
        UE.RECASRV.Slend% = SL.END								     											! Total de strings
        Call ACT.REC.SIGA	                             											! Confirma recaudo
     EndIf																						 											!
     If (TS.LINETYPE = 6 And TS.LINEDATA = 1) And     								     \! Store Data y Transacc
        Gr.RecSrv.CrdSoc% = -1 Then Begin                											! Si recaudo Siga
        UE.RECASRV.Slend% = SL.END								     											! Total de strings
        Call ACT.REC.CRDSOC                            											! Confirma recaudo credito social
     EndIf																						 											!
     
    EndIf																							 											! Fin validaciones

 If TS.INTRX THEN                                                          \! Si esta en una compra
  If (TS.LINETYPE = 6 And TS.LINEDATA = 1) Then Begin                      \! Store Data y Transacc
  	 TS.TEMP5I2 = 0																													!
  	 For TS.TEMP5I2 = 1 To 50                                               ! Valida si hay impresiones        
  	   If Ue.RecSrv.AplMsg$(TS.TEMP5I2) = "1" Then Begin                    ! si genera mensaje                
  	   	  TS.TEMP4I2 = Val(Ue.RecSrv.Msg$(TS.TEMP5I2,0))                    ! Numero de Lineas a Imprimir      
  	   	  For TS.TEMP3I2 = 1 To TS.TEMP4I2                                  ! Imprime el mensaje en el tiquete 
  	   	      TS.TEMP6$ = Ue.RecSrv.Msg$(TS.TEMP5I2,TS.TEMP3I2)             ! Linea a imprimir                 
              If TS.TEMP3I2 < TS.TEMP4I2 Then                              \! Proceso Impresion                
  	   	       Call U.Imprime(TS.TEMP6$,4100H) Else                        \! Genera mensaje en la tirilla     
  	   	       Call U.Imprime(TS.TEMP6$,4200H)                              ! Genera mensaje en la tirilla     
  	   	  Next TS.TEMP3I2                                                   ! Fin genera mensaje               
  	   EndIf																										            !                                   
  	 Next TS.TEMP5I2																						            !                                   
  EndIf   																											            !                                   
  
  EndIf 

!--- Ejecucion en EAMTSU43
  If UE.RECASRV.PARAMETRO% = 43 Then Begin
      If UE.RECASRV.FLAG% = -1 Then Begin 
         SL.IT.XPRICE = Val(UE.RECASRV.VL$)
         UE.RECASRV.VL$ = Str$(SL.IT.XPRICE)
         UE.RECASRV.FLAG%   = -2
      EndIf 
      

!     If SL.IT.ITEMCODE$ = "7701015313002" Then Begin                ! Recaudo credito social
!     	  Gr.RecSrv.RecComfa% = 0     	  														! cancela poderse pagar asi mismo
!     EndIf
      
!      If TS.TRAINING Then Begin																		! En entrenamiento
!         UE.RECASRV.VL$   = "1000"																! Valor del recuado
!         SL.IT.XPRICE     = 1000																	! para entrenamiento
!         SL.IE.SALEPRIC   = 1000																	!
!      EndIf																												! Fin Entrenamiento
      
  EndIf

!--- Ejecucion en EAMTSU53
  If UE.RECASRV.PARAMETRO% = 53 Then Begin
     
     If UE.RECASRV.Recup% = 0 Then Begin
        Call Init.Proc.Recaudos
     EndIf 
        
     If Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "99" Then Begin   	    	        ! Si DATA/ENTRY
        TS.TEMP1$ = Unpack$(MID$(SL.STR.ENTRY$,3,4))												! Numero de proyecto
        For XI% = 1 To UE.RECASRV.CONT%																			! Total de recaudos definidos
            UE.RECASRV.POS% = MATCH("CLAVE",UE.RECASRV.MASK$(XI%),1)
            If UE.RECASRV.POS% > 0 Then UE.RECASRV.LENCLAVE% = 8
            If UE.RECASRV.LENCLAVE% > 0 Then \
               XCLAVE$ = Mid$(UE.RECASRV.MASK$(XI%),(UE.RECASRV.POS%+5),UE.RECASRV.LENCLAVE%) Else \
               XCLAVE$ = "11111111"
            If TS.TEMP1$ = XCLAVE$ THEN BEGIN                               ! Pago Servicios
               XI% = UE.RECASRV.CONT%	+ 10																	! Sale del ciclo
               UE.RECASRV.QTY% = UE.RECASRV.QTY% + 1												! Almacena strings
               UE.RECASRV.Recup% = -1                                       ! Recuperando un recaudo 
               Call TSHIECET
               Call VISORES4690(1,"RECAUDO TERCERO","RECUPERADO ",1200,"L")
            EndIf 
        Next XI%
     EndIf                                                                                                 
  
  EndIf 

!--- Ejecucion en EAMTSU56
  If UE.RECASRV.PARAMETRO% = 56 Then Begin
     Call Init.Proc.Recaudos
  EndIf 

!--- Ejecucion en EAMTSU60   
  If UE.RECASRV.PARAMETRO% = 60 Then Begin
 
   !If (TS.LINETYPE = 29) And UE.RECASRV.FLAG% = -2 Then Begin 
   
   	If UE.RECASRV.FLAG% = -2 Then Begin 
             To.USEREXIT(20) = 0 
             To.USEREXIT(60) = 0 
             UE.RECASRV.FLAG% = 0
             
             If Gr.RecSrv.Simple% = 0 Then Begin									! No Es un recaudo simple - GR 22 Sept 2015
                Call U.IMPRIME("DOCUMENTO : "+RIGHT$(STRING$(24," ")+UE.RECASRV.DC$,24),6100H)
             EndIf
              
!             Ue.RecSrv.InTrx% = -1															! Control trx exclusiva - GR 30 Jul 2014

             Ue.RecSrv.OnTrx% = -1 															! Recaudss en proceso
             Gr.Fis.Doc% = -1																		! Anula numeracion fiscal
             Ue.Cnv.Fiscal%     = -1
             
             If VAL(UE.RECASRV.VL$) > 0 Then Begin 
                Call Format.Amount(Val(UE.RECASRV.VL$))
                Call U.IMPRIME("VALOR     : "+RIGHT$(STRING$(24," ")+TS.TEMP1$,24),6100H)
             EndIf 
             
             If Val(Gr.Rec.CtroCosto$) > 0 Then Begin
                Call U.IMPRIME("CTRO COSTO: "+RIGHT$(STRING$(24," ")+Gr.Rec.CtroCosto$,24),6100H)
             EndIf 
             If VAL(UE.RECASRV.FC$) > 0 Then \
                Call U.IMPRIME("FECHA DOC.: "+RIGHT$(STRING$(24," ")+UE.RECASRV.FC$,24),6100H)

             If TS.TRAINING Then Begin
             	  Call U.IMPRIME("** RECAUDO EN ENTRENAMIENTO **",4101H)
                Call U.IMPRIME("** RECAUDO EN ENTRENAMIENTO **",2100H)
             EndIf

!-- Mod 06Ene2009
             If Gr.REC.TipID% = 2 Then Begin 
                Gr.Rec.NitRsv$ = Str$(Val(Gr.Rec.NitRsv$))
                Call U.IMPRIME("NIT/C.C.  : "+Gr.Rec.NitRsv$,6100H)
                TS.TEMP1$ = LEFT$("CLIENTE   : "+Gr.Rec.NameRsv$,38)
                Call U.IMPRIME(TS.TEMP1$,6100H)
!-- Mod 27Jun2012
                If UE.RECASRV.TipoRecSales% <> 0 Then Begin 
                 Call U.IMPRIME("ESTIMADO HUESPED TENGA ENCUENTA",6100H)
                 Call U.IMPRIME("ESTA INFORMACION:",6100H)
                 Call U.IMPRIME("TEMPORADA BAJA",4101H)
                 Call U.IMPRIME("TEMPORADA BAJA",2100H)
                 Call U.IMPRIME("CHECK IN    4:00 PM",6100H)
                 Call U.IMPRIME("CHECK OUT   2:00 PM",6200H)
                 Call U.IMPRIME("TEMPORADA ALTA",4101H)
                 Call U.IMPRIME("TEMPORADA ALTA",2100H)
                 Call U.IMPRIME("CHECK IN    5:00 PM",6100H)
                 Call U.IMPRIME("CHECK OUT   2:00 PM",6100H)
                EndIF 
                Ue.RecSrv.InTrx% = -1															! Control trx exclusiva - GR 30 Jul 2014
                Ue.RecSrv.Reserva% = -1
                Ue.RecSrv.PagReserva% = -1
             EndIf 

             If Gr.RecSrv.Simple% = -1 Then Begin									! Es un recaudo simple - GR 22 Sept 2015
                TS.TEMP1$ = "NIT       : "+Gr.RecSrv.IdRef$
                TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
                Call U.Imprime(TS.TEMP1$,2100H)
                Call U.Imprime(TS.TEMP1$,4100H)
             	  TS.TEMP1$ = "PIN       : "+UE.RECASRV.DC$
                TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
                Call U.Imprime(TS.TEMP1$,2100H)
                Call U.Imprime(TS.TEMP1$,4100H)
             	  TS.TEMP1$ = "PLANILLA  : "+Gr.RecSrv.IdRef$
                TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
                Call U.Imprime(TS.TEMP1$,2100H)
                Call U.Imprime(TS.TEMP1$,4100H)
                TS.TEMP1$ = "PERIODO   : "+Ue.RecSrv.Periodo$
                TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
                Call U.Imprime(TS.TEMP1$,2100H)
                Call U.Imprime(TS.TEMP1$,4100H)
                Ue.RecSrv.Planilla$ = UE.RECASRV.DC$
             EndIf

!-- Se traslada el proceso de grabacion de String a este punto para evitar problemas de registro

         !If Gr.REC.Acuaviva% = 0 Then Begin          

      If NOT TS.RETV.IN.PROGRESS Then Begin            ! No esta en proceso de recuperacion de trx
      	                
         If UE.RECASRV.CLV$ <> "20090226" Then Begin 
         	
             If UE.RECASRV.PLU$ = "770999800597" Or   \! Recaudo Celsia          	
         	      UE.RECASRV.PLU$ = "770720821001" Then Begin
                UE.RECASRV.PLU$ = Ue.RecSrv.CelsiaPlu$         	      	
         	   EndIf
             TS.TEMP1$ = PACK$(UE.RECASRV.PLU$)   +   \! EAN DEL RECIBO
		                     ":"+PACK$(UE.RECASRV.TP$)+	  \! Tipo de Proceso              			
		                     ":"+PACK$(UE.RECASRV.DC$)+	  \! Nro documento                			
		                     ":"+PACK$(UE.RECASRV.VL$)+	  \! Vlr del recaudo	            			
		                     ":"+PACK$(UE.RECASRV.FC$)+   \! Fecha de Vencimiento         			
		                     ":"+PACK$(UE.RECASRV.NT$)+   \! Nit de la entidad recaudadora
		                     ":"+UE.RECASRV.RTA$      +   \! Trama de respuesta fechas y EAN entidad
		                     ":"+Gr.Rec.NitRsv$       +   \! Nit del cliente 
                         ":"+Gr.Rec.NameRsv$      +   \! Nombre del cliente
                         ":"+Pack$(Gr.Rec.CtroCosto$)+\! Centro de Costo Educacion
                         ":"+Gr.RecSrv.Rqst$      +   \! Request confirmacion simple
                         ":"+Ue.RecSrv.Periodo$   +   \! Periodo de cotizacion simple
                         ":"+Gr.RecSrv.IdRef$          ! Nit empresa
                         
          EndIf Else Begin 
             If UE.RECASRV.PLU$ = "770999800597" Or   \! Recaudo Celsia          	
         	      UE.RECASRV.PLU$ = "770720821001" Then Begin
                UE.RECASRV.PLU$ = Ue.RecSrv.CelsiaPlu$         	      	
         	   EndIf
             TS.TEMP1$ = PACK$(UE.RECASRV.PLU$)   +   \! EAN DEL RECIBO
		                     ":"+PACK$(UE.RECASRV.TP$)+	  \! Tipo de Proceso              			
		                     ":"+     (UE.RECASRV.DC$)+	  \! Nro documento                			
		                     ":"+PACK$(UE.RECASRV.VL$)+	  \! Vlr del recaudo	            			
		                     ":"+PACK$(UE.RECASRV.FC$)+   \! Fecha de Vencimiento         			
		                     ":"+PACK$(UE.RECASRV.NT$)+   \! Nit de la entidad recaudadora
		                     ":"+UE.RECASRV.RTA$      +   \! Trama de respuesta fechas y EAN entidad
		                     ":"+Gr.Rec.NitRsv$       +   \! Nit del cliente 
                         ":"+Gr.Rec.NameRsv$      +   \! Nombre del cliente
                         ":"+Pack$(Gr.Rec.CtroCosto$)+\! Centro de Costo Educacion
                         ":"+Gr.RecSrv.Rqst$      +   \! Request confirmacion simple
                         ":"+Ue.RecSrv.Periodo$   +   \! Periodo de cotizacion simple
                         ":"+Gr.RecSrv.IdRef$          ! Nit empresa
          EndIf
          
          If UE.RECASRV.Recup% <> -1 Then \
             Call Grabacion.String.Usuario2(UE.RECASRV.CLV$,TS.TEMP1$)

          If UE.RECASRV.Recup% = -1 Then Begin 
           If UE.RECASRV.DC$ <> UE.RECASRV.DCRECUP$ Then \
             Call Grabacion.String.Usuario2(UE.RECASRV.CLV$,TS.TEMP1$)
          EndIf 

!          If Gr.RecSrv.Fiscal% = 0 Then Begin ! Si recaudo no genera factura fiscal
!              Ue.Cnv.Fiscal%     = -1         ! Elimina proceso impresion numeracion fiscal
!          EndIf

           To.USEREXIT(20)    = -1
           To.USEREXIT(60)    = -1
         	 UE.RECASRV.DC$     = ""
         	 UE.RECASRV.VL$     = ""
         	 UE.RECASRV.FC$     = ""
         	 UE.RECASRV.NT$     = ""
         	 UE.RECASRV.TP$     = ""
         	 UE.RECASRV.CODE$   = ""
         	 Gr.Rec.CtroCosto$  = "00000"
         	 Gr.Rec.NitRsv$     = "0"
         	 Gr.Rec.NameRsv$    = ""
         	 UE.RECASRV.FLAG%   = 0
         	 UE.RECASRV.ITM%    = 0
         	 Gr.REC.Acuaviva% 	 = 0 
         	 Ue.RecSrv.Reserva% = 0
         	 Gr.RecSrv.Txn%     = 0

     endif 

  EndIf
EndIf

 
!--- Ejecucion en EAMTSU66
  If UE.RECASRV.PARAMETRO% = 66 Then Begin
    
    If UE.RECASRV.ANUL% = -1 Then \             ! Si en proceso de anulacion total
     If XData% = -1 Then Begin 			! Autorizacion correcta
        Call Init.Proc.Recaudos
     EndIf 
  EndIf

!--- Ejecucion en EAMTSU37
  If UE.RECASRV.PARAMETRO% = 37 Then Begin

     If TS.USER.RETURN = 0 Then Begin
        If Ucase$(Mid$(TS.DISP1$,1,4)) = "B039" Then Begin
         If UE.RECASRV.QTY% > 0 Then Begin
         	 Call VISORES4690(1,"ANULANDO DATOS","RECAUDO CAPTURADO",1000,"L")
           UE.RECASRV.FLAG% = 0                                                 
           UE.RECASRV.QTY% = UE.RECASRV.QTY% - 1                                
           UE.RECASRV.ITM% = 0                                                  
           UE.RECASRV.CODE$ = ""
           Gr.RecSrv.Simple% = 0
           Gr.REC.TipID% = 0
           Ue.RecSrv.InTrx% = 0
           Ue.RecSim.InTrx% = 0
           Gr.RecSrv.Siga% = 0
         EndIf  
        EndIf
     EndIf

  EndIf

End Function  
