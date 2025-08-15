!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMOVRMS.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Captura Remisiones Mercancia     *
!**************************************************
!Observaciones:
! Mod 15 Feb 2011
! Se ajusta procedimiento de impresion en articulos
! pesados, controlando el descriptor de peso mayuscula 
! o minuscula y eliminando la impresion del dato en el
! tiquete. Desarrollado Grupo Retail - OVS
!-----------------------------------------------------
! Mod 15 Feb 2011
! Se ajusta proceso de registro de remision de mercancia
! para que al llegar cerca a un proceso de recogida de
! dinero no permita el procedimiento de registro de 
! remision. 
! Desarrollado por Grupo Retail - OVS
!------------------------------------------------------
! Mod 18 Ene 2017
! Se desarrolla funcionalidad para validacion de las 
! ordenes de servicios en linea contra la plataforma
! de saleforce.
! Desarrollado por Grupo Retail - OVS/CAC
!------------------------------------------------------
! Mod 30 May 2017
! Se adiciona invocacion a la UE EAMTSU01 para la grabacion
! del user data de captura de numero de remision, para evitar
! perdida despues de procesos de recogida de dinero.
! Desarrollado por Grupo Retail - OVS/CAC
!------------------------------------------------------
! Mod 22 mAY 2018
! Se controla que el procedimiento no permita el manejo
! de transacciones suspendidas.
! Desarrollado por Grupo Retail - OVS/CAC
!------------------------------------------------------
! Mod 10 Mzo 2021
! Se modifica modulo para reportar si el producto tiene 
! impoconsumo o no para ser cargado en Saleforce.
! Desarrollado por Grupo Retail - OVS/CAC
!------------------------------------------------------
! Mod 29Mzo2022
! Se adiciona en la grabación del user data la seccion 
! de la estructura comercial para reportarla a saleforce
! en vez del dpto de venta.
! Desarrollado por Grupo Retail - OVS/CAC
!------------------------------------------------------
! Mod 23Ene2023
! Ajuste numero de remision para proceso de FE
! Desarrollado por Grupo Retail - OVS/CAC
!------------------------------------------------------
! Mod 13Mzo2024
! Ajuste reporte impuesto ICUI para calculo en 
! FECO.
! Desarrollado por Grupo Retail - OVS/CAC
!------------------------------------------------------
 
%ENVIRON T		                          																			! Ambiente de terminal

Integer*1 Global  Gr.Mrm.Ok%, Gr.Mrm.Cap%, Ue.Cnv.Fiscal%, UE.SGN%,          \!
                  Gr.Mrm.Int%, Gr.Mrm.Grab%                                   ! Control del proyecto
Integer*2 Global  Gr.Mrm.Key%            																			! Tecla motora
String    Global  Gr.Mrm.Remision$, Ue.Iva.Item$, Gr.Mrm.Pago$, Gr.Mrm.Nit$, \!
                  Gr.Mrm.Padre$, IR.USERDATA$                                 !
Integer*4 Global  Gr.Crd.Remision%, Asc.Tmp.Apun%															! Numero de remision por terminal
Integer*4 GLOBAL  TE.TR.AMT(1)                                                ! ENTERED TENDER 1-CASH Int 4
Integer*1 GLOBAL  UE.TARIFA%, Gr.UeTarifa%																		! Tarifa IVA
Integer*4 GLOBAL  UE.TABLAIVA(1)																							! Lista tarifas IVA
Integer*4 Global Gr.Iva.PtgIcui%                                              ! Porcentaje ICUI

%INCLUDE EAMTSWKG.J86          																								! working storage
%INCLUDE EAMTOPTS.J86          																								! Terminal Options
%INCLUDE EAMITEMR.J86          																								! Maestro de articulos 
%INCLUDE EAMTRANS.J86          																								! Variables de transancion
%INCLUDE EAMTSEVA.J86          																								! Variables Cliente Frecuente
%INCLUDE RECATSSU.011        																									! Rutinas Genericas Grupo Retail

Function FORMAT.AMOUNT(AMT1) External 							                        ! Formateo de valores
 Integer*1 FORMAT.AMOUNT
 Integer*4 AMT1
End Function

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                      *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                         *!
  STRING    DATA$      !* String to hold the data read from file.            *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                               *!
End Sub 

Sub IRRFEC.SPLIT1 (RECORD$) EXTERNAL
  STRING    RECORD$    !* The record (including the key) read from file      *!
End Sub 

Sub Asignacion.Consecutivo.Remision External																!
End Sub 																																		!

Sub Datos.Fiscal.Items External
End Sub 

Sub REMISION.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:RA" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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

Sub Valida.Remision.Saleforce
String Xrtrama$, Xenvia$, Xsale$, Xrta$, Xllega$
!-- Retirar para activar cuenta transitoria

! TS.TEMP1I4 = -1
! Gr.Mrm.Padre$ = "123456"
! Exit Sub 

 TS.TEMP1I4 = 0
 Call VISORES4690(1,"VALIDANDO REMISION",Gr.Mrm.Remision$,500,"L")
 TS.TEMP1$ = Right$("                  "+Gr.Mrm.Remision$,18)				        ! Consulta afiliado
 TS.TEMP2$ = Armar.Trama.Msg("18","01",TS.TEMP1$,"00","0001","123456") 		  ! Armar trama MSG                	
 Xenvia$   = TS.TEMP2$																											!
 Xsale$    = Date$																													! Hora salida requerimiento
 XRtrama$  = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)  			! Ejecuta Requerimiento
 
 !Xrtrama$ = "1801064200000Pedido disponible                                         "
 
 Xrta$     = Date$																													! Hora llegada requerimiento
 Xllega$   = XRtrama$																											  !
 Call REMISION.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                    ! Rastro auditoria

 TS.TEMP1$ = Valida.Rta(XRtrama$)																					  ! Valida rta entregada
 If TS.TEMP1$ = "00" Then Begin																							! Proceso satisfactorio
	  TS.TEMP1$ = MID$(XRtrama$,12,2)                                         ! Toma rta de la BD
		If TS.TEMP1$ <> "00" Then Begin								   									    	! Si presenta un Error
			 Call VISOR.And.BORRAR(Mid$(XRtrama$,14,40))                          ! Msg de Error	
       TS.TEMP1$ = MID$(XRtrama$,12,2)                                      ! Toma rta de la BD
			 TS.TEMP1I4 = 0																							  				! Proceso no satisfactorio
			 Exit Sub       																											! Sale del proceso
    EndIf																																		! Proceso satistactorio

    Gr.Mrm.Padre$ = Mid$(XRtrama$,53,18)                                    ! Numero orden padre
    If Gr.Mrm.Padre$ = "                  " Then                           \!
       Gr.Mrm.Padre$ = "999999999999999999"

    TS.TEMP1I4 = -1																								  				! Proceso satisfactorio
 EndIf																																			!

End Sub 

Sub Aplica.Remision.Pago																											!
      If Gr.Mrm.Int% = 1 Then BEGIN                                           ! Simula Data/Entry Minifactura
         Call Grabacion.Data.Entry("000000000019999","0","000000000",        \! Cabecera DataEntry
                      Right$("0000000"+Gr.Mrm.Remision$,7) +"0"+Gr.Mrm.Nit$, \! Numero documento y nit
                      Unpack$(TS.OPER$),"")                                   ! Codigo del cajero    	
      EndIf
      Dim Ts.Io.Keys(10)																											! Inicializa Vectores
      Dim Ts.Io.Data$(10)																											! 
      TS.IO.DATA$(3) = Right$(Gr.Mrm.Pago$,1)                                 ! Toma la variedad de pago
      TS.IO.KEYS(3)  = 78																											! Simula la tecla slash
      TS.IO.DATA$(7) = STR$(TS.TOTALS(0,0,0))																	! Precio de la compra
      TS.IO.KEYS(7)  = 90 + Val(Left$(Gr.Mrm.Pago$,1))                        ! Arma secuencia tecleo motor
      TS.IO.MOTORKEY = 90 + Val(Left$(Gr.Mrm.Pago$,1))                        ! Arma secuencia tecleo motor
      If Gr.Mrm.Int% = 1 Then Begin                                           ! Remision interna
      	 TS.IO.DATA$(9) = Gr.Mrm.Nit$                                         ! Asigna Nit para remisiones
      EndIf
End Sub 


Sub Datos.Itm.Remision
String Gbuff$, GR.BIN$, GrLt$, GrAlpha$, Grname$, GrImpo$
Integer*1 GR.INDI1%, XJ%
String Xkey$, Xsecc$, Xicui$, Xdepto$
  GrAlpha$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
  GR.INDI1% = SL.IT.INDICAT1                                                 !                         
  IF (GR.INDI1% AND 01H) THEN                                               \!                         
      GR.BIN$ = "1" ELSE GR.BIN$ = "0"                                       !                         
  FOR I% = 1 TO 7                                                            !                         
       GR.INDI1% = SHIFT(GR.INDI1%,1)                                        ! Inicializa sig. BIT     
       IF (GR.INDI1% AND 01H) THEN GR.BIN$="1"+GR.BIN$                      \!                         
           ELSE GR.BIN$ ="0"+GR.BIN$                                         !                         
  NEXT I%                                                                    !                         
  GrImpo$ = "00"																														 ! Si es impoconsumo
  UE.TARIFA% = 8																														 ! TARIFA Default 
  If LEFT$(GR.BIN$,4) = "1000" THEN                                         \! Si hay Indic Plan A     
                      UE.TARIFA% = 1		                                    \! indica Plan = A         
   ELSE                                                                     \! si no,                  
   IF LEFT$(GR.BIN$,4) = "0100" THEN                                        \! Si hay Indic Plan B     
                      UE.TARIFA% = 2		                                    \! indica Plan = B         
   ELSE                                                                     \! si no,                  
   IF LEFT$(GR.BIN$,4) = "0010" THEN                                        \! Si hay Indic Plan C     
                      UE.TARIFA% = 3		                                    \! indica Plan = C         
   ELSE                                                                     \! si no,                  
   IF LEFT$(GR.BIN$,4) = "0001" THEN                                        \! Si hay Indic Plan D     
                      UE.TARIFA% = 4		                                    \! indica Plan = D         
   ELSE                                                                     \! si no,                  
   IF LEFT$(GR.BIN$,4) = "1100" THEN                                        \! Si hay Indic Plan AB    
                      UE.TARIFA% = 5		                                    \! indica Plan = E         
   ELSE                                                                     \! si no,                  
   IF LEFT$(GR.BIN$,4) = "0110" THEN                                        \! Si hay Indic Plan BC    
                      UE.TARIFA% = 6		                                    \! indica Plan = F         
   ELSE                                                                     \! si no,                  
   IF LEFT$(GR.BIN$,4) = "0011" THEN                                        \! Si hay Indic Plan CD    
                      UE.TARIFA% = 7		                                    \! indica Plan = G         
   ELSE                                                                     \! si no,                  
                      UE.TARIFA% = 8		                                     ! indica 0%               
   
   Gr.UeTarifa% = UE.TARIFA%																								! Tarifa IVA
   
     IF ((Not TS.RECOVERY) And (Not TS.RETV.IN.PROGRESS)) Then Begin
       TS.TEMP5$ = "00"																					   					! Signo de la operacion
       TS.TEMP4$ = "0"                                                      ! Excento o excluido IVA
       If (SL.IT.INDICAT3A = 8) Then TS.TEMP5$ = "01"              					! Si es una anulacion 
       If (SL.IT.INDICAT3A = 2) Then TS.TEMP5$ = "01"              					! Si es una anulacion 
       If UE.TARIFA% = 7 Then TS.TEMP4$ = "1"															  ! Si es excluido
       If UE.TARIFA% = 8 Then TS.TEMP4$ = "0"															  ! Si es excento
       If UE.TARIFA% = 4 Then GrImpo$   = "01"                              ! Si es impoconsumo
       GrName$ = ""
       For XJ% = 1 To Len(IR.ITEMNAME$)																			! Recorre descripcion Item
         If match(Ucase$(Mid$(IR.ITEMNAME$,XJ%,1)),GrAlpha$,1) <= 0 Then   \! Caracter especial
         	GrLt$ = " " Else GrLt$ = Mid$(IR.ITEMNAME$,XJ%,1)                 ! Elimina char especial cambia x espacio
         GrName$ = GrName$ + GrLt$																					! Arma descripcion 
       Next XJ%
!--- Add 29Mzo2022
       Xkey$ = Pack$( Right$("000000000000"+SL.IT.ITEMCODE$,12) )           ! Llave de busqueda
       Call IRRFEC.READ01(Xkey$, 4, TS.TEMP1$, 0)                		  			! Lectura Datos Itemr
       Call IRRFEC.SPLIT1( TS.TEMP1$ ) 	                          		  		! Entrega datos al eamitemr.j86
       XSecc$ = LEFT$(IR.USERDATA$,16)																	  	! Estructura comercial
       XSecc$ = Mid$(XSecc$,2,3)                                            ! Seccion de venta
       Xdepto$ = Unpack$(IR.DEPARTME$)																			! Dpto Venta Add 2024
       Xicui$ = "000"                                                       ! Ptg ICUI Add 13Mzo2024
       If (IR.INDICAT1A AND 08H) Then                                      \! Ptg ICUI Add 13Mzo2024
       	  Xicui$ = Right$("000"+Str$(Gr.Iva.PtgIcui%),3)								    ! Add 13Mzo2024
       If Val(Xdepto$) >= 800 Then Xicui$ = "000"													  ! Add 13Mzo2024
       
       Gbuff$ = Pack$(SL.IT.ITEMCODE$)                     + ":" + 				 \! SKU Vendido
                Pack$(Str$(UE.TABLAIVA(UE.TARIFA%)))       + ":" +         \! Tarifa de IVA 
                Pack$(TS.TEMP5$)                           + ":" +         \! Signo del movimiento 
                TS.TEMP4$                                  + ":" +         \! Si Es excento o exclusivo
                Grname$                                    + ":" +         \! Nombre del prodcuto 
                Pack$(GrImpo$)                             + ":" +         \! Si es impoconsumo
                Pack$(XSecc$)                              + ":" +         \! Seccion producto estructura comercial
                Pack$(Xicui$)                                               ! ICUI producto
       Call Grabacion.Cadena.Usuario2("20170119", Gbuff$)                   ! Almacena String

      EndIf
      
End Sub 


Sub GRMOVRMS(XOPT%) Public																							    	! Captura remisiones
Integer*4 Xopt%                                                               !
String Xtmpx$, BARCODE$, ENDBCODE$

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                       ! Carga de parametros
  Gr.Mrm.Ok%  = 0                                                             ! Proyecto Apagado
  Gr.Mrm.Cap% = 0 
  TS.ER.RETURN = -1																													  ! Ctrl Errores                     
  OPEN "R::$SCNTRL" AS 94 NOWRITE NODEL					      												! Apertura archivo parametrizacion 
  If TS.ER.RETURN <> -1 Then BEGIN                                            !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA ARCHIVO CONTROL REMISION")
  	 Exit Sub 
  ENDIF 
  IF END #94 THEN UE.FIN.MOVRMS         																	    ! Si es EOF                        
  While (1)															  																    ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																				! Lectura registro                 
        IF TS.TEMP1$ = "[REMISION MERCANCIA]" Then Begin		   		   					! Remision Mercancia
         Read #94; TS.TEMP1$																									! Lectura registro                 
         Gr.Mrm.Ok%   = Val(Mid$(TS.TEMP1$,30,2))      				    					  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						  ! Lectura registro                 
         Gr.Mrm.Key%  = Val(Mid$(TS.TEMP1$,30,03))         				            ! Tecla motora secuencia
         Read #94; TS.TEMP1$     																						  ! Lectura registro                 
         Gr.Mrm.Pago$ = Mid$(TS.TEMP1$,30,2)              				            ! Tipo y variedad de pago
         Read #94; TS.TEMP1$     																						  ! Lectura registro                 
         Gr.Mrm.Nit$  = Mid$(TS.TEMP1$,30,12)              				            ! Numero Nit remisiones internas
         GoTo UE.FIN.MOVRMS 																								  ! Sale del ciclo de carga          
       Endif                                                                  !
   Wend                                                                       !
   UE.FIN.MOVRMS:                                                             !
     Close 94																																  ! Cierra archivo
   If Gr.Mrm.Ok% = -1 Then                                                   \! Proyecto Activo
      Call U.IMPRIME("MODULO REMISION MCIAS  ON 13/Mzo/2024",2100H) Else     \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO REMISION MCIAS OFF 13/Mzo/2024",2100H)           ! Msg Proyecto Cargado
EndIf 

If Gr.Mrm.Ok% <> -1 Then Exit Sub                                             ! Si proyecto apagado

!--- EAMTSU01.J86
If Xopt% = 1 Then Begin                                                     ! Al registro primer Item
 If Gr.Mrm.Cap% = -1 And Gr.Mrm.Grab% = 0 Then Begin											  ! En un proceso de remision
    Gr.Mrm.Int% = Val(TS.TEMP5$)                                            ! Tipo Ingreso Remision
    TS.TEMP1$ = Right$("                  "+Gr.Mrm.Remision$,18) + ":" +   \! Numero de remision 
                Right$("                  "+Gr.Mrm.Padre$,18)    + ":" +   \! Numero de remision padre
                PACK$(TS.TEMP5$) +                                         \! Almacena Udata para
                ":" + PACK$("00")                                           ! interfaces y procesos de 
    Call Grabacion.String.Usuario2("20110114",TS.TEMP1$)                    ! recuperacion.
    Gr.Mrm.Grab% = -1
 EndIf																																			! Fin grabacion
EndIf																																					! 

!--- EAMTSU02.J86
If Xopt% = 2 Then Begin                                                       ! Inicializacion de trx
   Gr.Mrm.Cap% = 0 
   Gr.Mrm.Remision$ = ""
   Gr.Mrm.Int% = 0   
   Gr.Mrm.Grab% = 0
EndIf 

!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                      ! Secuencias de tecleo
 If Gr.Mrm.Cap% = -1 Then Begin                                      					! Hay remision capturada
    If TS.IO.KEYS(1) = 82 Then Begin                                       		! Proceso de suspension de
	  	 Call VISOR.AND.BORRAR("PROCESO NO PERMITIDO PARA REMISION")        		! Msg Alerta
       Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)                								!
       TS.IO.MOTORKEY = 73 																										!
       Exit Sub      																													!
    EndIf																																			! Fin suspension
 EndIf																																				! 
	
 If TS.IO.MOTORKEY = Gr.Mrm.Key% Then Begin 																	! Captura tecla control remisiones
 	If TS.INTRX <> 0 Then Begin 																								! Ya esta dentro de una trx
  	 Call VISOR.AND.BORRAR("PROCEDIMIENTO NO VALIDO EN UNA TRX")              ! Msg alerta procedimiento no valido
     Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
     TS.IO.MOTORKEY = 73
     Exit Sub 
  Endif 
  
  If (TE.TR.AMT(1) + 50000) >= To.CDLIMIT2 Then Begin    											! SOBREPASA LIMITE CAJON MONEDERO
      Call VISOR.AND.BORRAR("REALICE PROCESO DE RECOGIDA CAJERO AHORA")				! Mensaje de alerta a Cajero
      Call VISOR.AND.BORRAR("PROCESO REMISIONES CANCELADO")
      Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
      TS.IO.MOTORKEY = 73
      Exit Sub 
  EndIf 	
  
 	If Gr.Mrm.Cap% = 0 Then Begin 																							! si tarjeta no capturada
 		Gr.Mrm.Int% = 0 
    TS.TEMP5$ = ASIC.DATOS$("INGRESE NUMERO DE ","REMISION..")								! Solicita dato al cajero
    If TS.TEMP5$ = "E" Or TS.TEMP5$ = "" Then BEGIN 									  			! Proceso cancelado
    	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
       TS.IO.MOTORKEY = 73
       Exit Sub 
    EndIf 
    Gr.Mrm.Remision$ = TS.TEMP5$																							! Numero remision capturado
    
    GR.REMI.INT:
!-- Comentariado mientras ajustan procesos SAP 
!-- Se retira comentario 20 Mayo 2012
!-- Se comentarea validacion 28 jun 2013

!    TS.TEMP5$ = ASIC.DATOS$("ES CONSUMO INTERNO  ","1.SI 9.NO ENT/BORRA")	  	! Solicita dato al cajero
!    If TS.TEMP5$ = "E" Or TS.TEMP5$ = "" Then BEGIN 									  			! Proceso cancelado
!    	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")
!       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
!       TS.IO.MOTORKEY = 73
!       Exit Sub 
!    EndIf 
!    If MATCH(TS.TEMP5$,"19",1) <= 0 Then Begin																! Valida opcion
!    	 Call VISOR.AND.BORRAR("OPCION INGRESADA NO ES VALIDA ENT/BORR")
!       GoTo GR.REMI.INT
!    EndIf

    TS.TEMP5$ = "9"  !!!!! Pilas Retirar esta linea al activar dialogo anterior OVS 24 ene 2013

    TS.TEMP1I4 = 0																													! Control proceso
    Call Valida.Remision.Saleforce																					! Validacion online
    If TS.TEMP1I4 <> -1 Then Begin																					! Falla en el proceso
    	 Call VISOR.AND.BORRAR("NUMERO DE REMISION NO EXISTE")								! Msj de alerta
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
    EndIf																																		! Fin proceso
    
!    Gr.Mrm.Int% = Val(TS.TEMP5$)                                            ! Tipo Ingreso Remision
!    TS.TEMP1$ = Right$("                  "+Gr.Mrm.Remision$,18) + ":" +   \! Numero de remision 
!                Right$("                  "+Gr.Mrm.Padre$,18)    + ":" +   \! Numero de remision padre
!                PACK$(TS.TEMP5$) +                                         \! Almacena Udata para
!                ":" + PACK$("00")                                           ! interfaces y procesos de 
!    Call Grabacion.String.Usuario2("20110114",TS.TEMP1$)                    ! recuperacion.

    Gr.Mrm.Cap% = -1																											  ! En un proceso de remision
    Ue.Cnv.Fiscal% = -1																											! Invalida proceso numeracion fiscal
    Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
    TS.IO.MOTORKEY = 73
  EndIf Else Begin 
  	Call Visor.And.Borrar("NUMERO REMISION YA CAPTURADO")
    Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
    TS.IO.MOTORKEY = 73
  EndIf 
  	
 EndIf 

   	 If Gr.Mrm.Cap% = -1 Then Begin 																					! Registro Remision
	 	  If TS.IO.KEYS(7) > 90 And TS.IO.KEYS(7) < 97 Then BEGIN                 ! Simular pago
         Call Aplica.Remision.Pago	 	  	 
	 	  EndIf 
	   EndIf 

EndIf 

!--- EAMTSU20.J86
If Xopt% = 20 Then Begin                                                      ! Impresion CR

If Gr.Mrm.Cap% = -1 Then Begin                                       					! Hay remision capturada
  UE.SGN% = 1
  If SL.IT.INDICAT2 And 80H  Then UE.SGN% = -1																!
  If SL.IT.INDICAT2 And 40H  Then UE.SGN% = -1																!
  If SL.IT.INDICAT2 = 6      Then UE.SGN% = -1																!
  If SL.IT.INDICAT2 = 2054   Then UE.SGN% = -1																!
  If SL.IT.INDICAT3A = 5     Then UE.SGN% = -1         												! 
  If SL.IT.INDICAT3A = 7 Then UE.SGN% = UE.SGN% * (-1)												!

 If Ts.Linetype = 7 And Ts.Linedata = 1 Then Begin														! Msg de total en la trx
   A$ = Right$(Ts.PrtBuf$,12)
   Ts.PrtBuf$ = "==>> SUBTOTAL/TOTAL      :$" + A$
   Exit Sub 
 EndIf

 If (TS.LINETYPE <= 1 OR TS.LINETYPE = 5) And (TS.LINEDATA = 1) Then Begin    ! Si es linea de articulo
 	  If Right$(TS.PRTBUF$,8) = "        " Then Begin                           ! 
       TS.PRTBUF$ = ""
       If PRT4610.ENABLE = 0 Then \
          TS.FORMCR$ = "C38 A0" Else \
          TS.FORMCR$ = "C52 A0" 
    Endif Else TS.LINEDATA = 0
 Endif 

 If MATCH("/KG",Ucase$(TS.PRTBUF$),1) > 0 Then Begin
    TS.PRTBUF$ = ""
    If PRT4610.ENABLE = 0 Then \
       TS.FORMCR$ = "C38 A0" Else \
       TS.FORMCR$ = "C52 A0" 
 EndIf 


 If TS.LINETYPE <= 1 OR TS.LINETYPE = 5 Then Begin
  If IR.ITEMTYPE < 8  And TS.LINEDATA = 0 Then Begin 
 	  TS.TEMP3$  = UnPack$(Ue.Iva.Item$)
 	  TS.TEMP3$  = Str$(Val(TS.TEMP3$))
 	  TS.TEMP2$  = Right$("       "+TS.TEMP3$,7)
 	  Call Format.Amount(SL.IE.QTYORWGT * UE.SGN%)
  	TS.TEMP3$ = Right$("          "+TS.TEMP1$,10)
 	  TS.TEMP1$  = TS.TEMP2$ + " " + IR.ITEMNAME$ + " " + TS.TEMP3$
 	  Call U.IMPRIME(TS.TEMP1$,6100H)
 	  Call Datos.Itm.Remision
 	  Call Datos.Fiscal.Items
    TS.PRTBUF$ = ""
    If PRT4610.ENABLE = 0 Then \
       TS.FORMCR$ = "C38 A0" Else \
       TS.FORMCR$ = "C52 A0" 

  EndIf 
 EndIf 

 If Not TS.TRAINING Then Begin                       												! Si no en entrenamiento
  If TS.INTRX Then                                  											 \! Si esta en una compra
    If TS.LINETYPE = 6 And                                                 \! Store Data y Transacc
       TS.LINEDATA = 1 Then Begin                    											  ! Fecha, Hora, etc
        If TS.TENDERED (0) <> 0 Or                                         \! Si hay pagos
           TS.TRX.STATUS <> 100 Then Begin                                  ! y NO hay VOID en curso
            If Gr.Mrm.Cap% = 0 Then Begin	                                  ! No hay remision capturada
               Exit Sub                                                     ! Sale del proceso y no genera numeracion 
            EndIf
            TS.TEMP1$ = "DOCUMENTO :" + Gr.Mrm.Remision$        						!
            Call U.IMPRIME(TS.TEMP1$,6100H)                 								! Imprime mensaje en el tiquete
            Call U.IMPRIME("  CONSTANCIA DE ENTREGA DE CREDITOS  ",6100H)

            !Call Asignacion.Consecutivo.Remision														! Genera consecutivo movimiento
            !TS.TEMP1$ = Right$("0000000"+STR$(Gr.Crd.Remision%),07)
            !TS.TEMP2$ = "   REMISION NRO: "+TS.STORE$+"-"+TS.TERMINAL$+"-"+TS.TEMP1$
            !TS.TEMP2$ = Left$(TS.TEMP2$ + STRING$(40," "),40)
            !Call U.Imprime(TS.TEMP2$,6100H)                

            Xtmpx$ = Str$(Val(TS.STORE$)) + Right$("000"+Str$(Val(TS.TERMINAL$)),3) + \!
                     Right$("0000"+Str$(Sl.Hd.Transnum + 1),4) + DATE$ + LEFT$(TIME$,4)
            TS.TEMP2$ = "   REMISION NRO: "+Xtmpx$
            TS.TEMP2$ = Left$(TS.TEMP2$ + STRING$(40," "),40)               !
            Call U.Imprime(TS.TEMP2$,6100H)                                 !

!-- Impresion EAN 128
            BARCODE$  = Chr$(1DH) + Chr$(6BH) 					                    ! Manejo de impresion
    				ENDBCODE$ = Chr$(00H) 						                              ! codigos de barra
    				TS.TEMP1$ = Chr$(1DH)+Chr$(77H)+Chr$(02H)+  		               \! Horizontal barcode Size
            				    Chr$(1DH)+Chr$(68H)+Chr$(50)+                      \! Vertical barcode Size 
                				Chr$(1DH)+Chr$(48H)+Chr$(02H)+                     \! Human Read characters
  	            				Chr$(1DH)+Chr$(66H)+Chr$(01H)    	                  ! activado
     				Call U.IMPRIME(TS.TEMP1$,4000H)				                          !
     				TS.TEMP1$ = (BARCODE$+Chr$(07H)+(Xtmpx$)+ENDBCODE$)             ! Arma String de impresion			
     				Write #34 ; TS.TEMP1$          					                        ! Imprime dato EAN-128
     				TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(0)				                  ! Ajusta impresion a la Izquierda
     				Call U.IMPRIME(TS.TEMP1$,4000H)				                          !                    
!-- Fin impresion EAN 128       
		        TS.TEMP1$ = Right$("0000000000000000"+Xtmpx$,16)		         
            TS.TEMP2$ = "TR+"                            + \! Arma Str Usuario con
                 "REMI"+Pack$(TS.TEMP1$)+":"+    + \!
                 STR$(ABS(TS.GROSSPOS + TS.GROSSNEG))     + \! #Fact, Vr/Vta Neto
                 ":" + Pack$("00") + ":" + Pack$("00")    + \! y Vrs de Iva
                 ":" + Pack$("00") + ":" + Pack$("00")    + \!
                 ":" + Pack$("00") + ":" + Pack$("00")    + \!
                 ":" + Pack$("00") + ":" + Pack$("00")       !

            Call Grabacion.Cadena.Usuario2("21",TS.TEMP2$)
            EMSS.TR.POINTS = 0 																							! Anula puntaje si captura EMS
            EMSS.RD.POINTS = 0 																							! Anula redencion su captura EMS
            Ue.Cnv.Fiscal% = -1
        EndIf 
    EndIf 
 EndIf        

Endif 

EndIf 

!--- EAMTSU53.J86
If Xopt% = 53 Then Begin                                                       	! Recuperacion de trx
   If UnPack$(LEFT$(SL.STR.ENTRY$,1)) = "99" Then Begin                 	    	! Si DATA/ENTRY
    If UnPack$(MID$(SL.STR.ENTRY$,3,4)) = "20110114" Then Begin                 ! Remision ya capturada
       Asc.Tmp.Apun% = 3
       TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                    ! Numero de proyecto
       TS.TEMP1$ = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)                   ! Numero de remision
       Gr.Mrm.Remision$ = TS.TEMP1$
       TS.TEMP1$ = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)                   ! Numero de remision padre
       Gr.Mrm.Padre$ = TS.TEMP1$
       TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                    ! Si movimiento interno
       Gr.Mrm.Int% = Val(TS.TEMP1$) 
    	 Gr.Mrm.Cap% = -1 
       Ue.Cnv.Fiscal% = -1																											! Invalida proceso numeracion fiscal
       Gr.Mrm.Grab% = -1
    EndIf    
   EndIf 
EndIf 

!--- EAMTSU56.J86
If Xopt% = 56 Then Begin                                                       	! Suspender una trx
   Gr.Mrm.Cap% = 0 
   Gr.Mrm.Remision$ = ""
   Gr.Mrm.Int% = 0   
   Gr.Mrm.Grab% = 0
EndIf 

End Sub 
