!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMBCOMP.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Generación Bonos Recompra        *
!**************************************************
!Observaciones:
! Mod 11Jun2014
! Se adiciona proceso de validacion para generación
! de cupones en línea, desarrollado por Grupo Retail
! OVS-CAC 
!--------------------------------------------------
! Mod 18May2017
! Se adiciona en archivo de parametros de bono recompra
! la definición del valor maximo de entrega del bono
! desarrollado Grupo Retail - OVS
!--------------------------------------------------
! Mod 27Nov2019
! Se modifica la creacion del bono en linea con la 
! siguiente estructura:
! 23 + id evento(8)+tipo bono(2)+fecha y hora(12)+ 
! fecha vencimiento(6) total 30 caracteres
! Solicitado por Comfandi para consumo de bonos en linea
! desarrollado Grupo Retail - OVS/CAC
!--------------------------------------------------
! Mod 13Ene2020
! Se modifica la estructura del bono recompra con la
! siguiente estructura:
! 23 + centro costo (3) + terminal(3) + transaccion(3) +
! fecha y hora(12) + fechas vigencia(12) total 35 caracteres.
! Se ajusta por problema de duplicidad de bono reportado
! desarrollado Grupo Retail - OVS/CAC
!--------------------------------------------------
! Mod 10Ago2020
! Se ajusta impresion bonos de formato PD417 a formato
! EAN-128.
! desarrollado Grupo Retail - OVS
!--------------------------------------------------
! Mod 07Jul2022
! Se controla generación de bonos recompra en operaciones
! con venta domicilios y secciones controladas.
! consideraciones de control
!Si en una transacción se registran medicamentos (sección 6) + PLU Domicilio(cualquiera de los 4 existentes)
!= El sistema NO genera Bono recompra.
!Si en una transacción se registran medicamentos + abarrotes + PLU Domicilio 
!= El sistema SI debe generar Bono recompra pero sólo por los ítems de abarrotes 
!que hagan parte de una promoción de bono recompra, a los medicamentos no les debe calcular nada 
!y no debe sumar dentro del bono recompra que se genere.
!Si en una transacción se registran medicamentos (sección 6) sin PLU domicilios 
!= El sistema SI genera Bono recompra.
!Si en una transacción se registran abarrotes (secciones diferentes a 6) y PLU domicilios 
!= El sistema SI genera Bono recompra.
!De forma resumida todas las ventas de medicamentos que contengan PLU de domicilios  
!no aplican para los cálculos de generación de bono recompra.
! Solicitado por Comfandi
! desarrollado Grupo Retail - OVS
!--------------------------------------------------
! Mod 29Ago2022
! Se modifica para el manejo de valor minimo de 
! generación de bono y control de productos excluidos
! en el evento definido.
! Solicitado por Comfandi
! desarrollado Grupo Retail - OVS/CAC
!--------------------------------------------------
! Mod 25Jun2023
! Ajuste apertura parametros bonos para problema en registro
! por datos incompletos dejando la trx Sin numeracion fiscal
! desarrollado Grupo Retail - OVS/CAC
!--------------------------------------------------
! Mod 21Mzo2025
! Se ajusta procedimiento para que en la transaccion si hay un plu de
! domicilio registrado, no genere bono recompra
! desarrollado Grupo Retail - OVS
!--------------------------------------------------
%Environ T		                          																		! Ambiente de terminal

Integer*1 Global Gr.BonCmp.Ok%,                                            \! Control del proyecto
                 Gr.BonCmp.Stat%,                                          \! Control de proceso de calculo compra
                 Gr.BonCmp.Oflne%,                                         \! Bono recompra Offline
                 Gr.BonCmp.VtaDomic%,                                      \! Control venta domicilios
                 Gr.BonCmp.VtaSecc%,                                       \! Control venta secciones
                 Gr.BonCmp.VtaOtrSecc%																			!

String    Global Gr.BonCmp.Tipo$(1),                                       \! Tipificaciones
                 Gr.BonCmp.Ptg$(1),                                        \! Porcentaje Dscto 
                 Gr.BonCmp.Msg$(1),                                        \! Mensaje a imprimir
                 Gr.BonCmp.Fini$,   																			 \! Fecha inicial 
                 Gr.BonCmp.Ffin$,  																				 \! Fecha final 
                 Gr.BonCmp.Prefijo$,                                       \! Prefijo bonos
                 Gr.BonCmp.Nbon$  																				  ! Nro Bono Generado

Integer*2 Global Gr.BonCmp.PtgT%                                            ! Porcentaje Aplicar
Integer*2 Global Gr.BonCmp.Ses%          																	  ! Sesion temporal
Integer*4 Global Gr.BonCmp.Vlr%                                             ! Valor Bono recompra
Integer*4 Global Gr.BonCmp.Monto%,																				 \! Valor maximo bonos
                 Gr.BonCmp.MonMin% 																		      ! Valor minimo de bonos
Integer*4 Global Asc.Tmp.Apun%                                              ! Apuntador strings
Integer*4 Global Asc.Pay.Impr%                                              !
Integer*1 Global Asic.Detalle%																							! Ctrl trailer trx
String    Global Gr.Lcl.Name$																								! Nombre del cliente
String    Global Gr.Lcl.Clte$                                               ! ID del cliente
Integer*1 Global Gr.Lcl.Ctrl%                                               ! Ctrl trx Lealtad
String    Global Gr.Lcl.TipClte$                                            ! Tipificaciones de clientes
String    Global IR.USERDATA$                                               ! Datos adicionales Itemr
Integer*4 GLOBAL UE.TABLAIVA(1)                                             ! Tarifas de IVA
String    Global Gr.BonCmp.Secc$,                                          \! Seccion controladas
                 Gr.BonCmp.Pludomi$                                         ! PLU Domicilios       

%INCLUDE EAMTSWKG.J86          																			        ! working storage
%INCLUDE EAMTRANS.J86          																			        ! working storage
%INCLUDE EAMITEMR.J86          																			        ! Maestra productos
%INCLUDE EAMTOPTS.J86          																			        ! working storage
%INCLUDE EAMTSEVA.J86          																							! Variables Cliente Frecuente
%INCLUDE EAMTSXHC.J86          																							! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          																							! Rutinas de Assembler

%INCLUDE RECATSSU.011          																			        ! Rutinas Genericas Grupo Retail

Function FORMAT.AMOUNT(AMT1) External   																		! Formateo de valores
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub TSPREC01 EXTERNAL          																							! print routine
End Sub

Sub TSHIECET EXTERNAL																												! Tono de alerta
End Sub

Function MATCHB(P1$,P2$,P3) EXTERNAL
  Integer*2 MATCHB
  String P1$
  String P2$
  Integer*2 P3
End Function

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                      *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                         *!
  STRING    DATA$      !* String to hold the data read from file.            *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                               *!
End Sub 

Sub IRRFEC.SPLIT1 (RECORD$) External
  String    RECORD$    !* The record (including the key) read from file      *!
End Sub 

Function Bono.Tarifa.Iva(UE.INDI1%) External
Integer*2 UE.INDI1%, Bono.Tarifa.Iva
End Function 

Sub BCOMP.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$) Public
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

Sub IMPRESION.BONO.COMPRA																										! Impresion Bono Recompra
String Xnro$, BARCODE$, ENDBCODE$, XDATE1$, XDATE2$, CPI17$
Integer*2 X2%, Xctl%
  If Gr.BonCmp.Vlr% > 0 Then Begin					     														! Hay Bono para Impresion
     !To.USEREXIT(20) = 0																									  ! Cancela lineas impresion
     !To.USEREXIT(60) = 0																									  !
     TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(11H)
     TS.TEMP1$ = TS.TEMP1$ + "$$$  B O N O  R E C O M P R A  $$$"          !
     Call U.Imprime(TS.TEMP1$,4100H)		                                    ! Impresion Bono recompra

     TS.TEMP1$ = "FECHA   : 20"+Left$(DATE$,2) + "-" + Mid$(DATE$,3,2) + "-" + \!
                 Right$(DATE$,2)
     TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												! Ajusta Dato impresion
     Call U.IMPRIME(TS.TEMP1$,4100H)																			  ! Imprime dato
     Xnro$ = "" : Xctl% = 0
     Xnro$ = Gr.BonCmp.Nbon$																								! Numero del bono
   
!     TS.TEMP1$ = LEFT$("Bono : "+Left$(Xnro$,29)+STRING$(37," "),37)			  ! Ajusta Dato impresion
!     Call U.IMPRIME(TS.TEMP1$,4100H)																			  ! Imprime dato
     
     Xdate1$ = "20" + Gr.BonCmp.Ptg$(11)                                    ! Fecha vencimiento
     Xdate1$ = Left$(Xdate1$,4)  + "/" +                                   \! Arma formato fecha
               Mid$(Xdate1$,5,2) + "/" +                                   \! para impresion
               Mid$(Xdate1$,7,2)                                            !
     Xdate2$ = "20" + Gr.BonCmp.Ptg$(12)                                    ! Fecha vencimiento
     Xdate2$ = Left$(Xdate2$,4)  + "/" +                                   \! Arma formato fecha
               Mid$(Xdate2$,5,2) + "/" +                                   \! para impresion
               Mid$(Xdate2$,7,2)                                            !
               
     TS.TEMP1$ = "VIGENCIA: "+Xdate1$+" AL "+XDATE2$
     TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												! Ajusta Dato impresion
     Call U.IMPRIME(TS.TEMP1$,4200H)																			  ! Imprime dato

!-- Impresion EAN 128
    BARCODE$  = Chr$(1DH) + Chr$(6BH) 					                            ! Manejo de impresion
    ENDBCODE$ = Chr$(00H) 						                                      ! codigos de barra
    TS.TEMP1$ = Chr$(1DH)+Chr$(77H)+Chr$(02H)+  		                       \! Horizontal barcode Size
                Chr$(1DH)+Chr$(68H)+Chr$(50)+                              \! Vertical barcode Size 
                Chr$(1DH)+Chr$(48H)+Chr$(02H)+                             \! Human Read characters
  	            Chr$(1DH)+Chr$(66H)+Chr$(01H)    	                          ! activado
     Call U.IMPRIME(TS.TEMP1$,4000H)				                                !
     TS.TEMP2$ = Right$("0000000000"+Str$(TS.TEMP5I4),10)                   ! Dato Impresion
     TS.TEMP1$ = (BARCODE$+Chr$(07H)+(XNRO$)+ENDBCODE$)           					! Arma String de impresion			
     Write #34 ; TS.TEMP1$          					                              ! Imprime cupon en PDF-417
     TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(0)				                        ! Ajusta impresion a la Izquierda
     Call U.IMPRIME(TS.TEMP1$,4000H)				                                !                    
!-- Fin impresion EAN 128


     TS.TEMP5I4 = ROUND(FLOAT(Gr.BonCmp.Vlr%) * (0.+FLOAT(Gr.BonCmp.PtgT%)/100.),0,0) ! Calcula dscto a otorgar
     If TS.TEMP5I4 > Gr.BonCmp.Monto% Then TS.TEMP5I4 = Gr.BonCmp.Monto%    ! Maximo valor bono parametro 4 Mayo 2017
     CALL FORMAT.AMOUNT(TS.TEMP5I4)																					!
     TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(11H)+"VALOR BONO :$"+TS.TEMP1$  	! Valor del Bono
     Call U.IMPRIME(TS.TEMP1$,4200H)																			  ! Imprime dato
     TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(00H)+" "
     Call U.IMPRIME(TS.TEMP1$,4000H)																			  ! Imprime dato
     If Gr.BonCmp.Msg$(1) <> "" Then Begin
        TS.TEMP1$ = Gr.BonCmp.Msg$(1)																				! Mensaje 1
        TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)											! Ajusta Dato impresion
        Call U.IMPRIME(TS.TEMP1$,4101H)																		  ! Imprime dato
     EndIf
     If Gr.BonCmp.Msg$(2) <> "" Then Begin
        TS.TEMP1$ = Gr.BonCmp.Msg$(2)																				! Mensaje 2
        TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)											! Ajusta Dato impresion
        Call U.IMPRIME(TS.TEMP1$,4101H)																		  ! Imprime dato
     EndIf
     If Gr.BonCmp.Msg$(3) <> "" Then Begin
        TS.TEMP1$ = Gr.BonCmp.Msg$(3)																				! Mensaje 3
        TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)											! Ajusta Dato impresion
        Call U.IMPRIME(TS.TEMP1$,4101H)																		  ! Imprime dato
     EndIf
     TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(00H)+" "
     Call U.IMPRIME(TS.TEMP1$,4400H)																			  ! Imprime dato
     TS.TEMP1$ = "FIRMA:______________________________"											! Firma
     TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												! Ajusta Dato impresion
     Call U.IMPRIME(TS.TEMP1$,4101H)																			  ! Imprime dato
    
     TS.TEMP1$ = Gr.Lcl.Name$                         											! Nombre del cliente
     TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												! Ajusta Dato impresion
     Call U.IMPRIME(TS.TEMP1$,4100H)																			  ! Imprime dato
!--- Add 3/May/2022
     TS.TEMP1$ = "PERSONAL E INTRANSFERIBLE APLICAN    "										! Mensaje 
     TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												! Ajusta Dato impresion
     Call U.IMPRIME(TS.TEMP1$,4100H)																			  ! Imprime dato
     TS.TEMP1$ = "TERMINOS Y CONDICIONES               "										! Mensaje 
     TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												! Ajusta Dato impresion
     Call U.IMPRIME(TS.TEMP1$,4100H)																			  ! Imprime dato

     
!-- pdf 417
!     BARCODE$  = Chr$(1DH) + Chr$(50H) 					                            ! Manejo de impresion
!     ENDBCODE$ = Chr$(00H) 						                                      ! codigos de barra
!     TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(1)				                        ! Centra la impresion
!     Call U.IMPRIME(TS.TEMP1$,4000H)					                              !        
!     TS.TEMP1$ = Chr$(1DH)+Chr$(77H)+Chr$(01H)+ 		                       \! Horizontal barcode
!                 Chr$(1DH)+Chr$(68H)+Chr$(60H)+                            \! Vertical barcode			
!                 Chr$(1DH)+Chr$(48H)+Chr$(02H)+                            \! Human Read characters
!		             Chr$(1DH)+Chr$(66H)+Chr$(00H)    	                        ! activado
!     Call U.IMPRIME(TS.TEMP1$,4000H)				                                !
!     TS.TEMP2$ = Right$("0000000000"+Str$(TS.TEMP5I4),10)                   ! Dato Impresion
!     TS.TEMP1$ = (BARCODE$+XNRO$+TS.TEMP2$+ENDBCODE$)                       ! Arma String de impresion			
!     Write #34 ; TS.TEMP1$          					                              ! Imprime cupon en EAN-128


!-- Impresion EAN 128
    BARCODE$  = Chr$(1DH) + Chr$(6BH) 					                            ! Manejo de impresion
    ENDBCODE$ = Chr$(00H) 						                                      ! codigos de barra
    TS.TEMP1$ = Chr$(1DH)+Chr$(77H)+Chr$(02H)+  		                       \! Horizontal barcode Size
                Chr$(1DH)+Chr$(68H)+Chr$(50)+                              \! Vertical barcode Size 
                Chr$(1DH)+Chr$(48H)+Chr$(02H)+                             \! Human Read characters
  	            Chr$(1DH)+Chr$(66H)+Chr$(01H)    	                          ! activado
     Call U.IMPRIME(TS.TEMP1$,4000H)				                                !
     TS.TEMP2$ = Right$("0000000000"+Str$(TS.TEMP5I4),10)                   ! Dato Impresion
     TS.TEMP1$ = (BARCODE$+Chr$(07H)+(XNRO$)+ENDBCODE$)           					! Arma String de impresion			
     Write #34 ; TS.TEMP1$          					                              ! Imprime cupon en PDF-417
     TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(0)				                        ! Ajusta impresion a la Izquierda
     Call U.IMPRIME(TS.TEMP1$,4000H)				                                !                    
!-- Fin impresion EAN 128

     Asic.Detalle% = 1																											! Dato de la trx actual
     TS.TEMP1$ = Linea.Detalle(38)  																				! Trailer del movimiento
     Call U.IMPRIME(TS.TEMP1$,4200H)                                        !
     TS.TEMP1$ = String$(37,"*")																						! Linea cierre voucher
     Call U.IMPRIME(TS.TEMP1$,4900H)																			  ! avance de papel
     Call U.CORTACR																												  ! Corta Papel

     Call visor.and.borrar("BONO A ENTREGAR POR VALOR "+str$(TS.TEMP5I4))		!
     Call U.Imprime("*----  B O N O  R E C O M P R A  ---*",2100H)		      ! Impresion Bono recompra
     CALL FORMAT.AMOUNT(TS.TEMP5I4)																					!
     Call U.IMPRIME("BONO GENERADO :",2100H)																!
     Call U.IMPRIME(XNRO$,2100H)																	          !
     Call U.IMPRIME("VALOR :"+TS.TEMP1$,2100H)															!
     Call U.IMPRIME("VENCE :"+Xdate1$+"-"+Xdate2$,2100H)     
     Call U.Imprime("*-----------------------------------*",2100H)		      ! Impresion Bono recompra
     TS.LINETYPE = 18																											  ! Genera cabecera 
     TS.LINEDATA = 1																											  ! en proximo tiquete
     Call TSPREC01																												  !
  EndIf																																			! Fin impresion del bono

End Sub 																																		! Fin Impresion Bono Recompra

Sub Local.Bono.Compra
String Xkey$, X.Tmp$
Integer*4 Xctl%, Xcont%
  TS.ER.RETURN = -1
  Open "R::$F:BLOCAL" KEYED RECL 17 AS 94              											! Apertura control local bonos
  If TS.ER.RETURN <> -1 Then Begin
  	Call VISOR.AND.BORRAR("ERROR OPEN TF:BLOCAL "+TS.TS11WERR$)             ! Msg de alerta
  	TS.ER.RETURN = 0                                     										! Ctrl Errores
    Exit Sub 
  EndIf                                                											!
  TS.TS11WERR$ = ""
  Xkey$ = Pack$(Right$("000000000000000000"+Gr.Lcl.Clte$,18))               ! Arma llave de consulta
  Read Form "C9 2I4";#94 KEY Xkey$; 																			 \! Busca cliente
       X.Tmp$,                                        										 \! Llave del registro
       Xcont%, 																												     \! Cantidad de Bonos
       Xctl%																																! Total de bonos 
  If TS.TS11WERR$ = "" Then Begin 																					! Encontrado
  	 If (Xcont% + 1) > Xctl% Then Begin 																		! Ya entrego localmente mas bonos
  	 	  Call Visor.And.Borrar("CLIENTE YA ALCANZO  NUMERO TOTAL BONOS")     ! Msg de alerta
  	 	  TS.ER.RETURN = 0 
  	 	  GoTo Fin.Local.Bono																									! Termina proceso
  	 EndIf Else Begin																												! No alcanza limite todavia
  	 	 Xcont% = Xcont% + 1																									! Numero de bono entregado
  	 EndIf																																	! 
  EndIf Else Begin 																													! No encontrado
     Xctl% = Val(Gr.BonCmp.Ptg$(7))																					! Total de Bonos
     Xcont% = 1																															! Numero de bono entregado
  EndIf
  Write Form "C9 2I4" ; #94;               											           \! Graba el registro
       Xkey$,                                                              \! Llave del registro     PD 9
       Xcont%, 																												     \! Cantidad de Bonos       I 4
       Xctl%																																! Total de bonos          I 4
  TS.ER.RETURN = -1																													! Proceso satisfactorio
  Fin.Local.Bono:
    Close 94
End Sub 

Function VALIDA.BONO.COMPRA
Integer*4 VALIDA.BONO.COMPRA
String    Xbuffer$, Xtemp4$, Xtrama$, XENVIA$,XLLEGA$, XSALE$, XRTA$, Xbono$
   VALIDA.BONO.COMPRA = 0																										! Init del proceso
   Call VISORES4690(1,"VALIDANDO CLIENTE   ","BONO RECOMPRA,ESPERE",0,"L")
   TS.TEMP5I4 = ROUND(FLOAT(Gr.BonCmp.Vlr%) * (0.+FLOAT(Gr.BonCmp.PtgT%)/100.),0,0) ! Calcula dscto a otorgar
   If TS.TEMP5I4 > Gr.BonCmp.Monto% Then TS.TEMP5I4 = Gr.BonCmp.Monto%
   Xbono$ = Left$(Gr.BonCmp.Nbon$,29)                                       ! Numero del bono para Base de datos
   Xbuffer$ = Right$("                 "+Gr.Lcl.Clte$,15)    +             \! ID del cliente             Asc 15
              Xbono$                                         +             \! Numero del bono            Asc 29
              Right$("0000000000"+Str$(TS.TEMP5I4),10)       +             \! Valor bono calculado       Asc 10
              Right$("000"+Gr.BonCmp.Ptg$(7),3)              +             \! Numero de bonos permitidos Asc 3
              Gr.BonCmp.Fini$                                +             \! Fecha inicial proyecto     Asc 6
              Gr.BonCmp.Ffin$                                +             \! Fecha final del proyecto   Asc 6
              Right$("0000000"+Gr.BonCmp.Ptg$(9),7)          +             \! Numero Id Bono             Asc 7
              Right$("00"+Gr.BonCmp.Ptg$(8),2)               +             \! Tipo de Bono               Asc 2
              Right$("00000000"+("20"+Gr.BonCmp.Ptg$(11)),8) +             \! Fecha inicio vigencia      Asc 8
              Right$("00000000"+("20"+Gr.BonCmp.Ptg$(12)),8)                ! Fecha final vigencia       Asc 8 
   Asc.Pay.Impr% = 1
   XTemp4$ = Armar.Trama.Msg("16","61",Xbuffer$,"00","0001","123456")       ! Armar trama MSG
   Xenvia$ = XTemp4$																											  !
   Xsale$  = DATE$ +":"+ Time$																							! Hora salida requerimiento
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)         ! Ejecuta Requerimiento
   XTEMP4$   = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   Xrta$     = DATE$ +":"+ Time$																						! Hora llegada requerimiento
   Xllega$   = Xtrama$																											!
   Call  BCOMP.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                    ! Rastro auditoria
   TS.ER.RETURN = -1																												! Control del proceso
   If Xtemp4$ <> "00" Then Begin 																						! Proceso No satisfactorio de java
   	  Gr.BonCmp.Oflne% = 0
   	  Call Local.Bono.Compra																							  ! Generar bono y encolar proceso
   	  If TS.ER.RETURN = -1 Then Gr.BonCmp.Oflne% = -1
   EndIf Else Begin																													! Comunicacion java exitosa 
    If Mid$(XTrama$,12,2) <> "00" Then Begin 																! Con Error de ejecucion 
    	Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))														! Presenta mensaje de Error 
    	 VALIDA.BONO.COMPRA = 0																								! Reporta falla en proceso
    	 Exit Function 																												! Sale del proceso
    EndIf	  																																!
   EndIf 																																		!

   If TS.ER.RETURN = -1 Then Begin 																					! Si proceso OK
   	If Gr.BonCmp.Oflne% = 0 Then Begin																			! Bono en linea
      Xbono$ = Left$(Gr.BonCmp.Nbon$,29)                                       ! Numero del bono para Base de datos
      Xbuffer$ = Xbono$                                      +             \! Numero del bono            Asc 30
                 Gr.BonCmp.Fini$                             +             \! Fecha inicial proyecto     Asc 6
                 Gr.BonCmp.Ffin$                             +             \! Fecha final del proyecto   Asc 6
                 Right$("0000000"+Gr.BonCmp.Ptg$(9),7)                      ! Numero Id Bono             Asc 6
      Asc.Pay.Impr% = 1
      XTemp4$ = Armar.Trama.Msg("16","62",Xbuffer$,"00","0001","123456")    ! Armar trama MSG
      Xenvia$ = XTemp4$																										  !
      Xsale$  = DATE$ +":"+ Time$																						! Hora salida requerimiento
      XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)      ! Ejecuta Requerimiento
      Xrta$     = DATE$ +":"+ Time$																					! Hora llegada requerimiento
      Xllega$   = Xtrama$																										!
      Call  BCOMP.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                 ! Rastro auditoria
      VALIDA.BONO.COMPRA = -1 
    EndIf Else Begin																												! Bono de contingencia
      TS.TEMP5I4 = ROUND(FLOAT(Gr.BonCmp.Vlr%) * (0.+FLOAT(Gr.BonCmp.PtgT%)/100.),0,0) ! Calcula dscto a otorgar
      If TS.TEMP5I4 > Gr.BonCmp.Monto% Then TS.TEMP5I4 = Gr.BonCmp.Monto%
      Xbono$ = Left$(Gr.BonCmp.Nbon$,29)                                    ! Numero del bono para Base de datos
      Xbuffer$ = Right$("                 "+Gr.Lcl.Clte$,15) +             \! ID del cliente             Asc 15
              Xbono$                                         +             \! Numero del bono            Asc 29
              Right$("0000000000"+Str$(TS.TEMP5I4),10)       +             \! Valor bono calculado       Asc 10
              Right$("000"+Gr.BonCmp.Ptg$(7),3)              +             \! Numero de bonos permitidos Asc 3
              Gr.BonCmp.Fini$                                +             \! Fecha inicial proyecto     Asc 6
              Gr.BonCmp.Ffin$                                +             \! Fecha final del proyecto   Asc 6
              Right$("0000000"+Gr.BonCmp.Ptg$(9),7)          +             \! Numero Id Bono             Asc 7
              Right$("00"+Gr.BonCmp.Ptg$(8),2)               +             \! Tipo de Bono               Asc 2
              Right$("00000000"+("20"+Gr.BonCmp.Ptg$(11)),8) +             \! Fecha vencimiento bono     Asc 8 
              Right$("00000000"+("20"+Gr.BonCmp.Ptg$(12)),8)                ! Fecha vencimiento bono     Asc 8 
      Asc.Pay.Impr% = 1
      XTemp4$ = Armar.Trama.Msg("16","64",Xbuffer$,"00","0001","123456")    ! Armar trama MSG
      Xenvia$ = XTemp4$																										  !
      Xsale$  = DATE$ +":"+ Time$																						! Hora salida requerimiento
      XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)      ! Ejecuta Requerimiento
      Xrta$     = DATE$ +":"+ Time$																					! Hora llegada requerimiento
      Xllega$   = Xtrama$																										!
      Call  BCOMP.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                 ! Rastro auditoria
      VALIDA.BONO.COMPRA = -1     																					! Proceso OK
    EndIf																																		! Fin encolamiento bono contingencia
   EndIf Else  VALIDA.BONO.COMPRA = 0																			  ! No tiene derecho a Bono
    	
End Function 

Sub CALCULO.BONO.COMPRA																											! Calculo del valor del bono recompra
  Integer*4 PRIC%, Xvta%, X.K%, Sp4%, Xvtasec%															!
  Integer*2 Xp1%, Xp2%, X.TARIFA%, XL%, SP2%																!
  String    X.TMP$, Xpago$, Xclte$, Xstruc$, Xnada$, Xsgn$									!
  String    SC1$, SC2$, SC3$, SC4$, Xitm$, Xsec$
  Xvta% = 0                                            											! Total Venta
  Xvtasec% = 0
  Dim Gr.BonCmp.Tipo$(7)                               											! Tipos de clientes
  Dim Gr.BonCmp.Ptg$(14)                               											! Ptg Asignados
  Dim Gr.BonCmp.Msg$(5)                                											! Mensajes parametro
  TS.ER.RETURN = -1                                    											! Ctrl Errores
  TS.TS11WERR$ = ""																													!
  Open "R::$F:PBONOS" KEYED RECL 67 AS Gr.BonCmp.Ses% UNLOCKED NOWRITE NODEL ! Parametros para bono recompra
  If TS.ER.RETURN <> -1 Then Begin                     											! Error apertura archivo
  	Call VISORES4690(1,"ERROR TF:PBONOS","COD "+TS.TS11WERR$,1200,"L")						!
 	 	Gr.BonCmp.Vlr% = 0                                 											! No aplica dscto en la trx
 	  Exit Sub                                           											! Sale del proceso
  EndIf                                                											!
  TS.TS11WERR$ = ""																													!
  TS.ER.RETURN = -1
  Read Form "C4 2C3 5C4 5C2 C1 C3 C2 C6 C4 2C3 C1 C4";#Gr.BonCmp.Ses% KEY "9998"; \!
       X.Tmp$,                                                             \! Llave del proyecto
       Gr.BonCmp.Fini$,                                                    \! Fecha inicial proyecto
       Gr.BonCmp.Ffin$,                                                    \! Fecha final del proyecto
       Gr.BonCmp.Tipo$(1),                                                 \! Tipificacion de cliente 1
       Gr.BonCmp.Tipo$(2),                                                 \! Tipificacion de cliente 2
       Gr.BonCmp.Tipo$(3),                                                 \! Tipificacion de cliente 3
       Gr.BonCmp.Tipo$(4),                                                 \! Tipificacion de cliente 4
       Gr.BonCmp.Tipo$(5),                                                 \! Tipificacion de cliente 5
       Gr.BonCmp.Ptg$(1),                                                  \! Porcentaje descuento 1
       Gr.BonCmp.Ptg$(2),                                                  \! Porcentaje descuento 2
       Gr.BonCmp.Ptg$(3),                                                  \! Porcentaje descuento 3
       Gr.BonCmp.Ptg$(4),                                                  \! Porcentaje descuento 4
       Gr.BonCmp.Ptg$(5),                                                  \! Porcentaje descuento 5
       Gr.BonCmp.Ptg$(6),                                                  \! Tipo validacion 1.Itm 2.Dpto 3.Todo
       Gr.BonCmp.Ptg$(7),                                                  \! Numero de bonos a imprimir
       Gr.BonCmp.Ptg$(8),                                                  \! Tipo de bono
       Gr.BonCmp.Ptg$(9),                                                  \! Consecutivo bono
       Gr.BonCmp.Ptg$(10),                                                 \! Valor maximo bono
       Gr.BonCmp.Ptg$(11),                                                 \! Fecha Vigencia inicial Bono
       Gr.BonCmp.Ptg$(12),                                                 \! Fecha Vigencia final Bono
       Gr.BonCmp.Ptg$(13),                                                 \! Forma aplicacion bono 0.proxima compra 1.dentro de la compra
       Gr.BonCmp.Ptg$(14)                                                   ! Valor minimo para generacion de bonos

  If TS.ER.RETURN <> -1 Then Begin                     											! No existe registro control
 	 	Gr.BonCmp.Fini$ = "000000"
 	 	Gr.BonCmp.Ffin$ = "000000"
 	 	Gr.BonCmp.Monto% = 0
 	 	Gr.BonCmp.MonMin% = 0
 	 	Call U.IMPRIME("ERROR PARAMETROS BONO RECOMPRA",2100H)
 	 	Call VISOR.AND.BORRAR("ERROR PARAMETROS BONO RECOMPRA")
 	 	GoTo Proc.Bono.Compra
  EndIf Else Begin                                     !
 	 	Gr.BonCmp.Fini$ = UnPack$(Gr.BonCmp.Fini$)
 	 	Gr.BonCmp.Ffin$ = UnPack$(Gr.BonCmp.Ffin$)
 	 	Gr.BonCmp.Monto% = Val(Unpack$(Gr.BonCmp.Ptg$(10)))
 	 	Gr.BonCmp.MonMin% = Val(Unpack$(Gr.BonCmp.Ptg$(14)))
 	 	Gr.BonCmp.Ptg$(11) = Unpack$(Gr.BonCmp.Ptg$(11))
 	 	Gr.BonCmp.Ptg$(12) = Unpack$(Gr.BonCmp.Ptg$(12))
  EndIf
  TS.ER.RETURN = -1
  Read Form "C4 C37 C26";#Gr.BonCmp.Ses% KEY "9995";                       \! Primer Mensaje definido 
       X.Tmp$,                                                             \! Llave del proyecto       
       Gr.BonCmp.Msg$(1),                                                  \! Fecha inicial proyecto   
       Xnada$																																!
  If TS.ER.RETURN <> -1 Then Begin
 	 	Gr.BonCmp.Fini$ = "000000"
 	 	Gr.BonCmp.Ffin$ = "000000"
 	 	Gr.BonCmp.Monto% = 0
 	 	Gr.BonCmp.MonMin% = 0
 	 	Call U.IMPRIME("ERROR PARAMETROS BONO RECOMPRA",2100H)
 	 	GoTo Proc.Bono.Compra
  EndIf
  TS.ER.RETURN = -1
  Read Form "C4 C37 C26";#Gr.BonCmp.Ses% KEY "9996";                       \! Segundo Mensaje definido 
       X.Tmp$,                                                             \! Llave del proyecto       
       Gr.BonCmp.Msg$(2),                                                  \! Fecha inicial proyecto   
       Xnada$																																!
  If TS.ER.RETURN <> -1 Then Begin
 	 	Gr.BonCmp.Fini$ = "000000"
 	 	Gr.BonCmp.Ffin$ = "000000"
 	 	Gr.BonCmp.Monto% = 0
 	 	Gr.BonCmp.MonMin% = 0
 	 	Call U.IMPRIME("ERROR PARAMETROS BONO RECOMPRA",2100H)
 	 	GoTo Proc.Bono.Compra
  EndIf
  TS.ER.RETURN = -1
  Read Form "C4 C37 C26";#Gr.BonCmp.Ses% KEY "9997";                       \! Tercer Mensaje definido
       X.Tmp$,                                                             \! Llave del proyecto     
       Gr.BonCmp.Msg$(3),                                                  \! Fecha inicial proyecto 
       Xnada$																																!
  If TS.ER.RETURN <> -1 Then Begin
 	 	Gr.BonCmp.Fini$ = "000000"
 	 	Gr.BonCmp.Ffin$ = "000000"
 	 	Gr.BonCmp.Monto% = 0
 	 	Gr.BonCmp.MonMin% = 0
 	 	Call U.IMPRIME("ERROR PARAMETROS BONO RECOMPRA",2100H)
 	 	GoTo Proc.Bono.Compra
  EndIf

  Proc.Bono.Compra:
  Close Gr.BonCmp.Ses%																 											! cierra sesion temporal
  
  If Gr.BonCmp.Monto% = 0 Then Begin
  	 Gr.BonCmp.Vlr% = 0
  	 Exit Sub 
  EndIf
  
  If Val(date$) >= Val(Gr.BonCmp.Fini$) Then Begin     											! Es una fecha mayor o igual a la inicial
   If Val(date$) >  Val(Gr.BonCmp.Ffin$) Then Begin    											! Es una fecha mayor a la fecha final 
 	 	  Gr.BonCmp.Vlr% = 0                               											! No aplica dscto en la trx
 	    Exit Sub                                         											! Sale del proceso
   EndIf
  EndIf Else Begin																		 											! Es fecha menor a la inicial
 	 	Gr.BonCmp.Vlr% = 0                                 											! No aplica dscto en la trx
 	  Exit Sub                                           											! Sale del proceso
  EndIf
  TS.ER.RETURN = -1                                    											! Ctrl Errores
  TS.TS11WERR$ = ""
  Open "R::$F:COMPRA" KEYED RECL 7 AS Gr.BonCmp.Ses% UNLOCKED NOWRITE NODEL ! Articulos para bono recompra
  If TS.ER.RETURN <> -1 Then Begin
  	Call VISORES4690(1,"ERROR DE APERTURA","TF:COMPRA",1200,"L")						!
 	 	Gr.BonCmp.Vlr% = 0                                 											! No aplica dscto en la trx
 	  Exit Sub                                           											! Sale del proceso
  EndIf                                                											!
  Gr.BonCmp.Vlr% = 0                                   											! Init variable
  For I% = 1 TO SL.END                                 											! FOR ALL StringS
    H$ = READ.SL.STR$(I%)                             											! GET String
    If LEN(H$) > 5 Then                                                     \ ASSURE GOOD String
    If ASC(H$) = 1 Then Begin                         											! ITEM ENTRY String
     X.TMP$ = ASIC.GETUNPK(H$,3)	   				           											! Tomo informacion del item vendido
     X.Tmp$ = Right$("000000000000"+X.Tmp$,12)         											! Plu a validar
     Xitm$ = ";" + Str$(Val(X.Tmp$)) + ";"                                  ! arma llave busqueda
     If Match(Xitm$,Gr.BonCmp.Pludomi$,1) > 0 Then                         \! Si vta domic en trx
     	  Gr.BonCmp.VtaDomic% = -1																						! reporta venta domicilio
     X.Tmp$ = Pack$( X.tmp$ )                          											! Arma llave de busqueda
     TS.ER.RETURN = -1																 											! Ctrl errores
     Call IRRFEC.READ01 (X.tmp$, 4, TS.TEMP1$, 0) 		 											! Lectura Datos Itemr
     Call IRRFEC.SPLIT1 ( TS.TEMP1$ ) 			           											! Entrega datos al eamitemr.j86
     Xstruc$ = Left$(IR.USERDATA$,16)		  					   											! Estructura comercial
     Xstruc$ = Mid$(Xstruc$,2,9)                       											! Toma los primeros 9 digitos 
     Xsec$ = Left$(Xstruc$,3)																								! toma seccion de venta
     If Xstruc$ = "         " Then GoTo Next.Itm.Compra 										! Error en estructura comercial no procesa
!--- Busqueda excluido de bonos recompra
     TS.ER.RETURN = -1                                 											! Ctrl de Error
     X.tmp$ = Pack$("02") + IR.ITEMCODE$         											      ! Consulta si Item excluido
     Read Form "C7";#Gr.BonCmp.Ses% KEY X.Tmp$; X.Tmp$ 									    ! Lee archivo      
     If TS.ER.RETURN = -1 Then GoTo Next.Itm.Compra 										    ! Item excluido para calculo bono recompra
!--- retorna en TS.ER.RETURN
     TS.ER.RETURN = -1                                 											! Ctrl de Error
     TS.TS11WERR$ = ""                                 											!
     If Gr.BonCmp.Ptg$(6) <> "3" Then Begin            											! Validacion por Item o estrucura comercial
     	  If Gr.BonCmp.Ptg$(6) = "1" Then               								     \! Validacion por Item
     	  	 X.tmp$ = Pack$("01") + IR.ITEMCODE$         											! 
     	  If Gr.BonCmp.Ptg$(6) = "2" Then               								     \! Validacion por Estructura comercial
     	  	 X.tmp$ = Pack$("01"+Right$("000000000000"+Xstruc$,12)) 					! 
     	  TS.TS11WERR$ = ""
     	  TS.ER.RETURN = -1                                 								  ! Ctrl de Error
        Read Form "C7";#Gr.BonCmp.Ses% KEY X.Tmp$; X.Tmp$ 									! Lee archivo 
        If TS.ER.RETURN <> -1 Then BEGIN                  									! Not found 
     	     GoTo Next.Itm.Compra                           									! Siguiente Item vendido
        ENDIF 
     EndIf
     X.TARIFA% = Bono.Tarifa.Iva(IR.INDICAT1)          											! Tarifa Impuesto
     L% = ASC(RIGHT$(H$,1)) / 10H                      											! GET ITEM TYPE
     If L% = 0 OR L% = 8 Then Begin                    											! Item SALE OR CANCEL
      PRIC% = 0                                        											! ASSUME NO PRICE
      K% = MATCHB(":",H$,3) + 1                        											! START OF PRICE
      J% = MATCHB(":",H$,K%)                           											! END OF PRICE
      If J% > K% Then Begin                            											! PRICE FOUND
       PRIC% = PACKBIN4(H$,K%-1,J%-K%)                 											! GET PRICE
       PRIC% = ROUND(FLOAT(PRIC%)/(1.+FLOAT(UE.TABLAIVA(X.TARIFA%))/100.),0,0) ! Calcula valor Base
       If L% = 8 Then PRIC% = 0 - PRIC%                											! INVERT PRICE FOR VOID
       Xvta% = Xvta% + PRIC%                           											! Acumula venta
       If Match(Xsec$,Gr.BonCmp.Secc$,1) > 0 Then                          \! Si vta secciones
     	    Xvtasec% = Xvtasec% + Pric%
      EndIf                                            											! PRICE FOUND
     EndIf                                             											! ITEM SALE OR CANCEL
    EndIf                                              											! Item ENTRY String
    
    If ASC(H$) = 153 Then Begin                                             ! Lectura User data
    	 Asc.Tmp.Apun% = 3
    	 X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                            ! Codigo del proyecto
    	 If X.tmp$  = "97" Then Begin																					! Descuento motor de promociones
    	 	  IR.ITEMCODE$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                  ! codigo del producto
          IR.ITEMCODE$ = Pack$(Right$("000000000000"+IR.ITEMCODE$,12))      ! Ajsuta Item para consulta
!--- Control de Item excluido 29Ago2022    	 	  
          TS.ER.RETURN = -1                            											! Ctrl de Error
          X.tmp$ = Pack$("02") + IR.ITEMCODE$    											      ! Consulta si Item excluido
          Read Form "C7";#Gr.BonCmp.Ses% KEY X.Tmp$; X.Tmp$ 						    ! Lee archivo      
    	 	  If TS.ER.RETURN = -1 Then GoTo Next.Itm.Compra 			  				    ! Item excluido para calculo bono recompra
    	 	  PRIC%   = Val(ASIC.GETUNPK(H$,Asc.Tmp.Apun%))                     ! Valor dscto Promocion
    	 	  X.TMP$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                        ! codigo de la promocion
    	 	  X.TARIFA% = Val(ASIC.GETUNPK(H$,Asc.Tmp.Apun%))                   ! flag del iva 
    	 	  X.TMP$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                        ! codigo del departamento SA
    	 	  Xstruc$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                        ! estructura comercial 
          Xstruc$ = Left$(Xstruc$,9)                                        ! Toma 9 posiciones iniciales
    	 	  Xsgn$   = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                        ! signo de la operacion

          If Gr.BonCmp.Ptg$(6) <> "3" Then Begin                            ! Validacion por Item o estrucura comercial
     	     If Gr.BonCmp.Ptg$(6) = "1" Then                                 \! Validacion por Item                      
     	  	    X.tmp$ = Pack$("01") + IR.ITEMCODE$                           !                                          
     	     If Gr.BonCmp.Ptg$(6) = "2" Then                                 \! Validacion por Estructura comercial      
     	  	    X.tmp$ = Pack$("01"+Right$("000000000000"+Xstruc$,12))        ! 
     	  	 TS.ER.RETURN = -1
           Read Form "C7";#Gr.BonCmp.Ses% KEY X.Tmp$; sc1$                  ! Lee archivo           
           If TS.ER.RETURN = -1 Then BEGIN                                  ! Si found
              PRIC% = ROUND(FLOAT(PRIC%)/(1.+FLOAT(UE.TABLAIVA(X.TARIFA%))/100.),0,0) ! Calcula valor Base
              If Xsgn$ = "01" Then PRIC% = 0 - PRIC%                        ! INVERT PRICE FOR VOID             
              Xvta% = Xvta% - PRIC%                                         ! Resta valor dscto a la Acumulacion venta   
              If Match(Xsec$,Gr.BonCmp.Secc$,1) > 0 Then                   \! Si vta secciones
     	           Xvtasec% = Xvtasec% - Pric%
                                
           EndIf
          EndIf Else Begin  																								!
              PRIC% = ROUND(FLOAT(PRIC%)/(1.+FLOAT(UE.TABLAIVA(X.TARIFA%))/100.),0,0) ! Calcula valor Base
              If Xsgn$ = "01" Then PRIC% = 0 - PRIC%                        ! INVERT PRICE FOR VOID             
              Xvta% = Xvta% - PRIC%                                         ! Resta valor dscto a la Acumulacion venta                     
              If Match(Xsec$,Gr.BonCmp.Secc$,1) > 0 Then                          \! Si vta secciones
     	           Xvtasec% = Xvtasec% - Pric%

          EndIf																															!
    	 EndIf																																! Fin dscto motor de promociones
    EndIf																																		! 
    Next.Itm.Compra:                                   											! Siguiente Item vendido
  Next I%                                              											! NEXT String
  Close Gr.BonCmp.Ses%                                 											! Close File
  Gr.BonCmp.Vlr% = Xvta%                               											! Retorna total de venta en la trx
! Mod 29Ago2022
  If Gr.BonCmp.Vlr% < Gr.BonCmp.MonMin% Then Begin													! Si valor calculado menor a monto minimo
  	 !Call visor.and.borrar("VALOR DE COMPRA NO  APLICA PARA BONO")
  	 Gr.BonCmp.Vlr% = 0																											! reporta no generacion de bono
  EndIf																																			! fin control monto minimo generacion
  
  Gr.BonCmp.PtgT% = 0                                  											! Init variable
  If Gr.BonCmp.Vlr% > 0 Then Begin										 											! Hay Ventas de productos seleccionados
  	 Xclte$ = ""
  	 Gr.BonCmp.PtgT% = 0
  	 For XL% = 1 To 5 																											! Recorre lista de tipificaciones
  	   X.K% = MATCH(Gr.BonCmp.Tipo$(XL%),Gr.Lcl.TipClte$,1)  							  ! compara primer tipo de cliente
  	   If X.K% > 0 Then Begin                                               ! Encontro Clasificacion
  	   	  If Val(Gr.BonCmp.Ptg$(XL%)) > Gr.BonCmp.PtgT% Then Begin          ! Encuentra mejor beneficio
  	   	  	 Gr.BonCmp.PtgT% = Val(Gr.BonCmp.Ptg$(XL%))                     ! Asigna Porcentaje aplicar
  	   	  	 Xclte$ = Gr.BonCmp.Tipo$(XL%)                                  ! Asigna tipificacion cliente
  	   	  EndIf
  	   EndIf
  	 Next XL%
  	
  	 If Gr.BonCmp.PtgT% = 0 Then Begin                                      ! No hay ningun descuento
 	 	    Gr.BonCmp.Vlr% = 0                                                  ! No aplica dscto en la trx
 	      Exit Sub                                                            ! Sale del proceso
  	 EndIf

!--- Add 7Jul2022
!--- Add 21Mzo2025 Se reporta valor en bono cero(0) con vta domicilio
     If Gr.BonCmp.VtaDomic% = -1 Then Begin                                 ! Si venta plu domicilios
!     	Gr.BonCmp.Vlr% = Gr.BonCmp.Vlr% - Xvtasec%													  ! Recalcula valor bono
!     	If (Gr.BonCmp.Vlr% <= 0 ) Then Begin                                  ! Si no hay venta
	
 	 	     Gr.BonCmp.Vlr% = 0                                                 ! No aplica dscto en la trx
 	       Exit Sub                                                           ! Sale del proceso

!     	EndIf																																	! fin vta secciones controladas

     EndIf																																	! fin venta domicilios
     
!    Gr.BonCmp.Nbon$ = Right$("000000000000000"+Gr.Lcl.Clte$,15) +          \! Id del cliente
!             "20"+ Date$ + TIME$                                            ! Fecha y hora de la trx

!-- Add 27Nov2019 - Nueva estructura bono
!    Gr.BonCmp.Nbon$ = Gr.BonCmp.Prefijo$ +                                 \! Prefijo del bono       Asc  2
!                      Right$("0000000"+Gr.BonCmp.Ptg$(9),7)  +             \! Nro del evento         Asc  7
!                      Right$("00"+Gr.BonCmp.Ptg$(8),2)       +             \! Tipo de bono           Asc  2
!                      (Date$ + TIME$)                        +             \! aammddhhmmss           Asc 12
!                      Gr.BonCmp.Ptg$(11)                     +             \! Fecha inicio vigencia  Asc  6
!                      Gr.BonCmp.Ptg$(12)                                    ! Fecha final  vigencia  Asc  6

!--- Add 10Ene20 - Ajuste estructura por duplicado
    Gr.BonCmp.Nbon$ = Gr.BonCmp.Prefijo$ +                                 \! Prefijo del bono       Asc  2
                      Right$("000"+TS.STORE$,3)              +             \! Numero del almacen     Asc  3
                      Right$("000"+Str$(SL.HD.TERMINAL),3)   +             \! Numero de terminal     Asc  3
                      Right$("000"+Str$(SL.HD.TRANSNUM+1),3) +             \! Numero de transaccion  Asc  3
                      (Date$ + TIME$)                        +             \! aammddhhmmss           Asc 12
                      Gr.BonCmp.Ptg$(11)                     +             \! Fecha inicio vigencia  Asc  6
                      Gr.BonCmp.Ptg$(12)                                    ! Fecha final  vigencia  Asc  6

    TS.TEMP4I4 = VALIDA.BONO.COMPRA
    If TS.TEMP4I4 <> -1 Then Begin																					! Si no genera mensaje
 	 	    Gr.BonCmp.Vlr% = 0                                                  ! No aplica dscto en la trx
 	      Exit Sub                                                            ! Sale del proceso
    EndIf
     
     TS.TEMP1$ = PACK$(Gr.Lcl.Clte$)           +                   		     \! Id del cliente
		    ":"+PACK$(STR$(Gr.BonCmp.Vlr%))  			 +	               				   \! Valor de Compra
		    ":"+PACK$(STR$(Gr.BonCmp.PtgT%)) 			 +	                         \! Dscto Aplicado
        ":"+Xclte$                             +                           \! Tipificacion de cliente 
        ":"+Pack$(Gr.BonCmp.Nbon$)             +                           \! Numero del Bono
        ":"+Pack$(Gr.BonCmp.Ptg$(6))           +                           \! Tipo Validacion
        ":"+Pack$(Gr.BonCmp.Ptg$(9))           +                           \! Serial bono
		    ":"+"00"			                             													! Filler 

      Call Grabacion.String.Usuario2("20140114",TS.TEMP1$)                  ! Graba String de Usuario

  EndIf Else Begin
  	TS.TEMP4I4 = 0
    Gr.BonCmp.PtgT% = 0 
 	 	Gr.BonCmp.Vlr% = 0
 	 	Exit Sub   
  EndIf

End Sub 																																		! Fin bono recompra

Sub BONOCOMPRAS(XOPT%) Public			  																				! Bonos Recompra
Integer*4 Xopt%                                                             !
String    Xtmp$

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.BonCmp.Ok%  = 0                                                        ! Proyecto Apagado
  Gr.BonCmp.Stat% = 0																												! Init variable
  Gr.BonCmp.Vlr%  = 0																											  ! Valor bono recompra
  Gr.BonCmp.Monto% = 0																											! Monto maximo bono recompra
  Gr.BonCmp.MonMin% = 0																											! Monto minimo bono recompra
  TS.ER.RETURN = -1																													! Ctrl Errores                     
  TS.TS11WERR$ = ""
  OPEN "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL 													  ! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then BEGIN                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA BONO RECOMPRA")									! MSg alerta
  	 Exit Sub 																															! Sale del proceso
  ENDIF 																																		!
  IF END #94 THEN UE.FIN.BONOCMP        																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        IF TS.TEMP1$ = "[BONOS RECOMPRA]" Then Begin		  	         				! Bonos Recompra
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.BonCmp.Ok%   = Val(Mid$(TS.TEMP1$,30,2))   				    					! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.BonCmp.Ses%  = Val(Mid$(TS.TEMP1$,30,02))      				          ! Numero Sesion Temporal
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.BonCmp.Prefijo$= Mid$(TS.TEMP1$,30,02)     				              ! Prefijo del bono
         Read #94; TS.TEMP1$     																						! Lectura registro
         Gr.BonCmp.Secc$ = Mid$(TS.TEMP1$,30,30)     				                ! Secciones controladas
         Read #94; TS.TEMP1$     																						! Lectura registro
         Gr.BonCmp.Pludomi$ = Mid$(TS.TEMP1$,30,50)     				            ! PLU Domicilios
         
         GoTo UE.FIN.BONOCMP 																								! Sale del ciclo de carga          
       Endif                                                                !
   Wend                                                                     !
   UE.FIN.BONOCMP:                                                          !
     Close 94																																! Cierra archivo

   If Gr.BonCmp.Ok% = -1 Then                                              \! Proyecto Activo
      Call U.IMPRIME("MODULO BONO RECOMPRA ON  21-Mzo-2025",2100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO BONO RECOMPRA OFF 21-Mzo-2025",2100H)          ! Msg Proyecto Cargado
EndIf 																																			! Fin carga opciones

If Gr.BonCmp.Ok% <> -1 Then Exit Sub                                        ! Si proyecto apagado

!--- EAMTSU02.J86
If Xopt% = 02 Then Begin                                                    ! Inicio de transaccion
	 Call IMPRESION.BONO.COMPRA																								! Generación Bono Recompra
	 Gr.BonCmp.Stat% = 0																										  ! Init variable
	 Gr.BonCmp.Vlr%  = 0																											! Valor bono recompra
   Gr.BonCmp.VtaDomic% = 0                                                  ! Control venta domicilios
   Gr.BonCmp.VtaSecc%  = 0                                                  ! Control venta secciones
   Gr.BonCmp.VtaOtrSecc% = 0
EndIf																																				! Fin Inicio de transaccion

!--- EAMTSU20.J86
If Xopt% = 20 Then Begin                                                    ! Impresion en CR

  If MATCH(TS.SDESC$(59),TS.PRTBUF$,1) > 0 THEN BEGIN												! Si anulacion total de la trx
  	 Gr.BonCmp.Stat% = 1 																										! Para no ejecutar proceso calculo
  EndIf																																			!

 If Not TS.TRAINING Then Begin                       												! Si no en entrenamiento
  If TS.INTRX Then                                  											 \! Si esta en una compra
    If TS.LINETYPE = 6 And                                                 \! Store Data y Transacc
       TS.LINEDATA = 1 Then Begin                    											  ! Fecha, Hora, etc
        If TS.TENDERED (0) <> 0 Or                                         \! Si hay pagos
           TS.TRX.STATUS <> 100 Then Begin                                  ! y NO hay VOID en curso
           If Gr.BonCmp.Stat% = 0	Then Begin																! Init variable
            If USER.FBACT.READ And Gr.Lcl.Ctrl% <> 0 Then Begin							! Si cliente y no anul trx
           	   Call CALCULO.BONO.COMPRA                                     ! Calcula el valor del bono
           	EndIf
           	Gr.BonCmp.Stat% = -1	  																			  ! Proceso Ejecutado
           EndIf																														!
        EndIf
     EndIf
  EndIf
  
EndIf																																				! Fin impresion CR


End Sub 
