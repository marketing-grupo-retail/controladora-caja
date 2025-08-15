!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRBCFPAG.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Generación Bonos Recompra        *
!                Medio de pago 
!**************************************************
!Observaciones:
! Version 1.0 20 enero 2016
!--------------------------------------------------
! Mod 18May2017
! Se adiciona en archivo de parametros de bono recompra
! la definición del valor maximo de entrega del bono
! desarrollado Grupo Retail - OVS
!--------------------------------------------------
! Mod 30 May 2018
! Se ajusta funcionalidad para manejo dscto por 
! diferentes formas de pago y calculos con nuevo 
! motor de promociones.
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
! Mod 10Ene2020
! Se modifica la estructura del bono recompra con la
! siguiente estructura:
! 23 + centro costo (3) + terminal(3) + transaccion(4) +
! fecha y hora(12)+ tipo bono(2) + "999" relleno total
! 29 caracteres.
! Se ajusta por problema de duplicidad de bono reportado
! desarrollado Grupo Retail - OVS/CAC
!--------------------------------------------------
! Mod 10Ago2020
! Se ajusta impresion bonos de formato PD417 a formato
! EAN-128.
! desarrollado Grupo Retail - OVS
!--------------------------------------------------
! Mod 10Sep2021
! Se implementa control para no generación de bono
! cuando en la trx se presenta pago con el tipo y 
! variedad 34 - Bono Electrofiesta
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
! Mod 23Jun2023
! Ajuste para la validación de parámetros bono recompra, si estan incompletos
! no permite la generación de los bonos.
! desarrollado Grupo Retail - OVS
!--------------------------------------------------
! Mod 21Mzo2025
! Se ajusta procedimiento para que en la transaccion si hay un plu de
! domicilio registrado, no genere bono recompra
! desarrollado Grupo Retail - OVS
!--------------------------------------------------

%ENVIRON T		                          																		! Ambiente de terminal

Integer*1 Global Gr.BonPag.Ok%,                                            \! Control del proyecto
                 Gr.BonPag.Stat%,                                          \! Control de proceso de calculo compra
                 Gr.BonPag.Oflne%,                                         \! Bono recompra Offline
                 Gr.BonPag.VtaDomic%,                                      \! Control venta domicilios
                 Gr.BonPag.VtaSecc%,                                       \! Control venta secciones
                 Gr.BonPag.VtaOtrSecc%																			!
                 
String    Global Gr.BonPag.Tipo$(1),                                       \! Tipificaciones
                 Gr.BonPag.Ptg$(1),                                        \! Porcentaje Dscto 
                 Gr.BonPag.Msg$(1),                                        \! Mensaje a imprimir
                 Gr.BonPag.Fini$,   																			 \! Fecha inicial 
                 Gr.BonPag.Ffin$,  																				 \! Fecha final 
                 Gr.BonPag.Prefijo$,                                       \! Prefijo bonos recompra
                 Gr.BonPag.Nbon$   																				  ! Nro Bono Generado

Integer*2 Global Gr.BonPag.PtgT%                                            ! Porcentaje Aplicar
Integer*2 Global Gr.BonPag.Ses%          																	  ! Sesion temporal
Integer*4 Global Gr.BonPag.Vlr%,                                           \! Valor Bono recompra
                 Gr.BonPag.VlrMin%                                          ! Valor minimo compra generacion
Integer*4 Global Gr.BonPag.Ptv%                                             ! Valor pago tipo y variedad
Integer*4 Global Asc.Pay.Impr%                                              !
Integer*4 Global Asc.Tmp.Apun%                                              ! Apuntador strings
Integer*1 Global Asic.Detalle%																							! Ctrl trailer trx
String    Global Gr.Lcl.Name$																								! Nombre del cliente
String    Global Gr.Lcl.Clte$                                               ! ID del cliente
Integer*1 Global Gr.Lcl.Ctrl%                                               ! Ctrl trx Lealtad
String    Global Gr.Lcl.TipClte$                                            ! Tipificaciones de clientes
String    Global IR.USERDATA$                                               ! Datos adicionales Itemr
Integer*4 Global UE.TABLAIVA(1)                                             ! Tarifas de IVA
Integer*4 Global Gr.BonPag.Monto%																						! Valor maximo bono recompra
String    Global Gr.BonPag.Secc$,                                          \! Seccion controladas
                 Gr.Lcl.IdClte$,                                           \!
                 Gr.BonPag.Pludomi$                                         ! PLU Domicilios       
!-- Adicion para manejo de bines TEF
String    Global Gr.Bonpag.Bines$(2)
Integer*2 Global Gr.Bonpag.NumBin%

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


Sub BCOMP.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$) External
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$,      \!
       X.RTA$, X.BUFF$, XDate1$, Xdate2$
Integer*4 X.Len%
End Sub 

Sub IMPRESION.BONO.COMPRA.PAGO																							! Impresion Bono Recompra
String Xnro$, BARCODE$, ENDBCODE$, XDATE1$, XDATE2$
Integer*2 X2%, Xctl%
  If Gr.BonPag.Vlr% > 0 Then Begin					     														! Hay Bono para Impresion
     !To.USEREXIT(20) = 0																									  ! Cancela lineas impresion
     !To.USEREXIT(60) = 0																									  !
     TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(11H)
     TS.TEMP1$ = TS.TEMP1$ + "$*$  B O N O  R E C O M P R A  $*$"          !
     Call U.Imprime(TS.TEMP1$,4100H)		                                    ! Impresion Bono recompra
     TS.TEMP1$ = "FECHA   : 20"+Left$(DATE$,2) + "-" + Mid$(DATE$,3,2) + "-" + \!
                 Right$(DATE$,2)
     TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												! Ajusta Dato impresion
     Call U.IMPRIME(TS.TEMP1$,4100H)																			  ! Imprime dato
     Xnro$ = "" : Xctl% = 0

     Xnro$ = Gr.BonPag.Nbon$                                                ! Numero del bono

!     TS.TEMP1$ = LEFT$("BONO : "+Left$(Xnro$,29)+STRING$(37," "),37)  		  ! Ajusta Dato impresion
!     Call U.IMPRIME(TS.TEMP1$,4100H)																			  ! Imprime dato

     Xdate1$ = "20" + Gr.BonPag.Ptg$(12)                                     ! Fecha vencimiento
     Xdate1$ = Left$(Xdate1$,4)  + "/" +                                     \! Arma formato fecha
              Mid$(Xdate1$,5,2) + "/" +                                     \! para impresion
              Mid$(Xdate1$,7,2)         
     Xdate2$ = "20" + Gr.BonPag.Ptg$(13)                                     ! Fecha vencimiento
     Xdate2$ = Left$(Xdate2$,4)  + "/" +                                     \! Arma formato fecha
              Mid$(Xdate2$,5,2) + "/" +                                     \! para impresion
              Mid$(Xdate2$,7,2)         
                                                   !
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
     TS.TEMP1$ = (BARCODE$+Chr$(07H)+XNRO$+ENDBCODE$)                       ! Arma String de impresion			
     Write #34 ; TS.TEMP1$          					                              ! Imprime cupon en PDF-417
     TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(0)				                        ! Ajusta impresion a la Izquierda
     Call U.IMPRIME(TS.TEMP1$,4000H)				                                !                    
!-- Fin EAN 128

     TS.TEMP5I4 = ROUND(FLOAT(Gr.BonPag.Vlr%) * (0.+FLOAT(Gr.BonPag.PtgT%)/100.),0,0) ! Calcula dscto a otorgar
     If TS.TEMP5I4 > Gr.BonPag.Monto% Then TS.TEMP5I4 = Gr.BonPag.Monto%    ! Maximo bono parametro 4 mayo 2017
     CALL FORMAT.AMOUNT(TS.TEMP5I4)																					!
     TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(11H)+"VALOR BONO :$"+TS.TEMP1$  	! Valor del Bono

     !TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												! Ajusta Dato impresion

     Call U.IMPRIME(TS.TEMP1$,4200H)																			  ! Imprime dato
     TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(00H)+" "
     Call U.IMPRIME(TS.TEMP1$,4000H)																			  ! Imprime dato

     If Gr.BonPag.Msg$(1) <> "" Then Begin
        TS.TEMP1$ = Gr.BonPag.Msg$(1)																				! Mensaje 1
        TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)											! Ajusta Dato impresion
        Call U.IMPRIME(TS.TEMP1$,4101H)																		  ! Imprime dato
     EndIf
     If Gr.BonPag.Msg$(2) <> "" Then Begin
        TS.TEMP1$ = Gr.BonPag.Msg$(2)																				! Mensaje 2
        TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)											! Ajusta Dato impresion
        Call U.IMPRIME(TS.TEMP1$,4101H)																		  ! Imprime dato
     EndIf
     If Gr.BonPag.Msg$(3) <> "" Then Begin
        TS.TEMP1$ = Gr.BonPag.Msg$(3)																				! Mensaje 3
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
     
!-- Impresion PDF-417
!     BARCODE$  = Chr$(1DH) + Chr$(6BH) 					                            ! Manejo de impresion
!     BARCODE$  = Chr$(1DH) + Chr$(50H) 					                            ! Manejo de impresion
!     ENDBCODE$ = Chr$(00H) 						                                      ! codigos de barra
!     TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(1)				                        ! Centra la impresion
!     Call U.IMPRIME(TS.TEMP1$,4000H)					                              !        
!     TS.TEMP1$ = Chr$(1DH)+Chr$(77H)+Chr$(01H)+ 		                       \! Horizontal barcode
!                 Chr$(1DH)+Chr$(68H)+Chr$(40H)+                            \! Vertical barcode			
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
     TS.TEMP1$ = (BARCODE$+Chr$(07H)+XNRO$+ENDBCODE$)                       ! Arma String de impresion			
     Write #34 ; TS.TEMP1$          					                              ! Imprime cupon en PDF-417
     TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(0)				                        ! Ajusta impresion a la Izquierda
     Call U.IMPRIME(TS.TEMP1$,4000H)				                                !                    
!-- Fin EAN 128
     Asic.Detalle% = 1																											! Dato de la trx actual
     TS.TEMP1$ = Linea.Detalle(38)  																				! Trailer del movimiento
     Call U.IMPRIME(TS.TEMP1$,4200H)                                        !
     TS.TEMP1$ = String$(37,"*")																						! Linea cierre voucher
     Call U.IMPRIME(TS.TEMP1$,4900H)																			  ! avance de papel
     Call U.CORTACR																												  ! Corta Papel
     Call visor.and.borrar("BONO A ENTREGAR POR VALOR "+str$(TS.TEMP5I4))
     Call U.Imprime("*----  B O N O  R E C O M P R A  ---*",2100H)		      ! Impresion Bono recompra
     CALL FORMAT.AMOUNT(TS.TEMP5I4)																					!
     Call U.IMPRIME("BONO GENERADO :",2100H)
     Call U.IMPRIME(XNRO$,2100H)
     Call U.IMPRIME("VALOR :"+TS.TEMP1$,2100H)
     Call U.IMPRIME("VENCE :"+XDATE1$+"-"+XDATE2$,2100H)
     Call U.Imprime("*-----------------------------------*",2100H)		      ! Impresion Bono recompra
     !To.USEREXIT(20) = -1																									  ! Activa  lineas impresion
     !To.USEREXIT(60) = -1																									  !
     TS.LINETYPE = 18																											  ! Genera cabecera 
     TS.LINEDATA = 1																											  ! en proximo tiquete
     Call TSPREC01																												  !
  EndIf																																			! Fin impresion del bono

End Sub 																																		! Fin Impresion Bono Recompra

Function Bono.Tarifa.Iva(UE.INDI1%) Public
String    UE.BIN$
Integer*2 UE.INDI1%, Bono.Tarifa.Iva, UE.TARIFA%, XI%
  UE.TARIFA% = 8																														! Tarifa defualt
  IF (UE.INDI1% AND 01H) THEN                                              \!
      UE.BIN$ = "1" ELSE UE.BIN$ = "0"                 											!
  FOR XI% = 1 TO 7                                      										!
      UE.INDI1% = SHIFT(UE.INDI1%,1)                  											! Inicializa sig. BIT
      IF (UE.INDI1% AND 01H) THEN UE.BIN$="1"+UE.BIN$                      \!
          ELSE UE.BIN$ ="0"+UE.BIN$                   											!
  NEXT XI%                                              										!
  
  IF LEFT$(UE.BIN$,4) = "1000" THEN UE.TARIFA% = 1                          ! Si hay Indic Plan A
  IF LEFT$(UE.BIN$,4) = "0100" THEN UE.TARIFA% = 2                          ! Si hay Indic Plan B
  IF LEFT$(UE.BIN$,4) = "0010" THEN UE.TARIFA% = 3                          ! Si hay Indic Plan C
  IF LEFT$(UE.BIN$,4) = "0001" THEN UE.TARIFA% = 4                          ! Si hay Indic Plan D
  IF LEFT$(UE.BIN$,4) = "1100" THEN UE.TARIFA% = 5                          ! Si hay Indic Plan E
  IF LEFT$(UE.BIN$,4) = "0110" THEN UE.TARIFA% = 6                          ! Si hay Indic Plan F
  IF LEFT$(UE.BIN$,4) = "0011" THEN UE.TARIFA% = 7                          ! Si hay Indic Plan G
  IF LEFT$(UE.BIN$,4) = "0000" THEN UE.TARIFA% = 8                          ! Si hay Indic Plan H
  Bono.Tarifa.Iva = UE.TARIFA%																							! Retorna tarifa iva
End Function 


Sub Local.Bono.Compra.Pago
String Xkey$, X.Tmp$
Integer*4 Xctl%, Xcont%
  TS.TS11WERR$ = ""																													! Manejo Error appl
  TS.ER.RETURN = -1
  Open "R::$F:BPGLOC" KEYED RECL 17 AS 94              											! Apertura control local bonos
  If TS.ER.RETURN <> -1 Then Begin                     											! Error apertura archivo
  	Call VISOR.AND.BORRAR("ERROR TF:BPGLOC "+TS.TS11WERR$)                  ! Msg de alerta
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
     Xctl% = Val(Gr.BonPag.Ptg$(7))																					! Total de Bonos
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

Function VALIDA.BONO.COMPRA.PAGO
Integer*4 VALIDA.BONO.COMPRA.PAGO
String    Xbuffer$, Xtemp4$, Xtrama$, XENVIA$,XLLEGA$, XSALE$, XRTA$, Xbono$
   VALIDA.BONO.COMPRA.PAGO = 0																										! Init del proceso
   Call VISORES4690(1,"VALIDANDO CLTE BONO ","COMPRA PAGO, ESPERE",0,"L")
   TS.TEMP5I4 = ROUND(FLOAT(Gr.BonPag.Vlr%) * (0.+FLOAT(Gr.BonPag.PtgT%)/100.),0,0) ! Calcula dscto a otorgar
   If TS.TEMP5I4 > Gr.BonPag.Monto% Then TS.TEMP5I4 = Gr.BonPag.Monto%    ! Maximo bono parametro 4 mayo 2017
   Xbono$ = Left$(Gr.BonPag.Nbon$,29)
   Xbuffer$ = Right$("                 "+Gr.Lcl.Clte$,15)    +             \! ID del cliente             Asc 15
              Xbono$                                         +             \! Numero del bono            Asc 29
              Right$("0000000000"+Str$(TS.TEMP5I4),10)       +             \! Valor bono calculado       Asc 10
              Right$("000"+Gr.BonPag.Ptg$(7),3)              +             \! Numero de bonos permitidos Asc 3
              Gr.BonPag.Fini$                                +             \! Fecha inicial proyecto     Asc 6
              Gr.BonPag.Ffin$                                +             \! Fecha final del proyecto   Asc 6
              Right$("0000000"+Gr.BonPag.Ptg$(9),7)          +             \! Numero Id Bono             Asc 7
              Right$("00"+Gr.BonPag.Ptg$(8),2)               +             \! Tipo de Bono               Asc 2
              Right$("00000000"+("20"+Gr.BonPag.Ptg$(12)),8) +             \! Fecha inicial vigencia     Asc 8 
              Right$("00000000"+("20"+Gr.BonPag.Ptg$(13)),8)                ! Fecha final   vigencia     Asc 8 
   Asc.Pay.Impr% = 1
   XTemp4$ = Armar.Trama.Msg("16","61",Xbuffer$,"00","0001","123456")       ! Armar trama MSG
   Xenvia$ = XTemp4$																											  !
   Xsale$  = DATE$ + ":" + Time$																						! Hora salida requerimiento
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)         ! Ejecuta Requerimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   Xrta$     = DATE$ + ":" + Time$																					! Hora llegada requerimiento
   Xllega$   = Xtrama$																											!
   Call  BCOMP.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                    ! Rastro auditoria
   TS.ER.RETURN = -1																												! Control del proceso
   If Xtemp4$ <> "00" Then Begin 																						! Proceso No satisfactorio de java
   	  Gr.BonPag.Oflne% = 0
   	  Call Local.Bono.Compra.Pago																					  ! Generar bono y encolar proceso
   	  If TS.ER.RETURN = -1 Then Gr.BonPag.Oflne% = -1
   EndIf Else Begin																													! Comunicacion java exitosa 
    If Mid$(XTrama$,12,2) <> "00" Then Begin 																! Con Error de ejecucion 
    	Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))														! Presenta mensaje de Error 
    	 VALIDA.BONO.COMPRA.PAGO = 0																								! Reporta falla en proceso
    	 Exit Function 																												! Sale del proceso
    EndIf	  																																!
   EndIf 																																		!

   If TS.ER.RETURN = -1 Then Begin 																					! Si proceso OK
   	If Gr.BonPag.Oflne% = 0 Then Begin																			! Bono en linea
      Xbono$ = Left$(Gr.BonPag.Nbon$,29)
      Xbuffer$ = Xbono$                                      +             \! Numero del bono            Asc 29
                 Gr.BonPag.Fini$                             +             \! Fecha inicial proyecto     Asc 6
                 Gr.BonPag.Ffin$                             +             \! Fecha final del proyecto   Asc 6
                 Right$("000000"+Gr.BonPag.Ptg$(9),6)                       ! Numero Id Bono             Asc 6
      Asc.Pay.Impr% = 1
      XTemp4$ = Armar.Trama.Msg("16","62",Xbuffer$,"00","0001","123456")    ! Armar trama MSG
      XENVIA$ = XTemp4$																											  !
      Xsale$  = DATE$ + ":" + Time$																						! Hora salida requerimiento
      XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)      ! Ejecuta Requerimiento
      Xrta$     = DATE$ + ":" + Time$																					! Hora llegada requerimiento
      Xllega$   = Xtrama$																											!
      Call  BCOMP.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                    ! Rastro auditoria
      VALIDA.BONO.COMPRA.PAGO = -1 
    EndIf Else Begin																												! Bono de contingencia
      TS.TEMP5I4 = ROUND(FLOAT(Gr.BonPag.Vlr%) * (0.+FLOAT(Gr.BonPag.PtgT%)/100.),0,0) ! Calcula dscto a otorgar
      If TS.TEMP5I4 > Gr.BonPag.Monto% Then TS.TEMP5I4 = Gr.BonPag.Monto%    ! Maximo bono parametro 4 mayo 2017
      Xbono$ = Left$(Gr.BonPag.Nbon$,29)
   Xbuffer$ = Right$("                 "+Gr.Lcl.Clte$,15)    +             \! ID del cliente             Asc 15
              Xbono$                                         +             \! Numero del bono            Asc 29
              Right$("0000000000"+Str$(TS.TEMP5I4),10)       +             \! Valor bono calculado       Asc 10
              Right$("000"+Gr.BonPag.Ptg$(7),3)              +             \! Numero de bonos permitidos Asc 3
              Gr.BonPag.Fini$                                +             \! Fecha inicial proyecto     Asc 6
              Gr.BonPag.Ffin$                                +             \! Fecha final del proyecto   Asc 6
              Right$("0000000"+Gr.BonPag.Ptg$(9),7)          +             \! Numero Id Bono             Asc 7
              Right$("00"+Gr.BonPag.Ptg$(8),2)               +             \! Tipo de Bono               Asc 2
              Right$("00000000"+("20"+Gr.BonPag.Ptg$(12)),8) +             \! Fecha inicial vigencia     Asc 8 
              Right$("00000000"+("20"+Gr.BonPag.Ptg$(13)),8)                ! Fecha final   vigencia     Asc 8 
      Asc.Pay.Impr% = 1
      XTemp4$ = Armar.Trama.Msg("16","64",Xbuffer$,"00","0001","123456")    ! Armar trama MSG
      XENVIA$ = XTemp4$																											  !
      Xsale$  = DATE$ + ":" + Time$																						! Hora salida requerimiento
      XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)      ! Ejecuta Requerimiento
      Xrta$     = DATE$ + ":" + Time$																					! Hora llegada requerimiento
      Xllega$   = Xtrama$																											!
      Call  BCOMP.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                    ! Rastro auditoria
      VALIDA.BONO.COMPRA.PAGO = -1     																			! Proceso OK
    EndIf																																		! Fin encolamiento bono contingencia
   EndIf Else  VALIDA.BONO.COMPRA.PAGO = 0										    				  ! No tiene derecho a Bono
    	
End Function 

Function Valida.String.Tef Public                                  ! Validacion Bin Trx Tef
Integer*4 XI%
String Trx$, GA$, GB$, Valida.String.Tef
  Asc.Tmp.Apun% = 3																																		! Ajusta apuntador String
  Valida.String.Tef = ""																										! 
  GA$ = TRX$																																! Toma String UD analizar
  
!  GA$ = Asic.Getunpk(GB$,Asc.Tmp.Apun%)																			! Toma proyecto
!  GA$ = Asic.Getunpk4(GB$,Asc.Tmp.Apun%)																		! Toma String de tef

  Write #34; "TR:"+LEFT$(H$,60) + CHR$(10)

  If MATCH("423949",H$,1) > 0 Then Begin																		! Si pago con Visa Comfandi
     Write #34; "BIN EN EL TEF " + CHR$(10)
     For XI% = 1 To Gr.Bonpag.NumBin%																			  ! Analiza si trx con bin excento
      
      If Gr.Bonpag.Bines$(XI%,0) = "423949" Then                           \! Bin Visa Comfanid
       	 Valida.String.Tef = Gr.Bonpag.Bines$(XI%,1)   											! Reporta tipo variedad pago a reportar
     Next XI%																																!
  EndIf Else BEGIN 
  	Write #34; "NO BIN EN EL TEF " + CHR$(10)
  ENDIF
End Function 

Sub CALCULO.BONO.COMPRA.PAGO																								! Calculo del valor del bono recompra
  Integer*4 PRIC%, Xvta%, X.K%, Xmonto%, Xdsctofin%, Pago99%, Xvtasec%
  Integer*2 Xp1%, Xp2%, X.TARIFA%, XL%, Xs%, Xptg%, Xpos%, Xfnd%, Xptg99%
  String    X.TMP$, Xpago$, Xclte$, Xstruc$, Xnada$, Xsgn$, Xtmp2$          !
  String    Xpagos$(1), Xitm$, Xsec$
  Xvta% = 0                                                                 ! Total Venta          
  Xvtasec% = 0
!-- Control para proceso FE 26Ene2023
  If Val(Gr.Lcl.IdClte$) > 3 Then Begin
 	 	Gr.BonPag.Vlr% = 0                                 											! No aplica dscto en la trx
 	  Exit Sub                                           											! Sale del proceso
  EndIf
  Dim Gr.BonPag.Tipo$(7)                                                    ! Tipos de clientes    
  Dim Gr.BonPag.Ptg$(15)                                                    ! Parametros Asignados 
  Dim Gr.BonPag.Msg$(5)                                                     ! Mensajes parametro   
  Dim Xpagos$(100)																													! Formas de pago
  Xtmp2$ = ""
  TS.ER.RETURN = -1                                                         ! Ctrl Errores         
  TS.TS11WERR$ = ""
  Open "R::$F:PBONPG" KEYED RECL 77 AS Gr.BonPag.Ses% UNLOCKED NOWRITE NODEL ! Parametros para bono recompra
  If TS.ER.RETURN <> -1  Then Begin                    											! Error apertura archivo
  	Call VISORES4690(1,"ERROR DE APERTURA","TF:PBONPG",1200,"L")
 	 	Gr.BonPag.Vlr% = 0                                 											! No aplica dscto en la trx
 	  Exit Sub                                           											! Sale del proceso
  EndIf                                                											!
  TS.TS11WERR$ = ""
  TS.ER.RETURN = -1                                                         ! Ctrl Errores         
  Read Form "C4 2C3 5C4 5C2 C1 C3 C10 C2 C6 C4 2C3 C1 C4";#Gr.BonPag.Ses% KEY "7778";   \!
       X.Tmp$,                                                             \! Llave del proyecto                  
       Gr.BonPag.Fini$,                                                    \! Fecha inicial proyecto              
       Gr.BonPag.Ffin$,                                                    \! Fecha final del proyecto            
       Gr.BonPag.Tipo$(1),                                                 \! Tipo y variedad de pago 1           
       Gr.BonPag.Tipo$(2),                                                 \! Tipo y variedad de pago 2           
       Gr.BonPag.Tipo$(3),                                                 \! Tipo y variedad de pago 3           
       Gr.BonPag.Tipo$(4),                                                 \! Tipo y variedad de pago 4           
       Gr.BonPag.Tipo$(5),                                                 \! Tipo y variedad de pago 5           
       Gr.BonPag.Ptg$(1),                                                  \! Porcentaje descuento 1              
       Gr.BonPag.Ptg$(2),                                                  \! Porcentaje descuento 2              
       Gr.BonPag.Ptg$(3),                                                  \! Porcentaje descuento 3              
       Gr.BonPag.Ptg$(4),                                                  \! Porcentaje descuento 4              
       Gr.BonPag.Ptg$(5),                                                  \! Porcentaje descuento 5              
       Gr.BonPag.Ptg$(6),                                                  \! Tipo validacion 1.Itm 2.Dpto 3.Todo 
       Gr.BonPag.Ptg$(7),                                                  \! Numero de bonos a imprimir          
       Gr.BonPag.Ptg$(10),                                                 \! Lista tipificaciones
       Gr.BonPag.Ptg$(8),                                                  \! Tipo de bono                        
       Gr.BonPag.Ptg$(9),                                                  \! Consecutivo bono                    
       Gr.BonPag.Ptg$(11),                                                 \! Valor maximo bono recompra
       Gr.BonPag.Ptg$(12),                                                 \! Fecha inicial vigencia
       Gr.BonPag.Ptg$(13),                                                 \! Fecha final   vigencia
       Gr.BonPag.Ptg$(14),                                                 \! Forma aplicacion bono 0.proxima compra 1.dentro de la compra
       Gr.BonPag.Ptg$(15)                                                   ! Valor minimo generacion bono

  If TS.ER.RETURN <> -1 Then Begin                     											! No existe registro control
 	 	Gr.BonPag.Fini$ = "000000"
 	 	Gr.BonPag.Ffin$ = "000000"
 	 	Gr.BonPag.Monto% = 0 
 	 	Gr.BonPag.VlrMin% = 0
 	 	Call U.Imprime("Falla Cabecera Bonos Pago",2100H)
 	 	GoTo Proc.Bono.Compra.Pago
  EndIf Else Begin                                     											!
 	 	Gr.BonPag.Fini$ = UnPack$(Gr.BonPag.Fini$)
 	 	Gr.BonPag.Ffin$ = UnPack$(Gr.BonPag.Ffin$)
    Gr.BonPag.Monto% = Val(Unpack$(Gr.BonPag.Ptg$(11)))
    Gr.BonPag.Ptg$(12) = Unpack$(Gr.BonPag.Ptg$(12))
    Gr.BonPag.Ptg$(13) = Unpack$(Gr.BonPag.Ptg$(13))
    Gr.BonPag.VlrMin% = Val(Unpack$(Gr.BonPag.Ptg$(15)))
  EndIf
  TS.ER.RETURN = -1
  Read Form "C4 C37 C36";#Gr.BonPag.Ses% KEY "7775";                       \! Primer Mensaje definido   
       X.Tmp$,                                                             \! Llave del proyecto        
       Gr.BonPag.Msg$(1),                                                  \! Fecha inicial proyecto    
       Xnada$                                                                                           
  If TS.ER.RETURN <> -1 Then Begin
 	 	Gr.BonPag.Fini$ = "000000"
 	 	Gr.BonPag.Ffin$ = "000000"
 	 	Gr.BonPag.Monto% = 0 
 	 	Gr.BonPag.VlrMin% = 0
 	 	Call U.Imprime("Falla Cabecera Bonos Pago",2100H)
 	 	GoTo Proc.Bono.Compra.Pago
  EndIf
  TS.ER.RETURN = -1  
  Read Form "C4 C37 C36";#Gr.BonPag.Ses% KEY "7776";                       \! Segundo Mensaje definido  
       X.Tmp$,                                                             \! Llave del proyecto        
       Gr.BonPag.Msg$(2),                                                  \! Fecha inicial proyecto    
       Xnada$
  TS.ER.RETURN = -1       
  If TS.ER.RETURN <> -1 Then Begin
 	 	Gr.BonPag.Fini$ = "000000"
 	 	Gr.BonPag.Ffin$ = "000000"
 	 	Gr.BonPag.Monto% = 0 
 	 	Gr.BonPag.VlrMin% = 0
 	 	Call U.Imprime("Falla Cabecera Bonos Pago",2100H)
 	 	GoTo Proc.Bono.Compra.Pago
  EndIf
  TS.ER.RETURN = -1
  Read Form "C4 C37 C36";#Gr.BonPag.Ses% KEY "7777";                       \! Tercer Mensaje definido   
       X.Tmp$,                                                             \! Llave del proyecto        
       Gr.BonPag.Msg$(3),                                                  \! Fecha inicial proyecto    
       Xnada$
  If TS.ER.RETURN <> -1 Then Begin
 	 	Gr.BonPag.Fini$ = "000000"
 	 	Gr.BonPag.Ffin$ = "000000"
 	 	Gr.BonPag.Monto% = 0 
 	 	Gr.BonPag.VlrMin% = 0
 	 	Call U.Imprime("Falla Cabecera Bonos Pago",2100H)
 	 	GoTo Proc.Bono.Compra.Pago
  EndIf                                                                                                        

  Proc.Bono.Compra.Pago:
  Close Gr.BonPag.Ses%																                      ! cierra sesion temporal                  
  If Gr.BonPag.Monto% = 0 Then BEGIN 
  	 Gr.BonPag.Vlr% = 0
  	 Exit Sub 
  ENDIF 
  If Val(date$) >= Val(Gr.BonPag.Fini$) Then Begin                          ! Es una fecha mayor o igual a la inicial 
   If Val(date$) >  Val(Gr.BonPag.Ffin$) Then Begin                         ! Es una fecha mayor a la fecha final     
 	 	  Gr.BonPag.Vlr% = 0                                                    ! No aplica dscto en la trx               
 	    Exit Sub                                                              ! Sale del proceso                        
  EndIf                                                                                                               
  EndIf Else Begin																		                      ! Es fecha menor a la inicial             
 	 	Gr.BonPag.Vlr% = 0                                                      ! No aplica dscto en la trx               
 	  Exit Sub                                                                ! Sale del proceso                        
  EndIf                                                                                                               
                                                                                                                      
  TS.ER.RETURN = -1                                                         ! Ctrl Errores                            
  TS.TS11WERR$ = ""
  Open "R::$F:COMPAG" KEYED RECL 7 AS Gr.BonPag.Ses% UNLOCKED NOWRITE NODEL ! Articulos para bono recompra
  If TS.ER.RETURN <> -1 Then Begin                                          ! Error apertura archivo
  	Call VISORES4690(1,"ERROR DE APERTURA","TF:COMPAG",1200,"L")
 	 	Gr.BonPag.Vlr% = 0                                                      ! No aplica dscto en la trx         
 	  Exit Sub                                                                ! Sale del proceso                  
  EndIf                                                                     !                                   
  Gr.BonPag.Vlr% = 0 : Pago99% = 0                                          ! Init variable                     
  For I% = 1 TO SL.END                                                      ! FOR ALL StringS                   
    H$ = READ.SL.STR$(I%)                                                   ! GET String                        
    If LEN(H$) > 5 Then                                                     \ ASSURE GOOD String                
    If ASC(H$) = 1 Then Begin                                               ! ITEM ENTRY String                 
     X.TMP$ = ASIC.GETUNPK(H$,3)	   				                                ! Tomo informacion del item vendido 
     X.Tmp$ = Right$("000000000000"+X.Tmp$,12)                              !   
     Xitm$ = ";" + Str$(Val(X.Tmp$)) + ";"
     If Match(Xitm$,Gr.BonPag.Pludomi$,1) > 0 Then                         \! Si vta domic en trx
     	  Gr.BonPag.VtaDomic% = -1																						! reporta venta domicilio
     X.Tmp$ = Pack$( X.tmp$ )                                               ! Arma llave de busqueda            
     TS.ER.RETURN = -1																                      ! Ctrl errores                      
     Call IRRFEC.READ01 (X.tmp$, 4, TS.TEMP1$, 0) 		                      ! Lectura Datos Itemr               
     Call IRRFEC.SPLIT1 ( TS.TEMP1$ ) 			                                ! Entrega datos al eamitemr.j86     
     Xstruc$ = Left$(IR.USERDATA$,16)		  					                        ! Estructura comercial              
     Xstruc$ = Mid$(Xstruc$,2,9)                                            ! Toma los primeros 9 digitos
     Xsec$ = Left$(Xstruc$,3)																								! toma seccion de venta
     If Xstruc$ = "         " Then GoTo Next.Itm.Compra                     ! Error en estructura comercial no procesa

!--- Busqueda excluido de bonos recompra
     TS.ER.RETURN = -1                                 											! Ctrl de Error
     X.tmp$ = Pack$("02") + IR.ITEMCODE$         											      ! Consulta si Item excluido
     Read Form "C7";#Gr.BonPag.Ses% KEY X.Tmp$; X.Tmp$ 									    ! Lee archivo      
     If TS.ER.RETURN = -1 Then GoTo Next.Itm.Compra 										    ! Item excluido para calculo bono recompra
!--- retorna en TS.ER.RETURN
     TS.ER.RETURN = -1                                                      ! Ctrl de Error                     
     If Gr.BonPag.Ptg$(6) <> "3" Then Begin                                 ! Validacion por Item o estrucura comercial
     	  If Gr.BonPag.Ptg$(6) = "1" Then                                    \! Validacion por Item                      
     	  	 X.tmp$ = Pack$("01") + IR.ITEMCODE$                                            !                                          
     	  If Gr.BonPag.Ptg$(6) = "2" Then                                    \! Validacion por Estructura comercial      
     	  	 X.tmp$ = Pack$("01") + Pack$(Right$("000000000000"+Xstruc$,12))  ! 

        Read Form "C7";#Gr.BonPag.Ses% KEY X.Tmp$; X.Tmp$                   ! Lee archivo           
        If TS.ER.RETURN <> -1 Then BEGIN                                    ! Not found             
     	     GoTo Next.Itm.Compra                                             ! Siguiente Item vendido
        ENDIF 
     EndIf
     X.TARIFA% = Bono.Tarifa.Iva(IR.INDICAT1)                               ! Tarifa Default     
                                                                                                 
     L% = ASC(RIGHT$(H$,1)) / 10H                                           ! GET ITEM TYPE      
     If L% = 0 OR L% = 8 Then Begin                                         ! Item SALE OR CANCEL
      PRIC% = 0                                                             ! ASSUME NO PRICE    
      K% = MATCHB(":",H$,3) + 1                                             ! START OF PRICE     
      J% = MATCHB(":",H$,K%)                                                ! END OF PRICE       
      If J% > K% Then Begin                                                 ! PRICE FOUND        
       PRIC% = PACKBIN4(H$,K%-1,J%-K%)                                      ! GET PRICE          
       PRIC% = ROUND(FLOAT(PRIC%)/(1.+FLOAT(UE.TABLAIVA(X.TARIFA%))/100.),0,0) ! Calcula valor Base
       If L% = 8 Then PRIC% = 0 - PRIC%                                     ! INVERT PRICE FOR VOID             
       Xvta% = Xvta% + PRIC%                                                ! Acumula venta                     
       If Match(Xsec$,Gr.BonPag.Secc$,1) > 0 Then                          \! Si vta secciones
     	    Xvtasec% = Xvtasec% + Pric%
      EndIf                                                                 ! PRICE FOUND                       
     EndIf                                                                  ! ITEM SALE OR CANCEL               
    EndIf                                                                   ! Item ENTRY String                 
    Asc.Tmp.Apun% = 3																	                      ! Apuntador strings                 
    If ASC(H$) = 5 Then Begin                                               ! FORMA PAGO String                 
    	 X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                            ! Tipo y variedad de pago registrado
  	 	 Xpago$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                          	! Valor forma de pago               
  	 	 If Xtmp2$ <> "" Then Begin																		        ! con pago visa comfandi
  	 	 	  X.TMP$ = Xtmp2$																										! Reporta variedad parametro
  	 	 	  Xtmp2$ = ""
  	 	 EndIf
       Gr.BonPag.Ptv% = Val(Xpago$)                        									!
       Xpagos$(Val(X.TMP$)) = Str$( Val(Xpagos$(Val(X.TMP$))) + Gr.BonPag.Ptv% )
       Pago99% = Pago99% + Gr.BonPag.Ptv%                                   ! Monto otros pagos
    EndIf																							                      ! FIN  FORMA PAGO String            
    If ASC(H$) = 6 Then Begin                                               ! ANUL FORMA PAGO String            
    	 X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                            ! Tipo y variedad de pago registrado
  	 	 Xpago$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                          	! Valor forma de pago               
  	 	 If Xtmp2$ <> "" Then Begin																		        ! con pago visa comfandi
  	 	 	  X.TMP$ = Xtmp2$																										! Reporta variedad parametro
  	 	 	  Xtmp2$ = ""
  	 	 EndIf
       Gr.BonPag.Ptv% = Val(Xpago$)                        									!
       Xpagos$(Val(X.TMP$)) = Str$( Val(Xpagos$(Val(X.TMP$))) - Gr.BonPag.Ptv% )
       Pago99% = Pago99% - Gr.BonPag.Ptv%                                   ! Monto otros pagos
    EndIf																							                      ! FIN ANUL FORMA PAGO String        
    If ASC(H$) = 9 Then Begin                                               ! Vueltas String 
    	 X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                            ! Tipo y variedad de vueltas
  	 	 Xpago$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                          	! Valor vueltas               
       Gr.BonPag.Ptv% = Val(Xpago$)                        									!
       Xpagos$(Val(X.TMP$)) = Str$( Val(Xpagos$(Val(X.TMP$))) - Gr.BonPag.Ptv% )
       Pago99% = Pago99% - Gr.BonPag.Ptv%                                   ! Monto otros pagos
    EndIf																							                      ! FIN ANUL FORMA PAGO String      

    If ASC(H$) = 153 Then Begin                                             ! Lectura User data
    	 X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                            ! Codigo del proyecto
    	 If X.tmp$ = "51" Then Begin																					! String de TEF
          If MATCH("423949",H$,1) > 0 Then Begin														! Si pago con Visa Comfandi
           Xtmp2$ = "52"  
          EndIf Else BEGIN 
  	        Xtmp2$ = ""
          ENDIF
    	 EndIf																																! Fin TEf
    	 If X.tmp$  = "97" Then Begin																					! Descuento motor de promociones
    	 	  IR.ITEMCODE$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                  ! codigo del producto
    	 	  PRIC%   = Val(ASIC.GETUNPK(H$,Asc.Tmp.Apun%))                     ! Valor dscto Promocion
    	 	  X.TMP$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                        ! codigo de la promocion
    	 	  X.TARIFA% = Val(ASIC.GETUNPK(H$,Asc.Tmp.Apun%))                   ! flag del iva 
    	 	  X.TMP$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                        ! codigo del departamento SA
    	 	  Xstruc$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                        ! estructura comercial 
    	 	  Xstruc$ = Left$(Xstruc$,9)                                        ! Toma 9 posiciones iniciales
    	 	  Xsgn$   = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                        ! signo de la operacion
          IR.ITEMCODE$ = Pack$(Right$("000000000000"+IR.ITEMCODE$,12))      ! Ajsuta Item para consulta
          
!--- Busqueda excluido de bonos recompra
          TS.ER.RETURN = -1                            											! Ctrl de Error
          X.tmp$ = Pack$("02") + IR.ITEMCODE$         								      ! Consulta si Item excluido
          Read Form "C7";#Gr.BonPag.Ses% KEY X.Tmp$; X.Tmp$ 					      ! Lee archivo      
          If TS.ER.RETURN = -1 Then GoTo Next.Itm.Compra 										! Item excluido para calculo bono recompra
!--- retorna en TS.ER.RETURN
          
          If Gr.BonPag.Ptg$(6) <> "3" Then Begin                            ! Validacion por Item o estrucura comercial
     	     If Gr.BonPag.Ptg$(6) = "1" Then                                 \! Validacion por Item                      
     	  	    X.tmp$ = Pack$("01") + IR.ITEMCODE$                           !                                          
     	     If Gr.BonPag.Ptg$(6) = "2" Then                                 \! Validacion por Estructura comercial      
     	  	    X.tmp$ = Pack$("01") + Pack$(Right$("000000000000"+Xstruc$,12)) ! 
           TS.ER.RETURN = -1
           Read Form "C7";#Gr.BonPag.Ses% KEY X.Tmp$; X.Tmp$                ! Lee archivo           
           If TS.ER.RETURN = -1 Then BEGIN                                  ! Si found
              PRIC% = ROUND(FLOAT(PRIC%)/(1.+FLOAT(UE.TABLAIVA(X.TARIFA%))/100.),0,0) ! Calcula valor Base
              If Xsgn$ = "01" Then PRIC% = 0 - PRIC%                        ! INVERT PRICE FOR VOID             
              Xvta% = Xvta% - PRIC%                                         ! Resta valor dscto a la Acumulacion venta    
              If Match(Xsec$,Gr.BonPag.Secc$,1) > 0 Then                   \! Si vta secciones
     	           Xvtasec% = Xvtasec% - Pric%
           EndIf																														! 
          EndIf Else Begin  																								!
              PRIC% = ROUND(FLOAT(PRIC%)/(1.+FLOAT(UE.TABLAIVA(X.TARIFA%))/100.),0,0) ! Calcula valor Base
              If Xsgn$ = "01" Then PRIC% = 0 - PRIC%                        ! INVERT PRICE FOR VOID             
              Xvta% = Xvta% - PRIC%                                         ! Resta valor dscto a la Acumulacion venta                     
              If Match(Xsec$,Gr.BonPag.Secc$,1) > 0 Then                   \! Si vta secciones
     	           Xvtasec% = Xvtasec% - Pric%
          EndIf																															!
    	 EndIf																																! Fin dscto motor de promociones
    EndIf																																		! 
    Next.Itm.Compra:                                                        ! Siguiente Item vendido            
  Next I%                                                                   ! NEXT String                       
  Close Gr.BonPag.Ses%                                                      ! Close File                        
  Gr.BonPag.Vlr% = Xvta%                                                    ! Retorna total de venta en la trx  
  
! Mod 29Ago2022
  If Gr.BonPag.Vlr% < Gr.BonPag.VlrMin% Then Begin													! Si valor calculado menor a monto minimo
  	 Gr.BonPag.Vlr% = 0																											! reporta no generacion de bono
  EndIf																																			! fin control monto minimo generacion
  
  Gr.BonPag.PtgT% = 0                                                       ! Init variable                     
  Xdsctofin% = 0  : Xptg99% = 0 
  If Gr.BonPag.Vlr% > 0 Then Begin										 											! Hay Ventas de productos seleccionados
   If Val(Xpagos$(34)) > 0 Then Begin																				! Si pago con Bono Electrofiesta  Add 10Sep2021
      Gr.BonPag.Vlr% = 0                                                 		! No aplica generación de bono
      Exit Sub                                                           		! Sale del proceso
   EndIf																																		! Fin control electrofiesta
   For XL% = 1 To 5 																											  ! Recorre lista de formas de pago
    Xs% = Val( Right$(Gr.BonPag.Tipo$(Xl%),2) )                          		! Tipo y variedad a validar
    If Xs% = 99 Then Xptg99% = Val(Gr.BonPag.Ptg$(XL%))											! Ptg dscto otros pagos
    If Val(Xpagos$(Xs%)) > 0 Then Begin																			! Si reporta pago con ese tipo y variedad
       Xmonto% = Val(Xpagos$(Xs%))																					! Valor pagado 
       Pago99% = Pago99% - Xmonto%                                          ! Monto otros pagos
    	 If Xmonto% > Gr.BonPag.Vlr% Then Begin																! Si monto pago mayor que venta
    	 	  Xmonto% = Gr.BonPag.Vlr%																					! Reporta solo lo comprado
    	 EndIf																																!
    	 Xptg% = Val(Gr.BonPag.Ptg$(XL%))                           					! Porcentaje definido
    	 TS.TEMP5I4 = ROUND(FLOAT(Xmonto%) * (0.+FLOAT(Xptg%)/100.),0,0) 			! Calcula dscto a otorgar
    	 If Xdsctofin% < TS.TEMP5I4 Then Begin																! Si dscto calculado superior al anterior
    	 	  Xdsctofin% = TS.TEMP5I4  																					! Asigna valor del descuento
    	 	  Gr.BonPag.PtgT% = Xptg% 																					! Reporta dscto aplicado
    	 EndIf 
    EndIf	
   Next Xl%																																	! Fin formas de pago 
  EndIf	Else Begin																													! Si no hay compras
    Gr.BonPag.Vlr% = 0                                                  		! No aplica dscto en la trx
    Exit Sub                                                            		! Sale del proceso
  EndIf																																			! Fin si no aplica
!-- Calculo otros pagos
If Pago99% <> 0 Then Begin
  If Pago99% > Gr.BonPag.Vlr% Then Begin																		! Si pago superior al valor de venta productos
     Pago99% = Gr.BonPag.Vlr%																								! Reporta el valor venta items
  EndIf	
  TS.TEMP5I4 = ROUND(FLOAT(Pago99%) * (0.+FLOAT(Xptg99%)/100.),0,0) 				! Calcula dscto a otorgar
  If Xdsctofin% < TS.TEMP5I4 Then Begin																			! Si dcto otros pagos menor 
   	 Gr.BonPag.Vlr% = Pago99%																								! Reporta valor compra
   	 Gr.BonPag.PtgT% = Xptg99%																							! Reporta dcto aplicar
   EndIf Else Begin
   	 Gr.BonPag.Vlr% =  Xmonto%
   EndIf
EndIf 
!-- Fin calculo otros pagos  	
	Xclte$ = Gr.Lcl.TipClte$ 																									! Tipificaciones Validas
  XL% = 0 : Xpos% = 1 : Xfnd% = 0																						! Init variables
  For XL% = 1 To 5 																											    ! Recorre lista de formas de pago
    Xclte$ =  "TC" + Mid$(Gr.BonPag.Ptg$(10),XPOS%,2)                       ! Toma tipificacion para el evento
    Xpos% = Xpos% + 2																												! Siguiente tipificacion
    If Match(Xclte$,Gr.Lcl.TipClte$,1) > 0 Then                            \! Si tipificacion definida
       Xfnd% = -1																														! Lo reporta como encontrada
  Next XL%																																	!
  If (Gr.BonPag.PtgT% = 0 Or Xfnd% = 0) Then Begin                       		! No hay ningun descuento
    Gr.BonPag.Vlr% = 0                                                  		! No aplica dscto en la trx
    Exit Sub                                                            		! Sale del proceso
  EndIf

!--- Add 7Jul2022
!--- Add 21Mzo2025 Se reporta valor en bono cero(0) con vta domicilio
     If Gr.BonPag.VtaDomic% = -1 Then Begin                                 ! Si venta plu domicilios
!     	Gr.BonPag.Vlr% = Gr.BonPag.Vlr% - Xvtasec%													  ! Recalcula valor bono
!     	If (Gr.BonPag.Vlr% <= 0 ) Then Begin                                  ! Si no hay venta

 	 	     Gr.BonPag.Vlr% = 0                                                 ! No aplica dscto en la trx
 	       Exit Sub                                                           ! Sale del proceso

!     	EndIf																																	! fin vta secciones controladas

     EndIf																																	! fin venta domicilios

  Xclte$ = Gr.Lcl.TipClte$																									!
!  Gr.BonPag.Nbon$ = Right$("000000000000000"+Gr.Lcl.Clte$,15) +            \! Id del cliente
!                    "20"+ Date$ + TIME$                                     ! Fecha y hora de la trx

!-- Add 27Nov2019 - Nueva estructura bono
!    Gr.BonPag.Nbon$ = Gr.BonPag.Prefijo$ +                                 \! Prefijo del bono       Asc  2
!                      Right$("0000000"+Gr.BonPag.Ptg$(9),7)  +             \! Nro del evento         Asc  7
!                      Right$("00"+Gr.BonPag.Ptg$(8),2)       +             \! Tipo de bono           Asc  2
!                      (Date$ + TIME$)                        +             \! aammddhhmmss           Asc 12
!                      Gr.BonPag.Ptg$(12)                     +             \! Fecha inicio vigencia  Asc  6
!                      Gr.BonPag.Ptg$(13)                                    ! Fecha final  vigencia  Asc  6

!--- Add 10Ene20 - Ajuste estructura por duplicado
     Gr.BonPag.Nbon$ = Gr.BonPag.Prefijo$ +                                \! Prefijo del bono       Asc  2
                      Right$("000"+TS.STORE$,3)              +             \! Numero del almacen     Asc  3
                      Right$("000"+Str$(SL.HD.TERMINAL),3)   +             \! Numero de terminal     Asc  3
                      Right$("000"+Str$(SL.HD.TRANSNUM+1),3) +             \! Numero de transaccion  Asc  3
                      (Date$ + TIME$)                        +             \! aammddhhmmss           Asc 12
                      Gr.BonPag.Ptg$(12)                     +             \! Fecha inicio vigencia  Asc  6
                      Gr.BonPag.Ptg$(13)                                    ! Fecha final  vigencia  Asc  6

    TS.TEMP4I4 = VALIDA.BONO.COMPRA.PAGO
    If TS.TEMP4I4 <> -1 Then Begin																					! Si no genera mensaje
 	 	    Gr.BonPag.Vlr% = 0                                                  ! No aplica dscto en la trx
 	      Exit Sub                                                            ! Sale del proceso
    EndIf

     TS.TEMP1$ = PACK$(Gr.Lcl.Clte$)           +                   		     \! Id del cliente
		    ":"+PACK$(STR$(Gr.BonPag.Vlr%))  			 +	               				   \! Valor de Compra
		    ":"+PACK$(STR$(Gr.BonPag.PtgT%)) 			 +	                         \! Dscto Aplicado
        ":"+Xclte$                             +                           \! Tipificacion de cliente 
        ":"+Pack$(Gr.BonPag.Nbon$)             +                           \! Numero del Bono
        ":"+Pack$(Gr.BonPag.Ptg$(6))           +                           \! Tipo Validacion
        ":"+Pack$(Gr.BonPag.Ptg$(9))           +                           \! Consecutivo serial bono
		    ":"+"00"			                             													! Filler 

      Call Grabacion.String.Usuario2("20160205",TS.TEMP1$)                  ! Graba String de Usuario

End Sub 																																		! Fin bono recompra

Sub BONOCOMPRASPAGO(XOPT%) Public			  																		! Bonos Recompra forma de pago
Integer*4 Xopt%                                                             !
String    Xtmp$

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.BonPag.Ok%  = 0                                                        ! Proyecto Apagado
  Gr.BonPag.Stat% = 0																												! Init variable
  Gr.BonPag.Vlr%  = 0																											  ! Valor bono recompra
  Gr.Bonpag.NumBin% = 0
  Dim Gr.Bonpag.Bines$(20,1)
  TS.ER.RETURN = -1																													! Ctrl Errores                     
  TS.TS11WERR$ = ""
  OPEN "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL 													  ! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then BEGIN                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA BONO RECOMPRA")									! MSg alerta
  	 Exit Sub 																															! Sale del proceso
  ENDIF 																																		!
  IF END #94 THEN UE.FIN.BONOPAG        																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        IF TS.TEMP1$ = "[BONOS RECOMPRA]" Then Begin		  	         				! Bonos Recompra
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.BonPag.Ok%   = Val(Mid$(TS.TEMP1$,30,2))   				    					! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.BonPag.Ses%  = Val(Mid$(TS.TEMP1$,30,02))      				          ! Numero Sesion Temporal
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.BonPag.Prefijo$ = Mid$(TS.TEMP1$,30,02)    				              ! prefijo bonos recompra
         Read #94; TS.TEMP1$     																						! Lectura registro
         Gr.BonPag.Secc$ = Mid$(TS.TEMP1$,30,30)     				                ! Secciones controladas
         Read #94; TS.TEMP1$     																						! Lectura registro
         Gr.BonPag.Pludomi$ = Mid$(TS.TEMP1$,30,50)     				            ! PLU Domicilios
!         GoTo UE.FIN.BONOPAG 																								! Sale del ciclo de carga          

       Endif                                                                !
       IF TS.TEMP1$ = "[INTERFACE TEF]" Then Begin		  	         				  ! Parametros bines tef
         Read #94; TS.TEMP1$																								! Lectura registro                 
         TS.TEMP1$   = Mid$(TS.TEMP1$,30,80)           				    					! Lista formas de pago validas interface
         While (1)																													! Toma lista bines excentos 
           Read #94; TS.TEMP1$     																					! Lectura registro                 
           Xtmp$ = Mid$(TS.TEMP1$,30,06)                  				          ! Bin excento interface
           If Xtmp$ = "999999" Then GoTo UE.FIN.BONOPAG  										! Sale del ciclo de carga          
           Gr.Bonpag.NumBin% = Gr.Bonpag.NumBin% + 1 												! Aumenta apuntador
           Gr.Bonpag.Bines$(Gr.Bonpag.NumBin%,0) = Xtmp$										! Asigna bin excento
           Gr.Bonpag.Bines$(Gr.Bonpag.NumBin%,1) = Mid$(TS.TEMP1$,36,02) 		! Tipo Variedad de pago a reportar en plantilla
         Wend 
       EndIf																																! Fin carga bines TEF
   Wend                                                                     !
   UE.FIN.BONOPAG:                                                          !
     Close 94																																! Cierra archivo

   If Gr.BonPag.Ok% = -1 Then                                              \! Proyecto Activo
      Call U.IMPRIME("MODULO BONO RECOM FP ON  21-Mzo-2025",2100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO BONO RECOM FP OFF 21-Mzo-2025",2100H)          ! Msg Proyecto Cargado
EndIf 																																			! Fin carga opciones

If Gr.BonPag.Ok% <> -1 Then Exit Sub                                        ! Si proyecto apagado

!--- EAMTSU02.J86
If Xopt% = 02 Then Begin                                                    ! Inicio de transaccion
	 Call IMPRESION.BONO.COMPRA.PAGO																					! Generación Bono Recompra
	 Gr.BonPag.Stat% = 0																										  ! Init variable
	 Gr.BonPag.Vlr%  = 0																											! Valor bono recompra
	 Gr.BonPag.VlrMin% = 0																										! Valor minimo compra para expedir bono
	 Gr.BonPag.Ptv%  = 0																											! Valor compra con tipo y variedad
   Gr.BonPag.VtaDomic% = 0                                                  ! Control venta domicilios
   Gr.BonPag.VtaSecc%  = 0                                                  ! Control venta secciones
   Gr.BonPag.VtaOtrSecc% = 0

EndIf																																				! Fin Inicio de transaccion

!--- EAMTSU20.J86
If Xopt% = 20 Then Begin                                                    ! Impresion en CR

  If MATCH(TS.SDESC$(59),TS.PRTBUF$,1) > 0 THEN BEGIN												! Si anulacion total de la trx
  	 Gr.BonPag.Stat% = 1 																										! Para no ejecutar proceso calculo
  EndIf																																			!

 If Not TS.TRAINING Then Begin                       												! Si no en entrenamiento
  If TS.INTRX Then                                  											 \! Si esta en una compra
    If TS.LINETYPE = 6 And                                                 \! Store Data y Transacc
       TS.LINEDATA = 1 Then Begin                    											  ! Fecha, Hora, etc
        If TS.TENDERED (0) <> 0 Or                                         \! Si hay pagos
           TS.TRX.STATUS <> 100 Then Begin                                  ! y NO hay VOID en curso
           If Gr.BonPag.Stat% = 0	Then Begin																! Init variable
            If USER.FBACT.READ And Gr.Lcl.Ctrl% <> 0 Then Begin							! Si cliente y no anul trx
           	   Call CALCULO.BONO.COMPRA.PAGO                                ! Calcula el valor del bono
           	EndIf
           	Gr.BonPag.Stat% = -1	  																			  ! Proceso Ejecutado
           EndIf																														!
        EndIf
     EndIf
  EndIf
  
EndIf																																				! Fin impresion CR


End Sub 
