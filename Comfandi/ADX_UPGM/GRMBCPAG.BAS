!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMBCPAG.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Generación Bonos Recompra        *
!                Validacion Forma de pago         *
!**************************************************
!Observaciones:
!     -- Version 1.0 7-Ene-2016 --
!--------------------------------------------------

%ENVIRON T		                          																		! Ambiente de terminal

Integer*1 Global Gr.BonPag.Ok%,                                            \! Control del proyecto
                 Gr.BonPag.Stat%,                                          \! Control de proceso de calculo compra
                 Gr.BonPag.Oflne%                                           ! Bono recompra Offline
                 
String    Global Gr.BonPag.Tipo$(1),                                       \! Tipificaciones
                 Gr.BonPag.Ptg$(1),                                        \! Porcentaje Dscto 
                 Gr.BonPag.Msg$(1),                                        \! Mensaje a imprimir
                 Gr.BonPag.Fini$,   																			 \! Fecha inicial 
                 Gr.BonPag.Ffin$,  																				 \! Fecha final 
                 Gr.BonPag.Nbon$   																				  ! Nro Bono Generado

Integer*2 Global Gr.BonPag.PtgT%                                            ! Porcentaje Aplicar
Integer*2 Global Gr.BonPag.Ses%          																	  ! Sesion temporal
Integer*4 Global Gr.BonPag.Vlr%                                             ! Valor Bono recompra

Integer*1 Global Asic.Detalle%																							! Ctrl trailer trx
String    Global Gr.Lcl.Name$																								! Nombre del cliente
String    Global Gr.Lcl.Clte$                                               ! ID del cliente
Integer*1 Global Gr.Lcl.Ctrl%                                               ! Ctrl trx Lealtad
String    Global Gr.Lcl.TipClte$                                            ! Tipificaciones de clientes
String    Global IR.USERDATA$                                               ! Datos adicionales Itemr
Integer*4 GLOBAL UE.TABLAIVA(1)                                             ! Tarifas de IVA
                 
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


Sub BCOMPAG.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
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

Sub IMPRESION.BONO.COMPRA.PAG 																							! Impresion Bono Recompra Pagos
String Xnro$, BARCODE$, ENDBCODE$
Integer*2 X2%, Xctl%
  If Gr.BonPag.Vlr% > 0 Then Begin					     														! Hay Bono para Impresion
     To.USEREXIT(20) = 0																									  ! Cancela lineas impresion
     To.USEREXIT(60) = 0																									  !
     Call U.Imprime("*----  B O N O  R E C O M P R A  ---*",4201H)		      ! Impresion Bono recompra
     TS.TEMP1$ = "FECHA  : 20"+Left$(DATE$,2) + "-" + Mid$(DATE$,3,2) + "-" + \!
                 Right$(DATE$,2)
     TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												! Ajusta Dato impresion
     Call U.IMPRIME(TS.TEMP1$,4100H)																			  ! Imprime dato
     Xnro$ = "" : Xctl% = 0
     For X2% = 1 To Len(Gr.BonPag.Nbon$)
         If Xctl% = 0 Then Begin
         	  If Mid$(Gr.BonPag.Nbon$,X2%,1) <> "0" Then \
         	  	 Xctl% = 1 : Xnro$ = Mid$(Gr.BonPag.Nbon$,X2%,1)
         EndIf Else Begin
         	Xnro$ = Xnro$ + Mid$(Gr.BonPag.Nbon$,X2%,1)
         EndIf
     Next X2%  
     TS.TEMP1$ = LEFT$("BONO : "+Xnro$+STRING$(37," "),37)		 						  ! Ajusta Dato impresion
     Call U.IMPRIME(TS.TEMP1$,4400H)																			  ! Imprime dato
     TS.TEMP5I4 = ROUND(FLOAT(Gr.BonPag.Vlr%) * (0.+FLOAT(Gr.BonPag.PtgT%)/100.),0,0) ! Calcula dscto a otorgar
     CALL FORMAT.AMOUNT(TS.TEMP5I4)																					!
     TS.TEMP1$ = "VALOR DEL BONO :$"+TS.TEMP1$   														! Valor del Bono
     TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												! Ajusta Dato impresion
     Call U.IMPRIME(TS.TEMP1$,4211H)																			  ! Imprime dato
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
        Call U.IMPRIME(TS.TEMP1$,4401H)																		  ! Imprime dato
     EndIf
     TS.TEMP1$ = "FIRMA:______________________________"											! Firma
     TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												! Ajusta Dato impresion
     Call U.IMPRIME(TS.TEMP1$,4101H)																			  ! Imprime dato
     TS.TEMP1$ = Gr.Lcl.Name$                         											! Nombre del cliente
     TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												! Ajusta Dato impresion
     Call U.IMPRIME(TS.TEMP1$,4100H)																			  ! Imprime dato

!     BARCODE$  = Chr$(1DH) + Chr$(6BH) 					                            ! Manejo de impresion
!     ENDBCODE$ = Chr$(00H) 						                                      ! codigos de barra
!     TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(1)				                        ! Centra la impresion
!     Call U.IMPRIME(TS.TEMP1$,4000H)					                              !        
!     TS.TEMP1$ = Chr$(1DH)+Chr$(77H)+Chr$(01H)+ 		                       \! Horizontal barcode
!                 Chr$(1DH)+Chr$(68H)+Chr$(40H)+                            \! Vertical barcode			
!                 Chr$(1DH)+Chr$(48H)+Chr$(02H)+                            \! Human Read characters
!		             Chr$(1DH)+Chr$(66H)+Chr$(00H)    	                        ! activado
!     Call U.IMPRIME(TS.TEMP1$,4000H)				                                !
!     TS.TEMP1$ = (BARCODE$+Chr$(07H)+XNRO$+ENDBCODE$)                       ! Arma String de impresion			
!     Write #34 ; TS.TEMP1$          					                              ! Imprime cupon en EAN-128
!     TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(0)				                        ! Ajusta impresion a la Izquierda
!     Call U.IMPRIME(TS.TEMP1$,4000H)				                                !                    

     BARCODE$  = Chr$(1DH) + Chr$(6BH) 					                          ! Manejo de impresion
     ENDBCODE$ = Chr$(00H) 						                                    ! codigos de barra
     TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(1)				                      ! Centra la impresion
     Call U.IMPRIME(TS.TEMP1$,4000H)				                              !        
 		 TS.TEMP1$ = Chr$(1DH)+Chr$(77H)+Chr$(01H)+ 		                     \! Horizontal barcode
                 Chr$(1DH)+Chr$(68H)+Chr$(40H)+                          \! Vertical barcode			
                 Chr$(1DH)+Chr$(48H)+Chr$(02H)+                          \! Human Read characters
		             Chr$(1DH)+Chr$(66H)+Chr$(00H)    	                      ! activado
     Call U.IMPRIME(TS.TEMP1$,4000H)				                                !
     TS.TEMP2$ = Right$("000000000"+Str$(TS.TEMP5I4),9)                   ! Dato Impresion
     TS.TEMP1$ = BARCODE$+Chr$(07H)+Left$((XNRO$+TS.TEMP2$),44)+ENDBCODE$   ! Arma String de impresion			
     Call U.imprime(TS.TEMP1$,4000H)
     TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(0)				                        ! Ajusta impresion a la Izquierda
     Call U.IMPRIME(TS.TEMP1$,4000H)				                                !                    



     Asic.Detalle% = 1																											! Dato de la trx actual
     TS.TEMP1$ = Linea.Detalle(38)  																				! Trailer del movimiento
     Call U.IMPRIME(TS.TEMP1$,4200H)                                        !
     TS.TEMP1$ = String$(37,"*")																						! Linea cierre voucher
     Call U.IMPRIME(TS.TEMP1$,4900H)																			  ! avance de papel
     Call U.CORTACR																												  ! Corta Papel
     Call visor.and.borrar("BONO A ENTREGAR POR VALOR "+str$(TS.TEMP5I4))
     Call U.Imprime("*----  B O N O  R E C O M P R A  ---*",2100H)		      ! Impresion Bono recompra
     CALL FORMAT.AMOUNT(TS.TEMP5I4)																					!
     Call U.IMPRIME("BONO :"+XNRO$,2100H)
     Call U.IMPRIME("VALOR "+TS.TEMP1$,2100H)
     Call U.Imprime("*-----------------------------------*",2100H)		      ! Impresion Bono recompra
     To.USEREXIT(20) = -1																									  ! Activa  lineas impresion
     To.USEREXIT(60) = -1																									  !
     TS.LINETYPE = 18																											  ! Genera cabecera 
     TS.LINEDATA = 1																											  ! en proximo tiquete
     Call TSPREC01																												  !
  EndIf																																			! Fin impresion del bono

End Sub 																																		! Fin Impresion Bono Recompra

Sub Local.Bono.Compra
String Xkey$, X.Tmp$
Integer*4 Xctl%, Xcont%
  TS.TS11WERR$ = ""																													! Manejo Error appl
  Open "R::$F:BLOCAL" KEYED RECL 17 AS 56              											! Apertura control local bonos
  If TS.TS11WERR$ <> "" Then Begin                     											! Error apertura archivo
  	Call VISOR.AND.BORRAR("ERROR DE APERTURA TF:PLOCAL")                    ! Msg de alerta
  	TS.ER.RETURN = 0                                     										! Ctrl Errores
    Exit Sub 
  EndIf                                                											!
  TS.TS11WERR$ = ""
  Xkey$ = Pack$(Right$("000000000000000000"+Gr.Lcl.Clte$,18))               ! Arma llave de consulta
  Read Form "C9 2I4";#56 KEY Xkey$; 																			 \! Busca cliente
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
  Write Form "C9 2I4" ; #56;               											           \! Graba el registro
       Xkey$,                                                              \! Llave del registro     PD 9
       Xcont%, 																												     \! Cantidad de Bonos       I 4
       Xctl%																																! Total de bonos          I 4
  TS.ER.RETURN = -1																													! Proceso satisfactorio
  Fin.Local.Bono:
    Close 56
End Sub 

Function VALIDA.BONO.COMPRA.PAG
Integer*4 VALIDA.BONO.COMPRA.PAG
String    Xbuffer$, Xtemp4$, Xtrama$, XENVIA$,XLLEGA$, XSALE$, XRTA$
   VALIDA.BONO.COMPRA.PAG = 0																										! Init del proceso
   Call VISORES4690(1,"VALIDANDO CLIENTE   ","BONO RECOMPRA,ESPERE",0,"L")
   TS.TEMP5I4 = ROUND(FLOAT(Gr.BonPag.Vlr%) * (0.+FLOAT(Gr.BonPag.PtgT%)/100.),0,0) ! Calcula dscto a otorgar
   Xbuffer$ = Right$("                 "+Gr.Lcl.Clte$,15)    +             \! ID del cliente           Asc 15
              Gr.BonPag.Nbon$                                +             \! Numero del bono          Asc 29
              Right$("0000000000"+Str$(TS.TEMP5I4),10)       +             \! Valor bono calculado     Asc 10
              Right$("000"+Gr.BonPag.Ptg$(7),3)              +             \! Numero de bonos permitidos Asc 3
              Gr.BonPag.Fini$                                +             \! Fecha inicial proyecto   Asc 6
              Gr.BonPag.Ffin$                                               ! Fecha final del proyecto Asc 6

   XTemp4$ = Armar.Trama.Msg("16","51",Xbuffer$,"00","0001","123456")       ! Armar trama MSG
   Xenvia$ = XTemp4$																											  !
   Xsale$  = Time$																													! Hora salida requerimiento
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)         ! Ejecuta Requerimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   Xrta$     = Date$																												! Hora llegada requerimiento
   Xllega$   = Xtrama$																											!
   Call  BCOMPAG.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                    ! Rastro auditoria
   TS.ER.RETURN = -1																												! Control del proceso
   If Xtemp4$ <> "00" Then Begin 																						! Proceso No satisfactorio de java
   	  Gr.BonPag.Oflne% = 0
   	  Call Local.Bono.Compra																							  ! Generar bono y encolar proceso
   	  If TS.ER.RETURN = -1 Then Gr.BonPag.Oflne% = -1
   EndIf Else Begin																													! Comunicacion java exitosa 
    If Mid$(XTrama$,12,2) <> "00" Then Begin 																! Con Error de ejecucion 
    	Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))														! Presenta mensaje de Error 
    	 VALIDA.BONO.COMPRA.PAG = 0																						! Reporta falla en proceso
    	 Exit Function 																												! Sale del proceso
    EndIf	  																																!
   EndIf 																																		!

   If TS.ER.RETURN = -1 Then Begin 																					! Si proceso OK
   	If Gr.BonPag.Oflne% = 0 Then Begin																			! Bono en linea
      Xbuffer$ = Gr.BonPag.Nbon$                             +             \! Numero del bono          Asc 29
                 Gr.BonPag.Fini$                             +             \! Fecha inicial proyecto   Asc 6
                 Gr.BonPag.Ffin$                                            ! Fecha final del proyecto Asc 6
      XTemp4$ = Armar.Trama.Msg("16","52",Xbuffer$,"00","0001","123456")    ! Armar trama MSG
      XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)      ! Ejecuta Requerimiento
      VALIDA.BONO.COMPRA.PAG = -1 
    EndIf Else Begin																												! Bono de contingencia
      TS.TEMP5I4 = ROUND(FLOAT(Gr.BonPag.Vlr%) * (0.+FLOAT(Gr.BonPag.PtgT%)/100.),0,0) ! Calcula dscto a otorgar
      Xbuffer$ = Right$("                 "+Gr.Lcl.Clte$,15)    +          \! ID del cliente           Asc 15
                 Gr.BonPag.Nbon$                                +          \! Numero del bono          Asc 29
                 Right$("0000000000"+Str$(TS.TEMP5I4),10)       +          \! Valor bono calculado     Asc 10
                 Right$("000"+Gr.BonPag.Ptg$(7),3)              +          \! Numero de bonos permitidos Asc 3
                 Gr.BonPag.Fini$                                +          \! Fecha inicial proyecto   Asc 6
                 Gr.BonPag.Ffin$                                            ! Fecha final del proyecto Asc 6
      XTemp4$ = Armar.Trama.Msg("16","54",Xbuffer$,"00","0001","123456")    ! Armar trama MSG
      XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)      ! Ejecuta Requerimiento
      VALIDA.BONO.COMPRA.PAG = -1     																			! Proceso OK
    EndIf																																		! Fin encolamiento bono contingencia
   EndIf Else  VALIDA.BONO.COMPRA.PAG = 0																	  ! No tiene derecho a Bono
    	
End Function 

Sub CALCULO.BONO.COMPRA.PAG														                      ! Calculo del valor del bono recompra
  Integer*4 PRIC%, Xvta%, X.K%                                                                                   
  Integer*2 Xp1%, Xp2%, X.TARIFA%, XL%                                                                           
  String X.TMP$, Xpago$, Xclte$, Xstruc$,Xnada$                             !                                    
  Xvta% = 0                                                                 ! Total Venta                        
  Dim Gr.BonPag.Tipo$(7)                                                    ! Tipos de clientes                  
  Dim Gr.BonPag.Ptg$(7)                                                     ! Ptg Asignados                      
  Dim Gr.BonPag.Msg$(5)                                                     ! Mensajes parametro                 
  TS.ER.RETURN = -1                                                         ! Ctrl Errores                       
  TS.TS11WERR$ = ""                                                                                              
  Open "R::$F:PBONOS" KEYED RECL 44 AS Gr.BonPag.Ses% UNLOCKED NOWRITE NODEL  ! Parametros para bono recompra
  If TS.TS11WERR$ <> "" Then Begin                                          ! Error apertura archivo
  	Call VISORES4690(1,"ERROR DE APERTURA","TF:PBONOS",1200,"L")
 	 	Gr.BonPag.Vlr% = 0                                                      ! No aplica dscto en la trx
 	  Exit Sub                                                                ! Sale del proceso
  EndIf                                                                     !
  TS.TS11WERR$ = ""																													!
  Read Form "C4 2C3 5C4 5C2 C1 C3";#Gr.BonPag.Ses% KEY "8998";             \!
       X.Tmp$,                                                             \! Llave del proyecto                 
       Gr.BonPag.Fini$,                                                    \! Fecha inicial proyecto             
       Gr.BonPag.Ffin$,                                                    \! Fecha final del proyecto           
       Gr.BonPag.Tipo$(1),                                                 \! Tipificacion de cliente 1          
       Gr.BonPag.Tipo$(2),                                                 \! Tipificacion de cliente 2          
       Gr.BonPag.Tipo$(3),                                                 \! Tipificacion de cliente 3          
       Gr.BonPag.Tipo$(4),                                                 \! Tipificacion de cliente 4          
       Gr.BonPag.Tipo$(5),                                                 \! Tipificacion de cliente 5          
       Gr.BonPag.Ptg$(1),                                                  \! Porcentaje descuento 1             
       Gr.BonPag.Ptg$(2),                                                  \! Porcentaje descuento 2             
       Gr.BonPag.Ptg$(3),                                                  \! Porcentaje descuento 3             
       Gr.BonPag.Ptg$(4),                                                  \! Porcentaje descuento 4             
       Gr.BonPag.Ptg$(5),                                                  \! Porcentaje descuento 5             
       Gr.BonPag.Ptg$(6),                                                  \! Tipo validacion 1.Itm 2.Dpto 3.Todo
       Gr.BonPag.Ptg$(7)                                                    ! Numero de bonos a imprimir         

  If TS.TS11WERR$ <> "" Then Begin                                          ! No existe registro control
 	 	Gr.BonPag.Fini$ = "000000"
 	 	Gr.BonPag.Ffin$ = "000000"
  EndIf Else Begin                                                          !
 	 	Gr.BonPag.Fini$ = UnPack$(Gr.BonPag.Fini$)
 	 	Gr.BonPag.Ffin$ = UnPack$(Gr.BonPag.Ffin$)
  EndIf

  Read Form "C4 C37 C3";#Gr.BonPag.Ses% KEY "8995";                        \! Primer Mensaje definido
       X.Tmp$,                                                             \! Llave del proyecto     
       Gr.BonPag.Msg$(1),                                                  \! Fecha inicial proyecto 
       Xnada$

  Read Form "C4 C37 C3";#Gr.BonPag.Ses% KEY "8996";                        \! Segundo Mensaje definido
       X.Tmp$,                                                             \! Llave del proyecto      
       Gr.BonPag.Msg$(2),                                                  \! Fecha inicial proyecto  
       Xnada$
       
  Read Form "C4 C37 C3";#Gr.BonPag.Ses% KEY "8997";                        \! Tercer Mensaje definido                 
       X.Tmp$,                                                             \! Llave del proyecto                      
       Gr.BonPag.Msg$(3),                                                  \! Fecha inicial proyecto                  
       Xnada$                                                                                                        
                                                                                                                     
  Close Gr.BonPag.Ses%																                      ! cierra sesion temporal                  
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
  If TS.TS11WERR$ <> "" Then Begin                                         ! Error apertura archivo
  	Call VISORES4690(1,"ERROR DE APERTURA","TF:COMPAG",1200,"L")
 	 	Gr.BonPag.Vlr% = 0                                                      ! No aplica dscto en la trx                
 	  Exit Sub                                                                ! Sale del proceso                         
  EndIf                                                                     !                                          
  Gr.BonPag.Vlr% = 0                                                        ! Init variable                            
  For I% = 1 TO SL.END                                                      ! FOR ALL StringS                          
    H$ = READ.SL.STR$(I%)                                                   ! GET String                               
    If LEN(H$) > 5 Then                                                     \ ASSURE GOOD String                       
    If ASC(H$) = 1 Then Begin                                               ! ITEM ENTRY String                        
     X.TMP$ = ASIC.GETUNPK(H$,3)	   				                                ! Tomo informacion del item vendido        
     X.Tmp$ = Right$("000000000000"+X.Tmp$,12)                              !                                          
     X.Tmp$ = Pack$( X.tmp$ )                                               ! Arma llave de busqueda                   
     TS.ER.RETURN = -1																                      ! Ctrl errores                             
     Call IRRFEC.READ01 (X.tmp$, 4, TS.TEMP1$, 0) 		                      ! Lectura Datos Itemr                      
     Call IRRFEC.SPLIT1 ( TS.TEMP1$ ) 			                                ! Entrega datos al eamitemr.j86            
     Xstruc$ = Left$(IR.USERDATA$,16)		  					                        ! Estructura comercial                     
     Xstruc$ = Mid$(Xstruc$,2,9)                                            ! Toma los primeros 9 digitos              
                                                                                                                       
     TS.ER.RETURN = -1                                                      ! Ctrl de Error                            
     TS.TS11WERR$ = ""                                                      !                                          
                                                                                                                       
     If Gr.BonPag.Ptg$(6) <> "3" Then Begin                                 ! Validacion por Item o estrucura comercial
     	  If Gr.BonPag.Ptg$(6) = "1" Then                                    \! Validacion por Item                 
     	  	 X.tmp$ = IR.ITEMCODE$ + Pack$("01")                              !                                    
     	  If Gr.BonPag.Ptg$(6) = "2" Then                                    \! Validacion por Estructura comercial
     	  	 X.tmp$ = Pack$(Right$("000000000000"+Xstruc$,12)) + Pack$("01")  ! 
     	  	 
        Read Form "C7";#Gr.BonPag.Ses% KEY X.Tmp$; X.Tmp$                   ! Lee archivo 
        If TS.ER.RETURN <> -1 Then BEGIN                               ! Not found 
     	     GoTo Next.Itm.Compra                                             ! Siguiente Item vendido
        ENDIF 
     EndIf
     X.TARIFA% = 8                                                          ! Tarifa Default      
     If IR.INDICAT1 AND 80H Then X.TARIFA% = 1                              ! Tax plan A          
     If IR.INDICAT1 AND 40H Then X.TARIFA% = 2                              ! Tax plan B          
     If IR.INDICAT1 AND 20H Then X.TARIFA% = 3                              ! Tax plan C          
     If IR.INDICAT1 AND 10H Then X.TARIFA% = 4                              ! Tax plan D          
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
      EndIf                                                                 ! PRICE FOUND                      
     EndIf                                                                  ! ITEM SALE OR CANCEL              
    EndIf                                                                   ! Item ENTRY String                
    Next.Itm.Compra:                                                        ! Siguiente Item vendido           
  Next I%                                                                   ! NEXT String                      
  Close Gr.BonPag.Ses%                                                      ! Close File                       
  Gr.BonPag.Vlr% = Xvta%                                                    ! Retorna total de venta en la trx 
  Gr.BonPag.PtgT% = 0                                                       ! Init variable                    
  
  If Gr.BonPag.Vlr% > 0 Then Begin										 											! Hay Ventas de productos seleccionados
  	 Xclte$ = ""
  	 Gr.BonPag.PtgT% = 0
  	 For XL% = 1 To 5 																											! Recorre lista de tipificaciones
  	   X.K% = MATCH(Gr.BonPag.Tipo$(XL%),Gr.Lcl.TipClte$,1)  							  ! compara primer tipo de cliente
  	   If X.K% > 0 Then Begin                                               ! Encontro Clasificacion
  	   	  If Val(Gr.BonPag.Ptg$(XL%)) > Gr.BonPag.PtgT% Then Begin          ! Encuentra mejor beneficio
  	   	  	 Gr.BonPag.PtgT% = Val(Gr.BonPag.Ptg$(XL%))                     ! Asigna Porcentaje aplicar
  	   	  	 Xclte$ = Gr.BonPag.Tipo$(XL%)                                  ! Asigna tipificacion cliente
  	   	  EndIf
  	   EndIf
  	 Next XL%
  	
  	 If Gr.BonPag.PtgT% = 0 Then Begin                                      ! No hay ningun descuento
 	 	    Gr.BonPag.Vlr% = 0                                                  ! No aplica dscto en la trx
 	      Exit Sub                                                            ! Sale del proceso
  	 EndIf


    Gr.BonPag.Nbon$ = Right$("000000000000000"+Gr.Lcl.Clte$,15) +          \! Id del cliente
             "20"+ Date$ + TIME$                                            ! Fecha y hora de la trx

    TS.TEMP4I4 = VALIDA.BONO.COMPRA.PAG
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
		    ":"+"00"			                             													! Filler 

      Call Grabacion.String.Usuario2("20160107",TS.TEMP1$)                  ! Graba String de Usuario

  EndIf

End Sub 																																		! Fin bono recompra

Sub BONOCOMPRASPAG(XOPT%) Public		 																				! Bonos Recompra pago
Integer*4 Xopt%                                                             !
String    Xtmp$

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.BonPag.Ok%  = 0                                                        ! Proyecto Apagado
  Gr.BonPag.Stat% = 0																												! Init variable
  Gr.BonPag.Vlr%  = 0																											  ! Valor bono recompra
  TS.ER.RETURN = -1																													! Ctrl Errores                     
  TS.TS11WERR$ = ""
  OPEN "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL 													  ! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then BEGIN                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA BONO RECOMPRA")									! MSg alerta
  	 Exit Sub 																															! Sale del proceso
  ENDIF 																																		!
  IF END #94 THEN UE.FIN.BONOCMPAG      																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        IF TS.TEMP1$ = "[BONOS RECOMPRA PAGO]" Then Begin		         				! Bonos Recompra Pago
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.BonPag.Ok%   = Val(Mid$(TS.TEMP1$,30,2))   				    					! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.BonPag.Ses%  = Val(Mid$(TS.TEMP1$,30,02))      				          ! Numero Sesion Temporal
         GoTo UE.FIN.BONOCMPAG 																							! Sale del ciclo de carga          
       Endif                                                                !
   Wend                                                                     !
   UE.FIN.BONOCMPAG:                                                        !
     Close 94																																! Cierra archivo

   If Gr.BonPag.Ok% = -1 Then                                              \! Proyecto Activo
      Call U.IMPRIME("MODULO BONO COMP PAG ON  07-Ene-2015",6100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO BONO COMP PAG OFF 07-ene-2015",6100H)          ! Msg Proyecto Cargado
EndIf 																																			! Fin carga opciones

If Gr.BonPag.Ok% <> -1 Then Exit Sub                                        ! Si proyecto apagado

!--- EAMTSU02.J86
If Xopt% = 02 Then Begin                                                    ! Inicio de transaccion
	 Call IMPRESION.BONO.COMPRA.PAG																						! Generación Bono Recompra
	 Gr.BonPag.Stat% = 0																										  ! Init variable
	 Gr.BonPag.Vlr%  = 0																											! Valor bono recompra
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
           	   Call CALCULO.BONO.COMPRA.PAG                                 ! Calcula el valor del bono
           	EndIf
           	Gr.BonPag.Stat% = -1	  																			  ! Proceso Ejecutado
           EndIf																														!
        EndIf
     EndIf
  EndIf
  
EndIf																																				! Fin impresion CR


End Sub 
