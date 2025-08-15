!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMABJSN.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Dscto Escalonado Abbot    *
!                integracion Json                 *
!**************************************************
!Observaciones:
!
! - Version 1.0 - Octubre 2020
!-------------------------------------------------
! Mod 14Dic2021
! Si al terminar la trx no hay beneficio Gr.Abb2.VlrBnf%
! no se genera voucher en caja
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%ENVIRON T		                         																			! Ambiente de terminal

Integer*1 Global  Gr.Abb2.Ok%, Gr.Abb2.Stmp%, Gr.Abb2.CtlPago%,            \!
                  Gr.Abb2.Clte%, Asic.Detalle%, Gr.Abb2.Anul%,             \!
                  Gr.Abb2.Intrx%, Gr.Abb2.Open%, Gr.Abb2.Apl%,             \!
                  Gr.Abb2.CtzOk%
Integer*1 Global  Gr.NoTax.Proc%																						! Dia Sin iva
Integer*2 Global  Gr.Abb2.Motor%
Integer*4 Global  Asc.Tmp.Apun%, Gr.Abb2.Xcont%, Gr.Abb2.Cont%,				     \!
                  Gr.Abb2.TotBnf%, Gr.Abb2.VlrBnf%, PJ%, Gr.Abb2.Qitm%
String    Global  Gr.Abb2.Gdsc$, Gr.Abb2.Itm$(2), Gr.Lcl.Clte$,  				   \!
                  Gr.Abb2.Tm$, Gr.Abb2.Ean$, Gr.Abb2.Benef$(2),            \!
                  Gr.Abb2.TknConf$, Gr.Abb2.Voucher$(1), Gr.Lcl.Name$,     \!
                  Gr.Abb2.TknCotiza$, Gr.Abb2.TknConf2$, HORA.MUNDIAL$,    \!
                  Gr.Abb2.Ticket$, Gr.Abb2.TknAnul$, Gr.Abb2.Pagos$,       \!
                  Gr.Abb2.Prog$, Gr.Abb2.IdCot$, Gr.Abb2.TipCot$,          \!
                  Gr.Abb2.Precio$, Gr.Abb2.Name$, Gr.Abb2.Plu$
Integer*4 Global  Gr.Abb2.Loc%

%INCLUDE EAMTSEVA.J86          																							! Variables Cliente Frecuente
%INCLUDE EAMTSWKG.J86          																							! working storage
%INCLUDE EAMTOPTS.J86          																							! Terminal Options
%INCLUDE EAMTRANS.J86          																							! Variables de transancion
%INCLUDE EAMITEMR.J86          																							! Variables EamItemr
%INCLUDE EAMTSXHC.J86          																							! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          																							! Rutinas de Assembler

%INCLUDE RECATSSU.011        																								! Rutinas Genericas Grupo Retail

Function FORMAT.AMOUNT(AMT1) EXTERNAL
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                    *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                       *!
  STRING    DATA$      !* String to hold the data read from file.          *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                             *!
End Sub 

Sub IRRFEC.SPLIT1 (RECORD$) External
  String    RECORD$    !* The record (including the key) read from file    *!
End Sub 

Sub TSHIECET External                                                       !
End Sub 

Sub TSPREC01 EXTERNAL                                                       ! print routine                  
End Sub                                                                                                      
                                                                                                             
Sub TSTDEC01 EXTERNAL                                                       ! TENDERING                      
End Sub                                                                     !                                
                                                                                                             
Sub TSBDEC01 EXTERNAL                                                       ! balance due (normal/foodstamp) 
End Sub                                                                     !                                
                                                                                                             
Sub TSCSECCA EXTERNAL                                                       ! checker time analysis          
End Sub                                                                     !                                
                                                                                                             
Sub TSTDEC03 EXTERNAL                                                       ! tendering                      
End Sub 

Sub ABBOT.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)											!
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$,      \!
       X.RTA$, X.BUFF$																											!
Integer*4 X.Len%																														!
			TS.ER.RETURN = -1																											!
			X.FILE$ = "R::ADX_UDT1:AB" + Left$(DATE$,6) + "." +                  \!
			          Right$("000"+Str$(SL.HD.TERMINAL),3)                        !
			Open X.FILE$ AS 56 Append																							!
			If TS.ER.RETURN <> -1 Then Begin    																	! Si no existe
			   TS.ER.RETURN = -1																									!
				 CREATE X.FILE$ AS 56																								!
         If TS.ER.RETURN <> -1 Then Begin																		! 
            Call VISORES4690(1,"ERROR EN CREACION","DE AUDITORIA ",1500,"L")!
            Exit Sub 																												!
         EndIf 																															!
			EndIf 																																!
			X.Finr$ = Chr$(13) + Chr$(10)																					!
			X.BUFF$ = "["+X.SALE$+"]"+"MSG:"+X.ENVIA$															!
			X.Len% = Len(X.BUFF$)						  				  								          ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						          ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					        ! Graba registro
			X.BUFF$ = "["+X.RTA$+"]"+"RTA:"+X.LLEGA$                              !
			X.Len% = Len(X.BUFF$)						  				  								          ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						          ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					        ! Graba registro      
			Close 56																															!
End Sub 																																		!

Sub GR.DISENO.VOUCHABB2																										  ! Diseño Impresion Voucher
String XL$																																	!
     Dim Gr.Abb2.Voucher$(50)                                  			        ! Maximo 50 lineas de mensaje
     TS.ER.RETURN = -1 																											!
     OPEN "R::$F:ABBVCH" AS 94 Unlocked	NoWrite NoDel												! Apertura archivo formato mensajes
     If TS.ER.RETURN <> -1 Then BEGIN                                       ! Error apertura
  	    Call VISOR.AND.BORRAR("ERROR ARCHIVO VOUCHER ABBOT 2 ")             ! Msg Alerta
  	    Call U.IMPRIME("ERROR APERTURA VOCUHER ABB2  ",2100H)               !
    	  Gr.Abb2.Ok% = 0 																								  	! Inactiva proyecto
  	    Exit Sub 																														!
     ENDIF 																																	!
     K% = 1																																	!
     IF END #94 THEN UE.ABBOT.MSGVOUCHER2   												        ! Si es EOF
     While (1)																												      !
      Read #94; A$																													!
      If K% <= 50 Then Begin																						  	! Maximo 50 lineas de mensaje
         Gr.Abb2.Voucher$ (K%) = A$															    			  ! Almacena linea de mensaje
         Gr.Abb2.Voucher$ (0)  = STR$(K%)										 	      			  ! Numero de lineas total a imprimir
      EndIf																															  	!
      K% = K% + 1																									  		  	!
     Wend																																		!         
     UE.ABBOT.MSGVOUCHER2:																									!
       Close 94																															!
End Sub 																																		! Fin carga diseno voucher

Function Voucher.Str.Abb2(X.A$)            													        ! Coloca datos por parametro
String Voucher.Str.Abb2, X.A$, X.B$, X.C$, X.D$, XM$, Xwork$
Integer*4 X.A%, X.B%, Ypos%, XI%
Integer*1 Xsgn%

X.A% = MATCH("&",X.A$,1)						            														! Busca dato a imprimir
If X.A% > 0 Then Begin                          														! Si lo encotro
   X.B% = LEN(X.A$)                             														! Arma mensaje con el dato 
   X.B$ = Left$(X.A$,X.A%-1)                    														! solicitado, entregando 
   X.C$ = Mid$(X.A$,X.A%,2)                     														! 38 caracteres
   X.D$ = Mid$(X.A$,(X.A%+2),X.B%)
   If UCASE$(X.C$) = "&A" Then Begin            														! Valor del beneficio
      Call Format.Amount(Gr.Abb2.VlrBnf%)         													! 
      X.C$ = TS.TEMP1$																											! Retorna dato
   EndIf
   If UCASE$(X.C$) = "&B" Then Begin            														! Numero de cliene
      X.C$ = Gr.Lcl.Clte$
   EndIf
   If UCASE$(X.C$) = "&C" Then Begin            														! Nombre del cliente
      X.C$ = Gr.Lcl.Name$
   EndIf
   If UCASE$(X.C$) = "&D" Then Begin            														! Lista Items promocionados
   	  Xwork$ = ""
      For XI% = 1 To (Gr.Abb2.TotBnf% - 1)
          Xwork$ = Xwork$ + (Gr.Abb2.Benef$(XI%,2) +	" "	+	               \! EAN
                   Gr.Abb2.Benef$(XI%,3) + " " + 							 					   \! Descripcion
                   Gr.Abb2.Benef$(XI%,1) + CHR$(0DH))					    		      ! Qty
      Next XI%
      If Val(Gr.Abb2.Prog$) = 6 Or Val(Gr.Abb2.Prog$) = 7 Then Begin            ! Muestra gratis
         Xwork$ = Xwork$ + Gr.Abb2.Plu$  +	" "	+	               \! EAN
                   Gr.Abb2.Name$ + " " + 							 					   \! Descripcion
                   "1"  + CHR$(0DH)
      EndIf      
      X.C$ = "XXXXXX"
   EndIf
   Xsgn% = 1
   X.B$ = X.B$ + X.C$ + X.D$
   If X.C$ <> "XXXXXX" Then \
    X.B$ = Left$(X.B$+String$(37," "),37) Else \
    X.B$ = Xwork$
   X.A$ = X.B$
EndIf    
Voucher.Str.Abb2 = X.A$

End Function
!--- Fin funcion data String


Sub Confirma.Prg.Abb2
String Xbuff$, Xa$, Xtrama$, Xrta$
Integer*2 XV%, XT%, XJ%
Xv% = 1 : Xt% = 0
Xbuff$ = Gr.Abb2.TknConf$              +                                   \! Token confirmacion
         Gr.Abb2.Ean$                  + 																	 \! Ean evento
         Gr.Abb2.IdCot$                +                                   \! ID cotizacion 
         Gr.Abb2.Prog$                                                      ! ID programa
For XJ% = 1 To Gr.Abb2.Qitm%																					  		! Adiciona qty Items
  If Gr.Abb2.Itm$(Xj%,1) = "1" Then                                        \! Solo reporta los seleccionados
    Xbuff$ = Xbuff$ + Gr.Abb2.Itm$(Xj%,1) + Gr.Abb2.Itm$(Xj%,0)             ! Arma lista items
Next XJ%
If Val(Gr.Abb2.Prog$) = 6 Or Val(Gr.Abb2.Prog$) = 7 Then Begin
   Xbuff$ = Xbuff$ + "1"+Gr.Abb2.Ean$
EndIf Else Begin
	 If Gr.Abb2.Qitm% = 0 Then \
	 	  Xbuff$ = Xbuff$ + "1" + Gr.Abb2.Ean$
EndIf
While Xt% = 0
Act.Abbt.Prc:
  Call visores4690(1,"CONFIRMANDO PROCESO","ABBOT.. "+Str$(XV%),1000,"L")
  Xa$ = Right$("00"+Str$(Xv%),2) + Xbuff$																		! Arma mensaje
  Xa$ = Armar.Trama.Msg("18","63",Xa$,"00","0001","123456")   				      ! Armar trama MSG
  TS.TEMP4$ = DATE$ +":"+ Time$           																	!
  Xtrama$   = Rutina.Java("com.appl.ApplKernel","threader", Xa$)   		  	  ! Ejecuta Requerimiento
  
  !Xtrama$ = "1863086200000COTIZACION        SATISFACTORIA         3137                                    "
  
  TS.TEMP5$ = DATE$ +":"+ Time$           																	!
  XRTA$     = Valida.Rta(Xtrama$)	  																				! Valida rta entregada 
  Call ABBOT.AUDITORIA(Xa$,Xtrama$, TS.TEMP4$, TS.TEMP5$)         		  		! Log de auditoria Abbot     
  If Xrta$ <> "00" Then Begin
  	 Call VISOR.AND.BORRAR("FALLA GENERACION CONFIRMACION")
  	 Xv% = Xv% + 1
  	 If Xv% > 3 Then Begin																									! 
        Call U.IMPRIME("PROCESO NO CONFIRMADO ABBOT",2100H)
        Call VISOR.AND.BORRAR("PROCESO NO CONFIRMADO ABBOT")
  	    Exit Sub         
     EndIf
     GoTo Act.Abbt.Prc
  EndIf  
  If Mid$(Xtrama$,12,2) <> "00" Then Begin														  		! Falla en proceso
  	 Call VISOR.AND.BORRAR(Mid$(Xtrama$,14,40))						  		       		  ! Presenta Msg Error
  	 Call U.IMPRIME("PROCESO NO CONFIRMADO ABBOT",2100H)
  	 Call U.IMPRIME(Mid$(Xtrama$,14,40),2100H)
  	 Exit Sub 
  EndIf																																		  ! Fin falla proceso
  Call VISOR.AND.BORRAR("PROCESO SATISFACTORIO !!!")
  Xt% = -1
Wend 
End Sub 

Sub IMPRESION.VOUCHER.ABB2 																							    ! Impresion Voucher credito social
Integer*4 Xl%, Xfin%, Xj%, Xpos%, XM%
String    Xbuff$

  If Gr.Abb2.CtzOk% = -1 Then                                              \! Confirma movimiento
     Call Confirma.Prg.Abb2																									! Confirma proceso
  If Gr.Abb2.VlrBnf% <= 0 Then Exit Sub 																		! Sin beneficio no genera voucher 14Dic2021
  If Gr.Abb2.TotBnf% > 0 Then Begin				     													    ! Hay beneficio abbot
  	 Xpos% = 1                                														  ! autorizador bancario
  	 Xl% = 0 : Xfin% = Val(Gr.Abb2.Voucher$(0)) 									          ! Init Variables
      For XL% = 1 To Xfin%																									! Recorre diseño del voucher
         Xbuff$ = Voucher.Str.Abb2(Gr.Abb2.Voucher$(Xl%))            		    ! Analiza String
         If Xbuff$ <> "" Then Begin																			    ! Hay Info para imprimir
            Call U.Imprime(Xbuff$,6100H)	                                  ! Impresion voucher
         EndIf																															!
      Next Xl%																															! Fin impresion
      Asic.Detalle% = 1
      TS.TEMP1$ = Linea.Detalle(38)  																				! Trailer del movimiento
      Call U.IMPRIME(TS.TEMP1$,6200H)                                       !
      TS.TEMP1$ = String$(37,"-")																						! Linea cierre voucher
      Call U.IMPRIME(TS.TEMP1$,6900H)																			  ! avance de papel
      Call U.CORTACR																												! Corta Papel
     TS.LINETYPE = 18																											  ! Genera cabecera 
     TS.LINEDATA = 1																											  ! en proximo tiquete
     Call TSPREC01																												  !
  EndIf																																			! Fin impresion del bono
End Sub 

Function VALIDA.ABBOT(Xplu$)
String Xplu$,Xdummy$, X.Ean$
Integer*2 VALIDA.ABBOT
VALIDANDO.PLU.ABBOT:
    TS.TEMP2$ = Pack$(Right$("000000000000"+XPLU$,12))				  				    ! Armo dato para busqueda
    Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)                					! Lectura Datos Itemr
    Call IRRFEC.SPLIT1( TS.TEMP1$ ) 			                           				! Entrega datos al eamitemr.j86
    If UnPack$(IR.INDICAT2$) = "05" Then BEGIN 															! Si metodo precio 5
       XPlu$ = UnPack$(IR.SALEQUAN$) + UnPack$(IR.SALEPRIC$)                ! Toma PLU asignado a la barra
       GoTo VALIDANDO.PLU.ABBOT
    EndIf
    XPlu$ = Pack$( Right$("000000000000"+Xplu$,12) )                        ! Llave de busqueda
    TS.ER.RETURN = -1																										    ! Control Errores
    Read Form "C6 C7"; #93 KEY Xplu$; Xdummy$, X.Ean$                       !
    If TS.ER.RETURN = -1 Then Begin  																	     \! Encontrado en lista Abbot
       VALIDA.ABBOT = -1 
       Gr.Abb2.Precio$ = IR.SALEPRIC$
       Gr.Abb2.Ean$ = Str$(Val(Unpack$(X.ean$)))
       Gr.Abb2.Name$ = IR.ITEMNAME$
       Gr.Abb2.Plu$  = Unpack$(Xplu$)
    EndIf Else VALIDA.ABBOT = 0
End Function 

Sub ABB.GETUNPK
Integer*4 pk%
  PK% = MATCH(",",B$,PJ%) 																									! SEARCH FOR FIELD SEPERATOR
  If (PK% - PJ% ) > 0 Then                                                 \!
   A$ = (MID$(B$,PJ%,PK% - PJ%)) Else                                      \! UNPACK FIELD
   A$ = ""
  PJ% = PK% + 1 																														! POINT TO BeginNING OF NEXT FIELD
End Sub 


Function VALIDA.COTIZA.ABBOT
String Xplu$, Xclte$, Xrta$, Xtrama$, Xa$, Xtmp1$, Xtmp2$,                 \!
       Xlista$, X$, Xqty$
Integer*1 VALIDA.COTIZA.ABBOT
Integer*4 Xqty%, Xben%, XI%, XJ%
  Call VISORES4690(1,"VALIDANDO COTIZACION","ESPERE POR FAVOR",0,"L")
  Dim Gr.Abb2.Itm$(100,1)
  Dim Gr.Abb2.Benef$(10,4)
  Gr.Abb2.TotBnf% = 0
  If TS.IO.DATA$(6) <> "" Then \
  	Xqty$ = Right$("000"+TS.IO.DATA$(6),3) Else \
    Xqty$ = "001"
  Xclte$ = Gr.Abb2.Prog$ + Gr.Abb2.TknConf$ + Xqty$ + Gr.Abb2.Ean$
  Call TSHIECET
  Xa$ = Armar.Trama.Msg("18","62",Xclte$,"00","0001","123456")   				    ! Armar trama MSG
  TS.TEMP4$ = DATE$ +":"+ Time$           																	!
  Xtrama$   = Rutina.Java("com.appl.ApplKernel","threader", Xa$)   		  	  ! Ejecuta Requerimiento
  
  !Xtrama$ = "1862249200069COTIZACION SIN BENEFICIOS               15820       SaleDirect"
  
  TS.TEMP5$ = DATE$ +":"+ Time$           																	!
  XRTA$     = Valida.Rta(Xtrama$)	  																				! Valida rta entregada 
  Call ABBOT.AUDITORIA(Xa$,Xtrama$, TS.TEMP4$, TS.TEMP5$)         		  		! Log de auditoria Abbot     
  If Xrta$ <> "00" Then Begin
  	 Call VISOR.AND.BORRAR("FALLA GENERACION COTIZACION")
  	 VALIDA.COTIZA.ABBOT = 0
  	 Exit Function 
  EndIf  
  If Mid$(Xtrama$,12,2) <> "00" Then Begin														  		! Falla en proceso
   If Mid$(Xtrama$,12,2) = "69" Then Begin
  	 Call VISOR.AND.BORRAR(Mid$(Xtrama$,14,40))						  		       		  ! Presenta Msg Error
     Gr.Abb2.TknCotiza$ = ""
     Gr.Abb2.IdCot$     = Mid$(Xtrama$,54,12)
     Gr.Abb2.TipCot$    = Mid$(Xtrama$,66,10)
     Gr.Abb2.Qitm% = 0
     Gr.Abb2.CtzOk% = -1																												! Cotizacion satisfactoria
     Gr.Abb2.VlrBnf% = 0 																											!
     Gr.Abb2.TotBnf% = 0
  	 VALIDA.COTIZA.ABBOT = -1
  	 Exit Function 
   EndIf
	 Call VISOR.AND.BORRAR(Mid$(Xtrama$,14,40))						  		       		  ! Presenta Msg Error
 	 VALIDA.COTIZA.ABBOT = 0
 	 Exit Function 
  EndIf																																		  ! Fin falla proceso
  Gr.Abb2.TknCotiza$ = Mid$(Xtrama$,54,181)															    ! Lista beneficios
  Gr.Abb2.IdCot$     = Mid$(Xtrama$,54,12)
  Gr.Abb2.TipCot$    = Mid$(Xtrama$,66,10)
  Xben% = Val(Mid$(Xtrama$,76,03))																					! Nro beneficios 
  If Val(Gr.Abb2.Prog$) = 6 Or Val(Gr.Abb2.Prog$) = 7 Then                 \! Muestra gratis
     Xben% = Xben% -1
  Xlista$ = Mid$(Xtrama$,86,170)
  Gr.Abb2.Qitm% = 0
  Gr.Abb2.CtzOk% = -1																												! Cotizacion satisfactoria
  Pj% = 1 : A$ = ""
  Xqty% = 0 : XI% = 0
  B$ = XLista$																															!
  While XI% = 0 																														!
   Call ABB.GETUNPK																													!
   If A$ <> "" Then Begin																										!
   	  Xqty% = Xqty% + 1																											!
   	  Gr.Abb2.Itm$(Xqty%,0) = A$ 																						!
   	  Gr.Abb2.Itm$(Xqty%,1) = "0"																						!
   EndIf Else XI% = -1																											!
  Wend 																																			!
  Gr.Abb2.VlrBnf% = 0 																											!
  XI% = 0 : XJ% = 0
  Gr.Abb2.Qitm% = Xqty%																											! total items para seleccionar
  Call VISOR.AND.BORRAR("NRO BENEFICIOS PROG."+Str$(Xben%))
  For XI% = 1 To Xben%																											! Total de beneficios
   For XJ% = 1 To Xqty%																											! Total items de beneficio
       Xtmp2$ = Pack$(Right$("000000000000"+Left$(Gr.Abb2.Itm$(XJ%,0),12),12)) ! Armo dato para busqueda
       Lectura.Itm.Benef:
       Call IRRFEC.READ01 (XTMP2$, 4, XTMP1$, 0)                					  ! Lectura Datos Itemr
       If unpack$(MID$(XTMP1$,10,1)) = "05" Then Begin											! Si MP 5
       	  XPlu$ = UnPack$(MID$(XTMP1$,17,1)) + UnPack$(MID$(XTMP1$,18,5))   ! Toma PLU asignado a la barra
       	  Xtmp2$ = Pack$(Right$("000000000000"+Xplu$,12))				  		      ! Armo dato para busqueda
       	  GoTo Lectura.Itm.Benef
       EndIf																																!
       Call TSHIECET																												! Tono de alerta
       TS.TEMP3$ = ASIC.DATOS$("BNF "+STR$(XI%)+":"+MID$(XTMP1$,25,18),"PLU/ACEPTA BORR/NO")    !
       If TS.TEMP3$ <> "E" Then Begin
       	  Call VISOR.AND.BORRAR("PRODUCTO ACEPTADO   "+MID$(XTMP1$,25,18))	!
       	  Gr.Abb2.TotBnf% = Gr.Abb2.TotBnf% + 1
       	  Gr.Abb2.Benef$(Gr.Abb2.TotBnf%,0) = unpack$(Xtmp2$)               ! Almacena PLU
       	  Gr.Abb2.Benef$(Gr.Abb2.TotBnf%,2) = Gr.Abb2.Itm$(XJ%,0)           ! Almacena EAN
       	  Gr.Abb2.Benef$(Gr.Abb2.TotBnf%,3) = MID$(XTMP1$,25,18)            ! Almacena Descripcion
       	  Gr.Abb2.Benef$(Gr.Abb2.TotBnf%,4) = UnPack$(MID$(XTMP1$,18,5))    ! Almacena Precio venta
       	  Gr.Abb2.Benef$(Gr.Abb2.TotBnf%,1) = "1"                           ! Almacena Qty
       	  Gr.Abb2.Itm$(XJ%,1) = "1"                                         ! Entregado beneficio
       	  Gr.Abb2.VlrBnf% = Gr.Abb2.VlrBnf% + Val(UnPack$(MID$(XTMP1$,18,5))) ! Total beneficios
       	  XJ% = Xqty% + 1																										! Sale de lista 
       EndIf
   Next Xj%																																	!
  Next XI%																																	!
  If Val(Gr.Abb2.Prog$) = 6 Or Val(Gr.Abb2.Prog$) = 7 Then Begin            ! Muestra gratis
  	 Gr.Abb2.VlrBnf% = Gr.Abb2.VlrBnf% + Val(unpack$(Gr.Abb2.Precio$))      ! Total de muestras
  EndIf
  Gr.Abb2.TotBnf% = Gr.Abb2.TotBnf% + 1
  Gr.Abb2.Benef$(Gr.Abb2.TotBnf%,0) = "999999999999"             					  ! Almacena PLU
  Gr.Abb2.Benef$(Gr.Abb2.TotBnf%,2) = "999999999999"                			  ! Almacena EAN
  
  VALIDA.COTIZA.ABBOT = -1

End Function 

Function VALIDA.TOKEN.ABBOT
String Xplu$, Xclte$, Xrta$, Xtrama$, Xa$
Integer*1 VALIDA.TOKEN.ABBOT
  Call VISORES4690(1,"VALIDANDO TOKEN     ","ESPERE POR FAVOR",0,"L")
  
!  Xclte$ = asic.datos$("ingrese id cliente","...")
!      If Xclte$ = "E" Then BEGIN 																		  	    ! Proceso cancelado
!      	 Call VISOR.AND.BORRAR("PROCESO CANCELADO   POR EL USUARIO.")				! Msg Alerta
!      	 VALIDA.TOKEN.ABBOT = 0
!      	 Exit Function 
!      EndIf																																	!
!  Xclte$ = Right$("            "+Str$(Val(xclte$)),18)								      ! ID del cliente
     
  Xclte$ = Right$("            "+Str$(Val(Gr.Lcl.Clte$)),18)								! ID del cliente

  Call TSHIECET
  Xa$ = Armar.Trama.Msg("18","61",Xclte$,"00","0001","123456")   				    ! Armar trama MSG
  TS.TEMP4$ = DATE$ +":"+ Time$           																	!
  Xtrama$   = Rutina.Java("com.appl.ApplKernel","threader", Xa$)   		  	  ! Ejecuta Requerimiento
  
  !Xtrama$ = "1861110200000CONSULTA SATIFACTORIA                   3f1fbd8f3cfe2267905d2a007c2b0e10874f4115dfa140df071f37555c9add30"
  
  TS.TEMP5$ = DATE$ +":"+ Time$           																	!
  XRTA$     = Valida.Rta(Xtrama$)	  																				! Valida rta entregada 
  Call ABBOT.AUDITORIA(Xa$,Xtrama$, TS.TEMP4$, TS.TEMP5$)         		  		! Log de auditoria Abbot     
  If Xrta$ <> "00" Then Begin
  	 Call VISOR.AND.BORRAR("FALLA EN GENERACION TOKEN /Borrar")
  	 VALIDA.TOKEN.ABBOT = 0
  	 Exit Function 
  EndIf  
  If Mid$(Xtrama$,12,2) <> "00" Then Begin														  		! Falla en proceso
  	 Call VISOR.AND.BORRAR(Mid$(Xtrama$,14,40))						  		       		  ! Presenta Msg Error
  	 VALIDA.TOKEN.ABBOT = 0
  	 Exit Function 
  EndIf																																		  ! Fin falla proceso
  Gr.Abb2.TknConf$ = Mid$(Xtrama$,54,64)																	  ! Token de procedimiento
  Call VISORES4690(1,"TOKEN GENERADO",Gr.Abb2.TknConf$,1500,"L")
  VALIDA.TOKEN.ABBOT = -1
End Function 


Sub Aplica.Beneficio.Abbot																									!
String Xpago$, Xtmp$
Integer*4 XI%
	    Call VISORES4690(1,"APLICANDO PAGO",Str$(Gr.Abb2.VlrBnf%),1200,"L")
   	  Xpago$ = "05" 
      TS.TOTALS(0,0,0)  = TS.TOTALS(0,0,0) - Gr.Abb2.VlrBnf%             	  !
      TS.BALDUE(0)      = TS.BALDUE(0) - Gr.Abb2.VlrBnf%              	    ! Actualiza variables de la                             
      Sl.End = Sl.End + 1 																									! Aumenta apuntador
      Sl.Str$(Sl.End) = Pack$(Xpago$)                     +		             \! String forma de pago
		                    ":"+Pack$(Gr.Abb2.Gdsc$)          +		             \! Tipo y variedad de pago 
		                    ":"+Pack$(Str$(Gr.Abb2.VlrBnf%))  +                \! Valor del pago
		                    ":"+Pack$("00")                   +                \! Tender cashing fee amount
		                    ":"+Pack$(Gr.Lcl.Clte$)           +                \! Cliente 
		                    ":"+Pack$("50")                                     ! Status del pago
		  TS.TEMP1I1 = Val(Left$(Gr.Abb2.Gdsc$,1))										  				! Toma tipo de pago
		  TS.TEMP2$ = TS.SDESC$(TS.TEMP1I1)																			! Extrae descriptor
      Call Format.Amount(Gr.Abb2.VlrBnf%) 															    !
		  TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)                         !
		  TS.TEMP3$ = "     "+TS.TEMP2$ + " "+TS.TEMP1$  											  !
      Call U.IMPRIME(TS.TEMP3$,4101H)																				!
      Call U.IMPRIME(TS.TEMP3$,2100H)																				!
      TS.BD.DSPPARM = -1                              					  					! SHOW BD TO DISPLAY SUBTOTAL
      CALL TSBDEC01                                   											! DISPLAY NEW SUBTOTAL
      Dim Ts.Io.Keys(10)																										! Inicializa Vectores
      Dim Ts.Io.Data$(10)																										! 
      TS.IO.MOTORKEY = 0 																										! Tecla total
      TS.GUIDANCE = 1020																										! Obliga totalizar
      For XI% = 1 To (Gr.Abb2.TotBnf% - 1)      
       	 Xtmp$ =  Pack$(Gr.Abb2.Ean$)                    +                 \! EAN Compra
       	    ":" + Pack$(Gr.Abb2.Benef$(xi%,2))           +                 \! Ean Beneficio
 	          ":" + Pack$(Gr.Abb2.Benef$(xi%,1))			     + 								 \! Qty Beneficio
 	          ":" + Pack$(Gr.Abb2.Benef$(xi%,4))           +                 \! Valor Beneficio Item
 	          ":" + Gr.Abb2.TknConf$                       +                 \! Token confirmacion
 	          ":" + Pack$(Gr.Lcl.Clte$)                    +                 \! Cliente beneficiado
       	    ":" + Gr.Abb2.IdCot$                         +                 \! ID cotizacion 
            ":" + Gr.Abb2.Prog$                          +                 \! ID programa
 	          ":" + Pack$("00")																								! Signo Operacion
       Call Grabacion.Cadena.Usuario2("20201113",Xtmp$)        							! Almacena user data              
      Next XI% 
End Sub 

Sub GRABJSN(XOPT%) Public																							   		! Dscto Escalonado Abbot - Json
Integer*4 Xopt%, Xi%
String    Xtmp$
!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.Abb2.Ok%   = 0                                                         ! Proyecto Apagado
  Gr.Abb2.Clte% = 0
  Gr.Abb2.Tm$  = ""
  Gr.Abb2.CtlPago% = 0                                                      !
  Gr.Abb2.TotBnf% = 0
  TS.ER.RETURN = -1																													! Ctrl Errores                     
  OPEN "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL 														! Apertura archivo parametrizacion 
  If TS.ER.RETURN <> -1 Then BEGIN                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA ARCHIVO CONTROL ABBOT")
  	 Exit Sub 
  ENDIF 
  IF END #94 THEN UE.FIN.GRABJSN        																    ! Si es EOF                        
  While (1)															  															    ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        IF TS.TEMP1$ = "[DSCTO ABBOT 2]" Then Begin		 		   					      ! Beneficios Abbot
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Abb2.Ok%   = Val(Mid$(TS.TEMP1$,30,2))      			    				  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Abb2.Gdsc$ = (Mid$(TS.TEMP1$,30,2))         			  					  ! Tipo y Variedad de Pago
         GoTo UE.FIN.GRABJSN																						    ! Sale del ciclo de carga          
       Endif                                                                !
   Wend                                                                     !
   UE.FIN.GRABJSN:                                                          !
     Close 94																															  ! Cierra archivo
     Call GR.DISENO.VOUCHABB2																							  ! Carga diseño Voucher
     If Gr.Abb2.Open% = 0 Then Begin																				! Si apertura no procesado
     	  TS.ER.RETURN = -1																										! Ctrl Errores
        Open "R::$F:ITMAB2" KEYED RECL 13 AS 93 UNLOCKED NOWRITE NODEL      ! Apertura Articulos controlados Abbot
        If TS.ER.RETURN <> -1 Then Begin 																		! Falla en apertura
        	 Call VISOR.AND.BORRAR("ERROR APERTURA ARCHIVO TF:ITMAB2")        ! 
           Call U.IMPRIME("FALLA APERTURA TF:ITMAB2",2100H)                 !
           Gr.Abb2.Ok% = 0										      												! Ctrl aplicacion
        EndIf Else Begin                                                    !
        	 Gr.Abb2.Open% = -1																						  	! Apertura satisfactoria
        EndIf
     EndIf                                                                  !
   If Gr.Abb2.Ok% = -1 Then                                                \! Proyecto Activo
      Call U.IMPRIME("MODULO DSCTO ABBOT 2  ON 08/Ene/2020",2100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO DSCTO ABBOT 2 OFF 08/Ene/2020",2100H)          ! Msg Proyecto Cargado
EndIf 																																			! Final Eamtsu07

If Gr.Abb2.Ok% <> -1 Then Exit Sub                                          ! Si proyecto apagado

!--- EAMTSU02.J86
If Xopt% = 2 Then Begin                                                     ! Inicializacion de trx
  Call IMPRESION.VOUCHER.ABB2                                            ! Genera voucher abbot
  Gr.Abb2.CtlPago% = 0																											! Ctrl Aplicacion dscto
  Gr.Abb2.Clte% = 0 																												! Ctrl captura de cliente
  Gr.Abb2.Ean$ = ""                                                         ! Numero de cotizacion
  Gr.Abb2.Tm$  = ""                                                         ! Temporal
  Gr.Abb2.TknConf$ = ""
  Gr.Abb2.TotBnf% = 0
  Gr.Abb2.Xcont% = 0
  Gr.Abb2.Apl% = 0
  Gr.Abb2.Loc% = 0
  Gr.Abb2.TknCotiza$ = ""
  Gr.Abb2.Anul% = 0
  Gr.Abb2.TknConf2$ = ""
  Gr.Abb2.Ticket$ = ""
  Gr.Abb2.TknAnul$ = ""
  Gr.Abb2.Intrx% = 0
  Gr.Abb2.Cont% = 0
  Gr.Abb2.VlrBnf% = 0
  Gr.Abb2.IdCot$ = ""
  Gr.Abb2.TipCot$ = ""
  Gr.Abb2.CtzOk% = 0
EndIf 																																			! Final Eamtsu02

!--- EAMTSU08.J86
If Xopt% = 8 Then Begin         																						
	 If Gr.NoTax.Proc% <> 0 Then Exit Sub 																		! Aplicando dia Sin iva
	 TS.TEMP1I1 = 0
	 If Gr.Abb2.Intrx% = 0 Then \
	    TS.TEMP1I1 = VALIDA.ABBOT(SL.IT.ITEMCODE$)														! Valida Item Abbot
	 If TS.TEMP1I1 = -1 And Gr.Abb2.Intrx% = 0 Then Begin									    ! Es producto Abbot
      If Gr.Lcl.Clte$ = "" Then Begin																		  	! Cliente no capturado
         Call VISOR.AND.BORRAR("CLIENTE NO CAPTURADOEN COMPRA ABBOT")				! Msg Alerta
         Call VISOR.AND.BORRAR("PROCEDE VENTA NORMAL /Borrar")
         Exit Sub 
      EndIf 																													 		  ! fin valida client
      If TS.INTRX Then Begin                                                ! 
         Call VISOR.AND.BORRAR("HAY PROD. EN COMPRA NO APLICA BENEF")				! Msg Alerta
         Call VISOR.AND.BORRAR("PROCEDE VENTA NORMAL /Borrar")
         Exit Sub 
      EndIf
	 	  If TS.TRAINING Then Begin																							! Si en entrenamiento
	 	  	 Call VISOR.AND.BORRAR("PROCESO NO PERMITIDO EN ENTRENAMIENTO")     ! Msg Alerta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Exit Sub 																											    ! Termina proceso
	 	  EndIf																																	! Fin Salida
      Call TSHIECET																													! Tono de alerta
      TS.TEMP1$ = ASIC.DATOS$("INGRESE NRO DEL","PROGRAMA")
      If TS.TEMP1$ = "E" Then BEGIN 																		  	! Proceso cancelado
      	 Call VISOR.AND.BORRAR("PROCESO CANCELADO   POR EL USUARIO.")				! Msg Alerta
      	 IR.INDICAT0 = IR.INDICAT0 OR 04H			                             	! No permite venta Item
      	 Exit Sub 
      EndIf																																	!
      Gr.Abb2.Prog$ = Right$("   "+TS.TEMP1$,3)													    ! Nro del programa
      TS.TEMP1I1 = VALIDA.TOKEN.ABBOT            												    ! Valida servicio en linea
      If TS.TEMP1I1 = -1 Then Begin																				  ! Token Generado OK
         TS.TEMP1I1 = VALIDA.COTIZA.ABBOT        												    ! Valida cotizacion 
         If TS.TEMP1I1 = 0 Then Begin																				! Falla validacion
      	    Gr.Abb2.Prog$ = ""
      	    Gr.Abb2.TknConf$ = ""
            Exit Sub 
         EndIf
         Gr.Abb2.Intrx% = -1		  																					! Operacion exclusiva
         Gr.Abb2.Apl% = -1
         Gr.Abb2.Cont% = 0
         IR.LINKEDTO$ = Pack$(Gr.Abb2.Benef$(Gr.Abb2.Cont%,0))	            !
         
      EndIf Else Begin																											! Falla generacion del token
      	 Gr.Abb2.Prog$ = ""
      	 Gr.Abb2.TknConf$ = ""
         Call VISOR.AND.BORRAR("PROCEDE VENTA NORMAL /Borrar")
         Exit Sub 
      EndIf
   EndIf Else Begin																													! Fin Item abbot
       If Gr.Abb2.Intrx% = -1 And Gr.Abb2.Apl% = 0 Then Begin
         Call VISOR.AND.BORRAR("MOVIMIENTO EXCLUSIVOEN COMPRA ABBOT")				! Msg Alerta
         IR.INDICAT0 = IR.INDICAT0 OR 04H			                             	! No permite venta Item
         Exit Sub 
       EndIf
   EndIf

	 If Gr.Abb2.Intrx% = -1 And Gr.Abb2.Apl% = -1 Then Begin							    ! En simulacion
	 	  If Gr.Abb2.Cont% <= Gr.Abb2.TotBnf% Then Begin											  ! 
	 	  	 CHAIN.COUNT = 1                                             				! Contador de item linkeados
         TS.IO.KEYS(8) = 0
	 	  	 If Gr.Abb2.Benef$(Gr.Abb2.Cont%+1,0) = "999999999999" Then  Begin  ! Ultimo del proceso
	 	  	 	  IR.LINKEDTO$ = Pack$("0000")																  	! Termina ciclo
	 	  	 	  Gr.Abb2.Apl% = 0 
	 	  	 EndIf Else Begin 
            IR.LINKEDTO$ = Pack$(Gr.Abb2.Benef$(Gr.Abb2.Cont%+1,0))	        !
         EndIf
         If Gr.Abb2.Cont% > 0 Then Begin
           TS.IO.KEYS(6)  = 75																							  ! Simula cantidad
           TS.IO.DATA$(6) = "1"
         EndIf
	 	  	 Gr.Abb2.Cont% = Gr.Abb2.Cont% + 1																  ! 
	 	  EndIf 
	 EndIf																																		! Fin simulacion
  
EndIf																																		    ! Final Eamtsu08


!-- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                    ! En secuencia de tecleo

   If	TS.IO.KEYS(1) = 82 And TS.IO.MOTORKEY = 81 Then Begin                 ! Si Suspension de trx
 		If (Gr.Abb2.Intrx% <> 0) Then Begin                                     ! Si abbot capturado
 			 Call Visor.And.Borrar("SUSPENSION NO AUTOR.CON BENEFICIO ABBOT")     ! Msg de alerta
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
 	  EndIf																																		!
   EndIf

EndIf																																				! Final Eamtsu14

!-- EAMTSU20.J86
If Xopt% = 20 Then Begin                                                    ! En calculo de precio
   If MATCH(TS.SDESC$(59),TS.PRTBUF$,1) > 0 THEN BEGIN											! Si anulacion total de la trx
   	  Gr.Abb2.TotBnf% = 0
   	  Gr.Abb2.CtzOk%  = 0 
   EndIf
EndIf																																				! Final Eamtsu20

!-- EAMTSU43.J86
If Xopt% = 43 Then Begin                                                    ! En calculo de precio
	 If Gr.NoTax.Proc% <> 0 Then Exit Sub 																		! Aplicando dia Sin iva
	 If Gr.Abb2.Intrx% = -1 And Gr.Abb2.Apl% = -1 Then Begin							    ! En simulacion
	 	  If Gr.Abb2.Cont% <= Gr.Abb2.TotBnf% Then Begin											  ! 

 	    EndIf
   Endif 
   If Gr.Abb2.Intrx% = -1 And Gr.Abb2.Apl% = 0  Then Begin							    ! En simulacion
      If Gr.Abb2.Cont% >= Gr.Abb2.TotBnf% Then Begin                        ! Termina lista beneficios
  	     Gr.Abb2.CtlPago% = -1																							!
      EndIf
   EndIf
EndIf																																				! Final Eamtsu43

!--- EAMTSU60.J86
If Xopt% = 60 Then Begin                                                    ! Impresion CR:

 If TS.LINETYPE = 1 And (TS.LINEDATA = 1 Or TS.LINEDATA = 0) Then Begin                          !
  If (Gr.Abb2.CtlPago% = -1) Then begin  																	  ! No hay dscto aplicado   
     Call Aplica.Beneficio.Abbot
 	   Gr.Abb2.CtlPago% = 0
  	 TO.MGRKEY = 0
  EndIf
 EndIf

EndIf																									    									! Final Eamtsu60

End Sub 