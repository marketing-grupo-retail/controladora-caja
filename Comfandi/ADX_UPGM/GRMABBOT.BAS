!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMABBOT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Dscto Escalonado Abbot    *
!**************************************************
!Observaciones:
!
!Mod 29 Ago 2018
! Desarrollo entrega producto programa mas x ti 
! alimentacion.
! Desarrollado por Grupo Retail - OVS
!-------------------------------------------------- 

%ENVIRON T		                         																			! Ambiente de terminal

!Integer   Global  SL.END

Integer*1 Global  Gr.Abbot.Ok%, Gr.Abbot.Stmp%, Gr.Abbot.CtlPago%,         \!
                  Gr.Abbot.Clte%, Asic.Detalle%, Gr.Abbot.Anul%,           \!
                  Gr.Abbot.Intrx%
Integer*2 Global  Gr.Abbot.Motor%
Integer*4 Global  Asc.Tmp.Apun%, Gr.Abbot.Xcont%,         							   \!
                  Gr.Abbot.Apl%, Gr.Abbot.TotBnf%
String    Global  Gr.Abbot.Gdsc$, Gr.Abbot.Itm$(2), Gr.Lcl.Clte$,  				 \!
                  Gr.Abbot.Tm$, Gr.Abbot.Cotiza$, Gr.Abbot.Benef$(2),      \!
                  Gr.Abbot.TknConf$, Gr.Abbot.Voucher$(1), Gr.Lcl.Name$,   \!
                  Gr.Abbot.TknCotiza$, Gr.Abbot.TknConf2$, HORA.MUNDIAL$,  \!
                  Gr.Abbot.Ticket$, Gr.Abbot.TknAnul$, Gr.Abbot.Pagos$
Integer*4 Global  Gr.Abbot.Loc%
Integer*1 Global  Ue.mktp.Line%																							! Control motor promociones
Integer*1 Global  Gr.Abbot.Alimxti%

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

Sub TSHIECET External 
End Sub 

Sub IRRFEC.SPLIT1 (RECORD$) External
  String    RECORD$    !* The record (including the key) read from file    *!
End Sub 

Sub TSPREC01 EXTERNAL          ! print routine
End Sub

Sub TSTDEC01 EXTERNAL                           ! TENDERING
End Sub                                          !

Sub TSBDEC01 EXTERNAL          ! balance due (normal/foodstamp)
End Sub                        !

Sub TSCSECCA EXTERNAL          ! checker time analysis
End Sub                        !

Sub TSTDEC03 EXTERNAL          ! tendering
End Sub 


Sub ABBOT.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%

			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:AB" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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

Function Pfizer.Digito.Chequeo(Xplu$) External
String  Xplu$, Pfizer.Digito.Chequeo
End Function 

Sub Find.Items.Abbot
  Integer*4 PRIC%, TOT4%, XI%, XInd2%, X.J%, Xqty%, Xfind%				  !
  Integer*2 Xsgn%, Xpos%
  String    XTMP$, Xitm$, XPric$, Xdummy$, Xkey$, Xbasura$, X.Ean$, Xcot$   !
  
  TS.ER.RETURN = -1
  Open "R::$F:ITMABB" KEYED RECL 13 AS 94 UNLOCKED NOWRITE NODEL ! Apertura Articulos controlados
  If TS.ER.RETURN <> -1 Then BEGIN 																					! Error Apertura
  	 Call U.Imprime("Error Validacion Abbot ",2100H)                        !
     TS.TEMP1I1 = 0																													!
     Gr.Abbot.Xcont% = 0 																										! Numero de Items
  	 Exit Sub  																															!
  EndIf																																			!
  Gr.Abbot.Xcont% = 0 																											! Numero de Items
  Dim Gr.Abbot.Itm$(20,3)					 																			    ! Lista de Items para Abbot
  X.J% = 0 
  FOR X.J% = 1 TO SL.END                    																! FOR ALL StringS
    H$ = READ.SL.STR$(X.J%)                 																! GET String
    If LEN(H$) > 5 Then  Begin                                              ! ASSURE GOOD String
     If ASC(H$)  = 1 Then Begin             																! ITEM ENTRY String
     	  Asc.Tmp.Apun% = 3
        XItm$   = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Item Vendido
        Xpric$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Precio Venta
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Departamento
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Family 
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Indicat 1
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Indicat 2
        XInd2%  = Val(Xdummy$)																							! Vlr numerico Indicat 2
        Xsgn% = 1 																													! Trx de compra
        Xqty% = 0                                                           ! Sin cantidad o peso
        If (Xind2% And 0080H) Then Xsgn% = -1                               ! Item anulado
        If (Xind2% And 0040H) Then Xsgn% = -1                               ! Item anulado
        Xkey$ = Pack$( Right$("000000000000"+Xitm$,12) )                    ! Llave de busqueda
        TS.ER.RETURN = -1																										! Control Errores
        Xfind% = 0																													! Encontrado
        Xpos% = 0 
        TS.ER.RETURN = -1
        Read Form "C6 C7"; #94 KEY XKey$; Xdummy$, X.Ean$                   !
        If TS.ER.RETURN = -1 Then BEGIN 																		! Encontrado en lista Abbot
        	 Xpos% = 0																												! Ctrl busqueta Item 
        	 For XI% = 1 To Gr.Abbot.Xcont%																	  ! Busca Item
        	     If Val(Unpack$(X.Ean$)) = Val(Gr.Abbot.Itm$(Xi%,0)) Then    \! Lo encontro	
        	        Xpos% = Xi%																								! Asigna posicion
        	 Next XI%																													!
        	 If Xpos% = 0 Then Begin                                          ! Nuevo Item
        	    Gr.Abbot.Xcont% = Gr.Abbot.Xcont% + 1												  ! Posicion Almacenar
        	    Gr.Abbot.Itm$(Gr.Abbot.Xcont%,0) = Unpack$(X.Ean$)            ! Ean del Producto
        	    Gr.Abbot.Itm$(Gr.Abbot.Xcont%,1) = Str$( Xsgn% * 1 )          ! Cantidad Vendida
        	    Gr.Abbot.Itm$(Gr.Abbot.Xcont%,2) = Unpack$(XKey$)             ! SKU producto
        	    Gr.Abbot.Itm$(Gr.Abbot.Xcont%,3) = Xpric$                     ! Precio de Venta
        	    Xpos% = Gr.Abbot.Xcont%                                       ! Asigna posicion de vector
        	 EndIf Else Begin																									! Item Encontrado
        	 	  Gr.Abbot.Itm$(Xpos%,1) = Str$(Val(Gr.Abbot.Itm$(Xpos%,1)) +  \!
        	 	                                 (Xsgn% * 1))                   ! Cantidad Vendida
        	 EndIf
        EndIf	                  																				    ! Encontrado en Abbot
     EndIf                                  																! ITEM ENTRY String
     Asc.Tmp.Apun% = 3																										  !     
     If ASC(H$)  = 2 Then Begin             																! ITEM ENTRY String Extention
     	 If Xpos% <> 0 Then Begin 																						! Extention Item Abbot
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! MPGROUP       
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! DEALQUAN      
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! METODO PRECIO 
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! PRECIO        
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! CANTIDAD      
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! CANTIDAD      
        Xqty% = Val(Xdummy$)																								!
        Xqty% = Xqty% * Xsgn%									      												! Movimiento
        Gr.Abbot.Itm$(Xpos%,1) = Str$(Xqty% + Val(Gr.Abbot.Itm$(Xpos%,1)) - (Xsgn% * 1))! Cantidad Vendida
       EndIf 																																!
     EndIf																																	! ITEM ENTRY String Extention
    EndIf
  NEXT X.J%   
  Close 94																											! Cierre validacion 

End Sub 

Sub APLICA.DSCTO.ABBOT               																			  ! Calculo descuento
String Xtmp$, XEan$, XQty$, Xtrama$, Xrta$, Xdummy$, Xproc$, Xenvia$,			 \!
       Xrespta$, Xlista$, Xben$, Xplu$, Xtr$
Integer*4 XI%, XJ%, Grconf%

Xlista$ = ""
For XI% = 1 To Gr.Abbot.Xcont%																							! Recorre vector con items
  If Val(Gr.Abbot.Itm$(XI%,1)) > 0 Then Begin																! Si hay qty vendida
  	 Xean$ = Gr.Abbot.Itm$(XI%,0)																						! Toma Ean matriculado
  	 Xqty$ = Gr.Abbot.Itm$(XI%,1)																						! Toma Qty vendida
  	 Xplu$ = Gr.Abbot.Itm$(XI%,2)																						! SKU producto
     Xtrama$ = Right$("000"+Xqty$,3)   +                     							 \! Qty vendida
               Right$("0000000000000"+Xean$,13)                             ! Ean Producto
     Xlista$ = Xlista$ + Xtrama$																						! Arma lista productos
  EndIf																																			! Fin Qty vendida
Next Xi%																																		! Fin recorrido vector
If Xlista$ <> "" Then Begin																									! Lista de productos
	 Call VISORES4690(1,"VALIDANDO BENEFICIOS","ESPERE POR FAVOR",0,"L")
	 Xlista$ = Gr.Abbot.Cotiza$ + Xlista$																			! Msg lista 
   Xtrama$ = Armar.Trama.Msg("18","52",Xlista$,"00","0001","123456")      	! Armar trama MSG
   Xenvia$   = DATE$ +":"+ Time$																						!
   Xdummy$  = Rutina.Java("com.appl.ApplKernel","threader", Xtrama$)    		! Ejecuta Requerimiento          	
   
   !Xdummy$ = "1852274200000COTIZACION        SATISFACTORIA         D6AF02A6-E63B-4E4C-92CF-C6224B5CE6F80007702207305348"
      
   Xrespta$  = DATE$ +":"+ Time$																						!
   Xproc$ = Valida.Rta(Xdummy$)																							! Valida rta entregada 
   Call Abbot.AUDITORIA(Xtrama$,Xdummy$, Xenvia$, Xrespta$)       	  			! Log de auditoria Abbot    
   If Xproc$ <> "00" Then Begin																							! Falla comunicacion
   	  Call VISOR.AND.BORRAR("NO APLICA BENEFICIOS ABBOT 1")									! Msg alerta
	    Gr.Abbot.CtlPago% = -2																								! Cancela proceso
	    Exit Sub 																															!
   EndIf 																																		! Fin falla comunicacion
   If Mid$(Xdummy$,12,2) <> "00" Then Begin														  		! Falla en proceso
   	  Call VISOR.AND.BORRAR(Mid$(Xdummy$,14,40))						  		       		! Presenta Msg Error
			Gr.Abbot.CtlPago% = -2																								! Cancela proceso
			Exit Sub 																															!
   EndIf																																		! Fin falla proceso
   XBEN$ = MID$(Xdummy$,90,193)																							! Lista Beneficios
   Gr.Abbot.TknCotiza$ = MID$(Xdummy$,54,36)																! Token Cotizacion
   Dim Gr.Abbot.Benef$(20,5)
   For XJ% = 1 To Len(XBEN$) STEP 16
       Xtmp$ = MID$(XBEN$,XJ%,16)																						! Toma respuesta
       If XTMP$ <> "                " Then Begin														! Si hay respuesta
       	  Call TSHIECET																											! tono de alerta
       	  Xean$ = MID$(Xtmp$,4,12)																					! Ean Producto
       	  Xqty$ = MID$(Xtmp$,1,3)																						! Cantidad beneficio
          TS.ER.RETURN = -1																									!
          Xean$ = Pack$(Right$("000000000000"+Xean$,12))             				! Arma llave busqueda eamitemr
          Call IRRFEC.READ01 (Xean$, 4, TS.TEMP1$, 0)           						! Lectura Datos Itemr
          Call IRRFEC.SPLIT1 ( TS.TEMP1$ ) 			                   		  		! Entrega datos al eamitemr.j86
          If TS.ER.RETURN <> -1 Then IR.ITEMNAME$ = "ITEM NO ENCONTRADO" 		! Descripcion del Item
          !Xrta$ = ASIC.DATOS$(IR.ITEMNAME$,Xqty$+" BENEF. 1.SI 0.NO ")			! 
          Xrta$ = "1"																												! Siempre entrega solicitado 8 May 2017
          If Xrta$ = "1" Then Begin																					! Si No entrega
          	 Gr.Abbot.Loc% = Gr.Abbot.Loc% + 1 															! 
          	 Gr.Abbot.Benef$(Gr.Abbot.Loc%,0) = MID$(Xtmp$,4,13)					  ! Ean 
          	 Gr.Abbot.Benef$(Gr.Abbot.Loc%,1) = Xqty$                       ! Cantidad beneficio
          	 Gr.Abbot.Benef$(Gr.Abbot.Loc%,2) = Unpack$(IR.SALEPRIC$)       ! SKU
          	 Gr.Abbot.Benef$(Gr.Abbot.Loc%,3) = IR.ITEMNAME$                ! DESCRIPCION
             If Xqty$ = "000" Then                                         \! Programa de alimentacion mas x ti 
             		Gr.Abbot.Benef$(Gr.Abbot.Loc%,5) = "X"                      ! Reporta caso especial Item
          	 XI% = 0																												!
          	 For XI% = 1 To Gr.Abbot.Xcont%																	! Recorre vector con items
          	     If Val(Gr.Abbot.Itm$(XI%,2)) = Val(Gr.Abbot.Benef$(Gr.Abbot.Loc%,2)) Then \
          	     	  Gr.Abbot.Benef$(Gr.Abbot.Loc%,4) = Gr.Abbot.Itm$(XI%,1) ! Almacena cantida venta
          	 Next XI%																												!
          EndIf																															! Fin si entrega
       EndIf 																																! fin si hay respuesta
   Next XJ%																																	!
   XJ% = 0
   XLISTA$ = Gr.Abbot.Cotiza$ + Gr.Abbot.TknCotiza$													! Token consulta y cotizacion
   If Gr.Abbot.Loc% = 0 Then Begin																					! 
      Call VISOR.AND.BORRAR("NO APLICA BENEFICIOSEN ESTA COMPRA  Borrar")	  ! Msg alerta
    For XJ% = 1 To Gr.Abbot.Xcont%																					! Recorre vector con items
     If Val(Gr.Abbot.Itm$(XJ%,1)) > 0 Then Begin														! Si hay qty vendida, se adiciona igual a cero
        XLISTA$ = XLISTA$ + Right$("000"+Gr.Abbot.Itm$(XJ%,1),3)  +        \! Cantidad vendida
                            "000"+ Gr.Abbot.Itm$(XJ%,0)											! Qty benef y Ean Beneficio
     EndIf Else Begin

!     	If Val(Gr.Abbot.Itm$(XJ%,1)) = 0 And Gr.Abbot.Itm$(XJ%,5) = "X" Then Begin ! si programa alimentacion abbot
!        XLISTA$ = XLISTA$ + Right$("000"+Gr.Abbot.Itm$(XJ%,1),3)  +        \! Cantidad vendida
!                            "000"+ Gr.Abbot.Itm$(XJ%,0)											! Qty benef y Ean Beneficio
!     	EndIf
     EndIf
    Next XJ%
	  Call VISORES4690(1,"CONFIRMA  COMPRA    ","ESPERE POR FAVOR",0,"L")
   EndIf Else Begin  	
    For XJ% = 1 To Gr.Abbot.Loc%
       XLISTA$ = XLISTA$ + Right$("000"+Gr.Abbot.Benef$(Gr.Abbot.Loc%,4),3)  + \! Cantidad vendida
                           Right$("000"+Gr.Abbot.Benef$(Gr.Abbot.Loc%,1),3)  + \! Cantidad Beneficio
                           Gr.Abbot.Benef$(XJ%,0)														! Ean Beneficio
    Next XJ%
    Call VISORES4690(1,"CONFIRMA  BENEFICIOS","ESPERE POR FAVOR",0,"L")
   EndIf
   Grconf% = 0
   Grconf% = Grconf% + 1
   Xtr$ = Right$("00"+Str$(Grconf%),2) + Xlista$
   Xtrama$ = Armar.Trama.Msg("18","53",Xtr$,"00","0001","123456")      	    ! Armar trama MSG
   CONFIRMA.ABBOT:
   Xenvia$   = DATE$ +":"+ Time$																						!
   Xdummy$  = Rutina.Java("com.appl.ApplKernel","threader", Xtrama$)    		! Ejecuta Requerimiento          	
   
   !Xdummy$ = "1853118200000CONFIRMACION        SATISFACTORIA       2735C379-C28B-4EF6-80BC-2F220110F14FED484081-D70A-4AB8-B246-F0880DC5F65A"
   
   Xrespta$  = DATE$ +":"+ Time$																						!
   Xproc$ = Valida.Rta(Xdummy$)																							! Valida rta entregada 
   Call Abbot.AUDITORIA(Xtrama$,Xdummy$, Xenvia$, Xrespta$)       	  			! Log de auditoria Abbot      
!   If Xproc$ <> "00" Then Begin																						! Falla comunicacion
!      Call VISOR.AND.BORRAR("PROCESO NO CONFIRMADO")
!			Gr.Abbot.CtlPago% = -2																								! Cancela proceso
!			Exit Sub 																															!
!   EndIf

   If Mid$(Xdummy$,12,2) <> "00" Then Begin														  		! Falla en proceso
   	  Call VISOR.AND.BORRAR(Mid$(Xdummy$,14,40))						  		       		! Presenta Msg Error
      Xrta$ = ASIC.DATOS$("CONFIRMA NUEVAMENTE","BENEF.? 1.SI 0.NO")			  ! 
      If Xrta$ = "1" Then Begin																					    ! Si reconfirma
      	 Call VISORES4690(1,"RECONFIRMA PROCESO  ","ESPERE POR FAVOR",0,"L")
         Grconf% = Grconf% + 1
         Xtr$ = Right$("00"+Str$(Grconf%),2) + Xlista$
         Xtrama$ = Left$(Xtrama$,57) + Xtr$

      	 GoTo CONFIRMA.ABBOT
   	  EndIf
			Gr.Abbot.CtlPago% = -2																								! Cancela proceso
			Exit Sub 																															!
   EndIf																																		! Fin falla proceso
   Call VISOR.AND.BORRAR(Mid$(Xdummy$,14,34)+" Borra")	  		       				! Presenta Msg Confirmado
   Gr.Abbot.TknConf$  = Mid$(Xdummy$,54,36)                                 ! Token de Confirmacion
   Gr.Abbot.TknConf2$ = Mid$(Xdummy$,90,36)                                 ! Token de Confirmacion 2
   Gr.Abbot.Ticket$   = Right$(HORA.MUNDIAL$,12)														! AAMMDDHHMMSS de la operacion
	 Gr.Abbot.CtlPago% = -3																										! 
	 Exit Sub 																																!
EndIf Else Begin																														! Sin lista productos
	Gr.Abbot.CtlPago% = -2																										! Cancela proceso
	Exit Sub 																																	!
EndIf																																				! Fin proceso envio

End Sub 


Sub VALIDA.CLIENTE.ABBOT																										! Valida Cliente Abbot
String Xcl$, XRTA$, Xtrama$, Xa$, Xb$																				! VariabLES
String tata$
If Gr.Lcl.Clte$ = "" Then Begin																							! Cliente no capturado
   Call VISOR.AND.BORRAR("CLIENTE NO CAPTURADOEN COMPRA ABBOT")							! Msg Alerta
   TS.TEMP1$ = ASIC.DATOS$("SIGUE SIN BENEFICIO","1.SI 0.NO")								! Continua Sin beneficio
   If TS.TEMP1$ = "E" Then BEGIN 																		    		! Proceso cancelado   
   	  Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  		!
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  		!
      TS.IO.MOTORKEY = 73																						    		!
      TS.TEMP1I1 = 0 																												! Falla en el proceso
      Exit Sub 																											    		!
 EndIf 																														    		  !
 If TS.TEMP1$ = "1" Then TS.TEMP1I1 = 2 Else 														   \! Sigue Sin beneficio
	                 TS.TEMP1I1 = 0 																	        ! Falla en el proceso
 Exit Sub 																																  ! Sale del proceso
   
EndIf
TS.TEMP1$ = Right$("            "+Str$(Val(Gr.Lcl.Clte$)),18)								! ID del cliente

!-- Se comentarean estas lineas solo para desarrollo del modulo 
!If Val(Gr.Lcl.Clte$) = 1 Then tata$ = "52794364"
!If Val(Gr.Lcl.Clte$) = 2 Then tata$ = "1121925651"
!If Val(Gr.Lcl.Clte$) = 3 Then tata$ = "51878282"
	
!If Val(Gr.Lcl.Clte$) = 31712516 Then Begin
!	 tata$ = "399421"
!   TS.TEMP1$ = Right$("            "+tata$,18)								! ID del cliente
!Endif

Call VISORES4690(1,"VALIDA CLIENTE","ABBOT, ESPERE..",0,"L")
Call TSHIECET
Xa$ = Armar.Trama.Msg("18","51",TS.TEMP1$,"00","0001","123456")   					! Armar trama MSG     
TS.TEMP4$ = DATE$ +":"+ Time$           																		!
Xtrama$   = Rutina.Java("com.appl.ApplKernel","threader", Xa$)   		  			! Ejecuta Requerimiento

!Xtrama$ = "1851082200000CONSULTA SATIFACTORIA                   1F62A63C-A430-43E9-953B-E1EC3DE09B05"

TS.TEMP5$ = DATE$ +":"+ Time$           																		!
XRTA$     = Valida.Rta(Xtrama$)	  																				  ! Valida rta entregada 

Call ABBOT.AUDITORIA(Xa$,Xtrama$, TS.TEMP4$, TS.TEMP5$)         		  			! Log de auditoria Abbot     
If XRTA$ <> "00" Then Begin 
   TS.TEMP1$ = ASIC.DATOS$("SIGUE SIN BENEFICIO","1.SI 0.NO")								! Continua Sin beneficio
   If TS.TEMP1$ = "E" Then BEGIN 																		    		! Proceso cancelado   
   	  Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  		!
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  		!
      TS.IO.MOTORKEY = 73																						    		!
      TS.TEMP1I1 = 0 																												! Falla en el proceso
      Exit Sub 																											    		!
   EndIf 																														    		!
   If TS.TEMP1$ = "1" Then TS.TEMP1I1 = 2 Else 														\	! Sigue Sin beneficio
	                   TS.TEMP1I1 = 0 																	      ! Falla en el proceso
   Exit Sub 																																! Sale del proceso
EndIf
Xrta$ = Mid$(XTRAMA$,12,2)	             															  		! Valida rta entregada
If Xrta$ <> "00" Then Begin 																				  			! Rta Satisfactoria
	 Call VISOR.AND.BORRAR(Mid$(XTRAMA$,14,40))						  		          		! Presenta Msg Error
	 TS.TEMP1$ = ASIC.DATOS$("SIGUE SIN BENEFICIO","1.SI 0.NO")								! Continua Sin beneficio
   If TS.TEMP1$ = "E" Then BEGIN 																		    		! Proceso cancelado   
   	  Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  		!
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  		!
      TS.IO.MOTORKEY = 73																						    		!
      TS.TEMP1I1 = 0 																												! Falla en el proceso
      Exit Sub 																											    		!
   EndIf 																														    		!
	 If TS.TEMP1$ = "1" Then TS.TEMP1I1 = 2 Else 														\	! Sigue Sin beneficio
	                         TS.TEMP1I1 = 0 																	! Falla en el proceso
	 Exit Sub 																																! Sale del proceso
EndIf
Gr.Abbot.Cotiza$ = Mid$(XTRAMA$,54,36)																			! Token general de trx
TS.TEMP1I1 = -1 																														! Proceso OK
End Sub 

Sub Aplica.Benef.Nutrixti
Integer*4 Xi%, Xj%, Xvlr%
String Xtmp$

 Xvlr% = 0    	 
 For XJ% = 1 To Gr.Abbot.Loc%																								! Mira beneficios recibidos
  For XI% = 1 To Gr.Abbot.Xcont%																						! Mira lista items abbot en trx
    If Val(Gr.Abbot.Benef$(XJ%,2)) = Val(Gr.Abbot.Itm$(XI%,2)) Then Begin   ! Si hay venta 
     If Gr.Abbot.Benef$(XJ%,5) = "X" Then Begin															! Y es Nutricion x ti
     	  Xvlr% = Xvlr% + Val(Gr.Abbot.Itm$(XI%,3))														! suma valor
  	    Xtmp$ = Pack$(Gr.Abbot.Itm$(XI%,2))          +                     \! Ean Beneficio
 	          ":" + Pack$("00")				                 + 										 \! Qty Beneficio
 	          ":" + Pack$(Gr.Abbot.Itm$(XI%,3))        +                     \! Valor Beneficio Item
 	          ":" + Gr.Abbot.TknConf$                  +                     \! Token confirmacion
 	          ":" + Pack$("00")                                               ! Signo de la operacion
    	  Call Grabacion.Cadena.Usuario2("20170404",Xtmp$)          					! Almacena user data
     EndIf																																	!
    EndIf  
  Next XI%																																	!
 Next XJ%																																		!
  If Xvlr% > 0 Then Begin																										! Si hay que reportar
     Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 															! Init vectores de carga
     TS.IO.DATA$(3) = Right$(Gr.Abbot.Gdsc$,1)                   				    ! Variedad de Pago
     TS.IO.KEYS(3)  = 78                                             				! Tecla Slash
     TS.IO.DATA$(7) = Str$(Xvlr%)																						! Valor a entregar
     TS.IO.DATA$(9) = Gr.Lcl.Clte$																				  ! ID del Beneficiario
     TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.Abbot.Gdsc$,1))          				    ! Tipo de Pago
     TS.IO.MOTORKEY = TS.IO.KEYS(7)																					!
     Call TSTDEC03																													! Ejecuta secuencia pago
  EndIf
End Sub 

Sub Aplica.Beneficio.Abbot																									!
String Xpago$
   	  Ue.mktp.Line% = -1																										! Activa motor promociones
   	  If Gr.Abbot.Anul% = -1 Then Begin
   	  	 Gr.Abbot.Xcont% = 0
   	  	 Gr.Abbot.CtlPago% = 0
   	  	 Xpago$ = "06"
         TS.TOTALS(0,0,0)  = TS.TOTALS(0,0,0) + Gr.Abbot.TotBnf%           	!
         TS.BALDUE(0)      = TS.BALDUE(0) + Gr.Abbot.TotBnf%              	! Actualiza variables de la                             
      EndIf Else Begin
      	 Xpago$ = "05" 
         TS.TOTALS(0,0,0)  = TS.TOTALS(0,0,0) - Gr.Abbot.TotBnf%           	!
         TS.BALDUE(0)      = TS.BALDUE(0) - Gr.Abbot.TotBnf%              	! Actualiza variables de la                             
      EndIf
      Sl.End = Sl.End + 1 																									! Aumenta apuntador
      Sl.Str$(Sl.End) = Pack$(Xpago$)                     +		             \! String forma de pago
		                    ":"+Pack$(Gr.Abbot.Gdsc$)         +		             \! Tipo y variedad de pago 
		                    ":"+Pack$(Str$(Gr.Abbot.TotBnf%)) +                \! Valor del pago
		                    ":"+Pack$("00")                   +                \! Tender cashing fee amount
		                    ":"+Pack$(Gr.Lcl.Clte$)           +                \! Cliente 
		                    ":"+Pack$("50")                                     ! Status del pago
		  TS.TEMP1I1 = Val(Left$(Gr.Abbot.Gdsc$,1))										  				! Toma tipo de pago
		  TS.TEMP2$ = TS.SDESC$(TS.TEMP1I1)																			! Extrae descriptor
		  If Gr.Abbot.Anul% = -1 Then                     				             \! Formatea valor 
		     Call Format.Amount(Gr.Abbot.TotBnf% * -1) Else                    \!
		     Call Format.Amount(Gr.Abbot.TotBnf%) 															!
		  TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)                         !
		  TS.TEMP3$ = "     "+TS.TEMP2$ + " "+TS.TEMP1$  											          !
      Call U.IMPRIME(TS.TEMP3$,4101H)																				!
      Call U.IMPRIME(TS.TEMP3$,2100H)																				!
      Gr.Abbot.CtlPago% = -5
      TS.BD.DSPPARM = -1                              					  					! SHOW BD TO DISPLAY SUBTOTAL
      CALL TSBDEC01                                   											! DISPLAY NEW SUBTOTAL
      Dim Ts.Io.Keys(10)																										! Inicializa Vectores
      Dim Ts.Io.Data$(10)																										! 
      TS.IO.MOTORKEY = 0 																										! Tecla total
      TS.GUIDANCE = 1020																										! Obliga totalizar
End Sub 

Sub GR.DISENO.VOUCHABBOT																										! Diseño Impresion Voucher
String XL$																																	!
     Dim Gr.Abbot.Voucher$(50)                                  			      ! Maximo 50 lineas de mensaje
     TS.ER.RETURN = -1 																											!
     OPEN "R::$F:ABBVCH" AS 94	UNLOCKED NoWrite NoDel											! Apertura archivo formato mensajes
     If TS.ER.RETURN <> -1 Then BEGIN                                       ! Error apertura
  	    Call VISOR.AND.BORRAR("ERROR ARCHIVO VOUCHER ABBOT   ")             ! Msg Alerta
  	    Call U.IMPRIME("ERROR APERTURA VOCUHER ABBOT ",2100H)               !
    	  Gr.Abbot.Ok% = 0 																								  	! Inactiva proyecto
  	    Exit Sub 																														!
     ENDIF 																																	!
     K% = 1																																	!
     IF END #94 THEN UE.ABBOT.MSGVOUCHER    												        ! Si es EOF
     While (1)																												      !
      Read #94; A$																													!
      If K% <= 50 Then Begin																						  	! Maximo 50 lineas de mensaje
         Gr.Abbot.Voucher$ (K%) = A$															    			! Almacena linea de mensaje
         Gr.Abbot.Voucher$ (0)  = STR$(K%)										 	      			! Numero de lineas total a imprimir
      EndIf																															  	!
      K% = K% + 1																									  		  	!
     Wend																																		!         
     UE.ABBOT.MSGVOUCHER:																										!
       Close 94																															!
End Sub 																																		! Fin carga diseno voucher

Function Voucher.Str.Abbot(X.A$)            													! Coloca datos por parametro
String Voucher.Str.Abbot, X.A$, X.B$, X.C$, X.D$, XM$, Xwork$
Integer*4 X.A%, X.B%, Ypos%, XI%
Integer*1 Xsgn%

X.A% = MATCH("&",X.A$,1)						            														! Busca dato a imprimir
If X.A% > 0 Then Begin                          														! Si lo encotro
   X.B% = LEN(X.A$)                             														! Arma mensaje con el dato 
   X.B$ = Left$(X.A$,X.A%-1)                    														! solicitado, entregando 
   X.C$ = Mid$(X.A$,X.A%,2)                     														! 38 caracteres
   X.D$ = Mid$(X.A$,(X.A%+2),X.B%)

   If UCASE$(X.C$) = "&A" Then Begin            														! Valor del beneficio
      Call Format.Amount(Gr.Abbot.TotBnf%)         													! 
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
      For XI% = 1 To Gr.Abbot.Loc%
          Xwork$ = Xwork$ + (Gr.Abbot.Benef$(XI%,0) +	" "	+	 \! EAN
                   Gr.Abbot.Benef$(XI%,3) + " " + 							 					 \! Descripcion
                   Gr.Abbot.Benef$(XI%,1) + CHR$(0DH))					    		    ! Qty
      Next XI%
      X.C$ = "XXXXXX"
   EndIf

   Xsgn% = 1

   X.B$ = X.B$ + X.C$ + X.D$
   If X.C$ <> "XXXXXX" Then \
    X.B$ = Left$(X.B$+String$(37," "),37) Else \
    X.B$ = Xwork$
   X.A$ = X.B$
EndIf    
Voucher.Str.Abbot = X.A$

End Function
!--- Fin funcion data String

Sub IMPRESION.VOUCHER.ABBOT 																							  ! Impresion Voucher credito social
Integer*4 Xl%, Xfin%, Xj%, Xpos%, XM%
String    Xbuff$
  If Gr.Abbot.TotBnf% > 0 Then Begin				     													  ! Hay beneficio abbot
  	 Xpos% = 1                                														  ! autorizador bancario
  	 Xl% = 0 : Xfin% = Val(Gr.Abbot.Voucher$(0)) 									          ! Init Variables
!     To.USEREXIT(20) = 0																									  ! Cancela lineas impresion
!     To.USEREXIT(60) = 0																									  !
      For XL% = 1 To Xfin%																									! Recorre diseño del voucher
         Xbuff$ = Voucher.Str.Abbot(Gr.Abbot.Voucher$(Xl%))            		  ! Analiza String
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
!     To.USEREXIT(20) = -1																									  ! Activa  lineas impresion
!     To.USEREXIT(60) = -1																									  !
     TS.LINETYPE = 18																											  ! Genera cabecera 
     TS.LINEDATA = 1																											  ! en proximo tiquete
     Call TSPREC01																												  !
  EndIf																																			! Fin impresion del bono
End Sub 

Sub ANULA.BENEFICIO.ABBOT																										!
String Xtrama$, Xlista$, Xenvia4, Xdummy$, Xrespta$, Xproc$, Xenvia$
   Call VISORES4690(1,"ANULANDO  BENEFICIOS","ESPERE POR FAVOR",0,"L")
   Xlista$ =    Gr.Abbot.Cotiza$ + Gr.Abbot.TknConf$ + Gr.Abbot.Ticket$     ! Token cliente confirmacion y mmss trx
   Xtrama$ = Armar.Trama.Msg("18","54",Xlista$,"00","0001","123456")      	! Armar trama MSG
   Xenvia$   = DATE$ +":"+ Time$																						!
   Xdummy$  = Rutina.Java("com.appl.ApplKernel","threader", Xtrama$)    		! Ejecuta Requerimiento          	
   
   !Xdummy$ = "1854082200000CANCELACION         SATISFACTORIA       153F25AF-1DB6-4A41-9951-8C9DA7F32A79"
   
   Xrespta$  = DATE$ +":"+ Time$																						!
   Xproc$ = Valida.Rta(Xdummy$)																							! Valida rta entregada 
   Call Abbot.AUDITORIA(Xtrama$,Xdummy$, Xenvia$, Xrespta$)       	  			! Log de auditoria Abbot    
   If Xproc$ <> "00" Then Begin																							! Falla comunicacion
      Call VISOR.AND.BORRAR("PROCESO NO CONFIRMADO")
      TS.TEMP1I1 = 0
			Exit Sub 																															!
   EndIf
   If Mid$(Xdummy$,12,2) <> "00" Then Begin														  		! Falla en proceso
   	  Call VISOR.AND.BORRAR(Mid$(Xdummy$,14,40))						  		       		! Presenta Msg Error
   	  TS.TEMP1I1 = 0
			Exit Sub 																															!
   EndIf																																		! Fin falla proceso
   Gr.Abbot.TknAnul$ = Mid$(Xdummy$,54,36)																	! Token de anulacion
   TS.TEMP1I1 = -1
End Sub 																																		!

Sub GRABBOT(XOPT%) Public																							   		! Dscto Escalonado Abbot
Integer*4 Xopt%, Xi%
String    Xtmp$
!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.Abbot.Ok%   = 0                                                        ! Proyecto Apagado
  Gr.Abbot.Clte% = 0
  Gr.Abbot.Tm$  = ""
  Gr.Abbot.CtlPago% = 0                                                     !
  Gr.Abbot.TotBnf% = 0
  Gr.Abbot.Alimxti% = 0
  TS.ER.RETURN = -1																													! Ctrl Errores                     
  OPEN "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL 														! Apertura archivo parametrizacion 
  If TS.ER.RETURN <> -1 Then BEGIN                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA ARCHIVO CONTROL ABBOT")
  	 Exit Sub 
  ENDIF 
  IF END #94 THEN UE.FIN.GRAbbot        																    ! Si es EOF                        
  While (1)															  															    ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        IF TS.TEMP1$ = "[DSCTO ABBOT]" Then Begin		   		   					      ! Beneficios Abbot
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Abbot.Ok%   = Val(Mid$(TS.TEMP1$,30,2))      			    				  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Abbot.Gdsc$ = (Mid$(TS.TEMP1$,30,2))         			  					  ! Tipo y Variedad de Pago
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Abbot.Motor% = Val(Mid$(TS.TEMP1$,30,3))      			   				  ! Tecla Motora
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Abbot.Stmp%  = Val(Mid$(TS.TEMP1$,30,2))      			   				  ! Sesion Temporal
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Abbot.Pagos$  = Mid$(TS.TEMP1$,30,40)         			   				  ! Teclas motoras pagos
         
         GoTo UE.FIN.GRAbbot																						    ! Sale del ciclo de carga          
       Endif                                                                !
   Wend                                                                     !
   UE.FIN.GRAbbot:                                                          !
     Close 94																															  ! Cierra archivo
     Call GR.DISENO.VOUCHABBOT																							! Carga diseño Voucher
   If Gr.Abbot.Ok% = -1 Then                                               \! Proyecto Activo
      Call U.IMPRIME("MODULO DSCTO ABBOT    ON 27/Sep/2018",2100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO DSCTO ABBOT   OFF 27/Sep/2018",2100H)          ! Msg Proyecto Cargado
EndIf 																																			! Final Eamtsu07

If Gr.Abbot.Ok% <> -1 Then Exit Sub                                         ! Si proyecto apagado

!--- EAMTSU02.J86
If Xopt% = 2 Then Begin                                                     ! Inicializacion de trx
  If Gr.Abbot.TotBnf% > 0 Then 																						 \! Con beneficio
     Call IMPRESION.VOUCHER.ABBOT                                           ! Genera voucher abbot

  Gr.Abbot.CtlPago% = 0																											! Ctrl Aplicacion dscto
  Gr.Abbot.Clte% = 0 																												! Ctrl captura de cliente
  Gr.Abbot.Cotiza$ = ""                                                     ! Numero de cotizacion
  Gr.Abbot.Tm$  = ""                                                        ! Temporal
  Gr.Abbot.TknConf$ = ""
  Gr.Abbot.TotBnf% = 0
  Gr.Abbot.Xcont% = 0
  Gr.Abbot.Apl% = 0
  Gr.Abbot.Loc% = 0
  Gr.Abbot.TknCotiza$ = ""
  Gr.Abbot.Anul% = 0
  Gr.Abbot.TknConf2$ = ""
  Gr.Abbot.Ticket$ = ""
  Gr.Abbot.TknAnul$ = ""
  Gr.Abbot.Intrx% = 0
  Gr.Abbot.Alimxti% = 0
EndIf 																																			! Final Eamtsu02


!--- EAMTSU08.J86
If Xopt% = 8 Then Begin                                             			 	! Validacion Venta Items
 If (Gr.Abbot.CtlPago% = -2 Or Gr.Abbot.CtlPago% = -4 Or Gr.Abbot.CtlPago% = -5) Then Begin 	! Si dscto Aplicado Abbot
 	   Call VISOR.AND.BORRAR("EXISTE DSCTO ABBOT NO PERMITE VENTA")           ! Msg al operador
 	   IR.INDICAT0 = IR.INDICAT0 OR 04H			                                 	! No permite venta Item
 EndIf
 If (Gr.Abbot.CtlPago% = -3) Then Begin   																 	! Aplicando beneficios Abbot
     If Gr.Abbot.Apl% < Gr.Abbot.Loc% Then Begin														! No es ultimo Item
     	  CHAIN.COUNT = 1                                             				! Contador de item linkeados
     	  If Val(Gr.Abbot.Benef$(Gr.Abbot.Apl%,1)) = 0 Then                  \!
     	  	 TS.IO.DATA$(6) = "1" Else                                       \!
     	  	 TS.IO.DATA$(6) = Gr.Abbot.Benef$(Gr.Abbot.Apl%,1)						    ! Qty Vendida
        TS.IO.KEYS(6)  = 75
     	  IR.LINKEDTO$ = pack$(Gr.Abbot.Benef$(Gr.Abbot.Apl%+1,2))
     	  If Gr.Abbot.Anul% = -1 Then TS.IO.KEYS(1) = 70
     EndIf Else Begin																												! Termina Lista
     	  CHAIN.COUNT = 1                                             				! Contador de item linkeados
     	  If Val(Gr.Abbot.Benef$(Gr.Abbot.Apl%,1)) = 0 Then                  \!
     	  	 TS.IO.DATA$(6) = "1" Else                                       \!
     	  	 TS.IO.DATA$(6) = Gr.Abbot.Benef$(Gr.Abbot.Apl%,1)						    ! Qty Vendida
     	  TS.IO.KEYS(6)  = 75
     	  IR.LINKEDTO$ = pack$("0000")
     	  If Gr.Abbot.Anul% = -1 Then TS.IO.KEYS(1) = 70
     EndIf																																	!
 EndIf																																			! Fin beneficios abbot
 
EndIf 																												             	! Final Eamtsu02


!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                    ! Secuencias de tecleo
 
 If (Gr.Abbot.CtlPago% = 0) Then \				  																! No hay dscto aplicado
  If (TS.IO.KEYS(7) > 90 AND TS.IO.KEYS(7) < 97) Or                        \!
     (Match((";"+Str$(TS.IO.MOTORKEY)+";"),Gr.Abbot.Pagos$,1) > 0 ) Then Begin                            ! Si forma de pago 
    Gr.Abbot.Xcont% = 0																											! Reset contador
    Call Find.Items.Abbot																										! Analiza lista de Items Abbot
    If Gr.Abbot.Xcont% > 0 Then Begin 																			! Si hay Items Abbot
    	 TS.TEMP1I1 = 0 																											! Control del proceso
    	 Call VALIDA.CLIENTE.ABBOT																						! Valida derechos cliente
    	 If TS.TEMP1I1 <> 0 Then BEGIN 																				! Si cliente OK
          If TS.TEMP1I1 = 2 Then Begin																			! Si proceso cancelado por cliente
          	 Gr.Abbot.CtlPago% = -1																					! Activa continuar proceso
          	 Gr.Abbot.Xcont% = 0																						! Anula items
          	 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!	Reinicia secuencia de 
          	 TS.IO.MOTORKEY = 73																						! tecleo
          	 Exit Sub 																											! Sale del proceso
          EndIf
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													!	Genera aplicacion beneficios
     	    TS.IO.MOTORKEY = Gr.Abbot.Motor%																	! Aplica dscto Abbot
    	    Gr.Abbot.CtlPago% = -1																						!
    	 EndIf  Else BEGIN																										! Falla validacion cliente
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													! Reinicia secuencias tecleo
     	    TS.IO.MOTORKEY = 73               																! 
     	    Gr.Abbot.Xcont% = 0																								! Reset contador
          Exit Sub 
    	 EndIf
    EndIf
  EndIf

  If TS.IO.MOTORKEY = Gr.Abbot.Motor% And Gr.Abbot.Xcont% > 0  Then Begin   ! Calculo Beneficios 
  	 Call APLICA.DSCTO.ABBOT																								! Aplica beneficios Abbot
  	 If Gr.Abbot.CtlPago% = -3 And TS.IO.KEYS(1) <> 70 Then Begin           ! Autorización recibida
  	 	  If Gr.Abbot.Loc% = 0 Then Begin																			! Confirmado Sin beneficios
  	 	     Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													! Inicializa secuencias de 
  	 	     TS.IO.MOTORKEY = 73																							! Tecleo
  	 	     Gr.Abbot.TotBnf% = 0
  	 	  	 Gr.Abbot.CtlPago% = -5
  	 	  	 Exit Sub  
  	 	  EndIf																																!
  	 	  Call U.IMPRIME("*****  ITEMS BENEFICIO ABBOT *****",4101H)					!
  	 	  Call U.IMPRIME("*****  ITEMS BENEFICIO ABBOT *****",2100H)					!
  	 	  If Gr.Abbot.Benef$(1,5) <> "X" Then Begin
           Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													! Inicializa secuencia
           Gr.Abbot.Apl% = 1																								!
           Ue.mktp.Line% = 0 																								! Apaga motor promociones
           TS.IO.KEYS(2)  = 80																							!
           TS.IO.DATA$(2) = Gr.Abbot.Benef$(1,2)														! SKU a registrar
           TS.IO.DEVICE   = 1																								!
           TS.IO.MOTORKEY = 80																							!
           Gr.Abbot.Intrx% = -1										  												! Anula verificacion precio
        EndIf Else Begin
           Call Aplica.Benef.Nutrixti																							!
        EndIf 	
  	 EndIf Else Begin																												! Autorización no recibida
  	 	  Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														! Inicializa secuencias de 
  	 	  TS.IO.MOTORKEY = 73																									! Tecleo
  	 EndIf																																	!
  EndIf																																			! Fin calculo beneficios
  
  If TS.IO.KEYS(1) = 70 And TS.IO.MOTORKEY = 81 Then Begin                  ! Secuencia Anul - total
!   Call visores4690(1,"ctlpago "+str$(Gr.Abbot.CtlPago%),"Totbnf "+str$(Gr.Abbot.TotBnf%),2000,"l")
   If (Gr.Abbot.CtlPago% <> 0 And Gr.Abbot.TotBnf% <= 0) Then Begin		  		! Si beneficio y dscti no Aplicado Abbot
   	    TS.TEMP1I1 = 0
   	    Call ANULA.BENEFICIO.ABBOT																					! Anula beneficios abbot
   	    If TS.TEMP1I1 = 0 Then Begin																				!
   	    	 Call VISOR.AND.BORRAR("PROCESO ANULACION ABBOT FALLIDO")					!
  	 	     Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													! Inicializa secuencias de 
  	 	     TS.IO.MOTORKEY = 73																							! Tecleo
  	 	     Exit Sub 																												!
   	    EndIf																																!
  	 	  Gr.Abbot.TotBnf% = 0																								! Anula impresion voucher
        Call Gr.Init.Trx																										! Anula movimiento
        Exit Sub         	    
   EndIf
   If (Gr.Abbot.CtlPago% = -5 And Gr.Abbot.TotBnf% > 0) Then Begin		  		! Si beneficio Aplicado Abbot
   	    TS.TEMP2I4 = 0																											!
        Call Entrada.Autorizacion                         									! Captura validacion de llave
        TS.TEMP2I4 = -1																											!
        If TS.TEMP2I4 = 0 Then Begin                      									! Si proceso fallido
  	 	     Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													! Inicializa secuencias de 
  	 	     TS.IO.MOTORKEY = 73																							! Tecleo
  	 	     Exit Sub 																												!
        EndIf																																!
   	    TS.TEMP1I1 = 0
   	    Call ANULA.BENEFICIO.ABBOT																					! Anula beneficios abbot
   	    If TS.TEMP1I1 = 0 Then Begin																				!
   	    	 Call VISOR.AND.BORRAR("PROCESO ANULACION ABBOT FALLIDO")					!
  	 	     Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													! Inicializa secuencias de 
  	 	     TS.IO.MOTORKEY = 73																							! Tecleo
  	 	     Exit Sub 																												!
   	    EndIf																																!
  	 	  Gr.Abbot.TotBnf% = 0																								! Anula impresion voucher
        Call Gr.Init.Trx																										! Anula movimiento
        Exit Sub         	    
   EndIf																																		! Fin Anul beneficio abbot
  EndIf 
EndIf 																																			! Final Eamtsu14
  
!--- EAMTSU20.J86
If Xopt% = 60 Then Begin                                                    ! Secuencias de tecleo

 If TS.LINETYPE = 1 And TS.LINEDATA = 0 Then Begin                          !
  If (Gr.Abbot.CtlPago% = -4) Then begin  																	! No hay dscto aplicado   
     Call Aplica.Beneficio.Abbot
  	 Gr.Abbot.CtlPago% = -5
  	 If Gr.Abbot.Anul% = -1 Then Begin
  	 	  Gr.Abbot.Anul% = -1
  	 	  Gr.Abbot.CtlPago% = 0
  	 	  TO.MGRKEY = 0
  	 EndIf
  EndIf
 EndIf

EndIf																									    									! Final Eamtsu20

!-- EAMTSU43.J86
If Xopt% = 43 Then Begin                                                    ! Recupearacion de trx
 If (Gr.Abbot.CtlPago% <= -3) Then Begin   																 	! Reportando beneficios abbot
 	  If Gr.Abbot.Apl% >= Gr.Abbot.Loc% Then                                 \! Termina lista beneficios
 	  	 Gr.Abbot.CtlPago% = -4																								!
 	  	 
 	  If Gr.Abbot.Anul% = 0 Then \
 	     Gr.Abbot.TotBnf% = Gr.Abbot.TotBnf% + SL.IT.XPRICE	       				    ! Total beneficios

!    If Val(Gr.Abbot.Benef$(Gr.Abbot.Apl%,1)) = 0 And                       \! 
!           Gr.Abbot.Benef$(Gr.Abbot.Apl%,5)  = "X" Then                    \!
! 	  	 SL.IT.XPRICE = 0

 	  If Gr.Abbot.Anul% = 0 Then Begin 
 	     Xtmp$ = Pack$(Gr.Abbot.Benef$(Gr.Abbot.Apl%,0))  +                  \! Ean Beneficio
 	          ":" + Pack$(Str$(SL.IE.QTYORWGT))				 + 										 \! Qty Beneficio
 	          ":" + Pack$(Str$(SL.IT.XPRICE))          +                     \! Valor Beneficio Item
 	          ":" + Gr.Abbot.TknConf$                  +                     \! Token confirmacion
 	          ":" + Pack$("00")																								! Signo Operacion
 	  EndIf Else Begin 																												! Almacena anulacion movimiento
 	     Xtmp$ = Pack$(Gr.Abbot.Benef$(Gr.Abbot.Apl%,0))  +                  \! Ean Beneficio
 	          ":" + Pack$(Str$(SL.IE.QTYORWGT))				 + 										 \! Qty Beneficio
 	          ":" + Pack$(Str$(SL.IT.XPRICE))          +                     \! Valor Beneficio Item
 	          ":" + Gr.Abbot.TknAnul$                  +                     \! Token confirmacion
 	          ":" + Pack$("01")																								! Signo operacion 
 	  EndIf
    Call Grabacion.Cadena.Usuario2("20170404",Xtmp$)          							! Almacena user data              
 	  Gr.Abbot.Apl% = Gr.Abbot.Apl% + 1
 EndIf																																			! Fin reporte beneficios Abbot 

EndIf																																				! Final Eamtsu53

!-- EAMTSU53.J86
If Xopt% = 53 Then Begin                                                    ! Recupearacion de trx
 If Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "99" THEN BEGIN   	    	            ! Si DATA/ENTRY
  If Unpack$(MID$(SL.STR.ENTRY$,3,4)) = "20170404" Then Begin               ! Proceso Abot
     Call VISOR.AND.BORRAR("PROCESO ABBOT FALLIDO")
     Call VISOR.AND.BORRAR("ANULANDO MOVIMIENTO  ")
     Call Gr.Init.Trx																										    ! Anula movimiento
  EndIf
 EndIf 
EndIf

!-- EAMTSU56.J86
If Xopt% = 56 Then Begin                                                    ! Suspension de trx
  Gr.Abbot.CtlPago% = 0																											! Ctrl Aplicacion dscto
  Gr.Abbot.Clte% = 0 																												! Ctrl captura de cliente
  Gr.Abbot.Cotiza$ = ""                                                     ! Numero de cotizacion
  Gr.Abbot.Tm$  = ""                                                        ! Temporal
  Gr.Abbot.TknConf$ = ""
  Gr.Abbot.TotBnf% = 0
  Gr.Abbot.Xcont% = 0
  Gr.Abbot.Apl% = 0
  Gr.Abbot.Loc% = 0
  Gr.Abbot.TknCotiza$ = ""
  Gr.Abbot.Anul% = 0
  Gr.Abbot.TknConf2$ = ""
  Gr.Abbot.Ticket$ = ""
  Gr.Abbot.TknAnul$ = ""
  Gr.Abbot.Alimxti% = 0
EndIf 																																			! Final Eamtsu56


End Sub 
