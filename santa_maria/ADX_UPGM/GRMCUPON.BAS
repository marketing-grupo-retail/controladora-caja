!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMCUPON.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Cupones tienda            *
!**************************************************
! Modificaciones:
! ----- Version 1.0 10 Noviembre 2023 -------------
! Mod 11Jun2025
! Se ajusta proceso de emisiòn cupon para registro
! con formas de pago
! Desarrollado por Grupo Retail - OV
!--------------------------------------------------
! Mod 11Jun2025
! Se modifica impresion cupones para impresion datos 
! de cliente.
! &N -> Nombre del cliente
! &I -> Identificacion del cliente
! &T -> Telefono del cliente
! Desarrollado por Grupo Retail - OV
!--------------------------------------------------

%Environ T																																	! Ambiente terminal
Integer   Global SL.END																											! Total String trx 
Integer*1 Global Gr.Cpns.Ok%,                                              \! Proyecto Activo
                 Asic.Detalle%,                                            \!
                 Gr.Cpns.Proc%                                              ! proceso cupones
Integer*2 Global Gr.Cpns.SlEnd%         
Integer*4 Global Asc.Tmp.Apun%                   														! Apuntadores
String    Global IR.USERDATA$																								! Extension maestra productos

%INCLUDE EAMTSWKG.J86          																							! working storage
%INCLUDE EAMTRANS.J86          																							! Variables de transacciones
%INCLUDE EAMTERMS.J86          																							! Variables de terminales
%INCLUDE EAMTOPTS.J86																												! Terminal options
%INCLUDE EAMITEMR.J86          																							! Variables EAMITEMR
%INCLUDE EAMTSXHC.J86          																							! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          																							! Rutinas de Assembler

%INCLUDE RECATSSU.011          					      															! RUTINAS GENERICAS APLICACION     

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                    *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                       *!
  STRING    DATA$      !* String to hold the data read from file.          *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                             *!
End Sub 

Sub IRRFEC.SPLIT1 (RECORD$) EXTERNAL
  STRING    RECORD$    !* The record (including the key) read from file    *!
End Sub 

Sub TSPREC01 EXTERNAL          																							! print routine
End Sub

Sub Alimenta.Logos External																									! Carga logo memoria
End sub 	

!------------------------------------------------------------------------------
! Retorna datos de cliente
! k = 1 codigo de cliente
! k = 2 nombre del cliente
! k = 3 direccion del cliente
! k = 4 telefono del cliente
! k = 5 idCliente
! k = 6 tipo del cliente o "7" si no se ha identificado
! k = 7 cadena de cliente o "" si no se ha identificado
! k = 8 idCliente o "9999999999999" si no se ha identificado
! k = 9 nombre del cliente o "CONSUMIDOR FINAL" si no se ha identificado
!------------------------------------------------------------------------------
Function datosCliente$(k) External 
   string datosCliente$
   integer*1 k
End Function 

Sub GRIMPRIME.CUPONES(Xmsg$,Xn%)
String Xmsg$, Xkey$, Xdata1$, Xdata2$, Xdata3$
Integer*4 Xn%, XJ%, XK%, XI%
String n$, i$, t$ 

!-- Extrae datos cliente para impresión
 I$ = datosCliente$(8)                																			! ID cliente
 n$ = datosCliente$(9)																											! Nombre del cliente
 t$ = datosCliente$(4)																											! telefono del cliente
 If I$ = "9999999999999" Then Begin																					! Si consumidor final
 	  I$ = "___________________________"
 	  n$ = "___________________________"
 	  t$ = "___________________________"
 EndIf 
 Xkey$ = Pack$(Right$("0000000"+Xmsg$,7) + "000")														! Registro cabecera 
 TS.ER.RETURN = -1                                 													! Ctrl de Error
 Read Form "C5 C38";#94 KEY Xkey$; Xdata1$, Xdata2$	    										! Lee archivo
 If TS.ER.RETURN = -1 Then Begin 																						! Encontrado
 	  Call U.Imprime("MENSAJE  :"+Xmsg$,2100H)
	  Call U.Imprime("NRO.VECES:"+Str$(Xn%),2100H)
 	  Xdata2$ = Left$(Xdata2$,3)                                              ! Numero de lineas del mensaje
 	  Call U.Imprime("NRO.LINEAS:"+Str$(Xn%),2100H)
 	  For XJ% = 1 To Xn%																											! total mensajes a imprimir
 	   For XK% = 1 To Val(Xdata2$)																						! Lineas a imprimir
     	  Xkey$  = Pack$(Right$("0000000"+Xmsg$,7) + Right$("000"+Str$(Xk%),3) )
     	  TS.ER.RETURN = -1
        Read Form "C5 C38";#94 KEY Xkey$; Xdata1$, Xdata3$									! Lee registro
        If TS.ER.RETURN = -1 Then Begin
        	
        	If Match("&I",Ucase$(Xdata3$),1) > 0 Then Begin										! Si parametro para id cliente
        		 Xdata3$ = "Cliente :"+I$ 																				!
          EndIf																															!
        	If Match("&N",Ucase$(Xdata3$),1) > 0 Then Begin										! Si parametro para nombre cliente
        		 Xdata3$ = "Nombre  :"+N$ 																				!
          EndIf																															!
        	If Match("&T",Ucase$(Xdata3$),1) > 0 Then Begin										! Si parametro para nombre cliente
        		 Xdata3$ = "Telefono:"+T$ 																				!
          EndIf																															!
        	
          TS.TEMP1$ = Left$(Xdata3$ + STRING$(38," "),38)             			! Ajuste impresion
          Write #34 ; TS.TEMP1$ + (CHR$(10))
        EndIf          
 	   Next XK%																																!
   	 Asic.Detalle% = 1																											!
     TS.TEMP1$ = Linea.Detalle(1)																						!
     TS.TEMP1$ = Left$(TS.TEMP1$ + STRING$(38," "),38)          			      ! Ajuste impresion
     Write #34 ; TS.TEMP1$ + (CHR$(10))     
     TS.TEMP1$ = String$(19,"- ")
     TS.TEMP1$ = Left$(TS.TEMP1$ + STRING$(38," "),38)          			      ! Ajuste impresion
     Write #34 ; TS.TEMP1$ + (CHR$(10))     
     For XI% = 1 To 9
       Write #34 ; " " + (CHR$(10))     
     Next XI%
     Call U.CORTACR       																									!
 	  Next XJ%																																!
 EndIf Else Begin
 	  Call U.Imprime("ERR MENSAJE :"+Xmsg$+" "+UNPACK$(XKEY$),2100H)
	  Call U.Imprime("NRO.VECES:"+Str$(Xn%),2100H)
	  Call U.IMPRIME("ERROR "+ERR+" SES:"+STR$(errf%),2100H)
 EndIf																																			!
 TS.LINETYPE = 18																														! Genera cabecera 
 TS.LINEDATA = 1																														! en proximo tiquete
 Call TSPREC01																															!

End Sub 

Sub GRCUPONES.TIENDA																												! Validacion cupones tienda
  Integer*4 PRIC%, TOT4%, XI%, XInd1%, XInd2%, X.J%, Xqty%, Xfind%,        \!
            Gr.Xcont%, Xpric%, Xl%, X, R.S												          !
  Integer*2 Xsgn%, Xpos%																										!
  Integer*4 REC.NO, BLK.NUM, REC.LEN, KEY.LEN, MAX.R.SEC, Grcpn%,  				 \!
            Gpagos%(2)   																										!
  String    XTMP$, Xitm$, XPric$, Xdummy$, Xkey$, Xbasura$, X.Ean$, Xcot$   !
  String    Gr.Tmp.Itm$(2), Xbuff$, Xiva$, Xund$, X.Flag$, Gr.Impoc$,      \!
            XDepto$, XPvp$, Xicui$, H$, PZERO$, GrLista$(2),               \!
            Xdat1$, Xdat2$, REG$, KEY$, Xtv$, Xvlr$, Gpago$(1)              !
  Gr.Xcont% = 0 																														! Numero de Items
  Dim Gr.Tmp.Itm$(1000,3)	  			 																			    ! Lista de Items vendidos
  Dim GrLista$(700,8)																												! Lista cupones
  Dim Gpagos%(66,1)																													! Formas de pago 
  X.J% = 0 : Grcpn% = 0																											! 
  FOR X.J% = 1 TO Gr.Cpns.SlEnd%                    												! FOR ALL StringS
    H$ = READ.SL.STR$(X.J%)                 																! GET String
    If LEN(H$) > 5 Then  Begin                                              ! ASSURE GOOD String
     If ASC(H$)  = 1 Then Begin             																! ITEM ENTRY String
     	  Asc.Tmp.Apun% = 3																										!
        XItm$   = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Item Vendido
        Xpric$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Precio Venta
        Xdepto$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Departamento
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Family 
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Indicat 1
        XInd1%  = Val(Xdummy$)																							! Vlr numerico Indicat 1
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Indicat 2
        XInd2%  = Val(Xdummy$)																							! Vlr numerico Indicat 2
        Xsgn% = 1 																													! Trx de compra
        Xqty% = 0                                                           ! Sin cantidad o peso
        If (Xind2% And 0080H) Then Xsgn% = -1                               ! Item anulado
        If (Xind2% And 0040H) Then Xsgn% = -1                               ! Item anulado

!        Xkey$ = Pack$( Right$("000000000000"+Xitm$,12) )                   ! Llave de busqueda
!        Call IRRFEC.READ01(Xkey$, 4, TS.TEMP1$, 0)                					! Lectura Datos Itemr
!        Call IRRFEC.SPLIT1( TS.TEMP1$ ) 	                          				! Entrega datos al eamitemr.j86
!        IR.DEPARTME$ = UnPack$(IR.DEPARTME$)																! Dpto venta 
!        Xdepto$ = Right$("000000000000"+Xdepto$,12)                         ! Toma la seccion comercial

        Xfind% = 0																													! Encontrado
     	  Xpos% = 0	: Xl% = 0																									! Ctrl busqueta Item 
     	  For Xl% = 1 To Gr.Xcont%																						! Busca Item en compras
     	   If Gr.Tmp.Itm$(Xl%,0) = Xitm$ Then                                \! Ya reportada la venta
     	   	  Xpos% = Xl% : Xl% = Gr.Xcont% + 1																! Reporta encontrado
     	  Next Xl%																														!
        If Xpos% = 0 Then Begin                                             ! Nuevo Item
           Gr.Xcont% = Gr.Xcont% + 1												                ! Posicion Almacenar
           Gr.Tmp.Itm$(Gr.Xcont%,0) = Xitm$                                 ! SKU producto
           Gr.Tmp.Itm$(Gr.Xcont%,1) = Str$( Val(Xpric$) * Xsgn%)            ! Precio de Venta
           Gr.Tmp.Itm$(Gr.Xcont%,2) = Xdepto$																! Seccion de venta
        EndIf Else Begin																									  ! Item Encontrado
          Gr.Tmp.Itm$(Xpos%,1) = Str$(Val(Gr.Tmp.Itm$(Xpos%,1)) +          \!
                                 (Val(Xpric$) * Xsgn%))                     ! Monto vendido
        EndIf
     EndIf                                  																! ITEM ENTRY String
     If ASC(H$)  = 5 Then Begin             																! Forma de pago
     	  Asc.Tmp.Apun% = 3																										!
        Xtv$   = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												  ! Forma de pago
        Xvlr$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												  ! Valor pago
     	  Gpagos%(Val(Xtv$),1) = Gpagos%(Val(Xtv$),1) + Val(Xvlr$)            ! Acumula valor pagado
     EndIf                                                                  ! Forma de pago
     If ASC(H$)  = 6 Then Begin             																! Anul Forma de pago
     	  Asc.Tmp.Apun% = 3																										!
        Xtv$   = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												  ! Forma de pago
        Xvlr$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												  ! Valor pago
     	  Gpagos%(Val(Xtv$),1) = Gpagos%(Val(Xtv$),1) - Val(Xvlr$)            ! Anula el valor pagado
     EndIf                                                                  ! Anul Forma de pago
    EndIf
  NEXT X.J%   
  X.J% = 0 : XI%  = 0

  TS.ER.RETURN = -1
  Open "R::TF:PAGCPN" As 94 NoWrite NoDel  																	! Lista pagos/cupones
  If TS.ER.RETURN <> -1 Then Begin																					! Si Error apertura
 	  Call VISOR.AND.BORRAR("ERROR APERTURA PAGOS CUPONES")									  ! MSg alerta
 	  Call U.IMPRIME("ERROR OPEN PAGOS CUPONES "+TS.TS11WERR$,2100H)          !
    Exit Sub 	 
  EndIf Else Begin																													! Apertura OK
  If End #94 Then UE.FIN.CPNPAGO        																	  ! Si es EOF
  While (1)															  																  ! Recorre archivo parametros
    Read #94; TS.TEMP1$, TS.TEMP2$ 																			    ! Lectura registro
    If Val(TS.TEMP1$) <= 66 Then Begin                                      ! forma de pago valida
    	 Gpagos%(Val(Xtv$),0) = Val(TS.TEMP2$)                                ! Numero de mensaje a generar
    EndIf 																																	!
  Wend 																																			! Fin carga
  UE.FIN.CPNPAGO:            	
    Close 94
  EndIf

  TS.ER.RETURN = -1
  Open "R::TF:ITMCPN" KEYED RECL 9  AS 94               										! Lista items / seccion compra
  If TS.ER.RETURN <> -1 Then Begin
 	  Call VISOR.AND.BORRAR("ERROR APERTURA ITEMS CUPONES")									  ! MSg alerta
 	  Call U.IMPRIME("ERROR OPEN ITEMS CUPONES "+TS.TS11WERR$,2100H)          !
    Exit Sub 	 
  EndIf
  
  TS.ER.RETURN = -1
  Open "R::TF:PCUPON" DIRECT RECL 512 AS 38 buffsize 32640  	     					! Apertura CUPONES
  If TS.ER.RETURN <> -1 Then Begin
 	  Call VISOR.AND.BORRAR("ERROR APERTURA PARAM CUPONES")									  ! MSg alerta
 	  Call U.IMPRIME("ERROR OPEN PARAM CUPONES "+TS.TS11WERR$,2100H)          !
    Close 94
    Exit Sub 	 
  EndIf

  REC.NO = 1					             																					! Initialize Counter
  I% = 1						             																						! Initialize Counter
  Read Form "T43 I4 I2 T55 I2 C456"; #38,REC.NO;                           \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		             													! 
  PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                     								! Armed Key Control
  MAX.R.SEC = 508/REC.LEN                                    								! Length record

For REC.NO = 2 TO BLK.NUM                                    								! Cicle to read all blocks  
  REG$ = ""
  Read Form "T5 C508"; #38, REC.NO;  H$                      								! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = Mid$(H$,X,KEY.LEN)                								! Extract First key
  While KEY$  NE  PZERO$                                     								! Inside sector loop 
    R.S = R.S + 1                                            								! Records On This Sector 
    Xdat1$ = Unpack$(MID$(H$,X+ 3, 4))																			! Fecha inicial
    Xdat2$ = Unpack$(MID$(H$,X+ 7, 4))																			! Fecha inicial
    Xdat1$ = Right$(Xdat1$,6) : Xdat2$ = Right$(Xdat2$,6)										! Ajusta fechas AAMMDD
    
    Call U.IMPRIME("FECHA 1 :"+XDAT1$,2100H)
    Call U.IMPRIME("FECHA 2 :"+XDAT2$,2100H)
    Call U.IMPRIME("SISTEMA :"+DATE$,2100H)
    
    If Val(date$) >= Val(Xdat1$) Then Begin     														! Es una fecha mayor o igual a la inicial
     If Val(date$) > Val(Xdat2$) Then Begin    															! Es una fecha mayor a la fecha final 
   	    GoTo Next.Cpn.Reg																										! Siguiente cupon
     EndIf																																	!
    EndIf Else Begin																												! Fecha menor a inicio evento
    	  GoTo Next.Cpn.Reg																										! Siguiente cupon
    EndIf
    Grcpn% = Grcpn% + 1																											! Total cupones
    GrLista$(Grcpn%,0) = Unpack$(MID$(H$,X+ 0, 3))													! Numero cupon
    GrLista$(Grcpn%,1) = Unpack$(MID$(H$,X+ 3, 4))													! Fecha inicial
    GrLista$(Grcpn%,2) = Unpack$(MID$(H$,X+ 7, 4))													! Fecha final
    GrLista$(Grcpn%,3) = (MID$(H$,X+11, 1))							        						! Tipo Validacion
    GrLista$(Grcpn%,4) = Unpack$(MID$(H$,X+12, 2))													! Nro bonos 
    GrLista$(Grcpn%,5) = Unpack$(MID$(H$,X+14, 5))													! Valor Min compra
    GrLista$(Grcpn%,6) = Unpack$(MID$(H$,X+19, 3))													! Nro Mensaje
    GrLista$(Grcpn%,7) = "0" 																								! Para total ventas 
    GrLista$(Grcpn%,8) = Unpack$(MID$(H$,X+22, 1))													! Maximo numero tkts
    Next.Cpn.Reg: 
    X = X + REC.LEN                                          								! Index to next key
    KEY$ = Mid$(H$,X,KEY.LEN)                                								! Pick up next key
    If R.S = MAX.R.SEC Then KEY$ = PZERO$                    								! If EOF() record or file
  Wend
Next REC.NO																																	! Fin barrido cupones
Close 38
XI% = 0 : XL% = 0																														! Inicia contadores
For XI% = 1 To Grcpn%																												! Total Cupones
 For XL% = 1 To Gr.Xcont%																										! Total Items 
     KEY$ = Pack$(Right$("000000"+GrLista$(XI%,0),6) +                     \! Numero del cupon
            Right$("000000000000"+Gr.Tmp.Itm$(XL%,0),12))										! Item vendido
     TS.ER.RETURN = -1                                 											! Ctrl de Error
     Read Form "C9";#94 KEY KEY$; KEY$ 									    								! Lee archivo      
     If TS.ER.RETURN = -1 Then Begin 																				! Encontrado
     	  GrLista$(XI%,7) = Str$( Val(GrLista$(XI%,7)) +                     \! Total ventas 
     	                          Val(Gr.Tmp.Itm$(XL%,1)) )                   ! por cupones 
     EndIf
 Next XL%																																		!
Next XI%																																		!
Close 94

TS.ER.RETURN = -1 																										 	  	! Control errores
Open "R::TF:MSGPRM" KEYED RECL 43 AS 94 nowrite nodel 		                	! Mensajes Promocionales
If TS.ER.RETURN <> -1 Then Begin                                         		!
  Call VISOR.AND.BORRAR("ERROR APERTURA CUPONES PROMO")											! MSg alerta
  Call U.IMPRIME("ERROR APERTURA CUPONES  "+TS.TS11WERR$,6100H)         		!
  Exit Sub 																																	! Sale del proceso
EndIf  																																			!

XI% = 0																																			! Init contador
For XI% = 1 To Grcpn%																												! Total Cupones imprimir
 If Val(GrLista$(XI%,7)) >= Val(GrLista$(XI%,5)) Then Begin									! Si compra cumple 
 	  If Val(GrLista$(XI%,5)) > 0 Then                                       \! Si parametro mayor a cero
 	     XL% = Val(GrLista$(XI%,7)) / Val(GrLista$(XI%,5)) Else XL% = 0       ! Calcula numero de veces
    If Xl% > 0 Then Begin                                                   ! Imprime mensaje
     If Xl% > Val(GrLista$(XI%,8)) Then \
        Xl% = Val(GrLista$(XI%,8))																					! Maximo de tiquetes a genear
    	 Call GRIMPRIME.CUPONES(GrLista$(XI%,0),XL%)													!
    EndIf
 EndIf																																			!
Next XI%																																		! fin impresion cupones

XI% = 0 : XL% = 0																														! Init variables
For XI% = 11 To 66																												  ! Total formas de pago
 If Gpagos%(XI%,0) > 0 Then Begin																						! Si tiene mensaje definido
    XL% = Gpagos%(XI%,0)																										! Codigo del mensaje
    For X.J% = 1 To Grcpn%																									! Revisa cupones definidos
     If Xl% = Val(GrLista$(X.J%,0)) Then Begin									            ! Si encuentra mensaje
     	If Gpagos%(XI%,1) >= Val(GrLista$(X.J%,5)) Then Begin	 								! Si pago cumple vlr minimo
        If Val(GrLista$(X.J%,5)) > 0 Then                                  \! Si parametro mayor a cero
 	        XL% = Gpagos%(XI%,1) / Val(GrLista$(X.J%,5)) Else XL% = 0         ! Calcula numero de veces
        If Xl% > 0 Then Begin                                               ! Imprime mensaje
         If Xl% > Val(GrLista$(X.J%,8)) Then                               \! Si supera qty Max cupones a emitir
            Xl% = Val(GrLista$(X.J%,8))																			! Maximo de tiquetes a genear
    	      Call GRIMPRIME.CUPONES(GrLista$(X.J%,0),XL%)										!
         EndIf
     	EndIf																																	! 
     EndIf																																	! Fin mensaje encontrado
    Next X.J%
 EndIf
Next XI%																																		!

Close 94
End Sub 																																		! fin cupones tienda


Sub GRCUPONES(Xopt%) Public 																							  ! Modulo de cupones
Integer*4 Xopt%            																								  ! UE ejecutar
String Xtmp$ 																																!

!--- EAMTSU07.J86																														!
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.Cpns.Ok%  = 0                                                          ! Proyecto Apagado
  TS.ER.RETURN = -1																												  ! Control errores
  Open "R::$ARGENER" As 94 UNLOCKED NOWRITE NODEL 												  ! Apertura archivo parametrizacion para TCxSky
  !Open "R::ASCNTRL" As 94 UNLOCKED NOWRITE NODEL 												  ! Apertura archivo parametrizacion para TCxSky
  If TS.ER.RETURN <> -1 Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR APERTURA CUPONES      ")									! MSg alerta
  	 Call U.IMPRIME("ERROR APERTURA CUPONES       ",6100H)                  !
  	 Exit Sub 																															! Sale del proceso
  EndIf  																																		!
  If End #94 Then UE.FIN.CUPONES         																	  ! Si es EOF
  While (1)															  																  ! Recorre archivo parametros
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[CUPONES TIENDA]" Then Begin	       				        ! Cupones Tienda
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Cpns.Ok% = Val(Mid$(TS.TEMP1$,30,2)) 		    					          ! Proyecto Activo 0. No -1 Si
        EndIf																																! Fin caga
  Wend 																																			! Fin carga
  UE.FIN.CUPONES:                                                           !
     Close 94																																! Cierra archivo
     !Call Alimenta.Logos																										! Carga logo publicidad
   If Gr.Cpns.Ok% = -1 Then                                                \! Proyecto Activo
      Call U.IMPRIME("MODULO CUPONES STORE ON  14-Feb-2025",6100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO CUPONES STORE OFF 14-Feb-2025",2100H)          ! Msg Proyecto Cargado
EndIf 																																			! Fin carga de parametros 

If Gr.Cpns.Ok% <> -1  Then Exit Sub                                         ! Si proyecto apagado
If TS.STANDALONE       Then Exit Sub	                                      ! Si en operacion TOF

!--- EAMTSU02.J86																														!
If Xopt% = 2 Then Begin                                                     ! Inicializacion de transaccion
 If Gr.Cpns.Proc% = -1 Then                                                \!
    Call GRCUPONES.TIENDA																										! Genera cupones tienda
    Gr.Cpns.Proc% = 0
    Gr.Cpns.SlEnd% = 0
EndIf 																																			! Fin Eamtsu02

!--- EAMTSU20.J86																														!
If Xopt% = 20 Then Begin                                                    ! Impresion tiquete


 If TS.INTRX Then                                                          \! Si esta en una compra   
   If TS.LINETYPE = 6 And                                                  \! Store Data y Transacc   
      TS.LINEDATA = 1 Then Begin                                            ! Fecha, Hora, etc        
       If TS.TENDERED (0) <> 0 Or                                          \! Si hay pagos            
          TS.TRX.STATUS <> 100 Then Begin                                   ! y NO hay VOID en curso  
          If Gr.Cpns.Proc% = 0 Then \
             Gr.Cpns.SlEnd% = SL.END
          Gr.Cpns.Proc% = -1
       EndIf
   EndIf


EndIf 																																			! Fin Eamtsu02

End Sub 																																		! Fin proyecto cupones
