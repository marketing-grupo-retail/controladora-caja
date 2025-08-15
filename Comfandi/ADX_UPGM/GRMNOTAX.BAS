!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMNOTAX.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Abril del 2016                   *
!Observaciones : Modulo dia sin IVA               *
!**************************************************
! Modificaciones:
! ----- Version 1.0 10 Junio 2020 -----------------
! Mod 29/Jun/2020
! Se ajusta para procesos de validacion en linea
! compra clientes.
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%ENVIRON T																																	! Ambiente terminal

Integer*1 Global Gr.NoTax.Ok%,                                             \! Control proyecto
                 Gr.NoTax.Aplicado%,                                       \! Si beneficio aplicado
                 Gr.Fis.Doc%,                                              \! Generacion factura fiscal
                 Gr.NoTax.Proc%																							! proceso appl items
Integer*1 Global Asc.Pay.Impr%                                              !  
Integer*4 Global Gr.Notax.Cont%,																				   \!
                 Gr.Motor2.Xpos%,                                          \! Total items con promocion
                 Gr.NoTax.Itm%,                                            \!
                 Gr.NTNEGAMT(1)                                             ! 
Integer*4 Global UE.TABLAIVA(1)                                             ! Tarifas de IVA
Integer*4 Global Gr.Crd.Remision%
Integer*2 Global ASC.DSC.TARIFA%, Asc.DSC.INDICAT1
String    Global IR.USERDATA$ 																						  ! Extension maestra productos
String    Global Gr.NoTax.LstPago$,																				 \! Formas pago autorizadas
                 Gr.NoTax.Descriptor$,                                     \! Descriptor tiquete
                 Gr.NoTax.Struc$(2),                                       \! Lista Estructuras vendidas
                 Gr.NoTax.LstItm$(2),                                      \!
                 Gr.Motor2.Items(2),                                       \! Dsctos motor promociones
                 Gr.NoTax.Dpto$,                                           \! Dpto exento iva
                 Gr.NoTax.Vmax$,                                           \!
                 Gr.NoTax.Qmax$,                                           \!
                 Gr.NoTax.Dmax$,                                           \!
                 Gr.NoTax.Fecha$                                            ! Fecha aplicacion evento
Integer*2 Global GR.NTNEGCNT(1)                                             !

%INCLUDE LCLTSUVA.011	         																							! Variables desarrollo
%INCLUDE EAMTSWKG.J86          																							! working storage
%INCLUDE EAMTRANS.J86          																							! Variables de transacciones
%INCLUDE EAMTSEVA.J86          																							! Variables EMS
%INCLUDE EAMTERMS.J86          																							! Variables de terminales
%INCLUDE EAMTOPTS.J86	         																							! Variables Terminal Options
%INCLUDE EAMITEMR.J86          																							! Variables EAMITEMR
%INCLUDE EXIRTSVA.J86          																							! Extension del EAMITEMR
%INCLUDE EAMTSXHC.J86          																							! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          																							! Rutinas de Assembler

%INCLUDE RECATSSU.011          					      															! RUTINAS GENERICAS APLICACION     

Sub TSHIECET EXTERNAL
End Sub

Sub TSBDEC01 EXTERNAL                                                       ! take a balance due
End Sub

Sub TSPREC01 EXTERNAL          																							! print routine
End Sub

Sub  TSCSEC03 External
End Sub 

Function FORMAT.AMOUNT(AMT1) EXTERNAL
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

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

Sub IRRFEC.SPLIT1 (RECORD$) EXTERNAL
  STRING    RECORD$    !* The record (including the key) read from file      *!
End Sub 

Sub Asignacion.Consecutivo.Remision External					 !
End Sub 																							 !

Sub Flag.Item.Iva(Xindi1%)	External																				! Calculo flag de iva
Integer*1 Xindi1%           																								!
End Sub 

Sub NOTAX.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:NT" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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

Sub Graba.Itm.FE.NoTax
  Integer*4 PRIC%, TOT4%, XI%, XInd1%, XInd2%, X.J%, Xqty%, Xfind%,        \!
            Gr.Xcont%, Xpric%																			          !
  Integer*2 Xsgn%, Xpos%																										!
  String    XTMP$, Xitm$, XPric$, Xdummy$, Xkey$, Xbasura$, X.Ean$, Xcot$   !
  String    Gr.Tmp.Itm$(2), Xbuff$, Xiva$, Xund$, X.Flag$, Gr.Impoc$,      \!
            XDepto$, XPvp$
  Gr.Xcont% = 0 																														! Numero de Items
  Dim Gr.Tmp.Itm$(1000,9)					 																			    ! Lista de Items vendidos
  X.J% = 0 
  FOR X.J% = 1 TO SL.END                    																! FOR ALL StringS
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
        Xkey$ = Pack$( Right$("000000000000"+Xitm$,12) )                    ! Llave de busqueda
        Call IRRFEC.READ01(Xkey$, 4, TS.TEMP1$, 0)                					! Lectura Datos Itemr
        Call IRRFEC.SPLIT1( TS.TEMP1$ ) 	                          				! Entrega datos al eamitemr.j86
        Call Flag.Item.Iva(XInd1%)																		  		! Retorna flag iva
        If TS.TEMP1I1 = 7 Then X.Flag$ = "1" Else X.Flag$ = "0"        			! Si excluido / excento
        Xiva$ = Str$(UE.TABLAIVA(TS.TEMP1I1))                               ! Tasa de IVA
        Gr.Impoc$ = "00"
        If TS.TEMP1I1 = 4 Then Begin																				! Impuesto impoconsumo
        	 Gr.Impoc$ = Str$(UE.TABLAIVA(TS.TEMP1I1))
        	 Xiva$ = "0" 
        	 X.Flag$ = "0"
        EndIf
        If (IR.INDICAT0 And 40H) Then Xund$ = "KLG" Else Xund$ = "UD"       ! Si pesable o unitario
        Xfind% = 0																													! Encontrado
     	  Xpos% = 0																														! Ctrl busqueta Item 

        If Xpos% = 0 Then Begin                                             ! Nuevo Item
           Gr.Xcont% = Gr.Xcont% + 1												                ! Posicion Almacenar
           Gr.Tmp.Itm$(Gr.Xcont%,0) = Xitm$                                 ! SKU producto
           Gr.Tmp.Itm$(Gr.Xcont%,1) = Str$( Val(Xpric$) * Xsgn%)            ! Precio de Venta
           Gr.Tmp.Itm$(Gr.Xcont%,2) = Str$( Xsgn% * 1 )                     ! Cantidad Vendida
           Gr.Tmp.Itm$(Gr.Xcont%,3) = Xiva$                                 ! Tarifa del IVA
           Gr.Tmp.Itm$(Gr.Xcont%,4) = X.flag$                               ! Excento o Excluido
           Gr.Tmp.Itm$(Gr.Xcont%,5) = Gr.Impoc$                             ! Impoconsumo
           Gr.Tmp.Itm$(Gr.Xcont%,6) = Xund$                                 ! Si es pesable o unitario
           Gr.Tmp.Itm$(Gr.Xcont%,7) = IR.ITEMNAME$													! Descripcion Item
           Gr.Tmp.Itm$(Gr.Xcont%,8) = Xdepto$																! Seccion de venta
           Gr.Tmp.Itm$(Gr.Xcont%,9) = "0"      														  ! Total Descuento
           Xpos% = Gr.Xcont%                                                ! Asigna posicion de vector
        EndIf Else Begin																									  ! Item Encontrado
          Gr.Tmp.Itm$(Xpos%,2) = Str$(Val(Gr.Tmp.Itm$(Xpos%,2)) +          \!
        	                                 (Xsgn% * 1))                     ! Cantidad Vendida
        EndIf
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
        Gr.Tmp.Itm$(Xpos%,2) = Str$(Xqty% + Val(Gr.Tmp.Itm$(Xpos%,2)) - (Xsgn% * 1))! Cantidad Vendida
       EndIf 																																!
     EndIf																																	! ITEM ENTRY String Extention
    EndIf
  NEXT X.J%   
  X.J% = 0
  XI%  = 0
  For X.J% = 1 To Gr.Motor2.Xpos%																						! Total promociones aplicadas
   For XI% = 1 To Gr.Xcont%
      If Val(Gr.Motor2.Items(X.J%,0)) = Val(Gr.Tmp.Itm$(XI%,0)) Then Begin  ! Plu encontrado
      	 Gr.Tmp.Itm$(XI%,9) = Gr.Motor2.Items(X.J%,1)												! Total dscto aplicado
      EndIf
   Next XI%
  Next X.J%
  X.J% = 0
  For X.J% = 1 To Gr.Xcont%
      XTMP$ = "00"																													! Signo operacion
      XPric$ = Gr.Tmp.Itm$(X.J%,1)																					! Toma venta total
      Xund$  = Gr.Tmp.Itm$(X.J%,2)																					! Qty total venta
      If Val(Xpric$) < 0 Then Begin																					! Si venta negativa
      	 Xpric$ = Str$( Val(Xpric$) * -1)																		! Invierte signo
      	 Xund$  = Str$( Val(Xund$) * -1 )																		! Invierte signo
      	 Xtmp$ = "01"																												! Reporta Item negativo
      EndIf																																	!
      If Gr.Tmp.Itm$(X.J%,6) = "UD" Then Begin															! Si venta por unidad
      	 Xpric% = ( Val(Xpric$) / Val(Xund$) )														  ! Precio unitario
      	 XPvp$ = Str$ (Xpric%)
      EndIf Else Begin																											! Si venta pesable
      	 XPric% = (Val(Xpric$)*1000) / Val(Xund$) 										      ! Precio unitario
      	 XPvp$ = Str$( XPric% ) 																						! Precio unitario
      EndIf
      Xbuff$ = Pack$(Gr.Tmp.Itm$(X.J%,0))                                + \! SKU producto         
               ":" + Pack$(Xpvp$)                                        + \! Precio Unitario
               ":" + Pack$(Xund$)                                        + \! Qty Vendida          
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,3))                          + \! Tasa Impuesto
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,4))                          + \! Excento o Excluido
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,5))                          + \! Impoconsumo
               ":" + Gr.Tmp.Itm$(X.J%,6)                                 + \! Unitario o pesado    
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,8))                          + \! Seccion de la venta
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,9))                          + \! Descuentos aplicados
               ":" + Pack$(Xtmp$)									                        	! Signo de la operacion
        Call Grabacion.Cadena.Usuario2("42",Xbuff$)                         ! UD Items para FE     
  Next X.J%
End Sub 

Function Bono.Tarifa.Iva(UE.INDI1%) External
Integer*2 UE.INDI1%, Bono.Tarifa.Iva
End Function 

Sub GR.VALIDA.FECHANOTAX																										! Valida fecha aplicacion evento
String Xkey$, Xdate$, Xd1$, Xd2$
  TS.ER.RETURN = -1																													! Control de errores
  Open "R::$F:DNOTAX" KEYED RECL 22 AS 94 nowrite nodel 	  								! Control de fecha
  If TS.ER.RETURN <> -1 Then Begin
     Gr.NoTax.Fecha$ = "20200101"															  			      ! Asigna fecha vencida
     Exit Sub 																															! Sale del proceso
  EndIf Else Begin
     Xkey$ = Pack$("1999999999999999")                                      ! Registro control
     TS.ER.RETURN = -1		  																								! Control de errores
  	 Read Form "C8 C7 C4 C3";#94 Key Xkey$; Xkey$, Xd1$, Xdate$, Xd2$  	    ! Busca fecha aplicacion
     If TS.ER.RETURN = -1 Then Begin																				! Registro encontrado
        Gr.NoTax.Fecha$ = UnPack$(Xdate$)																		! Asigna fecha 
     EndIf Else Gr.NoTax.Fecha$ = "20200101"
  EndIf
  Close 94																																	! Cierre archivo
End Sub 

Sub VALIDA.ESTRUCTURA.NOTAX(Xstr$)  																				! Validacion estructura Sin tax
String Xstr$, Xkey$, Xd1$, Xdate$, Xd2$, Xvlr$
  Xkey$ = Pack$(Xstr$)                                                      ! Busca estructura
  TS.ER.RETURN = -1		  																								    ! Control de errores
  Read Form "C8 C5 C2 C4 C3";#94 Key Xkey$;                                \! Busca estructura
   Xkey$, Xvlr$, Xd1$, Xdate$, Xd2$                                         ! comercial
  If TS.ER.RETURN <> -1 Then Begin                                          ! No encontrada
  	 TS.TEMP2I1 = 0																							    		    ! Control proceso
  EndIf Else Begin                                                          ! Encontrada
    Gr.NoTax.Vmax$ = UnPack$(Xvlr$)                                         ! Monto Maximo
    Gr.NoTax.Qmax$ = UnPack$(Xd1$)                                          ! Qty Maxima
    Gr.NoTax.Dmax$ = Xd2$                                                   ! Dpto Venta
    TS.TEMP2I1 = -1																							    		    ! Control proceso
  EndIf																																			!
End Sub 																																		! fin validacion

Sub VALIDA.STRUCT.CLIENT(Xpos%) 																						! Valida Item si cumple compra
Integer*4 Xpos%, Xsaldo%, Xcompra%, Xmax%																	  ! 
String Xkey$, Xqty$																													!
  Xcompra% = Val(Gr.NoTax.Struc$(Xpos%,1))                                  ! Cantidad de compra en trx 
  Xkey$ = Pack$(Right$("000000000000000000"+Gr.Lcl.Clte$,18) +             \! Arma llave consulta
          Gr.NoTax.Struc$(Xpos%,3))                                         ! cliente + structura
  TS.ER.RETURN = -1		  																								    ! Control de errores
  Read Form "C17 C2 ";#95 Key Xkey$;                                       \! Busca estructura
   Xkey$, Xqty$                                                             ! 
  If TS.ER.RETURN = -1 Then Begin																						! Existe compra
     Xsaldo% = Val(UnPack$(Xqty$))  																				! Qty ya compradas
     Xsaldo% = Val(Gr.NoTax.Qmax$) - Xsaldo%                                ! Qty disponible
     If Xcompra% <= Xsaldo% Then Begin                                      !
     	  Gr.NoTax.Struc$(Xpos%,4) = Str$(Xcompra%)
     EndIf Else Begin																												!
     	If Xsaldo% > 0 Then Begin																							! 
     		 Gr.NoTax.Struc$(Xpos%,4) = Str$(XSaldo%)
      EndIf Else Gr.NoTax.Struc$(Xpos%,4) = "0"                             !
     EndIf																																	!
     Xqty$ = Unpack$(Xqty$)
     XQty$ = Str$(Val(Xqty$) + Val(Gr.NoTax.Struc$(Xpos%,4)))               ! Ajusta monto comprado
     Write Form "C17 C2" ; #95;                                            \! Graba el registro
      Xkey$, Pack$(Right$("0000"+XQty$,4))                                  ! 
  EndIf Else Begin																													! No existe compra
  	 If Xcompra% <= Val(Gr.NoTax.Qmax$) Then Begin
  	 	  Gr.NoTax.Struc$(Xpos%,4) = Str$(Xcompra%)
  	 EndIf Else Begin
  	 	  Gr.NoTax.Struc$(Xpos%,4) = Gr.NoTax.Qmax$
  	 EndIf
     Xkey$ = Pack$(Right$("000000000000000000"+Gr.Lcl.Clte$,18) +          \! Arma llave consulta
             Gr.NoTax.Struc$(Xpos%,3))                                      ! cliente + structura
     Write Form "C17 C2" ; #95;                                            \! Graba el registro
      Xkey$, Pack$(Right$("0000"+Gr.NoTax.Struc$(Xpos%,4),4))               ! 
  EndIf																																			! 
End Sub 

Sub VALIDA.IVA.ONLINE(XJ%)																									! Validacion compra en linea
Integer*4 XJ%, XI%, XK%, XL%, Xsaldo%, Xcompra%, Xvlr%, XX%									!
Integer*1 Xfnd%																															!
String    Xbuff$, LstStruc$(1), Xtemp4$, Xtrama$, Xsnd$, Xfin$							!
String    RtaLst$(2), Xd$
Xbuff$ = Right$("000000000000000000"+Gr.Lcl.Clte$,18) +                    \! ID del cliente
            "03" + Gr.NoTax.Fecha$																					! Qty Max y fecha no iva
Dim LstStruc$(100)																													!
Dim RtaLst$(100,2)																													!
XK% = 0																																			! 
For XI% = 1 To XJ%																													! Total de items vendidos
 Xfnd% = 0																																	! 
 For XL% = 1 To XK%																													! Toma lista de las 
  If Gr.NoTax.Struc$(XI%,3) = LstStruc$(XL%) Then                          \! estructuras comerciales vendidas
  	 Xfnd% = -1																															! Si encuentra
 Next XL%																																		!
 If Xfnd% = 0 Then Begin																										! No encontrado
 	  XK% = XK% + 1																														! Aumenta apuntador
 	  LstStruc$(XK%) = Gr.NoTax.Struc$(XI%,3)																	! Toma estructura
 EndIf																																			!
Next XI%																																		! Termina ciclo
XI% = 0 
For XI% = 1 To XK%																													!
    Xbuff$ = Xbuff$ + Right$(LstStruc$(XI%),15)                             ! Arma String validacion
Next XI%																																		!
Call TSHIECET																																! Tono de alerta
Call VISORES4690(1,"VALIDANDO LA COMPRA ","ESPERE POR FAVOR",0,"L")         ! Msg alerta cajero
Asc.Pay.Impr% = 0                                                           ! Ajusta numero trx para seguimiento
Xsnd$ = DATE$ +":"+ Time$                                                   ! Fecha y hora rta del requerimiento
XTemp4$ = Armar.Trama.Msg("19","01",Xbuff$,"00","0001","123456")            ! Arma trama mensaje 
XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)            ! Ejecuta Requerimiento java
Xfin$ = DATE$ +":"+ Time$                                                   ! Fecha y hora rta del requerimiento
Call NOTAX.AUDITORIA(XTemp4$,Xtrama$, Xsnd$, Xfin$)                    	    ! Rastreo movimiento
XTEMP4$ = Valida.Rta(XTrama$)																			          ! Valida rta entregada
If Xtemp4$ <> "00" Then Begin 																							! Falla en proceso java
	 Call VISORES4690(1,"PROCESANDO A VALIDAR","LOCAL, ESPERE...",0,"L")      ! Msg alerta cajero
	 TS.TEMP3I1 = 0																														! Ctrl proceso
	 Exit Sub 																																! Sale del proceso
EndIf																																				! 
XTEMP4$ = Mid$(XTrama$,12,2)	           																    ! Valida rta entregada
If XTemp4$ <> "00" Then Begin 																				      ! Rta No Satisfactoria
  Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									              ! Presenta Msg Error
  TS.TEMP3I1 = 0																														! Ctrl proceso
  Exit Sub 
EndIf

Xtemp4$ = Mid$(XTrama$,54,170)
XX% = Len(Xtemp4$) / 17
XK% = 0 : XI% = 1 
For XL% = 1 To XX%
  XD$ = Mid$(Xtemp4$,XI%,17)																								! toma estructura
  If Mid$(XD$,3,15) <> "               " Then Begin													! No es blancos
    XK% = XK% + 1																														!
    RtaLst$(XK%,0) = "1" + Mid$(XD$,3,15)                                   ! Toma estructura cmial
    RtaLst$(XK%,1) = Left$(Xd$,2)                                           ! Qty de cupo
    Call U.Imprime("Est:"+RtaLst$(XK%,0)+" Cupo:"+RtaLst$(XK%,1),2100H)
  EndIf
  Xi% = Xi% + 17                                                            ! Salta siguiente campo
Next XL%
XI% = 0 : XL% = 0
For XI% = 1 To XJ%
 For XL% = 1 To XK%
   If Gr.NoTax.Struc$(XI%,3) = RtaLst$(XL%,0) Then Begin                    ! estructuras comerciales vendidas
   	  If Val(Gr.NoTax.Struc$(XI%,1)) <> 0 Then \
       Xvlr% = Val(Gr.NoTax.Struc$(XI%,2)) / Val(Gr.NoTax.Struc$(XI%,1)) Else \! calcula valor unitario
       Xvlr% = 0 
      Gr.NoTax.Struc$(XI%,2) = Str$(Xvlr%)                                  ! Precio para anulacion
      TS.TEMP4I1 = Val(Gr.NoTax.Struc$(XI%,6))                              ! Tarifa del iva
      Xvlr% = ROUND(FLOAT(Xvlr%)/(1.+FLOAT(UE.TABLAIVA(TS.TEMP4I1))/100.),0,0) ! Calcula valor Base
      Gr.NoTax.Struc$(XI%,7) = Str$(Xvlr%)                                  ! Nuevo precio de venta
      Xcompra% = Val(Gr.NoTax.Struc$(XI%,1))                                ! Cantidad de compra en trx 
      Xsaldo%  = Val(RtaLst$(XL%,1))                                        ! Cupo de la categoria
      Call U.Imprime("Compra:"+Str$(xCompra%) + " " + " Sld:"+str$(Xsaldo%),2100H)
      If Xcompra% <= Xsaldo% Then Begin                                     !
     	  Gr.NoTax.Struc$(XI%,4) = Str$(Xcompra%)                             !
      EndIf Else Begin																											!
     	 If Xsaldo% > 0 Then Begin																						! 
     		 Gr.NoTax.Struc$(XI%,4) = Str$(XSaldo%)                             !
       EndIf Else Gr.NoTax.Struc$(XI%,4) = "0"                              !
      EndIf																																	!
      RtaLst$(XL%,1) = Str$(Val(RtaLst$(XL%,1)) - Val(Gr.NoTax.Struc$(XI%,4))) !
      Call U.Imprime("Consume:"+Gr.NoTax.Struc$(XI%,4),2100H)
   EndIf																																		!
 Next XL%
Next XI%
TS.TEMP3I1 = -1																														  ! Ctrl proceso
End Sub 																																		! Fin validacion compra en linea

Sub GR.VALIDACION.NOTAX																											! Validacion dia Sin iva
Integer*4 XI%, Xfnd%, Xtrg%, Xloc%, PRIC%, XJ%, Qty%, Xvlr%, Xni%, XS%      !
Integer*2 X.tarifa%, XNoPag%, XnoPasa%																			!
String X.TMP$, Xstruc$, Xsgn$, Xplu$																				!
Dim Gr.NoTax.Struc$(1500,7)																									! Maximo 1500 estructuras diferentes
TS.ER.RETURN = -1																														!
Open "R::$F:DNOTAX" KEYED RECL 22 AS 94 NoWrite NoDel 	  							  	! Control de fecha
If TS.ER.RETURN <> -1 Then Exit Sub 																				! Error de apertura
TS.ER.RETURN = -1
Open "R::$F:CNOTAX" KEYED RECL 19 AS 95                  							  	  ! Control de compra clientes
If TS.ER.RETURN <> -1 Then Exit Sub 																				! Error de apertura
  XI% = 0																																		! Init contador
  Xtrg% = 0
  XNoPag% = 0
  For XI% = 1 TO SL.END                                											! FOR ALL StringS
   H$ = READ.SL.STR$(XI%)                             											! GET String
   If LEN(H$) > 5 Then Begin                                                ! ASSURE GOOD String
    If ASC(H$) = 1 Then Begin                         											! ITEM ENTRY String
     X.TMP$ = ASIC.GETUNPK(H$,3)	   				           											! Tomo informacion del item vendido
     X.Tmp$ = Right$("000000000000"+X.Tmp$,12)         											! 
     X.Tmp$ = Pack$( X.tmp$ )                          											! Arma llave de busqueda
     TS.ER.RETURN = -1																 											! Ctrl errores
     Call IRRFEC.READ01 (X.tmp$, 4, TS.TEMP1$, 0) 		 											! Lectura Datos Itemr
     Call IRRFEC.SPLIT1 ( TS.TEMP1$ ) 			           											! Entrega datos al eamitemr.j86
     Xstruc$ = Left$(IR.USERDATA$,16)		  					   											! Estructura comercial
     If Xstruc$ = "                " Then Xstruc$ = "1111111111111111"			! Error en estructura comercial no procesa
     If Xstruc$ = "1999999999999999" Then Xstruc$ = "1111111111111111"			! Error en estructura comercial no procesa
     TS.TEMP2I1 = 0																							    				! Control proceso
     Call VALIDA.ESTRUCTURA.NOTAX(Xstruc$)                                  !
     If TS.TEMP2I1 = 0 Then Begin																						!
     	  Gr.NoTax.Dmax$ = "777"                          										! Estructura no cumple
     	  XnoPasa% = 0
     	  GoTo Ntax.Itm.Compra																								! Siguiente Item
     EndIf																																	!
     XnoPasa% = -1
     Xplu$ = unpack$(X.tmp$)                                                ! Plu vendido
     X.TARIFA% = Bono.Tarifa.Iva(IR.INDICAT1)          											! Tarifa Impuesto
     L% = ASC(RIGHT$(H$,1)) / 10H                      											! GET ITEM TYPE
     If L% = 0 OR L% = 8 Then Begin                    											! Item SALE OR CANCEL
      PRIC% = 0                                        											! ASSUME NO PRICE
      K% = MATCHB(":",H$,3) + 1                        											! START OF PRICE
      J% = MATCHB(":",H$,K%)                           											! END OF PRICE
      If J% > K% Then Begin                            											! PRICE FOUND
       PRIC% = PACKBIN4(H$,K%-1,J%-K%)                 											! GET PRICE
       Qty% = 1
       If L% = 8 Then Begin 																								! Anul
       	  PRIC% = 0 - PRIC%                																	! INVERT PRICE FOR VOID
       	  Qty% = -1																													! Qty Negative
       EndIf																																!
       Xfnd% = 0
       For XJ% = 1 To Xtrg% 
           If Gr.NoTax.Struc$(XJ%,0) = Xplu$ Then Begin                     ! Si PLU ya existe
           	  Xfnd% = -1 : Xloc% = XJ%
           	  XJ% = Xtrg% + 1
           EndIf
       Next XJ%																															! Fin recorrido estructuras
       If Xfnd% = 0 Then Begin																							! No encontrado
          Xtrg% = Xtrg% + 1                                                 ! Aumenta contador
          Gr.NoTax.Struc$(Xtrg%,0) = Xplu$																  ! Almacena estructura
          Gr.NoTax.Struc$(Xtrg%,1) = Str$(Qty%)															! Qty Vendida
          Gr.NoTax.Struc$(Xtrg%,2) = Str$(Pric%)														! Total vendido
          Gr.NoTax.Struc$(Xtrg%,3) = Xstruc$    														! Extructura comercial
          Gr.NoTax.Struc$(Xtrg%,4) = "0"    														    ! Qty para ajustar precio
          Gr.NoTax.Struc$(Xtrg%,5) = Gr.NoTax.Dmax$ 										    ! Dpto Nuevo 
          Gr.NoTax.Struc$(Xtrg%,6) = Str$(X.Tarifa%)   									    ! Tarifa de iva
          Gr.NoTax.Struc$(Xtrg%,7) = "0"               									    ! Nuevo precio 
          Xloc% = Xtrg%                                                     ! 
       EndIf Else Begin																											! Si encontrado
          Gr.NoTax.Struc$(XLoc%,1) = Str$(Val(Gr.NoTax.Struc$(XLoc%,1))+Qty%)  ! Aumenta qty vendida 
          Gr.NoTax.Struc$(XLoc%,2) = Str$(Val(Gr.NoTax.Struc$(XLoc%,2))+Pric%) ! Aumenta valor venta
       EndIf                                                                !
      EndIf                                            											! PRICE FOUND
     EndIf                                             											! ITEM SALE OR CANCEL
    EndIf                                              											! Item ENTRY String
    If ASC(H$) = 2 Then Begin                          										  ! ITEM ENTRY Extend
    	 If XnoPasa% = 0 Then GoTo Ntax.Itm.Compra                            !
     	 Asc.Tmp.Apun% = 3 																								    ! Init apuntador String 
       Gr.NoTax.Struc$(XLoc%,1) = Str$(Val(Gr.NoTax.Struc$(XLoc%,1))- Qty%) ! Ajusta la cantidad
       X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! Mpgroup
       X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! DealQ
       X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! SaleQ
       X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! SalePric
       X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! QtyorWeitgh
       X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! QtyorWeitgh
       If Pric% < 0 Then X.tmp$ = "-" + X.tmp$															!
       Qty% = Val(X.tmp$)																										!
     	 Gr.NoTax.Struc$(XLoc%,1) = Str$(Val(Gr.NoTax.Struc$(XLoc%,1))+ Qty%) ! Ajusta la cantidad
    EndIf																																	  ! Fin Extension Item

    If ASC(H$) = 5 Then Begin                          										  ! Forma de pago
     	  Asc.Tmp.Apun% = 3 																								  ! Init apuntador String 
     	  X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! Tipo y variedad
        If (MATCH(X.tmp$,Gr.NoTax.LstPago$,1) <= 0) Then Begin              ! Pagos No autorizados
     	  	 XNoPag% = -1
     	  	 GoTo Salida.Proceso.NoIva
     	  EndIf
    EndIf     																															! fin forma de pago

   EndIf																																		! Fin buen String 
   Ntax.Itm.Compra:                                   											! Siguiente Item vendido
  Next XI%                                             											! NEXT String

!  XI% = 0 : XJ% = 0
!  For XS% = 1 To Xtrg%																											! Recorre items vendidos
!   For XI% = 0 To Gr.Motor2.Xpos%                                           ! Revisar promociones
!     If Val(Gr.NoTax.Struc$(XS%,0)) = Val(Gr.Motor2.Items(XI%,0)) Then Begin! Plu del producto
!        Xvlr% = Val(Gr.Motor2.Items(XI%,1))                                 ! El valor del dscto
!        If Gr.Motor2.Items(XI%,7) = "00" Then Xvlr% = 0 - Xvlr%
!        Gr.NoTax.Struc$(XS%,2) = Str$( Val(Gr.NoTax.Struc$(XS%,2)) - Xvlr% )!
!     EndIf																																	!
!   Next XI%																																! fin promociones
!  Next XS%																																	! fin items vendidos
  If Xtrg% <= 0 Then GoTo Salida.Proceso.NoIva                              ! No hay estructuras a validar
  TS.TEMP3I1 = 0																														! Ctrl proceso
  Call VALIDA.IVA.ONLINE(Xtrg%)																							! Valida compra en linea
  If TS.TEMP3I1 = -1 Then Begin                                             !
  	 GoTo PROCESA.VENTA.SINIVA 													                    ! Si proceso OnLine Ok
  EndIf Else Begin																													! Falla proceso OnLine
     Call TSHIECET																																! Tono de alerta
     Call VISORES4690(1,"VALIDANDO LOCALMENTE","ESPERE POR FAVOR",0,"L")         ! Msg alerta cajero
  EndIf																																			! 
  XI% = 0																																		! 
  For XI% = 1 To Xtrg%                                                      !
    Xstruc$ = Gr.NoTax.Struc$(XI%,3)                                        ! Toma estructura 
    TS.TEMP2I1 = 0																							    				! Control proceso
    Call VALIDA.ESTRUCTURA.NOTAX(Xstruc$)                                   ! Consulta estructura
    If TS.TEMP2I1 = -1 Then Begin																    				! Encontrado
      Xvlr% = Val(Gr.NoTax.Struc$(XI%,2)) / Val(Gr.NoTax.Struc$(XI%,1))     ! calcula valor unitario
      Gr.NoTax.Struc$(XI%,2) = Str$(Xvlr%)                                  ! Precio para anulacion
      TS.TEMP4I1 = Val(Gr.NoTax.Struc$(XI%,6))                              ! Tarifa del iva
      Xvlr% = ROUND(FLOAT(Xvlr%)/(1.+FLOAT(UE.TABLAIVA(TS.TEMP4I1))/100.),0,0) ! Calcula valor Base
      Gr.NoTax.Struc$(XI%,7) = Str$(Xvlr%)                                  ! Nuevo precio de venta
      If Xvlr% <= Val(Gr.NoTax.Vmax$) Then Begin                            ! Si aplica beneficio
      	 Call VALIDA.STRUCT.CLIENT(XI%)                                     ! Valida estructura vs cliente
      EndIf																																	! Fin ctrl precio maximo
    EndIf	                                                									! fin encontrado
  Next XI%																																	!
  PROCESA.VENTA.SINIVA:
  XI% = 0 : Xni% = 0
  Dim Gr.NoTax.LstItm$(1500,7)																							! 
!-- Asigna las anulaciones
  For XI% = 1 To Xtrg%
    If Val(Gr.NoTax.Struc$(XI%,4)) > 0 And                                 \! Si qty con beneficio
       Val(Gr.NoTax.Struc$(XI%,6)) < 7 Then Begin									  				! e Item con IVA
    	 Xni% = Xni% + 1																											!
    	 Gr.NoTax.LstItm$(Xni%,0) = Gr.NoTax.Struc$(XI%,0)									  ! Asigna PLU
    	 Gr.NoTax.LstItm$(Xni%,1) = Gr.NoTax.Struc$(XI%,4)									  ! Asigna Qty
    	 Gr.NoTax.LstItm$(Xni%,2) = "U" 																		  ! Tipo producto
    	 Gr.NoTax.LstItm$(Xni%,3) = "A"																				! Accion aplicar
    	 Gr.NoTax.LstItm$(Xni%,5) = Gr.NoTax.Struc$(XI%,2)					          ! Precio unitario devolucion
    EndIf																																		!
  Next XI%
!-- Asigna la nueva venta  
  XI% = 0
  For XI% = 1 To Xtrg%
    If Val(Gr.NoTax.Struc$(XI%,4)) > 0 And                                 \! Si qty con beneficio
       Val(Gr.NoTax.Struc$(XI%,6)) < 7 Then Begin									  				! e Item con IVA
     	 Xni% = Xni% + 1																											!
    	 Gr.NoTax.LstItm$(Xni%,0) = Gr.NoTax.Struc$(XI%,0)									  ! Asigna PLU
    	 Gr.NoTax.LstItm$(Xni%,1) = Gr.NoTax.Struc$(XI%,4)									  ! Asigna Qty
    	 Gr.NoTax.LstItm$(Xni%,2) = "U" 																		  ! Tipo producto
    	 Gr.NoTax.LstItm$(Xni%,3) = "V"																				! Accion aplicar
    	 Gr.NoTax.LstItm$(Xni%,4) = Gr.NoTax.Struc$(XI%,5)										! Nuevo dpto
    	 Gr.NoTax.LstItm$(Xni%,6) = Gr.NoTax.Struc$(XI%,7)					          ! Nuevo precio unitario
    	 Gr.NoTax.LstItm$(Xni%,7) = Gr.NoTax.Struc$(XI%,3)					          ! Estructura comercial
    EndIf																																		!
  Next XI%
  Salida.Proceso.NoIva:
   If XNoPag% = -1 Then Begin																								! Hay pagos no autorizados
  	  Xni% = 0																															! Reporta no beneficios
  	  Dim Gr.NoTax.LstItm$(1500,6)																					! Init matriz beneficios
   EndIf																																		!
  If Xni% > 0 Then Begin
	   Xni% = Xni% + 1																											  !
     Gr.NoTax.LstItm$(Xni%,0) = "999999999999"															! Fin de registro
     Gr.NoTax.Proc% = -1																										!
     Gr.Notax.Cont% = 1																											!
     Gr.NoTax.Itm% = Xni%																										! Total movimientos
     Gr.NoTax.Aplicado% = -1																								!
  EndIf Else Begin
     Gr.NoTax.Proc% = 0																											!
     Gr.Notax.Cont% = 0																											!
     Gr.NoTax.Itm% = 0																											! Total movimientos
     Gr.NoTax.Aplicado% = 0
  EndIf
  Close 94
  Close 95
  If Gr.NoTax.Proc% = -1 Then Begin																					!
   Call U.IMPRIME(Gr.NoTax.Descriptor$,4101H)					      								!
   Call U.IMPRIME(Gr.NoTax.Descriptor$,2100H)					      								!
   Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															    ! Init secuencias registro
   TS.IO.KEYS(2)  = 80																											!
   TS.IO.DATA$(2) = Str$(Val(Gr.NoTax.LstItm$(1,0)))  											! Entrega primer plu
   TS.IO.DEVICE   = 1
   TS.IO.MOTORKEY = 80
  EndIf
End Sub 																																		! fin validacion dia Sin iva

Sub GR.CALCULO.COMPRA																												! Calculo compra registrada
Integer*4 Xd1%, Xd2%
String    Xfec$
Integer*1 Xvlclte%

 TS.TEMP1I1 = 0
 If Gr.NoTax.Aplicado% = -3 Then                                           \! Beneficio ya aplicado
 	  Exit Sub 																																!
 Xfec$ = "20" + Date$ 																											! Control proceso
 If Gr.NoTax.Fecha$ <> Xfec$ Then Begin 																		! No es dia de beneficio
 	  Exit Sub  																															! No aplica fecha 
 EndIf																																			!

 If Gr.NoTax.Proc% = -2 Then Begin																					!
 	  Call VISOR.AND.BORRAR("BENEFICIO DIA NO IVA YA APLICADO /Borrar")				! Msg de alerta
 	  Call VISOR.AND.BORRAR("FINALICE LA COMPRA   EN PROCESO  /Borrar")				!
    TS.TEMP1I1 = -1																													!
    Exit Sub 																																!
 EndIf																																			!
 If USER.FBACT.READ = 0 Then Begin 																					! Cliente no capturado
 	  Call VISORES4690(1,"CLIENTE NO CAPTURADO","...",1000,"L")								!
    Call VISORES4690(1,"EXCEPCION IVA NO ES ","APLICADO...",1000,"L")				!
 	  Exit Sub 																																! Cliente No ingresado
 EndIf																																			!

!--- Validacion datos cliente para factura electronica 
 Xvlclte% = -1
 If Gr.Lcl.Surnames$ = "" Or                                               \!
    Gr.Lcl.Surnames$ = String$(20," ") Then Xvlclte% = 0                    ! Datos incompletos
 If Gr.Lcl.Address$  = "" Or                                               \!
    Gr.Lcl.Address$  = String$(25," ") Then Xvlclte% = 0                    ! Datos incompletos
 If Gr.Lcl.Phone$    = "" Or 																							 \! Datos incompletos
    Gr.Lcl.Phone$ = "               " Then Xvlclte% = 0 										! Datos incompletos
 If Gr.Lcl.eMail$ = "" Or                                                  \!
    Gr.Lcl.eMail$ = String$(50," ") Then Xvlclte% = 0                       ! Datos incompletos
 If Gr.Lcl.City$ = "" Or                                                   \!
    Gr.Lcl.City$ = String$(20," ") Then Xvlclte% = 0                        ! Datos incompletos
 
 If Xvlclte% = 0 Then Begin                    															! Datos incompletos
 	  Call VISOR.AND.BORRAR("DATOS DE CLIENTE NO  COMPLETOS   /Borrar")				! Msg de alerta
 	  Call VISOR.AND.BORRAR("NO SE GENERA PROCESO DIA SIN IVA /Borrar")				!
 	  TS.TEMP1I1 = 0  																												!
 	  Exit Sub 																																! Cliente No ingresado
 EndIf
  
!-- fin validación datos cliente

 Xd1% = TS.BALDUE(0)																											  ! Saldo de la trx
 If TS.IO.DATA$(7) = "" Then Xd2% = Xd1% Else Xd2% = Val(TS.IO.DATA$(7))    ! Toma valor del pago
 If (Xd1% - Xd2%) <= 0 Then Begin 																					! Si cierre de trx
   Call GR.VALIDACION.NOTAX																									! Valida trx 
 EndIf																																			! Fin cierre de trx
 TS.TEMP1I1 = 0																							    						! Control proceso
 
End Sub 																																		! fin calculo compra registrada

Sub REVERSO.MARCACION.IVA																										! Reverso compras cliente
Integer*4 XS%, Xsaldo%
String Xkey$, Xqty$
 If Gr.NoTax.Aplicado% = -3 Then Begin
 	  TS.ER.RETURN = -1																												! Control de errores
 	  Open "R::$F:CNOTAX" KEYED RECL 19 AS 95                  							  ! Control de compra clientes
    If TS.ER.RETURN <> -1 Then Begin 																				! Falla en la apertura
    	 Call VISOR.AND.BORRAR("FALLA ACTUALIZACION SALDO CLIENTES IVA")			! Msg Alerta
    	 Exit Sub 																														! Error de apertura
    EndIf
    For XS% = 1 To (Gr.NoTax.Itm% - 1)																			! Recorre lista beneficiados
      If Gr.NoTax.LstItm$(XS%,3) = "V" Then Begin														! Item Beneficiado
         Xkey$ = Pack$(Right$("000000000000000000"+Gr.Lcl.Clte$,18) +      \! Arma llave consulta
                 Gr.NoTax.LstItm$(XS%,7))                                   ! cliente + structura
         TS.ER.RETURN = -1		  																				    ! Control de errores
         Read Form "C17 C2 ";#95 Key Xkey$;                                \! Busca estructura
         Xkey$, Xqty$                                                       ! 
         If TS.ER.RETURN = -1 Then Begin																	  ! Existe compra
            Xsaldo% = Val(UnPack$(Xqty$))  																  ! Qty ya compradas
            Xsaldo% = Xsaldo% - Val(Gr.NoTax.LstItm$(XS%,1))                ! Ajusta saldo cliente
            Write Form "C17 C2" ; #95;                                     \! Graba el registro
            Xkey$, Pack$(Right$("0000"+Str$(Xsaldo%),4))                    ! 
         EndIf Else Begin																									  !
         	  !Call VISOR.AND.BORRAR("REGISTRO ACTUALIZAR NO EXISTE /Borrar ")	! Msg Alerta
         	  
         EndIf 																															!
      EndIf																																	!
     Next XS%																																! 
    Close 95
 EndIf																																			!
End Sub 															 																			! Fin reverso compras cliente

Sub REPORTAR.BENEFICIOS.IVA																									! Reporta saldos consumidos ONLINE
Integer*4 Xp%
String Xbuff$, LstBen$,Xtemp4$, Xtrama$, Xsnd$, Xfin$
Xbuff$ = Right$("000000000000000000"+Gr.Lcl.Clte$,18) +                    \! ID del cliente
            "03" + Gr.NoTax.Fecha$																					! Qty Max y fecha no iva
For XP% = 1 To Gr.NoTax.Itm%																								! Recorre lista beneficiados
 If Ucase$(Gr.Notax.LstItm$(XP%,3)) = "V" Then Begin                        ! Si un Item venta
 	  Xbuff$ = Xbuff$ + Right$("00"+Gr.Notax.LstItm$(XP%,1),2) +             \! Adiciona qty y categoria
 	           Right$("000000000000000"+Gr.Notax.LstItm$(XP%,7),15)           ! vendida
 EndIf																																			! fin Item de venta
Next XP%
Call TSHIECET																																! Tono de alerta
Call VISORES4690(1,"ACTUALIZANDO SALDOS ","ESPERE POR FAVOR",0,"L")         ! Msg alerta cajero
Asc.Pay.Impr% = 0                                                           ! Ajusta numero trx para seguimiento
Xsnd$ = DATE$ +":"+ Time$                                                   ! Fecha y hora rta del requerimiento
XTemp4$ = Armar.Trama.Msg("19","02",Xbuff$,"00","0001","123456")            ! Arma trama mensaje 
XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)            ! Ejecuta Requerimiento java
Xfin$ = DATE$ +":"+ Time$                                                   ! Fecha y hora rta del requerimiento
Call NOTAX.AUDITORIA(XTemp4$,Xtrama$, Xsnd$, Xfin$)                  	      ! Rastreo movimiento
XTEMP4$ = Valida.Rta(XTrama$)																			          ! Valida rta entregada
If Xtemp4$ <> "00" Then Begin 																							! Falla en proceso java
	 Call VISORES4690(1,"*** PROCESO FUE *** ","*** FALLIDO *** ",1000,"L")   ! Msg alerta cajero
	 Exit Sub 																																! Sale del proceso
EndIf																																				! 
XTEMP4$ = Mid$(XTrama$,12,2)	           																    ! Valida rta entregada
If XTemp4$ = "00" Then Begin 																					      ! Rta Satisfactoria
   Call TSHIECET																													  ! Tono de alerta
   Call VISORES4690(1,"*** PROCESO FUE *** ","*** EXITOSO *** ",1000,"L")   ! Msg alerta cajero
EndIf Else Begin 																												    ! Rta no satisfactoria
  Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									              ! Presenta Msg Error
  Exit Sub 
EndIf

End Sub 																																		!


Sub GR.DIA.SINIVA(Xopt%) Public 																						! Dia Sin iva
Integer*4 Xopt%,XS%, UE.I%																									! UE ejecutar

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.NoTax.Ok%  = 0                                                         ! Proyecto Apagado
  Gr.NoTax.Aplicado% = 0
  Dim GR.NTNEGCNT(7) 
  Dim GR.NTNEGAMT(7) 
  TS.TS11WERR$ = ""																												  ! Control errores
  Open "R::$SCNTRL" As 94 UNLOCKED NOWRITE NODEL 													  ! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then Begin                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA DIA SIN IVA  ")									! MSg alerta
  	 Call U.IMPRIME("ERROR APERTURA DIA SIN IVA   ",6100H)                  !
  	 Exit Sub 																															! Sale del proceso
  EndIf  																																		!
  If End #94 Then UE.FIN.NOTAX           																	  ! Si es EOF
  While (1)															  																  ! Recorre archivo parametros
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[DIA SIN IVA]" Then Begin	   	     				          ! Dia Sin iva 
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.NoTax.Ok%       = Val(Mid$(TS.TEMP1$,30,2)) 		    					  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.NoTax.LstPago$  = Mid$(TS.TEMP1$,30,70)   			    					  ! Lista formas de pago
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.NoTax.Descriptor$ = Mid$(TS.TEMP1$,30,37)  			    					  ! Descriptor tiquete
         For UE.I% = 0 To 7                        
             GR.NTNEGCNT(UE.I%) = To.NEGCNT(UE.I%) 
             GR.NTNEGAMT(UE.I%) = To.NEGAMT(UE.I%) 
         Next UE.I%                                
        GoTo UE.FIN.NOTAX  																								  ! Sale del ciclo de carga                   
        EndIf																																! Fin caga
  Wend 																																			! Fin carga
  UE.FIN.NOTAX:                                                             !
     Close 94																																! Cierra archivo
     Call GR.VALIDA.FECHANOTAX                                              ! Valida Fecha aplicacion evento
   If Gr.NoTax.Ok% = -1 Then                                               \! Proyecto Activo
      Call U.IMPRIME("MODULO DIA SIN IVA   ON  29-Jun-2020",2100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO DIA SIN IVA   OFF 29-Jun-2020",2100H)          ! Msg Proyecto Cargado
EndIf 																																			! Fin carga de parametros 

If Gr.NoTax.Ok% <> -1  Then Exit Sub                                        ! Si proyecto apagado
If TS.STANDALONE       Then Exit Sub	                                      ! Si en operacion TOF

!--- EAMTSU02.J86
If Xopt% = 02 Then Begin                                                    ! Nueva transaccion
   Gr.NoTax.Proc% = 0
   Gr.Notax.Cont% = 0
   Gr.NoTax.Itm%  = 0
   Gr.NoTax.Aplicado% = 0
   Dim Gr.NoTax.LstItm$(1500,7)																							! 
   For UE.I% = 0 To 7                        																!
       To.NEGCNT(UE.I%) = GR.NTNEGCNT(UE.I%)
       To.NEGAMT(UE.I%) = GR.NTNEGAMT(UE.I%) 
   Next UE.I%                                																!

EndIf																																				! fin nueva transaccion

!--- EAMTSU08.J86
If Xopt% = 08 Then Begin                                                    ! En validacion de productos
	 If Gr.NoTax.Aplicado% = -3 Then Begin
	 	  Call VISOR.AND.BORRAR("BENEFICIO DIA NO IVA YA APLICADO /Borrar")		  ! Msg de alerta
	 	  Call VISOR.AND.BORRAR("NO SE PERMITE VENTA  MAS ITEMS   /Borrar")		  ! Msg de alerta
	 	  IR.INDICAT0 = IR.INDICAT0 OR 04H			                	              ! No permite venta
	 	  Exit Sub 
	 EndIf
	 If Gr.NoTax.Proc% = -1 Then Begin																				! En simulacion
	 	  If Gr.NoTax.Cont% <= Gr.NoTax.Itm% Then Begin													! 
	 	  	 CHAIN.COUNT = 1                                             				! Contador de item linkeados
	 	  	 If Gr.NoTax.Cont% = 1 Then Begin                                   !
            SL.IT.ITEMCODE$ = Gr.Notax.LstItm$(Gr.NoTax.Cont%,0)            !
            TS.IO.DATA$(2)  = SL.IT.ITEMCODE$																!
	 	  	 EndIf                                                              !
         If IR.INDICAT0 AND 40H Then Begin        													! Si el Item es pesable 
            TS.IO.KEYS(6)  = 72																							! Simula secuencia registro
            TS.IO.DATA$(6) = Gr.Notax.LstItm$(Gr.NoTax.Cont%,1)							!
         EndIf Else Begin																										! Item por cantidad
            TS.IO.KEYS(6)  = 75																							! Simula cantidad
            TS.IO.DATA$(6) = Gr.Notax.LstItm$(Gr.NoTax.Cont%,1)							!
         EndIf																															!
	 	  	 If Ucase$(Gr.Notax.LstItm$(Gr.NoTax.Cont%,3)) = "A" Then Begin     ! Si un Item anulado
	 	  	 	  TS.IO.KEYS(8) = 67                                              ! Anula movimiento compra
            IR.PRICE1 = Val(Gr.Notax.LstItm$(Gr.NoTax.Cont%,5))
            IR.PRICE2 = IR.PRICE1
         EndIf
	 	  	 If Ucase$(Gr.Notax.LstItm$(Gr.NoTax.Cont%,3)) = "V" Then Begin     ! Si un Item venta
	 	  	 	  TS.IO.KEYS(8) = 0												                        !
	 	  	 	  Gr.NoTax.LstItm$(Gr.NoTax.Cont%,5) = Str$(IR.PRICE1)            ! 
            SL.IT.DEPARTME = Val(Gr.Notax.LstItm$(Gr.NoTax.Cont%,4))        ! Dpto nuevo venta
            IR.DEPARTME$ = Pack$(Right$("0000"+Str$(SL.IT.DEPARTME),4))     !
            IR.PRICE1 = Val(Gr.Notax.LstItm$(Gr.NoTax.Cont%,6))             !
            IR.PRICE2 = IR.PRICE1                                           !
            IR.INDICAT1 = 24H																								! Enciende Plan Iva C
            IR.INDICAT1 = IR.INDICAT1 Or 14H                 								! Enciende Plan Iva D
         	  TS.TEMP1$ = Pack$(Gr.NoTax.LstItm$(Gr.NoTax.Cont%,0)) +        \! PLU beneficiado
      	       ":" + Pack$(Gr.NoTax.LstItm$(Gr.NoTax.Cont%,1)) +           \! Cantidad beneficiada
      	       ":" + Pack$(Gr.NoTax.LstItm$(Gr.NoTax.Cont%,5)) +           \! Precio venta original
      	       ":" + Pack$(Gr.NoTax.LstItm$(Gr.NoTax.Cont%,6)) +           \! Nuevo precio unitario
      	       ":" + Pack$(Gr.NoTax.LstItm$(Gr.NoTax.Cont%,4)) +           \! Nuevo dpto reportado
      	       ":" + Pack$(Gr.NoTax.LstItm$(Gr.NoTax.Cont%,7))              ! Estructura comercial producto
            Call Grabacion.String.Usuario2("20200617",TS.TEMP1$)   					! Graba String usuario       
         EndIf
	 	  	 If Gr.Notax.LstItm$(Gr.NoTax.Cont%+1,0) = "999999999999" Then Begin! Ultimo del proceso
	 	  	 	  IR.LINKEDTO$ = Pack$("0000")																	  ! Termina ciclo
	 	  	 	  Gr.NoTax.Proc% = -2																							!
	 	  	 	  Gr.NoTax.Aplicado% = -2
	 	  	 EndIf Else Begin 																									!
            IR.LINKEDTO$ = Pack$(Gr.Notax.LstItm$(Gr.NoTax.Cont%+1,0))	    !
         EndIf																															!
	 	  	 Gr.NoTax.Cont% = Gr.NoTax.Cont% + 1																! 
	 	  EndIf 																																!
	 EndIf																																		! Fin simulacion
EndIf																																				! 

!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                    ! validando forma de pago

If	TS.IO.KEYS(1) = 82 And TS.IO.MOTORKEY = 81 Then Begin                   ! Si Suspension de trx
 If (Gr.NoTax.Aplicado% <> 0) Then Begin                                    ! Si subsidio capturado en trx
 			 Call Visor.And.Borrar("SUSPENSION NO AUTOR.CON DSCTO IVA APPL.")     ! Msg de alerta
       Call Visor.And.Borrar("TOTALICE O ANULE LA COMPRA /Borrar     ")     ! Msg de alerta
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
 EndIf																																		  !
EndIf
	
 If (TS.IO.STATE = 10) Then 										 											     \! No anulando pago
 If (TS.IO.KEYS(7) > 90 AND TS.IO.KEYS(7) < 97) Then Begin                  ! Valida si en forma de pago 
 	 If TS.IO.DATA$(3) = "" Then                                             \! No ingresa variedad
      TS.TEMP1$ = ";" + Str$(TS.IO.KEYS(7)-90) + "1;" Else                 \! por default reporta variedad 1
      TS.TEMP1$ = ";" + Str$(TS.IO.KEYS(7)-90) + TS.IO.DATA$(3) + ";"       ! Forma de pago valida
 	 If (MATCH(TS.TEMP1$,Gr.NoTax.LstPago$,1) > 0) Then Begin                 ! Pago autorizado
    Call GR.CALCULO.COMPRA																									! Valida dia Sin iva
    If TS.TEMP1I1 = -1 Then Begin																						! Tiene beneficio iva
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 0 																									! 
       TS.GUIDANCE = 1020																										! Solicita total a la trx
       Exit Sub 																														! Sale del proceso
    EndIf																																	  ! Fin tiene beneficio iva
    UE.I% = 0
    For UE.I% = 0 To 7                        															!
       To.NEGCNT(UE.I%) = 99999999
       To.NEGAMT(UE.I%) = 999
    Next UE.I%                                															!
    
   EndIf Else Begin																													! Si pago autorizado
   	  If Gr.NoTax.Aplicado% = -3 Then Begin
   	  	 Call VISOR.AND.BORRAR("BENEFICIO IVA YA    APLICADO /Borrar")
   	  	 Call VISOR.AND.BORRAR("FORMA DE PAGO NO    AUTORIZADA /Borrar")
   		   Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														! Inicializa secuencia de teclado
   		   TS.IO.MOTORKEY = 0 																								! 
         TS.GUIDANCE = 1020																									! Solicita total a la trx
         Exit Sub 																													! Sale del proceso
   	  EndIf
   EndIf
 EndIf																																			! fin forma de pago
EndIf																																				! fin formas de pago

!--- EAMTSU20.J86
If Xopt% = 20 Then Begin                                                    ! Impresion CR 

 If MATCH(TS.SDESC$(59),TS.PRTBUF$,1) > 0 THEN BEGIN												! Si anulacion total de la trx
    Call REVERSO.MARCACION.IVA   																						! Reversa las compras del cliente
    Gr.NoTax.Proc% = 0
    Gr.Notax.Cont% = 0
    Gr.NoTax.Itm%  = 0
    Gr.NoTax.Aplicado% = 0
    Dim Gr.NoTax.LstItm$(1500,7)																						! 
 EndIf																																			!

 If (TS.INTRX) AND (TS.LINETYPE = 6 AND TS.LINEDATA = 1) And  						 \!
    (TS.TENDERED (0) <> 0 Or TS.TRX.STATUS <> 100) Then Begin								!
    If Gr.NoTax.Aplicado% = -3 Then Begin																		! Si beneficio no iva aplicado
    	 Call REPORTAR.BENEFICIOS.IVA																					!
    	 Gr.NoTax.Aplicado% = -4																							!
    	 Gr.Fis.Doc% = -1                                                     ! No reporta numeracion fiscal
!-- Reporta remision mercancia    	 
       Call Asignacion.Consecutivo.Remision													        ! Genera consecutivo movimiento
       TS.TEMP1$ = Right$("0000000"+STR$(Gr.Crd.Remision%),07)              !
       TS.TEMP2$ = "   REMISION NRO: "+TS.STORE$+"-"+TS.TERMINAL$+"-"+TS.TEMP1$
       TS.TEMP2$ = Left$(TS.TEMP2$ + STRING$(40," "),40)                    !
       Call U.Imprime(TS.TEMP2$,6100H)                                      !
       TS.TEMP1$ = Right$("0000000000000000"+STR$(Gr.Crd.Remision%),16)     !
       TS.TEMP2$ = "TR+"                                           +       \! Arma Str Usuario con 
                   "REMI"+Pack$(TS.TEMP1$)+":"                           + \!                      
                   STR$(ABS(TS.GROSSPOS + TS.GROSSNEG))                  + \! #Fact, Vr/Vta Neto   
                   ":" + Pack$("00") + ":" + Pack$("00")                 + \! y Vrs de Iva         
                   ":" + Pack$("00") + ":" + Pack$("00")                 + \!                      
                   ":" + Pack$("00") + ":" + Pack$("00")                 + \!                      
                   ":" + Pack$("00") + ":" + Pack$("00")                    !                      
       Call Grabacion.Cadena.Usuario2("21",TS.TEMP2$)                       !
       Call Graba.Itm.FE.NoTax																							! Almacena Items para FE
    EndIf																																		! fin reporte beneficio
 EndIf 

EndIf																																				! Imppresion CR

!--- EAMTSU60.J86
If Xopt% = 60 Then Begin                                                    ! Impresion CR 
 If (TS.LINETYPE = 1) Then Begin
 	If Gr.NoTax.Aplicado% = -2 Then Begin
 		 Call U.IMPRIME(String$(37,"-"),6100H)
 		 Gr.NoTax.Aplicado% = -3
     TS.BD.DSPPARM = -1                              												! SHOW BD TO DISPLAY SUBTOTAL
     CALL TSBDEC01                                   												! DISPLAY NEW SUBTOTAL
     Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 															! Init vectores de carga
     TS.IO.DATA$(3) = "1"                   				    										! Variedad de Pago
     TS.IO.KEYS(3)  = 78                                             				! Tecla Slash
     TS.IO.DATA$(7) = "1"	  																					      ! Valor a entregar
     TS.IO.KEYS(7)  = 91
     TS.IO.MOTORKEY = TS.IO.KEYS(7)																					!
     
 	EndIf
 EndIf
EndIf																																				! Fin impresion CR

End Sub 																																		!
