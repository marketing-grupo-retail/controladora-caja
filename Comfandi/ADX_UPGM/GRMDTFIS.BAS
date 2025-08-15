!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMDTFIS.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Generacion datos fiscales *
!                integración Carvajal DIAN        *
!**************************************************
! Modificaciones:
! ----- Version 1.0 10 Junio 2022 -----------------

%ENVIRON T		                 																							! Ambiente de terminal

String   Global  Gr.Motor2.Items(2),                                       \! Dsctos motor promociones
                 IR.USERDATA$ 																						  ! Datos adicionales Item
Integer*4 Global Asc.Tmp.Apun%																							! 
Integer*4 Global UE.TABLAIVA(1)                                             ! Tarifas de IVA
Integer*4 Global Gr.Iva.PtgIcui%,                                          \! Porcentaje ICUI
                 Gr.Motor2.Xpos%,                                          \!
                 Gr.Fis.ConMem%,                                           \! Consecutivos fiscales 
                 Gr.Fis.FisMem%																							! tlog
String    Global Gr.Fis.PrefFis1$,                                         \! Prefijo fiscal 1
                 Gr.Fis.PrefFis2$, 																				 \! Prefijo fiscal 2
                 Gr.Fis.ResolFis1$,																				 \! Prefijo fiscal 2
                 Gr.Fis.ResolFis2$ 																				  ! Prefijo fiscal 2
                 
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

Sub TSHIECET External		    																								!  Tono de alerta
End Sub 

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                    *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                       *!
  STRING    DATA$      !* String to hold the data read from file.          *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                             *!
End Sub 

Sub IRRFEC.SPLIT1 (RECORD$) EXTERNAL
  STRING    RECORD$    !* The record (including the key) read from file    *!
End Sub 

Sub Flag.Item.Iva(Xindi1%)	External																				! Calculo flag de iva
Integer*1 Xindi1%           																								!
End Sub 

Sub Adjust.ItemName.Data																										! Ajuste caracteres Item Name
String GrAlpha$, GrName$, GrLt$																							!
Integer*2 XJ%																								                !
  GrAlpha$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890()*/+-"	 									! Lista caracteres admitidos
  GrName$ = ""																															!
  For XJ% = 1 To Len(IR.ITEMNAME$)																			    ! Recorre descripcion Item
    If match(Ucase$(Mid$(IR.ITEMNAME$,XJ%,1)),GrAlpha$,1) <= 0 Then        \! Caracter especial
     	 GrLt$ = " " Else GrLt$ = Mid$(IR.ITEMNAME$,XJ%,1)                    ! Elimina char especial cambia x espacio
       GrName$ = GrName$ + GrLt$																					  ! Arma descripcion 
  Next XJ%
  IR.ITEMNAME$ = Left$(GrName$+String$(18," "),18)                          ! AJUSTA DATO
End Sub 																																		!

Function LISTA.TRX.DIAN Public 																							! Datos para envio de trx carvajal
  Integer*4 PRIC%, TOT4%, XI%, XInd1%, XInd2%, X.J%, Xqty%, Xfind%,        \!
            Gr.Xcont%, Xpric%,Xtv%(1), Xpag%, XQlin%, XQItm%, Xchg%					!
  Integer*2 Xsgn%, Xpos%																										!
  String    XTMP$, Xitm$, XPric$, Xdummy$, Xkey$, Xbasura$, X.Ean$, Xcot$   !
  String    Gr.Tmp.Itm$(2), Xbuff$, Xiva$, Xund$, X.Flag$, Gr.Impoc$,      \!
            XDepto$, XPvp$, Xicui$, XDepto1$, LISTA.TRX.DIAN								!
  Gr.Xcont% = 0 																														! Numero de Items
  Dim Gr.Tmp.Itm$(1000,10)				 																			    ! Lista de Items vendidos
  Dim Xtv%(70)																															! Formas de pago 
  X.J% = 0 : XQlin% = 0 : XQItm% = 0	: Xchg%	= 0														!
  FOR X.J% = 1 TO SL.END                    																! FOR ALL StringS
    H$ = READ.SL.STR$(X.J%)                 																! GET String
    If LEN(H$) > 5 Then  Begin                                              ! ASSURE GOOD String
     If ASC(H$)  = 1 Then Begin             																! ITEM ENTRY String
     	  Asc.Tmp.Apun% = 3																										!
        XItm$   = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Item Vendido
        Xpric$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Precio Venta
        Xdepto1$= Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Departamento
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
        Call Adjust.ItemName.Data
        Xdepto$ = Left$(IR.USERDATA$,16)																		! Estructura comercial
        Xdepto$ = Mid$(Xdepto$,2,3)                                         ! Toma la seccion comercial
        Xiva$ = Str$(UE.TABLAIVA(TS.TEMP1I1))                               ! Tasa de IVA
        X.Flag$ = "0"																												! Data por default
        If Val(Xiva$) = 0 Then Begin                                        ! IVA en Cero        
         If TS.TEMP1I1 = 7 Then X.Flag$ = "0"                               ! Es excluido
         If TS.TEMP1I1 = 8 Then X.Flag$ = "1"       			                  ! Es excento
        EndIf
        Gr.Impoc$ = "00"																										! dato por default
        If TS.TEMP1I1 = 4 Then Begin																				! Impuesto impoconsumo
        	 Gr.Impoc$ = Str$(UE.TABLAIVA(TS.TEMP1I1))
        	 Xiva$ = "0" 
        	 X.Flag$ = "0"
        EndIf
        Xicui$ = "000"																											!
        If (IR.INDICAT0 And 40H) Then Xund$ = "KLG" Else Xund$ = "UD"       ! Si pesable o unitario
        If (IR.INDICAT1A AND 08H) Then                                     \! Ptg ICUI
        	  Xicui$ = Right$("000"+Str$(Gr.Iva.PtgIcui%),3)								  !
        If Val(Xdepto1$) >= 800 Then Xicui$ = "000"													!
        Xfind% = 0	: XI% = 0																								! Encontrado
     	  Xpos% = 0																														! Ctrl busqueta Item 
     	  For XI% = 1 To Gr.Xcont%																						! Total de items
     	   If Gr.Tmp.Itm$(XI%,0) = Xitm$ Then Begin                     			! SKU producto encontrado
            Xpos% = XI%																											! Encontrado
            XI% = Gr.Xcont% + 2																							! Sale de la busqueda
         EndIf																															!
     	  Next XI%
        If Xpos% = 0 Then Begin                                             ! Nuevo Item
           Gr.Xcont% = Gr.Xcont% + 1												                ! Posicion Almacenar
           Gr.Tmp.Itm$(Gr.Xcont%,0) = Xitm$                                 ! SKU producto
           Gr.Tmp.Itm$(Gr.Xcont%,1) = Str$( Val(Xpric$) * Xsgn%)            ! Precio de Venta
           Gr.Tmp.Itm$(Gr.Xcont%,2) = Str$( Xsgn% * 1 )                     ! Cantidad Vendida
           Gr.Tmp.Itm$(Gr.Xcont%,3) = Xiva$                                 ! Tarifa del IVA
           Gr.Tmp.Itm$(Gr.Xcont%,4) = X.flag$                               ! Excento o Excluido
           Gr.Tmp.Itm$(Gr.Xcont%,5) = Gr.Impoc$                             ! Impoconsumo
           Gr.Tmp.Itm$(Gr.Xcont%,6) = Xund$                                 ! Si es pesable o unitario
           Gr.Tmp.Itm$(Gr.Xcont%,7) = TrimRight$(IR.ITEMNAME$)							! Descripcion Item
           Gr.Tmp.Itm$(Gr.Xcont%,8) = Xdepto$																! Seccion de venta
           Gr.Tmp.Itm$(Gr.Xcont%,9) = "0"      														  ! Total Descuento
           Gr.Tmp.Itm$(Gr.Xcont%,10) = Xicui$ 														  ! Ptg ICUI
           Xpos% = Gr.Xcont%                                                ! Asigna posicion de vector
        EndIf Else Begin																									  ! Item Encontrado
          Gr.Tmp.Itm$(Xpos%,2) = Str$(Val(Gr.Tmp.Itm$(Xpos%,2)) +          \!
        	                                 (Xsgn% * 1))                     ! Cantidad Vendida
          Gr.Tmp.Itm$(Xpos%,1) = Str$(Val(Gr.Tmp.Itm$(Xpos%,1)) +          \! Suma totales de venta
                                     (Val(Xpric$) * Xsgn%))									!
        EndIf
     EndIf                                  																! ITEM ENTRY String
     Asc.Tmp.Apun% = 3																										  !     
     If ASC(H$)  = 2 Then Begin             																! ITEM ENTRY String Extention
     	 If Xpos% <> 0 Then Begin 																						! Extention Item
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
     If ASC(H$) = 5 Then Begin                          										! Forma de pago
     	  Asc.Tmp.Apun% = 3 																								  ! Init apuntador String 
     	  Xtmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! Tipo y variedad
     	  Xpag% = Val(Xtmp$)
     	  Xtmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! Valor Pagado
     	  Xtv%(Xpag%) = Xtv%(Xpag%) + Val(XTmp$)
     	  Xchg% = Xchg% + Val(XTmp$)
     EndIf     																															! fin forma de pago
     If ASC(H$) = 6 Then Begin                          										! Anulacion Forma de pago
     	  Asc.Tmp.Apun% = 3 																								  ! Init apuntador String 
     	  Xtmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! Tipo y variedad
     	  Xpag% = Val(Xtmp$)
     	  Xtmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! Valor Pagado
     	  Xtv%(Xpag%) = Xtv%(Xpag%) - Val(XTmp$)
     	  Xchg% = Xchg% - Val(XTmp$)
     EndIf     																															! fin Anulacion forma de pago
     
!     If ASC(H$) = 9 Then Begin                          										! Cambio al cliente
!     	  Asc.Tmp.Apun% = 3 																								  ! Init apuntador String 
!     	  Xtmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! Tipo y variedad
!     	  Xpag% = Val(Xtmp$)																									!
!     	  Xtmp$ = Str$(TS.BALDUE(0))
!     	  If TS.BALDUE(0) > 0 Then Xtmp$ = Str$(TS.BALDUE(0) * -1)						! Si reintegro
!     	  Xtv%(Xpag%) = Xtv%(Xpag%) + Val(XTmp$)
!     EndIf     																															! fin Anulacion forma de pago

    EndIf
  NEXT X.J%   
  X.J% = 0 : XI%  = 0
  For X.J% = 1 To Gr.Motor2.Xpos%																						! Total promociones aplicadas
   For XI% = 1 To Gr.Xcont%
      If Val(Gr.Motor2.Items(X.J%,0)) = Val(Gr.Tmp.Itm$(XI%,0)) Then Begin  ! Plu encontrado
      	 Gr.Tmp.Itm$(XI%,9) = Gr.Motor2.Items(X.J%,1)												! Total dscto aplicado
      EndIf
   Next XI%
  Next X.J%
  X.J% = 0 : XItm$ = "" : XQItm% = 0
  For X.J% = 1 To Gr.Xcont%																									! Productos vendidos
      XTMP$ = "00"																													! Signo operacion
      XPric$ = Gr.Tmp.Itm$(X.J%,1)																					! Toma venta total
      Xund$  = Gr.Tmp.Itm$(X.J%,2)																					! Qty total venta
      If Val(Xund$) = 0 Then GoTo Next.Itm.Vta															! No hay Items vendidos
      If Val(Xpric$) < 0 Then Begin																					! Si venta negativa
      	 Xpric$ = Str$( Val(Xpric$) * -1)																		! Invierte signo
      	 Xund$  = Str$( Val(Xund$) * -1 )																		! Invierte signo
      	 Xtmp$ = "01"																												! Reporta Item negativo
      EndIf																																	!
      If Gr.Tmp.Itm$(X.J%,6) = "UD" Then Begin															! Si venta por unidad
      	 Xpric% = ( Val(Xpric$) / Val(Xund$) )														  ! Precio unitario
       	 If Val(Xpric$) < 0 Then                                           \!
      	    XQItm% = XQItm% - Val(Xund$)		Else  											   \! Total items 
      	    XQItm% = XQItm% + Val(Xund$)																		!
      	 XPvp$ = Str$ (Xpric%)																							!
      EndIf Else Begin																											! Si venta pesable
      	 XPric% = (Val(Xpric$)*1000) / Val(Xund$) 										      ! Precio unitario
      	 XPvp$ = Str$( XPric% ) 																						! Precio unitario
      	 If Val(Xpric$) < 0 Then                                           \!
      	    XQItm% = XQItm% - 1		Else  																   \! Total items 
      	    XQItm% = XQItm% + 1																							!
      EndIf																																	!
      If Val(Xund$) <> 0 Then Begin																					!
        XQlin% = XQlin% + 1																									! Numero de productos
        Xitm$  = Xitm$ + (Gr.Tmp.Itm$(X.J%,0))                           + \! SKU producto
               ":" + (Gr.Tmp.Itm$(X.J%,7))                               + \! Item Name 
               ":" + (Xpvp$)                                             + \! Precio unitario venta
               ":" + (Xund$)                                             + \! Qty Vendida          
               ":" + (Gr.Tmp.Itm$(X.J%,9))                               + \! Descuentos aplicados
               ":" + (Gr.Tmp.Itm$(X.J%,5))                               + \! Impoconsumo
               ":" + (Gr.Tmp.Itm$(X.J%,3))                               + \! Tasa Impuesto
               ":" + (Gr.Tmp.Itm$(X.J%,10))                              + \! Ptg ICUI
               ":" + Gr.Tmp.Itm$(X.J%,6) + ","                              ! Unitario o pesado
        TOT4% = TOT4% + Val(Gr.Tmp.Itm$(X.J%,9))														! Total descuentos en la trx
      EndIf																																	!
    Next.Itm.Vta:
    
  Next X.J%																																	! Fin items vendidos 
  Xtmp$ = Str$(TOT4%)																												! Total descuentos 
  X.J% = 0 : Xpric$ = ""																										!

!  Write #34 ; "TOTAL TRX :"+STR$(TS.TOTALS(0,0,0)) + CHR$(10)
!  Write #34 ; "PAGOS TOT :"+STR$(XCHG%) + CHR$(10)
!  Write #34 ; "VUELTAS   :"+Str$(TS.TOTALS(0,0,0) - Xchg%) + Chr$(10)

  Xchg% = (TS.TOTALS(0,0,0) - Xchg%) * -1																		! Valor de las vueltas
  For X.J% = 11 To 66																												! Formas de Pago
      If Xtv%(X.J%) <> 0 Then Begin																					! Pago recibido
      	 If Xtv%(X.J%) > Xchg% Then Begin																		! Si pago puede asumir vueltas
      	 	  Xtv%(X.J%) = Xtv%(X.J%) - Xchg%																	! Descuenta las vueltas
      	 	  Xchg% = 0																												! Init proceso
      	 EndIf 																															!
      	 XPric$ = Xpric$ + Str$(X.J%) + ":" + Str$(Xtv%(X.J%)) + ","        ! Valor pagos 
      EndIf																																	! 
  Next X.J%																																	!
  LISTA.TRX.DIAN = Xitm$ + ";" + Xpric$ + ";"         							        ! Retorna trx 
  TS.TEMP5I4 = XQlin% : TS.TEMP4I4 = XQItm%																	! Total de lineas e items
End Function 																																!

Sub REVISAR.CONSEC.TLOG	Public																							! Revision ultimo consecutivo Tlog
String Xllave$, TERM$, SLOGNAME$, OSLOGNAME$, FECHA.CIER$,RESERVED$,  		 \!
       Xlog$, Str.Log$, B$, Xtmp$, Xterm$, Xtrx$, Xpref1$,                 \!
       Xpref2$																															!
Integer*4 ClosePNT, Q%, P%, A%																							!
Integer*1 F%																																!
  TS.ER.RETURN = -1																													! Ctrl errores
  Open "R::EAMCSCF1" KEYED RECL 36 AS 100 NOWRITE NODEL											! Busca Eamtran Activo 
  If TS.ER.RETURN = -1 Then Begin																						! Apertura correcta
     Xllave$ = Pack$("9998")																								! Busqueda dato tlog
     Read Form "C2 C8 C8 I4 C5 C9";#100 Key Xllave$; TERM$,                \!
       SLOGNAME$,OSLOGNAME$,ClosePNT,FECHA.CIER$,RESERVED$  								!
     Close 100																															!
  EndIf	Else Begin																													! Falla acceso tlog
	 Call TSHIECET
	 Call VISOR.AND.BORRAR("FALLA EN VALIDACION CONSECUTIVOS FISCALES")
   Call VISOR.AND.BORRAR("ACCESO LOG TRANSAC. NO VALIDO /Borrar")
   Call U.IMPRIME("REPORTAR DE INMEDIATO SOPORTE POS",4101H)
   Call U.IMPRIME("REPORTAR DE INMEDIATO SOPORTE POS",2100H)
   Call U.IMPRIME("ERROR ACCESO LOG DE TRANSACCIONES",4101H)
   Call U.IMPRIME("ERROR ACCESO LOG DE TRANSACCIONES",2100H)
   Call VISOR.AND.BORRAR("TERMINAL SIN NUMERACION FISCAL")					
   Call VISORES4690(1,"TERMINAL BLOQUEADA","SIN NUMERACION FISCAL",0,"L")
   Call U.IMPRIME(" ",4900H)
   Call TSHIECET
   Call TSHIECET
   Call TSHIECET
   Stop 
  EndIf																																			!
  Xlog$ = "R::"+SLOGNAME$																										! Nombre Tlog
  TS.ER.RETURN = -1
  Open Xlog$ AS 100 NOWRITE NODEL																						! Apertura Tlog
  If TS.ER.RETURN = -1 Then Begin																						! Proceso OK
  	 F% = -1																																!
     While (F% = -1)																												! Ciclo Infinito                 
      Lectura.Tlog:                                                         !
       TS.ER.RETURN = -1
       Read #100; Line Str.Log$																							! Lectura Tlog
       If TS.ER.RETURN <> -1 Then Begin																			! Falla lectura
       	  GoTo Fin.Tlog																											! Termina recorrido Tlog
       EndIf																																!
       If Len(Str.Log$) < 12 Then GoTo Lectura.Tlog													! Si basura en Tlog                
       Str.Log$ = Str.Log$ + ","																						!                                
       Q% = 1 																															! Reset Variable                 
       While (Q% < Len(Str.Log$))																						! Validar si String completos      
         P% = Match(",",Str.Log$,Q%) 																				! Encontrar delimitador coma (,)   
         If (P% - Q%) < 3 Then Begin 																				! Revisar falta de strings         
	         GoTo Lectura.Tlog		   																					! Lectura siguiente registro     
	       EndIf																															!                                
         B$ = Mid$(Str.Log$,Q%+1,(P%-Q%)-2) 																! Analiza cadena de String       
         B$ = B$ + ":" 																											! Suma un semi colon               
         Q% = P% + 1 																												! Prepara siguiente informacion    
         A% = Val(Unpack$(Left$(B$,1))) 																		! Determina tipo de String
         If A% = 0 Then Begin																							  ! Header trx 
         	Asc.Tmp.Apun% = 3																									! 
         	Xterm$ = ASIC.GETUNPK(B$,Asc.Tmp.Apun%)                						! numero de terminal
         	Xtrx$ = ASIC.GETUNPK(B$,Asc.Tmp.Apun%)                						! numero de trx
         	If Val(Xterm$) <> Val(TS.TERMINAL$) Then                         \! Si terminal diferente
         		 GoTo Lectura.Tlog																							! 
         EndIf																															!
         If A% = 99 Then Begin																							! User Data
         	Asc.Tmp.Apun% = 3																									! 
         	Xtmp$ = ASIC.GETUNPK(B$,Asc.Tmp.Apun%)                            ! numero de proyecto
         	If Xtmp$ = "65" Then Begin																				! UD numfiscal
             Xtmp$ = ASIC.GETUNPK(B$,Asc.Tmp.Apun%)                         ! Tipo factura
             Xtmp$ = ASIC.GETUNPK4(B$,Asc.Tmp.Apun%)                        ! Prefijo contingencia
             Gr.Fis.PrefFis1$ = Xtmp$
             Xtmp$ = ASIC.GETUNPK(B$,Asc.Tmp.Apun%)                         ! Documento generado contingencia
             If Val(Xtmp$) <> 0 Then \
                Gr.Fis.ConMem% = Val(Xtmp$)
             Xtmp$ = ASIC.GETUNPK(B$,Asc.Tmp.Apun%)                         ! doc inicial
             Xtmp$ = ASIC.GETUNPK(B$,Asc.Tmp.Apun%)                         ! doc final 
             Gr.Fis.ResolFis1$ = ASIC.GETUNPK(B$,Asc.Tmp.Apun%)             ! nro resolucion
             Xtmp$ = ASIC.GETUNPK(B$,Asc.Tmp.Apun%)                         ! Fecha inicial
             Xtmp$ = ASIC.GETUNPK(B$,Asc.Tmp.Apun%)                         ! Fecha final
             Gr.Fis.PrefFis2$ = ASIC.GETUNPK4(B$,Asc.Tmp.Apun%)             ! Prefijo fiscal
             Xtmp$ = ASIC.GETUNPK(B$,Asc.Tmp.Apun%)                         ! Documento generado fiscal
             If Val(Xtmp$) <> 0 Then \
                Gr.Fis.FisMem% = Val(Xtmp$)
             Xtmp$ = ASIC.GETUNPK(B$,Asc.Tmp.Apun%)                         ! doc inicial
             Xtmp$ = ASIC.GETUNPK(B$,Asc.Tmp.Apun%)                         ! doc final 
             Gr.Fis.ResolFis2$ = ASIC.GETUNPK(B$,Asc.Tmp.Apun%)             ! nro resolucion
             Xtmp$ = ASIC.GETUNPK(B$,Asc.Tmp.Apun%)                         ! Fecha inicial
             Xtmp$ = ASIC.GETUNPK(B$,Asc.Tmp.Apun%)                         ! Fecha final
                
         		 GoTo Lectura.Tlog
         	EndIf																															!	fin numfiscal
         EndIf																															! fin user data
       Wend 																																! Fin analisis tlog              
     Wend 																																	! Fin del ciclo                  
  EndIf																																			!
  Fin.Tlog:
    Close 100 
End Sub 																																		! 
