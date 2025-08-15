!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMDTRPI.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Detalle venta producots para la  *
!                integración con RAPPI - Ecuador  *
!**************************************************
! Modificaciones:
! ----- Version 1.0 10 Junio 2022 -----------------

%ENVIRON T		                 																							! Ambiente de terminal

! Contiene la informacion de descuentos ya aplicados
! se define global para simplificar su uso al distribuir los descuentos
String global desAplic(1)

String   Global  Gr.Motor2.Items(2),                                       \! Dsctos motor promociones
                 IR.USERDATA$ 																						  ! Datos adicionales Item
Integer*4 Global Asc.Tmp.Apun%																							! 
Integer*4 Global UE.TABLAIVA(1)                                             ! Tarifas de IVA
Integer*4 Global Gr.Iva.PtgIcui%,                                          \! Porcentaje ICUI
                 Gr.Motor2.Xpos%
Integer   Global SL.END																											! Apuntador String 
Integer*4 Global GR.TOTAL.DSC%,																						 \! Total descuentos 
                 GR.TOTAL.TAX%																							! Total impuestos 

String    Global Gr.Rappi.Compra$,																				 \!
                 Gr.Rappi.Imptos$,               												   \!
                 Gr.Rappi.Dsctos$																						!

%INCLUDE EAMTSWKG.J86          																							! working storage
%INCLUDE EAMTRANS.J86          																							! Variables de transacciones
%INCLUDE EAMTERMS.J86          																							! Variables de terminales
%INCLUDE EAMTOPTS.J86	         																							! Variables Terminal Options
%INCLUDE EAMITEMR.J86          																							! Variables EAMITEMR
%INCLUDE RESPROD.J86																												! Arreglo descuentos motor NUO

%INCLUDE EAMTSXHC.J86          																							! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          																							! Rutinas de Assembler
%INCLUDE RECATSSU.011          					      															! RUTINAS GENERICAS APLICACION     

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External                  !
  STRING    KEY$       !* KEY of the record to be read.                    *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                       *!
  STRING    DATA$      !* String to hold the data read from file.          *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                             *!
End Sub                                                                     !

Sub IRRFEC.SPLIT1 (RECORD$) EXTERNAL
  STRING    RECORD$    !* The record (including the key) read from file    *!
End Sub 

!-----------------------------------------------------------------------
! formatea un entero considerando 2 decimales, sin separador de miles
! separador decimal "."
! ejemplos 3 -> 0.03  123456789 -> 1234567.89
!-----------------------------------------------------------------------
Function formateaMonto( numero ) External
   String    formateaMonto
   Integer*4 numero
End Function 

Function  NUOTPROMresPromociones external
   String NUOTPROMresPromociones
End Function 

Sub limpiaDescuentos external 
End Sub 

Sub limpiaFactura external
End Sub 

!-----------------------------------------------------------------------------
! Aplica el descuento a cada linea de venta con el descuento indice r
!-----------------------------------------------------------------------------
Sub aplicaDescuentos( r ) External 
   Integer*2 r
End Sub 


Sub Flag.Tax.Iva(Xindi1%)	Public  																					! Calculo flag de iva
Integer*1 Xindi1%, Xtarifa%																									!
Integer*4 XX%
String    Xbin$																															!
  IF (Xindi1% AND 01H) THEN                                                \!
      Xbin$ = "1" ELSE Xbin$ = "0"                 													!
  FOR I% = 1 TO 7                                      											!
       Xindi1% = SHIFT(Xindi1%,1)                  													! Inicializa sig. BIT
       IF (Xindi1% AND 01H) THEN Xbin$="1"+Xbin$                           \!
           ELSE Xbin$ ="0"+Xbin$                   													!
  NEXT I%                                              											!	
  Xtarifa% = 8																															!
  If LEFT$(Xbin$,4) = "1000" THEN                  												 \! Si hay Indic Plan A
                      Xtarifa% = 1		                                     \! indica Plan = A
  Else                                                                     \! si no,
  If LEFT$(Xbin$,4) = "0100" THEN                  												 \! Si hay Indic Plan B
                      Xtarifa% = 2		              											 \! indica Plan = B
  Else                                                                     \! si no,
  If LEFT$(Xbin$,4) = "0010" THEN                  												 \! Si hay Indic Plan C
                      Xtarifa% = 3		              											 \! indica Plan = C
  Else                                               											 \! si no,
  If LEFT$(Xbin$,4) = "0001" THEN                  												 \! Si hay Indic Plan D
                      Xtarifa% = 4		              											 \! indica Plan = D
  Else                                               											 \! si no,
  If LEFT$(Xbin$,4) = "1100" THEN                  												 \! Si hay Indic Plan AB
                      Xtarifa% = 5		              											 \! indica Plan = E
  Else                                               											 \! si no,
  If LEFT$(Xbin$,4) = "0110" THEN                  											   \! Si hay Indic Plan BC
                      Xtarifa% = 6		              											 \! indica Plan = F
  Else                                               											 \! si no,
  If LEFT$(Xbin$,4) = "0011" THEN                  												 \! Si hay Indic Plan CD
                      Xtarifa% = 7		              											 \! indica Plan = G
  Else                                               										   \! si no,
                      Xtarifa% = 8		               												! indica 0% 
   TS.TEMP1I1 = Xtarifa%																										!
   
End Sub 																																		! Fin calculo flag iva

Sub Carga.Iva.Tienda																												! Carga tarifas de iva
String Xdat1$
Integer*1 Xpos%
  Dim UE.TABLAIVA(8)
  UE.TABLAIVA(1) = 0
  UE.TABLAIVA(2) = 0
  UE.TABLAIVA(3) = 0
  UE.TABLAIVA(4) = 0
  UE.TABLAIVA(5) = 0
  UE.TABLAIVA(6) = 0
  UE.TABLAIVA(7) = 0
  UE.TABLAIVA(8) = 0
  Xpos% = 1
  TS.ER.RETURN = -1
  Open "R::$ARESTAB" As 80 NOWRITE NODEL
  If TS.ER.RETURN NE -1 THEN BEGIN
     Call VISORES4690(1,"ERROR APERTURA IVA","...",1500,"C")
     GoTo SALIDA.IVA
  EndIf 
  If End # 80 Then Salida.Iva
  While (1)															  																  ! Recorre archivo                  
        Read #80; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[IMPUESTOS]" Then Begin		 	  		        					! Parametros pago Rappi
         Read #80; TS.TEMP1$																								! Lectura registro                 
         Xdat1$ = Mid$(TS.TEMP1$,30,1)      			    					            ! Proyecto Activo 0. No 1 Si
         Read #80; TS.TEMP1$																								! Lectura registro                 
         Xdat1$ = Mid$(TS.TEMP1$,30,2)      			    					            ! Tarifa de impuesto iva general
         UE.TABLAIVA(Xpos%) = Val(Xdat1$)																	  ! Almacena iva
         GoTo Salida.Iva                                         					  ! Sale del proceso
        EndIf																																!
  Wend 																																		  ! Fin recorrido archivo
SALIDA.IVA:
  Close 80

End Sub 																																		! fin carga tarifas

Function LISTA.TRX.RAPPI Public 																						! Datos para envio de trx rappi
  Integer*4 PRIC%, TOT4%, XI%, XInd1%, XInd2%, X.J%, Xqty%, Xfind%,        \!
            Gr.Xcont%, Xpric%,Xtv%(1), Xpag%, XQlin%, XQItm%, Xchg%					!
  Integer*2 Xsgn%, Xpos%																										!
  String    Xtmp$, Xitm$, XPric$, Xdummy$, Xkey$, Xbasura$, X.Ean$, Xcot$   !
  String    Gr.Tmp.Itm$(2), Xbuff$, Xiva$, Xund$, X.Flag$, Gr.Impoc$,      \!
            XDepto$, XPvp$, Xicui$, XDepto1$, LISTA.TRX.RAPPI
  String    Xtotc$, XtotD$, XtotT$, Xtax$
  Gr.Xcont% = 0 																														! Numero de Items
  Call Carga.Iva.Tienda																											! Carga tarifas de iva para tienda
  Dim Gr.Tmp.Itm$(1000,10)				 																			    ! Lista de Items vendidos
  Dim Xtv%(70)																															! Formas de pago 
  X.J% = 0 : XQlin% = 0 : XQItm% = 0	: Xchg%	= 0														!
  GR.TOTAL.TAX% = 0 : Xitm$ = ""																						!
  For X.J% = 1 TO SL.END                    																! FOR ALL StringS
    H$ = READ.SL.STR$(X.J%)                 																! GET String
    If Len(H$) > 5 Then  Begin                                              ! ASSURE GOOD String
     If Asc(H$)  = 1 Then Begin             																! ITEM ENTRY String
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
        Call Flag.Tax.Iva(XInd1%)																		  			! Retorna flag iva
        Xiva$ = Str$(UE.TABLAIVA(TS.TEMP1I1))                               ! Tasa de IVA
        Xitm$ = Str$(GETN4( IR.USERDATA$, 16 ))                             ! Codigo interno santa maria 
        If (IR.INDICAT0 And 40H) Then Xund$ = "KL" Else Xund$ = "UN"        ! Si pesable o unitario
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
           Gr.Tmp.Itm$(Gr.Xcont%,0) = Xitm$                                 ! Codigo Interno Santa Maria
           Gr.Tmp.Itm$(Gr.Xcont%,1) = Str$( Val(Xpric$) * Xsgn%)            ! Precio de Venta
           Gr.Tmp.Itm$(Gr.Xcont%,2) = Str$( Xsgn% * 1 )                     ! Cantidad Vendida
           Gr.Tmp.Itm$(Gr.Xcont%,3) = Xiva$                                 ! Tarifa del IVA
           Gr.Tmp.Itm$(Gr.Xcont%,4) = X.flag$                               ! Excento o Excluido
           Gr.Tmp.Itm$(Gr.Xcont%,5) = Gr.Impoc$                             ! Impoconsumo
           Gr.Tmp.Itm$(Gr.Xcont%,6) = Xund$                                 ! Si es pesable o unitario
           Gr.Tmp.Itm$(Gr.Xcont%,7) = "AA"                      					  ! Descripcion Item
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
    EndIf
  NEXT X.J%
!--- Consulta dsctos aplicados en compra  
  X.J% = 0 : XI%  = 0
  Call aplicaDescuentos(0)
  For XI% = 0 To resProdNum																									! Total Promociones

!   Write #34 ; "plu " + unpack$(resProdCod$(XI%)) + chr$(10)
!   Write #34 ; "sku " + unpack$(resProdSku$(XI%)) + chr$(10)
!   Write #34 ; "vta " + Str$(resProdVen%(XI%)) + chr$(10)
!   Write #34 ; "des " + Str$(resProdDes%(xi%)) + chr$(10)

   For X.J% = 1 To Gr.Xcont%																								! Total productos 
    If Val(unpack$(resProdSku$(XI%))) = Val(Gr.Tmp.Itm$(X.J%,0)) Then Begin ! Item encontrado
       Gr.Tmp.Itm$(X.J%,9) = Str$(resProdDes%(xi%) / 100)								    ! Total Descuento
    EndIf																																		! Fin Item Encontrado
    resProdDes%(xi%) = 0
   Next X.J% 																																! Fin Total productos
  Next XI% 																																	! Fin total promociones

  X.J% = 0 : XItm$ = "" : XQItm% = 0
  For X.J% = 1 To Gr.Xcont%																									! Productos vendidos
      XTMP$ = "00"																													! Signo operacion
      XPric$ = Str$(Val(Gr.Tmp.Itm$(X.J%,1))	- Val(Gr.Tmp.Itm$(X.J%,9)))		! Toma venta total
      Xund$  = Gr.Tmp.Itm$(X.J%,2)																					! Qty total venta
      If Val(Xund$) = 0 Then GoTo Next.Itm.Vta															! No hay Items vendidos
      If Val(Xpric$) < 0 Then Begin																					! Si venta negativa
      	 Xpric$ = Str$( Val(Xpric$) * -1)																		! Invierte signo
      	 Xund$  = Str$( Val(Xund$) * -1 )																		! Invierte signo
      	 Xtmp$ = "01"																												! Reporta Item negativo
      EndIf																																	!
      If Gr.Tmp.Itm$(X.J%,6) = "UN" Then Begin															! Si venta por unidad
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
        Xpvp$ = formateaMonto( Val(Xpvp$) )																	! Formatea valor unitario 2 decimales
        Xtax$ = Gr.Tmp.Itm$(X.J%,3)																					! Tarifa impuesto
        Xtax$ = Str$(round( 100.0 * Val(Xtax$) * Val(Xpvp$) / ( 100 + Val(Xtax$) ), 0, 0 ))
        GR.TOTAL.TAX% = GR.TOTAL.TAX% + ( Val(Xtax$) * Val(Xund$) )					! Total impuestos 
        Xtax$ = formateaMonto( Val(Xtax$) )																	! Formatea valor unitario 2 decimales
        Xitm$  = Xitm$ + (Gr.Tmp.Itm$(X.J%,0))                           + \! SKU producto
               ":" + (Xund$)                                             + \! Qty Vendida          
               ":" + (Gr.Tmp.Itm$(X.J%,6))                               + \! Unitario o pesado
               ":" + (Xpvp$)                                             + \! Precio unitario venta
               ":" + (Xtax$) + ","                                          ! Tarifa del impuesto
        TOT4% = TOT4% + Val(Gr.Tmp.Itm$(X.J%,9))														! Total descuentos en la trx
      EndIf																																	!
    Next.Itm.Vta:
    
  Next X.J%																																	! Fin items vendidos 
  Xtmp$ = Str$(TOT4%)																												! Total descuentos 
  X.J% = 0 : Xpric$ = ""																										!

  Xtotc$ = formateaMonto(TS.TOTALS(0,0,0) - GR.TOTAL.DSC%)									! Total de la compra
  XtotD$ = formateaMonto(GR.TOTAL.DSC%)																			! Total de los descuentos 
  XtotT$ = formateaMonto(GR.TOTAL.TAX%)																			! Total de los impuestos 

  Gr.Rappi.Compra$ = Str$((TS.TOTALS(0,0,0) - GR.TOTAL.DSC%))
  Gr.Rappi.Imptos$ = Str$(GR.TOTAL.TAX%)
  Gr.Rappi.Dsctos$ = Str$(GR.TOTAL.DSC%)

  LISTA.TRX.RAPPI = Xtotc$+":"+XtotD$+":"+XtotT$+";"+Xitm$ + ";"  	        ! Retorna trx 

End Function 																																!
