!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMPROM2.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Abril del 2016                   *
!Observaciones : Motor Promociones Version 2      *
!**************************************************
! Modificaciones:
!
! Mod 13 Sep 2018
! Se ajusta el proceso de reintegro con venta de otros 
! productos.
! Ajustado por Grupo Retail - OVS 
!--------------------------------------------------
! Mod 15 Nov 2018
! Se ajusta lista tipificaciones con una letra
! para aplicacion de promociones.
! Ajustado por Grupo Retail - OVS 
!--------------------------------------------------
! Mod 12 Jun 2019
! Se ajusta modulo para que en procesos de entrenamiento
! no se genere la impresión de los mensajes promocionales
! solicitado por Comfandi - Andrea Guzman
!--------------------------------------------------
! Mod 17Nov2021
! Se ajusta grabacion de dato negativo en String 04
! que daña los totales de la contabilidad del sistema
! Ajustado por Grupo Retail - OVS 
!--------------------------------------------------
! Mod 10Nov2022
! Ajustes proceso para reporte medio de pago 
! tarjeta visa comfandi
! Ajustado por Grupo Retail - OVS 
!--------------------------------------------------
! Mod 30May2023
! Se ajusta problema reportado en la aplicación de un
! descuento con pago parcial y anulación de producto con 
! beneficio generando descuadre de planilla.
! Ajuste por falla en recuperación de descuento en proceso
! de recuperación de transacción.
! Ajustado por Grupo Retail - OVS 
!--------------------------------------------------
! Mod 28Feb2024
! Se controla la inicialización de las variables 
! Gr.Motor2.Xpos% y Gr.Motor2.Items para evitar 
! grabación de basura en los UD de FECO
! Ajustado por Grupo Retail - OVS 
!--------------------------------------------------
! Mod 15Ago2024
! Se controla proceso de nota credito para evitar 
! que se ejecuten notas crédito.
! Ajustado por Grupo Retail - OVS 
!--------------------------------------------------


%ENVIRON T																																	! Ambiente terminal

Integer*1 Global Gr.Motor2.Ok%,																						 \! Proceso motor promociones
                 Gr.Motor2.Msg%,																					 \!
                 Gr.Motor2.Apl%,																					 \! Ctrl aplicacion promociones
                 Gr.Motor2.Fileok%,                                        \!
                 Gr.Motor2.AplTot%,                                        \! Control Aplicacion total
                 Gr.Motor2.DctApl%,                                        \!
                 Gr.NoTax.Aplicado%,                                       \! Dia no IVA
                 Gr.Motor2.Refund%,                                        \! En proceso de reintrego
                 Asic.Detalle%																							!
Integer*2 Global Gr.Motor2.Ses1%, 																			   \!
                 Gr.Motor2.Ses2%,                                          \!
                 Gr.Motor2.Ses3%                                            !
Integer*4 Global Gr.Motor2.VlrDs%,																				 \! Monto total descuento
                 Gr.Motor2.Total%,                                         \!
                 Gr.Motor2.Xpos%,                                          \!
                 Gr.Motor2.NroSim%,                                        \!
                 Gr.Motor2.Bol%,                                           \!
                 Gr.Motor2.OldDscto%,                                      \!
                 Gr.Motor2.TotalPagos%,                                    \!
                 Gr.Motor2.Baldue%,                                        \!
                 Gr.Motor2.Dsto44%,                                        \!
                 Gr.NoTax.Itm%,                                            \! Total items no tax
                 UE.TOTALS(1),                                             \!
                 Asc.Tmp.Apun%																							!
String    Global Gr.Motor2.LstPago$,																			 \! Teclas formas de pago
                 Gr.Motor2.Descrip$,                                       \! Descriptor tiquete
                 Gr.Motor2.GrpDscto$,																			 \! Grupo dscto promocion
                 Gr.Motor2.UltPago$,                                       \! Ultimo pago
                 Gr.Lcl.Tipifica$,																				 \! Tipíficaciones Cliente
                 Gr.Motor2.Items(2),                                       \! Lista items con dscto
                 Gr.Motor2.LstM$,                                          \!
                 Gr.Motor2.Appl$,                                          \! Codigo aplicacion
                 Gr.Motor2.Func$,                                          \! Codigo funcion
                 GrPromSim$(1),                                            \! Lista promociones
                 Gr.Lcl.Clte$,																						 \! ID del cliente
                 Gr.Notax.LstItm$(2),                                      \! Items dia Sin iva
                 Gr.Lcl.Dir$,																					     \!
                 Gr.Lcl.IdClte$,                                           \! Tipo cliente fiscal
                 Gr.Lcl.Tel$																								!

!-- Variables control aplicaciones Comfandi
Integer*2 GLOBAL UE.RECASRV.QTY%																						! Modulo de recaudos
Integer*1 Global Gr.Mrm.Cap%                                                ! Modulo remisiones
Integer*1 Global  Gr.Fis.TrjVisa%																						! Si pago TC Visa Comfandi
String    Global IR.USERDATA$																								! Extension maestra productos
Integer*1 Global Gr.NtaCrd.InTrx% 																					! Proceso notas credito
Integer*4 Global Gr.NtaCrd.Discount%																				! Total dscto NC

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

Sub TSBDEC01 EXTERNAL            ! take a balance due
End Sub

Sub TSBDEC02 EXTERNAL            ! discounts
End Sub

Sub TSPREC01 EXTERNAL          																							! print routine
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

Sub PROMO.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:MO" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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

Sub Flag.Item.Iva(Xindi1%)	Public  																				! Calculo flag de iva
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
   
  If Gr.NoTax.Aplicado% = -3 Then Begin																	    ! Beneficio IVA Aplicado
   For XX% = 1 To Gr.NoTax.Itm%
 	   If Val(Gr.Notax.LstItm$(XX%,0)) = Val(UNPACK$(TS.TEMP2$)) Then Begin   !
 	   	  TS.TEMP1I1 = 7																											! Reporta IVA Excento
 	   EndIf
 	 Next XX%
 	EndIf
   
   
   
End Sub 																																		! Fin calculo flag iva

Sub Almacena.Promocion(Gopt$)																								! Almacena items promocionados
String Gopt$, Gsgn$
Integer*1 Xiva%
Integer*4 Xi%

If Gopt$ = "00" Then Begin																									! Si aplica promocion
   Gsgn$ = "03"
   If Gr.Motor2.VlrDs% < 0 Then Begin
   	 Gr.Motor2.VlrDs% = (Gr.Motor2.VlrDs% * -1)
     Gsgn$ = "04"
   EndIf
   
!    !-- Aplica nuevo descuento                                                                                         
!   	SL.END = SL.END + 1                                                   ! Aumenta apuntador de String
!	  SL.STR$(SL.END) = Pack$(Gsgn$)+":"+Pack$(Gr.Motor2.GrpDscto$) + ":" +  \! Almacena String de descuento
!		                  Pack$("00")+ ":" + Pack$(Str$(Gr.Motor2.VlrDs%))  +  \!
!		                  ":"+Pack$("00") + ":" 															  !

  For Xi% = 1 To Gr.Motor2.Xpos%																						!
   Xiva% = Val(Gr.Motor2.Items(Xi%,4))																			!
   TS.TEMP1$ =  Pack$(Gr.Motor2.Items(Xi%,0))          +                   \! Codigo del producto
                ":" + Pack$(Gr.Motor2.Items(Xi%,1))    +                   \! Valor del descuento
                ":" + Pack$(Gr.Motor2.Items(Xi%,2))    +                   \! Codigo de la promocion
                ":" + Pack$(Gr.Motor2.Items(Xi%,4))    +                   \! flag de iva
                ":" + Pack$(Gr.Motor2.Items(Xi%,5))    +                   \! Codigo del departamento
                ":" + Pack$(Gr.Motor2.Items(Xi%,6))    +                   \! Estructura comercial 
                ":" + Pack$(Gr.Motor2.Items(Xi%,7))    +                   \! Signo de la operacion Item
                ":" + Pack$(Gr.Motor2.Items(Xi%,8))    +                   \! Total venta items          
                ":" + Pack$(Gr.Motor2.Items(Xi%,9))                         ! cantidad vendida items     
   Call Grabacion.String.Usuario2("97",TS.TEMP1$)                  					! Graba String usuario  
   If Gr.Motor2.Items(Xi%,7) = "00" Then                                   \!
      Ue.Totals(Xiva%) = Ue.Totals(Xiva%) - Val(Gr.Motor2.Items(Xi%,1)) Else \! Retira dscto del iva
      Ue.Totals(Xiva%) = Ue.Totals(Xiva%) + Val(Gr.Motor2.Items(Xi%,1))     ! Retira dscto del iva
  Next Xi%
EndIf																																				! fin aplica promocion


End Sub 

Function Gr.Listas.Item(Xplu$)
String   Gr.Listas.Item, Xlst$, Xplu$, xtmp$, Xkyd$
Integer*4 Xi%
  Xlst$ = ""
  Xkyd$ = Pack$(Right$("000000000000"+Xplu$,12))														! Sku para busqueda
  TS.ER.RETURN = -1																													! Control errores
  READ FORM "C6 C502";#61 Key Xkyd$; Xkyd$, Xtmp$            	! Busca lista Item
  If TS.ER.RETURN = -1 Then Begin																						!
   For Xi% = 1 To Len(Xtmp$)																								! Hasta la longitud de la cadena
     If Mid$(Xtmp$,Xi%,1) <> " " Then                                      \! 
        Xlst$ = Xlst$ + Mid$(Xtmp$,Xi%,1)																		! Arma lista Sin basura
   Next Xi%																																	!
  EndIf Else Xlst$ = " "																										! Si no hay listas reporta blanco
  Gr.Listas.Item = Xlst$
End Function 

Function Apl.Dscto.Compra
  Integer*1 Xfnd%, Grind1%, Xsgn%																						!
  Integer*4 Apl.Dscto.Compra, PRIC%, Xvta%, Xpos%, Xr%, XtotItm%, Xqty%			!
  Integer*4 Vta.Tot%, Xdscto%, Gri%, Grmenor%, Tva%, Tvb%, Upa%, Upb%				!
  Integer*4 Xcont%, Xtv%(1), Xpag%, Xloc%, Xs%
  String X.TMP$, Xpago$, Xitems$(2), Xd1$, Xd2$, Xd0$, GrPromo$, Ykey$			!
  String Xdummy$, Xitm1$, Xitm2$, Xitm3$, Xprc1$, Xprc2$, Xprc3$, Xact$			!
  String GrPlu$, Xitm$, Xtrama$, XRtrama$, XLista$(1), Xtc$, XTEMP4$				!
  String Xplu$, Xdc$, Xpr$, Xlmsg$, Xlitm$, Xsnd$, Xllega$, Xfr$, Xqty$
  String Xcomer$, Xsgn$
  Real   Peso%
  Xvta% = 0                                            											!
  XtotItm% = 0
  Gr.Motor2.TotalPagos% = 0
  Dim Xitems$(1000,5)																												!
  Dim XLista$(1000)
  Dim Xtv%(70)
  
  Gr.Motor2.Xpos% = 0
  Dim Gr.Motor2.Items(1000,9)
  
  Call VISORES4690(1,"VALIDA PROMOCIONES","ESPERE POR FAVOR",0,"L")         ! Msg Alerta
  For I% = 1 TO SL.END                                 											! FOR ALL StringS
    H$ = READ.SL.STR$(I%)                              											! GET String
    If LEN(H$) > 5 Then Begin                          										  ! ASSURE GOOD String
     If ASC(H$) = 1 Then Begin                         											! ITEM ENTRY String
     	  Asc.Tmp.Apun% = 3																										!
     	  X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)      												! Plu
     	  X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)      												! Precio
     	  X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)      												! Departamento
     	  X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)      												! Family 
     	  X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)      												! Indicat1
     	  Grind1% = Val(X.Tmp$)
        X.TMP$ = ASIC.GETUNPK(H$,3)	 				           											! Tomo informacion del item vendido
        L% = ASC(RIGHT$(H$,1)) / 10H                   											! GET ITEM TYPE       
        If L% = 0 OR L% = 8 Or L% = 2 Then Begin                            ! Item SALE OR CANCEL              
           PRIC% = 0                                                        ! ASSUME NO PRICE                  
           K% = MATCHB(":",H$,3) + 1                                        ! START OF PRICE                   
           J% = MATCHB(":",H$,K%)                                           ! END OF PRICE                     
           If J% > K% Then Begin                                            ! PRICE FOUND
             PRIC% = PACKBIN4(H$,K%-1,J%-K%)                                ! GET PRICE
             Xqty% = 1																											! DEFAULT QTY 1
             If (L% = 8 Or L% = 2) Then PRIC% = 0 - PRIC% : Xqty% = -1      ! INVERT PRICE And QTY For VOID
             Xr% = 0 : Xpos% = 0 : Xfnd% = 0															  ! Init variables
             For Xr% = 1 To XtotItm%																				! Total Items Almacenados
              If Val(X.tmp$) = Val(Xitems$(Xr%,0)) Then Begin								! Item Encontrado
           	     Xpos% = Xr%																								! Ubicacion en vector
           	     Xfnd% = -1																									! Localizado
              EndIf																													!
            Next Xr%																												! Fin barrido trx
            If Xfnd% = 0 Then Begin 																				! Si no encontrado
         	     XtotItm% = XtotItm% + 1 																			! Actualiza apuntador
 	             IR.ITEMCODE$ = Pack$(Right$("000000000000"+X.Tmp$,12))       ! Ajusta dato
 	             TS.ER.RETURN = -1																						!
               %INCLUDE EAMIRRD3.J86																				! Lectura eamitemr
               GrPlu$ = X.Tmp$																							!
         	     Xitems$(XtotItm%,0) = X.tmp$																	! Asigna Plu
         	     Xitems$(XtotItm%,1) = Str$(PRIC%)														! Asigna Precio
         	     Xitems$(XtotItm%,4) = ASC.IR.NDEPTO$													! Estructura comercial
         	     Xitems$(XtotItm%,5) = Gr.Listas.Item(X.tmp$)                 ! Lista de items
         	     If (IR.INDICAT0 And 40H) Then Begin                         \! Si pesable
         	     	  Xitems$(XtotItm%,2) = Str$(0)													    ! Asigna Qty
         	     	  Xitems$(XtotItm%,3) = "P"  
         	     EndIf Else Begin 												                   \! Item Unitario
         	     	  Xitems$(XtotItm%,2) = Str$(Xqty%)													! Asigna Qty
         	     	  Xitems$(XtotItm%,3) = "U"                      						! Item Unitario	
         	     Endif
         	     Xpos% = XtotItm%																							! Asigna posicion 
            EndIf Else Begin																								! Si encontrado
         	     Xitems$(Xpos%,1) = Str$(Val(Xitems$(Xpos%,1)) + PRIC%)    		! Calcula total compra
         	     Xitems$(Xpos%,2) = Str$(Val(Xitems$(Xpos%,2)) + Xqty%)       ! Aumenta cantidad vendida
            EndIf																														! Fin almacenamiento items trx
           EndIf    																												!
        EndIf                                                               ! PRICE FOUND                      
     EndIf                                                                  ! ITEM SALE OR CANCEL              

     If ASC(H$) = 2 Then Begin                          										! ITEM ENTRY Extend
     	  Asc.Tmp.Apun% = 3 																								  ! Init apuntador String 
     	  If Xitems$(Xpos%,3) = "U" Then                                     \! Solo para items unitarios
     	     Xitems$(Xpos%,2) = Str$(Val(Xitems$(Xpos%,2)) - Xqty%)           ! Ajusta cantidad
        X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! Mpgroup
        X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! DealQ
        X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! SaleQ
        X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! SalePric
        X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! QtyorWeitgh
        X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! QtyorWeitgh
        If Pric% < 0 Then X.tmp$ = "-" + X.tmp$															!
     	  Xitems$(Xpos%,2) = Str$(Val(Xitems$(Xpos%,2)) + Val(X.tmp$))        ! Aumenta cantidad vendida
     EndIf																																	! Fin Extension Item
     If ASC(H$) = 5 Then Begin                          										! Forma de pago
     	  Asc.Tmp.Apun% = 3 																								  ! Init apuntador String 
     	  X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! Tipo y variedad
     	  Xpag% = Val(X.tmp$)
     	  X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! Valor Pagado
     	  Xtv%(Xpag%) = Xtv%(Xpag%) + Val(X.Tmp$)
     EndIf     																															! fin forma de pago
     If ASC(H$) = 6 Then Begin                          										! Anulacion Forma de pago
     	  Asc.Tmp.Apun% = 3 																								  ! Init apuntador String 
     	  X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! Tipo y variedad
     	  Xpag% = Val(X.tmp$)
     	  X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! Valor Pagado
     	  Xtv%(Xpag%) = Xtv%(Xpag%) - Val(X.Tmp$)
     EndIf     																															! fin Anulacion forma de pago
     If ASC(H$) = 153 Then Begin                         										! User data 
     	  Asc.Tmp.Apun% = 3 																								  ! Init apuntador String 
     	  X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! numero del proyecto
     	  If X.tmp$ = "20170404" Then Begin																		! Descuento Abbot
           X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)												  ! Ean del beneficio
           X.tmp$ = Left$(X.tmp$,12)                                        ! Elimina digito de chequeo
           IR.ITEMCODE$ = Pack$(Right$("000000000000"+X.Tmp$,12))       		! Ajusta dato
           TS.ER.RETURN = -1																								!
           %INCLUDE EAMIRRD3.J86																						! Lectura eamitemr
           Xqty% = Val(Asic.Getunpk(H$,Asc.Tmp.Apun%))										  ! cantidad
           Pric% = Val(Asic.Getunpk(H$,Asc.Tmp.Apun%))										  ! valor beneficio
           X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)												  ! token
           Xsgn% = Val(Asic.Getunpk(H$,Asc.Tmp.Apun%))										  ! signo
     	  	 X.tmp$ = Str$(Val(Unpack$(IR.SALEQUAN$ + IR.SALEPRIC$)))     		! Plu del producto
           Xr% = 0 : Xpos% = 0 : Xfnd% = 0															    ! Init variables
           For Xr% = 1 To XtotItm%																				  ! Total Items Almacenados
              If Val(X.tmp$) = Val(Xitems$(Xr%,0)) Then Begin								! Item Encontrado
           	     Xpos% = Xr%																								! Ubicacion en vector
           	     Xfnd% = -1																									! Localizado
              EndIf																													!
           Next Xr%																												  ! Fin barrido trx
     	  	 If Xfnd% = -1 Then Begin																					! Producto encontrado
     	  	 	 If Xsgn% = 0 Then Begin
         	     Xitems$(Xpos%,1) = Str$(Val(Xitems$(Xpos%,1)) - PRIC%)    		! Calcula total compra
         	     Xitems$(Xpos%,2) = Str$(Val(Xitems$(Xpos%,2)) - Xqty%)       ! Reduce cantidad vendida
         	   EndIf Else Begin
         	     Xitems$(Xpos%,1) = Str$(Val(Xitems$(Xpos%,1)) + PRIC%)    		! Calcula total compra
         	     Xitems$(Xpos%,2) = Str$(Val(Xitems$(Xpos%,2)) + Xqty%)       ! ajusta cantidad vendida
         	   EndIf
     	  	 EndIf																														!
     	  	 
     	  EndIf																																! Fin dscto abbot
     EndIf																																	! Fin User Data
     
    EndIf                                                                   ! Item ENTRY Extend       
  Next I%                                                                   ! NEXT String                      
  I% = 0																																		!
  Xcont% = 0																																!
  
  For I% = 0 To XtotItm%																										! Total de items
    If Val(Xitems$(I%,2)) <> 0 Then Begin						    								    ! Si Producto vendido
    	Xcont% = Xcont% + 1																										!

!    	If Gr.Motor2.Refund% = 0 Then Begin																		! Es una venta

    	If Val(Xitems$(I%,2)) >= 0 Then Begin                                 ! Es una venta
    		If Xitems$(I%,3) = "U" Then Begin                                   ! Si Item por unidad
    			 Xqty$ = Xitems$(I%,2)      																	    !
    		EndIf Else Begin                                                    ! Si es pesable
    			 Peso% = Val(Xitems$(I%,2))                                       ! 
    			 Peso% = Round(Peso%/1000,3,0)																		! Formatea el dato
    			 Xqty$ = Str$(Peso%)																							! 
    	  EndIf                                                               !
    	  XQTY$ = Translate$(XQTY$,",",".")
         XLista$(Xcont%) = Xqty$ + ":" +                                   \! Cantidad
                           Xitems$(I%,0) + ":" +                           \! Plu
                           Right$(Xitems$(I%,4),15) + ":" +                \! Estructura comercial
                           Xitems$(I%,1) + ":" +                           \! Total de venta
                           Xitems$(I%,5)																	  ! Lista promociones del Item
      EndIf Else Begin																											! En proceso de reintegro
    		If Xitems$(I%,3) = "U" Then Begin                                   ! Si Item por unidad
    			 Xqty$ = Str$( Val(Xitems$(I%,2)) * -1 )      								    !
    		EndIf Else Begin                                                    ! Si es pesable
    			 Peso% = Val(Xitems$(I%,2))                                       ! 
    			 Peso% = Round(Peso%/1000,3,0)																		! Formatea el dato
    			 Xqty$ = Str$(Peso% * -1)																					! 
    	  EndIf                                                               !
    	  XQTY$ = Translate$(XQTY$,",",".")

         XLista$(Xcont%) = Xqty$ + ":" +                                   \! Cantidad
                           Xitems$(I%,0) + ":" +                           \! Plu
                           Right$(Xitems$(I%,4),15) + ":" +                \! Estructura comercial
                           Str$(Val(Xitems$(I%,1))*-1) + ":" +             \! Total de venta
                           Xitems$(I%,5)																	  ! Lista promociones del Item
      EndIf
    EndIf																																		!
  Next I%																																		! Fin total items
  I% = 0																																		!
  Xtrama$ = ""																															!
!-- Lista de productos

  If Xcont% <= 0 Then Begin																									! No hay productos en compra
  	 !Call VISOR.AND.BORRAR("NO HAY PRODUCTOS    PARA PROMOCION...")
  	 Apl.Dscto.Compra = 0
  	 Exit Function 
  EndIf

  For I% = 1 To Xcont%																											!
    If I% < Xcont% Then                                                    \!	
       Xtrama$ = Xtrama$ + Xlista$(I%) + "," Else 		  									 \! Arma lista productos
       Xtrama$ = Xtrama$ + Xlista$(I%)																			!
  Next I%																																		!
  Xtrama$ = Xtrama$ + ";"																										!
!-- Lista de formas de pago
  I% = 0
  Xpr$ = ""
  Asc.Tmp.Apun% = 1																													!	
  TS.TEMP1$ = Asic.Getunpk4(Gr.Motor2.UltPago$,Asc.Tmp.Apun%)								! Ultimo pago capturado
  TS.TEMP2$ = Asic.Getunpk4(Gr.Motor2.UltPago$,Asc.Tmp.Apun%)								! Valor capturado
  Xtv%(Val(TS.TEMP1$)) = Xtv%(Val(TS.TEMP1$)) + Val(TS.TEMP2$)  +          \! Suma ultimo pago capturado
                         Gr.Motor2.OldDscto%                                ! mas ultimo descuento
  For I% = 11 To 70																													! Recorre lista formas de pago
    If Xtv%(I%) <> 0 Then Begin																							! Si pago presente
    	If Gr.Motor2.Refund% = 0 Then Begin																		! Es una venta
       Xpr$ = Xpr$ + Right$("00"+Str$(I%),2) + ":" + Str$(Xtv%(I%)) + ":"+","
       Gr.Motor2.TotalPagos% = Gr.Motor2.TotalPagos% + Xtv%(I%)             ! Total pagos
      EndIf Else Begin
       Xpr$ = Xpr$ + Right$("00"+Str$(I%),2) + ":" + Str$(Xtv%(I%)*-1) + ":"+","
       Gr.Motor2.TotalPagos% = Gr.Motor2.TotalPagos% + (Xtv%(I%)*-1)        ! Total pagos
      EndIf
    EndIf
  Next I%
  If Len(Xpr$) > 0 Then \
     Xpr$ = Left$(Xpr$,Len(Xpr$)-1)																					! Ajusta String
  Xtrama$ = Xtrama$ + Xpr$ + ";"             																! Entrega trama armada
!-- Datos del cliente
  If Len(Gr.Lcl.Tipifica$) > 0 Then Begin            											  ! Si CLF capturado
  	 Xtc$ = Translate$(Gr.Lcl.Tipifica$,"/",",")														!
  	 Xpr$ = ""
  	 For Xs% = 1 To Len(Xtc$)
  	   If Mid$(Xtc$,Xs%,1) <> " " Then                                     \! Arma String para validar
  	   	  Xpr$ = Xpr$ + Mid$(Xtc$,Xs%,1)																		! tipificaciones
  	 Next Xs%																																!
  	 Xtc$ = Xpr$
  	 
  	 Xtc$ = Right$(Xtc$,(Len(Xtc$)-1))
  	 Xtrama$ = Xtrama$ + Gr.Lcl.Clte$ + ";" +                              \! ID del cliente
               Xtc$ + ";"                                                   ! Tipificaciones del cliente
  EndIf Else Begin																													! Fin cliente
  	 Xtrama$ = Xtrama$ + " " + ";" +                                       \! ID del cliente
               " " + ";"                                                    ! Tipificaciones del cliente
  EndIf																																			!
  PROCESA.PROMOCION:
  If Xcont% <= 20 Then        																						 \! Hasta 20 productos
  	 Gr.Motor2.Func$ = "01" Else                                           \!
     Gr.Motor2.Func$ = "02"																									! Para mas de 20 productos en compra

  TS.TEMP2$ = Armar.Trama.Msg(Gr.Motor2.Appl$,Gr.Motor2.Func$,Xtrama$,"00","0001","123456") 		 	! Armar trama MSG
  Xsnd$ = DATE$ +":"+ Time$                                                 ! Fecha y hora del requerimiento  
  XRtrama$  = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)  		! Ejecuta Requerimiento
  
!-- Simulacion promociones  
!  TS.TEMP1$ = ASIC.DATOS$("TRAMA 1 O 2 ","")
!  If TS.TEMP1$ = "1" Then \
!   XRtrama$ = "7701046200000CONSULTA SATIFACTORIA                   3146:4440:3812900:,;" Else \
!   XRtrama$ = "7701046200000CONSULTA SATIFACTORIA                                       "
!--- Fin simulacion promociones  

  Xllega$ = DATE$ +":"+ Time$                                               ! Fecha y hora de la respuesta requerimiento  
  Call PROMO.AUDITORIA(ts.temp2$,Xrtrama$, Xsnd$, Xllega$)                ! Rastreo movimiento
  XTEMP4$ = Valida.Rta(Xrtrama$)																			      ! Valida rta entregada
  If Xtemp4$ <> "00" Then Begin 																						! Si falla proceso
  	 Call VISOR.AND.BORRAR("FALLA COMUNICACION  MOTOR PROMOCIONES")
  	 Call TSHIECET
  	 TS.TEMP1$ = ASIC.DATOS$("REINTENTA COMUNICACION","1.SI 0.NO")          !
  	 If TS.TEMP1$ = "1" Then GoTo PROCESA.PROMOCION                         !
  	 !Call VISOR.AND.BORRAR("NO HAY PROMOCIONES  PARA ESTA COMPRA.")
     Apl.Dscto.Compra = 0																										! Falla valor dscto
     !Gr.Motor2.Apl% = -1																									  !
     Exit Function 																													! 
  EndIf
  
  If Len(Xrtrama$) < 54 Then Begin																					! Si no hay promociones
  	 !Call VISOR.AND.BORRAR("NO HAY PROMOCIONES  PARA ESTA COMPRA.")
     Apl.Dscto.Compra = 0																										! Falla valor dscto
     !Gr.Motor2.Apl% = -1																									  !
     Exit Function 																													! 
  EndIf																																			!
  
  !Dim Xitems$(1000,5)																												!
  
  I% = 0
  Xdscto% = 0
  TS.TEMP4$ = ""
  For I% = 54 To Len(Xrtrama$)
    TS.TEMP4$ = TS.TEMP4$ + MID$(Xrtrama$,I%,1)
  Next I%
  I% = 0
  Xloc% = 1
  Gr.Motor2.Xpos% = 0
  Asc.Tmp.Apun% = 1
  Dim Gr.Motor2.Items(1000,9)
  Xlitm$ = Asic.Getunpk3(TS.TEMP4$,Xloc%)																		! Lista items con dscto
  Xlmsg$ = Asic.Getunpk3(TS.TEMP4$,Asc.Tmp.Apun%)														! Lista mensajes
  Xloc% = 1
  Asc.Tmp.Apun% = 1
  While Xloc% < Len(Xlitm$)																									! Recorre lista items con promo
    X.tmp$ = Asic.Getunpk5(Xlitm$,Xloc%)									    							! Lista Items
    Xloc% = Asc.Tmp.Apun%																										! Graba posicion
    Asc.Tmp.Apun% = 1																												!
    Xplu$  = Asic.Getunpk4(X.tmp$,Asc.Tmp.Apun%)								    				! Plu
    Xdc$   = Asic.Getunpk4(X.tmp$,Asc.Tmp.Apun%)								    				! Vlr dscto
    Xpr$   = Asic.Getunpk4(X.tmp$,Asc.Tmp.Apun%)								    				! Codigo promocion
    TS.TEMP1I1 = 8 
    TS.TEMP2$ = Xplu$ 												                              ! Codigo del producto
    TS.TEMP2$ = Pack$(Right$("000000000000"+TS.TEMP2$,12))				  				! Armo dato para busqueda
    Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)                					! Lectura Datos Itemr
    Call IRRFEC.SPLIT1( TS.TEMP1$ ) 			                           				! Entrega datos al eamitemr.j86
    Call Flag.Item.Iva(IR.INDICAT1)																					! Retorna flag iva
    Xcomer$ = LEFT$(IR.USERDATA$,16)																				! Estructura comercial
    Xsgn$ = "00"																														! Default signo 00 venta
    For I% = 1 To XTOTITM%																									! Busca signo Item
      If Val(Xplu$) = Val(Xitems$(I%,0)) Then Begin													!
      	 If Val(Xitems$(I%,2)) < 0 Then Xsgn$ = "01"                        ! Ventas anuladas
      	 Xvta% = Val(Xitems$(I%,1))                                         ! Total Venta
      	 If Xsgn$ = "00" Then 					    												       \! Si venta 
      	    XQty% = Val(Xitems$(I%,2)) Else 															 \! Cantidad vendida
      	    XQty% = Val(Xitems$(I%,2)) * -1																	! reintegro venta
      EndIf																																	!
    Next I%  																																!
    Gr.Motor2.Xpos% = Gr.Motor2.Xpos% + 1																		!
    Gr.Motor2.Items(Gr.Motor2.Xpos%,0) = Xplu$															! Plu del producto
    Gr.Motor2.Items(Gr.Motor2.Xpos%,1) = Xdc$ 															! valor dscto
    Gr.Motor2.Items(Gr.Motor2.Xpos%,2) = Xpr$ 															! codigo promocion
    Gr.Motor2.Items(Gr.Motor2.Xpos%,3) = IR.ITEMNAME$												! descripcion Item
    Gr.Motor2.Items(Gr.Motor2.Xpos%,4) = Str$(TS.TEMP1I1)										! flag iva
    Gr.Motor2.Items(Gr.Motor2.Xpos%,5) = Unpack$(IR.DEPARTME$)							! codigo del departamento
    Gr.Motor2.Items(Gr.Motor2.Xpos%,6) = Right$(Xcomer$,15)    							! Estructura comercial
    Gr.Motor2.Items(Gr.Motor2.Xpos%,7) = Xsgn$                 							! Signo de venta Item
    Gr.Motor2.Items(Gr.Motor2.Xpos%,8) = Str$(Xvta%)           							! total venta Item 
    Gr.Motor2.Items(Gr.Motor2.Xpos%,9) = Str$(XQty%)           							! cantidad vendida
    If Xsgn$ = "00" Then \
       Xdscto% = Xdscto% + Val(Xdc$) Else 																 \! 
       Xdscto% = Xdscto% - Val(Xdc$)
  Wend 
  
  !If Xdscto% < 0 Then Gr.Motor2.Refund% = -1                         
  
  Apl.Dscto.Compra = Xdscto% 																								! RETORNA DESCUENTO
  !Gr.Motor2.Apl% = -1																									  	!
  If Len(Xlmsg$) > 0 Then Begin																							! Mensajes para imprimir
     Gr.Motor2.LstM$ = Xlmsg$																								! Lista mensajes
     Gr.Motor2.Msg% = -1																										! Aplicacion mensajes
  EndIf
End Function 																																!

Function APLICA.DSCTOSV2																										! Calculo promociones
 String Xmsg$, Gsgn$
 Integer*4 Xdscto.Pago%, Xvlr%
 Integer*1 APLICA.DSCTOSV2


 !--- Mod 23Ene2023 solo para personas naturales aplica beneficios
 If USER.FBACT.READ <> 0 Then     																				 \! Cliente capturado
  If Val(Gr.Lcl.IdClte$) > 3 Then Begin                                     ! Es una entidad juridica
   		 Gr.Motor2.VlrDs% = 0																									!
       Gr.Motor2.Xpos% = 0																									! Elimina dsctos aplicados
       Dim Gr.Motor2.Items(1000,9)																					! Reinicia vector promociones
       APLICA.DSCTOSV2 = 0																									!                                         
    	 Exit Function                                                    		!                                      		
  EndIf																																			! Fin valida cliente
 
 If TS.IO.MOTORKEY = 81 Then Begin																					! Si tecla total
 	  Xdscto.Pago%  = Apl.Dscto.Compra                            	      		! Si hay descuentos en motor
    If Gr.Motor2.DctApl% = -1 Then                                         \! Dscto ya aplicado en la trx
    If Gr.Motor2.OldDscto% <> Xdscto.Pago% Then Begin												! Anula dscto aplicado
      TS.BD.DSPPARM = -1                              											! SHOW BD TO DISPLAY SUBTOTAL
      Call TSBDEC01                                   											! DISPLAY NEW SUBTOTAL
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
      TS.IO.KEYS(1) = 70
      TS.IO.DATA$(5) = Gr.Motor2.GrpDscto$
      TS.IO.KEYS(5) = 62
      TS.IO.MOTORKEY = 62
      Gr.Motor2.Dsto44% = (Gr.Motor2.OldDscto%)															! Valor descuento
      Call TSBDEC02                                   											! APLICA  DESCUENTOS
      TS.BD.DSPPARM = -1                              											! SHOW BD TO DISPLAY SUBTOTAL
      Call TSBDEC01                                   											! DISPLAY NEW SUBTOTAL
      Gr.Motor2.DctApl% = 0
      Gr.Motor2.OldDscto% = 0
      Gr.Motor2.Dsto44% = 0
      Gr.Motor2.VlrDs% = 0
    EndIf  

   	If Xdscto.Pago% = 0 Then Begin																					! No hay descuentos para aplicar
   		 Gr.Motor2.VlrDs% = 0
       APLICA.DSCTOSV2 = 0																									!                                         
    	 Exit Function                                                    		!                                      		
   	EndIf                           
    Gr.Motor2.VlrDs% = Xdscto.Pago%																			  	! Reporta valor dscto                          
    APLICA.DSCTOSV2 = -1																							 			!
    Exit Function
 EndIf																																			! 

 Xdscto.Pago%  = Apl.Dscto.Compra                            	      		    ! Si hay descuentos en motor

!  If Xdscto.Pago% = 0 Then Begin																					  ! No hay descuentos para aplicar
! 	  Gr.Motor2.VlrDs% = 0
!    APLICA.DSCTOSV2 = 0																									    !                                         
! 	  Exit Function                                                    		    !                                      		
! EndIf
 
 If Gr.Motor2.OldDscto% = Xdscto.Pago% Then Begin												    ! No hay una promocion nueva
    APLICA.DSCTOSV2 = 0																									    !
    Gr.Motor2.VlrDs% = Xdscto.Pago%
    Exit Function                                                    		    !                                      		
 EndIf																																		  !

 If Gr.Motor2.OldDscto% <> 0 Then Begin																			! Anula dscto aplicado
   Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
   TS.IO.KEYS(1) = 70
   TS.IO.DATA$(5) = Gr.Motor2.GrpDscto$
   TS.IO.KEYS(5) = 62
   TS.IO.MOTORKEY = 62
   Gr.Motor2.Dsto44% = (Gr.Motor2.OldDscto%)																! Valor descuento
   Call TSBDEC02                                   													! APLICA  DESCUENTOS
   TS.BD.DSPPARM = -1                              													! SHOW BD TO DISPLAY SUBTOTAL
   Call TSBDEC01                                   													! DISPLAY NEW SUBTOTAL
   Gr.Motor2.DctApl% = 0
   Gr.Motor2.OldDscto% = 0
   Gr.Motor2.Dsto44% = 0
   Gr.Motor2.VlrDs% = 0
 EndIf  
  TS.BD.DSPPARM = -1                              													! SHOW BD TO DISPLAY SUBTOTAL
  Call TSBDEC01                                   													! DISPLAY NEW SUBTOTAL
  Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
  TS.IO.DATA$(5) = Gr.Motor2.GrpDscto$
  TS.IO.KEYS(5) = 62
  TS.IO.MOTORKEY = 62
  Gr.Motor2.Dsto44% = (Xdscto.Pago%)																				! Valor descuento
  Call TSBDEC02                                   													! APLICA  DESCUENTOS
  Gr.Motor2.OldDscto% = Xdscto.Pago%																		  	! Actualiza valor dscto
  Gr.Motor2.DctApl% = -1
  Gr.Motor2.VlrDs% = Xdscto.Pago%																			  		! Reporta valor dscto
  TS.BD.DSPPARM = -1                              													! SHOW BD TO DISPLAY SUBTOTAL
  Call TSBDEC01                                   													! DISPLAY NEW SUBTOTAL
  APLICA.DSCTOSV2 = -1																							  			!                                            
End Function 																																! Fin calculo promociones

Function Prm2.String(X.A$)                       														! Coloca datos por parametro
String Prm2.String, X.A$, X.B$, X.C$, X.D$, XM$
Integer*4 X.A%, X.B%
X.A% = MATCH("&",X.A$,1)						            														! Busca dato a imprimir
If X.A% > 0 Then Begin                          														! Si lo encotro
   X.B% = LEN(X.A$)                             														! Arma mensaje con el dato 
   X.B$ = Left$(X.A$,X.A%-1)                    														! solicitado, entregando 
   X.C$ = Mid$(X.A$,X.A%,2)                     														! 38 caracteres
   X.D$ = Right$(X.A$,X.B% - (X.A%+1))
   If UCASE$(X.C$) = "&D" Then Begin            														! Direccion Cliente
   	If Gr.Lcl.Dir$ <> "                    " Then \
     X.C$ = Gr.Lcl.Dir$ Else \
     X.C$ = "____________________"	
   EndIf
   If UCASE$(X.C$) = "&P" Then Begin            														! Telefono Cliente
   	 If Gr.Lcl.Tel$ <> "          " Then \
        X.C$ = Gr.Lcl.Tel$ Else \
    	  X.C$ = "____________________"
   EndIf

   If UCASE$(X.C$) = "&N" Then Begin            														! Numero de Boletas
      X.C$ = str$(Gr.Motor2.Bol%)
   EndIf

!   If UCASE$(X.C$) = "&T" Then Begin            														! Monto de la compra
!      Call Format.Amount(Val(Gr.Mkp.Compra$))
!      X.C$ = "$"+TS.TEMP1$
!   EndIf
   
   X.B$ = X.B$ + X.C$ + X.D$
   X.B$ = Left$(X.B$+String$(37," "),37)
   X.A$ = X.B$
EndIf    
Prm2.String = X.A$

End Function
!--- Fin funcion data String


Sub IMPRIME.MSG.PROMOCIONAL																									! Imprime mensaje promocional
Integer*4 Xloc%, Gi%, Xpos%, Gj%																						!
String    X.tmp$, Xmsg$, Xnvc$, Xkey$, Xd1$, Xd2$, Xln$, Xrif$							!

 If TS.TRAINING Then Gr.Motor2.Msg% = 0																			! En entrenamiento no genera mensajes

! If TS.TOTALS(0,0,0) <= 0 Then Gr.Motor2.Msg% = 0                           ! Elimina generación boletas rifas si reintregros 24Ago2023

 If Gr.Motor2.Msg% = -1 Then Begin																					! Genera Msg Promocionales
 	  Xloc% = 1																																!
 	  Asc.Tmp.Apun% = 1																												!
 	  While Xloc% < Len(Gr.Motor2.LstM$)																			! Lista mensajes promocionales
 	    X.tmp$ = Asic.Getunpk5(Gr.Motor2.LstM$,Xloc%)						    					! Mensaje impresion
      Xloc% = Asc.Tmp.Apun%																									! Graba posicion
      Asc.Tmp.Apun% = 1																											!
      Xmsg$  = Asic.Getunpk4(X.tmp$,Asc.Tmp.Apun%)							    				! Nro Mensaje
      Xnvc$  = Asic.Getunpk4(X.tmp$,Asc.Tmp.Apun%)							    				! Nro veces imprimir
      
      Xkey$ = Pack$(Right$("0000000"+Xmsg$,7) + "000")											! Busqueda mensaje
      TS.ER.RETURN = -1																											! Control errores
      READ FORM "C5 C38";#60 KEY XKEY$;                       \! Lee mensaje
           Xd1$, Xd2$																												!
      If TS.ER.RETURN = -1 Then Begin																				! Encontrado
      	 Xd2$ = Left$(Xd2$,3)																								! Total Lineas del mensaje
      	 Xpos% = Val(Xd2$)
         Gr.Motor2.Bol% = Val(Xnvc$)
         If Gr.Motor2.Bol% > 0 Then Begin
         	  Call U.Imprime("MENSAJE  :"+Xmsg$,2100H)
         	  Call U.Imprime("NRO.VECES:"+Str$(Gr.Motor2.Bol%),2100H)
         EndIf
      	 For Gj% = 1 To Val(Xnvc$)																					! Total de veces a imprimir mensaje
      	 !For Gj% = 1 To 1
      	  Gi% = 2
      	  While (Gi% <= Xpos%)																							! Imprime mensaje
      	     If Gi% = 2 Then Begin
		          If CA.CUSTOMER$ = "" Then CA.CUSTOMER$ ="ESTIMADO(A) CLIENTE"	!
		          TS.TEMP1$ = "Sr(a) "+CA.CUSTOMER$															!
		          TS.TEMP1$ = Left$(TS.TEMP1$+String$(37," "),37)								!
              Call U.Imprime(TS.TEMP1$,4101H)																!
              TS.TEMP1$ = "ID CLTE:"+Gr.Lcl.Clte$
		          TS.TEMP1$ = Left$(TS.TEMP1$+String$(37," "),37)								!
              Call U.Imprime(TS.TEMP1$,4100H)
              If TS.TRAINING Then Begin                           				  ! Si en entrenamiento
          	     TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(10H) + "TIQUETE EN ENTRENAMIENTO"
          	     TS.TEMP1$ = LEFT$(TS.TEMP1$ + STRING$(38," "),38)
          	     Call U.Imprime(TS.TEMP1$,4100H)
          	     TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(10H) + "TRANSACCION SIN VALIDEZ"
          	     TS.TEMP1$ = LEFT$(TS.TEMP1$ + STRING$(38," "),38)
          	     Call U.Imprime(TS.TEMP1$,4100H)
              EndIf																													! Fin entrenamiento
      	     EndIf
      	     Xkey$ = Pack$(Right$("0000000"+Xmsg$,7) + Right$("000"+Str$(Gi%),3)) ! Busqueda mensaje
             TS.ER.RETURN = -1																						  ! Control errores
             READ FORM "C5 C38";#60 KEY XKEY$;                \! Lee mensaje
             Xd1$, Xd2$																											!
      	     If TS.ER.RETURN = -1 Then Begin																! Encontrado
      	     	  Xln$ = Prm2.String(Xd2$)																		!
      	     	  !Xln$ = xd2$
      	     	  Call U.Imprime(Xln$,4100H)
      	     EndIf
      	     Gi% = Gi% + 1
      	  Wend 
          If TS.TRAINING Then Begin                           				  		! Si en entrenamiento
       	     TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(10H) + "TIQUETE EN ENTRENAMIENTO"
       	     TS.TEMP1$ = LEFT$(TS.TEMP1$ + STRING$(38," "),38)
       	     Call U.Imprime(TS.TEMP1$,4100H)
       	     TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(10H) + "TRANSACCION SIN VALIDEZ"
       	     TS.TEMP1$ = LEFT$(TS.TEMP1$ + STRING$(38," "),38)
       	     Call U.Imprime(TS.TEMP1$,4100H)
          EndIf																															! Fin entrenamiento
      	 Asic.Detalle% = 1																									!
         TS.TEMP1$ = Linea.Detalle(1)																				!
         Call U.IMPRIME(TS.TEMP1$,4100H)                 									  ! Detalle Trailer Trx        
         Call U.IMPRIME(String$(19,"- "),4100H)															!
         Call U.IMPRIME("",4900H)																						!
         Call U.CORTACR       																							!
         TS.LINETYPE = 18																										! Genera cabecera 
         TS.LINEDATA = 1																										! en proximo tiquete
         Call TSPREC01																											!
     	  Next Gj%
      EndIf Else Begin
      	Call visor.and.borrar("msg:"+unpack$(Xkey$)+" no existe")
      EndIf
 	  Wend 																																		!
 EndIf																																			! Fin generacion mensajes
End Sub 																																		! 

Sub Carga.Mensajes.Promo2																										! Carga mensakes promocion
String XL$, Gfile$
Integer*2 Gi%
If Gr.Motor2.Fileok% = 0 Then Begin																					! Si archivo no abierto
   TS.TS11WERR$ = ""																											 	! Control errores
   Open "R::$F:MSGPRM" KEYED RECL 43 AS 60 nowrite nodel 		  ! Mensajes Promocionales
   If TS.TS11WERR$ <> "" Then Begin                                         !
  	  Call VISOR.AND.BORRAR("ERROR APERTURA MENSAJ. PROMO")									! MSg alerta
  	  Call U.IMPRIME("ERROR APERTURA PROMO V2 "+TS.TS11WERR$,6100H)         !
  	  Gr.Motor2.Ok%  = 0                                                    ! Proyecto Apagado
  	  Exit Sub 																															! Sale del proceso
   EndIf  																																	!
   TS.TS11WERR$ = ""
   Open "R::$F:ITMLST" KEYED RECL 508 AS 61 nowrite nodel 
   If TS.TS11WERR$ <> "" Then Begin																				  ! Error apertura
      Call U.Imprime("Error Apertura TF:ITMLST Cancelado",6100H)						!
      Call U.IMPRIME("ERROR="+TS.TS11WERR$+" COD="+TS.DISPERR$+" FILE="+   \! Msg Error appl
                    STR$(TS.ERRF),6100H)																		!
      Close 22																															! Ciere archivo mensajes
      Gr.Motor2.Ok%  = 0                                                    ! Proyecto Apagado
      Exit Sub  																														!
   EndIf                                                                    !
   Gr.Motor2.Fileok% = -1																										! Archivo abierto
EndIf
End Sub 																																		! Fin carga de mensajes

Sub ARMAR.PAGO.DESCUENTO
If TS.IO.MOTORKEY = 81 Then Begin																						! Si tecla total
	 Gr.Motor2.UltPago$ = "11:1:"																							! Simula pago efectivo
   TS.TEMP1I1 = APLICA.DSCTOSV2 																				  	! Busqueda y aplicacion descuentos
   TS.IO.KEYS(7) = 99																												!
   Exit Sub 																																! Retorna proceso
EndIf																																				! Fin tecla total

!-- Forma de pago efectivo
  If (TS.IO.KEYS(7) = 91) Then Begin       																	! o tecla motora para tef
    If Val(TS.IO.DATA$(7)) > 0 Then Begin																		! Si valor pago capturado
    	 TS.TEMP5I4 = Val(TS.IO.DATA$(7))
    EndIf Else TS.TEMP5I4 = TS.BALDUE(0)																		! Reporta saldo de la cuenta
    If TS.IO.KEYS(1) = 70 Then TS.TEMP5I4 = 0 - TS.TEMP5I4 
    If Val(TS.IO.DATA$(7)) > TS.BALDUE(0) Then TS.TEMP5I4 = TS.BALDUE(0)    ! Si pago mayor que saldo factura
   	    If TS.IO.DATA$(3) <> "" Then                                       \! Variedad capturada
 	   	     Gr.Motor2.UltPago$ = Str$(TS.IO.KEYS(7)-90) + TS.IO.DATA$(3) + ":"+ \!
 	   	                          Str$(TS.TEMP5I4) + ":" Else                \!
 	   	     Gr.Motor2.UltPago$ = Str$(TS.IO.KEYS(7)-90) + "1" + ":" +       \!
 	   	                          Str$(TS.TEMP5I4) + ":"                      !
 	   
     TS.TEMP1I1 = APLICA.DSCTOSV2 																				  ! Busqueda y aplicacion descuentos

     If Gr.Motor2.Refund% = 0 Then Begin                                    !
        If Val(TS.IO.DATA$(7)) = 0 Then                                    \!
        	TS.IO.DATA$(7) = Str$(TS.BALDUE(0))                               !
        	Exit Sub 
     EndIf Else Begin 
     	    TS.IO.DATA$(7) = ""
     	    Exit Sub 
     EndIf
  EndIf																																			! Fin Pago con TEF

!--- Formas de pago nativas y TEF
  If (TS.IO.KEYS(7) > 91 AND TS.IO.KEYS(7) < 97) Then Begin                \! Valida si en forma de pago nativa
    If Val(TS.IO.DATA$(7)) > 0 Then Begin																		! Si valor pago capturado
    	 TS.TEMP5I4 = Val(TS.IO.DATA$(7))
    EndIf Else TS.TEMP5I4 = TS.BALDUE(0)																		! Reporta saldo de la cuenta
    If (TS.BALDUE(0) - TS.TEMP5I4) <= 0 Then Begin													! Si cerrando la trx
    	   TS.TEMP5I4 = TS.BALDUE(0)																					! Retorna saldo pendiente de pago
    EndIf 
    If TS.IO.DATA$(3) <> "" Then                                       \! Variedad capturada
 	   	     Gr.Motor2.UltPago$ = Str$(TS.IO.KEYS(7)-90) + TS.IO.DATA$(3) + ":"+ \!
 	   	                          Str$(TS.TEMP5I4) + ":" Else                    \!
 	   	     Gr.Motor2.UltPago$ = Str$(TS.IO.KEYS(7)-90) + "1" + ":" +           \!
 	   	                          Str$(TS.TEMP5I4) + ":"                          !
 	  If (Left$(Gr.Motor2.UltPago$,2) = "41" Or Left$(Gr.Motor2.UltPago$,2) = "51") And \
         Gr.Fis.TrjVisa% = -1 Then \
	 	     Gr.Motor2.UltPago$ = "52"+":"+Str$(TS.TEMP5I4) + ":"          ! Si en pago TEF y TC Visa Comfandi
 	   
     TS.TEMP1I1 = APLICA.DSCTOSV2 																				  ! Busqueda y aplicacion descuentos
     
     !If (Val(TS.IO.DATA$(7)) = 0) Or (Val(TS.IO.DATA$(7)) > TS.BALDUE(0)) Then \
     
     If TS.TEMP5I4 >= TS.BALDUE(0) Then \
     	  TS.IO.DATA$(7) = Str$(TS.BALDUE(0))                                 !
     
     Gr.Motor2.Baldue% = TS.BALDUE(0)	 
  EndIf																																			! Fin Pago con TEF

!--- Formas de pago nativas y TEF
  If (TS.IO.KEYS(7) = 176) Or (TS.IO.KEYS(7) = 173) Then Begin							! o tecla motora para tef
    If Val(TS.IO.DATA$(7)) > 0 Then Begin																		! Si valor pago capturado
    	 TS.TEMP5I4 = Val(TS.IO.DATA$(7))
    EndIf Else TS.TEMP5I4 = TS.BALDUE(0)																		! Reporta saldo de la cuenta
     If (TS.BALDUE(0) - TS.TEMP5I4) <= 0 Then Begin													! Si cerrando la trx
    	   TS.TEMP5I4 = TS.BALDUE(0)																					! Retorna saldo pendiente de pago
     EndIf 
     If TS.IO.KEYS(7) <> 176 Then Begin
   	    If TS.IO.DATA$(3) <> "" Then                                       \! Variedad capturada
 	   	     Gr.Motor2.UltPago$ = Str$(TS.IO.KEYS(7)-90) + TS.IO.DATA$(3) + ":"+ \!
 	   	                          Str$(TS.TEMP5I4) + ":" Else                    \!
 	   	     Gr.Motor2.UltPago$ = Str$(TS.IO.KEYS(7)-90) + "1" + ":" +           \!
 	   	                          Str$(TS.TEMP5I4) + ":"                          !
 	   	     If (Left$(Gr.Motor2.UltPago$,2) = "41" Or Left$(Gr.Motor2.UltPago$,2) = "51") And \
              Gr.Fis.TrjVisa% = -1 Then \
	 	          Gr.Motor2.UltPago$ = "52"+":"+Str$(TS.TEMP5I4) + ":"          ! Si en pago TEF y TC Visa Comfandi
 	   	           
 	   	EndIf Else Begin
 	   		   Gr.Motor2.UltPago$ = "41"+":"+Str$(TS.TEMP5I4) + ":"             ! Reporta secuencia simulada de pago
 	   		   
           If Gr.Fis.TrjVisa% = -1 Then \
	 	        Gr.Motor2.UltPago$ = "52"+":"+Str$(TS.TEMP5I4) + ":"            ! Simula pago efectivo y TC Visa Comfandi
 	   		   
 	   	EndIf
 	   	If TS.IO.KEYS(7) = 173 Then Begin																			! Reporte secuencia lifefiles
 	   		 Gr.Motor2.UltPago$ = "54"+":"+Str$(TS.TEMP5I4) + ":"               ! Reporta secuencia simulada de pago
 	    EndIf
     TS.TEMP1I1 = APLICA.DSCTOSV2 																				  ! Busqueda y aplicacion descuentos
!     If (Val(TS.IO.DATA$(7)) = 0) Or (Val(TS.IO.DATA$(7)) > TS.BALDUE(0)) Then \
!     	  TS.IO.DATA$(7) = Str$(TS.BALDUE(0))                                 !

     If TS.TEMP5I4 >= TS.BALDUE(0) Then \
     	  TS.IO.DATA$(7) = Str$(TS.BALDUE(0))                                 !

     Gr.Motor2.Baldue% = TS.BALDUE(0)	 
  EndIf																																			! Fin Pago con TEF

  
!-- Forma de pago credito social
  If (TS.IO.KEYS(5) = 221) Then Begin        																! Credito social
    If Val(TS.IO.DATA$(5)) > 0 Then Begin																		! Si valor pago capturado
    	 TS.TEMP5I4 = Val(TS.IO.DATA$(5))
    EndIf Else TS.TEMP5I4 = TS.BALDUE(0)																		! Reporta saldo de la cuenta
     If (TS.BALDUE(0) - TS.TEMP5I4) <= 0 Then Begin													! Si cerrando la trx
    	   TS.TEMP5I4 = TS.BALDUE(0)																					! Retorna saldo pendiente de pago
     EndIf   																																!
	   Gr.Motor2.UltPago$ = "56"+":"+Str$(TS.TEMP5I4) + ":"             			! Reporta secuencia simulada de pago

     TS.TEMP1I1 = APLICA.DSCTOSV2 																				  ! Busqueda y aplicacion descuentos

     !If Val(TS.IO.DATA$(5)) = 0 Then TS.IO.DATA$(5) = Str$(TS.BALDUE(0))    !
     	
!     If (Val(TS.IO.DATA$(5)) = 0) Or (Val(TS.IO.DATA$(5)) > TS.BALDUE(0)) Then \
!     	  TS.IO.DATA$(5) = Str$(TS.BALDUE(0))                                 !
     	
     If TS.TEMP5I4 >= TS.BALDUE(0) Then \
     	  TS.IO.DATA$(5) = Str$(TS.BALDUE(0))                                 !
     	
  EndIf																																			! Fin Pago con TEF
  
!-- Forma de pago Pes           
  If (TS.IO.KEYS(2) = 250) Then Begin        																! Pago Electronico
    If Val(TS.IO.DATA$(2)) > 0 Then Begin																		! Si valor pago capturado
    	 TS.TEMP5I4 = Val(TS.IO.DATA$(2))
    EndIf Else TS.TEMP5I4 = TS.BALDUE(0)																		! Reporta saldo de la cuenta
     If (TS.BALDUE(0) - TS.TEMP5I4) <= 0 Then Begin													! Si cerrando la trx
    	   TS.TEMP5I4 = TS.BALDUE(0)																					! Retorna saldo pendiente de pago
     EndIf   																																!
	   Gr.Motor2.UltPago$ = "43"+":"+Str$(TS.TEMP5I4) + ":"             			! Reporta secuencia simulada de pago

     TS.TEMP1I1 = APLICA.DSCTOSV2 																				  ! Busqueda y aplicacion descuentos

!     If Val(TS.IO.DATA$(2)) = 0 Then TS.IO.DATA$(2) = Str$(TS.BALDUE(0))    !
     	
!     If (Val(TS.IO.DATA$(2)) = 0) Or (Val(TS.IO.DATA$(2)) > TS.BALDUE(0)) Then \
!     	  TS.IO.DATA$(2) = Str$(TS.BALDUE(0))                                 !

      If TS.TEMP5I4 >= TS.BALDUE(0) Then \
     	  TS.IO.DATA$(2) = Str$(TS.BALDUE(0))                                 !
    	
  EndIf																																			! Fin Pago con TEF

End Sub 

Sub CALCULO.PROMOCIONES.V2 Public
Integer*1 Xnewpago%
     Call ARMAR.PAGO.DESCUENTO																							! Armar pago descuento
     If TS.TEMP1I1 = -1 Then Begin																					! Si descuento aplicado
        Exit Sub 
     EndIf 																																  ! fin descuento aplicados
End Sub 

Sub GR.CARGA.PROMO																													! rutina carga promociones terminal
String Xsnd$, Xrtrama$, Xllega$, Xtemp4$
  Call VISORES4690(1,"CARGANDO PROMOCIONES","ESPERE POR FAVOR...",0,"L")
  TS.TEMP2$ = Armar.Trama.Msg(Gr.Motor2.Appl$,"03","","00","0001","123456")	! Armar trama MSG
  Xsnd$ = DATE$ +":"+ Time$                                                 ! Fecha y hora del requerimiento  
  XRtrama$  = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)  		! Ejecuta Requerimiento
  
  !XRtrama$ = "7703046200000CONSULTA SATIFACTORIA                   "
  
  Xllega$ = DATE$ +":"+ Time$                                               ! Fecha y hora de la respuesta requerimiento  
  Call PROMO.AUDITORIA(ts.temp2$,Xrtrama$, Xsnd$, Xllega$)                	! Rastreo movimiento
  XTEMP4$ = Valida.Rta(Xrtrama$)																			      ! Valida rta entregada
  If Xtemp4$ <> "00" Then Begin 																						! Si falla proceso
  	 Call VISOR.AND.BORRAR("CARGA PROMOCIONES   NO EXITOSA /Borra")
  EndIf Else Begin
  	 Call VISOR.AND.BORRAR("CARGA PROMOCIONES   SI EXITOSA /Borra")
  EndIf
  Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																	! Termina proceso
  TS.IO.MOTORKEY = 73																												!
End Sub 


Sub VTA.ITM.PROMOCION
String Xitm$, Xente$, Xqty$
Xitm$ = SL.IT.ITEMCODE$
PROC.ITM.PROMO:
  TS.TEMP2$ = Pack$(Right$("000000000000"+Xitm$,12))              					!
  Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)              							! Lectura Datos Itemr
  Call IRRFEC.SPLIT1( TS.TEMP1$ ) 			                       							! Entrega datos al eamitemr.j86
  If UNPACK$(IR.INDICAT2$) = "05" Then BEGIN 																! Si metodo precio 5
     Xitm$ = UNPACK$(IR.SALEQUAN$) + UNPACK$(IR.SALEPRIC$)                  ! Toma PLU asignado a la barra
     GoTo PROC.ITM.PROMO
  EndIf																																			!
  SL.IE.SALEPRIC = Val(unpack$(IR.SALEPRIC$))																! Precio maestra
  IR.PRICE1 = SL.IE.SALEPRIC
  IR.PRICE2 = IR.PRICE1
End Sub 

Sub PROMOCIONV2(Xopt%) Public 																							! Motor Promociones V2
Integer*4 Xopt%, Gi%																												! UE ejecutar
String Xtmpp$
!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.Motor2.Ok%  = 0                                                        ! Proyecto Apagado
  Gr.Motor2.VlrDs% = 0                  																	!
  Gr.Motor2.OldDscto% = 0																									!
  Gr.Motor2.Apl% = 0																							  			! Complea retirada
  Gr.Motor2.LstM$ = ""																										! Lista mensajes
  Gr.Motor2.Msg% = 0 																											! Aplicacion mensajes
  Gr.Motor2.Refund% = 0																										! Proceso reintegro
  Gr.Motor2.AplTot% = 0
  Gr.Motor2.DctApl% = 0
  Gr.Motor2.Dsto44% = 0
  TS.TS11WERR$ = ""																												  ! Control errores
  Open "R::$SCNTRL" As 94 UNLOCKED NOWRITE NODEL 													  ! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then Begin                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA PROMOCION V2 ")									! MSg alerta
  	 Call U.IMPRIME("ERROR APERTURA PROMOCIONES V2",6100H)                  !
  	 Exit Sub 																															! Sale del proceso
  EndIf  																																		!
  If End #94 Then UE.FIN.MOTORV2         																	  ! Si es EOF
  While (1)															  																  ! Recorre archivo parametros
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[PROMOCIONES V2]" Then Begin	   	     				      ! Motor Promociones V2
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Motor2.Ok%       = Val(Mid$(TS.TEMP1$,30,2)) 		    					  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Motor2.Descrip$  = Mid$(TS.TEMP1$,30,21)   			    					  ! Descriptor descuento
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Motor2.GrpDscto$ = Mid$(TS.TEMP1$,30,02)   			    					  ! Grupo Descuento Motor Or
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Motor2.LstPago$  = Mid$(TS.TEMP1$,30,70)   			    					  ! Lista formas de pago
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Motor2.Ses1%     = Val(Mid$(TS.TEMP1$,30,2))		    					  ! Sesion 1
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Motor2.Ses2%     = Val(Mid$(TS.TEMP1$,30,2))		    					  ! Sesion 2
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Motor2.Ses3%     = Val(Mid$(TS.TEMP1$,30,2))		    					  ! Sesion 3
         Read #94; TS.TEMP1$																								! Lectura registro
         Gr.Motor2.Appl$ = Mid$(TS.TEMP1$,30,2)                             ! Codigo aplicacion
         Read #94; TS.TEMP1$																								! Lectura registro
         Gr.Motor2.Func$ = Mid$(TS.TEMP1$,30,2)                             ! Codigo aplicacion
         Gr.Motor2.Ses1% = 61
         Gr.Motor2.Ses2% = 60
         Gr.Motor2.Ses3% = 66

         GoTo UE.FIN.MOTORV2 																								! Sale del ciclo de carga          
        EndIf																																! Fin carga proyecto
  Wend 																																			! Fin carga parametros
   UE.FIN.MOTORV2:                                                          !
     Close 94																																! Cierra archivo
   Call Carga.Mensajes.Promo2																								! Apertura archivo mensajes
   Call GR.CARGA.PROMO																											! Carga definicion de rifas
   If Gr.Motor2.Ok% = -1 Then                                              \! Proyecto Activo
      Call U.IMPRIME("MODULO PROMOCION V2  ON  28-Feb-2024",2100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO PROMOCION V2  OFF 28-Feb-2024",2100H)          ! Msg Proyecto Cargado
      
EndIf																																				! Fin carga de parametros

If Gr.Motor2.Ok% <> -1 Then Exit Sub                                        ! Si proyecto apagado
If TS.STANDALONE       Then Exit Sub	                                      ! Si en operacion TOF

!--- EAMTSU02.J86
If Xopt% = 02 Then Begin                                                    ! Nueva transaccion
    Call IMPRIME.MSG.PROMOCIONAL																						!
    Gr.Motor2.VlrDs% = 0                  																	!
    Gr.Motor2.OldDscto% = 0																									!
    Gr.Motor2.Apl% = 0																							  			! Complea retirada
    Gr.Motor2.LstM$ = ""																										! Lista mensajes
    Gr.Motor2.Msg% = 0 																											! Aplicacion mensajes
    Gr.Motor2.Refund% = 0																										! Proceso reintegro
    Gr.Motor2.AplTot% = 0
    Gr.Motor2.DctApl% = 0
    Gr.Motor2.Baldue% = 0
    Gr.Fis.TrjVisa% = 0
    Gr.Motor2.Xpos% = 0
    Gr.Motor2.Dsto44% = 0
    Dim Gr.Motor2.Items(1000,9)
EndIf																																				! Fin nueva trx 

!--- EAMTSU08.J86
If XOPT% = 08 Then  BEGIN																										! Procesando Items
   Call VTA.ITM.PROMOCION																										!
   Gr.Motor2.AplTot% = 0																										!
   Gr.Motor2.VlrDs% = 0
EndIf

!--- EAMTSU14.J86
If XOPT% = 14 Then Begin 																										! Ue secuencias de tecleo

 If (TS.IO.KEYS(5) = 61) And (TS.IO.STATE = 10) Then Begin      						! Si presiona secuencia
   If TS.INTRX Then Begin 																									! Si dentro de una trx
   	  Call VISOR.AND.BORRAR("PROCESO NO VALIDO   EN UNA TRX. BORRAR  ")			! Msg de alerta
      TS.IO.MOTORKEY = 73    																								! Inicializa secuencia
      Dim TS.IO.KEYS(10)                                          					! de tecleo 
      Dim TS.IO.DATA$(10)                                         					! 
      Exit Sub 																															!
   EndIf 																																		!
   If (Val(TS.IO.DATA$(5)) = 60) Then Call GR.CARGA.PROMO    								! Proceso cierre fin dia datafono
   	
 EndIf 																																			!

If (TS.INTRX) And TS.IO.STATE = 10 Then Begin																! En una trx de venta
 If TS.IO.MOTORKEY = 81 And Gr.Motor2.AplTot% = 0 Then Begin								! Presiona tecla total
 	  
!-- Controles de no ejecucion procesos Comfandi
    If UE.RECASRV.QTY% > 0 Then Begin 																			! En proceso de recaudos
    	 Exit Sub 																														! No ejecuta motor 
    EndIf																																		! Fin proceso de recaudos
    If Gr.Mrm.Cap% = -1 Then Begin 																					! En proceso de remisiones
    	 Exit Sub 																														! No ejecuta motor 
    EndIf																																		! Fin proceso de recaudos
    If Gr.NtaCrd.InTrx% = -1 Then Begin 																		! En proceso de notas credito
    	 Exit Sub 																														! No ejecuta motor 
    EndIf								
!-- Fin controles de no ejecucion 
    TS.TEMP1I1 = 0																													!	
    For I% = 1 To 10																												! Valida secuencias entradas
        If (TS.IO.KEYS(I%) <> 0 And I% <> 6) Then Begin											! revisa si total con otra secuencia
        	 TS.TEMP1I1 = -1																									! hay otra secuencia
        EndIf																																!
    Next I%																																	!
    If TS.TEMP1I1 = -1 Then Exit Sub 																				! Otras secuencias con total
    
    !If TS.TOTALS(0,0,0) < 0 Then Gr.Motor2.Refund% = -1											! En proceso de reintegro
    	
    Call CALCULO.PROMOCIONES.V2																							! Calculo promociones
    Call TSBDEC01
    Gr.Motor2.AplTot% = -1
 EndIf
EndIf

If (TS.INTRX) Then Begin																										! En una trx de venta
 TS.TEMP5$ = ";"+Str$(TS.IO.MOTORKEY) + ";"																	! Tecla motora presionada
 If (TS.IO.STATE = 10) Then 										 											     \! No anulando pago
 If (TS.IO.KEYS(7) > 90 AND TS.IO.KEYS(7) < 97) Or                         \! Valida si en forma de pago 
     (MATCH(TS.TEMP5$,Gr.Motor2.LstPago$,1) > 0) Then Begin        					! o tecla motora para pagos
  	If TS.TRX.STATUS <> 1 And TS.PROCEDURE = -1 THEN BEGIN				  	      ! No se ha ingresado total en venta
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 0 																									! 
       TS.GUIDANCE = 1020																										! Solicita total a la trx
       Exit Sub 																														! Sale del proceso
    EndIf 																																	!
!-- Controles de no ejecucion procesos Comfandi
    If UE.RECASRV.QTY% > 0 Then Begin 																			! En proceso de recaudos
    	 Exit Sub 																														! No ejecuta motor 
    EndIf																																		! Fin proceso de recaudos
    If Gr.Mrm.Cap% = -1 Then Begin 																					! En proceso de remisiones
    	 Exit Sub 																														! No ejecuta motor 
    EndIf																																		! Fin proceso de recaudos
    If Gr.NtaCrd.InTrx% = -1 Then Begin 																		! En proceso de notas credito
    	 Exit Sub 																														! No ejecuta motor 
    EndIf								
!-- Fin controles de no ejecucion 
    !If TS.TOTALS(0,0,0) < 0 Then Gr.Motor2.Refund% = -1											! En proceso de reintegro
    
    Call SALVAR.SECUENCIA
    Call CALCULO.PROMOCIONES.V2																							! Calculo promociones
    Call RESTAURAR.SECUENCIA

    If TS.IO.KEYS(7) > 91 And Gr.Motor2.VlrDs% > 0 Then Begin
    	 If Val(TS.IO.DATA$(7)) > ts.baldue(0) Then Begin
        TS.IO.DATA$(7) = str$(ts.baldue(0)) 
       EndIf
    EndIf

!    TS.TEMP5$ = ";"+Str$(TS.IO.MOTORKEY) + ";"																	! Tecla motora presionada
!    If (MATCH(TS.TEMP5$,Gr.Motor2.LstPago$,1) > 0) And Gr.Motor2.VlrDs% > 0 Then Begin
!	     TS.IO.DATA$(7) = str$(ts.baldue(0)) 
!    EndIf

!    Call u.imprime("data 7 :"+ts.io.data$(7),6100H)
    
!    If TS.IO.MOTORKEY = 173 And Gr.Motor2.Baldue% <> 0 Then Begin
!	     TS.IO.DATA$(7) = Str$(Gr.Motor2.Baldue%)
!	     Gr.Motor2.Baldue% = 0
!    EndIf
    
 EndIf																																			! Fin control secuencias pago
EndIf
EndIf																																				! Fin Eamtsu14

!--- EAMTSU20.J86
If XOPT% = 20 Then Begin 																										! En Impresion CR
	
 If MATCH("SUSPEND",TS.PRTBUF$,1) > 0 Then Begin 							    					! Si mensaje de Suspension Total
    Gr.Motor2.Msg% = 0            																					! Genera Msg Promocionales
 EndIf																																			!
 If MATCH(TS.SDESC$(59),TS.PRTBUF$,1) > 0 THEN BEGIN												! Si anulacion total de la trx
  	Gr.Motor2.Msg% = 0            																					! Genera Msg Promocionales
 EndIf																																			!

 If (TS.INTRX) AND (TS.LINETYPE = 6 AND TS.LINEDATA = 1) And  						 \!
    (TS.TENDERED (0) <> 0 Or TS.TRX.STATUS <> 100) Then Begin								!
    If Gr.Motor2.VlrDs% <> 0 Then Begin																			! hay dscto motor
    	 
!    	 If Gr.Motor2.VlrDs% >= 0 Then                                       \! Es una venta
!    	    Call Almacena.Promocion("00") Else                               \!
!    	    Call Almacena.Promocion("01")																			! Un reintegro
   
       Call Almacena.Promocion("00")	  																		! Almacena promocion y aplica dscto
    	 TS.TEMP1$ = "******** DESCUENTOS APLICADOS *******"									!
       Call U.IMPRIME(TS.TEMP1$,4101H)																			!                               
       Call U.IMPRIME(TS.TEMP1$,2100H)																			!                               
       For Gi% = 1 To Gr.Motor2.Xpos%																				!                                     
           TS.TEMP2$ = Gr.Motor2.Items(Gi%,0)                               ! Codigo del producto
           TS.TEMP2$ = Pack$(Right$("000000000000"+TS.TEMP2$,12))	  				! Armo dato para busqueda
           Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)         					! Lectura Datos Itemr
           Call IRRFEC.SPLIT1( TS.TEMP1$ ) 			                     				! Entrega datos al eamitemr.j86
           If Gr.Motor2.Items(Gi%,7) = "00" Then                           \!
              Call Format.Amount(Val(Gr.Motor2.Items(Gi%,1))*-1) Else 	   \! Formatea valor
              Call Format.Amount(Val(Gr.Motor2.Items(Gi%,1)))
           TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)                    ! 
           TS.TEMP1$ = Right$("       "+Gr.Motor2.Items(Gi%,0),7) +        \! Linea impresion
                       " " + IR.ITEMNAME$ + " " + TS.TEMP1$									! descuento
           TS.TEMP1$ = Left$(TS.TEMP1$+String$(38," "),38)                  ! Ajusta impresion
           Call U.IMPRIME(TS.TEMP1$,4101H)																	!
           Call U.IMPRIME(TS.TEMP1$,2100H)																	!
       Next Gi%                                                             !
       If Gr.Motor2.VlrDs% > 0 Then                                        \! Es una venta
          Call Format.Amount(Gr.Motor2.VlrDs% * -1)	Else 									 \! Formatea valor
        	Call Format.Amount(Gr.Motor2.VlrDs%)     	 										    ! Formatea valor
 	     !TS.TEMP1$ = "   TOTAL DESCUENTO   :"+Right$("          "+TS.TEMP1$,10) !
 	     TS.TEMP1$ = "       AHORRASTE HOY     :"+Right$("          "+TS.TEMP1$,10) !
       Call U.IMPRIME(TS.TEMP1$,4101H)																			!
       Call U.IMPRIME(TS.TEMP1$,2100H)																			!
       Call U.IMPRIME(String$(38,"-"),4101H)																!
       Call U.IMPRIME(String$(38,"-"),2100H)																!
       If TS.TOTALS(0,0,0) < 0 Then Gr.Motor2.Msg% = 0											! En reintrego no genera mensaje
    EndIf																																		!
 EndIf																																			!
EndIf																																				! Fin Eamtsu20

!--- EAMTSU23.J86
If XOPT% = 23 Then Begin 																										! En Visores de cliente

 If Gr.Motor2.VlrDs% <> 0 And Gr.Motor2.DctApl% = 0 Then                   \!	
  If Match("** TOTAL",TS.DISP2$,1) > 0 Then Begin													  ! Presentacion linea total
   If Gr.Motor2.Dsto44% > 0 Then                                            \!	
 		  Call Format.AMOUNT(Gr.Motor2.VlrDs% * -1) Else 										     \!
 		  Call Format.AMOUNT(Gr.Motor2.VlrDs%) 
	 TS.DISP1$ = "DESCUENTO:"+Right$("          "+TS.TEMP1$,10)							  !
   If Gr.Motor2.Dsto44% > 0 Then                                            \!
 		  TS.TEMP1I4 = (TS.BALDUE(0) - Gr.Motor2.VlrDs%)  Else                 \!
 		  TS.TEMP1I4 = (TS.BALDUE(0) + (Gr.Motor2.VlrDs% * -1)) 
     Call Format.Amount(TS.TEMP1I4)																					!
     TS.DISP2$ = Left$(TS.SDESC$(68),10)+Right$("          "+TS.TEMP1$,10)  ! 
     TS2.DISP1$ = TS.DISP1$																									!
     TS2.DISP2$ = TS.DISP2$																									!
  EndIf																																			! Fin Linea Total

!TS.DISP1$ Top line of the operator display
!TS.DISP2$ Bottom line of the operator display
!TS2.DISP1$ Top line of the customer display
!TS2.DISP2$ Bottom line of the customer display
	
EndIf

!--- EAMTSU32.J86
If XOPT% = 32 Then Begin 																										! En Visores de cliente

! If Gr.Motor2.Baldue% <> 0 Then Begin
!	  TS.IO.DATA$(7) = Str$(Gr.Motor2.Baldue%)
!	  Gr.Motor2.Baldue% = 0
! EndIf

EndIf 

!--- EAMTSU53.J86
If XOPT% = 53 Then Begin 																										! En recuperacion de trx
   If Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "03" THEN BEGIN     	              ! Descuento aplicado
      Asc.Tmp.Apun% = 3																											! Init apuntador
      Xtmpp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                    ! grupo descuento
      Xtmpp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                    ! porcentaje descuento
      Xtmpp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                    ! monto de descuento
      Gr.Motor2.VlrDs% = Val(Xtmpp$)																				! Aisgna valor del descuento
      Gr.Motor2.OldDscto% = Gr.Motor2.VlrDs%															  ! Actualiza valor dscto
      Gr.Motor2.DctApl% = -1																								! Dscto Aplicado
      Gr.Motor2.Dsto44% = Gr.Motor2.VlrDs%
   EndIf
   If Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "04" THEN BEGIN     	              ! Anul Descuento aplicado
      Asc.Tmp.Apun% = 3																											! Init apuntador
      Xtmpp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                    ! grupo descuento
      Xtmpp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                    ! porcentaje descuento
      Xtmpp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                    ! monto de descuento
      Gr.Motor2.VlrDs% = 0           																			  ! Aisgna valor del descuento
      Gr.Motor2.OldDscto% = 0               															  ! Actualiza valor dscto
      Gr.Motor2.DctApl% = 0 																								! Dscto Aplicado
      Gr.Motor2.Dsto44% = 0
   EndIf
   
EndIf																																				! fin recuperacion de trx

!--- EAMTSU44.J86
If XOPT% = 44 Then Begin 																										! En calculos adicionales descuentos
!	 Call U.IMPRIME("TOTALS:"+STR$(TS.TOTALS(0,0,0)),6100H)
!	 Call U.IMPRIME("DSCTOS:"+STR$(TS.TOTALS(0,0,1)),6100H)
!   Call U.IMPRIME("SAVE  :"+STR$(TS.DISC.SAVE(0,0)),6100H)
 If Gr.NtaCrd.InTrx% = 0 Then Begin	    																		! No Es una nota credito
   TS.DISC.SAVE(0,0) = Gr.Motor2.Dsto44%
 EndIf
 
EndIf																																				! Fin EAMTSU44

End Sub																																			!
 
