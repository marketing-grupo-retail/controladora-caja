!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRIVEXIT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Impresion de IVAS         *
!**************************************************
! Mod 17Oct2012
! Se adiciona la impresion de los header de tiquete
! el en rollo de auditoria.
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 16Abr2015
! Se modifica la leyenda del numero de autorizacion
! y fecha de ser Autorretenedores
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 12Mzo2019
! Se modifica la funcion de Impresion.Detalle.Iva
! para presentación impuesto al consumo por tarifa 
! de IVA.
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 14Ago2019
! Se modifica la presentacion del descriptor de 
! peso en tiquete tomando como parametro los defi-
! nidos en el eamsdesc.dat descriptonres numeros
! 102 (Kg) y 103 (/Kg)
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! 20 Abr 2020
! Se adiciona la impresión del mensaje 
! Bienes Exentos  -  Decreto 417 del 17 de marzo de 2020
! en las transacciones con ventas de iva excento
! requerimiento decreto 551 del 2020 Coronavirus
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! 16 Jun 2020
! Manejo para dia Sin iva, recalculo de impuesto
!--------------------------------------------------
! Mod 21Oct2021
! Se controla la no impresión de lineas fiscales 
! cuando se reporta la no impresión de factura 
! fiscal para el caso del dia Sin  iva
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 23Ene2023
! Se retira impresión mensaje de bienes exentos
! solicitado por Comfandi
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 13Jul2023
! Se adiciona proceso para manejo de venta y reporte
! impuesto a la bolsa plastica
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 17/Oct/2023
! Se adiciona funcionalidad y calculo para manejo de 
! nuevo impuestos ICUI (impuesto consumo ultraprocesados
! industriales) 
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 11/Abr/2024
! Se ajusta libreria fiscal para manejo facturacion 
! electronica Colombia (FECO Ver. 1.9)
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 03/Jul/2024
! Se ajusta mensaje de observaciones solicitado por 
! Comfandi, en temas de resolucion fiscal e ICA
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 9/Ene/2024
! Se actualiza la fecha de resolución de autorretenedores
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%ENVIRON T		                 																							! Ambiente de terminal

String    Global UE.EPS.CAPITACION$, vend.medica$, nom.oper$, 						 \!
                 Asc.DSC.DEPARTME$, CFVERSION$,                            \!
                 Gr.NoTax.LstItm$(2), NO.FAC$																!
Integer*2 Global ASC.DSC.TARIFA%, Asc.DSC.INDICAT1													!
Integer*4 Global Asc.Tmp.Apun%        																			! Apuntador Lectura Strings                 
Integer*1 Global Gr.Tul.Rif%, OFERTA%, Gr.NoTax.Proc%, Gr.Fis.Doc%					!
Integer*4 Global Gr.Tul.Monto%, ASC.MKP.DSCT%, ASC.MKP.DSCTO%,             \!
                 Gr.Escl.ItmVl%, Gr.Itm.DscItm%, Gr.NoTax.Cont%,           \!
                 Gr.Motor2.Xpos%, Gr.Fis.Nlin%                              !
Integer*1 Global Gr.Mrm.Cap%, Gr.Itm.Habadia%, Gr.UeTarifa%									!
Integer*1 Global Gr.FeNat.Aplicado%                                         ! Si beneficio aplicado
String    Global Gr.Motor2.Items(2)   																			! Dsctos motor promociones
String    Global Gr.Fis.DatQr$(1)																						! Datos para QR
String    Global Gr.Fis.Empresa$, Gr.Fis.Nit$, Gr.Fis.Direc$, Gr.Fis.City$  ! Variables fiscales 
String    Global Gr.Mnf.Fac$
           
%INCLUDE EAMTSWKG.J86                                                       ! working storage                   
%INCLUDE EAMTRANS.j86                                                       ! Variables procesos de transaccion 
%INCLUDE EAMTERMS.J86                                                       ! Variables procesos terminal       
%INCLUDE EAMITEMR.J86                                                       ! Variables procesos terminal       
%INCLUDE EAMTOPTS.J86                                                       ! Variables Terminal Options        
%INCLUDE EAMTSEVA.J86                                                       ! Variables Cliente Frecuente       
%INCLUDE GRIVTSVA.011	                                                      ! Variables desarrollo              
%INCLUDE GRITEMR.J86        	                                              ! Variables Itemr                   
%INCLUDE EAMTSXHC.J86                                                       !                                   
%INCLUDE EAMASMRT.J86                                                       !                                   
%INCLUDE RECATSSU.011        																								! Rutinas Genericas

Sub TSPREC01  External																											! Rutina de impresion SMA
End Sub

Sub TSPREC03  External																											! Rutima de MSG SMA
End Sub

Sub TSHIECET External								    																		!  Tono de alerta
End Sub

Function MATCHB(P1$,P2$,P3) EXTERNAL
  Integer*2 MATCHB
  String P1$
  String P2$
  Integer*2 P3
End Function

Function FORMAT.AMOUNT(AMT1) External 																			! Formateo de valores
 Integer*1 FORMAT.AMOUNT
 Integer*4 AMT1
End Function

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                      *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                         *!
  STRING    DATA$      !* String to hold the data read from file.            *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                               *!
End Sub 

Sub SPLIT.ITEMFILE (RECORD$) External
  STRING    RECORD$    !* The record (including the key) read from file      *!
End Sub 

Sub IRRFEC.READ02 (SESS.NO, LOCK.IT) External
  INTEGER*2 SESS.NO    !* SESSION number to be used.                         *!
  INTEGER*2 LOCK.IT    !* Lock the record ?                                  *!
End Sub 

Sub IRRFEC.SPLIT1 (RECORD$) EXTERNAL
  STRING    RECORD$    !* The record (including the key) read from file      *!
End Sub 


Sub FORMAT.PESO ( WGT ) External
  Integer*4 WGT
End Sub 

Sub ARMAR.ITEM																															! Arma Item impresion 
    TS.TEMP2$ = IR.ITEMCODE$                                       					! Item a consultar
    Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)                					! Lectura Datos Itemr
    CALL SPLIT.ITEMFILE(TS.TEMP1$)																					! Extrae data
    If UnPack$(GR.INDICAT2$) <> "05" Then Begin 														! Si codigo de barras
       Ue.Iva.Item$ = GR.ITEMCODE$																					!
    EndIf Else Begin 																												!
	     Ue.Iva.Item$ = GR.SALEQUAN$ + GR.SALEPRIC$														!
    EndIf 																																	!
    Ue.Iva.Name$ = GR.ITEMNAME$																							!
    Gr.Iva.Iconsumo% = GETN4(GR.USERDATA$,16)                     					! Valor del impoconsumo
End Sub 

Sub CABECERA.FISCAL.TIQUETE

 If UE.NIT = 1 Then Begin 
	    Gr.FeNat.Aplicado% = -1                                       				! Si beneficio aplicado
      UE.NIT = 0
      To.USEREXIT(20) = 0 
      To.USEREXIT(60) = 0
      TS.TEMP1$ = Left$(Gr.Fis.Empresa$+String$(38," "),38)
      Call U.Imprime(TS.TEMP1$,6100H)
      TS.TEMP1$ = Left$(Gr.Fis.Nit$+String$(38," "),38)
      Call U.Imprime(TS.TEMP1$,6100H)
      TS.TEMP1$ = Left$(Gr.Fis.Direc$+String$(38," "),38)
      Call U.Imprime(TS.TEMP1$,6100H)
      TS.TEMP1$ = Left$(Gr.Fis.City$+String$(38," "),38)
      Call U.Imprime(TS.TEMP1$,6100H)
      TS.TEMP1$ = "  CANT       PRECIO     UM     TOTAL  "
      Call U.Imprime(TS.TEMP1$,6100H)																				! Detalle tiquete
      To.USEREXIT(20) = -1
      To.USEREXIT(60) = -1

 EndIf

End Sub 

Sub CALCULO.IMPTO.ICUI																											! Calculo Impto ICUI
String Xplu$, Xvta$, X.Tmp$, XBIN$, Xkey$, Xdpto$
Integer*1 Xind%, Xtar%
Integer*4 Xqty%, Xind1%, Pric%, XTotBag%, XTcomp%, Xtqty%, XI1%, XTAX%,    \!
          Gr.Tmp.Tax%(1), Gbase%, Gicui%, Giva%, XK%
  Dim Gr.Tmp.Tax%(10)		  		 																			        ! acumulacion por tarifa iva
  Gr.Iva.TaxIcui% = 0																												! Valor impuesto icui
  XBIN$ = ""
  XTotBag% = 0 : Xind% = 0
  XTcomp% = 0 : Xtqty% = 0
  If Gr.Iva.IndIcui% <> -1 Then Exit Sub 																		! Proyecto apagado
  For I% = 1 TO SL.END                                 											! FOR ALL StringS
    H$ = READ.SL.STR$(I%)                              											! GET String
    If LEN(H$) > 5 Then Begin                          										  ! ASSURE GOOD String
     If ASC(H$) = 1 Then Begin                         											! ITEM ENTRY String
     	  Asc.Tmp.Apun% = 3																										!
     	  Xplu$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)      												! Plu
     	  Xvta$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)      												! Precio
     	  Xdpto$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)      												! Departamento
     	  X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)      												! Family 
     	  X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)      												! Indicat1
     	  Xind% = 0
        Xkey$ = Pack$( Right$("000000000000"+Xplu$,12) )                    ! Llave de busqueda
        Call IRRFEC.READ01(Xkey$, 4, TS.TEMP1$, 0)                					! Lectura Datos Itemr
        Call IRRFEC.SPLIT1( TS.TEMP1$ ) 	                          				! Entrega datos al eamitemr.j86
     	 If (IR.INDICAT1A AND 08H) And (Val(Xdpto$) < 800) Then Begin         ! Es un Item con ICUI
     		Xind% = -1 
     	  Xind1% = Val(X.Tmp$)																								! Inicat tax iva
        L% = ASC(RIGHT$(H$,1)) / 10H                   											! GET ITEM TYPE       
        PRIC% = Val(Xvta$)                                                  ! ASSUME NO PRICE
        If L% = 0 OR L% = 8 Or L% = 2 Then Begin                            ! Item SALE OR CANCEL              
           K% = MATCHB(":",H$,3) + 1                                        ! START OF PRICE                   
           J% = MATCHB(":",H$,K%)                                           ! END OF PRICE                     
           If J% > K% Then Begin                                            ! PRICE FOUND
             PRIC% = PACKBIN4(H$,K%-1,J%-K%)                                ! GET PRICE
             If (L% = 8 Or L% = 2) Then PRIC% = 0 - PRIC%                   ! INVERT PRICE And QTY For VOID
           EndIf    																												!
           Xtar% = 8																												! Tarifa iva default  
           XI1% = Xind1%                              	   	      					! Retorna indicat1    
           IF (XI1% AND 01H) Then XBIN$ = "1" Else XBIN$ = "0"		          !                     
           For XK% = 1 TO 7                                      	          !                     
               XI1% = SHIFT(XI1%,1)                   	                    ! Inicializa sig. BIT 
               IF (XI1% AND 01H) Then XBIN$ = "1" + XBIN$                  \!                      
                   Else XBIN$ = "0" + XBIN$                      	          !                       
           Next XK%                                                          !                      
           If LEFT$(XBIN$,4) = "1000" Then XTAR% = 1		                    ! Tax Plan A          
           If LEFT$(XBIN$,4) = "0100" Then XTAR% = 2		                    ! Tax Plan B          
           If LEFT$(XBIN$,4) = "0010" Then XTAR% = 3		                    ! Tax Plan C          
           If LEFT$(XBIN$,4) = "0001" Then XTAR% = 4		                    ! Tax Plan D          
           If LEFT$(XBIN$,4) = "1100" Then XTAR% = 5		                    ! Tax Plan AB         
           If LEFT$(XBIN$,4) = "0110" Then XTAR% = 6		                    ! Tax Plan BC         
           If LEFT$(XBIN$,4) = "0011" Then XTAR% = 7		                    ! Tax Plan CD         
           If LEFT$(XBIN$,4) = "0000" Then XTAR% = 8		                    ! Tax Plan All OFF    
           Gr.Tmp.Tax%(XTAR%)= Gr.Tmp.Tax%(XTAR%) + Pric%                   ! Acumula por tarifa
                      
        EndIf                                                               ! PRICE FOUND                      
      EndIf																																	! PLU CON ICUI
     EndIf                                                                  ! ITEM SALE OR CANCEL              
    EndIf                                                                   ! Item ENTRY Extend       
  Next I%                                                                   ! NEXT String                      
!-- Reporte descuentos en transaccion
  J% = 0
  For J% = 1 To Gr.Motor2.Xpos%																							! Total promociones aplicadas
    Xkey$ = Pack$( Right$("000000000000"+Gr.Motor2.Items(J%,0),12) )        ! Llave de busqueda
    Call IRRFEC.READ01(Xkey$, 4, TS.TEMP1$, 0)                							! Lectura Datos Itemr
    Call IRRFEC.SPLIT1( TS.TEMP1$ ) 	                          						! Entrega datos al eamitemr.j86
 	  If (IR.INDICAT1A AND 08H) And ( Val(UnPack$(IR.DEPARTME$)) < 800 ) Then Begin ! Es un Item con ICUI
           Xtar% = 8																												! Tarifa iva default  
           XI1% = IR.INDICAT1                          	   	      					! Retorna indicat1    
           IF (XI1% AND 01H) Then XBIN$ = "1" Else XBIN$ = "0"		          !
           I% = 0
           For I% = 1 TO 7                                      	          !                     
               XI1% = SHIFT(XI1%,1)                   	                    ! Inicializa sig. BIT 
               IF (XI1% AND 01H) Then XBIN$ = "1" + XBIN$                  \!                      
                   Else XBIN$ = "0" + XBIN$                      	          !                       
           Next I%                                                          !                      
           If LEFT$(XBIN$,4) = "1000" Then XTAR% = 1		                    ! Tax Plan A          
           If LEFT$(XBIN$,4) = "0100" Then XTAR% = 2		                    ! Tax Plan B          
           If LEFT$(XBIN$,4) = "0010" Then XTAR% = 3		                    ! Tax Plan C          
           If LEFT$(XBIN$,4) = "0001" Then XTAR% = 4		                    ! Tax Plan D          
           If LEFT$(XBIN$,4) = "1100" Then XTAR% = 5		                    ! Tax Plan AB         
           If LEFT$(XBIN$,4) = "0110" Then XTAR% = 6		                    ! Tax Plan BC         
           If LEFT$(XBIN$,4) = "0011" Then XTAR% = 7		                    ! Tax Plan CD         
           If LEFT$(XBIN$,4) = "0000" Then XTAR% = 8		                    ! Tax Plan All OFF    
           Pric% = Val(Gr.Motor2.Items(J%,1))
 	  	     Gr.Tmp.Tax%(XTAR%)= Gr.Tmp.Tax%(XTAR%) - Pric%                   ! Acumula por tarifa
    EndIf																																		!
  Next J%

!-- Fin descuentos
  I% = 0
  For I% = 1 To 8																														! Recorre tarifas IVA
    If Gr.Tmp.Tax%(I%) <> 0 Then Begin																			! Si venta en tarifa
       Xtax% = UE.TABLAIVA(I%) + Gr.Iva.PtgIcui%                            ! Tarifa IVA + % ICUI
       Xtcomp% = Gr.Tmp.Tax%(I%)
       TS.TEMP1$ = Str$(ROUND(FLOAT(XTcomp%)/(1.+FLOAT(Xtax%)/100.),0,0))   ! Base de la compra
       Gbase% = Val(TS.TEMP1$)                                              ! Valor Base compra
       Gicui% = (Gbase% * Gr.Iva.PtgIcui%) / 100                            ! Valor del ICUI
       Giva%  = Gr.Tmp.Tax%(I%) - (Gicui%+Gbase%)                           ! Valor del IVA
       
       UE.TOTALS(I%)  = UE.TOTALS(I%) + (Gbase% + Giva%)                    ! Acumula compra
       Gr.Iva.TaxIcui% = Gr.Iva.TaxIcui% + Gicui%                           ! Total ICUI
    EndIf 																																	! fin venta tarifa
  Next I%																																		! fin tarifas iva

End Sub 

Sub CALCULO.IMPTO.BOLSAS Public 																						! Calculo impuesto bolsa
String Xplu$, Xvta$, X.Tmp$, XBIN$
Integer*1 Xind%, Xtar%
Integer*4 Xqty%, Xind1%, Pric%, XTotBag%, XTcomp%, Xtqty%, XI1%

  Gr.Iva.TaxBol% = 0																												! Valor impuesto bolsas
  XBIN$ = ""
  XTotBag% = 0 : Xind% = 0
  XTcomp% = 0 : Xtqty% = 0
  If Gr.Iva.IndBol% <> -1 Then Exit Sub 																		! Proyecto apagado
  For I% = 1 TO SL.END                                 											! FOR ALL StringS
    H$ = READ.SL.STR$(I%)                              											! GET String
    If LEN(H$) > 5 Then Begin                          										  ! ASSURE GOOD String
     If ASC(H$) = 1 Then Begin                         											! ITEM ENTRY String
     	  Asc.Tmp.Apun% = 3																										!
     	  Xplu$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)      												! Plu
     	  Xvta$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)      												! Precio
     	  X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)      												! Departamento
     	  X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)      												! Family 
     	  X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)      												! Indicat1
     	  Xind% = 0
     	If Val(Xplu$) = Gr.Iva.PluBol% Then Begin
     		Xind% = -1 
     	  Xind1% = Val(X.Tmp$)
        L% = ASC(RIGHT$(H$,1)) / 10H                   											! GET ITEM TYPE       
        If L% = 0 OR L% = 8 Or L% = 2 Then Begin                            ! Item SALE OR CANCEL              
           PRIC% = 0                                                        ! ASSUME NO PRICE                  
           K% = MATCHB(":",H$,3) + 1                                        ! START OF PRICE                   
           J% = MATCHB(":",H$,K%)                                           ! END OF PRICE                     
           If J% > K% Then Begin                                            ! PRICE FOUND
             PRIC% = PACKBIN4(H$,K%-1,J%-K%)                                ! GET PRICE
             Xqty% = 1																											! DEFAULT QTY 1
             If (L% = 8 Or L% = 2) Then PRIC% = 0 - PRIC% : Xqty% = -1      ! INVERT PRICE And QTY For VOID
             XTcomp% = XTcomp% + Pric%																			! Total venta bolsas  
             XTqty% = Xtqty% + Xqty%																				! Total unidades vendidas
           EndIf    																												!
        EndIf                                                               ! PRICE FOUND                      
      EndIf																																	! PLU BOLSAS ENCONTRADO
     EndIf                                                                  ! ITEM SALE OR CANCEL              

     If ASC(H$) = 2 And Xind% = -1 Then Begin            										! ITEM ENTRY Extend
     	  Asc.Tmp.Apun% = 3 																								  ! Init apuntador String 
        X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! Mpgroup
        X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! DealQ
        X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! SaleQ
        X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! SalePric
        X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! QtyorWeitgh
        X.tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													    ! QtyorWeitgh
        If Pric% < 0 Then X.tmp$ = "-" + X.tmp$															!
        XTqty% = Xtqty% - Xqty%
        XTqty% = Xtqty% + Val(X.tmp$)																				! Ajusta cantidades
     EndIf																																	! Fin Extension Item
    EndIf                                                                   ! Item ENTRY Extend       
  Next I%                                                                   ! NEXT String                      
  Gr.Iva.TaxBol% = Gr.Iva.VlrBol% * XTqty%                                  ! Total Impuesto bolsas
  XTcomp% = XTcomp% - Gr.Iva.TaxBol%																				! Total valor venta bolsas
  Xtar% = 8																																	! Tarifa iva default
  XI1% = Xind1%                              	   	          								! Retorna indicat1
  IF (XI1% AND 01H) Then XBIN$ = "1" Else XBIN$ = "0"		                    !
  For I% = 1 TO 7                                      		                  !
      XI1% = SHIFT(XI1%,1)                   	                              ! Inicializa sig. BIT
      IF (XI1% AND 01H) Then XBIN$ = "1" + XBIN$                           \!
          Else XBIN$ = "0" + XBIN$                      		                !
  Next I%                                                                   !
  If LEFT$(XBIN$,4) = "1000" Then XTAR% = 1		                              ! Tax Plan A
  If LEFT$(XBIN$,4) = "0100" Then XTAR% = 2		                              ! Tax Plan B
  If LEFT$(XBIN$,4) = "0010" Then XTAR% = 3		                              ! Tax Plan C
  If LEFT$(XBIN$,4) = "0001" Then XTAR% = 4		                              ! Tax Plan D
  If LEFT$(XBIN$,4) = "1100" Then XTAR% = 5		                              ! Tax Plan AB
  If LEFT$(XBIN$,4) = "0110" Then XTAR% = 6		                              ! Tax Plan BC
  If LEFT$(XBIN$,4) = "0011" Then XTAR% = 7		                              ! Tax Plan CD
  If LEFT$(XBIN$,4) = "0000" Then XTAR% = 8		                              ! Tax Plan All OFF

  UE.TOTALS(Xtar%)  = UE.TOTALS(Xtar%) + XTcomp%														! Acumula venta para calculo iva
  
End Sub 

Sub Impresion.Detalle.Iva Public
Integer*4 Gbase%, Giva%, Gsale%, Gcon%
    If Gr.Fis.Doc% = -1 Then Exit Sub 
    Call CALCULO.IMPTO.BOLSAS																								! Calculo impto bolsas
    Call CALCULO.IMPTO.ICUI																									! Calculo ICUI
    Dim Gr.Fis.DatQr$(10)																										! Datos para QR
    Call U.IMPRIME("----   RESUMEN TARIFAS DE IMPTOS  ----",6100H)
    Call U.IMPRIME("Tipo    Compra    Base Consumo   Impto",6100H)
    Gr.Total.Base%  = 0
    Gr.Total.Impto% = 0
    Gr.Total.Consumo% = 0
    FOR K% = 1 TO 3
     Gr.Ind.Iva$ = MID$("ABCDEFGH",K%,1)                      
    IF UE.TOTALS(K%) <> 0 THEN BEGIN 
       !-- Tipo de Impuesto
       TS.TEMP1$ = RIGHT$("00"+Str$(UE.TABLAIVA(K%)),2)
       TS.TEMP1$ = Gr.Ind.Iva$+"="+TS.TEMP1$+"%"
       PrtLine = RIGHT$("     "+TS.TEMP1$,5)+" "
       !-- Valor Compra 
       TS.TEMP1$ = Str$(UE.TOTALS(K%))
       PrtLine = PrtLine + RIGHT$("        "+TS.TEMP1$,8)+" "
       !-- Base de la compra
       Gsale% = (UE.TOTALS(K%) - UE.CONSUMO(K%))
       TS.TEMP1$ = Str$(ROUND(FLOAT(Gsale%)/(1.+FLOAT(UE.TABLAIVA(K%))/100.),0,0))
       PrtLine = PrtLine + RIGHT$("       "+TS.TEMP1$,7)+" "
       Gbase% = Val(TS.TEMP1$)
       !-- Valor Impoconsumo
       TS.TEMP1$ = Str$(UE.CONSUMO(K%))
       PrtLine = PrtLine + RIGHT$("       "+TS.TEMP1$,7)+" "
       Gcon% = UE.CONSUMO(K%)
       !-- Valor del Impuesto
       Giva% = UE.TOTALS(K%) - (Gbase%+Gcon%)
       TS.TEMP1$ = Str$(Giva%)
       PrtLine = PrtLine + RIGHT$("       "+TS.TEMP1$,7)
       Call U.IMPRIME(PRTLINE,6100H)
       Gr.Total.Base%    = Gr.Total.Base%  + Gbase%
       Gr.Total.Impto%   = Gr.Total.Impto% + Giva%
       Gr.Total.Consumo% = Gr.Total.Consumo% + UE.CONSUMO(K%)
       
    ENDIF
    NEXT K%

    FOR K% = 5 TO 8
    IF UE.TOTALS(K%) <> 0 THEN BEGIN 
       TS.TEMP1$ = MID$("ABCDEFGH",K%,1)                      
       !-- Tipo de Impuesto
       IF K% = 7 THEN TS.TEMP1$ = "G=EXC "                                  ! Item Excluido
	     IF K% = 8 THEN TS.TEMP1$ = "H=EXE "                                  ! Item Excento

       !F K% = 7 THEN TS.TEMP1$ = "H=EXC " ELSE TS.TEMP1$ = "G=EXE "
	
       PrtLine = TS.TEMP1$

       !-- Valor Compra 
       TS.TEMP1$ = Str$(UE.TOTALS(K%))
       PrtLine = PrtLine + RIGHT$("        "+TS.TEMP1$,8)+" "
       !-- Base de la compra
       Gsale% = (UE.TOTALS(K%) - UE.CONSUMO(K%))
       TS.TEMP1$ = Str$(ROUND(FLOAT(Gsale%)/(1.+FLOAT(UE.TABLAIVA(K%))/100.),0,0))
       PrtLine = PrtLine + RIGHT$("       "+TS.TEMP1$,7)+" "
       Gbase% = Val(TS.TEMP1$)
       !-- Valor Impoconsumo
       TS.TEMP1$ = Str$(UE.CONSUMO(K%))
       PrtLine = PrtLine + RIGHT$("       "+TS.TEMP1$,7)+" "
       Gcon% = UE.CONSUMO(K%)
       !-- Valor del Impuesto
       Giva% = UE.TOTALS(K%) - (Gbase%+Gcon%)
       TS.TEMP1$ = Str$(Giva%)
       PrtLine = PrtLine + RIGHT$("       "+TS.TEMP1$,7)
       
       Call U.IMPRIME(PRTLINE,6100H)
       Gr.Total.Base%    = Gr.Total.Base%  + Gbase%
       Gr.Total.Impto%   = Gr.Total.Impto% + Giva%
       Gr.Total.Consumo% = Gr.Total.Consumo% + UE.CONSUMO(K%)
    ENDIF
    NEXT K%
    TS.TEMP3$ = RIGHT$("       "+Str$(Gr.Total.Consumo%),7)
    TS.TEMP2$ = RIGHT$("       "+Str$(Gr.Total.Base%),7)
    TS.TEMP1$ = RIGHT$("       "+Str$(Gr.Total.IMPTO%),7)
    Call U.IMPRIME("Totales        "+TS.TEMP2$+" "+TS.TEMP3$+" "+TS.TEMP1$,6100H)

!-- Se maneja tarifa 4 de IVA para manejo de IMPUESTO CONSUMO

    IF UE.TOTALS(4) <> 0 THEN BEGIN 
    	 !-- Tipo de Impuesto
       TS.TEMP1$ = "D=INC " + RIGHT$("00"+Str$(UE.TABLAIVA(4)),2) + "%"
       PrtLine = TS.TEMP1$
       !-- Valor Compra 
       TS.TEMP1$ = Str$(UE.TOTALS(4))
       PrtLine = PrtLine + RIGHT$("      "+TS.TEMP1$,6)+" "
       !-- Base de la compra
       Gsale% = (UE.TOTALS(4) - UE.CONSUMO(4))
       TS.TEMP1$ = Str$(ROUND(FLOAT(Gsale%)/(1.+FLOAT(UE.TABLAIVA(4))/100.),0,0))
       PrtLine = PrtLine + RIGHT$("       "+TS.TEMP1$,7)+" "
       Gbase% = Val(TS.TEMP1$)
       !-- Valor Impoconsumo
       TS.TEMP1$ = Str$(UE.CONSUMO(4))
       PrtLine = PrtLine + RIGHT$("       "+TS.TEMP1$,7)+" "
       Gcon% = UE.CONSUMO(4)
       !-- Valor del Impuesto
       Giva% = UE.TOTALS(4) - (Gbase%+Gcon%)
       TS.TEMP1$ = Str$(Giva%)
       PrtLine = PrtLine + RIGHT$("       "+TS.TEMP1$,6)

       Call U.IMPRIME(PRTLINE,6100H)
    ENDIF
!--Mod 20Abr20 - Decreto 551 del 2020 (Coronavirus)
!--Mod 23Ene2023 - Se retira impresion linea
!    If UE.TOTALS(7) <> 0 Then Begin
!     	 Call U.Imprime("BIENES EXENTOS DECRETO 417 17/MZO/2020",6100H)
!    EndIf
!    If UE.TOTALS(9) <> 0 Then Begin      ! Impresion del Impoconsumo
!    	 Call Format.Amount(UE.TOTALS(9))
!    	 Call U.IMPRIME("IMPUESTO CONSUMO "+TS.TEMP1$,6100H)
!    EndIf  

!-- Mod 14/Jul/2023 - Impresion impuesto bolsas

     TS.TEMP2$ = Left$("IMPUESTO CONSUMO BOLSAS"+String$(29," "),29)
  	 Call Format.Amount(Gr.Iva.TaxBol%)
     TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)
     TS.TEMP1$ = Left$(TS.TEMP2$+TS.TEMP1$+String$(38," "),38)
   	 Call U.IMPRIME(TS.TEMP1$,6100H)

!-- Fin Lineas Adicionadas    

!-- Mod 17/Oct/2023 - Impresion impuesto ultraprocesados
  	 Call Format.Amount(Gr.Iva.TaxIcui%)																		! Total impuesto ICUI
     TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)													! formatea valor
     TS.TEMP2$ = Left$(Gr.Iva.DscIcui$+String$(29," "),29)                  ! Descriptor impuesto
     TS.TEMP1$ = Left$(TS.TEMP2$+TS.TEMP1$+String$(38," "),38)
   	 Call U.IMPRIME(TS.TEMP1$,6100H)				                    						! Imprime dato tiquete
     TS.TEMP4I4 = TS.TOTALS(0,0,0) - (Gr.Total.Impto% + Gr.Total.Consumo% + Gr.Iva.TaxIcui% + Gr.Iva.TaxBol% + Giva% )
     Gr.Fis.DatQr$(6) = Str$(TS.TEMP4I4)																		! Total Base compra de la compra
     Gr.Fis.DatQr$(7) = Str$(Gr.Total.Impto%)																! Valor del IVA
     Gr.Fis.DatQr$(8) = Str$(Giva% + Gr.Total.Consumo% + Gr.Iva.TaxIcui% + Gr.Iva.TaxBol% ) ! Otros impuestos
     Gr.Fis.DatQr$(9) = Str$(TS.TOTALS(0,0,0))															! Total de la compra
     TS.TEMP2$ = Left$("FORMA DE PAGO:  CONTADO"+String$(38," "),38)        ! Ajuste descriptor
     Call U.IMPRIME(TS.TEMP2$,6100H)                                        ! Forma de pago
!-- Fin Lineas Adicionadas    

End Sub 

Sub Datos.Fiscal.Items Public 
String Xtmp$
Integer*2 Xiva%, Xico%, Xtip%
Integer*4 Xvlr1%, Xvlr2%
     IF ((Not TS.RECOVERY) And (Not TS.RETV.IN.PROGRESS)) Then Begin
       TS.TEMP5$ = "00"																					   					! Signo de la operacion
       If (SL.IT.INDICAT3A = 8) Then TS.TEMP5$ = "01"              					! Si es una anulacion 
       If (SL.IT.INDICAT3A = 2) Then TS.TEMP5$ = "01"              					! Si es una anulacion 
       Xiva% = UE.TABLAIVA(Gr.UeTarifa%)																		! Tarifa Iva
       If Gr.UeTarifa% = 4 Then Begin                                       ! Es impoconsumo
       	  Xico%  = UE.TABLAIVA(Gr.UeTarifa%)																! Tarifa impuesto
       	  Xiva% = 0
       EndIf Else Xico% = 0																									! Default no tiene impoconsumo
       Xtip% = 0																														!
       Xvlr1% = TS.XPRICE																										! Valor total venta producto
       If TS.XPRICE < 0 Then Xvlr1% = TS.XPRICE * -1                        ! Si dato negativo
       If Gr.UeTarifa% = 8 Then Xtip% = 1																		! Si es excento
       TS.TEMP1$ = Str$(ROUND(FLOAT(Xvlr1%)/(1.+FLOAT(UE.TABLAIVA(Gr.UeTarifa%))/100.),0,0)) ! Base de impto
       TS.TEMP2$ = Str$(Xvlr1% - Val(TS.TEMP1$))                            ! Valor del IVA
       Xtmp$ = Pack$(SL.IT.ITEMCODE$)                     + ":" + 				 \! SKU Vendido     
               Pack$(Str$(SL.IE.SALEPRIC))                + ":" +          \! Precio Unitario
               Pack$(Str$(Xvlr1%))                        + ":" +          \! Venta Total
               Pack$(Str$(SL.IE.QTYORWGT))                + ":" +          \! Cantidad o Peso
               Pack$(Str$(Xiva%))                         + ":" +          \! Tarifa de IVA Producto 
               Pack$(Str$(Xico%))                         + ":" +          \! Tarifa de Impoconsumo
               Pack$(Str$(Xtip%))                         + ":" +          \! Si es Excento "0" o Excluido "1"
               Pack$(TS.TEMP1$)                           + ":" +          \! Base Impuesto
               Pack$(TS.TEMP2$)                           + ":" +          \! Valor del Impuesto
               Pack$(TS.TEMP5$)                                             ! Signo operacion "00" Venta "01" Anulacion
               
       !Call Grabacion.String.Usuario2("20221102", Xtmp$)                    ! Almacena String
       
       Call Grabacion.Cadena.Usuario2("20221102", Xtmp$)                    ! Almacena String
     EndIf

End Sub 

Sub FECO.OBSERVACIONES Public 
!-- Lineas Propias de Comfandi
    To.USEREXIT(20) = 0 
    To.USEREXIT(60) = 0
    TS.TEMP1$ = STRING$(38,"-")
    Call U.IMPRIME(TS.TEMP1$,6100H)
    TS.TEMP1$ = Left$("OBSERVACIONES:"+STRING$(38," "),38)
    Call U.IMPRIME(TS.TEMP1$,4100H)
    Call U.IMPRIME(TS.TEMP1$,2100H)
    TS.TEMP1$ = Left$("ATENDIDO POR: "+Gr.Oper.Name$+STRING$(38," "),38)
    Call U.IMPRIME(TS.TEMP1$,6100H)
    IF Len(vend.medica$) > 1 THEN BEGIN
       Call U.IMPRIME("DESPACHO: " +" "+vend.medica$+" "+nom.oper$,6100H)
       Call Grabacion.String.Usuario2("20080407",PACK$(vend.medica$)+":"+PACK$(STR$(ITEM.COUNT)))
    EndIf   
!--- Impresion mensaje Habadia
   If Gr.Itm.Habadia% = -1 Then Begin
   	  TS.TEMP1$ = Left$("FACTURADO EN NOMBRE DE PAPELERIA"+STRING$(38," "),38)
   	  Call U.IMPRIME(TS.TEMP1$,6100H)
   	  TS.TEMP1$ = Left$("HABADIA NIT 800.148.615-8"+STRING$(38," "),38)
   	  Call U.IMPRIME(TS.TEMP1$,6100H)
   EndIf
   
      If Gr.Fis.Linea1$ <> String$(41," ") Then \
      Call U.IMPRIME(Gr.Fis.Linea1$,6100H)
      If Gr.Fis.Linea2$ <> String$(41," ") Then \
      Call U.IMPRIME(Gr.Fis.Linea2$,6100H)
      If Gr.Fis.Linea3$ <> String$(41," ") Then \
      Call U.IMPRIME(Gr.Fis.Linea3$,6100H)
      If Gr.Fis.Linea4$ <> String$(41," ") Then \
      Call U.IMPRIME(Gr.Fis.Linea4$,6100H)
      If Gr.Fis.Linea5$ <> String$(41," ") Then \
      Call U.IMPRIME(Gr.Fis.Linea5$,6100H)
      If Gr.Fis.Linea6$ <> String$(41," ") Then \
      Call U.IMPRIME(Gr.Fis.Linea6$,6100H)
      If Gr.Fis.Linea7$ <> "" Then \
      Call U.IMPRIME(Gr.Fis.Linea7$,6100H)
      If Gr.Fis.Linea8$ <> "" Then \
      Call U.IMPRIME(Gr.Fis.Linea8$,6100H)
      If Gr.Fis.Linea9$ <> "" Then \
      Call U.IMPRIME(Gr.Fis.Linea9$,6100H)    

!--- Add 03Jul2024
       Call U.IMPRIME("AUTORETENEDORES DE RENTA RES 1660 DE  ",6100H)
       Call U.IMPRIME("07-03-2000 GRANDES CONTRIBUYENTES RES.",6100H)
       Call U.IMPRIME("00200 DE 27-12-2024 AGENTE RETENEDOR  ",6100H)
       Call U.IMPRIME("DE IVA NO REALIZAR RETENCION DE ICA EN",6100H)
       Call U.IMPRIME("BUGA,CARTAGO,ARMENIA,GINEBRA,JAMUNDI, ",6100H)
       Call U.IMPRIME("TULUA, JAMUNDI, POPAYAN, BUGALAGRANDE ",6100H)
       Call U.IMPRIME("POPAYAN,BUGALAGRANDE,PEREIRA,PALMIRA Y",6100H)
       Call U.IMPRIME("CALI POR SER AUTORRETENEDORES Y TENER ",6100H)
       Call U.IMPRIME("CONDICIONES ESPECIALES.               ",6100H)
       Call U.IMPRIME("VIGILADO SUPERSUB. FAMILIAR-SUPERSALUD",6100H)

       Call U.IMPRIME("TOSHIBA GLOBAL COMMERCE SOLUTIONS     ",6100H)
       Call U.IMPRIME("(COLOMBIA) S.A.S NIT 900545074        ",6100H)
       Call U.IMPRIME("SUPERMARKET APPLICATION               ",6100H)
       Call U.IMPRIME("PROVEEDOR TECNOLOGICO: CARVAJAL TECNO-",6100H)
       Call U.IMPRIME("LOGIA Y SERVICIOS S.A.S BIC           ",6100H)
       Call U.IMPRIME("NOMBRE DEL SW: CEN FINANCIERO         ",6100H)
       Call U.IMPRIME("NIT: 890.321.151-0                    ",6100H)
       CALL U.IMPRIME(CFVERSION$,6100H)                											! Version Compilacion App Caja
       To.USEREXIT(20) = -1 
       To.USEREXIT(60) = -1

End Sub 


Sub IVATSU01.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  LEALTAD DE CLIENTES                                  **
!   **    User Exit  :  IVATSU01.011                                         **
!   **    Inclu¡r en :  EAMTSU01.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

DIM UE.TOTALS(9)			! RESET ACUMULADO POR IVAS
DIM UE.CONSUMO(9)			! RESET ACUMULADO IMPOCONSUMO
UE.TRX.ANUL% = 0			! INDICA SI TRX ANULADA
!UE.NIT = 1

Call CABECERA.FISCAL.TIQUETE  ! DATOS CABECERA FISCAL

End Sub

Sub IVATSU02.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  LEALTAD DE CLIENTES                                  **
!   **    User Exit  :  IVATSU01.011                                         **
!   **    Inclu¡r en :  EAMTSU01.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

Dim UE.TOTALS(9)			! TOTALES USUARIO POR IVA
DIM UE.CONSUMO(9)			! RESET ACUMULADO IMPOCONSUMO
USER.FBACT.READ = 0
Gr.Iva.Rtrx% = 0
UE.NIT = 1
Gr.Itm.Habadia% = 0
Gr.Fis.Nlin% = 0																														! Numero lineas registradas
End Sub

Sub IVATSU28.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  LEALTAD DE CLIENTES                                  **
!   **    User Exit  :  IVATSU01.011                                         **
!   **    Inclu¡r en :  EAMTSU01.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
   
TS.ER.RETURN = -1 
OPEN "R::$AMOPERA" KEYED RECL 72 AS 94 UNLOCKED NOWRITE NODEL    !Read operator file
IF TS.ER.RETURN = -1 THEN BEGIN 
   READ  FORM "C5,C67";#94 KEY TS.OPER$;\
  	     A$,Gr.Oper.Name$
   Gr.Oper.Name$ = MID$(Gr.Oper.Name$,28,20)  	     
   Close 94 
ENDIF ELSE Gr.Oper.Name$ = "CAJERO(A)"

End Sub 


Sub IVATSU07.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  LEALTAD DE CLIENTES                                  **
!   **    User Exit  :  IVATSU01.011                                         **
!   **    Inclu¡r en :  EAMTSU01.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

DIM UE.TABLAIVA(10)
DIM UE.TOTALS(9)				! TOTALES USUARIO POR IVA
DIM UE.CONSUMO(9)			! RESET ACUMULADO IMPOCONSUMO
OPTION DECIMAL ","  
UE.NIT = 1 
UE.LINEA = 0

TS.ER.RETURN = -1
Open "R::$ABLAIVA" AS 94 UNLOCKED NOWRITE NODEL 
IF TS.ER.RETURN NE -1 THEN BEGIN
   Call VISORES4690(1,"ERROR APERTURA IVA","...",1500,"C")
   GOTO SALIDA.IVA
ENDIF 
IF END # 94 THEN  CONT.IVA
LEER.IVA:
   READ # 94 ;  UE.PORCENTAJE
!   IF UE.PORCENTAJE <> 0 THEN BEGIN

     UE.LINEA = UE.LINEA + 1    
     UE.TABLAIVA(UE.LINEA) = UE.PORCENTAJE
     
!   ENDIF
GOTO LEER.IVA 
CONT.IVA:
  CLOSE 94

SALIDA.IVA:

TS.ER.RETURN = -1																													! Ctrl Errores
OPEN "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL 														! Apertura archivo parametrizacion
If TS.ER.RETURN <> -1 THEN BEGIN																					! Error en la apertura
 Call TSHIECET																														! Tono de Alerta
 Call Visores4690(1,"Error Parametros","Tiquete Fiscal     ",1500,"L")		! Msg Alerta
 Call U.IMPRIME("MODULO LINEAS FISCAL  OFF 04/Ene/2013",2100H)            ! Msg Proyecto Cargado

 EndIf Else Begin		
 IF END #94 THEN UE.LIN.FINAL		       																	  ! Si es EOF
  While (1)															  																! Recorre archivo
    Read #94; Line TS.TEMP1$			 																				! Lectura registro
    IF TS.TEMP1$ = "[LINEAS FISCALES]" Then Begin					   	   					! Proyecto de Lealtad
     Read #94; Line TS.TEMP1$           																				! Lectura registro
     Gr.Fis.Linea1$ = Mid$(TS.TEMP1$,30,41)															! Toma registro
     Read #94; Line TS.TEMP1$           																				! Lectura registro
     Gr.Fis.Linea2$ = Mid$(TS.TEMP1$,30,41)															! Toma registro
     Read #94; Line TS.TEMP1$           																				! Lectura registro
     Gr.Fis.Linea3$ = Mid$(TS.TEMP1$,30,41)															! Toma registro     
     Read #94; Line TS.TEMP1$           																				! Lectura registro
     Gr.Fis.Linea4$ = Mid$(TS.TEMP1$,30,41)															! Toma registro     
     Read #94; Line TS.TEMP1$           																				! Lectura registro
     Gr.Fis.Linea5$ = Mid$(TS.TEMP1$,30,41)															! Toma registro
     Read #94; Line TS.TEMP1$           																				! Lectura registro
     Gr.Fis.Linea6$ = Mid$(TS.TEMP1$,30,41)															! Toma registro        
     Read #94; Line TS.TEMP1$           																	      ! Lectura registro
     Gr.Fis.Linea7$ = Mid$(TS.TEMP1$,30,41)															! Toma registro   
     Read #94; Line TS.TEMP1$           																	      ! Lectura registro
     Gr.Fis.Linea8$ = Mid$(TS.TEMP1$,30,41)															! Toma registro   
     Read #94; Line TS.TEMP1$           																	      ! Lectura registro
     Gr.Fis.Linea9$ = Mid$(TS.TEMP1$,30,41)															! Toma registro     

!--  Variables adicionales para manejo de mensaje de rifa en Tulua 
!--  Desarrollado por Grupo retail julio 12 de 2.010 - OVS
     
     Read #94; Line TS.TEMP1$           																	      ! Lectura registro
     Read #94; Line TS.TEMP1$           																	      ! Lectura registro
     Gr.Tul.Rif%   = Val(Mid$(TS.TEMP1$,30,2)) 													  ! Proyecto Activo
     Read #94; Line TS.TEMP1$           																	      ! Lectura registro
     Gr.Tul.Monto% = Val(Mid$(TS.TEMP1$,30,10))													  ! Valor minimo compra
     
     Gr.Fis.Linea1$ = LEFT$(Gr.Fis.Linea1$+STRING$(38," "),38)
     Gr.Fis.Linea2$ = LEFT$(Gr.Fis.Linea2$+STRING$(38," "),38)
     Gr.Fis.Linea3$ = LEFT$(Gr.Fis.Linea3$+STRING$(38," "),38)
     Gr.Fis.Linea4$ = LEFT$(Gr.Fis.Linea4$+STRING$(38," "),38)          
     Gr.Fis.Linea5$ = LEFT$(Gr.Fis.Linea5$+STRING$(38," "),38)          
     Gr.Fis.Linea6$ = LEFT$(Gr.Fis.Linea6$+STRING$(38," "),38)
     Gr.Fis.Linea7$ = LEFT$(Gr.Fis.Linea7$+STRING$(38," "),38)
     Gr.Fis.Linea8$ = LEFT$(Gr.Fis.Linea8$+STRING$(38," "),38)     
     Gr.Fis.Linea9$ = LEFT$(Gr.Fis.Linea9$+STRING$(38," "),38)   
     GoTo UE.LIN.FINAL
   Endif
 Wend
 UE.LIN.FINAL:
   Close 94																																	! Cierra archivo
 TS.ER.RETURN = -1																													! Ctrl Errores
 Gr.Iva.IndBol% = 0
 Gr.Iva.TaxBol% = 0
 Gr.Iva.PluBol% = 0
 Gr.Iva.VlrBol% = 0
 Open "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL 														! Apertura archivo parametrizacion
 If TS.ER.RETURN <> -1 THEN BEGIN																						! Error en la apertura
  Call TSHIECET																															! Tono de Alerta
  Call Visores4690(1,"Error Parametros","Impuesto Bolsas    ",1500,"L")			! Msg Alerta
 EndIf Else Begin		
  IF END #94 THEN UE.BOL.FINAL		       																	  ! Si es EOF
  While (1)															  																	! Recorre archivo
    Read #94; Line TS.TEMP1$			 																					! Lectura registro
    If TS.TEMP1$ = "[BOLSAS IMPUESTO]" Then Begin					   	   					  ! Proyecto IMPUESTO BOLSAS
       Read #94; Line TS.TEMP1$         																		! Lectura registro
       Gr.Iva.IndBol% = Val(Mid$(TS.TEMP1$,30,02))  											  ! Proyecto activo
       Read #94; Line TS.TEMP1$         																		! Lectura registro
       Gr.Iva.PluBol% = Val(Mid$(TS.TEMP1$,30,12))  											  ! Plu Bolsas
       Read #94; Line TS.TEMP1$         																		! Lectura registro
       Gr.Iva.VlrBol% = Val(Mid$(TS.TEMP1$,30,05))  											  ! Valor impuesto bolsa
       GoTo UE.BOL.FINAL																										! Sale proceso carga parametros
    EndIf
  Wend   
 EndIf
 UE.BOL.FINAL:
   Close 94																																	! Cierra archivo

 TS.ER.RETURN = -1																													! Ctrl Errores
 Gr.Iva.IndIcui% = 0	 		  																								! Activación proyecto ICUI     
 Gr.Iva.TaxIcui% = 0      																									! Valor impuesto Icui en trx  
 Gr.Iva.PtgIcui% = 0        																								! Porcentaje impuesto ICUI    

 Open "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL 														! Apertura archivo parametrizacion
 If TS.ER.RETURN <> -1 THEN BEGIN																						! Error en la apertura
  Call TSHIECET																															! Tono de Alerta
  Call Visores4690(1,"Error Parametros","Impuesto ICUI      ",1500,"L")			! Msg Alerta
 EndIf Else Begin		
  IF END #94 THEN UE.ICUI.FINAL		       																	  ! Si es EOF
  While (1)															  																	! Recorre archivo
    Read #94; Line TS.TEMP1$			 																					! Lectura registro
    If TS.TEMP1$ = "[IMPUESTO ICUI]" Then Begin					   	   					    ! Proyecto IMPUESTO ICUI
       Read #94; Line TS.TEMP1$         																		! Lectura registro
       Gr.Iva.IndIcui% = Val(Mid$(TS.TEMP1$,30,02))  											  ! Proyecto activo
       Read #94; Line TS.TEMP1$         																		! Lectura registro
       Gr.Iva.PtgIcui% = Val(Mid$(TS.TEMP1$,30,03))  											  ! porcentaje impuesto ICUI
       Read #94; Line TS.TEMP1$         																		! Lectura registro
       Gr.Iva.flgIcui$ = Mid$(TS.TEMP1$,30,01)  											  		! flag indicador impuesto tiquete
       Read #94; Line TS.TEMP1$         																		! Lectura registro
       Gr.Iva.DscIcui$ = Mid$(TS.TEMP1$,30,20)  											  		! descriptor impuesto
       GoTo UE.ICUI.FINAL																										! Sale proceso carga parametros
    EndIf
  Wend   
 EndIf
 UE.ICUI.FINAL:
   Close 94																																	! Cierra archivo

   
   !Call U.IMPRIME("MODULO LINEAS FISCAL   ON 04/Ene/2013",2100H)            ! Msg Proyecto Cargado
   !Call U.IMPRIME("MODULO LINEAS FISCAL   ON 20/Abr/2020",2100H)            ! Msg Proyecto Cargado
   !Call U.IMPRIME("MODULO LINEAS FISCAL   ON 02/Nov/2022",2100H)            ! Msg Proyecto Cargado
   !Call U.IMPRIME("MODULO LINEAS FISCAL   ON 13/Jul/2023",2100H)            ! Msg Proyecto Cargado
   Call U.IMPRIME("MODULO LINEAS FISCAL   ON 17/Oct/2023",2100H)            ! Msg Proyecto Cargado
Endif

Call IVATSU28.011

End Sub

Sub IVATSU08.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  LEALTAD DE CLIENTES                                  **
!   **    User Exit  :  IVATSU01.011                                         **
!   **    Inclu¡r en :  EAMTSU01.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
String     Gr.Tmp$
Integer*4  Gr.Tmp1%, Gr.Tmp2%

  Call ARMAR.ITEM
!--- Rutina coloca al cupon ligado el codigo de departamento e iva del producto
IF IR.ITEMTYPE < 6 Then Begin
    UE.INDI1% = IR.INDICAT1                              	   	!
    IF (UE.INDI1% AND 01H) Then          			               \!
        UE.BIN$ = "1" ELSE UE.BIN$ = "0"                 		  !
     FOR I% = 1 TO 7                                      		!
       UE.INDI1% = SHIFT(UE.INDI1%,1)                   	    ! Inicializa sig. BIT
       IF (UE.INDI1% AND 01H) Then UE.BIN$="1"+UE.BIN$       \!
           ELSE UE.BIN$ ="0"+UE.BIN$                      		!
     NEXT I%                                               		!
     ASC.DSC.TARIFA% = 8					 	                          ! Tarifa Default
     If LEFT$(UE.BIN$,4) = "1000" Then                       \! Si hay Indic Plan A
        ASC.DSC.TARIFA% = 1		                               \! indica Plan = A
     	Else                                                   \! si no,
     If LEFT$(UE.BIN$,4) = "0100" Then                       \! Si hay Indic Plan B
        ASC.DSC.TARIFA% = 2		                               \! indica Plan = B
      Else                                                   \! si no,
     If LEFT$(UE.BIN$,4) = "0010" Then                       \! Si hay Indic Plan C
        ASC.DSC.TARIFA% = 3		                               \! indica Plan = C
      Else                                                   \! si no,
     If LEFT$(UE.BIN$,4) = "0001" Then                       \! Si hay Indic Plan D
        ASC.DSC.TARIFA% = 4		                               \! indica Plan = D
      Else                                                   \! si no,
     If LEFT$(UE.BIN$,4) = "1100" Then                       \! Si hay Indic Plan AB
        ASC.DSC.TARIFA% = 5		                               \! indica Plan = E
      Else                                                   \! si no,
     If LEFT$(UE.BIN$,4) = "0110" Then                       \! Si hay Indic Plan BC
        ASC.DSC.TARIFA% = 6		                               \! indica Plan = F
      Else                                                   \! si no,
     If LEFT$(UE.BIN$,4) = "0011" Then                       \! Si hay Indic Plan CD
        ASC.DSC.TARIFA% = 7		                               \! indica Plan = G
      Else                                                   \! si no,
     IF LEFT$(UE.BIN$,4) = "0000" Then                       \! 
        ASC.DSC.TARIFA% = 8		                                ! indica Plan H
    
    Asc.DSC.INDICAT1  = IR.INDICAT1					                  ! Toma tipo de iva
    
    !ASC.DSC.DEPARTME$ = IR.DEPARTME$					                ! y departamento para asignarlo al cupon
    
    ASC.DSC.INDICAT1  = ASC.DSC.TARIFA%	  		                ! Asigna tarifa calculada            
 EndIf Else If IR.ITEMTYPE > 6 Then Begin                     ! Si es un cupon
     
     !   IR.DEPARTME$ = ASC.DSC.DEPARTME$	  		              ! Asigna departamento del articulo
     
        If Asc.DSC.INDICAT1 = 1 Then IR.INDICAT1 = 82H        ! Le asigna la tarifa de IVA
        If Asc.DSC.INDICAT1 = 2 Then IR.INDICAT1 = 42H
        If Asc.DSC.INDICAT1 = 3 Then IR.INDICAT1 = 22H
        If Asc.DSC.INDICAT1 = 4 Then IR.INDICAT1 = 12H
        If Asc.DSC.INDICAT1 = 5 Then IR.INDICAT1 = 0C2H
        If Asc.DSC.INDICAT1 = 6 Then IR.INDICAT1 = 62H
        If Asc.DSC.INDICAT1 = 7 Then IR.INDICAT1 = 32H
        If Asc.DSC.INDICAT1 = 8 Then IR.INDICAT1 = 92H
 EndIf 
End Sub


Sub IVATSU20.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  LEALTAD DE CLIENTES                                  **
!   **    User Exit  :  IVATSU01.011                                         **
!   **    Inclu¡r en :  EAMTSU01.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
Integer*4 Gr.Tmp1%, Gr.Tmp2%, Gr.SPric%, Gr.SUpric%, Gr.Sqty%
String    XTMP$, Xicui$
!--- IVASTS20.A11

If Gr.Mrm.Cap% = -1 Then Exit Sub                                           ! En remision

If Ts.Linetype = 7 And Ts.Linedata = 1 Then Begin
   A$ = Right$(Ts.PrtBuf$,12)
   Ts.PrtBuf$ = "==>> SUBTOTAL/TOTAL      :$" + A$
   Exit Sub 
EndIf

If TS.LINETYPE = 22 THEN UE.TRX.ANUL% = 1  !  Prende bandera de letrero de NIT 
  
!--- IVASTS20.B11
IF TS.LINETYPE <= 1 Then Begin 			\! TS.LINEDATA = 1 
  IF (TS.LINEDATA = 1 ) AND (RIGHT$(TS.PRTBUF$,4)="    ") THEN GOTO UE.SIGA	! PARA EVITAR DUPLICIDAD DE ACUMULACION
  UE.SGN% = 1
  IF SL.IT.INDICAT2 AND 80H  THEN UE.SGN% = -1
  IF SL.IT.INDICAT2 AND 40H  THEN UE.SGN% = -1
  IF SL.IT.INDICAT2 = 6      THEN UE.SGN% = -1
  IF SL.IT.INDICAT2 = 2054   THEN UE.SGN% = -1
  If SL.IT.INDICAT3A = 5     THEN UE.SGN% = -1         ! Devolucion Miscelaneo Add 14 Sep 2010
  	
  IF SL.IT.INDICAT3A = 7 Then \
		UE.SGN% = UE.SGN% * (-1)

  If (TS.IO.KEYS(1) = 70 OR TS.IO.KEYS(8) = 67 ) Then Begin ! Si una anulacion
  	If SL.IT.INDICAT3A = 5 Then UE.SGN% = 1         	 ! Controla signo anulacion 29 Jul 2015 GR
  EndIf

  ! Acumula el monto venta a su respectivo IVA
  UE.INDI1% = SL.IT.INDICAT1                           !
  IF (UE.INDI1% AND 01H) THEN                         \!
      UE.BIN$ = "1" ELSE UE.BIN$ = "0"                 !
  FOR I% = 1 TO 7                                      !
       UE.INDI1% = SHIFT(UE.INDI1%,1)                  ! Inicializa sig. BIT
       IF (UE.INDI1% AND 01H) THEN UE.BIN$="1"+UE.BIN$\!
           ELSE UE.BIN$ ="0"+UE.BIN$                   !
  NEXT I%                                              !
  UE.TARIFA% = 8    																	 ! Tarifa por default
  If Left$(UE.BIN$,4) = "1000" Then UE.TARIFA% = 1     ! Si hay Indic Plan A  Letra A
  If Left$(UE.BIN$,4) = "0100" Then UE.TARIFA% = 2     ! Si hay Indic Plan B  Letra B
  If Left$(UE.BIN$,4) = "0010" Then UE.TARIFA% = 3     ! Si hay Indic Plan C  Letra C
  If Left$(UE.BIN$,4) = "0001" Then UE.TARIFA% = 4     ! Si hay Indic Plan D  Letra D
  If Left$(UE.BIN$,4) = "1100" Then UE.TARIFA% = 5     ! Si hay Indic Plan AB Letra E
  If Left$(UE.BIN$,4) = "0110" Then UE.TARIFA% = 6     ! Si hay Indic Plan BC Letra F
  If Left$(UE.BIN$,4) = "0011" Then UE.TARIFA% = 7     ! Si hay Indic Plan CD Letra G
  If Left$(UE.BIN$,4) = "0000" Then UE.TARIFA% = 8     ! Si hay Indic Plan A  Letra H
                      
   Xicui$ = " "
   Gr.Ind.Iva$ = MID$("ABCDEFGH",UE.TARIFA%,1)                  
   Gr.UeTarifa% = UE.TARIFA%
   UE.TOTALS(9) = UE.TOTALS(9) + (Gr.Iva.Iconsumo% * UE.SGN%)  							! Acumulacion Impoconsumo
  
!--- Validacion Item Bolsas
  If Val(SL.IT.ITEMCODE$) = Gr.Iva.PluBol% Then GoTo UE.SIGA                ! No acumula IVA
!--- Fin validacion Item Bolsas  

!-- Si UE Option 1 activa no acumula IVA   	                                ! Add 14/Oct/2023
  If (IR.INDICAT1A AND 08H) And (SL.IT.DEPARTME < 800) Then Begin           ! No son concesiones o recaudos
  	 Xicui$ = Gr.Iva.flgIcui$ 																							! Marcador impuesto icui
  	 GoTo UE.SIGA                     																			! No acumula IVA
  EndIf
!--- Fin validacion Impto ICUI

!--- Validacion Venta items Habadia
  If SL.IT.DEPARTME = 103 Or SL.IT.DEPARTME = 104 Then Begin
  	 Gr.Itm.Habadia% = -1
  EndIf
!--- Fin items Habadia
  
  ON UE.TARIFA% GOSUB IV1,IV2,IV3,IV4,IV5,IV6,IV7,IV8
  UE.TARIFA% = 0
  GOTO UE.SIGA

  IV1:
    UE.TOTALS(1)  = UE.TOTALS(1)  + (UE.SGN% * SL.IT.XPRICE)
    UE.CONSUMO(1) = UE.CONSUMO(1) + (Gr.Iva.Iconsumo% * UE.SGN%)
  Return
  IV2:
    UE.TOTALS(2)  = UE.TOTALS(2)  + (UE.SGN% * SL.IT.XPRICE)
    UE.CONSUMO(2) = UE.CONSUMO(2) + (Gr.Iva.Iconsumo% * UE.SGN%)
  Return
  IV3:
    UE.TOTALS(3)  = UE.TOTALS(3)  + (UE.SGN% * SL.IT.XPRICE)
    UE.CONSUMO(3) = UE.CONSUMO(3) + (Gr.Iva.Iconsumo% * UE.SGN%)
  Return
  IV4:
    UE.TOTALS(4)  = UE.TOTALS(4)  + (UE.SGN% * SL.IT.XPRICE)
    UE.CONSUMO(4) = UE.CONSUMO(4) + (Gr.Iva.Iconsumo% * UE.SGN%)
  Return
  IV5:
    UE.TOTALS(5)  = UE.TOTALS(5)  + (UE.SGN% * SL.IT.XPRICE)
    UE.CONSUMO(5) = UE.CONSUMO(5) + (Gr.Iva.Iconsumo% * UE.SGN%)
  Return
  IV6:
    UE.TOTALS(6)  = UE.TOTALS(6)  + (UE.SGN% * SL.IT.XPRICE)
    UE.CONSUMO(6) = UE.CONSUMO(6) + (Gr.Iva.Iconsumo% * UE.SGN%)
  Return
  IV7:
    UE.TOTALS(7)  = UE.TOTALS(7)  + (UE.SGN% * SL.IT.XPRICE)
    UE.CONSUMO(7) = UE.CONSUMO(7) + (Gr.Iva.Iconsumo% * UE.SGN%)
  Return
  IV8:
    UE.TOTALS(8)  = UE.TOTALS(8)  + (UE.SGN% * SL.IT.XPRICE)
    UE.CONSUMO(8) = UE.CONSUMO(8) + (Gr.Iva.Iconsumo% * UE.SGN%)
  Return
  
  
  UE.SIGA:
  If SL.IT.DEPARTME > 800 Then Begin																				! Items de recaudo
     Xicui$ = " "																														! Mod 8Nov2023 GR
     Gr.Ind.Iva$ = " "																											!
  EndIf																																			! 
EndIf 

!--- PLUTSU20.011

If Match("**  **",TS.PRTBUF$,1) > 0 Then Begin 																! Control de cupones de tienda
   TS.PRTBUF$ = ""																														!
   TS.FORMCR$ = "C38 A0" 																											!
   Exit Sub 																																	!
EndIf 																																				!

If TS.LINETYPE = 5 And TS.LINEDATA = 3 Then Begin															! Venta items pesados
   TS.PRTBUF$ = ""																														! Limpia Buffer de impresion
   TS.FORMCR$ = "C38 A0" 
   Exit Sub 
EndIf
 
If TS.LINETYPE  = 1 Then Begin																								! Linea de Item
 If TS.LINEDATA = 0 Then Begin																								! Item Sin Cantidad
     Gr.Fis.Nlin% = Gr.Fis.Nlin% + 1																					! Nro linea impresion
     If (IR.INDICAT0 And 40H) Then Begin																			! Item pesable
     	   TS.TEMP6$ = "KLG" 																										! Unidad de medida
     	   Call Format.Peso(SL.IE.QTYORWGT)																			! Formateo pesable
         TS.TEMP5$ = Right$("      "+TS.TEMP1$,6)                             ! Ajsute campo
     EndIf Else Begin																													! Item unitario
     	   TS.TEMP6$ =" UD"                                                     ! Unidad de medida
     	   Call Format.Amount(SL.IE.QTYORWGT)																  	! Formateo unidades
     	   TS.TEMP5$ = Right$("      "+TS.TEMP1$,6)                             ! Ajuste campo     	   
     EndIf
     Call Format.Amount(SL.IE.SALEPRIC)																				! Formateo precio unitario
     TS.TEMP2$ = Right$("          "+TS.TEMP1$,10)														! Ajuste tiquete
     Call Format.Amount(TS.XPRICE)																						! Formateo precio total
     TS.TEMP3$ = Right$("          "+TS.TEMP1$,10)														! Ajuste tiquete
     TS.TEMP6$ = TS.TEMP5$ + " X " + TS.TEMP2$ + "   " + TS.TEMP6$ + " " +   \!
                 TS.TEMP3$                                                    !
     Call U.IMPRIME(TS.TEMP6$,6100H)																			    !
 	   TS.TEMP3$  = UnPack$(Ue.Iva.Item$)																				! Codigo del Item
 	   TS.TEMP3$  = Str$(Val(TS.TEMP3$))																				! Elimina Ceros izqierda
 	   TS.TEMP2$  = Right$("       "+TS.TEMP3$,7)																! Ajusta dato Impresion
     TS.TEMP6$  = Right$("000"+Str$(Gr.Fis.Nlin%),3)                          ! Nro Linea 
 	   TS.TEMP5$ = Left$(IR.ITEMNAME$+String$(24," "),24)                       !
 	   TS.TEMP2$  = TS.TEMP6$ + " " + TS.TEMP2$ + " " + TS.TEMP5$ +            \! # LINEA + DESCRIPCION
 	                " " + Gr.Ind.Iva$	+ Xicui$        													! tax flag
 	   TS.TEMP6$  = Left$(TS.TEMP2$+String$(40," "),40)													! Ajusta Tiquete
     Call U.IMPRIME(TS.TEMP6$,6100H)																			    !
 	   TS.PRTBUF$ = ""																											    ! Limpia Buffer de impresion
     TS.FORMCR$ = "C38 A0" 
 	   Call Datos.Fiscal.Items																									! UD datos de Item
  EndIf Else Begin																														! Linea de Item Con Cantidad
   If Right$(TS.PRTBUF$,8) <> "        " Then Begin                           ! 
     Gr.Fis.Nlin% = Gr.Fis.Nlin% + 1																					! Nro linea impresion
     If (IR.INDICAT0 And 40H) Then Begin																			! Item pesable
     	   TS.TEMP6$ = "KLG" 																										! Unidad de medida
     	   Call Format.Peso(SL.IE.QTYORWGT)																			! Formateo pesable
         TS.TEMP5$ = Right$("      "+TS.TEMP1$,6)                             ! Ajsute campo
     EndIf Else Begin																													! Item unitario
     	   TS.TEMP6$ =" UD"                                                     ! Unidad de medida
     	   Call Format.Amount(SL.IE.QTYORWGT)																  	! Formateo unidades
     	   TS.TEMP5$ = Right$("      "+TS.TEMP1$,6)                             ! Ajuste campo     	   
     EndIf
     Call Format.Amount(SL.IE.SALEPRIC)																				! Formateo precio unitario
     TS.TEMP2$ = Right$("          "+TS.TEMP1$,10)														! Ajuste tiquete
     Call Format.Amount(TS.XPRICE)																						! Formateo precio total
     TS.TEMP3$ = Right$("          "+TS.TEMP1$,10)														! Ajuste tiquete
     TS.TEMP6$ = TS.TEMP5$ + " X " + TS.TEMP2$ + "   " + TS.TEMP6$ + " " +   \!
                 TS.TEMP3$                                                    !
     Call U.IMPRIME(TS.TEMP6$,6100H)																			    !
 	   TS.TEMP3$  = UnPack$(Ue.Iva.Item$)																				! Codigo del Item
 	   TS.TEMP3$  = Str$(Val(TS.TEMP3$))																				! Elimina Ceros izqierda
 	   TS.TEMP2$  = Right$("       "+TS.TEMP3$,7)																! Ajusta dato Impresion
     TS.TEMP6$  = Right$("000"+Str$(Gr.Fis.Nlin%),3)                          ! Nro Linea 
 	   TS.TEMP5$ = Left$(IR.ITEMNAME$+String$(24," "),24)                       !
 	   TS.TEMP2$  = TS.TEMP6$ + " " + TS.TEMP2$ + " " + TS.TEMP5$ +            \! # LINEA + DESCRIPCION
 	                " " + Gr.Ind.Iva$	+ Xicui$        													! tax flag
 	   TS.TEMP6$  = Left$(TS.TEMP2$+String$(40," "),40)													! Ajusta Tiquete
     Call U.IMPRIME(TS.TEMP6$,6100H)																			    !
 	   TS.PRTBUF$ = ""																											    ! Limpia Buffer de impresion
     TS.FORMCR$ = "C38 A0" 
 	   Call Datos.Fiscal.Items																									! UD datos de Item
 	 EndIf Else Begin 
 	   	 TS.PRTBUF$ = ""																												! Limpia Buffer de impresion
       TS.FORMCR$ = "C38 A0" 
 	 EndIf 
  EndIf 
EndIf
!--- Fin impresion plu productos 

!--- Mensaje de Boletas rifa tulua 12 Jul 2010 - Grupo retail OVS
IF TS.PROCEDURE < 1 AND TS.INTRX AND \
   TS.LINETYPE = 6 AND TS.LINEDATA = 1 \
   AND UE.TRX.ANUL% = 0            THEN \ ! Si es Store data line
  BEGIN
   If (Gr.Tul.Rif% = -1) And (Gr.Tul.Monto% > 0) Then Begin                 ! Proyecto Activo
    If TS.TOTALS(0,0,0) >= Gr.Tul.Monto% Then BEGIN                         ! Monto minimo de compra
       TS.TEMP3$ = Str$( TS.TOTALS(0,0,0) / Gr.Tul.Monto% )                 ! Numero de Boletas
       Call Grabacion.String.Usuario2("34",PACK$(TS.TEMP3$)+":"+PACK$("00"))! Almacena user data para reporte       
       TS.TEMP3$ = Right$("          "+TS.TEMP3$,10)                        ! Ajusta dato
       Call VISOR.AND.BORRAR("ENTREGAR  "+TS.TEMP3$+"BOLETAS AL CLIENTE")   ! Mensaje al cajero
    ENDIF 
   EndIf 
ENDIF 


! Imprime detalle del IVA

If (TS.INTRX) AND (TS.LINETYPE = 6 AND TS.LINEDATA = 1) And  \
   (TS.TENDERED (0) <> 0 Or TS.TRX.STATUS <> 100) Then Begin
    Call Impresion.Detalle.Iva																							! Detalle IVAS Compra
EndIf

End Sub


Sub IVATSU20.012 PUBLIC
If Not TS.TRAINING Then Begin                                               ! Si no en entrenamiento  
 If TS.INTRX Then                                                          \! Si esta en una compra   
   If TS.LINETYPE = 6 And                                                  \! Store Data y Transacc   
      TS.LINEDATA = 1 Then Begin                                            ! Fecha, Hora, etc        
       If TS.TENDERED (0) <> 0 Or                                          \! Si hay pagos            
          TS.TRX.STATUS <> 100 Then Begin                                   ! y NO hay VOID en curso  
           If Gr.Fis.Doc% = -1 Then Exit Sub
           If Gr.Mnf.Fac$ <> ""    Then Exit Sub 														! Proceso MF 
           Call FECO.OBSERVACIONES
       EndIf
   EndIf
EndIf

End Sub 

Sub IVATSU21.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  LEALTAD DE CLIENTES                                  **
!   **    User Exit  :  IVATSU01.011                                         **
!   **    Inclu¡r en :  EAMTSU01.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If Ts.Linetype = 7 and Ts.Linedata = 1 Then Begin
   A$ = Right$(Ts.PrtBuf$,12)
   Ts.PrtBuf$ = "==>> SUBTOTAL/TOTAL      :$" + A$
   TS.SJDATA$ = "==>> SUBTOTAL/TOTAL      :$" + A$
Endif

!If Match("@",TS.PRTBUF$,1) > 0 Then Begin 
!   TS.PRTBUF$ = ""
!EndIf 

If Ucase$(Left$(TS.PRTBUF$,2)) = "WT" Then Begin 
   TS.PRTBUF$ = ""
EndIf 


End Sub

Sub IVATSU23.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  LEALTAD DE CLIENTES                                  **
!   **    User Exit  :  IVATSU01.011                                         **
!   **    Inclu¡r en :  EAMTSU01.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
   
  %Include PLUTSU23.011

End Sub 


Sub IVATSU53.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  LEALTAD DE CLIENTES                                  **
!   **    User Exit  :  IVATSU01.011                                         **
!   **    Inclu¡r en :  EAMTSU01.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If Gr.Iva.Rtrx% = 0 Then Begin 
   Call IVATSU28.011
   Gr.Iva.Rtrx% = -1
EndIf 

   
If UnPack$(Left$(SL.STR.ENTRY$,1)) = "01" Then Begin 		        ! Si es vta articulo
   Asc.Tmp.Apun% = 3																					  ! Apuntador String                   
   A$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	     	 		    ! Numero del proyecto                
   Ue.Iva.Item$ = Pack$(Right$(String$(12,"0")+A$,12)) 		      ! CODIGO PLU O EAN
   Gr.Vtas.IndAnul%   =  1 																			! Venta 
   IR.ITEMCODE$ = Ue.Iva.Item$
   Call ARMAR.ITEM                                               ! Buscar datos del producto
EndIf


End Sub 

Sub IVATSU56.011 Public

Dim UE.TOTALS(9)			! TOTALES USUARIO POR IVA
DIM UE.CONSUMO(9)			! RESET ACUMULADO IMPOCONSUMO
USER.FBACT.READ = 0
Gr.Iva.Rtrx% = 0
UE.NIT = 1

End Sub 
