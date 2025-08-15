!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMFENAT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Factura Electronica       *
!                Personas naturales - Comfandi    *
!                integración Expressmed           *
!**************************************************
! Modificaciones:
! ----- Version 1.0 10 Junio 2022 -----------------
! Mod 24Ene2024
! Se adiciona el manejo de numero y valor del UVT 
! para calculo de generación de FE para personas 
! naturales o juridicas a consumidor final
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 15May2023
! Se controla en procesos de reintregros fuera de 
! tiqute no se genere factura fiscal si la venta es 
! menos o igual a cero.
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 12Jul2023
! Se adiciona numero de documento minifactura para 
! facturas con prefijos no fiscales (000-004)
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%Environ T																																	! Ambiente terminal

Integer*1 Global Gr.FeNat.Ok%,                                             \! Control proyecto
                 Gr.FeNat.Aplicado%,                                       \! Si beneficio aplicado
                 Gr.Fis.Doc%,                                              \! Generacion factura fiscal
                 Gr.FeNat.Proc%,																					 \! proceso appl items
                 Gr.Fenat.Uvt%                                              ! Numero UVT factura electronica
                 
Integer*1 Global Asc.Pay.Impr%                                              !  
Integer*4 Global Gr.FeNat.Cont%,																				   \!
                 Gr.Motor2.Xpos%,                                          \! Total items con promocion
                 Gr.FeNat.Itm%,                                            \!
                 Gr.FeNat.VlrUvt%,                                         \! Valor UVT 
                 Gr.Crd.Remision%
                 
Integer*4 Global UE.TABLAIVA(1)                                             ! Tarifas de IVA
Integer*2 Global ASC.DSC.TARIFA%, Asc.DSC.INDICAT1,                        \!
                 Gr.FeNat.Motora%																						! Tecla motora dialogo
String    Global IR.USERDATA$ 																						  ! Extension maestra productos
String    Global Gr.FeNat.Struc$(2),                                       \! Lista Estructuras vendidas
                 Gr.FeNat.LstItm$(2),                                      \!
                 Gr.Motor2.Items(2),                                       \! Dsctos motor promociones
                 Gr.FeNat.Dpto$,                                           \! Dpto exento iva
                 Gr.FeNat.Vmax$,                                           \!
                 Gr.FeNat.Qmax$,                                           \!
                 Gr.FeNat.Dmax$,                                           \!
                 Gr.FeNat.Fecha$                                            ! Fecha aplicacion evento
Integer*2 GLOBAL UE.RECASRV.QTY%																						! Modulo de recaudos
Integer*1 Global Gr.Mrm.Cap%,                                              \! Modulo remisiones
                 Gr.NtaCrd.InTrx% 																					! Modulo Notas Credito
Integer*1 Global Asc.Eps.Cntrl%																							! Proceso minifactura
String    Global Gr.Motor2.LstPago$                                         ! Lista formas de pago motor
String    Global NO.FAC$, Nite1$, Gr.Mf.Lst$, NITE$													! Nro Doc. Minifacturas
Integer*4 Global Gr.Iva.PtgIcui%                                            ! Porcentaje ICUI
String    Global Gr.Mnf.Fac$
String    Global ItmsNtCrd$(2)  																					  ! Lista Items Nta Crd
Integer*4 Global Gr.NtaCrd.Itm%  																					  ! Total items Nta Crd
 
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

Function Bono.Tarifa.Iva(UE.INDI1%) External
Integer*2 UE.INDI1%, Bono.Tarifa.Iva
End Function 

Sub Asignacion.Consecutivo.Remision External					 !
End Sub 																							 !

Sub Flag.Item.Iva(Xindi1%)	External																				! Calculo flag de iva
Integer*1 Xindi1%           																								!
End Sub 

Function CALCULO.COMPRA.FISCAL Public                                       ! Calculo compra fiscal
  Integer*4 PRIC%, Xvta%, X.K%, CALCULO.COMPRA.FISCAL				                !
  Integer*2 Xp1%, Xp2%, X.TARIFA%, XL%, SP2%																!
  String    X.TMP$, Xpago$, Xclte$, Xstruc$, Xnada$, Xsgn$, Xitm$						!

  Xvta% = 0                                            											! Total Venta
  CALCULO.COMPRA.FISCAL = 0
  For I% = 1 TO SL.END                                 											! FOR ALL StringS
    H$ = READ.SL.STR$(I%)                             											! GET String
    If LEN(H$) > 5 Then                                                     \ ASSURE GOOD String
    If ASC(H$) = 1 Then Begin                         											! ITEM ENTRY String
     X.TMP$ = ASIC.GETUNPK(H$,3)	   				           											! Tomo informacion del item vendido
     X.Tmp$ = Right$("000000000000"+X.Tmp$,12)         											! 
     X.Tmp$ = Pack$( X.tmp$ )                          											! Arma llave de busqueda
     TS.ER.RETURN = -1																 											! Ctrl errores
     Call IRRFEC.READ01 (X.tmp$, 4, TS.TEMP1$, 0) 		 											! Lectura Datos Itemr
     Call IRRFEC.SPLIT1 ( TS.TEMP1$ ) 			           											! Entrega datos al eamitemr.j86
     TS.ER.RETURN = -1                                 											! Ctrl de Error
     TS.TS11WERR$ = ""                                 											!
     X.TARIFA% = Bono.Tarifa.Iva(IR.INDICAT1)          											! Tarifa Impuesto
     L% = ASC(RIGHT$(H$,1)) / 10H                      											! GET ITEM TYPE
     If L% = 0 OR L% = 8 Then Begin                    											! Item SALE OR CANCEL
      PRIC% = 0                                        											! ASSUME NO PRICE
      K% = MATCHB(":",H$,3) + 1                        											! START OF PRICE
      J% = MATCHB(":",H$,K%)                           											! END OF PRICE
      If J% > K% Then Begin                            											! PRICE FOUND
       PRIC% = PACKBIN4(H$,K%-1,J%-K%)                 											! GET PRICE
       PRIC% = ROUND(FLOAT(PRIC%)/(1.+FLOAT(UE.TABLAIVA(X.TARIFA%))/100.),0,0) ! Calcula valor Base
       If L% = 8 Then PRIC% = 0 - PRIC%                											! INVERT PRICE FOR VOID
       Xvta% = Xvta% + PRIC%                           											! Acumula venta
      EndIf                                            											! PRICE FOUND
     EndIf                                             											! ITEM SALE OR CANCEL
    EndIf                                              											! Item ENTRY String
    Next.Itm.Compra:                                   											! Siguiente Item vendido
  Next I%                                              											! NEXT String
  CALCULO.COMPRA.FISCAL = Xvta%                        											! Retorna total de venta en la trx
End Function																																! Fin compra fiscal

Sub Graba.Itm.FeNatural Public 
  Integer*4 PRIC%, TOT4%, XI%, XInd1%, XInd2%, X.J%, Xqty%, Xfind%,        \!
            Gr.Xcont%, Xpric%, Xs%																			          !
  Integer*2 Xsgn%, Xpos%																										!
  String    XTMP$, Xitm$, XPric$, Xdummy$, Xkey$, Xbasura$, X.Ean$, Xcot$   !
  String    Gr.Tmp.Itm$(2), Xbuff$, Xiva$, Xund$, X.Flag$, Gr.Impoc$,      \!
            XDepto$, XPvp$, Xicui$, XDepto1$, XcodStr$, Xdcto$              !
  Gr.Xcont% = 0 																														! Numero de Items
  Dim Gr.Tmp.Itm$(1000,10)				 																			    ! Lista de Items vendidos
  X.J% = 0 
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
        Xfind% = 0																													! Encontrado
     	  Xpos% = 0 : XI% = 0																									! Ctrl busqueta Item 
        If Xpos% = 0 Then Begin                                             ! Nuevo Item
           Gr.Xcont% = Gr.Xcont% + 1												                ! Posicion Almacenar
           Gr.Tmp.Itm$(Gr.Xcont%,0) = Xitm$                                 ! SKU producto
           Gr.Tmp.Itm$(Gr.Xcont%,1) = Str$( Val(Xpric$) * Xsgn%)            ! Precio de Venta Total
           Gr.Tmp.Itm$(Gr.Xcont%,2) = Str$( Xsgn% * 1 )                     ! Cantidad Vendida
           Gr.Tmp.Itm$(Gr.Xcont%,3) = Xiva$                                 ! Tarifa del IVA
           Gr.Tmp.Itm$(Gr.Xcont%,4) = X.flag$                               ! Excento o Excluido
           Gr.Tmp.Itm$(Gr.Xcont%,5) = Gr.Impoc$                             ! Impoconsumo
           Gr.Tmp.Itm$(Gr.Xcont%,6) = Xund$                                 ! Si es pesable o unitario
           Gr.Tmp.Itm$(Gr.Xcont%,7) = IR.ITEMNAME$													! Descripcion Item
           Gr.Tmp.Itm$(Gr.Xcont%,8) = Xdepto$																! Seccion de venta
           Gr.Tmp.Itm$(Gr.Xcont%,9) = "0"      														  ! Total Descuento
           Gr.Tmp.Itm$(Gr.Xcont%,10) = Xicui$ 														  ! Ptg ICUI
           Xpos% = Gr.Xcont%                                                ! Asigna posicion de vector
        EndIf Else Begin																									  ! Item Encontrado
          Gr.Tmp.Itm$(Xpos%,2) = Str$(Val(Gr.Tmp.Itm$(Xpos%,2)) +          \!
        	                                 (Xsgn% * 1))                     ! Cantidad Vendida
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
  X.J% = 0
  XI%  = 0
  For X.J% = 1 To Gr.Motor2.Xpos%																						! Total promociones aplicadas
   For XI% = 1 To Gr.Xcont%
      If Val(Gr.Motor2.Items(X.J%,0)) = Val(Gr.Tmp.Itm$(XI%,0)) Then Begin  ! Plu encontrado
      	 Gr.Tmp.Itm$(XI%,9) = Gr.Motor2.Items(X.J%,1)												! Total dscto aplicado
      EndIf
   Next XI%
  Next X.J%
  X.J% = 0 : XI% = 0
  XcodStr$ = "62"																														! UD venta fiscal
  If Gr.NtaCrd.InTrx% = -1 Then XcodStr$ = "68"															! UD Nta Credito
  TS.TEMP6$ = ""
  For X.J% = 1 To Gr.Xcont%																									!
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
      If Gr.NtaCrd.InTrx% = -1 Then Begin																		! En nota credito
      	 Xs% = 0
      	 For Xs% = 1 To Gr.NtaCrd.Itm%																			! Recorre lista items 
      	  If Val(ItmsNtCrd$(Xs%,0)) = Val(Gr.Tmp.Itm$(X.J%,0)) Then Begin   ! valida items
      	  	 If Val(ItmsNtCrd$(Xs%,6)) > 0 Then Begin                       ! Si tiene dsctos
             	  Gr.Tmp.Itm$(X.J%,9) = ItmsNtCrd$(Xs%,6)                     ! Dscto aplicado
             EndIf																													! Fin con dscto
      	  EndIf
      	 Next Xs%
      EndIf																																	! Fin nota credito
      Xbuff$ = Pack$(Gr.Tmp.Itm$(X.J%,0))                                + \! SKU producto         
               ":" + Pack$(Xpvp$)                                        + \! Precio Unitario
               ":" + Pack$(Xund$)                                        + \! Qty Vendida          
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,3))                          + \! Tasa Impuesto
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,4))                          + \! Excento o Excluido
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,5))                          + \! Impoconsumo
               ":" + Gr.Tmp.Itm$(X.J%,6)                                 + \! Unitario o pesado    
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,8))                          + \! Seccion de la venta
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,9))                          + \! Descuentos aplicados
               ":" + Pack$(Xtmp$)									                       + \! Signo de la operacion
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,10))                            ! Ptg ICUI
        Call Grabacion.Cadena.Usuario2("62",Xbuff$)                         ! UD Items para FE     
        If XcodStr$ = "68" Then Begin																				! Si es una nta crd
           TS.TEMP6$ = TS.TEMP6$ + Gr.Tmp.Itm$(X.J%,0) + ":" + Xund$ +     \! Lista itms devueltos
                       ":" + Xpvp$ + ":"+","                                ! 
        EndIf
        Next.Itm.Feco:																											!
        
  Next X.J%
!--- Mod 12Jul2023
 If Gr.Mnf.Fac$ <> "" Then Begin																					  ! Nro documento minifactura
    Xbuff$ = Pack$(Gr.Mnf.Fac$)		   							                        	! Nro minifactura ingresada
    Call Grabacion.Cadena.Usuario2("63",Xbuff$)                             ! UD para documento MF
 EndIf																																			! Fin doc minifactura

End Sub 

Sub VALIDA.DATOS.FENATURAL																									! VALIDACION DATOS FE NATURAL
Integer*4 Xd1%, Xd2%
String    Xfec$, Xdata$
Integer*1 Xvlclte%

! If Gr.FeNat.Aplicado% = -1 Then Begin                                      ! FE YA APLICADA
! 	  TS.TEMP1I1 = -1  																												!
! 	  Exit Sub 																																!
! EndIf
! TS.TEMP4I4 = CALCULO.COMPRA.FISCAL
! If TS.TEMP4I4 <= 0 Then Begin																							! Es un reintegro de trx 
! 	  TS.TEMP1I1 = 0  																												! No aplica FE
!    Exit Sub 																																! Cliente No ingresado
! EndIf

 If USER.FBACT.READ = 0 Then Begin 																					! Cliente no capturado
 	  Call VISOR.AND.BORRAR("DATOS DE CLIENTE NO  CAPTURADO   /Borrar")				! Msg de alerta
 	  Call VISOR.AND.BORRAR("NO SE GENERA FACTURA ELECTRONICA /Borrar")				!
 	  TS.TEMP1I1 = 0  																												!
 	  Exit Sub 																																! Cliente No ingresado
 EndIf																																			!
 TS.TEMP1I1 = 0  																												    !
 Xvlclte% = -1
 If Val(Gr.Lcl.IdClte$) = 5 Then Begin																			! Tipo cliente mixto fiscal
 	  GR.TIPO.CLTE:
    Xdata$ = asic.datos$("TIPO DE CLIENTE?","1.NATUR. 4.JURIDICO")          !
    TS.TEMP1$ = ";"+XDATA$+";"
    If (Xdata$ = "E") Or (MATCH(TS.TEMP1$,";1;4;",1) <= 0) Then Begin	! Dato errado
  	   Call VISOR.AND.BORRAR("DATO NO VALIDO      /Borrar")							    !
       GoTo GR.TIPO.CLTE
    EndIf																																	  !
    Gr.Lcl.IdClte$ = Xdata$																						      ! Reporta tipo de cliente
 EndIf																																			!
  
 If Gr.Lcl.Surnames$ = "" Or                                               \!
    Gr.Lcl.Surnames$ = String$(20," ") Then Xvlclte% = 0                    ! Nombres del cliente
 If Gr.Lcl.Phone$ = "" Or Gr.Lcl.Phone$ = "               " Then Xvlclte% = 0 ! Telefono del cliente
 If Gr.Lcl.eMail$ = "" Or                                                  \!
    Gr.Lcl.eMail$ = String$(50," ") Then Xvlclte% = 0                       ! Correo electronico cliente

! If Gr.Lcl.Address$  = "" Or                                               \!
!    Gr.Lcl.Address$  = String$(25," ") Then Xvlclte% = 0                    ! Datos incompletos
! If Gr.Lcl.City$ = "" Or                                                   \!
!    Gr.Lcl.City$ = String$(20," ") Then Xvlclte% = 0                        ! Datos incompletos
 
 If Xvlclte% = 0 Then Begin                    															! Datos incompletos
 	  Call VISOR.AND.BORRAR("DATOS DE CLIENTE NO  COMPLETOS   /Borrar")				! Msg de alerta
 	  Call VISOR.AND.BORRAR("NO SE GENERA FACTURA ELECTRONICA /Borrar")				!
 	  TS.TEMP1I1 = 0  																												!
 	  Exit Sub 																																! Cliente No ingresado
 EndIf																																			!
 TS.TEMP1I1 = -1  																												  !
 
End Sub 																																		! fin calculo compra registrada

Function VALIDA.COMPRA.UVT Public 
Integer*1 VALIDA.COMPRA.UVT
       TS.TEMP5I4 = Gr.Fenat.Uvt% * Gr.FeNat.VlrUvt%                        ! Calcula vlr minimo para FE obligatoria
       TS.TEMP4I4 = CALCULO.COMPRA.FISCAL																		! Calculo compra Sin impuestos
       
       TS.TEMP5I4 = TS.TEMP4I4
       
       If (USER.FBACT.READ = 0) And (TS.TEMP4I4 >= TS.TEMP5I4) Then Begin   ! Cliente no capturado y monto minimo FE
     	   Call VISOR.AND.BORRAR("SE DEBE INGRESAR EL ID CLIENTE OBLIGAT.")   !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         VALIDA.COMPRA.UVT = 0                                              ! Falla validacion 
         Exit Function 																									    !
       EndIf																																!
       If Gr.FeNat.Aplicado% = 0 And (TS.TEMP4I4 >= TS.TEMP5I4) Then Begin  !
     	   Call VISOR.AND.BORRAR("TRX OBLIGATORIA PARAFACTURA ELECTRONICA")   !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         VALIDA.COMPRA.UVT = 0                                              ! Falla validacion 
         Exit Function  																								    !
       EndIf
       If Gr.FeNat.Aplicado% = -1 Then Begin																! Si FE activa
         If Gr.Lcl.Surnames$ = "" Or                                       \!
            Gr.Lcl.Surnames$ = String$(20," ") Then Gr.FeNat.Aplicado% = 0  ! Nombres del cliente
         If Gr.Lcl.Phone$ = "" Or Gr.Lcl.Phone$ = "               " Then   \! Telefono del cliente
         	  Gr.FeNat.Aplicado% = 0  																				! 
         If Gr.Lcl.eMail$ = "" Or Gr.Lcl.eMail$ = String$(50," ") Then     \! Correo electronico
         	  Gr.FeNat.Aplicado% = 0  																				! 
         If Gr.FeNat.Aplicado% = 0 Then Begin																! Datos cliente modificados
 	          Call VISOR.AND.BORRAR("DATOS DE CLIENTE NO  COMPLETOS   /Borrar") ! Msg de alerta
 	          Call VISOR.AND.BORRAR("NO SE GENERA FACTURA ELECTRONICA /Borrar")	!
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!
            TS.IO.MOTORKEY = 73																						  !
            VALIDA.COMPRA.UVT = 0                                           ! Falla validacion 
            Exit Function 																								  !
         EndIf
       EndIf																																! Fin si FE activa
       VALIDA.COMPRA.UVT = -1
End Function  

Sub GENERA.REMISION.MFFIS																										!
String Xtmpx$, BARCODE$, ENDBCODE$
! Xtmpx$ = Str$(Val(TS.STORE$)) + Right$("000"+Str$(Val(TS.TERMINAL$)),3) + \!
!          Right$("0000"+Str$(Sl.Hd.Transnum + 1),4) + DATE$ + LEFT$(TIME$,4)
          
 Xtmpx$ = Str$(Val(TS.STORE$)) + Right$("000"+Str$(Val(TS.TERMINAL$)),3) + \!
          Right$("0000"+Str$(Sl.Hd.Transnum + 1),4) + UNPACK$(SL.HD.DATETIME$)
 TS.TEMP2$ = "   REMISION NRO: "+Xtmpx$																			!
 TS.TEMP2$ = Left$(TS.TEMP2$ + STRING$(40," "),40)               						!
 Call U.Imprime(TS.TEMP2$,6100H)                                 						!

!-- Impresion EAN 128
 BARCODE$  = Chr$(1DH) + Chr$(6BH) 					                    						! Manejo de impresion
 ENDBCODE$ = Chr$(00H) 						                              						! codigos de barra
 TS.TEMP1$ = Chr$(1DH)+Chr$(77H)+Chr$(02H)+  		                           \! Horizontal barcode Size
 				     Chr$(1DH)+Chr$(68H)+Chr$(50)+                                 \! Vertical barcode Size 
             Chr$(1DH)+Chr$(48H)+Chr$(02H)+                                \! Human Read characters
  	         Chr$(1DH)+Chr$(66H)+Chr$(01H)    	                  					! activado
 Call U.IMPRIME(TS.TEMP1$,4000H)				                          					!
 TS.TEMP1$ = (BARCODE$+Chr$(07H)+(Xtmpx$)+ENDBCODE$)             						! Arma String de impresion			
 Write #34 ; TS.TEMP1$          					                        					! Imprime dato EAN-128
 TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(0)				                  					! Ajusta impresion a la Izquierda
 Call U.IMPRIME(TS.TEMP1$,4000H)				                          					!                    
!-- Fin impresion EAN 128       
		         
 TS.TEMP1$ = Right$("0000000000000000"+Xtmpx$,16)
 TS.TEMP2$ = "TR+"                                                       + \! Arma Str Usuario con 
             "REMI"+Pack$(TS.TEMP1$)+":"                                 + \!                      
             STR$(ABS(TS.GROSSPOS + TS.GROSSNEG))                        + \! #Fact, Vr/Vta Neto   
             ":" + Pack$("00") + ":" + Pack$("00")                       + \! y Vrs de Iva         
             ":" + Pack$("00") + ":" + Pack$("00")                       + \!                      
             ":" + Pack$("00") + ":" + Pack$("00")                       + \!                      
             ":" + Pack$("00") + ":" + Pack$("00")                          !                      
 Call Grabacion.Cadena.Usuario2("21",TS.TEMP2$)                             !

End Sub 																																		!



Sub GR.FE.NATURAL(Xopt%) Public 																						! Factura electronica personas naturales
Integer*4 Xopt%,XS%, UE.I%																									! UE ejecutar
String    Xdata$, Xtmpx$, BARCODE$, ENDBCODE$
!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.FeNat.Ok%  = 0                                                         ! Proyecto Apagado
  Gr.FeNat.Aplicado% = 0
  TS.TS11WERR$ = ""																												  ! Control errores
  Open "R::$SCNTRL" As 94 UNLOCKED NOWRITE NODEL 													  ! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then Begin                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA FE NATURALES ")									! MSg alerta
  	 Call U.IMPRIME("ERROR APERTURA FE NATURALES  ",6100H)                  !
  	 Exit Sub 																															! Sale del proceso
  EndIf  																																		!
  If End #94 Then UE.FIN.FENAT           																	  ! Si es EOF
  While (1)															  																  ! Recorre archivo parametros
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[FE NATURALES]" Then Begin	   	     				        ! FE Naturales
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.FeNat.Ok%       = Val(Mid$(TS.TEMP1$,30,2)) 		    					  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.FeNat.Motora%   = Val(Mid$(TS.TEMP1$,30,3)) 		    					  ! Tecla motora proceso
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.FeNat.Uvt%   = Val(Mid$(TS.TEMP1$,30,2)) 		    					      ! Numero minimo UVT factura electronica
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.FeNat.VlrUvt%   = Val(Mid$(TS.TEMP1$,30,6)) 		    					  ! Valor UVT
         GoTo UE.FIN.FENAT  																							  ! Sale del ciclo de carga                   
        EndIf																																! Fin caga
  Wend 																																			! Fin carga
  UE.FIN.FENAT:                                                             !
     Close 94																																! Cierra archivo
   If Gr.FeNat.Ok% = -1 Then                                               \! Proyecto Activo
      Call U.IMPRIME("MODULO FE NATURALES  ON  24-Ene-2023",2100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO FE NATURALES  OFF 24-Ene-2023",2100H)          ! Msg Proyecto Cargado
EndIf 																																			! Fin carga de parametros 

If Gr.FeNat.Ok% <> -1  Then Exit Sub                                        ! Si proyecto apagado
If TS.STANDALONE       Then Exit Sub	                                      ! Si en operacion TOF

!--- EAMTSU02.J86
If Xopt% = 02 Then Begin                                                    ! Nueva transaccion
   Gr.FeNat.Proc% = 0
   Gr.FeNat.Cont% = 0
   Gr.FeNat.Itm%  = 0
   Gr.FeNat.Aplicado% = 0
   Dim Gr.FeNat.LstItm$(1500,7)																							! 
   Gr.Mnf.Fac$ = ""
EndIf																																				! fin nueva transaccion

!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                    ! validando forma de pago
    If (TS.INTRX) And TS.IO.STATE = 10 Then Begin														! En una trx de venta	
!-- Controles de no ejecucion procesos Comfandi
     If UE.RECASRV.QTY% > 0 Then Exit Sub																		! En proceso de recaudos
     If Gr.Mrm.Cap% = -1    Then Exit Sub																		! En proceso de remisiones
     If Asc.Eps.Cntrl% = 0  Then Exit Sub																		! En proceso de minifacturas
     If NO.FAC$ <> ""       Then Exit Sub																		! En proceso de minifacturas
!-- Fin controles de no ejecucion 

	   TS.TEMP5$ = ";"+Str$(TS.IO.MOTORKEY) + ";"															! Tecla motora presionada
     If (TS.IO.KEYS(7) > 90 AND TS.IO.KEYS(7) < 97) Or                     \! Valida si en forma de pago 
       (MATCH(TS.TEMP5$,Gr.Motor2.LstPago$,1) > 0) Then Begin        			  ! o tecla motora para pagos
       If TS.TOTALS(0,0,0) = 0 Then Begin																		! Transacción en Cero
       	  Call VISOR.AND.BORRAR("NO SE PERMITE FACT. EN CEROS  /BORRAR   ") !
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)											    !
          TS.IO.MOTORKEY = 73																						    !
          Exit Sub 																											    !
       EndIf																																!
       If TS.TOTALS(0,0,0) < 0 Then Begin																		! Transacciones negativas
          Gr.Fis.Doc% = -1                                       						! No reporta numeracion fiscal
       EndIf Else Begin 
       	  Gr.Fis.Doc% = 0																						        !
       	  Call VALIDA.COMPRA.UVT																						! Validacion montos de compra
       	  Call VALIDA.DATOS.FENATURAL
       	  If TS.TEMP1I1 = 0 Then Begin
             Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)										    !
             TS.IO.MOTORKEY = 73																					  !
             Exit Sub 																										  !
       	  EndIf
       EndIf
   	 EndIf																																	! En proceso de pagos
    EndIf 
EndIf																																				! fin formas de pago

!--- EAMTSU20.J86
If Xopt% = 20 Then Begin                                                    ! Impresion CR 

 If MATCH(TS.SDESC$(59),TS.PRTBUF$,1) > 0 THEN BEGIN												! Si anulacion total de la trx
    Gr.FeNat.Proc% = 0
    Gr.FeNat.Cont% = 0
    Gr.FeNat.Itm%  = 0
    Gr.FeNat.Aplicado% = 0
    Dim Gr.FeNat.LstItm$(1500,7)																						! 
 EndIf																																			!

 If (TS.INTRX) AND (TS.LINETYPE = 6 AND TS.LINEDATA = 1) And  						 \!
    (TS.TENDERED (0) <> 0 Or TS.TRX.STATUS <> 100) Then Begin								!

    If Gr.FeNat.Aplicado% = -1 Then Begin																		! Si aplica FE naturales
    	
     If Gr.Mnf.Fac$ <> "" Then Begin   																		  ! Proceso MF 
        TS.TEMP1$ = Unpack$(Nite1$)																					! MF capturada
        TS.TEMP1$ = ";" + Right$(TS.TEMP1$,3) + ";"													! Plan de la MF
        If Match(TS.TEMP1$,Gr.Mf.Lst$,1) > 0 Then Begin											! Prefijos 5 a 8 
           Exit Sub 																												! no genera FECO Expressmed
        EndIf
        Call GENERA.REMISION.MFFIS																				  ! Genera remision
        Call Graba.Itm.FeNatural																					  ! Almacena Items para FE
        Exit Sub 																														! no genera FECO
     EndIf																																	!

     If Gr.Fis.Doc% = -1 Then Exit Sub                                      ! No reporta numeracion fiscal

     Xtmpx$ = Str$(Val(TS.STORE$)) + Right$("000"+Str$(SL.HD.TERMINAL),3) + \!
              Right$("0000"+Str$(Sl.Hd.Transnum + 1),4) + UNPACK$(SL.HD.DATETIME$)
              
     TS.TEMP2$ = "IUT: "+Xtmpx$																							!
     TS.TEMP2$ = Left$(TS.TEMP2$ + STRING$(40," "),40)                    	!
     Call U.Imprime(TS.TEMP2$,2100H)                                      	! Rastro trx journal
!-- Impresion EAN 128
    BARCODE$  = Chr$(1DH) + Chr$(6BH) 					                            ! Manejo de impresion
    ENDBCODE$ = Chr$(00H) 						                                      ! codigos de barra
    TS.TEMP1$ = Chr$(1DH)+Chr$(77H)+Chr$(02H)+  		                       \! Horizontal barcode Size
                Chr$(1DH)+Chr$(68H)+Chr$(50)+                              \! Vertical barcode Size 
                Chr$(1DH)+Chr$(48H)+Chr$(02H)+                             \! Human Read characters
  	            Chr$(1DH)+Chr$(66H)+Chr$(01H)    	                          ! activado
     Call U.IMPRIME(TS.TEMP1$,4000H)				                                !
     TS.TEMP1$ = (BARCODE$+Chr$(07H)+(Xtmpx$)+ENDBCODE$)           					! Arma String de impresion			
     Write #34 ; TS.TEMP1$          					                              ! Imprime dato EAN-128
     TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(0)				                        ! Ajusta impresion a la Izquierda
     Call U.IMPRIME(TS.TEMP1$,4000H)				                                !                    

!-- Fin impresion EAN 128       
       
!       TS.TEMP1$ = Right$("0000000000000000"+Xtmpx$,16)                     !
!       TS.TEMP2$ = "TR+"                                           +       \! Arma Str Usuario con 
!                   "REMI"+Pack$(TS.TEMP1$)+":"                           + \!                      
!                   STR$(ABS(TS.GROSSPOS + TS.GROSSNEG))                  + \! #Fact, Vr/Vta Neto   
!                   ":" + Pack$("00") + ":" + Pack$("00")                 + \! y Vrs de Iva         
!                   ":" + Pack$("00") + ":" + Pack$("00")                 + \!                      
!                   ":" + Pack$("00") + ":" + Pack$("00")                 + \!                      
!                   ":" + Pack$("00") + ":" + Pack$("00")                    !                      
!       Call Grabacion.Cadena.Usuario2("21",TS.TEMP2$)                       !

       Call Graba.Itm.FeNatural																						  ! Almacena Items para FE

    EndIf																																		! fin reporte beneficio
 EndIf 

EndIf																																				! Imppresion CR

End Sub 																																		!
