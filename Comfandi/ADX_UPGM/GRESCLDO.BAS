!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRESCLDO.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Julio de 2006                    *
!Observaciones : Modulo Descuentos Escalonados    *
!**************************************************
!Observaciones
!La aplicacion trabaja con el codigo de aplicacion
!15 y funcion 21 para la consulta
!-------------------------------------------------
! Mod 18 MAY 2017
! Se ajusta grabacion valor del descuento retirando
! el valor del IVA, solicitado por Comfandi
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%ENVIRON T

Integer*1 Global USER.FBACT.READ
Integer*4 Global Gr.Fis.FacNum%
String    Global Gr.Fis.Factura$
Integer*1 Global Gr.Escl.Iva%
Integer*4 Global UE.TABLAIVA(1)
Integer*1 Global  Gr.NtaCrd.InTrx%																					! Control Nta Crd

%INCLUDE ESCTSUVA.011          ! Variables Aplicacion
%INCLUDE EAMTOPTS.J86          ! Terminal options 
%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMITEMR.J86          ! Maestra de articulos
%INCLUDE EAMTSXHC.J86          ! Rutinas SMA
%INCLUDE RECATSSU.011  				 ! Rutinas Genericas

Function FORMAT.AMOUNT(AMT1) EXTERNAL
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub TSHIECET EXTERNAL
End Sub

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                      *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                         *!
  STRING    DATA$      !* String to hold the data read from file.            *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                               *!
End Sub 

Sub IRRFEC.SPLIT1 (RECORD$) External
  STRING    RECORD$    !* The record (including the key) read from file      *!
End Sub 


Sub Cntrl.Vta.Escalonado(VtItm$,VtClte$)																		! Cntrl vta escalonados
String VtItm$, VtTrama$,VtEscala$, VtClte$, VtTipo$								          ! Variables
Integer*1 VtFound%, VtI%, VtQty%, Vtpos%, Vtloc%														!
   
   TS.TEMP2$ = PACK$(Right$("000000000000"+VtItm$,12))											! Llave de busqueda
   Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)             							! Lectura Datos Itemr
   Call IRRFEC.SPLIT1 ( TS.TEMP1$ ) 			                     							! Entrega datos al eamitemr.j86
   If UnPack$(IR.INDICAT2$) = "05" Then Begin																! Es un codigo de barras
      TS.TEMP2$ = IR.SALEQUAN$ + IR.SALEPRIC$             			   					! Busca articulo ligado                	
      Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)             					! Lectura Datos Itemr
      Call IRRFEC.SPLIT1 ( TS.TEMP1$ ) 			                     			  		! Entrega datos al eamitemr.j86
      SL.IT.ITEMCODE$ = UnPack$(IR.ITEMCODE$)																! Retorna PLU al que esta ligado
      VtItm$ = SL.IT.ITEMCODE$																							! Aisgna PLU del producto
   EndIf																																		!

VtFound% = 0 																																! Ctrl busqueda
For VtI% = 1 To Gr.Escl.Nitm%																								! Busqueda en Items
 If Val(Gr.Escl.ItmVta$(VtI%,0)) = Val(VtItm$) Then Begin 									! Si Item ya registrado
 	  VtFound% = VtI%																													! Posicion donde lo encontro
 EndIf																																			!
Next VtI%																																		!

If VtFound% = 0 Then Begin																									! No Encontrado
 Call TSHIECET
 Call VISORES4690(1,"VALIDANDO ESCALONADO",Str$(Val(VtItm$)),0,"L")

 TS.TEMP1$ = Right$("                  "+VtClte$,18) + Right$("000000000000"+VtItm$,12)
 TS.TEMP2$ = Armar.Trama.Msg("15","21",TS.TEMP1$,"00","0001","123456") 			! Armar trama MSG                	
 VtTrama$  = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)  			! Ejecuta Requerimiento          	
 TS.TEMP1$ = Valida.Rta(VtTrama$)																					  ! Valida rta entregada 
      
!--- Simulacion de respuesta      
!TS.TEMP1$ = "00"
!VtTrama$ = "3201244200000xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx100020003000400050006000T00040002VF0" !Nueva trama

 If TS.TEMP1$ <> "00" Then Begin	  														        		! Si presenta un error, verif. local
    Call VISORES4690(1,"VENTA ITEM A PRECIO ","COMERCIAL.          ",1200,"L")
    Gr.Escl.Registro% = -1
    Exit Sub																																! Sale del proceso
 EndIf 																																			!
 TS.TEMP1$ = MID$(VtTrama$,12,2)                                      			! Toma status de la Operacion
 If TS.TEMP1$ <> "00" Then Begin					 														  		! Si presenta un error, verif. local
    TS.TEMP1$ = MID$(VtTrama$,14,40)                              					! Toma mensaje del Error 
    Call VISOR.And.BORRAR(Left$(TS.TEMP1$,20)+Right$(TS.TEMP1$,20))     		!
    Call VISORES4690(1,"VENTA ITEM A PRECIO ","COMERCIAL.          ",1200,"L")
    Gr.Escl.Registro% = -1
    TS.TEMP5I1 = 0 																													! Reporta Error en la consulta			
    Exit Sub     																														! 
 EndIf																																			! Fin proceso java
 VtTipo$ = "TC" + Ucase$(Mid$(VtTrama$,87,02))															! Tipificacion cliente
 If Match(VtTipo$,Gr.Lcl.TipClte$,1) <= 0 Then Begin     			              ! Si cliente no tiene derecho a escala
  	 Call VISOR.And.BORRAR("CLIENTE NO TIENE USODSCTO ESCALONADO")
     Call VISOR.And.BORRAR("VENTA ITEM A PRECIO COMERCIAL")
     Gr.Escl.Registro% = -1
     Gr.Escl.Tasa% = 0 
     TS.TEMP5I1 = 0
     Exit Sub 
EndIf

 Gr.Escl.Nitm% = Gr.Escl.Nitm% + 1 																					! Aumenta Vector
 Gr.Escl.ItmVta$(Gr.Escl.Nitm%,0) = VtItm$																	! Asigna Item
 Gr.Escl.ItmVta$(Gr.Escl.Nitm%,1) = Mid$(VtTrama$,83,04)										! Asigna Qty Vendida
 Gr.Escl.ItmVta$(Gr.Escl.Nitm%,2) = Mid$(VtTrama$,54,36)    								! Asigna trama registro
 Gr.Escl.ItmVta$(Gr.Escl.Nitm%,3) = Mid$(VtTrama$,89,01)    								! Asigna Ultima Escala Aplicada
 Gr.Escl.ItmVta$(Gr.Escl.Nitm%,4) = Mid$(VtTrama$,79,04)    								! Qty Venta Maxima al mes
 VtFound% = Gr.Escl.Nitm%
EndIf

If (TS.IO.KEYS(1) = 70 Or TS.IO.KEYS(8) = 67 ) THEN BEGIN	                  ! Si es una anulacion

!	 VtQty% = -1
!   Vtpos% = Val(Gr.Escl.ItmVta$(Vtfound%,3))                                ! Ultima escala aplicada
!   If Vtpos% = 0 Then Begin																									! Si nunca ha aplicado dscto
!   	  Gr.Escl.Tasa% = 0																											! No aplicar descuento
!   EndIf Else Begin																													! Anula con ultimo dscto 
!   	 VtLoc% = (Vtpos% * 4) - 3                                              ! Posicion para buscar dscto en trama
!     VtEscala$ = Mid$(Gr.Escl.ItmVta$(Vtfound%,2),Vtloc%,4)                 ! Toma la escala asignada
!     Gr.Escl.Tasa% = Val(VtEscala$)																					! Valor de la escala
!     Vtpos% = Vtpos% - 1
!   EndIf

EndIf Else Begin
 VtQty% = 1
 Vtpos% = Val(Gr.Escl.ItmVta$(Vtfound%,3))                                  ! Ultima escala aplicada
 If Vtpos% = 6 Then Begin																										! Si ultima escala aplicada
	 If Mid$(Gr.Escl.ItmVta$(Vtfound%,2),25,1) = "T" Then Begin               ! Si resetea escala
	 	  Vtpos% = 0 																														! Reinicia secuencia para asignar dscto
	 EndIf																																		! 
 EndIf																																			!
 Vtpos% = Vtpos% + 1 																											  ! Aumenta en 1 numero de escala
 VtLoc% = (Vtpos% * 4) - 3                                                  ! Posicion para buscar dscto en trama
 VtEscala$ = Mid$(Gr.Escl.ItmVta$(Vtfound%,2),Vtloc%,4)                     ! Toma la escala asignada
 Gr.Escl.Tasa% = Val(VtEscala$)																							! Valor de la escala
 Gr.Escl.NumEscl% = Vtpos%																									! Escala Aplicada

 If Gr.Escl.Tasa% = 0 Then Begin																						! Si no hay escala para el producto
	 If Mid$(Gr.Escl.ItmVta$(Vtfound%,2),25,1) = "T" Then Begin               ! Si resetea escala
	 	  Vtpos% = 1 																														! Reinicia secuencia para asignar dscto
      Gr.Escl.NumEscl% = Vtpos%																							! Escala Aplicada
      VtLoc% = (Vtpos% * 4) - 3                                             ! Posicion para buscar dscto en trama
      VtEscala$ = Mid$(Gr.Escl.ItmVta$(Vtfound%,2),Vtloc%,4)                ! Toma la escala asignada
      Gr.Escl.Tasa% = Val(VtEscala$)																				! Valor de la escala
	 EndIf Else Begin																													! No resetea Escala
	 	  Vtpos% = Val(Gr.Escl.ItmVta$(VtFound%,3))															! Asigna ultima escala aplicada
	 	  Gr.Escl.NumEscl% = Vtpos%					  																	! Escala Aplicada
      VtLoc% = (Vtpos% * 4) - 3                                             ! Posicion para buscar dscto en trama
      VtEscala$ = Mid$(Gr.Escl.ItmVta$(Vtfound%,2),Vtloc%,4)                ! Toma la escala asignada
      Gr.Escl.Tasa% = Val(VtEscala$)																				! Valor de la escala
	 EndIf
 EndIf
EndIf
Actualiza.Escalonado:

If (Val(Gr.Escl.ItmVta$(VtFound%,1)) + VtQty%) <= (Val(Gr.Escl.ItmVta$(VtFound%,4))) Then Begin  ! 
  Gr.Escl.ItmVta$(VtFound%,1) = Str$(Val(Gr.Escl.ItmVta$(VtFound%,1)) + VtQty% )
  Gr.Escl.ItmVta$(VtFound%,3) = Str$(Vtpos%)                             		! Asigna la escala aplicada
  Gr.Escl.Registro% = -1
  TS.TEMP5I1 = -1																														! Venta autorizada
EndIf Else Begin 
	Call VISOR.AND.BORRAR("SUPERA MAXIMO COMPRAPERMITIDA AL MES")							! Msg al operador
	Call VISOR.AND.BORRAR("VENTA ITEM A PRECIO COMERCIAL")
	Gr.Escl.Registro% = -1																										! Elimina control verif precio
	Gr.Escl.Tasa% = 0 																												! No aplica dscto
	TS.TEMP5I1 = 0																														! No valida registro
EndIf

End Sub 

Sub CONSULTA.ESCALONADO
String X.ITEM$,X.CLTE$
 TS.TEMP5I1 = 0																														! Control proceso

  If (TS.IO.DATA$(6) <> "") Then Begin 																    !
     Call Visor.And.Borrar("ITEM NO PERMITE     VENTA POR CANTIDAD")      ! Msg de alerta
     IR.INDICAT0 = IR.INDICAT0 OR 04H	          													! No permite venta
     Gr.Escl.Registro% = -1																								! Elimina control verif precio
     Exit Sub																															! Sale del proceso
  EndIf																																 	  !
  Call Cntrl.Vta.Escalonado(SL.IT.ITEMCODE$,Gr.Lcl.Clte$)							    ! Valida venta del producto
  
End Sub 


Sub ACT.ONLINE.ESCALONADO																										! Actualizacion Online Dscto
Integer*4 PRIC%, TOT4%, X.I%																								!
String    ZBuffer$, X.Tmp$, C01$,C02$,C03$,C04$,C05$,C06$,C07$,C08$,C09$,C10$ !
  ZBuffer$ = ""																															! Init variable
  For X.I% = 1 TO SL.END                    																! FOR ALL StringS
    H$ = READ.SL.STR$(X.I%)                 																! GET String
    If LEN(H$) > 5 Then                                                    \! ASSURE GOOD String
     If ASC(H$)  = 153 Then Begin            																! User Data String
        Asc.Tmp.Apun% = 3																										! Asigna apuntador
        X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	 														! Verifica el numero de proyecto
        If Val(X.TMP$) = 20111207 Then Begin    														! Si articulo escalonado
        	 C01$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  										    ! Numero de tienda 						C4
        	 C02$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  										    ! Codigo del cliente					C18
        	 C03$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  										    ! Item Vendido								C12
        	 C04$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  										    ! Precio articulo							C10
        	 C05$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  										    ! Dscto obtenido							C10
        	 C06$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  										    ! Signo de la operacion       C02
        	 C07$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  										    ! consecutivo registro items  C02
        	 C08$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  										    ! Numero de escala aplicada   C02
        	 C09$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  										    ! Porcentaje aplicado         C04
        	 C10$ = ASIC.GETUNPK4(H$,Asc.Tmp.Apun%)	  										    ! Numero Fiscal Venta         C20
        	 ZBuffer$ = Right$("                  "+C02$,18) +               \! Cliente
        	            Right$("000000000000"+C03$,12) +                     \! Item
        	            Right$("0000000000"+C04$,10) +                       \! Precio
        	            Right$("0000000000"+C05$,10) +                       \! Dscto Valor
        	            Right$("00"+C06$,2) +                                \! Signo
        	            Right$("00"+C07$,2) +                                \! Consecutivo
        	            Right$("00"+C08$,2) +                                \! Escala aplicada
        	            Right$("0000"+C09$,4) +                              \! Ptg Aplicado
        	            Left$(C10$+"                    ",20)                 ! Factura Fiscal de venta

           Call VISORES4690(1,"GRABANDO MOVIMIENTO","CLIENTE, ESPERE ("+C07$+")",0,"L")      !
           TS.TEMP1$ = Zbuffer$																										! Trama a actualizar
           TS.TEMP2$ = Armar.Trama.Msg("15","22",TS.TEMP1$,"00","0001","123456")	! Armar trama MSG                	
           TS.TEMP3$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$) 	! Ejecuta Requerimiento          	
        EndIf																																!
     EndIf                                  																! User Data String
  Next X.I%   
End Sub 

!--- Bloque Principal Modulo
Sub ESCALONADO(XOPT%) Public																							! Modulo Dscto Escalonado
Integer*4 XOPT%           																								! Variables 
String    XSgn$, Xtmp$
!--- EAMTSU07.J86
If XOPT% = 07 Then Begin 																									! Carga de opciones
   GR.ESCL.ACT%	= 0																												! Proyecto apagado
   Gr.Escl.ItmVl% = 0 																										! Dscto Escalonado entregado
   Gr.Escl.TotEscal% = 0																									! 
   Gr.Escl.Registro% = 0 																									! Ctrl Registro escalonados
   Gr.Escl.ConItm% = 0																										! Ctrl numero items registrados
   Gr.Escl.NumEscl% = 0  																									! Escala Aplicada
   Gr.Escl.CtlImp% = 0																										! Control actualizacion
   TS.ER.RETURN = -1																											! Ctrl Errores
   Dim Gr.Escl.ItmVta$(20,4)																							! Maximo 20 productos x trx
   Gr.Escl.Nitm% = 0 																											! Numero items vendidos
   Open "R::$SCNTRL" AS 94 UNLOCKED NOWRITE NODEL													! Apertura archivo parametrizacion 
   If END #94 THEN GR.FIN.ESCALONADO       																! Si es EOF
   While (1)															  															! Recorre archivo
        Read #94; TS.TEMP1$			       																		! Lectura registro
        If TS.TEMP1$ = "[DSCTO ESCALONADO]" Then Begin	    			   			! Recaudo Propinas
         Read #94; TS.TEMP1$																							! Lectura registro
         GR.ESCL.ACT% = Val(Mid$(TS.TEMP1$,30,2))				          				! Proyecto Activo
         Read #94; TS.TEMP1$																							! Lectura registro
         Gr.Escl.Descrip$ = Mid$(TS.TEMP1$,30,20)				          				! Proyecto Activo
!         Read #94; TS.TEMP1$																							! Lectura registro
!         Gr.Escl.tipoClte$ = Ucase$(Mid$(TS.TEMP1$,30,04))  							! Tipo de cliente para validar

         GoTo GR.FIN.ESCALONADO 																					! Sale del ciclo de carga
        EndIf                                                             !
   Wend                                                                   !
   GR.FIN.ESCALONADO:                                                     !
     Close 94																															! Cierra archivo
   If GR.ESCL.ACT% = -1 Then                                             \! Proyecto Activo
      Call U.IMPRIME("MODULO DSCTO ESCALON.  ON 18/May/2017",2100H) Else \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO DSCTO ESCALON. OFF 18/May/2017",2100H)       ! Msg Proyecto Cargado
EndIf

If GR.ESCL.ACT% <> -1 Then Exit Sub 																			! Si proyecto apagadao

!--- EAMTSU02.J86
If XOPT% = 02 Then Begin 																									! Inicializacion de variables
	 Gr.Escl.ItmVl% = 0 																										! Dscto Escalonado entregado
	 Gr.Escl.TotEscal% = 0
	 Gr.Escl.PricItm$ = "0"
	 Dim Gr.Escl.ItmVta$(20,4)																							! Maximo 20 productos x trx
	 Gr.Escl.Nitm% = 0																											! Numero Items Vendidos
   Gr.Escl.Registro% = 0 																									! Ctrl Registro escalonados
   Gr.Escl.ConItm% = 0																										! Ctrl numero items registrados
   Gr.Escl.NumEscl% = 0  																									! Escala Aplicada
   Gr.Escl.CtlImp% = 0																										! Control actualizacion
EndIf


!--- EAMTSU08.J86
If XOPT% = 08 Then Begin 																									! Validacion Venta Item

 If TS.IO.KEYS(4) = 100 And TS.IO.KEYS(2) = 80 Then Begin  				  			! Consulta Precio
    Exit Sub
 EndIf

 If Gr.NtaCrd.InTrx% <> 0 Then Exit Sub                                   ! En proceso nota crd
 	
 Gr.Escl.Registro% = 0 
 Gr.Escl.Tasa% = 0 																											  !
 Gr.Escl.Iva% = 8
 If (USER.FBACT.READ) Then Begin                    											! Si CLF capturado
  If IR.INDICAT1A AND 02H Then Begin 								 											! User option 3 Activa producto escalonado
 	 If (TS.IO.KEYS(1) = 70 Or TS.IO.KEYS(8) = 67 ) THEN BEGIN	            ! Si es una anulacion
 	 	  Call VISOR.AND.BORRAR("NO SE PERMITE LA    ANULACION ESCALONADO")   !
       IR.INDICAT0 = IR.INDICAT0 OR 04H												            ! No permite venta      
       Exit Sub 																													!
   EndIf																																	!
   TS.TEMP5I1 = 0 																												!
   Gr.Escl.Tasa% = 0 																											!
   Call CONSULTA.ESCALONADO                                               ! Valida Item Escalonado
   If TS.TEMP5I1 = -1 Then Begin																					! Si Articulo escalonado
     If IR.INDICAT1 And 80h Then Gr.Escl.Iva% = 1													! Tax Flag A
     If IR.INDICAT1 And 40h Then Gr.Escl.Iva% = 2													! Tax Flag B
     If IR.INDICAT1 And 20h Then Gr.Escl.Iva% = 3													! Tax Flag C
     If IR.INDICAT1 And 10h Then Gr.Escl.Iva% = 4													! Tax Flag D
  	 Gr.Escl.PricItm$ = Str$(Val(UnPack$(IR.SALEPRIC$)))                  ! Precio del producto
  	 IR.PRICE1 = Val(UnPack$(IR.SALEPRIC$))                               ! Precio del producto
     IR.PRICE2 = ROUND(FLOAT(IR.PRICE1)*(FLOAT(Gr.Escl.Tasa%)/10000.),0,0) ! Calcula descuento
     IR.PRICE2 = IR.PRICE2 / (1.+FLOAT(UE.TABLAIVA(Gr.Escl.Iva%))/100.)   ! Valor antes de Iva 18/may/2017     
     IR.PRICE1 = IR.PRICE1 - IR.PRICE2																		! Asigna nuevo precio de venta
     If (TS.IO.KEYS(1) = 70) OR (TS.IO.KEYS(8) = 67) Then                \! Si es una devolucion / anulacion
         Gr.Escl.ItmVl% = 0 - IR.PRICE2	Else 														 \! Dscto Aplicado
         Gr.Escl.ItmVl% = IR.PRICE2																		    ! Dscto Aplicado
     IR.PRICE2 = IR.PRICE1																								! y libera la aplicacion Base         
   EndIf
  EndIf 
 EndIf																																		! Fin si FBACT 
EndIf

!--- EAMTSU14.J86
If Xopt% = 14 Then Begin																									! Secuencias de tecleo

  If TS.IO.KEYS(1) = 82 AND \                       											! Proceso de suspension de
     TS.IO.KEYS(6) = 81 AND \                       											! Transaccion con una NP
     TS.PROCEDURE < 1 Then Begin                    											! Capturada
    If Gr.Escl.TotEscal% <> 0 Then Begin        													! Dscto Escalonado aplicado
      Call Visor.And.Borrar("NO SE PERMITE SUSPENDER TRANSACCION")				!
      TS.IO.MOTORKEY = 0                            											! Inicializa secuencia de 
      Dim TS.IO.KEYS(10) : Dim TS.IO.DATA$(10)      											! Tecleo
      Exit Sub 																														!
    EndIf      
  EndIf

EndIf


!--- EAMTSU20.J86
If Xopt% = 20 Then Begin																										! Antes de Impresion cliente

If TS.LINEDATA = 1 And TS.LINETYPE = 29 And TS.PROCEDURE < 0 Then Begin  		! En cierre de trs
 If NOT TS.TRAINING THEN BEGIN                       												! Si no en entrenamiento
  If TS.INTRX THEN                                                         \! Si esta en una compra
   If UE.TRX.ANUL% = 0 Then Begin 																					!
     If (Gr.Escl.TotEscal% > 0) And (Gr.Escl.CtlImp% = 0) Then Begin        ! Hay ventas escalonadas
         Call ACT.ONLINE.ESCALONADO    																			! Actualizacion en linea dscto
         Gr.Escl.CtlImp% = -1																								! Control actualizacion
     EndIf 																																	!
   EndIf 																																		!
 EndIf 																																			!
EndIf 																																			!

EndIf 																																			!  Fin Eamtsu20

!--- EAMTSU56.J86
If XOPT% = 56 Then Begin 																									! Suspension de trx
   Gr.Escl.ItmVl% = 0 																										! Dscto Escalonado entregado
   Gr.Escl.TotEscal% = 0
   Gr.Escl.PricItm$ = "0"
   Dim Gr.Escl.ItmVta$(20,4)																							! Maximo 20 productos x trx
   Gr.Escl.Nitm% = 0																											! Numero Items Vendidos
   Gr.Escl.Registro% = 0 																									! Ctrl Registro escalonados
   Gr.Escl.ConItm% = 0																										! Ctrl numero items registrados
   Gr.Escl.NumEscl% = 0  																									! Escala Aplicada
EndIf


!--- EAMTSU53.J86
If XOPT% = 53 Then Begin 																									! Despues Impresion Item

EndIf


!--- EAMTSU60.J86
If XOPT% = 60 Then Begin 																									! Despues Impresion Item

If TS.LINETYPE = 29 Then begin                                           \! Es un articulo
 If (Gr.Escl.ItmVl% <> 0 ) Then Begin 																		! Hay Dscto Escalonado
    To.USEREXIT(20) = 0
    To.USEREXIT(60) = 0
 	  Gr.Escl.TotEscal% = Gr.Escl.TotEscal% + Gr.Escl.ItmVl%                ! Acumula total dscto escalonado 
 	  If Gr.Escl.ItmVl% < 0 Then Xsgn$ = "01" Else Xsgn$ = "00"             ! Signo del dscto
 	  If Gr.Escl.ItmVl% < 0 Then Xtmp$ = Str$(Gr.Escl.ItmVl% * -1) Else    \!
 	  	                         Xtmp$ = Str$(Gr.Escl.ItmVl%)               !
 	  TS.TEMP1I4 = Gr.Escl.ItmVl% * -1																			! Formatea Valor 
   	
   	!Xtmp$ = Str$(TS.TEMP1I4)																							! Dato para almacenar en UD 18/may/2017
   	
   	Call Format.amount(TS.TEMP1I4)																				!
    TS.TEMP1$ = Right$("          "+TS.TEMP1$,10) 												! Impresion Mensaje
    TS.TEMP2$ = Gr.Escl.Descrip$ + " ("+Str$(Gr.Escl.NumEscl%)+"):" +    \!
                TS.TEMP1$													                        ! Beneficio escalonado
    TS.TEMP1$ = RIGHT$(STRING$(37," ")+TS.TEMP2$,37)											! Obtenido
    Call U.Imprime(TS.TEMP1$,2100H)																			  !
    TS.TEMP1$ = chr$( 1BH ) + chr$( 47H ) + chr$( 01H ) + TS.TEMP1$ +    \!
                chr$( 1BH ) + chr$( 47H ) + chr$( 00H )										!
    Call U.Imprime(TS.TEMP1$,4100H)																			  !
    Gr.Escl.ConItm% = Gr.Escl.ConItm% + 1																	! Ctrl numero items registrados    
    TS.TEMP1$ = RIGHT$("          "+STR$(Gr.Fis.FacNum%+1),10)
    TS.TEMP2$ = Gr.Fis.Factura$+" "+TS.TEMP1$
    TS.TEMP1$  = PACK$(Right$("0000"+TS.STORE$,4))                  +    \! Numero de tienda
                ":"+PACK$(Gr.Lcl.Clte$)                             +	   \! Cliente
                ":"+PACK$(SL.IT.ITEMCODE$)                          +	   \! Item vendido
                ":"+PACK$(Gr.Escl.PricItm$)                         +	   \! Precio Articulo
                ":"+PACK$(Xtmp$)                                    +    \! Dscto Obtenido
                ":"+Pack$(Xsgn$)                                    +    \! Signo de la operacion 
                ":"+Pack$(Str$(Gr.Escl.ConItm%))                    +    \! Consecutivo registro items
                ":"+Pack$(Str$(Gr.Escl.NumEscl%))                   +    \! Numero escala aplicada
                ":"+Pack$(Str$(Gr.Escl.Tasa%))                      +    \! Porcentaje aplicado de dscto
                ":"+TS.TEMP2$                                             ! Numero Fiscal de venta
    Call Grabacion.Cadena.Usuario2("20111207",TS.TEMP1$)                  ! Almacena user data
    Gr.Escl.PricItm$ = "0"
    Gr.Escl.ItmVl% = 0 																										!
    Gr.Escl.Registro% = 0 
    To.USEREXIT(20) = -1
    To.USEREXIT(60) = -1
 EndIf																																		! 
EndIf																																			!
EndIf																																			!

End Sub 
