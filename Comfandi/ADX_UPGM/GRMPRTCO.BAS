!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMPRTCO.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 Toshiba - IBM         * 
!Observaciones : Modulo Precio Techo              *
!**************************************************
!Observaciones
! Mod 29-Nov-2017
! Se modifica modulo por requerimiento de Comfandi
! para que en todo momento independiente de si se
! ha capturado vecino fiel, se valide el precio techo
! del producto controlado.
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%ENVIRON T

Integer*1 Global Gr.Ptecho.InTrx%, \
                 Gr.Ptecho.Ok%,    \
                 Gr.Ptecho.Stat%,  \
                 Gr.Mrm.Cap%       ! Proceso remisiones
                 
String    Global Gr.Lcl.Clte$, \
                 Gr.Ptecho.Tip$, \
                 Gr.Ptecho.Str$
                 
Integer*4 Global Gr.Ptecho.Max%
Integer*1 Global  Gr.NtaCrd.InTrx%																					! Control Nta Crd

%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMITEMR.J86          ! Maestra de articulos
%INCLUDE EAMTSEVA.J86 				 ! Variables EMS
%INCLUDE EAMTRANS.J86 				 ! Variables EMS
%INCLUDE EAMTSXHC.J86          ! Rutinas SMA
%INCLUDE RECATSSU.011  				 ! Rutinas Genericas

Sub TSHIECET EXTERNAL
End Sub

Sub  TSCSEC03 External
End Sub 

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                      *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                         *!
  STRING    DATA$      !* String to hold the data read from file.            *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                               *!
End Sub 

Sub IRRFEC.SPLIT1 (RECORD$) EXTERNAL
  STRING    RECORD$    !* The record (including the key) read from file      *!
End Sub 

Function FORMAT.AMOUNT(AMT1) EXTERNAL
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub PTECHO.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:PT" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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

Sub consulta.Itm.Ptecho Public 																							!
String Xdat1$, Xdat2$, XTMP1$, XTmp2$
 Itm.Ptecho.Precio:
    TS.ER.RETURN = -1
    TS.TEMP2$ = IR.ITEMCODE$										  													! Armo dato para busqueda
    Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)            							! Lectura Datos Itemr
    Call IRRFEC.SPLIT1( TS.TEMP1$ ) 			                       						! Entrega datos al eamitemr.j86
    If TS.ER.RETURN = -1 Then Begin																					! Si consulta OK
      If Unpack$(IR.INDICAT2$) = "05" Then Begin
         IR.ITEMCODE$ = IR.SALEQUAN$ + IR.SALEPRIC$								  				! Armo dato para busqueda
         GoTo Itm.Ptecho.Precio																							! Consulta nuevamente
      EndIf																																	!
      Xdat1$ = IR.ITEMNAME$																									! Nombre producto
      XTMP1$ = Unpack$(IR.SALEPRIC$)                        								! Desempaqueta precio
      Call Format.Amount(Val(Xtmp1$))                        								! Formateo el Valor
      XTmp2$ = Right$("          " + TS.TEMP1$, 10)													! 
      XDAT2$ = "   *VP*   " + XTmp2$																				! Dato a presentar
    EndIf Else Begin
    	XDAT1$ = "ARTICULO NO EXISTE"
    	XDAT2$ = "EN MAESTRA PRODUCTOS"
    EndIf
    XDAT1$ = LEFT$(XDAT1$+String$(20," "),20)
    XDAT2$ = LEFT$(XDAT2$+String$(20," "),20)
    Call visor.and.borrar(XDAT1$+XDAT2$)
    IR.INDICAT0 = IR.INDICAT0 OR 04H												            		! No permite venta
    TS.IO.MOTORKEY = 73
    Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
End Sub 

Sub CONSULTA.PTECHO																													! Consulta Precio Techo
String XRtrama$, Xenvia$, Xllega$, XSALE$, XRTA$, XID$, X.code$
String Xt1$, Xt2$
%Include CNVITEMR.J86																												! Variables temporales
X.code$ = IR.ITEMCODE$
 TS.ER.RETURN = -1
 %Include CNVIRRD1.J86          																						! Lectura maestro de articulos
 If TS.ER.RETURN <> -1 Then BEGIN 
   Call VISOR.And.BORRAR("ERROR CONSULTA PRODUCTO")
	 TS.TEMP5I1 = 0 																													! Proceso fallido
	 Exit Sub      																														! Sale del proceso
 EndIf  
 Gr.Ptecho.Max% = 0																													! Precio maximo techo
 Gr.Ptecho.Tip$ = ""																												! Tipificacion producto
 If Unpack$(AR.INDICAT2$) = "05" Then                                      \! Si es un MP 5
 	  Xid$ = Str$(Val(Unpack$(AR.SALEQUAN$+AR.SALEPRIC$)))         Else      \!
    Xid$ = Str$(Val(Unpack$(IR.ITEMCODE$)))																	! Item a consultar   
 TS.TEMP5I1 = 0 																														! 
 Call visores4690(1,"VALIDA PRECIO TECHO",XID$,0,"L")
 Xt1$ = Right$("                  "+XID$,12)																! Consulta afiliado
 Xt2$ = Armar.Trama.Msg("18","81",Xt1$,"00","0001","123456") 		  					! Armar trama MSG                	
 Xenvia$   = Xt2$																											      !
 Xsale$    = Date$ +":"+ Time$                                              ! Fecha y hora del requerimiento
 XRtrama$  = Rutina.Java("com.appl.ApplKernel","threader", Xt2$)  			    ! Ejecuta Requerimiento
 
 !XRtrama$  = "1881064200000CONSULTA SATIFACTORIA                   44522          RNC"
 
 Xrta$     = Date$ +":"+ Time$                                              ! Fecha y hora rta del requerimiento
 Xllega$   = XRtrama$																												!
 Call PTECHO.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                      ! Rastro auditoria
 Xt1$ = Valida.Rta(XRtrama$)																					  		! Valida rta entregada
 If Xt1$ = "00" Then Begin																									! Proceso satisfactorio
	  Xt1$ = MID$(XRtrama$,12,2)                                         			! Toma rta de la BD
		If Xt1$ <> "00" Then Begin								   									    			! Si presenta un Error
			 Call VISOR.And.BORRAR(Mid$(XRtrama$,14,40))                          ! Msg de Error	
			 TS.TEMP5I1 = 0 																											! Proceso fallido
			 Exit Sub      																												! Sale del proceso
    EndIf																																		! Proceso satistactorio
    TS.TEMP5I1 = -1																													! Proceso correcto
    Gr.Ptecho.Max% = Val(Mid$(XRtrama$,54,15))															! Precio maximo techo
    Gr.Ptecho.Tip$ = Mid$(XRtrama$,69, 3)																		! Tipificacion producto
    Exit Sub 																																! Termina proceso
 EndIf																																			!

End Sub 																																		!


!--- Bloque Principal Modulo
Sub PRECIO.TECHO(XOPT%) Public																						! Modulo Precio Techo
Integer*4 XOPT%           																								! Variables 
String    XSgn$, Xtmp$
!--- EAMTSU07.J86
If XOPT% = 07 Then Begin 																									! Carga de opciones
   Gr.Ptecho.Ok% 	= 0																											! Proyecto apagado
   Open "R::$SCNTRL" AS 94	NoWrite NoDel																	! Apertura archivo parametrizacion 
   If End #94 Then GR.FIN.PTECHO           																! Si es EOF
   While (1)															  															! Recorre archivo
        Read #94; TS.TEMP1$			       																		! Lectura registro
        If TS.TEMP1$ = "[PRECIO TECHO]" Then Begin	    			   					! Precio Techo
         Read #94; TS.TEMP1$																							! Lectura registro
         GR.PTECHO.Ok% = Val(Mid$(TS.TEMP1$,30,2))				          			! Proyecto Activo
         GoTo GR.FIN.PTECHO     																					! Sale del ciclo de carga
        EndIf                                                             !
   Wend                                                                   !
   GR.FIN.PTECHO:                                                         !
     Close 94																															! Cierra archivo
   If GR.PTECHO.Ok% = -1 Then                                            \! Proyecto Activo
      Call U.IMPRIME("MODULO PRECIO TECHO    ON 12/Ene/2017",2100H) Else \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO PRECIO TECHO   OFF 12/Ene/2017",2100H)       ! Msg Proyecto Cargado
EndIf

If GR.PTECHO.Ok% <> -1 Then Exit Sub 						  												! Si proyecto apagadao

!--- EAMTSU02.J86
If XOPT% = 02 Then Begin 																									! Inicializacion de variables
   Gr.Ptecho.InTrx% = 0 
EndIf

!--- EAMTSU08.J86
If XOPT% = 08 Then Begin 																									! Validacion Venta Item
 If TS.IO.KEYS(4) = 100 And TS.IO.KEYS(2) = 80 Then Begin  				  			! Consulta Precio
    Exit Sub
 EndIf
 If Gr.NtaCrd.InTrx% <> 0 Then Exit Sub                                		! En proceso nota crd
 If (IR.INDICAT1A AND 01H) And (TS.IO.KEYS(5) = 74) Then Begin
    Call Visor.And.Borrar("CAMBIO PRECIO NO    AUTORIZADO P/TECHO. ")    	! Msg de alerta
    IR.INDICAT0 = IR.INDICAT0 OR 04H												            	! No permite venta
 	  Exit Sub 																															! Sale del proceso
 EndIf

 Gr.Ptecho.Stat% = 0 																											!
 If (IR.INDICAT1A AND 01H) Then Begin
 	  Call Visor.And.Borrar("ITEM  REGULADO NO SE PUEDE VENTA A NITS")     	! Msg de alerta
 EndIf
 If (IR.INDICAT1A AND 01H) And (Gr.Mrm.Cap% = -1) Then Begin  						! User option 4 Activa y en remision
    Call Visor.And.Borrar("PRODUCTO REGULADO NOVENTA EN REMISION")       	! Msg de alerta
    IR.INDICAT0 = IR.INDICAT0 OR 04H												            	! No permite venta
 	  Exit Sub 																															! Sale del proceso
 EndIf
 
! If (USER.FBACT.READ) Then Begin                    											! Si CLF capturado Mod 29-Nov-2017
	
  If (IR.INDICAT1A AND 01H) Then Begin 																		! User option 4 Activa precio techo
  	
   If Len(Gr.Lcl.Clte$) = 9 Then Begin																		! Capturado un nit
    If (Val(Gr.Lcl.Clte$) >= 800000000) And 														 \! Si cliente es una empresa
      (Val(Gr.Lcl.Clte$) <= 999999999) Then Begin		  										! del programa lealtad
      Call Visor.And.Borrar("PRODUCTO REGULADO NOVENTA A NITS ")          ! Msg de alerta
      IR.INDICAT0 = IR.INDICAT0 OR 04H												            ! No permite venta
   	  Exit Sub 																														! Sale del proceso
    EndIf   																																! Fin valida cliente
   EndIf
   If (Val(Gr.Lcl.Clte$) >= 8000000000) And 														 \! Si cliente es una empresa
      (Val(Gr.Lcl.Clte$) <= 9999999999) Then Begin												! del programa lealtad
      Call Visor.And.Borrar("PRODUCTO REGULADO NOVENTA A NITS ")          ! Msg de alerta
      IR.INDICAT0 = IR.INDICAT0 OR 04H												            ! No permite venta
   	  Exit Sub 																														! Sale del proceso
   EndIf   																																! Fin valida cliente
   
  EndIf 																																	! Fin Item controlado

! EndIf																																		! Fin si FBACT 
 
EndIf																																			! Fin Eamtsu08

!--- EAMTSU43.J86
If XOPT% = 43 Then Begin 																									! Validacion Venta Item

	 If Gr.NtaCrd.InTrx% <> 0 Then Exit Sub                                	! En proceso nota crd
   If IR.INDICAT1A AND 01H Then Begin																		 \! Venta de un Item precio techo
   	  Gr.Ptecho.InTrx% = -1																								! En una trx 

   TS.TEMP5I1 = 0 																												!
   Call CONSULTA.PTECHO                                               		! Valida Precio techo
   If TS.TEMP5I1 = -1 Then Begin																					! Si Articulo precio techo
   	
!    If Unpack$(IR.INDICAT2$) = "05" Then Begin
!      TS.TEMP2$ = IR.SALEQUAN$ + IR.SALEPRIC$										  				! Armo dato para busqueda
!      Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)            					! Lectura Datos Itemr
!      Call IRRFEC.SPLIT1( TS.TEMP1$ ) 			                       				! Entrega datos al eamitemr.j86
!    EndIf

!   Call VISOR.AND.BORRAR("PRECIO2IR:"+UnPack$(IR.SALEPRIC$)+"T:"+STR$(Gr.Ptecho.Max%)+" "+Unpack$(IR.INDICAT2$))
   	
!    If Val(UnPack$(IR.SALEPRIC$)) > Gr.Ptecho.Max% Then Begin 						! Si precio superior al parametrizado
	
	   If SL.IE.SALEPRIC > Gr.Ptecho.Max% Then Begin 						! Si precio superior al parametrizado
       Call Visor.And.Borrar("ITEM SUPERA PRECIO  TECHO DEFINIDO")        ! Msg de alerta
       TS.LINEDATA = 27																											!
       Call TSCSEC03
       IR.DEPARTME$ = Pack$("0000")
       Exit  Sub 

  
  ! 	   IR.INDICAT0 = IR.INDICAT0 OR 04H												            ! No permite venta
  ! 	   Exit Sub 																													! Sale del proceso
  
  
  
    EndIf																																	! 

    Gr.Ptecho.Stat% = -1																									! Items precio techo en la trx
    Gr.Ptecho.Str$  = IR.ITEMCODE$                  	 +                 \! Codigo del producto
                ":" + Gr.Ptecho.Tip$							     +                 \! Tipificacion
                ":" + Pack$(Str$(Gr.Ptecho.Max%))      +                 \! Precio maximo
                ":" + Pack$("00")																					! Filler 
   EndIf Else Begin																												! Si falla validacion
   	  IR.INDICAT0 = IR.INDICAT0 OR 04H												            ! No permite venta
   	  Exit Sub 																														! Sale del proceso
   EndIf																																	! Fin validacion Item techo




!      SL.IE.SALEPRIC = Gr.Ptecho.Max%	                										!
!      If SL.IE.QTYORWGT <> 0 Then                                        \!
!         SL.IT.XPRICE = (Gr.Ptecho.Max% * SL.IE.QTYORWGT) Else           \!
!   	     SL.IT.XPRICE = Gr.Ptecho.Max%

   EndIf
EndIf																																			! Fin Eamtsu43

!--- EAMTSU53.J86
If XOPT% = 53 Then Begin 																									! Validacion Venta Item
 If UNPACK$(LEFT$(SL.STR.ENTRY$,1)) = "01" THEN BEGIN   	    				  	! Si Item Entry
   If IR.INDICAT1A AND 01H Then 																				 \! Venta de un Item precio techo
   	  Gr.Ptecho.InTrx% = -1																								! En una trx 
 EndIf																																		!
EndIf																																			! Fin Eamtsu53


!--- EAMTSU67.J86
If Xopt% = 67 Then Begin                                                 	! Al terminar calculo de precio 
	 If ((Not TS.RECOVERY) And (Not TS.RETV.IN.PROGRESS)) Then Begin				! No esta en proceso de recuperacion
	 	If Gr.Ptecho.Stat% = -1 Then Begin                                   \! Precio Techo
	    Call Grabacion.Cadena.Usuario("20171025",Gr.Ptecho.Str$)         		! Almacena UD 
    EndIf
	 EndIf																																	!
	 Gr.Ptecho.Str$ = ""																										!
	 Gr.Ptecho.Stat% = 0																										!
EndIf												

End Sub 
