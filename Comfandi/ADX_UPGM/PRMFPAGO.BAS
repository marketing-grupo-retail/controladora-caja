!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : PRMFPAGO.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Julio de 2006                    *
!Observaciones : Promociones con formas de pago   *
!**************************************************
!

%ENVIRON T

String    GLOBAL ASC.MKP.SITIO$, XERRFX$
Integer*4 GLOBAL PPP.PPP%
Integer*1 GLOBAL UE.TRX.ANUL%
Integer   Global Sl.End

%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          ! Variables de transacciones
%Include MKTTSUVA.011          ! Definicion de variables
%INCLUDE EAMTSXHC.J86          ! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          ! Rutinas de Assembler
%INCLUDE RECATSSU.011     		 ! Rutinas Genericas

Function ADXERROR(TERM,GRP,NUM,SVRTY,EVENT,UNQ$) EXTERNAL
 Integer*2 TERM,NUM
 Integer*1 SVRTY,GRP,EVENT
 String    UNQ$
End Function

Function FORMAT.AMOUNT(AMT1) EXTERNAL
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub TSHIECET EXTERNAL
End Sub

Function MATCHB(P1$,P2$,P3) EXTERNAL
  Integer*2 MATCHB
  String P1$
  String P2$
  Integer*2 P3
End Function

Function Venta.Art.Trx(Xpago$)                         ! Venta de articulos por forma de pago en la trx
  Integer*4 Venta.Art.Trx, PRIC%, Xvta%                !
  String X.TMP$, Xpago$                                !
  Xvta% = 0                                            !
  ASC.MKP.SITIO$ = "VENTA.ART.TRX"
  TS.TS11WERR$ = ""
  Open "R::$F:AFPAGO" KEYED RECL 07 AS Gr.Mkp.SesTmp% UNLOCKED NOWRITE NODEL ! Parametros dscto forma de pago
  If TS.TS11WERR$ <> "" Then Begin                     ! Error apertura archivo
  	!Call VISORES4690(1,"ERROR DE APERTURA","TF:AFPAGO",1200,"L")
 	 	Venta.Art.Trx = 0                                  ! No aplica dscto en la trx
 	  Exit Function                                      ! Sale del proceso
  EndIf                                                !
  !Call VISORES4690(1,"BARRIENDO LA TRX ","TRX :"+STR$(SL.END),1200,"L")
  
  For I% = 1 TO SL.END                                 ! FOR ALL StringS
    H$ = READ.SL.STR$(I%)                              ! GET String
    If LEN(H$) > 5 Then                                \ ASSURE GOOD String
    If ASC(H$) = 1 Then Begin                          ! ITEM ENTRY String
     X.TMP$ = ASIC.GETUNPK(H$,3)	   				           ! Tomo informacion del item vendido
     X.Tmp$ = Right$("000000000000"+X.Tmp$,12)         ! 
     X.Tmp$ = Pack$( XPago$ + X.tmp$ )                 ! Arma llave de busqueda
     
     TS.ER.RETURN = -1                                 ! Ctrl de Error
     Read Form "C7";#Gr.Mkp.SesTmp% KEY X.Tmp$; X.Tmp$ ! Lee archivo 
     If TS.ER.RETURN <> -1 Then BEGIN 
     	  GoTo Next.Itm.Vta      ! Siguiente Item vendido
     ENDIF 
     L% = ASC(RIGHT$(H$,1)) / 10H                      ! GET ITEM TYPE
     If L% = 0 OR L% = 8 Then Begin                    ! Item SALE OR CANCEL
      PRIC% = 0                                        ! ASSUME NO PRICE
      K% = MATCHB(":",H$,3) + 1                        ! START OF PRICE
      J% = MATCHB(":",H$,K%)                           ! END OF PRICE
      If J% > K% Then Begin                            ! PRICE FOUND
       PRIC% = PACKBIN4(H$,K%-1,J%-K%)                 ! GET PRICE
       If L% = 8 Then PRIC% = 0 - PRIC%                ! INVERT PRICE FOR VOID
       Xvta% = Xvta% + PRIC%                           ! Acumula venta
      EndIf                                            ! PRICE FOUND
     EndIf                                             ! ITEM SALE OR CANCEL
    EndIf                                              ! Item ENTRY String
    Next.Itm.Vta:                                      ! Siguiente Item vendido
  Next I%                                              ! NEXT String
  Close Gr.Mkp.SesTmp%                                 ! Close File
  Venta.Art.Trx = Xvta%                                ! Retorna total de venta en la trx
End Function 

Function Dscto.Fpago(YTV$)                                                               ! Calculo Descuento aplicar
 String YTV$, Ykey$, Ydummy$, YFlag$, Yini$, Yfin$, Ydscto$, YPromo$, Ypromo2$, Lst.Promo$
 Integer*4 Dscto.Fpago, Xmonto%
 Integer*2 Xdscto%, XFoundPago%, Xtipo%
 Real Xdscto.Pago%, XptgPar%, XvtaArt%, Xpago%, Xbased%
 Gr.Mkp.Promo$ = "00"
 ASC.MKP.SITIO$ = "DSCTO.FPAGO"
 TS.TS11WERR$ = ""
 Open "R::$F:PFPAGO" KEYED RECL 16 AS Gr.Mkp.SesTmp% UNLOCKED NOWRITE NODEL						   ! Parametros dscto forma de pago
 If TS.TS11WERR$ <> "" Then Begin                                                        ! Error apertura archivo
 	  !Call Visores4690(1,"ERROR APERTURA ARCHIVO","TF:PFPAGO",1200,"L")
 	 	Dscto.Fpago = 0                                                                      ! No aplica dscto en la trx
 	  Exit Function                                                                        ! Sale del proceso
 EndIf                                                                                   !
 !Call Visores4690(1,"VALIDANDO "+yTV$,"TF:PFPAGO",1200,"L")
 
 TS.ER.RETURN = -1                                                                       ! Ctrl de error
 Ykey$ = Pack$(Right$("00"+Ytv$,2))                                                      ! Arma llave de busqueda
 READ FORM "C1 C1 2C3 C2 C4 C2";#Gr.Mkp.SesTmp% KEY Ykey$;                              \! Lee registro
            YDummy$, YFlag$, Yini$, Yfin$, Ydscto$, YPromo$, Ypromo2$                    !
 XFoundPago% = TS.ER.RETURN                                                              ! Retorna estado del proceso
 Close Gr.Mkp.SesTmp%                                                                    !
 If XFoundPago% <> -1 Then Begin                                                         ! Forma de pago no tiene descuento
 	  !Call Visores4690(1,"FORMA DE PAGO NO","TIENE DESCUENTO",1200,"L")
 	  Dscto.Fpago = 0                                                                      ! Retorna descuento nulo
 	  Exit Function                                                                        !
 EndIf
 !Ypromo$ = UnPack$(Ypromo$)																															 ! Promocion dirigida a lista de clientes
 
 Lst.Promo$ = (ASC.MKP.LSTPRO$)

  Lst.Promo$ = ASC.MKP.CADENA$

 
 If Match(Ypromo$,Lst.Promo$,1) <= 0 Then Begin           													 ! Si promo no para lista de clientes
 	  !Call visores4690(1,"promo no para clte",Ypromo$,1200,"l")
 	  Dscto.Fpago = 0                                                                      ! Retorna descuento nulo
 	  Exit Function                                                                        !
 EndIf                                                                                     !
 Yini$ = Unpack$(Yini$)                                                                  ! Fecha inicial de promocion
 YFin$ = Unpack$(YFin$)                                                                  ! Fecha final de la promocion
 If (Val(Date$) <= Val(Yini$)) And (Val(Date$) >= Val(YFin$)) Then Begin                 ! Trx por fuera de las fechas de parametro
 	  !Call visores4690(1,"FUERA DE FECHAS ",Ypromo$,1200,"l")
 	  Dscto.Fpago = 0                                                                      ! Retorna descuento nulo
 	  Exit Function                                                                        !
 EndIf                                                                                   !
 Xtipo%  = Val(unpack$(YFlag$))                                                          ! Tipo descuento 1 Dscto sobre compra 2. Dscto sobre articulos


 !Call visores4690(1,"TIPO DSCTO APLICA",STR$(Xtipo%),1200,"l")
 
 If Xtipo% = 1 Then Begin                                                                ! Dscto en la forma de pago
 	  Xdscto% = Val(unpack$(Ydscto$))                                                      ! Ptg Dscto otorgar al pago
 	  If Val(TS.IO.DATA$(7)) = 0 Then Begin                                                ! No ingreso Valor a Pagar
       Xpago% = TS.TOTALS(0,0,0) - TS.BALDUE(0)                                          ! Calcula el monto sobre el cual va a realizar el dscto
    EndIf Else XPago% = Val(TS.IO.DATA$(7))                                              !
    Xdscto.Pago% = (Xpago% /100) * Xdscto%                                               ! Calcula el descuento a otorgar
    Xpago% = Xpago% - Xdscto.Pago%                                                       ! Recalcula nuevo valor a pagar
    TS.IO.DATA$(7) = Str$(Xpago%)                                                        ! Retorna nuevo valor a pagar
    Dscto.Fpago = Xdscto.Pago%                                                           ! Retorna descuento otorgado
    Gr.Mkp.Promo$ = Ypromo$                                                              ! Codigo de la promocion
    Exit Function                                                                        ! Termina proceso
 EndIf                                                                                   !   

 If Xtipo% = 2 Then Begin                                                                ! Descuento a articulos ligados
 	  Xdscto% = Val(unpack$(Ydscto$))                                                      ! Ptg Dscto otorgar al pago
 	  If Val(TS.IO.DATA$(7)) = 0 Then Begin                                                ! No ingreso Valor a Pagar
       Xpago% = TS.TOTALS(0,0,0) - TS.BALDUE(0)                                          ! Calcula el monto sobre el cual va a realizar el dscto
    EndIf Else XPago% = Val(TS.IO.DATA$(7))                                              ! 
    XvtaArt% = Venta.Art.Trx(Ytv$)                                                       ! Valor vendido en la trx
    If XvtaArt% <> 0 Then Begin                                                          ! Si articulos vendidos
    	 If (XPago% >= XvtaArt%) Then Begin                                                ! Si el pago cubre la venta de los productos
          Xdscto.Pago% = (XvtaArt% /100) * Xdscto%                                       ! Calcula el descuento a otorgar
          Xpago% = Xpago% - Xdscto.Pago%                                                 ! Recalcula nuevo valor a pagar
          TS.IO.DATA$(7) = Str$(Xpago%)                                                  ! Retorna nuevo valor a pagar
          Dscto.Fpago = Xdscto.Pago%                                                     ! Retorna descuento otorgado
          Gr.Mkp.Promo$ = UnPack$(Ypromo$)                                               ! Codigo de la promocion
          Exit Function                                                                  ! Termina proceso
       Endif Else Begin                                                                  ! Pago no cubre compra total de los articulos
       	  XptgPar% = (XPago% / XvtaArt%) * 100                                           ! Ptg de participacion en la compra
       	  Xbased%  = (XvtaArt% /100) * XptgPar%                                          ! Calcula Base del descuento segun ptg participacion
          Xdscto.Pago% = (Xbased% /100) * Xdscto%                                        ! Calcula el descuento a otorgar
          Xpago% = Xpago% - Xdscto.Pago%                                                 ! Recalcula nuevo valor a pagar
          TS.IO.DATA$(7) = Str$(Xpago%)                                                  ! Retorna nuevo valor a pagar
          Dscto.Fpago = Xdscto.Pago%                                                     ! Retorna descuento otorgado
          Gr.Mkp.Promo$ = UnPack$(Ypromo2$)                                              ! Codigo de la promocion
          Exit Function                                                                  ! Termina proceso
       EndIf                                                                             !
 	  EndIf Else Dscto.Fpago = 0                                                           ! Si no items controlados vendidos dscto cero
 	  Exit Function                                                                        ! Termina proceso
 EndIf                                                                                   !
 Dscto.Fpago = 0                                                                         ! Retorno cero en dscto por basura en archivo
 
End Function 

Sub CALCULO.DSCTO.PAGO(XTV$)
 String XTV$
 Integer*4 Xdscto.Pago%
   
   !Call visores4690(1,"BUSCANDO PROMO PAGO",XTV$,1200,"L")
   
   ASC.MKP.SITIO$ = "CALCULO.DSCTO.PAGO"
   Xdscto.Pago% = 0                                                                        ! Init Variable
   If Gr.Mkp.CtlPago% = 0 Or Gr.Mkp.CtlPago% = -2  Then Begin                              ! Descuento no otorgado
   	 Xdscto.Pago% = Dscto.Fpago(Xtv$)                                                      ! Descuento a otorgar
     If Xdscto.Pago% <> 0 Then Begin                                                       ! Si hay descuento a aplicar
     	If (TS.IO.KEYS(1) <> 70) Then Begin 			                                           ! Si No es anulacion del pago
     		If Gr.Mkp.CtlPago% = -2 Then Begin                                                 ! Ya hay un dscto aplicado en la trx
     			 Call VISOR.AND.BORRAR("SOLO UN DESCUENTO POR FORMA DE PAGO ")                   ! Msg de alerta al cajero
     			 Exit Sub                                                                        !
     	  EndIf                                                                              !
     	  
     	  Call VISORES4690(1,"APLICANDO DESCUENTO","FORMA DE PAGO",1200,"L")
     	  
 	      SL.END = SL.END + 1                                                                ! Aumenta apuntador de String 
	      SL.STR$(SL.END) = Pack$("03")+":"+Pack$(Gr.Mkp.DctoPago$) + ":" +	\                ! Almacena String de descuento
			    	              Pack$("00")+ ":" + Pack$(Str$(Xdscto.Pago%))+":"+Pack$("00") + ":" ! 
        TS.BALDUE(0)      = TS.BALDUE(0) - Xdscto.Pago%                                    ! Actualiza variables de la 
        SL.HD.GROSSNEG    = SL.HD.GROSSNEG + Xdscto.Pago%                                  ! Aplicacion y de total de la 
        TS.GROSSNEG       = TS.GROSSNEG + Xdscto.Pago%                                     ! Transaccion eliminando el descuento
        TS.TOTALS(0,0,1)  = TS.TOTALS(0,0,1) + Xdscto.Pago%                                !
        TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) + Xdscto.Pago%                               !
        TS.TOTALS(0,0,0)  = TS.TOTALS(0,0,0) - Xdscto.Pago%                                !
        Call Format.Amount(Xdscto.Pago%)
        TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)
        TS.TEMP2$ = Right$("          "+TS.IO.DATA$(7),10)
        Call VISOR.AND.BORRAR("DCTO PAGO:"+TS.TEMP1$+"VLR PAGO :"+TS.TEMP2$)               ! Presenta informacion
        Gr.Mkp.CtlPago% = -1                                                               ! Dscto en aplicacion 
        Gr.Mkp.DctoPago% = Xdscto.Pago%
        Gr.Mkp.PagoRel$ = Xtv$
        TS.TEMP1$ = ""
        ASC.MKP.TMP1$  = Pack$(Gr.Mkp.Promo$)                  +                          \! Codigo de la promocion  
                      ":"+PACK$("00")                          +	                        \! Item vendido            
                      ":"+PACK$("00")                          +	                        \! Qty vendido             
                      ":"+PACK$("00")                          +	                        \! Precio Vendido          
                      ":"+PACK$("00")                          +	                        \! Dscto otorgado          
                      ":"+Pack$("00")                          +                          \! Signo de la operacion   
                      ":"+Pack$(Xtv$)                          +                          \! Forma pago promocion    
                      ":"+Pack$(Str$(Xdscto.Pago%))            +                          \! Dscto forma de pago     
                      ":"+Pack$("03")                                                      ! Tipo de promocion - forma de pago
        Call Grabacion.Cadena.Usuario2("68",ASC.MKP.TMP1$)                                 ! Almacena user data      
        Exit Sub 
      EndIf Else Begin                                                                     ! Es una anulacion de pago
      	!Call VISORES4690(1,"ANULADO DESCUENTO","FORMA DE PAGO",1200,"L")
      	
 	      SL.END = SL.END + 1                                                                ! Aumenta apuntador de String 
	      SL.STR$(SL.END) = Pack$("04")+":"+Pack$(Gr.Mkp.DctoPago$) + ":" +	\                ! Almacena String de descuento
			    	              Pack$("00")+ ":" + Pack$(Str$(Xdscto.Pago%))+":"+Pack$("00") + ":" ! 
        TS.BALDUE(0)      = TS.BALDUE(0) + Xdscto.Pago%                                    ! Actualiza variables de la 
        SL.HD.GROSSNEG    = SL.HD.GROSSNEG - Xdscto.Pago%                                  ! Aplicacion y de total de la 
        TS.GROSSNEG       = TS.GROSSNEG - Xdscto.Pago%                                     ! Transaccion eliminando el descuento
        TS.TOTALS(0,0,1)  = TS.TOTALS(0,0,1) - Xdscto.Pago%                                !
        TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) - Xdscto.Pago%                               !
        TS.TOTALS(0,0,0)  = TS.TOTALS(0,0,0) + Xdscto.Pago%                                !
        Call Format.Amount(Xdscto.Pago%)
        TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)
        TS.TEMP2$ = Right$("          "+TS.IO.DATA$(7),10)
        Call VISOR.AND.BORRAR("DCTO PAGO:"+TS.TEMP1$+"VLR PAGO :"+TS.TEMP2$)               ! Presenta informacion
        Gr.Mkp.CtlPago% = -1                                                               ! Dscto en aplicacion 
        Gr.Mkp.DctoPago% = Xdscto.Pago%
        Gr.Mkp.PagoRel$ = Xtv$
        TS.TEMP1$ = ""
        ASC.MKP.TMP1$  = Pack$(Gr.Mkp.Promo$)                  +                          \! Codigo de la promocion  
                      ":"+PACK$("00")                          +	                        \! Item vendido            
                      ":"+PACK$("00")                          +	                        \! Qty vendido             
                      ":"+PACK$("00")                          +	                        \! Precio Vendido          
                      ":"+PACK$("00")                          +	                        \! Dscto otorgado          
                      ":"+Pack$("01")                          +                          \! Signo de la operacion   
                      ":"+Pack$(Xtv$)                          +                          \! Forma pago promocion    
                      ":"+Pack$(Str$(Xdscto.Pago%))            +                          \! Dscto forma de pago     
                      ":"+Pack$("03")                                                      ! Tipo de Promocion - Forma Pago
        Call Grabacion.Cadena.Usuario2("68",ASC.MKP.TMP1$)                                 ! Almacena user data      
        Exit Sub 
      EndIf  
     EndIf 
   EndIf 

End Sub 


Sub PRMFPAGO(XOPT%) Public																								! Modulo recaudo Propinas
Integer*4 XOPT%, XAUX%																										! Variables 

If Ue.mktp.Line% <> -1 Then Exit Sub                                      ! Proyecto desactivado

!--- EAMTSU02.J86
If (XOPT% = 02 Or XOPT% = 56) Then Begin 																	! Init variables New trx
	 Gr.Mkp.CtlPago% = 0
	 Gr.Mkp.PagoRel$ = "00" 
	 Gr.Mkp.DctoPago% = 0 
EndIf 

!--- EAMTSU14.J86
If XOPT% = 14 Then Begin 																																	 ! Ue secuencias de tecleo

 !If TS.IO.KEYS(7) > 90 AND TS.IO.KEYS(7) < 97 Then Begin                                   ! Si forma de pago 
 	
 If TS.IO.MOTORKEY = 73 Then Begin 
 	If (Gr.Mkp.CtlPago% = -1) Then Begin                                                     ! Si dscto aplicado incompleto en la trx
 		    Call VISOR.AND.BORRAR("DSCTO FORMA PAGO MALAPLICADO           ")                   ! Msg de Alerta al cajero
        Call VISOR.AND.BORRAR("ANULANDO DESCUENTO  APLICADO EL LA TRX ")                   ! Msg de Alerta al cajero
 	      SL.END = SL.END + 1                                                                ! Aumenta apuntador de String 
	      SL.STR$(SL.END) = Pack$("04")+":"+Pack$(Gr.Mkp.DctoPago$) + ":" +	\                ! Almacena String de descuento
			    	              Pack$("00")+ ":" + Pack$(Str$(Gr.Mkp.DctoPago%))+":"+Pack$("00") + ":" ! 
        TS.BALDUE(0)      = TS.BALDUE(0) + Gr.Mkp.DctoPago%                                ! Actualiza variables de la 
        SL.HD.GROSSNEG    = SL.HD.GROSSNEG - Gr.Mkp.DctoPago%                              ! Aplicacion y de total de la 
        TS.GROSSNEG       = TS.GROSSNEG - Gr.Mkp.DctoPago%                                 ! Transaccion eliminando el descuento
        TS.TOTALS(0,0,1)  = TS.TOTALS(0,0,1) - Gr.Mkp.DctoPago%                            !
        TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) - Gr.Mkp.DctoPago%                           !
        TS.TOTALS(0,0,0)  = TS.TOTALS(0,0,0) + Gr.Mkp.DctoPago%                            !
        Gr.Mkp.CtlPago%   = 0                                                              ! Reinicializa proceso
        Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10) : TS.IO.MOTORKEY = 73                     ! Anula secuencia de pago y simula tecla total
  EndIf 
 EndIf 

	
 If TS.IO.KEYS(7) > 90 AND TS.IO.KEYS(7) < 97 Then Begin                  ! Si forma de pago 
   TS.TEMP3$ = Str$(TS.IO.KEYS(7) - 90)                                   ! Toma forma de pago
   If TS.IO.DATA$(3) = "" Then TS.TEMP3$ = TS.TEMP3$ + "1" Else          \! Toma variedad de pago 
      TS.TEMP3$ = TS.TEMP3$ + TS.IO.DATA$(3)                              ! 
   Call CALCULO.DSCTO.PAGO(TS.TEMP3$)                                     ! Si hay forma de pago busca dscto
 EndIf Else TS.TEMP3$ = "00"                                              ! No es pago 
 	
EndIf 

If XOPT% = 60 Then Begin 

 If (TS.LINETYPE = 2 And TS.LINEDATA = 0 ) AND (Gr.Mkp.CtlPago% = -1) Then Begin     ! Si en impresion de forma de pago y dscto aplicado
 	    If (TS.IO.KEYS(1) <> 70) Then Gr.Mkp.DctoPago% = Gr.Mkp.DctoPago% * -1         ! Si es una anulacion de pago
      Call Format.Amount(Gr.Mkp.DctoPago%)                                           ! Formatea valor
      TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)                                  ! Ajuste dato valor
      TS.TEMP2$ = "Dscto x Forma Pago   :$" + TS.TEMP1$                              ! Arma mensaje a tiquete
      Call U.IMPRIME(TS.TEMP2$,6100H)                                                ! Imprime vlr dscto
      Gr.Mkp.CtlPago% = -2                                                           ! Cierra control 
      If (TS.IO.KEYS(1) = 70) Then Gr.Mkp.CtlPago% = 0                               ! Si anulacion reinicia proceso
 EndIf	

EndIf 

End Sub 
