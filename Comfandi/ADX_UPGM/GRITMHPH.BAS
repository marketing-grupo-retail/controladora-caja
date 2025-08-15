!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRITMHPH.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : DSCTO Item Happy Hour            *
!**************************************************
!
! Rutinas Definidas
!

%ENVIRON T

Integer*1 Global Gr.Itm.Open%, Gr.Itm.Anul%, Gr.HpH.Fail% 
Integer*4 Global Gr.Itm.Dscto%, Gr.Itm.TotDsct%, Gr.Itm.DscItm%, Asc.Tmp.Apun%
Integer*4 Global Asc.MKP.DSCT%, Asc.MKP.DSCTO%, Gr.Mkp.DstAB%, ASC.MKP.TOTDSCT%, ASC.MKP.DscOtorgado%
String    Global ASC.MKP.TMP1$, Gr.Mkp.Promo$, Ue.Tmp.Sgn$, ASC.MKP.CADENA$, ASC.MKP.DESCRIPTOR$
String    Global ASC.IR.DATA$, ASC.IR.NDEPTO$

%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          ! Variables de transacciones
%INCLUDE EAMTERMS.J86          ! Variables de terminales
%INCLUDE EAMTOPTS.J86	         ! Variables Terminal Options
%INCLUDE EAMTSEVA.J86          ! Variables EMS
%INCLUDE EAMITEMR.J86          ! Variables EAMITEMR

Function FORMAT.AMOUNT(AMT1) EXTERNAL
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                      *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                         *!
  STRING    DATA$      !* String to hold the data read from file.            *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                               *!
End Sub 

SUB IRRFEC.SPLIT1 (RECORD$) EXTERNAL
  STRING    RECORD$    !* The record (including the key) read from file      *!
End Sub 

Function U.Imprime(UE.String$,UE.STATION) External													! Rutina de Impresion
String UE.String$      									                  									! Variables temporales
Integer*2 UE.STATION									                    									!
End Function
!--- Rutina de Impresion

Function  VISORES4690(U.D.VISOR%, U.D.LINEA1$, U.D.LINEA2$,U.D.TIEMPO%,U.D.POSICION$) External ! Msg Display
String U.d.Linea1$, U.d.Linea2$, U.d.Posicion$															! Variables de
Integer*2 U.d.visor%, U.d.tiempo%   																				! trabajo
String U.D.Linea1$, U.D.Linea2$		
End Function
!--- Fin manejo de visores

Function Grabacion.Cadena.Usuario2(X.Clave$,X.Datos$) External
String X.Clave$, X.Datos$
End Function 
!--- Fin grabacion user data

Function Asic.Getunpk(X.B$,X.J%) External
Integer*2 X.J%, X.K%
String    X.B$, Asic.Getunpk
End Function 

Function DIA.SEMANA(FN.DATE$) External
  STRING FN.DATE$                      ! Date passed to function  CCYYMMDD
  INTEGER*2 DIA.SEMANA                 ! Function return  0=Sunday...6=Saturday
End Function
!-- Fin rutina dia de la semana

Sub Find.Price.Hph																													! Busca Precio Item
    IR.ITEMCODE$ = Pack$(Right$("000000000000"+SL.IT.ITEMCODE$,12))					! Llave de busqueda
    Call IRRFEC.READ01 (IR.ITEMCODE$, 4, TS.TEMP1$, 0)       								! Lectura Datos Itemr
    Call IRRFEC.SPLIT1( TS.TEMP1$ ) 			                   								! Entrega datos al eamitemr.j86
    If UNPACK$(IR.INDICAT2$) = "05" Then Begin 															! Si metodo precio 5
    	 IR.ITEMCODE$ = IR.SALEQUAN$ + IR.SALEPRIC$														! Toma Item Ligado
       Call IRRFEC.READ01 (IR.ITEMCODE$, 4, TS.TEMP1$, 0)       					  ! Lectura Datos Itemr
       Call IRRFEC.SPLIT1( TS.TEMP1$ ) 			                   							! Entrega datos al eamitemr.j86
    EndIf																																		!
End Sub 

Function GRITMHPH(X.DPTO$) Public    ! Modulo Descuento por seccion 
String X.DPTO$, XC$, XLEC$, \
       TMP.KEY$,  DM.CUPON$,  DM.VLRCON$, \
       UE.DIA$(1), UE.HORA$(2), UE.HORA.ACT$, XLINK$, Ux.Promo$, XTmp$, \
       GR.FINI$, GR.FFIN$, GR.TDSC$
Integer*2 X.Dia%
Integer*4 GRITMHPH, X.G%, XdsctOld%
Gr.Itm.Dscto% = 0
XdsctOld%     = 0 
XLEC$ = "C16 C7 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C2 2C3 C1"  !  
For X.G% = 1 TO LEN(ASC.MKP.CADENA$) STEP 4			                          ! Hasta longitud cadena
  XTmp$ = Mid$(ASC.MKP.CADENA$,X.G%,4)			                              ! Extrae lista promociones  
  Dim UE.DIA$(10)
  Dim UE.HORA$(10,2)
  TMP.KEY$ = Right$("000000000000"+X.DPTO$,12) + XTmp$                     ! Arma llave de busqueda
  Gr.Itm.Dscto% = 0                                                       ! Init Variable
  TS.ER.RETURN = -1 																											! Control de errores
  Read Form XLEC$; #77 KEY TMP.KEY$;                                      \! LECTURA Happy Hour Item
     TMP.KEY$,   DM.VLRCON$,                   \
     UE.DIA$(1), UE.HORA$(1,1), UE.HORA$(1,2), \
     UE.DIA$(2), UE.HORA$(2,1), UE.HORA$(2,2), \
     UE.DIA$(3), UE.HORA$(3,1), UE.HORA$(3,2), \
     UE.DIA$(4), UE.HORA$(4,1), UE.HORA$(4,2), \
     UE.DIA$(5), UE.HORA$(5,1), UE.HORA$(5,2), \
     UE.DIA$(6), UE.HORA$(6,1), UE.HORA$(6,2), \
     UE.DIA$(0), UE.HORA$(0,1), UE.HORA$(0,2), \		
     Ux.Promo$,  GR.FINI$, GR.FFIN$, GR.TDSC$
     
  If TS.ER.RETURN = -1 Then Begin 																				! Si tiene descuento
  	 GR.FINI$ = UNPACK$(GR.FINI$) : GR.FFIN$ = UNPACK$(GR.FFIN$)          ! Toma fechas control descuento
  	 If Val(DATE$) < Val(GR.FINI$) Then Exit Function                     ! Si por fuera de las fechas 
  	 If Val(DATE$) > Val(GR.FFIN$) Then Exit Function                     ! de control del descuento
     X.Dia% = DIA.SEMANA("20"+DATE$)																		  ! Retorna el dia de la semana 
     If UE.DIA$(X.Dia%) = "1" Then Begin 																	! Si Dia activo para promocion
     	UE.HORA.ACT$ = LEFT$(TIME$,4)																				! Toma hora del sistema HHMM
	    If Val(UE.HORA$(X.Dia%,1)) <= VAL(UE.HORA.ACT$) And	\           		! Si se encuentra entre el rango
	       Val(UE.HORA$(X.Dia%,2)) >= VAL(UE.HORA.ACT$) Then Begin          ! de horas parametrizado
           GRITMHPH = Val(UnPack$(DM.VLRCON$))                            ! Retorna valor final Item
           Gr.Itm.Dscto% = Val(UnPack$(DM.VLRCON$))                       ! Retorna ptg dscto aplicar
           If Gr.Itm.Dscto% > XdsctOld% Then Begin                        ! Dscto Mayor al anterior 
           	 XdsctOld% = Gr.Itm.Dscto%                                    ! Almacena Dscto 
           	 TS.TEMP6$ = Unpack$(Ux.Promo$)                               ! Numero de la promocion
           EndIf
      EndIf Else Gr.Itm.Dscto% = 0      			                    		    ! Dia no activo de la promocion
     EndIf Else Gr.Itm.Dscto% = 0        					    										! Dia no activo de la promocion
  EndIf Else Begin 																												!
     Gr.Itm.Dscto% = 0            																				! Codigo del cupon de descuento 
  EndIf 
Next X.G%
  GRITMHPH = XdsctOld%     																						    ! Retorna descuento 
End Function 
!--- Fin promocion por departamento

Sub DCTOXITM(Xopt%) Public																			! Modulo Descuento por Happy Hour Item 
Integer*4 Xopt%, Gr.Tmp%, Gr.Pric%															! User Exit a Invocar

!-- EAMTSU07.J86
If Xopt% = 07 Then Begin                                        ! Si UE Carga opciones
	   TS.ER.RETURN = -1
	   If Gr.Itm.Open% = 0 Then Begin 														! Control de apertura
        Open "R::$F:ITMHPH" KEYED RECL 95 AS 77 UNLOCKED NOWRITE NODEL        ! Descuento por seccion   
        If TS.ER.RETURN NE -1 Then Begin 												! Error de Apertura
           Call U.IMPRIME("ERROR OPEN TF:ITMHPH",6100H)			    ! Msg de Alerta
           Gr.HpH.Fail% = 0
           Exit Sub 																						!
        EndIf Else Gr.HpH.Fail% = -1    												! Apertura OK
        Gr.Itm.Open% = -1																				! Apertura OK%
     EndIf 																											!
EndIf 																													!

If Gr.HpH.Fail% <> -1 Then Exit Sub                             ! Falla proceso apertura archivo

!-- EAMTSU02.J86
If Xopt% = 02 Then Begin                                        ! Si UE Inicia trx
   Gr.Itm.TotDsct%  = 0 
   Gr.Itm.DscItm%   = 0 
   Gr.Tmp%          = 0 
   Gr.Itm.Anul%     = 0 
EndIf 

!-- EAMTSU20.J86
If Xopt% = 20 Then Begin 

EndIf 

!-- EAMTSU43.J86
If Xopt% = 43 Then Begin                                        ! Si UE Control de precio
TS.TEMP1I4 = GRITMHPH(SL.IT.ITEMCODE$)	      	                ! Valida si descuento al Item
If TS.TEMP1I4 <> 0 Then Begin                                   ! Si Precio Happy Hour
 If TS.TEMP1I4 <= SL.IE.SALEPRIC Then Begin                     ! Si precio menor
 	 Call Find.Price.Hph                                          ! Busca Precio 
 	 SL.IE.SALEPRIC = Val(UnPack$(IR.SALEPRIC$))                  ! Toma precio del producto
	 Gr.Pric%       = SL.IE.SALEPRIC                              ! Precio Actual del producto
   If IR.INDICAT0 AND 40H Then Begin                            ! Si articulo pesable
      SL.IE.SALEPRIC = TS.TEMP1I4                               ! Precio especial
      SL.IT.XPRICE   = (SL.IE.SALEPRIC * SL.IE.QTYORWGT) / 1000 ! Precio total 
   EndIf Else Begin                                             !
      SL.IE.SALEPRIC = TS.TEMP1I4                               ! Precio especial
      SL.IT.XPRICE   = SL.IE.SALEPRIC * SL.IE.QTYORWGT          ! Precio total 
   EndIf                                                        !
   Gr.Itm.DscItm% = (Gr.Pric% - TS.TEMP1I4)                     ! Dscto Aplicado
   IR.PRICE1      = SL.IE.SALEPRIC                              ! Precios para impresion
   IR.PRICE2      = SL.IE.SALEPRIC                              ! Precios para impresion
   Gr.Mkp.Promo$  = TS.TEMP6$                                   ! Codigo de la promocion 
   If Asc.MKP.DSCT% <> 0 Then                                  \! Retira dscto anterior calculado
   	  ASC.MKP.TOTDSCT% = ASC.MKP.TOTDSCT% - Asc.MKP.DSCT%       !
   If Asc.MKP.DSCTO% <> 0 Then                                 \! Retira dscto anterior calculado
   	  ASC.MKP.TOTDSCT% = ASC.MKP.TOTDSCT% - ASC.MKP.DscOtorgado% !
	 Asc.MKP.DSCT%  = 0 																					! Elimina descuentos aplicacion
	 ASC.MKP.DSCTO% = 0 																				  ! de promociones
	 Gr.Mkp.DstAB%  = 0                                           !
 EndIf
EndIf
    
EndIf 

!-- EAMTSU53.J86
If Xopt% = 53 Then Begin                                        ! Si UE Recup trx

EndIf 

!-- EAMTSU56.J86
If Xopt% = 56 Then Begin                                        ! Si UE Suspencion trx
   Gr.Itm.TotDsct%  = 0 
   Gr.Itm.DscItm%   = 0 
   Gr.Tmp%          = 0 
   Gr.Itm.Anul%     = 0 
EndIf 

!-- EAMTSU60.J86
If Xopt% = 60 Then Begin                                        ! Si UE Impresion tiquete

If TS.LINETYPE = 1 Then Begin                                  \! Es un articulo
 If (Gr.Itm.DscItm% <> 0 ) Then Begin                           ! Dscto Happy Hour 
 	 ASC.MKP.TMP1$ = ASC.MKP.DESCRIPTOR$
 	 ASC.MKP.TMP1$ = LEFT$(ASC.MKP.TMP1$+STRING$(21," "),21)
 	 If IR.INDICAT0 AND 40H Then Begin                            ! Si articulo pesable
 	 	  TS.TEMP5I4 = (Gr.Itm.DscItm% * SL.IE.QTYORWGT) / 1000
   EndIf Else Begin 																					  ! Si articulo unitario
   	TS.TEMP5I4 = Gr.Itm.DscItm% * SL.IE.QTYORWGT
   EndIf
 	 If (TS.IO.KEYS(1) = 70 OR TS.IO.KEYS(8) = 67 ) Then           \! Es una anulación
    Call FORMAT.AMOUNT(TS.TEMP5I4) Else \! Es una venta
   	Call FORMAT.AMOUNT((TS.TEMP5I4 * -1))  !
   	
   ASC.MKP.TMP1$ = ASC.MKP.TMP1$ + RIGHT$("          "+TS.TEMP1$,10)
   ASC.MKP.TMP1$ = RIGHT$(STRING$(38," ")+ASC.MKP.TMP1$,38)
   Call U.IMPRIME(ASC.MKP.TMP1$,2100H)
   Call U.IMPRIME(ASC.MKP.TMP1$,4101H)
   Ue.Tmp.Sgn$ = "00"
   If (TS.IO.KEYS(1) = 70 OR TS.IO.KEYS(8) = 67 ) Then               \! Es una anulación
   	  Ue.Tmp.Sgn$ = "01"                                             ! Reporta signo negativo
   If NOT TS.RETV.IN.PROGRESS Then Begin
      ASC.MKP.TMP1$  = Pack$(Gr.Mkp.Promo$)                     +   \! Codigo de la promocion
                ":"+PACK$(SL.IT.ITEMCODE$)                      +	  \! Item vendido
                ":"+PACK$(Str$(SL.IE.QTYORWGT))                 +	  \! Qty vendido
                ":"+PACK$(Str$(SL.IE.SALEPRIC+Gr.Itm.DscItm%))  +	  \! Precio Vendido
                ":"+PACK$(Str$(TS.TEMP5I4))                     +	  \! Dscto otorgado
                ":"+Pack$(Ue.Tmp.Sgn$)                          +   \! Signo de la operacion 
                ":"+Pack$("00")                                 +   \! Forma pago promocion
                ":"+Pack$("00")                                 +   \! Dscto forma de pago
                ":"+Pack$("14")                                      ! Tipo Promocion - Happy Hour Item
      Call Grabacion.Cadena.Usuario2("68",ASC.MKP.TMP1$)             ! Almacena user data
      If (TS.IO.KEYS(1) = 70 OR TS.IO.KEYS(8) = 67 ) Then           \! Es una anulación
       ASC.MKP.TOTDSCT% = ASC.MKP.TOTDSCT% + (TS.TEMP5I4 * -1 ) Else \
       ASC.MKP.TOTDSCT% = ASC.MKP.TOTDSCT% + (TS.TEMP5I4)
    ENDIF 
    Gr.Itm.DscItm% = 0 
 EndIf
EndIf

EndIf 


End Sub 
