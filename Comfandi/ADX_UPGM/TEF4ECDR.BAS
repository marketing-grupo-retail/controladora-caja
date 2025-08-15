!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : TEF4ECDR.BAS                     *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Transferencia Electronica *
!                de Fondos Ecuador - Datafonos    *
!                Version 4 Medianet - Datafast    *
!**************************************************
!Modificaciones:
!

%ENVIRON T

!
! Supermarket application global data
!
Integer*1 Global Gr.Tef2.AnlRc%, Gr.RsmTkt.Ind%															! Anulacion recibo 
Integer*2 Global Gr.Tef2.Ok%, 																						 \! Control Proyecto TEF
                 Gr.Tef2.Key%,                                             \! Tecla motorkey TEF
                 Gr.Tef2.Anul%,                                            \! Ctrl Anul compra
                 Gr.Tef2.Ncopia%,                                          \! Nro copias vocuher
                 Gr.Tef2.Stat%,                                            \! Ctrl generacion vouchers
                 Gr.Tef2.AnOff%,                                           \! Anul fuera de trx
                 Gr.Tef2.ClOff%,                                           \! Anul fuera de trx
                 GR.TEF2.POS%,                                             \!
                 Gr.Tef2.Intrx%                                             ! Control trx tef

String    Global Gr.Tef2.Msg$(1),																					 \! Vector Mensajes
                 Gr.Tef2.TipVar$,																					 \! Tipo variedad pago TEF II
                 Gr.Tef2.SeqTrx$,                                          \! Sequential Trx
                 Gr.Tef2.TrxOper$,                                         \! Tipo de operacion
                 Gr.Tef2.NumRcv$,                                          \! Numero de recibo
                 Gr.Tef2.Voucher$(2),                                      \! Voucher impresion
                 Gr.Tef2.RtaTef$(2),                                       \! Rta Trama Tef
                 Gr.Tef2.AutoDife(1),                                      \! Lista diferidos
                 Gr.Tef2.Binparam(2),																			 \! Lista definicion de bines 
                 Gr.Tef2.NumAut$,                                          \! Numero autorizacion
                 Gr.Tef2.CardAnul$,                                        \! Numero tarjeta anulada
                 Gr.Tef2.TrackII$,                                         \! Track II tarjeta
                 Gr.Tef2.IdCte$,                                           \! Operador trx corrientes
                 Gr.Tef2.IdDif$,                                           \! Codigo del diferido
                 Gr.Tef2.Idtrxdif$,                                        \! Operador trx diferidos
                 Gr.Tef2.Cuota$,                                           \! Numero de cuotas diferido
                 Gr.Tef2.Ndife$,                                           \! Nombre del diferido
                 Gr.Tef2.Tpdif$,                                           \! Tipo diferido con o Sin interes
                 Gr.Tef2.Sequen$,                                          \! Numero de secuencial datafono
                 Gr.Tef2.MidData$,																				 \! Mid DataFast
                 Gr.Tef2.MidMNet$,																				 \! Mid MediaNet
                 Gr.Tef2.TidData$,																				 \! Tid DataFast
                 Gr.Tef2.TidMNet$,																				 \! Tid MediaNet
                 Gr.Tef2.Bin$                                               ! Numero bin tarjeta
                 
String    Global Gr.Tef2.tmp1$,                 									         \! Temporales de trabajo
                 Gr.Tef2.tmp2$,                      											 \!
                 Gr.Tef2.tmp3$, 																					 \!
                 Gr.Tef2.tmp4$,																						 \!
                 Gr.Tef2.tmp5$,                                            \!
                 Gr.Tef2.Str$

Integer*4 Global UE.TABLAIVA(2),                                           \! Totales manejo
                 UE.TOTALS(1),                                             \! Impuestos IVA
                 Gr.Tef2.Ttrx%,                                            \! Total trx Tef
                 Asc.Tmp.Apun%,																						 \! Control lectura strings
                 Gr.Tef2.LstBin%																						! Total bines cargados                 
                 
Integer   Global Sl.End
Integer*1 GLOBAL TS.RETV.IN.PROGRESS																				! retrieval in progress
Integer*1 GLOBAL TS.RECOVERY                    														! terminal recovery flag
Integer*1 Global Gr.Tef.TrxSale%, Gr.PromOr.CtlPago%, Gr.PromOr.AplNew%,   \! Motor promociones Or
                 Gr.Fiscal.Tktnf%
String    Global Gr.PromOr.Bin$																							! Motor promociones Or
Integer*4 Global Gr.PromOr.Total%
Integer*2 Global EP.ETO.PRINT.POST.TRANS


%INCLUDE EAMTSWKG.J86          																			        ! working storage
%INCLUDE EAMTRANS.J86          																			        ! working storage
%INCLUDE EAMITEMR.J86          																			        ! Maestra productos
%INCLUDE EAMTOPTS.J86          																			        ! working storage
%INCLUDE EAMTSXHC.J86          																							! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          																							! Rutinas de Assembler

%INCLUDE RECATSSU.011          																			        ! Rutinas Genericas Grupo Retail

Function FORMAT.AMOUNT(AMT1) External   																		! Formateo de valores
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub TSPREC01 EXTERNAL          																							! print routine
End Sub

Sub TSHIECET External								    																		!  Tono de alerta
End Sub

Sub TSTCEC01 EXTERNAL          																							! tender cashing
End Sub                        																							!

Sub TSTDEC01 EXTERNAL
End Sub

Sub TSTEEC01 EXTERNAL          																							! tender cashing
End Sub                        																							!

Sub TEFII.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$) External							! Auditoria proceso
String X.ENVIA$, X.LLEGA$, X.SALE$, X.RTA$
End Sub 

Function GR.LECTURA.TARJETA External																				! Retorna datos tarjeta
Integer*1 GR.LECTURA.TARJETA
End Function 

Function GR.CONSULTA.TARJETA External																				! retorna track ii tarjeta
Integer*1 GR.CONSULTA.TARJETA
End Function 

Sub GR.FINAL.DIA.TEF External																								! Proceso cierre dia datafono
End Sub 

Sub  GR.CONFIG.PINPAD	External																				 	 		! proceso configuracion pinpad
End Sub 

Sub GR.ACTSW.PINPAD	External																				        ! proceso actualiza sw pinpad
End Sub 

Sub Gr.Carga.Bines.Tef External																							! Carga lista de bines 
End Sub 																																		! red adquieriente

Function CALCULO.DSCTO.PAGO(XTV$) External																	! Promociones para el Bin 
 String XTV$
 Integer*1 CALCULO.DSCTO.PAGO
End Function 

Function Gr.Diferidos.TefII(Xaut%, Xtrx$) External													! Manejo diferidos
Integer*1 Xaut%, Gr.Diferidos.TefII
String    Xtrx$
End Function 

Sub TEFII.BUSCA.LIMITE.TV(X.TIPO.VAR$) External															!
String X.TIPO.VAR$
End Sub 

Function GR.DATOS$(UMSG1$,UMSG2$) Public     								  							! Captura de datos         
String UMSG1$, UMSG2$, A.DAT$, AB$, AQ$								                      ! Definicion de variables  
String GR.DATOS$, ADATA$																		                !                          
   TS.IO.STATE = 10                    												              ! Enter/Data state         
   Call VISORES4690(1,UMSG1$,UMSG2$,0,"L")										              ! Msg al operador          
   UNLOCKDEV 32,7                                							              ! Desbl Teclado y pasa     
   WAIT 32;100                                   							              ! al Estado ACCT NO        
   READ #32;ADATA$,AB$,AB$,AB$,               						                 \! Lectura Teclado y Status 
            AB$,AB$,AB$,AB$,AB$,AB$,AQ$              					              ! teclado con 10 Datos     
   IF MID$(ADATA$,13,1)  = "I" THEN GR.DATOS$ = "E"						              ! Presiono Borrar          
   IF MID$(ADATA$,14,1) <> " " THEN GR.DATOS$ = "E"        		              ! Si hay Errror            
   If Mid$(Adata$,13,1) = "P" Then Begin         						                ! Si Presiono Enter        
   	 If (Len(Aq$)-1) > 0 Then Begin																					!
        GR.Datos$ = Right$(Aq$,Len(Aq$)-1)         				  	              ! Asigna Dato Capturado    
     EndIf Else GR.Datos$ = ""														  	              ! Error retorna borrar     
   EndIf 																											              !                          
End Function																																!
!--- Fin entrada de datos

Sub GR.DISENO.VOUCHER																												! Diseño Impresion Voucher
String XL$																																	!
     Dim Gr.Tef2.Voucher$(6,50)                                    			    ! Maximo 5 autorizadores 50 lineas de mensaje
     TS.ER.RETURN = -1 																											!
     OPEN "R::$EFVOUCH" AS 94	NoWrite NoDel																	! Apertura archivo formato mensajes
     If TS.ER.RETURN <> -1 Then BEGIN                                       ! Error apertura
  	    Call VISOR.AND.BORRAR("ERROR ARCHIVO DISENO VOUCHER  ")             ! Msg Alerta
  	    Call U.IMPRIME("ERROR APERTURA ARCHIVO VOUCHER",2100H)              ! 
    	  Gr.Tef2.Ok% = 0 																									  ! Inactiva proyecto
  	    Exit Sub 																														!
     ENDIF 																																	!
     K% = 0																																	!
     IF END #94 THEN UE.TEF.MSGVOUCHER       												        ! Si es EOF
     While (1)																												      !
      Read #94; A$
      If Left$(A$,1) = "<" Then Begin																				! Carga los mensajes 
	       I% = Match(">",A$,1)																				  			! que van a utilizar 
	       J% = Val(MID$(A$,2,I%-2))																					! c/u de los operadores
	       K% = 1																															! bancarios
	    Endif Else Begin																											!
	     If J% < 6 Then                                                      \! Maximo 5 autorizadores
        If K% <= 50 Then Begin																						  ! Maximo 50 lineas de mensaje
           Gr.Tef2.Voucher$ (J%,K%) = A$																    ! Almacena linea de mensaje
           Gr.Tef2.Voucher$ (J%,0)  = STR$(K%)											 	      ! Numero de lineas total a imprimir
        EndIf																															  !
       K% = K% + 1																									  		  !
      EndIf
     Wend																																		!         
     UE.TEF.MSGVOUCHER:																											!
       Close 94																															!
End Sub 																																		! Fin carga diseno voucher

Sub GR.PARAMETROS.TEF																										    ! Parametros modulo TEF
Integer*4 Xpos%																															!
String    Xparm$																														!
    Xpos% = 1 																															! Init posicion
    Dim Gr.Tef2.Msg$(20)																										! Msg del aplicativo
    TS.TS11WERR$ = ""																												! Control errores
    Open "R::$EFLNMSG" As 94 NoWrite NoDel        													! Apertura parametros
    If TS.TS11WERR$ <> "" Then BEGIN 																				! Error apertura archivo
    	 Call VISOR.AND.BORRAR("ERROR APERTURA TEFPARAM")											! Msg alerta
    	 Call U.IMPRIME("ERROR ARCHIVO PARAMETROS TEF II",2100H)              ! 
    	 Gr.Tef2.Ok% = 0 																											! Init variables
    	 Exit Sub 																														! Sale de la rutina
    EndIf																																		! fin Error apertura
    If End #94 Then UE.FIN.PARAMTEF2     																	  ! Si es EOF                        
    While (1)														  																  ! Recorre archivo
       Read # 94 ; Xparm$																										! Lectura mensaje
       Gr.Tef2.Msg$(Xpos%) = Xparm$																					! Asigna mensaje
       Xpos% = Xpos% + 1 																										! Aumenta apuntador
    Wend 																																		! Fin carga de parametros
    UE.FIN.PARAMTEF2:																												! 
      Close 94																															! Cierra archivo
End Sub 																																		! Fin carga parametros tef

Sub BUSCA.MOV.TEF            																						    ! Busca Voucher para anulacion
Integer*4 Xp%																																!
  TS.TEMP5I4 = 0																														! Control busqueda
  For Xp% = 1 To Gr.Tef2.Ttrx%																							! Analiza vouchers capturados
    If Gr.Tef2.RtaTef$(Xp%,5) = Gr.Tef2.NumRcv$ Then Begin								  ! Localiza numero de recibo
    	 Gr.Tef2.SeqTrx$ = Gr.Tef2.RtaTef$(Xp%, 5)														! Numero de recibo
    	 Gr.Tef2.NumAut$ = Gr.Tef2.RtaTef$(Xp%, 9)														! Numero de autorizacion 
       Gr.Tef2.tmp4$ = Gr.Tef2.RtaTef$(Xp%,22)															! Valor Base Impto
       Gr.Tef2.tmp5$ = Gr.Tef2.RtaTef$(Xp%,23)															! Valor Impuesto
       Gr.Tef2.IdCte$ = Gr.Tef2.RtaTef$(Xp%,3)															! id de la red
       Gr.Tef2.Tpdif$	= Gr.Tef2.RtaTef$(Xp%,36)		 												  ! Tipo diferido con interes o Sin interes
       Gr.Tef2.IdDif$ = Gr.Tef2.RtaTef$(Xp%,33)		 												  ! Id del diferido
       Gr.Tef2.Cuota$ = Gr.Tef2.RtaTef$(Xp%,33)		 												  ! Nro de cuotas
       Gr.Tef2.Bin$   = Gr.Tef2.RtaTef$(Xp%,16)                             ! Nro Tarjeta habiente
       Gr.Tef2.Idtrxdif$ = Gr.Tef2.IdCte$                                   ! Id del diferido
    	 TS.TEMP5I4 = -1																											! Documento encontrado
    EndIf																																		!
  Next Xp%																																	!
End Sub 


!-- Invocacion servicios de tef
!-- Xvlr$, Valor de la transaccion
!-- Xtrx$, Tipo de transaccion a procesar
!--        51. Compra
!--        53. Anulaciones
!-- Xfnto$ Tipo de financiamiento
!--        00. Corriente
!--        01. Diferido
Sub PROCESA.COMPRA(Xvlr$,Xtrx$,Xfnto$)																							! Procesa compra 
String Xvlr$, Xtrx$, Xbuffer$, Xoper$, Xfnto$, Xbase$, Xnotax$, Xiva$
String Xenvia$, Xsale$, Xrtrama$, Xrta$, Xllega$, Xtax$, Xmtrama$, Xfunc$
String Xidred$, XCoddif$, XPlazo$, XTipodif$, Xtid$, Xmid$, Xmeses$
Integer*4 Tax.Epay%, Tax.Base%, Xx%
Integer*1 Gr.Proceso%
Real      Xra%, Xrb%, Xrx%

  Gr.Tef.TrxSale% = 0																																! Control Proceso
  If TS.PROCEDURE = -1 And Xtrx$ <> "53" Then                                      \! Proceso de venta
   If Val(Xvlr$) > ( TS.BALDUE(0) ) Then Begin                              				! Si pago supera total compra
  	 Call VISOR.AND.BORRAR(Gr.Tef2.Msg$(2))						                      				! Msg alerta
     Exit Sub 																														  				!
   EndIf																																						!

  If Xtrx$ <> "53" Then Begin 																											! No es una anulacion
     Gr.Proceso% = GR.LECTURA.TARJETA																								! Lectura inicial tarjeta 
     If Gr.Proceso% <> -1 Then Exit Sub 																						! si falla validacion tarjeta
     If Gr.PromOr.CtlPago% = 0 Then Begin 																					! Valida promo al motor
     	  Gr.Proceso% = 0																															!
     	  TS.TEMP4$ = Left$(Gr.Tef2.Bin$,6)																						! Reporta bin tarjeta
 	      Gr.Proceso% = CALCULO.DSCTO.PAGO(TS.TEMP4$)                                 ! Promocion para bin 
 	      Gr.Tef.TrxSale% = 2																													! Control Proceso
 	      If Gr.Proceso% = -1 Then Exit Sub 																					! Aplica Dscto recalcula
     EndIf	
     If Gr.PromOr.CtlPago% = -2 Then Begin 																					! Si dscto aplicado
     	  If Gr.PromOr.AplNew% = -1 Then Begin 																				! Si en espera de pago tarjeta
     	  	 TS.TEMP4$ = Left$(Gr.Tef2.Bin$,6)																				! Reporta bin tarjeta
     	  	 If TS.TEMP4$ <> Gr.PromOr.Bin$ Then Begin																! Si no es bin del descto
     	  	 	  Call Visor.And.Borrar("PASAR TARJETA CON LA QUE APLICO DESCUENTO")    !
     	  	 	  Gr.Proceso% = 0
     	  	 	  Exit Sub 
     	  	 EndIf
     	  EndIf
     EndIf	
     If Xfnto$ = "01" Then Begin
  	    Gr.Proceso% = Gr.Diferidos.TefII(Val(Gr.Tef2.Idtrxdif$), Xtrx$)							! Captura diferidos
  	    If Gr.Proceso% <> -1 Then Exit Sub 																					! si falla validacion tarjeta
     EndIf 
  Endif
  
  Call VISORES4690(1,"PROCESANDO COMPRA","PINPAD,   ESPERE",0,"L")
  If Xfnto$ = "00" Then Xoper$ = "01"																								! Operacion de compra cte
  If Xfnto$ = "01" Then Xoper$ = "02"																								! Operacion de diferido
  	
  If Xtrx$ = "53" Then Begin        																								! Operacion Anulacion
  	 Xoper$ = "03"
  	 If Gr.Tef2.AnOff% = 0 Then Begin 																							! No es anul voucher fuera trx
  	    Call BUSCA.MOV.TEF                                                   				! Busca Movimiento TEF
  	    If TS.TEMP5I4 <> -1 Then Begin 																							! Recibo no encontrado
			     Call VISOR.AND.BORRAR(Gr.Tef2.Msg$(1))                           				! Msg de Error	
  	       Gr.Tef.TrxSale% = 00																											! Control Proceso Fallido			  
			     Exit Sub 																																! Sale del proceso
  	    EndIf
  	 EndIf
  	Gr.Tef2.Sequen$ = Gr.Tef2.NumRcv$																								! Numero recibo 
  EndIf Else Begin
  	Gr.Tef2.Sequen$ = "      "																											! Numero de secuencial
  	Gr.Tef2.NumAut$ = "      "																							        ! Numero de autorizacion
  EndIf
  
  If Gr.Tef2.AnOff% = 0 Then Begin 																					        ! No es anul voucher fuera trx
   If Abs(VAL(Xvlr$)) <= Abs(TS.BALDUE(0)) Then Begin 					 					          ! Calculo del IVA Parcial
   	
!   	 Write #34 ; "ENTRE IVAS POR 1 " + CHR$(10)
!     TAX.EPAY% = Round(FLOAT(UE.TOTALS(1)),0,0) - \
!                 Round(FLOAT(UE.TOTALS(1))/(1.+FLOAT(UE.TABLAIVA(1,0))/100.),0,0)		! Calculo IVAS Ecuador 6 Mzo 2007		

     If Gr.PromOr.Total% <= Ue.Totals(1) Then Begin
     	  Xrx% = float(Ue.Totals(1)) - float(Gr.PromOr.Total%)                        ! Retiro el dscto
     EndIf Else Xrx% = float(Ue.Totals(1))
     	
!     TAX.BASE% = Round(FLOAT(UE.TOTALS(1))/(1.+FLOAT(UE.TABLAIVA(1,0))/100.),0,0)		! Calculo Base Compra total      		
!     TAX.EPAY% = Round(float(UE.TOTALS(1)) - TAX.BASE%,0,0)     

     TAX.BASE% = Round(Xrx% / (1.+FLOAT(UE.TABLAIVA(1,0))/100.),0,0)								! Calculo Base Compra total      		
     TAX.EPAY% = Round(Xrx% - TAX.BASE%,0,0)     																		! Valor del IVA

     xra% = Float(TAX.EPAY%) * (FLOAT(Val(Xvlr$)) / Float(TS.TOTALS(0,0,0)))        ! Iva  proporcional                  
     xrb% = Float(TAX.BASE%) * (FLOAT(Val(Xvlr$)) / Float(TS.TOTALS(0,0,0)))        ! Base proporcional                  
     If ( (xra% - Int(xra%)) = 0.5 And (xrb% - Int(xrb%))  = 0.5 ) Then Begin       ! Si redondeo exacto ambos           
        	 TAX.EPAY% = Int(xra%)                                                    ! Toma parte entera del IVA          
        	 TAX.BASE% = Round(xrb%,0,0)																							! Redondea la Base                   
     EndIf Else Begin 																															!                                	  
        	 TAX.EPAY% = xra%																													! Retorna totales de             		
        	 TAX.BASE% = xrb%																													! Base e impuesto                		
     EndIf                                                                                                           
!     Write #34 ; "Compra "+Xvlr$ + CHR$(10)
!     Write #34 ; "Base   "+Str$(TAX.BASE%) + CHR$(10)
!     Write #34 ; "IVA    "+Str$(TAX.EPAY%) + CHR$(10)
!     Write #34 ; "Dcto   "+Str$(Gr.PromOr.Total%) + Chr$(10)
     
   EndIf Else Begin																													        ! Es compra total con tarjeta        
!   	    Write #34 ; "ENTRE IVAS POR 2 " + CHR$(10)
!        TAX.EPAY% = Round(FLOAT(UE.TOTALS(1)),0,0) - \                                                               
!                    Round(FLOAT(UE.TOTALS(1))/(1.+FLOAT(UE.TABLAIVA(1,0))/100.),0,0) 	! Calculo IVAS Ecuador           		
                    
        TAX.BASE% = Round(FLOAT(UE.TOTALS(1))/(1.+FLOAT(UE.TABLAIVA(1,0))/100.),0,0) 	! Calculo IVAS Ecuador           		
        TAX.EPAY% = Round(float(UE.TOTALS(1)) - TAX.BASE%,0,0)     
!        Write #34 ; "Compra "+Str$(UE.TOTALS(1)) + CHR$(10)
!        Write #34 ; "Base   "+Str$(TAX.BASE%) + CHR$(10)
!        Write #34 ; "IVA    "+Str$(TAX.EPAY%) + CHR$(10)
!        Write #34 ; "dscto  "+Str$(TS.TOTALS(0,0,1)) + chr$(10)
        
   EndIf
   If Xtrx$ = "53" Then Begin
  	  Tax.Epay% = Val(Gr.Tef2.Tmp5$)
  	  Tax.base% = Val(Gr.Tef2.Tmp4$) 
   EndIf
  EndIf Else Begin																													! En proceso de anulacion
  	Tax.Epay% = Val(Gr.Tef2.Tmp5$)																					! Valor Impuesto
  	Tax.base% = Val(Gr.Tef2.Tmp4$) 																					! Valor Base impuesto
  	Xvlr$     =     Gr.Tef2.Tmp2$  																					! Valor de la compra
  EndIf
  Xnotax$ = Str$ ( Val(Xvlr$) - (Tax.Epay% + Tax.base%) )                   ! Monto compra Sin tax
  Xvlr$   = Right$("            "+Str$(Val(Xvlr$)),12)                      ! Valor de la transaccion  
  Xbase$  =	Right$("            "+Str$(Tax.Base%),12)                       ! Valor Base compra
  Xnotax$ =	Right$("            "+Xnotax$,12)                               ! Valor Base compra Sin impto
  Xtax$   =	Right$("            "+Str$(Tax.Epay%),12)                       ! Valor del impto
  
  If Xoper$ = "01" Then Begin																								! Es una trx corriente
     Xidred$   = Str$(Val(Gr.Tef2.IdCte$))
     XCoddif$  = "00"
     Xplazo$   = "  "
     XTipodif$ = " "
     Xmeses$   = "  "
     Gr.Tef2.Cuota$ = Xplazo$											        									! Retorna numero de cuotas
     Gr.Tef2.IdDif$ = XCoddif$                     						              ! Codigo diferido capturado
     Gr.Tef2.Ndife$ = "                    "										            ! Nombre del diferido
     Gr.Tef2.Tpdif$ = XTipodif$                                             ! Tipo de diferido con o Sin intereses    
  EndIf 
  If (Xoper$ = "02") Then Begin 																						! Es una trx diferido
     Xidred$   = Str$(Val(Gr.Tef2.Idtrxdif$))															  ! ID de la red
     XCoddif$  = Gr.Tef2.IdDif$																							! Codigo del diferido
     Xplazo$   = Gr.Tef2.Cuota$																							! Numero de cuotas
     XTipodif$ = Gr.Tef2.Tpdif$																							! Tipo diferido con interes o Sin interes
     XTipodif$ = " "																												! Nueva version 31Ago2017
     Xmeses$   = "00"																												! Meses de gracia
  EndIf
  If (Xoper$ = "03") Then Begin 																						! Es una anulcion de trx
     Xidred$   = Str$(Val(Gr.Tef2.Idtrxdif$))															  ! ID de la red
     If Xfnto$ = "01" Then Begin																						! Es una operacion diferido
        XCoddif$  = Gr.Tef2.IdDif$																					! Codigo del diferido
        Xplazo$   = Gr.Tef2.Cuota$																					! Numero de cuotas
        XTipodif$ = Gr.Tef2.Tpdif$																					! Tipo diferido con interes o Sin interes
        XTipodif$ = " "																											! Nueva version 31Ago2017
        Xmeses$   = "00"																										! Meses de gracia
     EndIf
     If Xfnto$ = "00" Then Begin																						! Es una operacion corriente
        XCoddif$  = "00"
        Xplazo$   = "  "
        XTipodif$ = " "
        Xmeses$   = "  "
     EndIf
  EndIf

  If Xidred$ = "1" Then Begin 																							! Operacion datafast
     Xtid$ = Gr.Tef2.TidData$
     Xmid$ = Gr.Tef2.MidData$
  EndIf Else Begin																													! Operacion medianet
     Xtid$ = Gr.Tef2.TidMnet$
     Xmid$ = Gr.Tef2.MidMnet$
  EndIf																																			! Fin operacion 


 !Call VISOR.AND.BORRAR("RETIRE TARJ PINPAD  PARA CONTINUAR /Borr")
  
  TS.TEMP1$ = "1"     +                                                    \! Nro de tramas          Asc 1
              Xoper$	+																									   \! Tipo de operacion      Asc 2
              Xidred$ +                                                    \! ID de la red           Asc 1
              XCoddif$ +                                                   \! Codigo del diferido    Asc 2
              XPlazo$  +                                                   \! Plazo del diferido     Asc 2
              Xmeses$  +                                                   \! Meses de gracias       Asc 2
              XTipodif$ +                                                  \! Tipo diferido          Asc 1
              Xvlr$	  +																									   \! Valor de la trx        Asc 12
              Xbase$  +                   																 \! Monto Base con Tax     Asc 12
              Xnotax$ +																									   \! Monto Base Sin Tax     Asc 12
              Xtax$   +   																								 \! Monto del Tax          Asc 12
              "            "    +                                          \! Impto Servicio         Asc 12
              "            "    +                                          \! Propina                Asc 12
              "            "    +                                          \! Monto Fijo             Asc 12
              Gr.Tef2.Sequen$   +                                          \! Numero de secuencia    Asc 05
              Gr.Tef2.NumAut$   +                                          \! Numero autorizacion    Asc 06
              Xmid$             +                                          \! Mid Operador           Asc 15
              Xtid$                                                         ! Tid Operador           Asc 8


  TS.TEMP2$ = Armar.Trama.Msg2("43","78",TS.TEMP1$,"00","0001","123456") 	  ! Armar trama MSG                	
  Xenvia$   = TS.TEMP2$																											!
  Xsale$    = Time$																													! Hora salida requerimiento
  XRtrama$  = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$) 	    ! Ejecuta Requerimiento
  Xrta$     = Time$																													! Hora llegada requerimiento
  Xllega$   = XRtrama$																											!
  Call TEFII.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                      ! Rastro auditoria
  TS.TEMP1$ = Valida.Rta(XRtrama$)																					! Valida rta entregada
  If TS.TEMP1$ = "00"  Then Begin			 																			! Proceso satisfactorio
	   TS.TEMP1$ = MID$(XRtrama$,12,2)                                        ! Toma rta modulo java
		 If TS.TEMP1$ <> "00" Then Begin								   									   	! Si presenta un Error
			  Call VISOR.AND.BORRAR(Mid$(XRtrama$,18,40))                         ! Msg de Error	
  	    Gr.Tef.TrxSale% = 00																								! Control Proceso Fallido			  
			  Exit Sub 																														! Sale del proceso
     EndIf																																	! Proceso satistactorio
	   TS.TEMP1$ = MID$(XRtrama$,14,2)                                        ! Toma rta del pinpad
		 If TS.TEMP1$ <> "00" Then Begin								   									   	! Si presenta un Error
        Call VISOR.AND.BORRAR(Mid$(XRtrama$,18,40))                         ! Msg de Error	
  	    Gr.Tef.TrxSale% = 00							  																! Control Proceso Fallido			  
			  Exit Sub 																														! Sale del proceso
     EndIf

     Call TSHIECET																													! Tono de alerta
     Xrta$ = Left$(Mid$(XRtrama$,18,40),20)																	! Presenta Msg Rta
     Xsale$ = Mid$(Mid$(XRtrama$,18,40),21,20)															!
     Gr.Tef2.Ttrx% = Gr.Tef2.Ttrx% + 1 																			! Total de trx tef 
     Call VISORES4690(1,Xrta$,Xsale$,1000,"L")															!
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 1) = Mid$(XRtrama$, 12,  2)							! Rta Trx TEF                 A
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 2) = Mid$(XRtrama$, 14,  2)							! codigo rta ejecucion pinpad	B
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 3) = Mid$(XRtrama$, 16,  2)							! codigo identif red					C
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 4) = Mid$(XRtrama$, 18, 40)							! Msg rta pin pad							D
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 5) = Mid$(XRtrama$, 58,  6)							! Secuencial trx							E
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 6) = Mid$(XRtrama$, 64,  6)							! Nro Lote										F
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 7) = Mid$(XRtrama$, 70,  6)							! Hora Trx										G
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 8) = Mid$(XRtrama$, 76,  8)							! Fecha Trx										H
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 9) = Mid$(XRtrama$, 84,  6)							! Nro Autorizacion						I
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,10) = Mid$(XRtrama$, 90,  8)							! Terminal ID     						J
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,11) = Mid$(XRtrama$, 98, 15)							! Merchat ID                  K
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,12) = Mid$(XRtrama$,113, 12)							! Valor de los interes     		L
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,13) = Mid$(XRtrama$,125,  3)							! Codigo banco adquiriente    M
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,14) = Mid$(XRtrama$,128, 30)							! Nombre banco adquiriente    N
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,15) = Mid$(XRtrama$,158, 25)							! Nombre Franquicia					  O
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,16) = Gr.Tef2.Bin$                       ! Nro Tarjeta	habiente 				P
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,17) = Mid$(XRtrama$,183,  2)							! Modo de lectura 						Q

     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,18) = Mid$(XRtrama$,185, 40)							! Nombre tarjeta habiente     R
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,19) = Mid$(XRtrama$,225, 20)							! ID APPL EMV solo chip       S

     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,21) = Xvlr$                 							! Valor de la compra    			1
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,22) = Xbase$                 						! Valor Base compra    				2
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,23) = Xtax$                   						! Valor Impuestos       			3
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,24) = Xnotax$                   					! Valor Excento Impuestos		  4
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,25) = Xoper$                   					! Tipo de Operacion           5
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,31) = XMID$                    					! Mid Operador bancario       6 
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,32) = XTID$                    					! Tid Operador Bancario       7
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,33) = Gr.Tef2.IdDif$											! Tipo Diferido								8
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,34) = Gr.Tef2.Cuota$											! Plazo diferido							9
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,35) = Gr.Tef2.Ndife$											! Descripcion diferido				0
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,36) = Gr.Tef2.Tpdif$											! Con o Sin intereses 				
     
     TS.TEMP2$ = Armar.Trama.Msg2("43","79","2","00","0001","123456")       ! Armar trama MSG
     Xenvia$   = TS.TEMP2$																									!
     Xsale$    = Time$																											! Hora salida requerimiento
     XRtrama$  = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$) 	! Ejecuta Requerimiento
     Xrta$     = Time$																											! Hora llegada requerimiento
     Xllega$   = XRtrama$																										!
     Call TEFII.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                   ! Rastro auditoria
     TS.TEMP1$ = Valida.Rta(XRtrama$)																				! Valida rta entregada
     !If TS.TEMP1$ = "00"  Then Begin			 																	! Proceso satisfactorio
     	  Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,20) = Mid$(XRtrama$, 12, 20)					! AID-EMV         						T
        Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,26) = "00"                     				! Rta trama 2     						U
        Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,27) = Mid$(XRtrama$, 32, 22)					! Valor EMV solo chip         V
        Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,28) = Mid$(XRtrama$, 69, 16)					! ARQC solo chip  						W
        Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,29) = Mid$(XRtrama$, 85, 80)					! Mensaje promocion 80	    	X
        Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,30) = Mid$(XRtrama$, 54, 15)					! Validacion del PIN  	    	Y
     !EndIf
     Gr.Tef2.NumRcv$ = Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 5)										! Numero de recibo
     Gr.Tef2.NumAut$ = Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 9)										! Numero de autorizacion
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,12) = Right$("000000000000"+            \!
                                       Str$(Val(Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,12))),12)
     If Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 9) = "000000" Then Begin             ! Error nro autorizacion
     	 Gr.Tef2.Ttrx% = Gr.Tef2.Ttrx% - 1 																		! Ajusta Total de trx tef 
       Gr.Tef.TrxSale% = 0  																								! Control Proceso 
       Exit Sub 																														! Sale del proceso    	
     EndIf Else Begin                                                       ! Proceso OK
       Gr.Tef.TrxSale% = -1																									! Control Proceso
     EndIf  
  EndIf	Else Begin																													! Fin proceso satisfactorio
		 Call VISOR.AND.BORRAR(Mid$(XRtrama$,14,40))                            ! Msg de Error
  	 Gr.Tef.TrxSale% = 00																										! Control Proceso Fallido
  EndIf
  
End Sub 

Function Voucher.String(Ypos%,X.A$)                													! Coloca datos por parametro
String Voucher.String, X.A$, X.B$, X.C$, X.D$, XM$
Integer*4 X.A%, X.B%, Ypos%, YTeI%
Integer*1 Xpaso%, Xsgn%
Xpaso% = 0

PASO.RTA.TRAMA:
X.A% = MATCH("&",X.A$,1)						            														! Busca dato a imprimir
If X.A% > 0 Then Begin                          														! Si lo encotro
   X.B% = LEN(X.A$)                             														! Arma mensaje con el dato 
   X.B$ = Left$(X.A$,X.A%-1)                    														! solicitado, entregando 
   X.C$ = Mid$(X.A$,X.A%,2)                     														! 38 caracteres
   X.D$ = Mid$(X.A$,(X.A%+2),X.B%)

   If UCASE$(X.C$) = "&Z" Then Begin            														! Fecha y hora de la trx
      X.C$ = Left$(Gr.Tef2.RtaTef$(Ypos%,08),4)  + "/" + 									 \! Fecha de la trx AAAA
             Mid$(Gr.Tef2.RtaTef$(Ypos%,08),5,2) + "/" + 									 \! Fecha de la trx MM
             Right$(Gr.Tef2.RtaTef$(Ypos%,08),2)            					      ! Fecha de la trx DD
      X.C$ = X.C$ + "  " + Left$(Gr.Tef2.RtaTef$(Ypos%,07),2)  + ":" + 		 \! Hora de la trx HH
             Mid$(Gr.Tef2.RtaTef$(Ypos%,07),3,2) + ":" + 									 \! Hora de la trx MM
             Right$(Gr.Tef2.RtaTef$(Ypos%,07),2)            						    ! Hora de la trx SS              
   EndIf

   If UCASE$(X.C$) = "&A" Then Begin            														! Rta Java trx
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,1)
   EndIf

   If UCASE$(X.C$) = "&B" Then Begin            														! Rta Pinpad
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,2)
   EndIf

   If UCASE$(X.C$) = "&C" Then Begin            														! Codigo de la red
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,3)
   EndIf
   If UCASE$(X.C$) = "&D" Then Begin            														! Msg Rta PinPdad
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,4)
   EndIf
   If UCASE$(X.C$) = "&E" Then Begin            														! Nro de secuencia
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,5)
   EndIf
   If UCASE$(X.C$) = "&F" Then Begin            														! Numero de lote 
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,6)
   EndIf
   If UCASE$(X.C$) = "&G" Then Begin            														! Hora trx hhmmss
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,7)
   EndIf
   If UCASE$(X.C$) = "&H" Then Begin            														! Fecha trx aaaammdd
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,8)
   EndIf
   If UCASE$(X.C$) = "&I" Then Begin            														! Nro autorizacion
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,9)
   EndIf
   If UCASE$(X.C$) = "&J" Then Begin            														! terminal id
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,10)
   EndIf
   If UCASE$(X.C$) = "&K" Then Begin            														! Merchat ID
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,11)
   EndIf
   If UCASE$(X.C$) = "&M" Then Begin            														! Cod Banco Adquiriente
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,13)
   EndIf
   If UCASE$(X.C$) = "&N" Then Begin            														! Nombre banco adquiriente
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,14)
   EndIf
   If UCASE$(X.C$) = "&O" Then Begin            														! Nombre franquicia
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,15)
   EndIf
   If UCASE$(X.C$) = "&P" Then Begin            														! Nro Tarjetra habiente
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,16)
   EndIf
   If UCASE$(X.C$) = "&Q" Then Begin            														! Modo de lectura     
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,17)
   EndIf
   If UCASE$(X.C$) = "&R" Then Begin            														! Nombre tarjeta habiente
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,18)
   EndIf
   If UCASE$(X.C$) = "&S" Then Begin            														! ID APPL EMV (chip)  
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,19)
   EndIf
   If UCASE$(X.C$) = "&T" Then Begin            														! AID-EMV  (Chip)     
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,20)
   EndIf
   If UCASE$(X.C$) = "&U" Then Begin            														! Rta trama 2 (00)     
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,26)
   EndIf
   If UCASE$(X.C$) = "&V" Then Begin            														! Valor EMV (CHIP)
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,27)
   EndIf
   If UCASE$(X.C$) = "&W" Then Begin            														! ARQC                
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,28)
   EndIf

   Xsgn% = 1

   If Gr.Tef2.RtaTef$(Ypos%,25) = "03" Then Xsgn% = -1
   	
   If UCASE$(X.C$) = "&1" Then Begin            														! Valor de la compra
      Call Format.Amount( Val(Gr.Tef2.RtaTef$(Ypos%,21)) * Xsgn%)						! Formatea dato
      TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)      										! Ajusta dato
      X.C$ = TS.TEMP1$
   EndIf
   If UCASE$(X.C$) = "&2" Then Begin            														! Valor Base Compra
      Call Format.Amount( Val(Gr.Tef2.RtaTef$(Ypos%,22)) * Xsgn%)						! Formatea dato
      TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)      										! Ajusta dato
      X.C$ = TS.TEMP1$
   EndIf
   If UCASE$(X.C$) = "&3" Then Begin            														! Valor de los impuestos
      Call Format.Amount( Val(Gr.Tef2.RtaTef$(Ypos%,23)) * Xsgn%)						! Formatea dato
      TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)      										! Ajusta dato
      X.C$ = TS.TEMP1$
   EndIf
   If UCASE$(X.C$) = "&4" Then Begin            														! Valor Excento Impuestos
      Call Format.Amount( Val(Gr.Tef2.RtaTef$(Ypos%,24)) * Xsgn%)						! Formatea dato
      TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)      										! Ajusta dato
      X.C$ = TS.TEMP1$
   EndIf
   If UCASE$(X.C$) = "&L" Then Begin            														! Valor intereses
      Call Format.Amount( Val(Gr.Tef2.RtaTef$(Ypos%,12)) * Xsgn%)						! Formatea dato
      TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)      										! Ajusta dato
      X.C$ = TS.TEMP1$
   EndIf

   If UCASE$(X.C$) = "&6" Then Begin            														! Mid Operador
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,31)
   EndIf
   If UCASE$(X.C$) = "&7" Then Begin            														! Tid Operador   
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,32)
   EndIf

   If UCASE$(X.C$) = "&8" Then Begin            														! Tipo diferido
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,33)
   EndIf
   If UCASE$(X.C$) = "&9" Then Begin            														! Plazo diferido - Cuotas
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,34)
   EndIf
   If UCASE$(X.C$) = "&0" Then Begin            														! Descripcion diferido
      X.C$ = Gr.Tef2.RtaTef$(Ypos%,35)
   EndIf

   If UCASE$(X.C$) = "&X" Then Begin            														! Linea Mensaje 1
      X.C$ = Mid$(Gr.Tef2.RtaTef$(Ypos%,29),1,40)
   EndIf
   If UCASE$(X.C$) = "&Y" Then Begin            														! Linea Mensaje 2
      X.C$ = Mid$(Gr.Tef2.RtaTef$(Ypos%,29),41,40)
   EndIf

   If UCASE$(X.C$) = "&*" Then Begin            														! Valor de la compra
   	  YTeI% = (Val(Gr.Tef2.RtaTef$(Ypos%,21)) * Xsgn%)		+   				     \! Compra
              (Val(Gr.Tef2.RtaTef$(Ypos%,12)) * Xsgn%)						          ! Intereses
      Call Format.Amount(Ytei%)																							! Formatea dato
      TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)      										! Ajusta dato
      X.C$ = TS.TEMP1$
   EndIf

   X.B$ = X.B$ + X.C$ + X.D$
   X.B$ = Left$(X.B$+String$(37," "),37)
   X.A$ = X.B$
   If Xpaso% = 0 Then Begin
   	  Xpaso% = 1 
   	  GoTo PASO.RTA.TRAMA																										! Analiza nuevamente la trama
   EndIf
EndIf    
Voucher.String = X.A$

End Function
!--- Fin funcion data String


Sub IMPRESION.VOUCHER.TEF2																									! Impresion Bono Recompra
Integer*4 Xl%, Xfin%, Xj%, Xpos%, XM%
String    Xbuff$

  If Gr.Fiscal.Tktnf% = -1 Then Begin																					! Si eliminacion tiquete fiscal
     Gr.RsmTkt.Ind% = -1																											! Imprime tiquete fiscal
     EP.ETO.PRINT.POST.TRANS = 0																							! 
  EndIf

  
  If Gr.Tef2.Ttrx% > 0 Then Begin					     														  ! Hay Vouchers para impresion
   For XM% = 1 To Gr.Tef2.Ttrx%																							! Total Voucher para impresion
  	 Xpos% = Val(Gr.Tef2.RtaTef$(XM%,3))																		! autorizador bancario
  	 If Gr.Tef2.RtaTef$(XM%, 9) = "000000" Then GoTo Proximo.Voucher	      ! Proceso invalido de autorizacion
  	 If Xpos% = 2 Then Xpos% = 3																					  ! Toma autorizador Medianet
  	 If Gr.Tef2.RtaTef$(Xm%,25) = "03" Then Xpos% = Xpos% + 1               ! Segundo diseño voucher para anulacion
  	 Xl% = 0 : Xfin% = Val(Gr.Tef2.Voucher$(XPOS%,0)) 											! Init Variables
     For Xj% = 1 To (Gr.Tef2.Ncopia% + 1)																		!
      For XL% = 1 To Xfin%																									! Recorre diseño del voucher
         Xbuff$ = Voucher.String(Xm%,Gr.Tef2.Voucher$(XPOS%,Xl%))           ! Analiza String
         If Xbuff$ <> "" Then Begin																			    ! Hay Info para imprimir
            Call U.Imprime(Xbuff$,4100H)	                                  ! Impresion voucher
         EndIf																															!
      Next Xl%																															! Fin impresion
      If Xj% = 1 Then Begin 																								! Si Voucher para cliente
      	TS.TEMP1$ = "         -- ORIGINAL --              "									!
      	Call U.IMPRIME(TS.TEMP1$,4101H)                                     !
      EndIf Else Begin																											! Si copias para tarjeta habiente
      	TS.TEMP1$ = "         --  COPIA   --              "									!
      	Call U.IMPRIME(TS.TEMP1$,4101H)                                     !
      EndIf																																	!
      TS.TEMP1$ = Linea.Detalle(38)  																				! Trailer del movimiento
      Call U.IMPRIME(TS.TEMP1$,4200H)                                       !
      TS.TEMP1$ = String$(37,"-")																						! Linea cierre voucher
      Call U.IMPRIME(TS.TEMP1$,4900H)																			  ! avance de papel
      Call U.CORTACR																												! Corta Papel
     Next Xj%																																! Fin numero copias
     TS.LINETYPE = 18																											  ! Genera cabecera 
     TS.LINEDATA = 1																											  ! en proximo tiquete
     Call TSPREC01																												  !
     Proximo.Voucher:
      
   Next XM%
  EndIf																																			! Fin impresion del bono
If Gr.Fiscal.Tktnf% = -1 Then Begin																					! Si eliminacion tiquete fiscal
   Gr.RsmTkt.Ind% = 0 																											! Imprime tiquete fiscal
   EP.ETO.PRINT.POST.TRANS = -1																						  ! 
EndIf

End Sub 																																		! Fin Impresion Bono Recompra

Sub PRUEBA.CAMBIO.VC
String Xdoc$, Xrtrama$, Xmtrama$
    Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10)             									  ! Init vectores de carga
    TS.IO.DATA$(5) = "2"											           										! Secuencia cambio a efectivo
    TS.IO.KEYS(5)  = 61											             										!
    TS.IO.MOTORKEY = 61											             										!
    CALL TSTEEC01                                  	     										! Ejecuta Procedimiento
   	TS.TEMP1$ = Left$(Gr.Tef2.Msg$(5),20)							     									! Captura mensaje
   	TS.TEMP2$ = Right$(Gr.Tef2.Msg$(5),20)							   									!
    TS.TEMP2$ = ASIC.DATOS$(TS.TEMP1$,TS.TEMP2$)				   							    ! Mensaje captura dato
    If TS.TEMP2$ = "E" Then BEGIN 											   									! Proceso cancelado
       Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO") 	 									! Mensaje alerta
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)				     									! Inicializa secuencias 
       TS.IO.MOTORKEY = 73															   									! de tecleo 
       Exit Sub 																				   									! Sale de la rutina
    EndIf 																							   									! Fin proceso cancelado
   TS.TS11WERR$ = ""																												! Control errores
   Open "R::$F2MVTRX" KEYED RECL 435 As 94                 									! Abre copias voucher
   If TS.TS11WERR$ <> "" Then BEGIN 																				! Error de apertura
 	    Call VISOR.AND.BORRAR(Gr.Tef2.Msg$(7))																! Mensaje alerta
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencias 
      TS.IO.MOTORKEY = 73																										! de tecleo 
      Exit Sub 																															! Sale de la rutina
   EndIf 																																		!
   TS.TEMP2$ = Right$("000000"+TS.TEMP2$,6)																	! Arma llave de busqueda
   Xdoc$ = TS.TEMP2$
   TS.ER.RETURN = -1																												! Control errores
   Read Form "C6 C198 C231" ;#94 KEY TS.TEMP2$;                            \! Lee Reg Cabecera 
        Gr.Tef2.tmp3$, 																					           \! Llave del registro
        Xrtrama$,     																						         \! Primer String
        Xmtrama$                                                            ! Segundo String
  If TS.ER.RETURN <> -1 Then Begin 																			    ! Voucher no localizado
 	    Call VISOR.AND.BORRAR(Gr.Tef2.Msg$(10))																! Mensaje alerta
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencias 
      TS.IO.MOTORKEY = 73																										! de tecleo 
      Close 94																															! Cierra archivo
      Call Gr.Init.Trx																											! Anula procedimiento
  EndIf Else Begin																													! Si lo encontro
      Gr.Tef2.NumRcv$ = Right$("000000"+TS.TEMP2$,6)                        ! Numero de recibo anular
      Gr.Tef2.NumAut$ = Mid$(XRtrama$, 73,  6)                  						! Ajusta dato
      Gr.Tef2.NumAut$ = Right$("000000"+Gr.Tef2.NumAut$,6)                  ! Numero de recibo anular
      Gr.Tef2.SeqTrx$ = Right$("000000"+TS.TEMP2$,6)                        ! Numero de recibo anular
      Gr.Tef2.Sequen$ = Xdoc$
    	Gr.Tef2.TrxOper$ = "53"																							  ! Proceso de anulacion
      Gr.Tef2.IdCte$ = Mid$(XRtrama$,  5,  2)                              	! ID de la red
      Gr.Tef2.Tpdif$ = " "
      Gr.Tef2.IdDif$ = Mid$(Xmtrama$,207, 2)																! Codigo del diferido
      Gr.Tef2.Cuota$ = Mid$(Xmtrama$,209, 2)																! Nro cuotas
      Gr.Tef2.Idtrxdif$ = Gr.Tef2.IdCte$                                    ! Id del diferido
    	If Gr.Tef2.IdDif$ = "00" Then                                        \! Compra Corriente
    	   Gr.Tef2.tmp1$ = "00"  Else 																			 \! Trx corriente
    	   Gr.Tef2.tmp1$ = "01"																								! Trx diferido
      Gr.Tef2.tmp2$ = Mid$(Xmtrama$, 81, 12)																! Reporta valor a pagar      
      Gr.Tef2.tmp4$ = Mid$(Xmtrama$, 93, 12)																! Valor Base Impto
      Gr.Tef2.tmp5$ = Mid$(Xmtrama$,105, 12)																! Valor Impuesto
      Gr.Tef2.CardAnul$ = Mid$(XRtrama$,172, 25)										  			! Numero de la tarjeta anulada
      Gr.Tef2.Bin$   = Gr.Tef2.CardAnul$																		! Retorna numero anulado
      Write #34 ; "VERIFICAR ESTADO EN PINPAD " + CHR$(10)
  	  Gr.Tef2.AnOff%  = -1
      Gr.Tef.TrxSale% = 0 																									! Control del proceso
      Call PROCESA.COMPRA(Gr.Tef2.tmp2$,Gr.Tef2.TrxOper$,Gr.Tef2.Tmp1$)     ! Procesa compra 
      If Gr.Tef.TrxSale% = -1 Then Begin																	  ! Proceso exitoso
      	  Write #34 ; "PROCESO OK PINPAD " + CHR$(10)
    		  TS.IO.STATE = 20																									!
          Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 										 			! Init vectores de carga
          TS.IO.KEYS(1)  = 70 																							!
          TS.IO.DATA$(3) = Right$(Gr.Tef2.TipVar$,1)                   			! Variedad de Pago
          TS.IO.KEYS(3)  = 78                                         			! Tecla Slash
          TS.IO.DATA$(7) = Gr.Tef2.tmp2$               											! Valor a entregar
          TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.Tef2.TipVar$,1))          			! Tipo de Pago
          TS.IO.DATA$(9) = TS.TEMP2$																		  	! Numero recibo
          TS.IO.MOTORKEY = TS.IO.KEYS(7)															 			! Tecla motora    
          Gr.Tef2.Intrx% = -1																								! En proceso de tef
          Gr.Tef2.Stat%  = -1 																							! Trx TEF 
          TS.BAL.TAKEN   = -1																								! Reporta ya total en la trx
          TS.TRX.STATUS  =  1 																							!
          Gr.Tef2.AnOff% = -1						  																	! En anul voucher fuera trx
          DelRec 94 ; Xdoc$																									! Elimina voucher para procesos
      EndIf Else BEGIN 
      	  Write #34 ; "FALLO VERIFICAR PINPAD " + CHR$(10)
      EndiF
      Close 94      
  EndIf  
    If Gr.Tef2.AnOff% = 0 Then Begin																				! No existe voucher
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)				     									! Inicializa secuencias 
       TS.IO.MOTORKEY = 73															   									! de tecleo 
       Call Gr.Init.Trx																											! Anula procedimiento
       Exit Sub 
    EndIf 
    
End Sub 

                                                
Sub ANUL.MOV.TEF         																									  ! Anulacion Movimiento TEF
String Xrtrama$, Xmtrama$, Xdoc$
    Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10)             									  ! Init vectores de carga
    TS.IO.DATA$(5) = "1"											           										! Secuencia cambio a efectivo
    TS.IO.KEYS(5)  = 61											             										!
    TS.IO.MOTORKEY = 61											             										!
    CALL TSTCEC01                                  	     										! Ejecuta Procedimiento
   	TS.TEMP1$ = Left$(Gr.Tef2.Msg$(5),20)							     									! Captura mensaje
   	TS.TEMP2$ = Right$(Gr.Tef2.Msg$(5),20)							   									!
    TS.TEMP2$ = ASIC.DATOS$(TS.TEMP1$,TS.TEMP2$)				   							    ! Mensaje captura dato
    If TS.TEMP2$ = "E" Then BEGIN 											   									! Proceso cancelado
       Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO") 	 									! Mensaje alerta
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)				     									! Inicializa secuencias 
       TS.IO.MOTORKEY = 73															   									! de tecleo 
       Exit Sub 																				   									! Sale de la rutina
    EndIf 																							   									! Fin proceso cancelado
   TS.TS11WERR$ = ""																												! Control errores
   Open "R::$F2MVTRX" KEYED RECL 435 As 94                 									! Abre copias voucher
   If TS.TS11WERR$ <> "" Then BEGIN 																				! Error de apertura
 	    Call VISOR.AND.BORRAR(Gr.Tef2.Msg$(7))																! Mensaje alerta
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencias 
      TS.IO.MOTORKEY = 73																										! de tecleo 
      Exit Sub 																															! Sale de la rutina
   EndIf 																																		!
   TS.TEMP2$ = Right$("000000"+TS.TEMP2$,6)																	! Arma llave de busqueda
   Xdoc$ = TS.TEMP2$
   TS.TS11WERR$ = ""																												! Control errores
   Read Form "C6 C198 C231" ;#94 KEY TS.TEMP2$;                            \! Lee Reg Cabecera 
        Gr.Tef2.tmp3$, 																					           \! Llave del registro
        Xrtrama$,     																						         \! Primer String
        Xmtrama$                                                            ! Segundo String
  If TS.TS11WERR$ <> "" Then Begin 																					! Voucher no localizado
 	    Call VISOR.AND.BORRAR(Gr.Tef2.Msg$(10))																! Mensaje alerta
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencias 
      TS.IO.MOTORKEY = 73																										! de tecleo 
      Close 94																															! Cierra archivo
      Call Gr.Init.Trx																											! Anula procedimiento
  EndIf Else Begin																													! Si lo encontro
      Gr.Tef2.NumRcv$ = Right$("000000"+TS.TEMP2$,6)                        ! Numero de recibo anular
      Gr.Tef2.NumAut$ = Mid$(XRtrama$, 73,  6)                  						! Ajusta dato
      Gr.Tef2.NumAut$ = Right$("000000"+Gr.Tef2.NumAut$,6)                  ! Numero de recibo anular
      Gr.Tef2.SeqTrx$ = Right$("000000"+TS.TEMP2$,6)                        ! Numero de recibo anular
      Gr.Tef2.Sequen$ = Xdoc$
    	Gr.Tef2.TrxOper$ = "53"																							  ! Proceso de anulacion
    	
      Gr.Tef2.IdCte$ = Mid$(XRtrama$,  5,  2)                              	! ID de la red
      Gr.Tef2.Tpdif$ = " "
      Gr.Tef2.IdDif$ = Mid$(Xmtrama$,207, 2)																! Codigo del diferido
      Gr.Tef2.Cuota$ = Mid$(Xmtrama$,209, 2)																! Nro cuotas
      Gr.Tef2.Idtrxdif$ = Gr.Tef2.IdCte$                                    ! Id del diferido
    	If Gr.Tef2.IdDif$ = "00" Then                                        \! Compra Corriente
    	   Gr.Tef2.tmp1$ = "00"  Else 																			 \! Trx corriente
    	   Gr.Tef2.tmp1$ = "01"																								! Trx diferido
      Gr.Tef2.tmp2$ = Mid$(Xmtrama$, 81, 12)																! Reporta valor a pagar      
      Gr.Tef2.tmp4$ = Mid$(Xmtrama$, 93, 12)																! Valor Base Impto
      Gr.Tef2.tmp5$ = Mid$(Xmtrama$,105, 12)																! Valor Impuesto
      Gr.Tef2.CardAnul$ = Mid$(XRtrama$,172, 25)										  			! Numero de la tarjeta anulada
      Gr.Tef2.Bin$   = Gr.Tef2.CardAnul$																		! Retorna numero anulado
      Gr.Tef2.AnOff% = -1																										! En anul voucher fuera trx
      Gr.Tef.TrxSale% = 0 																									! Control del proceso
      Call PROCESA.COMPRA(Gr.Tef2.tmp2$,Gr.Tef2.TrxOper$,Gr.Tef2.Tmp1$)     ! Procesa compra 
      If Gr.Tef.TrxSale% = -1 Then Begin																	  ! Proceso exitoso
          Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 												  ! Init vectores de carga
          TS.IO.DATA$(3) = Right$(Gr.Tef2.TipVar$,1)                  		  ! Variedad de Pago
          TS.IO.KEYS(3)  = 78                                           		! Tecla Slash
          TS.IO.DATA$(7) = Gr.Tef2.tmp2$                       							! Valor a entregar
          TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.Tef2.TipVar$,1))         		  ! Tipo de Pago
          TS.IO.DATA$(9) = Xdoc$																					  ! Numero Autorizacion
          TS.IO.MOTORKEY = TS.IO.KEYS(7)																	  ! Tecla motora
          Gr.Tef2.Intrx% = -1																								! En proceso de tef
          Gr.Tef2.Stat%  = -1 																							! Trx TEF 
          TS.BAL.TAKEN   = -1																								! Reporta ya total en la trx
          TS.TRX.STATUS  =  1 																							!
          DelRec 94 ; Xdoc$																									! Elimina voucher para procesos
  	      Close 94																													! Cierre archivo
      EndIf Else Begin																											! Fin proceso exitoso
      	  Close 94 
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													! Inicializa secuencias 
          TS.IO.MOTORKEY = 73																								! de tecleo 
          Call Gr.Init.Trx																									! Anula procedimiento
      EndIf 
  EndIf  

End Sub 

Sub COPIA.VOUCHER.TEF																												! COPIA VOUCHER TEF
String Xrtrama$, Xmtrama$
   TS.TEMP1$ = Left$(Gr.Tef2.Msg$(6),20)																		! Captura mensaje
   TS.TEMP2$ = Right$(Gr.Tef2.Msg$(6),20)																		!
   TS.TEMP2$ = ASIC.DATOS$(TS.TEMP1$,TS.TEMP2$)											    		! Mensaje captura dato
   If TS.TEMP4$ = "E" Then BEGIN 																						! Proceso cancelado
 	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")											! Mensaje alerta
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencias 
      TS.IO.MOTORKEY = 73																										! de tecleo 
      Exit Sub 																															! Sale de la rutina
   EndIf 																																		! Fin proceso cancelado
   TS.TS11WERR$ = ""																												! Control errores
   Open "R::$F2MVTRX" KEYED RECL 435 As 94                 									! Abre copias voucher
   If TS.TS11WERR$ <> "" Then BEGIN 																				! Error de apertura
 	    Call VISOR.AND.BORRAR(Gr.Tef2.Msg$(7))																! Mensaje alerta
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencias 
      TS.IO.MOTORKEY = 73																										! de tecleo 
      Exit Sub 																															! Sale de la rutina
   EndIf 																																		!
   TS.TEMP2$ = Right$("000000"+TS.TEMP2$,6)																	! Arma llave de busqueda
   TS.TS11WERR$ = ""																												! Control errores
   Read Form "C6 C198 C231" ;#94 KEY TS.TEMP2$;                            \! Lee Reg Cabecera 
        Gr.Tef2.tmp3$, 																					           \! Llave del registro
        Xrtrama$,     																						         \! Primer String
        Xmtrama$                                                            ! Segundo String
  If TS.TS11WERR$ <> "" Then Begin 																					! Voucher no localizado
 	    Call VISOR.AND.BORRAR(Gr.Tef2.Msg$(8))																! Mensaje alerta
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencias 
      TS.IO.MOTORKEY = 73																										! de tecleo 
      Dim Gr.Tef2.RtaTef$(20,50)																					  ! Rta trama TEF
  EndIf Else Begin
 	   Dim Gr.Tef2.RtaTef$(20,50)																						  ! Rta trama TEF
 	   Gr.Tef2.Ttrx% = 1																											! Un voucher a generar

!-- Primer String
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 1) = Mid$(XRtrama$,  1,  2)							! Rta Trx TEF                 A
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 2) = Mid$(XRtrama$,  3,  2)							! codigo rta ejecucion pinpad	B
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 3) = Mid$(XRtrama$,  5,  2)							! codigo identif red					C
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 4) = Mid$(XRtrama$,  7, 40)							! Msg rta pin pad							D
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 5) = Mid$(XRtrama$, 47,  6)							! Secuencial trx							E
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 6) = Mid$(XRtrama$, 53,  6)							! Nro Lote										F
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 7) = Mid$(XRtrama$, 59,  6)							! Hora Trx										G
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 8) = Mid$(XRtrama$, 65,  8)							! Fecha Trx										H
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 9) = Mid$(XRtrama$, 73,  6)							! Nro Autorizacion						I
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,10) = Mid$(XRtrama$, 79,  8)							! Terminal ID     						J
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,11) = Mid$(XRtrama$, 87, 15)							! Merchat ID                  K
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,12) = Mid$(XRtrama$,102, 12)							! Valor de los interes     		L
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,13) = Mid$(XRtrama$,114,  3)							! Codigo banco adquiriente    M
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,14) = Mid$(XRtrama$,117, 30)							! Nombre banco adquiriente    N
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,15) = Mid$(XRtrama$,147, 25)							! Nombre Franquicia					  O
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,16) = Mid$(XRtrama$,172, 25)             ! Nro Tarjeta	habiente 				P
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,17) = Mid$(XRtrama$,197,  2)							! Modo de lectura 						Q

     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,18) = Mid$(Xmtrama$,  1, 40)							 ! Nombre tarjeta habiente    R  
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,19) = Mid$(Xmtrama$, 41, 20)							 ! ID APPL EMV solo chip      S  
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,20) = Mid$(Xmtrama$, 61, 20)							 ! AID-EMV         						T   
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,21) = Mid$(Xmtrama$, 81, 12)							 ! Valor de la compra    			1   
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,22) = Mid$(Xmtrama$, 93, 12)							 ! Valor Base compra    			2 
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,23) = Mid$(Xmtrama$,105, 12)							 ! Valor Impuestos       			3   
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,24) = Mid$(Xmtrama$,117, 12)							 ! Valor Excento Impuestos		4
      
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,26) = Mid$(Xmtrama$,129,  2)							 ! Rta trama 2     						U   
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,27) = Mid$(Xmtrama$,131, 22)							 ! Valor EMV solo chip        V  
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,28) = Mid$(Xmtrama$,153, 16)							 ! ARQC solo chip  						W   
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,29) = ""                      					   ! Mensaje promocion 80	    	X   
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,30) = Mid$(Xmtrama$,169, 15)							 ! Validacion de pin        	Y 
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,31) = Mid$(Xmtrama$,184, 15)							 ! Mid Operador telefonico  	6 
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,32) = Mid$(Xmtrama$,199,  8)							 ! TID Operador telefonico  	7 
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,33) = Mid$(Xmtrama$,207,  2)              ! Tipo Diferido							8
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,34) = Mid$(Xmtrama$,209,  2)							 ! Plazo diferido							9   
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,35) = Mid$(Xmtrama$,211, 20)							 ! Descripcion diferido				0   
     Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,36) = Mid$(Xmtrama$,231,  1)							 ! Con o Sin interes   				    
       
     Call IMPRESION.VOUCHER.TEF2																						! Genera copia impresa
     Call VISOR.AND.BORRAR(Gr.Tef2.Msg$(9))																  ! Mensaje alerta
     Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															  ! Inicializa secuencias 
     TS.IO.MOTORKEY = 73																										! de tecleo 
 	   Dim Gr.Tef2.RtaTef$(20,50)																						  ! Rta trama TEF
 	   Gr.Tef2.Ttrx% = 0																											! Un voucher a generar
  EndIf
  Close 94
End Sub 

Sub GRTEF2ECDR(Xopt%) Public                                                ! TEF 2 Ecuador Datafonos
Integer*4 Xopt%, Gri%

!-- EAMTSU07.J86
If Xopt% = 07 Then Begin																										! En carga de opciones
  Gr.Tef2.Ok% = 0 																													! Init variables
  Gr.Tef2.Stat% = 0																												  !
  Gr.Tef2.Ttrx% = 0																												  !
  Gr.Tef2.AnOff% = 0
  Dim Gr.Tef2.RtaTef$(20,50)																								! Rta trama TEF
  Gr.Tef2.tmp1$ = "R::ADX_IDT1:TEFPARAM."+Right$("000"+TS.TERMINAL$,3)      ! Archivo de parametros
  Open Gr.Tef2.tmp1$ AS 94 NoWrite NoDel  	   															! Apertura archivo parametrizacion 
  If TS.ER.RETURN <> -1 Then BEGIN                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA PARAMETROS TEF")									! MSg alerta
  	 Call U.IMPRIME("ERROR APERTURA PARAMETROS TEF",6100H)									! Rastro rollo auditoria
  	 GoTo ESTADO.PROYECTO.TEF																								! Sale del proceso
  EndIf 																																		! Fin Error apertura
  If End #94 Then UE.FIN.TEF2           																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[TEF II ECUADOR]" Then Begin	         	     				! TEF II Ecuador
         Read #94; TS.TEMP1$																								! Lectura registro
         Gr.Tef2.Ok% = Val(Mid$(TS.TEMP1$,30,2))   			    					      ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																								! Lectura registro
         Gr.Tef2.Key% = Val(Mid$(TS.TEMP1$,30,3))  			    					      ! Tecla Motora Tef II
         Read #94; TS.TEMP1$																								! Lectura registro
         Gr.Tef2.TipVar$ = Mid$(TS.TEMP1$,30,2)    			    					      ! Tipo variedad pago TEF II
         Read #94; TS.TEMP1$																								! Lectura registro
         Gr.Tef2.Ncopia% = Val(Mid$(TS.TEMP1$,30,02))		    					      ! Numero copias vocuher
         Read #94; TS.TEMP1$																								! Lectura registro
         Gr.Tef2.MidMnet$ = Mid$(TS.TEMP1$,30,15)  			    					      ! Mid Medianet
         Read #94; TS.TEMP1$																								! Lectura registro
         Gr.Tef2.TidMnet$ = Mid$(TS.TEMP1$,30,08)  			    					      ! Tid Medianet
         Read #94; TS.TEMP1$																								! Lectura registro
         Gr.Tef2.MidData$ = Mid$(TS.TEMP1$,30,15)  			    					      ! Mid Data Fast
         Read #94; TS.TEMP1$																								! Lectura registro
         Gr.Tef2.TidData$ = Mid$(TS.TEMP1$,30,08)  			    					      ! Tid Data Fast
         Gr.Tef2.MidMnet$ = Right$("               "+Gr.Tef2.MidMnet$,15)   ! Ajusta informacion
         Gr.Tef2.MidData$ = Right$("               "+Gr.Tef2.MidData$,15)   ! a las longitudes de 
         Gr.Tef2.TidMnet$ = Right$("        "+Gr.Tef2.TidMnet$,8)           ! campo requeridos
         Gr.Tef2.TidData$ = Right$("        "+Gr.Tef2.TidData$,8)           !
         GoTo UE.FIN.TEF2    																								! Sale del ciclo de carga  
        EndIf																																! Fin carga parametros        
  Wend 																																			! Fin carga parametros 
  UE.FIN.TEF2:                                                              !
     Close 94																																! Cierra archivo
   Call GR.PARAMETROS.TEF                                                   ! Carga parametros TEF
   Call GR.DISENO.VOUCHER                                                   ! Carga diseno voucher
   Call Gr.Carga.Bines.Tef																									! Carga lista bines 
   ESTADO.PROYECTO.TEF:
   If Gr.Tef2.Ok% = -1 Then                                                \! Proyecto Activo
      Call U.IMPRIME("MODULO TEF 2 ECUADOR ON  29-Ago-2017",6100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO TEF 2 ECUADOR OFF 29-Ago-2017",6100H)          ! Msg Proyecto Cargado
EndIf																																				! Fin EAMTSU07

If Gr.Tef2.Ok% <> -1 Then Exit Sub                                          ! Si proyecto apagado

!-- EAMTSU02.J86
If Xopt% = 02 Then Begin																										! Al inicio de cada trx de venta
	 Call IMPRESION.VOUCHER.TEF2																							! Genera vouchers
	 Gr.Tef2.Stat% = 0																												!
	 Gr.Tef2.Ttrx% = 0																												!
	 Gr.Tef2.AnOff% = 0
	 Gr.Tef.TrxSale% = 0 
	 Dim Gr.Tef2.RtaTef$(20,50)																								! Rta trama TEF
	 Gr.Tef2.CardAnul$ = ""
	 Gr.Tef2.Bin$ = ""
	 Gr.Tef2.AnlRc% = 0
	 Gr.Tef2.ClOff% = 0 
EndIf																																				! Fin EAMTSU02

!-- EAMTSU14.J86
If Xopt% = 14 Then Begin																										! En entrada de datos 
	 If Gr.Tef2.ClOff% = -1 Then Begin																				!
	 	  Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
      TS.IO.KEYS(7)  = 91
      TS.IO.DATA$(7) = Gr.Tef2.tmp2$
	 	  TS.IO.MOTORKEY = 81
	 	  Gr.Tef2.ClOff% = 0
	 	  Exit Sub 
	 EndIf																																		!
   If	TS.IO.KEYS(1) = 70 And TS.IO.MOTORKEY = 81 Then Begin                 ! Si anulacion total
 	  If (TS.TOTALS(0,0,0) <> TS.BALDUE(0)) Then Begin												! Si pagos capturados
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
   	   TS.GUIDANCE = 1069																										! Msg Error appl nativa
	     TS.IO.MOTORKEY = 0																										!
	  EndIf
   EndIf

 If (TS.IO.KEYS(5) = 61) And (TS.IO.STATE = 10) Then Begin      						! Si presiona secuencia
   If TS.INTRX Then Begin 																									! Si dentro de una trx
   	  Call VISOR.AND.BORRAR("PROCESO NO VALIDO   EN UNA TRX. BORRAR  ")			! Msg de alerta
      TS.IO.MOTORKEY = 0     																								! Inicializa secuencia
      Dim TS.IO.KEYS(10)                                          					! de tecleo 
      Dim TS.IO.DATA$(10)                                         					! 
      Exit Sub 																															!
   EndIf 																																		!
   If (Val(TS.IO.DATA$(5)) = 60) Then Call GR.FINAL.DIA.TEF  								! Proceso cierre fin dia datafono
   If (Val(TS.IO.DATA$(5)) = 61) Then Call GR.CONFIG.PINPAD  								! Proceso Actualizacion IP  gateway
   If (Val(TS.IO.DATA$(5)) = 62) Then Call GR.ACTSW.PINPAD  								! Proceso Actualizacion Sw Pinpad
!   If (Val(TS.IO.DATA$(5)) = 63) Then Call ANUL.MOV.TEF                     ! Anulacion movimiento TEF
	
   If (Val(TS.IO.DATA$(5)) = 64) Then Call COPIA.VOUCHER.TEF							  ! Copia voucher datafono
   	
   If (Val(TS.IO.DATA$(5)) = 63) Then Call PRUEBA.CAMBIO.VC  							  ! Copia voucher datafono
   	
 EndIf 																																			!

   If TS.IO.MOTORKEY = Gr.Tef2.Key% Then Begin													    ! Si tecla motora tef ii

    If Gr.Tef2.Ttrx% >= 20 Then Begin																				! Para evitar Error SU por nro de operaciones
    	 Call VISOR.AND.BORRAR("NO SE PERMITEN MAS  OPERACIONES TEF")	        ! Msg alerta
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencias 
       TS.IO.MOTORKEY = 73																									! de tecleo 
       Exit Sub 
    EndIf

  	If TS.TRX.STATUS <> 1 And TS.PROCEDURE = -1 THEN BEGIN				  	      ! No se ha ingresado total en venta
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 0 																									! 
       TS.GUIDANCE = 1020																										! Solicita total a la trx
       Exit Sub 
    EndIf 

    If (TS.IO.KEYS(5) = 61) And (TS.IO.STATE = 10) Then Begin    						! Si secuencias fuera venta
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 0 																									! 
       TS.GUIDANCE = 1075																										! Revise Procedimiento
       Exit Sub 
    EndIf
    
   	Gr.Tef2.Anul% = 0																												! En Venta
    Gr.Tef2.tmp2$ = TS.IO.DATA$(7)																					! Reporta valor a pagar
    If Gr.Tef2.tmp2$ = "" Then 																					   \! Valor de la compra
       Gr.Tef2.tmp2$ = STR$(TS.BALDUE(0))																	  ! 

    If Val(Gr.Tef2.tmp2$) <= 0 Then Begin
    	 Call VISOR.AND.BORRAR(Gr.Tef2.Msg$(11))					                    ! Msg alerta
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencias 
       TS.IO.MOTORKEY = 73																									! de tecleo 
       Exit Sub 
    EndIf
    If TS.IO.KEYS(1) = 70 Then                                             \! Anulacion del pago
    	 Gr.Tef2.Anul% = -1

!-- Tipo de tarjeta a procesar
    Tipo.Trx.Tef:
    
   	TS.TEMP1$ = Left$(Gr.Tef2.Msg$(4),20)																		! Captura mensaje
   	TS.TEMP2$ = Right$(Gr.Tef2.Msg$(4),20)																	!
    TS.TEMP2$ = GR.DATOS$(TS.TEMP1$,TS.TEMP2$)										          ! Mensaje captura dato
    If TS.TEMP2$ = "E" Then BEGIN 																					! Proceso cancelado
    	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")											! Mensaje alerta
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencias 
       TS.IO.MOTORKEY = 73																									! de tecleo 
       Exit Sub 																														! Sale de la rutina
    EndIf 																																	! Fin proceso cancelado
    Gr.Tef2.Tmp1$ = ""																											! Tipo de Compra
    If TS.TEMP2$ = "1" Then Gr.Tef2.Tmp1$ = "00"                            ! Compra corriente
    If TS.TEMP2$ = "2" Then Gr.Tef2.Tmp1$ = "01"                            ! Compra diferido
    If TS.TEMP2$ = "3" Then Gr.Tef2.Tmp1$ = "02"                            ! Compra Debito
    If Gr.Tef2.Tmp1$ = "" Then Begin 																				! Opcion Invalida
    	 Call VISOR.AND.BORRAR("ERROR TIPO OPERACION")                        ! Msg Alerta
    	 GoTo Tipo.Trx.Tef																										! Repite Dialogo
    EndIf
!-- Valor de la compra
    If Gr.Tef2.Anul% <> -1 Then Begin																				! Proceso de venta
       Gr.Tef2.SeqTrx$ = "      "                                           ! Sequential Trx
       Gr.Tef2.NumAut$ = "      "                                           ! Numero autorizacion
       If TS.TEMP2$ = "1" Then Gr.Tef2.TrxOper$ = "51"                      ! Compra corriente
       If TS.TEMP2$ = "2" Then Gr.Tef2.TrxOper$ = "51"                      ! Compra diferido
    EndIf Else Begin 																												!
    	 Gr.Tef2.TrxOper$ = "53"																							! Proceso de anulacion
   	   TS.TEMP1$ = Left$(Gr.Tef2.Msg$(5),20)																! Captura mensaje
   	   TS.TEMP2$ = Right$(Gr.Tef2.Msg$(5),20)																!
       TS.TEMP2$ = ASIC.DATOS$(TS.TEMP1$,TS.TEMP2$)											    ! Mensaje captura dato
       If TS.TEMP4$ = "E" Then BEGIN 																				! Proceso cancelado
    	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")									! Mensaje alerta
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													! Inicializa secuencias 
          TS.IO.MOTORKEY = 73																								! de tecleo 
          Exit Sub 																													! Sale de la rutina
       EndIf 																																! Fin proceso cancelado
       Gr.Tef2.NumRcv$ = Right$("000000"+TS.TEMP2$,6)                       ! Numero de recibo anular
    EndIf
    Gr.Tef.TrxSale% = 0 																										! Control del proceso
    Call PROCESA.COMPRA(Gr.Tef2.tmp2$,Gr.Tef2.TrxOper$,Gr.Tef2.Tmp1$)       ! Procesa compra 
    If Gr.Tef.TrxSale% = 2 Then Begin 																			! Dscto forma pago 
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencias 
       TS.IO.MOTORKEY = 73																									! de tecleo 
       Gr.Tef2.Intrx% = 0																										! 
       Exit Sub 																														! Sale de la rutina
    EndIf
    If Gr.Tef.TrxSale% = -1 Then Begin																			! Compra satisfactoria
       Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 														! Init vectores de carga
       If Gr.Tef2.Anul% = -1 Then                                          \! Anulacion del pago
    	    TS.IO.KEYS(1) = 70																								! Reporta anulacion pago
       TS.IO.DATA$(3) = Right$(Gr.Tef2.TipVar$,1)                  				  ! Variedad de Pago
       TS.IO.KEYS(3)  = 78                                           				! Tecla Slash
       TS.IO.DATA$(7) = Gr.Tef2.tmp2$                       								! Valor a entregar
       TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.Tef2.TipVar$,1))         				  ! Tipo de Pago
       TS.IO.DATA$(9) = Gr.Tef2.NumAut$																	    ! Numero Autorizacion
       TS.IO.MOTORKEY = TS.IO.KEYS(7)																				! Tecla motora
       Gr.Tef2.Intrx% = -1																									! En proceso de tef
       Gr.Tef2.Stat%  = -1 																									! Trx TEF 
    EndIf Else Begin																										    ! Falla en compra
    	 Call VISOR.AND.BORRAR(Gr.Tef2.Msg$(3))						                    ! Msg alerta
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencias 
       TS.IO.MOTORKEY = 73																									! de tecleo 
       Gr.Tef2.Intrx% = 0																										! 
       Exit Sub 																														! Sale de la rutina
    EndIf																																		! Fin Proceso compra
   EndIf																																		! Fin tecla motora
EndIf																																				! Fin EAMTSU14

!-- EAMTSU20.J86
If Xopt% = 20 Then Begin																										! En la impresion del CR
	
EndIf																																				! Fin EAMTSU20

!-- EAMTSU53.J86
If Xopt% = 53 Then Begin																										! En recuperación de trx

 If Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "99" THEN BEGIN   	    	            ! Si DATA/ENTRY
  If Unpack$(MID$(SL.STR.ENTRY$,3,4)) = "20150504" Then Begin               ! TEF II Ecuador
    Asc.Tmp.Apun% = 3																												!
    TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                   ! Numero de proyecto
    If TS.TEMP1$ = "20150504" Then Begin																		! Hay trx TEF II
    	 Gr.Tef2.Ttrx% = Gr.Tef2.Ttrx% + 1																		! Total de tarjetas
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 1) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)  ! Rta Trx TEF                 A			 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 2) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	 ! codigo rta ejecucion pinpad B			 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 3) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)  ! codigo identif red					 C			 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 4) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)  ! Msg rta pin pad						 D			 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 5) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	 ! Secuencial trx							 E			 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 6) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	 ! Nro Lote										 F			 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 7) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	 ! Hora Trx										 G		   
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 8) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)  ! Fecha Trx									 H			 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 9) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	 ! Nro Autorizacion						 I			 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,10) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	 ! Terminal ID     						 J			 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,11) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	 ! Merchat ID                  K			 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,12) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	 ! Valor de los interes     	 L			 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,13) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)  ! Codigo banco adquiriente    M		   
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,14) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	 ! Nombre banco adquiriente    N			 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,15) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	 ! Nombre Franquicia					 O			 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,16) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	 ! Nro Tarjeta	habiente			 P			 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,17) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	 ! Modo de lectura 						 Q					
       Gr.Tef2.Intrx% = -1																									! En trx TEF
       Gr.Tef2.Stat%  = -1 																									! Trx TEF 
       Gr.Tef.TrxSale% = -1
       Gr.Tef2.NumRcv$ = Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 5)									! Numero del recibo
       Gr.Tef2.NumAut$ = Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 9)									! Numero de la autorizacion
    EndIf
	EndIf
  If Unpack$(MID$(SL.STR.ENTRY$,3,4)) = "20150505" Then Begin               ! EXTENSION TEF II Ecuador
    Asc.Tmp.Apun% = 3																												!
    TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                   ! Numero de proyecto
    If TS.TEMP1$ = "20150505" Then Begin																		! Hay trx TEF II
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,18) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%) ! Nombre tarjeta habiente     R     						 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,19) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	! ID APPL EMV solo chip       S     						 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,20) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	! AID-EMV         					  T    						 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,21) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	! Valor de la compra    			1    						 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,22) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	! Valor Base compra    			  2      						 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,23) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	! Valor Impuestos       			3    						 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,24) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	! Valor Excento Impuestos		  4      						 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,25) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	! Tipo de Operacion           5     						 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,26) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	! Rta trama 2     						U    						 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,27) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	! Valor EMV solo chip         V     						 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,28) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	! ARQC solo chip  						W    						 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,30) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	! Validacion de Pin           Y  		
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,31) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	! Mid Operador telefonico     6  		
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,32) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	! Tid Operador telefonico     7  		
    EndIf
	EndIf

  If Unpack$(MID$(SL.STR.ENTRY$,3,4)) = "20150506" Then Begin               ! EXTENSION TEF II Ecuador
    Asc.Tmp.Apun% = 3																												!
    TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                   ! Numero de proyecto
    If TS.TEMP1$ = "20150506" Then Begin																		! Hay trx TEF II
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,33) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%) ! Tipo Diferido								8    						 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,34) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	! Plazo diferido							9    						 
       Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,35) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	! Descripcion diferido				0   		
			 Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,36) = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)	! Con o Sin intereses 				
    Endif
  EndIf

 EndIf
 
EndIf																																				! Fin Eamtsu53


!-- EAMTSU60.J86
If Xopt% = 60 Then Begin																										! En la impresion del CR

!If TS.PROCEDURE <= 0 Then 																								 \! Proceso registro pago
!	If (TS.LINETYPE = 2 And TS.LINEDATA = 60) Then Begin				 							! Dato en tiquete


!If (UNPACK$(LEFT$(Gr.Tef2.Str$,1)) = "05") Or                              \! Medio de pago o anulacion
!   (UNPACK$(LEFT$(Gr.Tef2.Str$,1)) = "06") Then Begin                      \! de pago 

If TS.LINETYPE = 2 Then BEGIN										  													! Impresion pago tef 

	 If Gr.Tef2.Intrx% = -1 Then Begin 
	 	If Gr.Fiscal.Tktnf% = 0 Then Begin    																	! Impresion normal
      Call U.IMPRIME("******** PAGO TRANSACCION TEF *******",4100H)
      Call U.IMPRIME("RECIBO:" + Gr.Tef2.NumRcv$ + " AUT:" + Gr.Tef2.NumAut$,4100H)
      Call U.IMPRIME("RECIBO:" + Gr.Tef2.NumRcv$ + " AUT:" + Gr.Tef2.NumAut$,2100H)
    EndIf Else Begin
    	Call U.IMPRIME("******** PAGO TRANSACCION TEF *******",2100H)
    	Call U.IMPRIME("RECIBO:" + Gr.Tef2.NumRcv$ + " AUT:" + Gr.Tef2.NumAut$,2100H)
    	TS.TEMP4I4 = Val(Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,21))
    	Call Format.Amount(TS.TEMP4I4)
      Write #34 ; ("******** PAGO TRANSACCION TEF *******" + Chr$(10))
      Write #34 ; ("RECIBO:" + Gr.Tef2.NumRcv$ + " AUT:" + Gr.Tef2.NumAut$ + Chr$(10))
      Write #34 ; ("TARJETA:"+Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,16)) + Chr$(10)
      Write #34 ; ("MONTO COMPRA: USD "+TS.TEMP1$) + CHR$(10)
      TS.TEMP1$ = "20"+ Left$(DATE$,2) + "/" + Mid$(DATE$,3,2) + "/" +     \!
                  Right$(DATE$,2)            					      								! Fecha de la trx DD
      TS.TEMP1$ = TS.TEMP1$ + "  " + Left$(TIME$,2) + ":" + Mid$(TIME$,3,2) + \!
                  ":" + Right$(TIME$,2)            						    ! Hora de la trx SS              
      Write #34 ; TS.TEMP1$ + CHR$(10)
   	  For Gri% = 1 To 8
   	     Write #34 ; " " + Chr$(10)
   	  Next Gri%
   	  Call U.CORTACR
    EndIf
      Gr.Tef2.Intrx% = 0
      If Not(TS.RECOVERY) Then Begin

          TS.TEMP1$  =  Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 1)   +    						\ ! Rta Trx TEF                 A
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 2) 	+	              \ ! codigo rta ejecucion pinpad B   
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 3) 	+	              \ ! codigo identif red				  C   
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 4) 	+	              \ ! Msg rta pin pad						  D 
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 5) 	+	              \ ! Secuencial trx							E   
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 6) 	+	              \ ! Nro Lote										F   
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 7) 	+	              \ ! Hora Trx										G   
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 8) 	+	              \ ! Fecha Trx									  H 
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%, 9) 	+	              \ ! Nro Autorizacion						I   
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,10) 	+	              \ ! Terminal ID     						J   
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,11) 	+	              \ ! Merchat ID                  K  
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,12) 	+	              \ ! Valor de los interes     	  L 
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,13) 	+	              \ ! Codigo banco adquiriente    M  
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,14) 	+	              \ ! Nombre banco adquiriente    N  
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,15) 	+	              \ ! Nombre Franquicia					  O 
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,16)   +               \ ! Nro Tarjeta	habiente			  P 
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,17) 	 	                ! Modo de lectura 						Q   
          Call Grabacion.Cadena.Usuario("20150504",TS.TEMP1$)                 ! Almacena UD Tef II

          TS.TEMP1$  =  Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,18) 	+	              \ ! Nombre tarjeta habiente     R  
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,19) 	+	              \ ! ID APPL EMV solo chip       S  
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,20) 	+	              \ ! AID-EMV         						T   
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,21) 	+	              \ ! Valor de la compra    			1   
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,22) 	+	              \ ! Valor Base compra    			  2 
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,23) 	+	              \ ! Valor Impuestos       			3   
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,24) 	+	              \ ! Valor Excento Impuestos		  4 
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,25) 	+	              \ ! Tipo de Operacion           5  
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,26) 	+	              \ ! Rta trama 2     						U   
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,27) 	+	              \ ! Valor EMV solo chip         V  
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,28) 	+	              \ ! ARQC solo chip  						W   
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,30) 	+	              \ ! Validacion de pin        	  Y 
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,31) 	+	              \ ! Mid Operador telefonico  	  6 
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,32) 	 	                ! TID Operador telefonico  	  7 
           Call Grabacion.Cadena.Usuario("20150505",TS.TEMP1$)                ! Almacena UD Tef II

          TS.TEMP1$  =  Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,33) 	+	              \ ! Tipo Diferido								8 
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,34) 	+	              \ ! Plazo diferido							9 
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,35) 	+               \ ! Descripcion diferido				0 
                  ":" + Gr.Tef2.RtaTef$(Gr.Tef2.Ttrx%,36) 	                  ! Con o Sin interes   				  
          Call Grabacion.Cadena.Usuario("20150506",TS.TEMP1$)                 ! Almacena UD Tef II
       EndIf
   EndIf
  EndIf

EndIf																																				! Fin EAMTSU60

!-- EAMTSU23.J86
If Xopt% = 23 Then Begin																										! En la pantalla
	 If MATCH("TECLEE NUEVO PAGO",TS.DISP1$,1) > 0 Then Begin									!
      If Gr.Tef2.AnOff% = -1 Then Begin																			! En proceso de anulacion
      	 Gr.Tef2.ClOff% = -1
      EndIf      	 	   																											!
	 EndIf																																		!
EndIf																																				! 

End Sub 																																		! Fin TEF 2 Ecuador
