!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMRDPTO.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Redencion premios Leal           *
!**************************************************
!Observaciones:
! Version 1.0 - Junio 7 2022
!--------------------------------------------------

%ENVIRON T		                         																			! Ambiente de terminal

%INCLUDE EAMTSEVA.J86          																							! Variables Cliente Frecuente
%INCLUDE EAMTSWKG.J86          																							! working storage
%INCLUDE EAMTOPTS.J86          																							! Terminal Options
%INCLUDE EAMTRANS.J86          																							! Variables de transancion
%INCLUDE EAMITEMR.J86          																							! Variables EamItemr
%INCLUDE LCLTSUVA.011	                                                      ! Variables desarrollo              
%INCLUDE EAMTSXHC.J86          																							! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          																							! Rutinas de Assembler

%INCLUDE RECATSSU.011        																								! Rutinas Genericas Grupo Retail

Sub TSHIECET External 
End Sub 

Sub TSBDEC01 EXTERNAL          ! balance due (normal/foodstamp)
End Sub                        !

Sub TSTDEC03 EXTERNAL          ! tendering
End Sub 

Function FORMAT.AMOUNT(AMT1) External 																			! Formateo de valores
 Integer*1 FORMAT.AMOUNT
 Integer*4 AMT1
End Function

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                    *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                       *!
  STRING    DATA$      !* String to hold the data read from file.          *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                             *!
End Sub 

Sub LEALTAD.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$) External						!
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$,      \!
       X.RTA$, X.BUFF$																											!
Integer*4 X.Len%																														!
End Sub 

Sub Aplica.Pago.Leal(Xopc%)																									! Aplica pago redencion
String Xpago$, Xtmp$, Xsgn$
Integer*1 Xopc%
Integer*4 XI%
	    Call VISORES4690(1,"APLICANDO PAGO",Str$(Gr.Lcl.VlrBnf%),1000,"L")
      If Xopc% = 1 Then Begin	    
   	     Xpago$ = "05" 
         !TS.TOTALS(0,0,0)  = TS.TOTALS(0,0,0) - Gr.Lcl.VlrBnf%             	!
         TS.BALDUE(0)      = TS.BALDUE(0) - Gr.Lcl.VlrBnf%              	  ! Actualiza variables
         TS.TENDERED(0)    = TS.TENDERED(0) + Gr.Lcl.VlrBnf%             	  ! Actualiza variables
      EndIf Else Begin
   	     Xpago$ = "06" 
         !TS.TOTALS(0,0,0)  = TS.TOTALS(0,0,0) + Gr.Lcl.VlrBnf%             	!
         TS.BALDUE(0)      = TS.BALDUE(0) + Gr.Lcl.VlrBnf%              	  ! Actualiza variables
         TS.TENDERED(0)    = TS.TENDERED(0) - Gr.Lcl.VlrBnf%             	  ! Actualiza variables
      EndIf
      Sl.End = Sl.End + 1 																									! Aumenta apuntador
      Sl.Str$(Sl.End) = Pack$(Xpago$)                     +		             \! String forma de pago
		                    ":"+Pack$(Gr.Lcl.TVP$)            +		             \! Tipo y variedad de pago 
		                    ":"+Pack$(Str$(Gr.Lcl.VlrBnf%))   +                \! Valor del pago
		                    ":"+Pack$("00")                   +                \! Tender cashing fee amount
		                    ":"+Pack$(Gr.Lcl.Clte$)           +                \! Cliente 
		                    ":"+Pack$("50")                                     ! Status del pago
		  TS.TEMP1I1 = Val(Left$(Gr.Lcl.TVP$,1))  										  				! Toma tipo de pago
		  TS.TEMP2$ = TS.SDESC$(TS.TEMP1I1)																			! Extrae descriptor
		  If Xopc% = 1 Then \
         Call Format.Amount(Gr.Lcl.VlrBnf%) Else \
         Call Format.Amount(Gr.Lcl.VlrBnf% * -1)
		  TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)                         !
		  TS.TEMP3$ = "     "+TS.TEMP2$ + " "+TS.TEMP1$  											  !
      Call U.IMPRIME(TS.TEMP3$,4101H)																				!
      Call U.IMPRIME(TS.TEMP3$,2100H)																				!
      TS.BD.DSPPARM = -1                              					  					! SHOW BD TO DISPLAY SUBTOTAL
      CALL TSBDEC01                                   											! DISPLAY NEW SUBTOTAL
      If Xpago$ = "05" Then Xsgn$ = "00" Else Xsgn$ = "01"                  ! Signo de la operacion
      Xtmp$ = Pack$(Gr.Lcl.Clte$)                                  +       \! Cliente 
               ":"+Pack$(Gr.Lcl.Rdtrx$)													   +       \! Puntos redimidos en la trx 
               ":"+Pack$(Str$(Gr.Lcl.VlrBnf%))                     +       \! Valor de la redencion
               ":"+Pack$(Gr.Lcl.Idpos$)                            +       \! ID de transaccion
               ":"+Pack$(Xsgn$)                                             ! Signo de la operacion 
      Call Grabacion.Cadena.Usuario2("20220701",Xtmp$)        							! Almacena user data              
      Dim Ts.Io.Keys(10)																										! Inicializa Vectores
      Dim Ts.Io.Data$(10)																										! 
      TS.IO.MOTORKEY = 0 																										! Tecla total
      TS.GUIDANCE = 1020																										! Obliga totalizar

End Sub 

Function REVISA.ITEM.PREMIO
Integer*1 REVISA.ITEM.PREMIO
String    XT1$, XT2$
    TS.ER.RETURN = -1
    XT2$ = Pack$(Right$("000000000000"+Gr.Lcl.Premio$,12)) 				    			! Armo dato para busqueda
    Read Form "C6 C71" ; #4 KEY XT2$; XT2$, XT1$														! Busca en maestro items
    If TS.ER.RETURN <> -1 Then Begin 																				! Item no exite
    	 Call VISOR.AND.BORRAR("PRODUCTO NO EXITE   EN MAESTRA /Borrar")      ! Msg de alerta
    	 Call VISOR.AND.BORRAR("NO SE PERMITE LA    REDENCION  /Borrar")      ! Msg de alerta
    	 REVISA.ITEM.PREMIO = 0																								! Reporta falla
    EndIf Else REVISA.ITEM.PREMIO = -1																			! Encontrado
End Function 

Function REDEN.PREMIO.LEAL																									! Redencion de puntos
Integer*2 REDEN.PREMIO.LEAL, XVldItm%
String Gr.Clte$, Xsnd$, Xfin$, Xbuff$, XTEMP4$, Xprm$
REDEN.PREMIO.LEAL = 0																												! retorna proceso fallido
If Gr.Lcl.Ptot% <= 0 Then Begin
	 Call VISOR.AND.BORRAR("NO TIENE PUNTAJE A  REDIMIR    /Borrar")      		! Msg de alerta
	 Exit Function 
EndIf
Gr.Lcl.Premio$ = ASIC.DATOS$("DIGITE CODIGO PREMIO","")											! Captura numero de premio
If Gr.Lcl.Premio$ <> "E" Then Begin																					! Dato capturado
	 Call TSHIECET																														! Tono de alerta
	 Call Visores4690(1,"VALIDANDO PREMIO",Gr.Lcl.Premio$,0,"L")							! Msg de alerta
	 XVldItm% = REVISA.ITEM.PREMIO																						! Validación de premio en tienda
	 If XVldItm% <> -1 Then Exit Function 																		! Premio no definido en tienda
	 Xprm$ = Right$("0000000000000"+Gr.Lcl.Premio$,13)               					! Numero del premio
   GR.CLTE$ = Right$("                  "+Gr.Lcl.Clte$,18)	 			          ! id del cliente
   Xbuff$ = "CC"+Gr.Clte$+Xprm$+Gr.Lcl.UID$                        					! Llave de consulta premio
   TS.TEMP1I4    = 0
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Gr.Lcl.Tmp1$  = Armar.Trama.Msg(Gr.Lcl.Appl$,"15",Xbuff$,               \!
                                "00",Gr.Lcl.Cadena$,"123456")               ! Armar trama MSG
   Gr.Lcl.Tmp2$  = Rutina.Java("com.appl.ApplKernel","threader",Gr.Lcl.Tmp1$)! Ejecuta Requerimiento
   Gr.Lcl.Tmp2$  = "1515121200000REDENCION SATISFACTORIA                 240       610       2206091432340512000001000024  114257802      2000      "
   
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call LEALTAD.AUDITORIA(Gr.Lcl.Tmp1$,Gr.Lcl.Tmp2$, Xsnd$, Xfin$)          ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(Gr.Lcl.Tmp2$)	         															! Valida rta entregada 
   
   If XTEMP4$ = "00" Then Begin 		          															! Si proceso OK
    If Mid$(Gr.Lcl.Tmp2$,12,2) = "00" Then Begin                        		! Encontro Premio
    	 Gr.Lcl.Rdtrx$  = Mid$(Gr.Lcl.Tmp2$, 54,10)														! Puntos redimidos en la trx 
    	 Gr.Lcl.Points$ = Mid$(Gr.Lcl.Tmp2$, 64,10)														! Saldo Puntos cliente
    	 Gr.Lcl.Pntrx$  = Mid$(Gr.Lcl.Tmp2$, 74,30)														! Nro Factura entregado por Leal
    	 Gr.Lcl.Idpos$  = Mid$(Gr.Lcl.Tmp2$,104,15)														! ID de transaccion
    	 Gr.Lcl.VlrBnf% = Val(Mid$(Gr.Lcl.Tmp2$,119,10))											! Valor del beneficio
    	 REDEN.PREMIO.LEAL = -1																								! retorna proceso OK
    	 Exit Function 
    EndIf Else Begin																												! Presenta mensaje falla Leal
    	Call VISOR.AND.BORRAR(Mid$(Gr.Lcl.Tmp2$,14,40))			  		       		  ! Presenta Msg Error
    EndIf
	 EndIf
EndIf Else Begin																														! fin dato premio
	 Call VISOR.AND.BORRAR("PROCESO CANCELADO   POR EL USUARIO.")							! Msg Alerta
EndIf
End Function 																																! Fin redencion puntos


Function ANULREDEN.PREMIO.LEAL																							! Redencion de puntos
Integer*2 ANULREDEN.PREMIO.LEAL, XVldItm%
String Gr.Clte$, Xsnd$, Xfin$, Xbuff$, XTEMP4$, Xprm$
ANULREDEN.PREMIO.LEAL = 0																										! retorna proceso fallido
If Gr.Lcl.CtlPago% = -1 Then Begin
	 Call TSHIECET																														! Tono de alerta
   Gr.Lcl.Premio$ = ASIC.DATOS$("CODIGO PREMIO ANULAR","")									! Captura numero de premio
   If Gr.Lcl.Premio$ <> "E" Then Begin																			! Dato capturado
	 
	 EndIf Else Begin
    Call VISOR.AND.BORRAR("PROCESO CANCELADO   POR EL USUARIO.")						! Msg Alerta
    Exit Function 
   EndIf
   Gr.Lcl.Idpos$ = ASIC.DATOS$("ID TRX PARA ANULAR  ","")									  ! Captura numero de premio
   If Gr.Lcl.Idpos$ <> "E" Then Begin																			  ! Dato capturado
	 
	 EndIf Else Begin
    Call VISOR.AND.BORRAR("PROCESO CANCELADO   POR EL USUARIO.")						! Msg Alerta
    Exit Function 
   EndIf
	 Call Visores4690(1,"ANULANDO  PREMIO",Gr.Lcl.Premio$,0,"L")							! Msg de alerta
   GR.CLTE$ = Right$("                  "+Gr.Lcl.Clte$,18)	 			          ! id del cliente
   Gr.Lcl.Idpos$ = Left$(Gr.Lcl.Idpos$+String$(15," "),15)
   Xbuff$ = "CC"+Gr.Clte$+Xprm$+Gr.Lcl.Idpos$                      					! Llave de consulta premio
   TS.TEMP1I4    = 0
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Gr.Lcl.Tmp1$  = Armar.Trama.Msg(Gr.Lcl.Appl$,"16",Xbuff$,               \!
                                "00",Gr.Lcl.Cadena$,"123456")               ! Armar trama MSG
   Gr.Lcl.Tmp2$  = Rutina.Java("com.appl.ApplKernel","threader",Gr.Lcl.Tmp1$)! Ejecuta Requerimiento
   !Gr.Lcl.Tmp2$  = "1516121200000ANULACION SATISFACTORIA                 "
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call LEALTAD.AUDITORIA(Gr.Lcl.Tmp1$,Gr.Lcl.Tmp2$, Xsnd$, Xfin$)          ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(Gr.Lcl.Tmp2$)	         															! Valida rta entregada 
   
   If XTEMP4$ = "00" Then Begin 		          															! Si proceso OK
    If Mid$(Gr.Lcl.Tmp2$,12,2) = "00" Then Begin                        		! Anulacion Exitosa
    	 Call VISOR.AND.BORRAR(Mid$(Gr.Lcl.Tmp2$,14,40))			  		       	  ! Presenta Msg Error
    	 ANULREDEN.PREMIO.LEAL = -1																						! retorna proceso OK
    	 Exit Function 
    EndIf Else Begin																												! Presenta mensaje falla Leal
    	Call VISOR.AND.BORRAR(Mid$(Gr.Lcl.Tmp2$,14,40))			  		       		  ! Presenta Msg Error
    EndIf
	 EndIf
EndIf Else Begin																														! fin dato premio
	 Call VISOR.AND.BORRAR("NO HAY REDENCION A  SER ANULADA /Borrar")	  			! Msg Alerta
	 ANULREDEN.PREMIO.LEAL = 0																								! retorna proceso fallido
EndIf
End Function 																																! Fin redencion puntos


Sub RedePremioLeal(Xopt%) Public																						! Redencion premios Leal
Integer*4 Xopt%
String    Xtmp1$
!--- EAMTSU07
If Xopt% = 7 Then Begin																											! Carga de opciones
 Gr.Lcl.CtlPago% = 0
 Gr.Lcl.PrmLeal%  = 0 
 Gr.Lcl.Rdtrx$  = ""
 Gr.Lcl.Points$ = ""
 Gr.Lcl.Pntrx$  = ""
 Gr.Lcl.Idpos$  = ""
 Gr.Lcl.VlrBnf% = 0
 If Asc.Proy.Leal% = -1 Then                                               \!
     Call U.IMPRIME("MODULO REDENC. LEAL.   ON 06/Jun/2022",2100H) Else    \! Msg Proyecto Cargado
     Call U.IMPRIME("MODULO REDENC. LEAL.  OFF 06/Jun/2022",2100H)          ! Msg Proyecto Cargado    	
EndIf																																				! Fin Carga de opciones

If Asc.Proy.Leal% <> -1 Then Exit Sub                                       ! Si proyecto apagado
If TS.STANDALONE        Then Exit Sub	                                      ! Si en operacion TOF

!--- EAMTSU02
If Xopt% = 2 Then Begin																											! Inicio de transaccion
	 Gr.Lcl.CtlPago% = 0
	 Gr.Lcl.PrmLeal%  = 0 
 	 Gr.Lcl.Rdtrx$  = ""
 	 Gr.Lcl.Points$ = ""
 	 Gr.Lcl.Pntrx$  = ""
 	 Gr.Lcl.Idpos$  = ""
 	 Gr.Lcl.VlrBnf% = 0
 	 Gr.Lcl.AnlPago% = 0
EndIf																																				! Fin EAMTSU02

!--- EAMTSU08
If Xopt% = 08 Then Begin																										! Validando Item

! If TS.IO.MOTORKEY <> Gr.Lcl.Motor% Then \
! If Gr.Lcl.CtlPago% = -1 And Gr.Lcl.AnlPago% = 0 Then Begin
!    Call VISOR.AND.BORRAR("NO SE PERMITE VENTA DE PRODUCTOS   ")						! Msg Alerta
!    Call VISOR.AND.BORRAR("EN UN PROCEDIMIENTO DE REDENCION   ")						! Msg Alerta
!    IR.INDICAT0 = IR.INDICAT0 OR 04H			                             			! No permite venta Item
! EndIf

EndIf																																				! Fin EAMTSU08

!--- EAMTSU14
If Xopt% = 14 Then Begin																										! En entrada de datos
	
 If TS.IO.MOTORKEY = Gr.Lcl.Motor% Then Begin    														! tecla redencion premios
 	If Not(USER.FBACT.READ) Then Begin																				! Si cliente no capturado
 	   Call VISOR.AND.BORRAR("NO SE HA CAPTURADO  CLIENTE   /Borrar")  				!
     Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 									  					! Init vectores de carga
     TS.IO.MOTORKEY = 73																										!
     Exit Sub 
 	EndIf																																			! Fin cliente no capturado

! 	If TS.INTRX And TS.IO.KEYS(1) <> 70 Then Begin 													! Esta en plena trx de venta
! 	   Call VISOR.AND.BORRAR("NO SE PERMITE PROCE.EN COMPRAS /Borrar") 			!
!     Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 									  					! Init vectores de carga
!     TS.IO.MOTORKEY = 73																										!
!     Exit Sub 
! 	EndIf																																		! fin en trx venta

 	If TS.IO.KEYS(1) <> 70 Then Begin
    TS.TEMP5I2 = REDEN.PREMIO.LEAL																					! Redencion premio Leal
    If TS.TEMP5I2 = -1 Then Begin																						! Aplica redencion premio
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencia
      Gr.Lcl.PrmLeal% = -1																									!
      TS.IO.KEYS(2)  = 80																										!
      TS.IO.DATA$(2) = Gr.Lcl.Premio$																				! SKU a registrar
      TS.IO.DEVICE   = 1																										!
      TS.IO.MOTORKEY = 80																										!
      Exit Sub 
    EndIf Else Begin																										    ! Fin aplica premio
       Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 									  				! Init vectores de carga
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 
    EndIf
  EndIf Else Begin																													! Anulación de redencion
    TS.TEMP5I2 = ANULREDEN.PREMIO.LEAL																			! Anula Redencion premio Leal
    If TS.TEMP5I2 = -1 Then Begin																						! Aplica redencion premio
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencia
      Gr.Lcl.PrmLeal% = -1																									!
      Gr.Lcl.AnlPago% = -1
      TS.IO.KEYS(1)  = 70
      TS.IO.KEYS(2)  = 80																										!
      TS.IO.DATA$(2) = Gr.Lcl.Premio$																				! SKU a registrar
      TS.IO.DEVICE   = 1																										!
      TS.IO.MOTORKEY = 80																										!
      Exit Sub 
    EndIf Else Begin																										    ! Fin aplica premio
       Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 									  				! Init vectores de carga
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 
    EndIf
  	
  EndIf  
 EndIf																																			! Fin redencion premios 
EndIf																																				! Fin EAMTSU14

!--- EAMTSU60.J86
If Xopt% = 20 Then Begin                                                    ! Impresion CR:

 If TS.LINETYPE = 1 And (TS.LINEDATA = 1 Or TS.LINEDATA = 0) Then Begin     !
  If (Gr.Lcl.PrmLeal% = -1) And Gr.Lcl.CtlPago% = 0 Then begin  					  ! Aplica pago premio
     Call Aplica.Pago.Leal(1)
     Xtmp1$ = "PUNTOS REDIMIDOS:"+Gr.Lcl.Rdtrx$
     Xtmp1$ = Left$(Xtmp1$+String$(37," "),37)
     Call U.Imprime(Xtmp1$,6100H)
     Xtmp1$ = "SALDO  PUNTAJE  :"+Gr.Lcl.Points$
     Xtmp1$ = Left$(Xtmp1$+String$(37," "),37)
     Call U.Imprime(Xtmp1$,6100H)
     Xtmp1$ = "FACTURA LEAL    :"+Gr.Lcl.Pntrx$ 
     Xtmp1$ = Left$(Xtmp1$+String$(37," "),37)
     Call U.Imprime(Xtmp1$,2100H)
     Xtmp1$ = "ID TRANSACCION  :"+Gr.Lcl.Idpos$ 
     Xtmp1$ = Left$(Xtmp1$+String$(37," "),37)
     Call U.Imprime(Xtmp1$,6100H)
 	   Gr.Lcl.CtlPago% = -1
  	 TO.MGRKEY = 0
  EndIf
 EndIf

 If TS.LINETYPE = 1 And (TS.LINEDATA = 1 Or TS.LINEDATA = 0) Then Begin     !
  If (Gr.Lcl.PrmLeal% = -1 And Gr.Lcl.AnlPago% = -1) Then Begin  					  ! Aplica pago premio
     Call Aplica.Pago.Leal(0)
	   Gr.Lcl.CtlPago% = 0
	   Gr.Lcl.PrmLeal%  = 0 
 	   Gr.Lcl.Rdtrx$  = ""
 	   Gr.Lcl.Points$ = ""
 	   Gr.Lcl.Pntrx$  = ""
 	   Gr.Lcl.Idpos$  = ""
 	   Gr.Lcl.VlrBnf% = 0
 	   Gr.Lcl.AnlPago% = 0
  	 TO.MGRKEY = 0
  EndIf
 EndIf


EndIf		

End Sub 																																		! Fin redencion premios
