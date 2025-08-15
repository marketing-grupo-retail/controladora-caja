!**************************************************
!Empresa       : Grupo Retail Ltda - Colombia     *
!Programa      : GRMDEUNA.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Forma Pago DeUna          *
!                integracion banco pichincha      *
!**************************************************
! Modificaciones:
! Mod 27May2025
! Aplicación descuentos por pago tarjeta deuna
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 16Jul2025
! Se modifica manejo integracion por un servicio 
! WebHook desarrollado por Santa Maria.
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%ENVIRON T

String    Global Gr.DeUna.Tvp$,                                            \! Tipo y variedad de pago 
                 Gr.DeUna.Pin$,                                            \! Tipo y variedad de pago 
                 Gr.DeUna.PinDeUna$,                                       \! Pin reportado al cliente
                 Gr.DeUna.Motor$,                                          \! Tecla motora
                 Gr.DeUna.Tmp1$,                                           \! Temporal 1
                 Gr.DeUna.Tmp2$,                                           \! Temporal 2
                 Gr.DeUna.Tmp3$,                                           \! Temporal 3
                 Gr.DeUna.Tmp4$,                                           \! Temporal 4
                 Gr.DeUna.AnulPin$,                                        \! Pin anular
                 Gr.DeUna.PtVta$                                            ! numero de caja asignado por deuna
String    Global Gr.DeUna.Pagos$(2),                                       \! Vector pagos capturados
                 Gr.Deuna.Minimo$,                                         \! Valor compra minima descuento
                 Gr.DeUna.Ptg$                                              ! Porcentaje descuento aplicar

Integer*1 Global Gr.DeUna.Act%,																					   \! Control proyecto
                 Gr.DeUna.AplAn%,                                          \! Anulacion pago Kupi
                 Gr.DeUna.Apl%,                                            \! Pago Bono Aplicado
                 Asic.Detalle%,                                            \!
                 Asc.Pay.Impr%                                              ! Control nro trx 
Integer*2 Global GR.BKNEGCNT(1),                                           \!
                 Gr.DeUna.Numtrx%                                           ! Nro de pagos recibidos
Integer*4 Global Gr.DeUna.Motora%,                                         \! Tecla procedimiento
                 Gr.DeUna.TVPOS%,                                          \!
                 Gr.DeUna.MaxTv%,                                          \! Maximo pago   tipo y variedad
                 Gr.DeUna.ChgTv%,                                          \! Maximo cambio tipo y variedad
                 Gr.BKNEGAMT(1),                                           \!
                 Gr.DeUna.Dscto%,                                          \! Dscto pago deuna 
                 Asc.Tmp.Apun%                                              ! Apuntador String
Integer*4 Global GR.TOTAL.DSC%																						  ! Total descuentos 
                 
%INCLUDE EAMTOPTS.J86          																			        ! working storage
%INCLUDE EAMTSWKG.J86                                              					! working storage                 
%INCLUDE EAMTRANS.j86                                              					!
%INCLUDE JAVAGUIV.J86          																							! Variables java
%INCLUDE EAMTERMS.J86                                              					!
%INCLUDE EAMTSXHC.J86                                              					! Rutinas de Assembler            
%INCLUDE EAMASMRT.J86                                              					! Rutinas de Assembler            
                                                                                                              
%INCLUDE RECATSSU.011          					                                    ! RUTINAS GENERICAS APLICACION    
%INCLUDE JAVAGUIC.J86																												! RUTINAS AMBIENTE GRAFICO

Sub  TSCSEC03 External
End Sub 

Sub TSBDEC01 EXTERNAL          ! balance due (normal/foodstamp)
End Sub                        !

Sub TSHIECET external					 ! Tono de alerta
End Sub

Sub TSPREC01 EXTERNAL     		 ! print routine
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

Function GrEnter( msg1$, msg2$ )
 String  msg1$, msg2$, t$, s$
 String ADATA$, D$, R$, x$
 Integer*2 cod
 Integer*1 f%, GrEnter

  !call guardaDisplay
   TS.LINETYPE = 12  !DISPLAY BUFFERS AS PASSED (TERMINAL MONITOR)
   s$ = String$( 20, " " )
   TS.TEMP1$ = Left$( msg1$ + s$, 20 )
   TS.TEMP2$ = Left$( msg2$ + s$, 20 )

   ! Limpia IOPROCE
   Lockdev 32, purge

   t$ = ""
   f% = -1
   While (f% = -1)
      Call visores4690(1,TS.TEMP1$,TS.TEMP2$,0,"L")
      Unlockdev 32, 99
      Wait 32; 1000
      If Event% = 32 Then Begin
         ! Lectura Teclado y Status
         TS.ER.RETURN = -1
         Read #32; ADATA$, D$, D$, D$, D$, D$, D$, D$, D$, D$, D$
         If TS.ER.RETURN = -1 Then Begin
            x$ = Mid$( ADATA$, 13, 1 )
            If x$ = "" Then cod = 80 Else cod = asc( x$ )
            If cod = 80 Then f% = 1
            If cod = 73 Then f% = 0
         EndIf Else Begin
            f% = 0
         EndIf 
      EndIf 
   Wend
   lockdev 32, purge
   GrEnter = f%
   !call restauraDisplay

End Function  

Sub DEUNA.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::GRLOG:DU" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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


Sub DUNA.BUSCA.LIMITE.TV(UE.TIPO.VAR$)
  Integer*2 UE.I%
  Integer*1 UE.FOUND%
  String UE.TIPO.VAR$
  UE.I% = 1
  UE.FOUND% = 0 
  Gr.DeUna.MaxTv% = 0
  Gr.DeUna.ChgTv% = 0 
  While UE.I% <= TO.NUMTNDR AND NOT UE.FOUND%
    If TO.TENDOPTS(UE.I%,0) = VAL(LEFT$(UE.TIPO.VAR$,1)) AND  \
       TO.TENDOPTS(UE.I%,1) = VAL(RIGHT$(UE.TIPO.VAR$,1)) Then Begin 
       Gr.DeUna.MaxTv% = TO.TENDLIMITS(UE.I%,0)
       Gr.DeUna.ChgTv% = TO.TENDLIMITS(UE.I%,1)
       UE.FOUND%  = -1
       Gr.DeUna.TVPOS% = UE.I%
    EndIf Else UE.I% = UE.I% + 1
  Wend    
  UE.I% = 0
  For UE.I% = 0 To 7
      GR.BKNEGCNT(UE.I%) = To.NEGCNT(UE.I%)    
      GR.BKNEGAMT(UE.I%) = To.NEGAMT(UE.I%)    
  Next UE.I%
End Sub

Sub Validando.PagoDeUna
String Xsnd$, Xfin$, Xbuffer$, XTemp4$, Xtrama$, Xfunc$, d$, Xtmp$,        \!
       XtrxAnul$	
Integer*1 XT%, Xfnd%, Xrpt%, Xnvc%
Integer*4 Xvlr%

     If Gr.DeUna.AplAn% = 0 Then Xfunc$ = "11" Else Xfunc$ = "13"           ! 
     Xrpt% = 5 : Xnvc% = 0
     
     Write #34; "PROCESA PAGO "+Gr.DeUna.Tmp3$+ chr$(10)
     
     Call VISORES4690(1,"PROCESA PAGO DEUNA","ESPERE POR FAVOR ...",0,"L")	! Msg Operador
     Asc.Pay.Impr% = 0																											! Aumenta contador de trx
     Xsnd$ = DATE$ +":"+ Time$                                              ! Fecha y hora del requerimiento
     If Xfunc$ = "11" Then Begin
        Xbuffer$ = Right$("          "+Gr.DeUna.PtVta$,10)     +           \! Punto de Venta
                   Right$("           "+Gr.DeUna.Tmp3$,11)     +           \! Valor del pago 
                   Left$("PROCESA PAGO "+String$(50," "),50)                ! Descripcion Compra
     EndIf Else Begin
     	  XtrxAnul$ = Gr.DeUna.AnulPin$
        Xbuffer$ = Right$("                  "+XtrxAnul$,18)                ! Nro Autorizacion Anular
     EndIf  
     XTemp4$ = Armar.Trama.Msg("44",Xfunc$,Xbuffer$,"00","0001","123456")   ! Armar trama MSG
     XTrama$ = Rutina.Java("com.grpretail.mega.ecuador.sigui.actions.Core","execute", XTemp4$)
     
     Write #34; "Rta: "+Xtrama$+ chr$(10)
     
     !Xtrama$ = "4411105200000PAGO COLOCADO    SATISFACTORIAMENTE     "
     !Xtrama$ = Xtrama$ + "00283003425E2846789b7d7daa5-6385-4158-8390-a384b4f7adfa    "
     
     Xfin$ = DATE$ +":"+ Time$                                              ! Fecha y hora rta del requerimiento
     Call DEUNA.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                    ! Rastreo movimiento
     
     Xtemp4$ = Mid$(XTrama$,12,2)
     
     Write #34; "Estado: "+Xtemp4$+ chr$(10)
     
     If Xtemp4$ <> "00" Then Begin 																					! Si falla en proceso comunicacion
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
  	  TS.TEMP1I1 = 0																											  ! Reporta falla en el proceso      
   	  Exit Sub 																															!
     EndIf																																	!
   
    
     If Xfunc$ = "03" Then Begin																						! Proceso anulacion
     	  Gr.DeUna.PinDeUna$ = Mid$(Xtrama$,55,40)														! Trama autorizacion anulacion
        GoTo Deuna.print																										! Imprime tkt
     EndIf
     Gr.DeUna.Pin$ = Mid$(Xtrama$,54,19)                                    ! Nro ID trx Deuna

     PROCESA.DEUNA:
     Call TSHIECET
     Call visores4690(1,"SOLICITE ESCANEO QR ","AL CLIENTE PARA PAGO",0,"L")
     Wait ; 15000     
     Asc.Pay.Impr% = 0																											! Aumenta contador de trx
     Xsnd$ = DATE$ +":"+ Time$                                              ! Fecha y hora del requerimiento
     Xbuffer$ = Left$(Gr.DeUna.Pin$+STRING$(40," "),40) + "1"               ! Mensaje
     XTemp4$ = Armar.Trama.Msg("44","12",Xbuffer$,"00","0001","123456")     ! Armar trama MSG
     XTrama$ = Rutina.Java("com.grpretail.mega.ecuador.sigui.actions.Core","execute", XTemp4$)
     
     !Xtrama$ = "4402125200000PAGO EXISTE EN PLATAFORMA DEUNA         "
     !Xtrama$ = Xtrama$ + "APPROVED  2          00283003425E2846789                     32538501          "     
     
     Xfin$ = DATE$ +":"+ Time$                                              ! Fecha y hora rta del requerimiento
     Call DEUNA.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                    ! Rastreo movimiento
     !XTEMP4$ = Valida.Rta(XTrama$)																		      ! Valida rta entregada

     XTEMP4$ = Mid$(XTrama$,12,2)																						! Respuesta del procedimiento

     If Xtemp4$ <> "00" Then Begin 																					! Si falla en proceso comunicacion
     	  Xnvc% = Xnvc% + 1
     	  If Xnvc% <= Xrpt% Then Begin
     	    Call TSHIECET
          Call visores4690(1,"NO SE RECIBE AUTOR. ","REINTENTANDO "+STR$(Xnvc%)+"/"+Str$(Xrpt%),0,"L")
          Wait ; 2000 
          GoTo PROCESA.DEUNA
        EndIf Else Begin
          TS.TEMP1I1 = 0 																										  ! Reporta proceso fallido
          Gr.DeUna.Apl% = 0 																									! Pago aplicado
          Gr.DeUna.AplAn% = 0
          Exit Sub 
        EndIf
     EndIf																																	!	Fin falla proceso comunicacion

     XTEMP4$ = Mid$(XTrama$,12,2)	         																  ! Valida rta entregada
     
     If XTemp4$ <> "00" Then Begin 																				  ! Rta No Satisfactoria
     	  Xnvc% = Xnvc% + 1
     	  If Xnvc% <= Xrpt% Then Begin
     	    Call TSHIECET
          Call visores4690(1,"NO SE RECIBE AUTOR. ","REINTENTANDO "+STR$(Xnvc%)+"/"+Str$(Xrpt%),0,"L")
          Wait ; 2500 
          GoTo PROCESA.DEUNA
        EndIf Else Begin
          Call Visor.And.Borrar("NO SE RECIBIO RESPUESTA AUTOR./Borrar")
          jGuiSubState = 80501
          Call javaEvent(terminalSubStateMsg)
          Xtmp$ = "0"  ! Cancela proceso de reintentos
          
     	    !Xtmp$ = Asic.Datos$("REINTENTA CONSULTA?."," 1./SI 0./NO  ")
     	         	    
     	    !XT% = GrEnter("REINTENTA CONSULTA?.","SI/Intro NO/Borrar  ")
          If Val(Xtmp$) = 0 Then Begin
             TS.TEMP1I1 = 0 																										  ! Reporta proceso fallido
             Gr.DeUna.Apl% = 0 																									! Pago aplicado
             Gr.DeUna.AplAn% = 0
             Exit Sub 
          EndIf Else Begin
          	Xnvc% = 1
          	GoTo PROCESA.DEUNA
          EndIF
        EndIf
     EndIf																																	!
     
     Call Visor.And.Borrar(Mid$(XTrama$,54,10)+" /Borrar")                  !
     Gr.DeUna.PinDeUna$ = Mid$(XTrama$,115,18)                              ! Rta transaccion 

     
     Deuna.print:
     If Xfunc$ = "11" Then Xtmp$ = "PROCESO OPERACION DEUNA! VENTA" Else   \!
     	  Xtmp$ = "PROCESO OPERACION DEUNA! ANULACION"
     Write #34 ; XTMP$ + CHR$(10)
     Write #34 ; "AUT: "+Gr.DeUna.PinDeUna$ + CHR$(10)
     XVLR% = Val(Gr.DeUna.Tmp3$)
     TS.TEMP1$ = formateaMonto(Xvlr%)
     Write #34 ; "VALOR: "+TS.TEMP1$ + CHR$(10)
     XT% = 0
     For XT% = 1 To 9
       Write #34 ; " " + CHR$(10)
     Next XT%
     Call U.CORTACR																													! Guillotina papel
     If Xfunc$ = "11" Then Begin																						! Pago autorizado
      Gr.DeUna.Numtrx% = Gr.DeUna.Numtrx% + 1																! Aumenta contador
      Gr.DeUna.Pagos$(Gr.DeUna.Numtrx%,0) = Gr.DeUna.PinDeUna$              ! Nro de autorizacion
      Gr.DeUna.Pagos$(Gr.DeUna.Numtrx%,1) = Gr.DeUna.Tmp3$                  ! Valor operacion
      Gr.DeUna.Pagos$(Gr.DeUna.Numtrx%,2) = Xfunc$                  			 	! Operacion 
     EndIf
     TS.TEMP1I1 = -1																										    ! Reporta proceso exitoso
     Gr.DeUna.Apl% = -1																										  ! Pago aplicado
End Sub 																																		! Fin validación bonos 

Sub CARGA.PTOVENTA.DUNA
  Gr.DeUna.PtVta$ = "XXXXXXXXXX"                                            ! Init variable
  TS.ER.RETURN = -1 																												! Control errores
  Open "R::TF:VTAUNA" As 94 UNLOCKED NOWRITE NODEL 												  ! Apertura archivo parametros ptovta 
  If TS.ER.RETURN <> -1 Then Begin                                          ! Error apertura
  	 Gr.DeUna.Act% = 0 																									    ! Init proyecto inactivo
  	 Call VISOR.AND.BORRAR("ERROR PARAMETROSPAGO DEUNA!  ")                 !
     Exit Sub 
  EndIf
  If End #94 Then UE.FIN.PTVENTA       																	    ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
     Read #94; TS.TEMP1$, TS.TEMP2$																		      ! Lectura registro
     If Val(TS.TEMP1$) = Val(TS.TERMINAL$) Then Begin
     	  Gr.DeUna.PtVta$ = TS.TEMP2$
        GoTo UE.FIN.PTVENTA
     EndIf
  Wend   
  UE.FIN.PTVENTA:
   Close 94
End Sub 

Sub RECUPERA.TRX.DEUNA
String Xtmp$
Integer*2 XS%
 If Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "99" Then BEGIN   	   	              ! Si user data
  If Unpack$(MID$(SL.STR.ENTRY$,3,4)) = "1013" Then Begin                   ! Proyecto Deuna
    Asc.Tmp.Apun% = 3																												!
    Gr.DeUna.Numtrx% = Gr.DeUna.Numtrx% + 1																	! Total operaciones Deuna
    Xtmp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                   		! Numero de proyecto
    Xtmp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)           						! Punto de venta
    Xtmp$ = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)           						! Autorizacion
    Gr.DeUna.Pagos$(Gr.DeUna.Numtrx%,0) = Xtmp$                         		! Nro de autorizacion
    Xtmp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)           						! Valor compra
    Gr.DeUna.Pagos$(Gr.DeUna.Numtrx%,1) = Xtmp$                         		! Nro de autorizacion
    Xtmp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)           						! Signo operacion 
    Gr.DeUna.Pagos$(Gr.DeUna.Numtrx%,2) = Xtmp$                         		! Nro de autorizacion
    Gr.DeUna.Apl%   = -1																										! Pagos aplicados
    If Xtmp$ = "11" Then                                                   \!
     	  Xtmp$ = "PROCESO OPERACION DEUNA! VENTA" Else                      \!
     	  Xtmp$ = "PROCESO OPERACION DEUNA! ANULACION"												!
    Write #34 ; Xtmp$ + CHR$(10)																						!
    Write #34 ; "AUT: "+Gr.DeUna.Pagos$(Gr.DeUna.Numtrx%,0) + CHR$(10)			!
    TS.TEMP2I4 = Val(Gr.DeUna.Pagos$(Gr.DeUna.Numtrx%,1))										!
    Xtmp$ = formateaMonto(TS.TEMP2I4)																				!
    Write #34 ; "VALOR: "+Xtmp$ + CHR$(10)																	!
    XS% = 0																																	!
    For XS% = 1 To 9																												!
      Write #34 ; " " + CHR$(10)																						!
    Next XS%																																!
    Call U.CORTACR																													! Guillotina papel

  EndIf																																			! 
 EndIf 

End Sub 

Sub VouchDsctoDeuna
Integer*2 XI%
String FBDHDW$, CPI17$, X$, Y$
   
  If Gr.DeUna.Dscto% <>  0 Then Begin																				! Genera Voucher
  	 FBDHDW$ = CHR$(1BH)+CHR$(21H)+CHR$(31H)
  	 CPI17$  = CHR$(1BH)+CHR$(20H)+CHR$(02H)
  	 X$ = CHR$(1BH)+CHR$(21H)+CHR$(00H)
  	 Y$ = CHR$(1BH)+CHR$(20H)+CHR$(05H)
  	 Write #34 ; FBDHDW$ + CPI17$ + "***DEUNA***" + chr$(10)
  	 Write #34 ; X$+Y$+"Compra    :"+Str$(TS.TOTALS(0,0,0))+ chr$(10)
  	 Write #34 ; X$+Y$+"Descuento :"+Str$(Gr.DeUna.Dscto%) + chr$(10)
   	 Asic.Detalle% = 1																											!
     TS.TEMP1$ = Linea.Detalle(1)																						!
     TS.TEMP1$ = Left$(TS.TEMP1$ + STRING$(38," "),38)          			      ! Ajuste impresion
     Write #34 ; TS.TEMP1$ + (CHR$(10))     
     TS.TEMP1$ = String$(19,"- ")
     TS.TEMP1$ = Left$(TS.TEMP1$ + STRING$(38," "),38)          			      ! Ajuste impresion
     Write #34 ; TS.TEMP1$ + (CHR$(10))     
     For XI% = 1 To 9
       Write #34 ; " " + (CHR$(10))     
     Next XI%
     Call U.CORTACR       																									!
     TS.LINETYPE = 18																												! Genera cabecera 
     TS.LINEDATA = 1																												! en proximo tiquete
     Call TSPREC01																													!

  EndIf																																			!
End Sub 

Sub PAGO.DEUNA(Xopt%) Public																					  		! Modulo Pago De Una
Integer*4 Xopt%, Xj%
Integer*1 Xm%, Xfnd%
Real      Xptg%

!--- EAMTSU07.J86
If XOPT% = 07 Then Begin																										! Carga de opciones
  Gr.DeUna.Act% = 0 																										    ! Init proyecto activo
  Dim GR.BKNEGCNT(8)
  Dim GR.BKNEGAMT(8)
  Gr.DeUna.Numtrx% = 0																											! Nro de pagos
  Dim Gr.DeUna.Pagos$(20,2)																								  ! 20 pagos 
  TS.TS11WERR$ = ""																													! Control errores
  Open "R::$ARGENER" As 94 UNLOCKED NOWRITE NODEL 												  ! Apertura archivo parametrizacion para TCxSky
  If TS.TS11WERR$ <> "" Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR PARAMETROSPAGO DEUNA!  ")                 !
	   Gr.DeUna.Act% = 0																										  ! Init proyecto activo  	 
  	 Exit Sub																																!
  EndIf 
  If End #94 Then UE.FIN.DEUNA         																	    ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[PAGO DEUNA]" Then Begin		 	  		        				! Parametros pago deuna
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.DeUna.Act% = Val(Mid$(TS.TEMP1$,30,2))      			    					! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.DeUna.Tvp$ = Mid$(TS.TEMP1$,30,2)           			    					! Tipo y variedad de pago 
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.DeUna.Motor$ = Mid$(TS.TEMP1$,30,3)           			    			  ! Tecla motora
         Read #94; TS.TEMP1$																								! Lectura registro
         Gr.Deuna.Minimo$ = Mid$(TS.TEMP1$,30,9)                            ! Valor compra minima descuento
         Read #94; TS.TEMP1$																								! Lectura registro
         Gr.DeUna.Ptg$ = Mid$(TS.TEMP1$,30,4)                               ! Porcentaje descuento aplicar
         
         Call DUNA.BUSCA.LIMITE.TV(Gr.DeUna.Tvp$)														! Ajusta controles pago
         GoTo UE.FIN.DEUNA                                         					! Sale del proceso
        EndIf																																!
  Wend 																																		  ! Fin recorrido archivo
  UE.FIN.DEUNA: 
    Close 94 
    Call CARGA.PTOVENTA.DUNA
    If Gr.DeUna.Act% = -1 Then                                             \! Proyecto Activo
      Call U.IMPRIME("MODULO PAGO DEUNA      ON 16/Jul/2025",2100H) Else   \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO PAGO DEUNA     OFF 16/Jul/2025",2100H)         ! Msg Proyecto Cargado
EndIf

If Gr.DeUna.Act% = 0 Then Exit Sub 																				  ! Si proyecto apagado

!--- EAMTSU02.J86
If XOPT% = 02 Then Begin																										! Al inicio de una trx
   Gr.DeUna.PinDeUna$ = ""
   Gr.DeUna.Pin$  = ""
   Gr.DeUna.Tmp1$ = ""
   Gr.DeUna.Tmp2$ = ""
   Gr.DeUna.Tmp3$ = ""
   Gr.DeUna.AplAn% = 0
   Gr.DeUna.Apl%   = 0
   Gr.DeUna.AnulPin$ = ""
   To.TENDLIMITS(Gr.DeUna.TVPOS%,0) = Gr.DeUna.MaxTv%												! Retorna valores default
   To.TENDLIMITS(Gr.DeUna.TVPOS%,1) = Gr.DeUna.ChgTv%												! Retorna valores default
   TS.TEMP1I1 = 0
   For TS.TEMP1I1 = 0 To 7
       TO.NEGAMT(TS.TEMP1I1) = GR.BKNEGAMT(TS.TEMP1I1)
       TO.NEGCNT(TS.TEMP1I1) = GR.BKNEGCNT(TS.TEMP1I1)
   Next TS.TEMP1I1
   Dim Gr.DeUna.Pagos$(20,2)																								! 20 pagos 
   Gr.DeUna.Numtrx% = 0																											! Nro de pagos
   Gr.DeUna.Dscto% = 0																											!
EndIf																																				! Fin eamtsu02

!--- EAMTSU14.J86
If XOPT% = 14 Then Begin																										! En entrada de datos

 If TS.IO.MOTORKEY = Val(Gr.DeUna.Motor$) Then Begin												!
 	  If Gr.DeUna.Numtrx% >= 20 Then Begin
 	     Call VISOR.AND.BORRAR("NO SE PERMITEN MAS  OPERACIONES DEUNA!")      !
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      !
       TS.IO.MOTORKEY = 73																						      !
       Exit Sub 																											      !
 	  EndIf 
    If Not TS.BAL.TAKEN Then Begin
       TS.GUIDANCE = 1020
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      !
       TS.IO.MOTORKEY = 73																						      !
       Exit Sub
    EndIf

    Gr.DeUna.AplAn% = 0                         														! Init Variable
    If TS.IO.KEYS(1) = 70 Then Gr.DeUna.AplAn% = -1                         ! Anulacion pago
    If Gr.DeUna.AplAn% = 0 Then Begin                                       ! Es una compra
      !jGuiSubState = 80538																									! Captura dato Format valor
      !jGuiSubState = 80501																									! Captura dato numerico
     If java.init = -1 Then begin																					  ! Si ambiente gráfico
         jGuiSubState = 80538																							  ! Valor a pagar
         Call javaEvent(terminalSubStateMsg)																! numerico
     EndIf
     Gr.DeUna.Dscto% = 0
     Gr.DeUna.Tmp3$ = ASIC.DATOS$("INGRESE VALOR A   ","COBRAR DEUNA /Borrar")
     If Gr.DeUna.Tmp3$ = "E" Then BEGIN														          ! Proceso cancelado
 	       Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								    !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Exit Sub 																											    !
     EndIf 																													        !
     If Val(Gr.DeUna.Tmp3$) > TS.BALDUE(0) Then Begin												! Si valor superior a total compra
        Call VISOR.AND.BORRAR("PAGO SUPERIOR MONTO COMPRA, RECHAZADO ...")	!
        Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
        TS.IO.MOTORKEY = 73																						      !
        Exit Sub 																											      !
     EndIf
!-- Lineas adicionadas para manejo descuento pago total deuna
   
    If TS.TOTALS(0,0,0) >= Val(Gr.Deuna.Minimo$) Then        							 \! Cumple vlr minimo compra  
     If Val(Gr.DeUna.Tmp3$) >= TS.BALDUE(0) Then Begin                      ! Calculando dscto deuna
     	  Xptg% = Val(Gr.DeUna.Ptg$) / 100 																		! Dato ptg a calcular
  	  	Gr.DeUna.Tmp3$ = Str$(TS.BALDUE(0))                                 ! Total pendiente de pago 
     	  TS.TEMP5I4 = (Val(Gr.DeUna.Tmp3$) /100) * Xptg%                     ! Valor descuento
     	  Gr.DeUna.Dscto% = TS.TEMP5I4																				! Valor dscto aplicado
     	  Gr.DeUna.Tmp3$ = Str$( Val(Gr.DeUna.Tmp3$) - TS.TEMP5I4 )           ! Nuevo valor pago
     	  TS.TEMP1$ = formateaMonto(Val(Gr.DeUna.Tmp3$))											! Formateo valor
        TS.TEMP3$ = Left$("PAGO:"+TS.TEMP1$+String$(20," "),20)							!
        TS.TEMP1$ = formateaMonto(Gr.DeUna.Dscto%)													!
        TS.TEMP3$ = TS.TEMP3$ +"Dscto:"+TS.TEMP1$														!
     	  Call VISOR.AND.BORRAR(TS.TEMP3$)																		! Presentacion beneficio
     EndIf

    EndIf Else Begin
      If java.init = -1 Then begin																				  ! Si ambiente gráfico
         jGuiSubState = 80501																							  ! teclado numerico
         Call javaEvent(terminalSubStateMsg)																! numerico
      EndIf
  	  Gr.DeUna.AnulPin$ = ASIC.DATOS$("INGRESE NRO AUTORIZ.","ANULAR DEUNA    ")
      If Gr.DeUna.AnulPin$ = "E" Then BEGIN														        ! Proceso cancelado
 	       Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								    !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Exit Sub 																											    !
      EndIf 																													      !
      Xfnd% = 0
      For Xm% = 1 To Gr.DeUna.Numtrx%                                       ! Valida numero aprovacion
          If Gr.DeUna.Pagos$(Xm%,0) = Gr.DeUna.AnulPin$ Then Begin					! Encontrado
           	  Xfnd% = -1
           	  Gr.DeUna.Tmp3$ = Gr.DeUna.Pagos$(Xm%,1)                       ! Aisgna valor pagado
          EndIf
      Next Xm% 																														  !
      If Xfnd% = 0 Then Begin
 	     	  Call VISOR.AND.BORRAR("NRO DE AUTORIZACION NO VALIDO   /Borra")   ! Msg operador
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Gr.DeUna.AnulPin$ = ""
         Gr.DeUna.AplAn% = 0
         Exit Sub 																											    !
      Endif
    EndIf

    If Val(Gr.DeUna.Tmp3$) <= 0 Then Begin
 	     Call VISOR.AND.BORRAR("VALOR INGRESADO NO VALIDO")			              !
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      !
       TS.IO.MOTORKEY = 73																						      !
       Exit Sub 																											      !
	  EndIf
         TS.BD.DSPPARM = -1                     														! SET FOR NORMAL BD
         Call TSBDEC01                          														! FIRST TAKE BAL DUE
         TS.IO.STATE = 10
         Dim TS.IO.PREV.KEYS(10)
         Dim TS.IO.PREV.DATA$(10)
         TS.IO.PREV.MOTORKEY = 0
         Dim Ts.Io.Keys(10)																									! Inicializa Vectores
         Dim Ts.Io.Data$(10)																								! 
         If Gr.DeUna.AplAn% = -1 Then TS.IO.KEYS(1) = 70                   	! Anulacion pago
         TS.IO.DATA$(3) = Right$(Gr.DeUna.Tvp$,1)              		        	! Variedad de Pago
         TS.IO.KEYS(3)  = 78                                     		        ! Tecla Slash
         TS.IO.DATA$(7) = Gr.DeUna.Tmp3$ 													        	! Valor a entregar
         TS.IO.DATA$(9) = Gr.DeUna.PtVta$														        ! Id del cliente
         TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.DeUna.Tvp$,1))   		          	! Tipo de Pago
         TS.IO.MOTORKEY = TS.IO.KEYS(7)															        !
         To.TENDLIMITS(Gr.DeUna.TVPOS%,0) = 9999999   										  ! Modifica controles entrega efectivo
         To.TENDLIMITS(Gr.DeUna.TVPOS%,1) = 9999999									        ! Modifica controles entrega efectivo
         TS.TEMP1I1 = 0
         For TS.TEMP1I1 = 0 To 7
             TO.NEGAMT(TS.TEMP1I1) = 9999999
             TO.NEGCNT(TS.TEMP1I1) = 999
         Next TS.TEMP1I1           

         Exit Sub 
 EndIf																																			!

EndIf																																				! Fin Eamtsu14

!--- EAMTSU20.J86
If XOPT% = 20 Then Begin																										! En impresion de tiquete

EndIf 																																			! Fin Eamtsu02

!--- EAMTSU32.J86
If XOPT% = 32 Then Begin																										! Validacion forma de pago
 If (TS.IO.DATA$(3) = Right$(Gr.DeUna.Tvp$,1)) And                         \!
    (TS.IO.KEYS(7)  = Val("9"+Left$(Gr.DeUna.Tvp$,1))) Then Begin           ! Si pago bono kupi
 	  TS.TEMP1I1 = 0																													! Control de errores
    Call Validando.PagoDeUna   																							! Validando forma de pago
   	If TS.TEMP1I1 = 0 Then Begin 																		        ! Si falla el proceso
       TS.USER.RETURN = 99																									!
       TS.LINEDATA = 51                 																    ! "TARJETA INVALIDA"            
       TS.STACKERR(0) = 0                                                   ! manager's override required   
       TS.STACKERR(3) = 0                                                   ! item descriptor               
       TS.STACKERR(6) = 0                                                   ! put "M" on last display       
       TS.STACKERR(7) = -1                                                  ! indicate no printing required 
       TS.MO.REASON = 0                                                     ! invalid key with department   
       Call TSCSEC03       																							    !                               
       TS.IO.MOTORKEY = 0 																									!
       Gr.DeUna.PinDeUna$ = ""
       Gr.DeUna.Pin$  = ""
       Gr.DeUna.Tmp1$ = ""
       Gr.DeUna.Tmp2$ = ""
       Gr.DeUna.Tmp3$ = ""
       Gr.DeUna.AplAn% = 0
       Gr.DeUna.Apl%   = 0
       Gr.DeUna.Dscto% = 0
       Exit Sub                                                             !
   	EndIf Else Begin																							          !
   		 TS.TOTALS(0,0,0) = TS.TOTALS(0,0,0) - (Gr.DeUna.Dscto%)
   		 TS.TEMP2$ = "00"																											! Default venta
   		 If TS.IO.KEYS(1) = 70 Then TS.TEMP2$ = "01"                          ! Si una anulacion
   		 TS.IO.DATA$(9) = Gr.DeUna.PinDeUna$											        	  ! Pin de trx 
       TS.TEMP1$ = Gr.DeUna.PtVta$        + ":" +                          \! Pin punto de venta
                   Gr.DeUna.PinDeUna$     + ":" +                          \! String autorizacion
                   TS.IO.DATA$(7)  + ":" +                                 \! Valor compra
                   TS.TEMP2$                                                ! Signo Operacion  00.Pago 01. Anulacion
      Call Grabacion.Cadena.Usuario("1013",TS.TEMP1$)                       ! Almacena UD operaciones DEUNA
      Gr.DeUna.Apl%   = -1																									! Pago aplicado
    EndIf																																		!
 EndIf																																			! fin pago
EndIf																																				! Fin Eamtsu32

!--- EAMTSU53.J86
If XOPT% = 53 Then Begin																										! Recuperación de trx
   Call RECUPERA.TRX.DEUNA																									! Valida si operaciones deuna
EndIf																																				! Fin EAMTSU53

End Sub																																			! Fin Modulo control cambio precio
