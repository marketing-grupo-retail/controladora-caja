!**************************************************
!Empresa       : Grupo Retail Ltda - Colombia     *
!Programa      : GRMBNSYD.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Control Bonos premio      *
!                Supermercados y Droguerías       *
!**************************************************
! Modificaciones:
! Ajuste codigo de barras
! 78500944867590000005000
! Prefijo    3  caracteres 785 o 786
! Referencia 10 caracteres
! Valor      10 cararecres
!-------------------------------------------------

%ENVIRON T

Integer*1 Global Gr.Bnsyd.Act%,																					   \! Control proyecto
                 Gr.Bnsyd.Apl%,                                            \! Pago Bono Aplicado
                 Asc.Pay.Impr%                                              ! Control nro trx 
Integer*2 Global GR.SDNEGCNT(1)                                             !
Integer*4 Global Gr.Bnsyd.TVPOS%,                                          \!
                 Gr.Bnsyd.MaxTv%,                                          \! Maximo pago   tipo y variedad
                 Gr.Bnsyd.ChgTv%,                                          \! Maximo cambio tipo y variedad
                 Gr.SDNEGAMT(1),                                           \!
                 Asc.Tmp.Apun%        																			! Apuntador Lectura Strings                 
                 
String    Global Gr.Lcl.Clte$,																						 \! ID cliente
                 Gr.Bnsyd.BnEan$,                                          \! EAN capturado
                 Gr.Bnsyd.BnSup$,                                          \! Prefijo bono supermercado
                 Gr.Bnsyd.BnPag$,                                          \! Tipo y variedad de pago  
                 Gr.Bnsyd.BnDrg$                                            ! Prefijo bono droguería
                 
%INCLUDE ADX_UPGM:EAMTOPTS.J86 																			        ! working storage
%INCLUDE ADX_UPGM:EAMTSEVA.J86   																					  ! Variables EMS
%INCLUDE ADX_UPGM:EAMTSWKG.J86                                              ! working storage                 
%INCLUDE ADX_UPGM:EAMTRANS.j86                                              !                                 
%INCLUDE ADX_UPGM:EAMTERMS.J86                                              ! 
%INCLUDE ADX_UPGM:EAMTSXHC.J86                                              ! Rutinas de Assembler            
%INCLUDE ADX_UPGM:EAMASMRT.J86                                              ! Rutinas de Assembler            
                                                                                                              
%INCLUDE RECATSSU.011          					                                    ! RUTINAS GENERICAS APLICACION    

Sub  TSCSEC03 External
End Sub 

Function VALIDA.COMPRA.UVT External 
Integer*1 VALIDA.COMPRA.UVT
End Function 

Sub BONOELECTRO.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$) External
String X.ENVIA$, X.LLEGA$, X.SALE$, X.RTA$
End Sub 

Sub BNSD.BUSCA.LIMITE.TV(UE.TIPO.VAR$)
  Integer*2 UE.I%
  Integer*1 UE.FOUND%
  String UE.TIPO.VAR$
  UE.I% = 1
  UE.FOUND% = 0 
  Gr.Bnsyd.MaxTv% = 0
  Gr.Bnsyd.ChgTv% = 0 
  While UE.I% <= TO.NUMTNDR AND NOT UE.FOUND%
    If TO.TENDOPTS(UE.I%,0) = Val(LEFT$(UE.TIPO.VAR$,1)) AND  \
       TO.TENDOPTS(UE.I%,1) = Val(RIGHT$(UE.TIPO.VAR$,1)) Then Begin 
       Gr.Bnsyd.MaxTv% = TO.TENDLIMITS(UE.I%,0)
       Gr.Bnsyd.ChgTv% = TO.TENDLIMITS(UE.I%,1)
       UE.FOUND%  = -1
       Gr.Bnsyd.TVPOS% = UE.I%
    EndIf Else UE.I% = UE.I% + 1
  Wend    
  UE.I% = 0
  For UE.I% = 0 To 7
      GR.SDNEGCNT(UE.I%) = To.NEGCNT(UE.I%)    
      GR.SDNEGAMT(UE.I%) = To.NEGAMT(UE.I%)    
  Next UE.I%
End Sub

Sub CONFIRMAR.BONOSUPDRG
String Xtv$, Xtemp4$, Xtrama$,Xsnd$, Xfin$, Xcedulas$												!
String Xbuffer$, Xpago$, Xean$
Integer*4 XPago%
  Call Visores4690(1,"ACTUALIZANDO BONOS SD ","ESPERE POR FAVOR..",0,"L")   ! 
  Xpago% = 0
  For I% = 1 TO SL.END                                                      ! FOR ALL StringS                   
    Asc.Tmp.Apun% = 3
    H$ = READ.SL.STR$(I%)                                                   ! GET String                        
    If LEN(H$) > 5 Then Begin                                              \! ASSURE GOOD String                
     If ASC(H$) = 5 Then Begin                                              ! FORMA PAGO String                 
    	 Xtv$   = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                            ! Tipo y variedad de pago registrado
  	 	 Xpago$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                          	! Valor forma de pago               
  	 	 If Xtv$ = Gr.Bnsyd.BnPag$ Then Begin																	!
          Xpago% = XPago% + Val(Xpago$)                                     ! Monto otros pagos
       EndIf																																!
     EndIf																							                    ! FIN  FORMA PAGO String            
     If ASC(H$) = 6 Then Begin                                              ! FORMA PAGO String                 
    	 Xtv$   = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                            ! Tipo y variedad de pago registrado
  	 	 Xpago$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	                          	! Valor forma de pago               
  	 	 If Xtv$ = Gr.Bnsyd.BnPag$ Then Begin																	!
          Xpago% = XPago% - Val(Xpago$)                                     ! Monto otros pagos
       EndIf																																!
     EndIf																							                    ! FIN  FORMA PAGO String            
    EndIf																																		! 
  Next I%                                                                   ! NEXT String                       
  If XPago% <= 0 Then Exit Sub 																							! No hay pagos para reportar
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora del requerimiento
   Xean$ = Mid$(Gr.Bnsyd.BnEan$,2,2) + Right$(Gr.Bnsyd.BnEan$,10) +        \!
           Mid$(Gr.Bnsyd.BnEan$,4,10)                                       !
   Xbuffer$ = Right$("               "+Gr.Lcl.Clte$,15) +                  \! ID cliente y bono escaneado
              Xean$                                                         !
   Asc.Pay.Impr% = 0                                                        ! Ajusta numero trx para seguimiento
   XTemp4$ = Armar.Trama.Msg("21","61",Xbuffer$,"00","0001","123456")       ! Armar trama act bono electro
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)         ! Ejecuta Requerimiento
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call BONOELECTRO.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   EndIf
End Sub  

Sub Validando.BonosSupDrg
String Xsnd$, Xfin$, Xbuffer$, XTemp4$, Xtrama$, Xean$
  If TS.INTRX Then Begin                                                    ! Si esta dentro de una trx venta
  	 If USER.FBACT.READ = 0 Then Begin																		  ! Cliente no capturado
  	    Call VISOR.AND.BORRAR("NO SE HA CAPTURADO  VECINO FIEL...")         !
  	    TS.TEMP1I1 = 0																											! Reporta falla en el proceso
  	 	  Exit Sub 
  	 EndIf
     If TS.IO.KEYS(1) = 70 Then Begin																				!
     	  TS.IO.DATA$(9) = Gr.Bnsyd.BnEan$
     	  Gr.Bnsyd.Apl% = 0
     	  Exit Sub 
     EndIf
     Call VISORES4690(1,"VALIDANDO BONOS SD","ESPERE POR FAVOR ...",0,"L")	! Msg Operador
     Asc.Pay.Impr% = 0																											! Aumenta contador de trx
     Xsnd$ = DATE$ +":"+ Time$                                              ! Fecha y hora del requerimiento
     Xean$ = Mid$(Gr.Bnsyd.BnEan$,2,2) + Right$(Gr.Bnsyd.BnEan$,10) +      \!
             Mid$(Gr.Bnsyd.BnEan$,4,10)                                     !
     Xbuffer$ = Right$("               "+Gr.Lcl.Clte$,15) +                \! ID cliente y bono escaneado
                Xean$                                                       !
     XTemp4$ = Armar.Trama.Msg("21","60",Xbuffer$,"00","0001","123456")     ! Armar trama MSG
     XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)       ! Ejecuta Requerimiento
     Xfin$ = DATE$ +":"+ Time$                                              ! Fecha y hora rta del requerimiento
     Call BONOELECTRO.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)              ! Rastreo movimiento
     XTEMP4$ = Valida.Rta(XTrama$)																		      ! Valida rta entregada
     If Xtemp4$ <> "00" Then Begin 																					! Si falla en proceso comunicacion
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
  	  TS.TEMP1I1 = 0																											  ! Reporta falla en el proceso      
   	  Exit Sub 																															!
     EndIf																																	!
     XTEMP4$ = Mid$(XTrama$,12,2)	         																  ! Valida rta entregada
     If XTemp4$ <> "00" Then Begin 																				  ! Rta No Satisfactoria
     	  Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									        ! Presenta Msg Error
  	    TS.TEMP1I1 = 0																										  ! Reporta falla en el proceso      
   	    Exit Sub 																														!
     EndIf																																	!
     TS.TEMP1I1 = -1																										    ! Reporta proceso exitoso
  EndIf Else Begin																													! Fuera de una operacion de venta
  	 Call VISOR.AND.BORRAR("FORMA PAGO NO VALIDA PARA NO VENTA")            !
  	 TS.TEMP1I1 = 0																													! Reporta falla en el proceso
  EndIf																																			! fin operacion de venta
End Sub 																																		! Fin validación bonos 

Sub BONOS.ELECTROSUPDRG(Xopt%) Public																			  ! Modulo bonos SUPDRG
Integer*4 Xopt%, Xj%

!--- EAMTSU07.J86
If XOPT% = 07 Then Begin																										! Carga de opciones
  Gr.Bnsyd.Act% = 0 																										    ! Init proyecto activo
  Dim GR.SDNEGCNT(8)
  Dim GR.SDNEGAMT(8)
  TS.ER.RETURN = -1																													! Control errores
  OPEN "R::$SCNTRL" AS 94 UNLOCKED NOWRITE NODEL												    ! Apertura archivo parametrizacion 
  If TS.ER.RETURN <> -1 Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR PARAMTROS BONOS ELEC SD")                 !
	   Gr.Bnsyd.Act% = 0																										  ! Init proyecto activo  	 
  	 Exit Sub																																!
  EndIf 
  If End #94 Then UE.FIN.BNSYD        																	    ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[BONOS ELECTRO SUPDROG]" Then Begin		  		        ! Parametros bonos supermercado-drogueria
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Bnsyd.Act% = Val(Mid$(TS.TEMP1$,30,2))      			    					! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																								! Lectura registro
         Gr.Bnsyd.BnSup$ = Mid$(TS.TEMP1$,30,3)                             ! Prefijo bono supermercado
         Read #94; TS.TEMP1$																								! Lectura registro
         Gr.Bnsyd.BnDrg$ = Mid$(TS.TEMP1$,30,3)                             ! Prefijo bono droguería
         Read #94; TS.TEMP1$																								! Lectura registro
         Gr.Bnsyd.BnPag$ = Mid$(TS.TEMP1$,30,2)                             ! Tipo y variedad pago registro
         Call BNSD.BUSCA.LIMITE.TV(Gr.Bnsyd.BnPag$)												  ! Ajusta controles pago
         GoTo UE.FIN.BNSYD                                                  ! Sale del proceso
        EndIf																																!
  Wend 																																		  ! Fin recorrido archivo
  UE.FIN.BNSYD:    
    Close 94 
    If Gr.Bnsyd.Act% = -1 Then                                             \! Proyecto Activo
      Call U.IMPRIME("MODULO BONOS SUPDRG    ON 26/Jul/2023",2100H) Else   \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO BONOS SUPDRG   OFF 26/Jul/2023",2100H)         ! Msg Proyecto Cargado
    
EndIf

If Gr.Bnsyd.Act% = 0 Then Exit Sub 																				  ! Si proyecto apagado

!--- EAMTSU02.J86
If XOPT% = 02 Then Begin																										! Al inicio de una trx
   Gr.Bnsyd.Apl% = 0
   Gr.Bnsyd.BnEan$ = ""                                                     ! EAN capturado
   To.TENDLIMITS(Gr.Bnsyd.TVPOS%,0) = Gr.Bnsyd.MaxTv%											  ! Retorna valores default
   To.TENDLIMITS(Gr.Bnsyd.TVPOS%,1) = Gr.Bnsyd.ChgTv%											  ! Retorna valores default
   TS.TEMP1I1 = 0
   For TS.TEMP1I1 = 0 To 7
       TO.NEGAMT(TS.TEMP1I1) = GR.SDNEGAMT(TS.TEMP1I1)
       TO.NEGCNT(TS.TEMP1I1) = GR.SDNEGCNT(TS.TEMP1I1)
   Next TS.TEMP1I1
   
EndIf																																				! Fin eamtsu02

!--- EAMTSU14.J86
If XOPT% = 14 Then Begin																										! Secuencia de tecleo

!-- Add Registro bonos electro super y drog 26Jul2023
!-- Estrucutura
!-- xxx prefijo
!-- 10 numero de referencia
!-- 10 valor del bono 
If LEN(TS.IO.DATA$(2)) >= 23 And \																					! Si barra forma de pago
   (Left$(TS.IO.DATA$(2),3) = Gr.Bnsyd.BnSup$) Or                          \! si prefijo bonos supermercado
   (Left$(TS.IO.DATA$(2),3) = Gr.Bnsyd.BnDrg$) Then Begin                   ! o droguería
     If (TS.TRX.STATUS <> 1 And TS.PROCEDURE = -1) Then Begin	  	          ! No se ha ingresado total en venta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    ! Inicializa secuencia de teclado
         TS.IO.MOTORKEY = 0 																							  ! 
         TS.GUIDANCE = 1020																							    ! Solicita total a la trx
         Exit Sub 																												  ! Sale del proceso
     EndIf 																															    !
     TS.TEMP1I1 = VALIDA.COMPRA.UVT																					! Si cumple parametros compra minima UVT
     If TS.TEMP1I1 = -1 Then Begin 	  																			! Si pasa validacion Feco
     	Gr.Bnsyd.BnEan$ = TS.IO.DATA$(2)                                      ! Barra capturada
      TS.TEMP2$ = Right$(TS.IO.DATA$(2),10)                                 ! Valor a pagar 
      TS.TEMP1$ = Mid$(TS.IO.DATA$(2),4,10)                                 ! Numero de referencia
      TS.TEMP1I1 = 0
      If TS.IO.KEYS(1) = 70 Then TS.TEMP1I1 = -1														! Registra anulación
      Dim TS.IO.PREV.DATA$(10) : Dim TS.IO.PREV.KEYS(10)
      TS.IO.PREV.MOTORKEY = 80
      TS.IO.DEVICE = 1 
      Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 												      ! Init vectores de carga
      If TS.TEMP1I1 = -1 Then TS.IO.KEYS(1) = 70                            ! Si anulacion de pago 
      TS.IO.DATA$(3) = Right$(Gr.Bnsyd.BnPag$,1)   													! Variedad parametrizada
      TS.IO.KEYS(3)  = 78                                                   ! Tecla Slash
      TS.IO.DATA$(7) = Str$(Val(TS.TEMP2$))														      ! Valor a entregar
      TS.IO.DATA$(9) = TS.TEMP1$      													      		  ! Numero de autorizacion
      TS.IO.KEYS(7)  = Val("9"+Left$(Gr.Bnsyd.BnPag$,1))                    ! Tipo de pago parametrizado
      TS.IO.MOTORKEY = TS.IO.KEYS(7)																	      ! Reporta a la tecla motora
      To.TENDLIMITS(Gr.Bnsyd.TVPOS%,0) = 9999999   										  	  ! Modifica controles entrega efectivo
      To.TENDLIMITS(Gr.Bnsyd.TVPOS%,1) = 9999999									      		! Modifica controles entrega efectivo
      TS.TEMP1I1 = 0
      For TS.TEMP1I1 = 0 To 7
          TO.NEGAMT(TS.TEMP1I1) = 9999999
          TO.NEGCNT(TS.TEMP1I1) = 999
      Next TS.TEMP1I1           
      Gr.Bnsyd.Apl% = -1
     EndIf Else Begin																												! Fin validacion
     	Gr.Bnsyd.Apl% = 0 																										! Pago aplicado
     EndIf
EndIf    


EndIf																																				! Fin eamtsu14

!--- EAMTSU20.J86
If XOPT% = 20 Then Begin																										! En impresion de tiquete
 If Not TS.TRAINING Then Begin                       												! Si no en entrenamiento
  If TS.INTRX Then                                  											 \! Si esta en una compra
    If TS.LINETYPE = 6 And                                                 \! Store Data y Transacc
       TS.LINEDATA = 1 Then Begin                    											  ! Fecha, Hora, etc
        If TS.TENDERED (0) <> 0 Or                                         \! Si hay pagos
           TS.TRX.STATUS <> 100 Then Begin                                  ! y NO hay VOID en curso
           If Gr.Bnsyd.Apl% = -1	Then Begin																! Init variable
           	   Call CONFIRMAR.BONOSUPDRG                                    ! Almacena bono electro SUPDRG
           	   Gr.Bnsyd.Apl% = 0                                            ! proceso confirmado
           EndIf																														!
        EndIf
     EndIf
  EndIf
EndIf 																																			! Fin Eamtsu02

!--- EAMTSU32.J86
If XOPT% = 32 Then Begin																										! Validacion forma de pago
 If (TS.IO.DATA$(3) = Right$(Gr.Bnsyd.BnPag$,1) And                        \!
     TS.IO.KEYS(7)  = Val("9"+Left$(Gr.Bnsyd.BnPag$,1))) Then Begin         ! Si pago bono electro SUPDRG
 	  TS.TEMP1I1 = 0																													! Control de errores
 	  If Gr.Bnsyd.Apl% <> -1 Then Exit Sub                                    ! No es un bono SYD
    Call Validando.BonosSupDrg																							! Validando forma de pago
    If TS.IO.KEYS(1) = 70 Then Exit Sub
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
       Exit Sub                                                             !
   	EndIf																																		! 
 EndIf																																			! fin pago
EndIf																																				! Fin Eamtsu32

End Sub																																			! Fin Modulo control cambio precio
