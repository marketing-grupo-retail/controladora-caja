!**************************************************
!Empresa       : Grupo Retail Ltda - Colombia     *
!Programa      : GRMBKUPI.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Control Bonos Kupi        *
!**************************************************
! Modificaciones:

%ENVIRON T

String    Global Gr.BnKupi.Tvp$,                                           \! Tipo y variedad de pago 
                 Gr.BnKupi.Pin$,                                           \! Tipo y variedad de pago 
                 Gr.BnKupi.PinKupi$,                                       \! Pin reportado al cliente
                 Gr.BnKupi.Motor$,                                         \! Tecla motora
                 Gr.BnKupi.Tmp1$,                                          \! Temporal 1
                 Gr.BnKupi.Tmp2$,                                          \! Temporal 2
                 Gr.BnKupi.Tmp3$,                                          \! Temporal 3
                 Gr.BnKupi.Tmp4$,                                          \! Temporal 4
                 Gr.BnKupi.Empr$                                            ! Nit empresa
String    Global Gr.Lcl.Clte$																								! ID cliente
Integer*1 Global Gr.BnKupi.Act%,																					 \! Control proyecto
                 Gr.BnKupi.AplAn%,                                         \! Anulacion pago Kupi
                 Gr.BnKupi.Apl%,                                           \! Pago Bono Aplicado
                 Asc.Pay.Impr%                                              ! Control nro trx 
Integer*2 Global GR.BKNEGCNT(1)                                             !
Integer*4 Global Gr.BnKupi.Motora%,                                        \! Tecla procedimiento
                 Gr.BnKupi.TVPOS%,                                         \!
                 Gr.BnKupi.MaxTv%,                                         \! Maximo pago   tipo y variedad
                 Gr.BnKupi.ChgTv%,                                         \! Maximo cambio tipo y variedad
                 Gr.BKNEGAMT(1),                                           \!
                 Asc.Tmp.Apun%                                              ! Apuntador String

%INCLUDE EAMTOPTS.J86          																			        ! working storage
%INCLUDE EAMTSEVA.J86   																					  				! Variables EMS
%INCLUDE EAMTSWKG.J86                                              					! working storage                 
%INCLUDE EAMTRANS.j86                                              					!                                 
%INCLUDE EAMTERMS.J86                                              					! 
%INCLUDE EAMTSXHC.J86                                              					! Rutinas de Assembler            
%INCLUDE EAMASMRT.J86                                              					! Rutinas de Assembler            
                                                                                                              
%INCLUDE RECATSSU.011          					                                    ! RUTINAS GENERICAS APLICACION    

Sub  TSCSEC03 External
End Sub 

Sub TSBDEC01 EXTERNAL          ! balance due (normal/foodstamp)
End Sub                        !

Sub BONOKUPI.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:KP" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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


Sub BKUPI.BUSCA.LIMITE.TV(UE.TIPO.VAR$)
  Integer*2 UE.I%
  Integer*1 UE.FOUND%
  String UE.TIPO.VAR$
  UE.I% = 1
  UE.FOUND% = 0 
  Gr.BnKupi.MaxTv% = 0
  Gr.BnKupi.ChgTv% = 0 
  While UE.I% <= TO.NUMTNDR AND NOT UE.FOUND%
    If TO.TENDOPTS(UE.I%,0) = VAL(LEFT$(UE.TIPO.VAR$,1)) AND  \
       TO.TENDOPTS(UE.I%,1) = VAL(RIGHT$(UE.TIPO.VAR$,1)) Then Begin 
       Gr.BnKupi.MaxTv% = TO.TENDLIMITS(UE.I%,0)
       Gr.BnKupi.ChgTv% = TO.TENDLIMITS(UE.I%,1)
       UE.FOUND%  = -1
       Gr.BnKupi.TVPOS% = UE.I%
    EndIf Else UE.I% = UE.I% + 1
  Wend    
  UE.I% = 0
  For UE.I% = 0 To 7
      GR.BKNEGCNT(UE.I%) = To.NEGCNT(UE.I%)    
      GR.BKNEGAMT(UE.I%) = To.NEGAMT(UE.I%)    
  Next UE.I%
End Sub

Sub Validando.BonoKupi
String Xsnd$, Xfin$, Xbuffer$, XTemp4$, Xtrama$, Xfunc$
     If Gr.BnKupi.AplAn% = 0 Then Xfunc$ = "01" Else Xfunc$ = "03"          ! 
     Call VISORES4690(1,"VALIDA BONO KUPI  ","ESPERE POR FAVOR ...",0,"L")	! Msg Operador
     Asc.Pay.Impr% = 0																											! Aumenta contador de trx
     Xsnd$ = DATE$ +":"+ Time$                                              ! Fecha y hora del requerimiento
     
     If Xfunc$ = "01" Then Begin
        Xbuffer$ = Right$("               "+Gr.BnKupi.Tmp1$,15) +          \! Id de cliente
                   Right$("           "+Gr.BnKupi.Tmp3$,11)     +          \! Valor del pago 
                   Right$("            "+Gr.BnKupi.Empr$,12)                ! Nit empresa
     EndIf Else Begin
        Xbuffer$ = Right$("               "+Gr.BnKupi.Tmp1$,15) +          \! Id de cliente
                   Right$("        "+Gr.BnKupi.PinKupi$,8)     +           \! pin reportado por el cliente
                   Right$("        "+Gr.BnKupi.Pin$,10)                     ! Pin operacion kupi
     EndIf  
     XTemp4$ = Armar.Trama.Msg("20",Xfunc$,Xbuffer$,"00","0001","123456")   ! Armar trama MSG
     XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)       ! Ejecuta Requerimiento
     
     !Xtrama$ = "2001058200000ACTUALIZACION SE REALIZO DE MANERA SATIF1234567890"
     
     Xfin$ = DATE$ +":"+ Time$                                              ! Fecha y hora rta del requerimiento
     Call BONOKUPI.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                 ! Rastreo movimiento
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
     Gr.BnKupi.Pin$ = Mid$(Xtrama$,54,10)                                   ! Nro pin autorizacion
     If Xfunc$ = "03" Then Begin																						! Si proceso anulacion exitoso
        Gr.BnKupi.Pin$ = Mid$(Xtrama$,54,10)                                ! Nro pin autorizacion
        TS.TEMP1I1 = -1																										  ! Reporta proceso exitoso
        Gr.BnKupi.Apl% = 0 																									! Pago aplicado
        Gr.BnKupi.AplAn% = 0
     	  Exit Sub 																														! 
     EndIf
     Gr.BnKupi.PinKupi$ = Asic.Datos$("SOLICITE PIN KUPI AL","CLIENTE")
     If Gr.BnKupi.PinKupi$ = "E" Or Val(Gr.BnKupi.PinKupi$) <= 0 Then Begin ! Proceso cancelado   
   	   Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  		! Msg de Alerta
       TS.TEMP1I1 = 0  																									    ! Reporta proceso no exitoso
       Gr.BnKupi.Apl% = 0 																									! Pago aplicado
       Exit Sub       																							    		!
     EndIf 																														    	!
     Call VISORES4690(1,"CONFIRMANDO A KUPI","ESPERE POR FAVOR ...",0,"L")	! Msg Operador
     Asc.Pay.Impr% = 0																											! Aumenta contador de trx
     Xsnd$ = DATE$ +":"+ Time$                                              ! Fecha y hora del requerimiento
     Xbuffer$ = Right$("               "+Gr.BnKupi.Tmp1$,15) +             \! Id de cliente
                Right$("        "+Gr.BnKupi.PinKupi$,8)     +              \! pin reportado por el cliente
                Right$("          "+Gr.BnKupi.Pin$,12)                      ! Pin operacion kupi
     XTemp4$ = Armar.Trama.Msg("20","02",Xbuffer$,"00","0001","123456")     ! Armar trama MSG
     XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)      ! Ejecuta Requerimiento
     
     !Xtrama$ = "2002058200000ACTUALIZACION SE REALIZO DE MANERA SATIF1122334455"
     
     Xfin$ = DATE$ +":"+ Time$                                              ! Fecha y hora rta del requerimiento
     Call BONOKUPI.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                 ! Rastreo movimiento
     XTEMP4$ = Valida.Rta(XTrama$)																		      ! Valida rta entregada
     If Xtemp4$ <> "00" Then Begin 																					! Si falla en proceso comunicacion
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
      TS.TEMP1I1 = 0  																									    ! Reporta proceso no exitoso
      Gr.BnKupi.Apl% = 0 																									  ! Pago aplicado
   	  Exit Sub 																															!
     EndIf																																	!
     XTEMP4$ = Mid$(XTrama$,12,2)	         																  ! Valida rta entregada
     If XTemp4$ <> "00" Then Begin 																				  ! Rta No Satisfactoria
     	  Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									        ! Presenta Msg Error
        TS.TEMP1I1 = 0  																									  ! Reporta proceso no exitoso
        Gr.BnKupi.Apl% = 0 																									! Pago aplicado
   	    Exit Sub 																														!
     EndIf																																	!
     Gr.BnKupi.Pin$ = Mid$(Xtrama$,54,10)                                   ! Nro pin autorizacion
     TS.TEMP1I1 = -1																										    ! Reporta proceso exitoso
     Gr.BnKupi.Apl% = -1																										! Pago aplicado
End Sub 																																		! Fin validación bonos 

Sub BONOS.KUPI(Xopt%) Public																						  	! Modulo bonos kupi
Integer*4 Xopt%, Xj%

!--- EAMTSU07.J86
If XOPT% = 07 Then Begin																										! Carga de opciones
  Gr.BnKupi.Act% = 0 																										    ! Init proyecto activo
  Dim GR.BKNEGCNT(8)
  Dim GR.BKNEGAMT(8)
  TS.TS11WERR$ = ""																													! Control errores
  OPEN "R::$SCNTRL" AS 94 UNLOCKED NOWRITE NODEL												    ! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR PARAMTROS BONOS ELECTRO")                 !
	   Gr.BnKupi.Act% = 0																										  ! Init proyecto activo  	 
  	 Exit Sub																																!
  EndIf 
  If End #94 Then UE.FIN.BONOKUPI      																	    ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[BONOS KUPI]" Then Begin		 	  		        				! Parametros bonos kupi
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.BnKupi.Act% = Val(Mid$(TS.TEMP1$,30,2))      			    					! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.BnKupi.Tvp$ = Mid$(TS.TEMP1$,30,2)           			    					! Tipo y variedad de pago 
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.BnKupi.Empr$ = Mid$(TS.TEMP1$,30,12)           			    			  ! Nit empresa
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.BnKupi.Motor$ = Mid$(TS.TEMP1$,30,3)           			    			  ! Tecla motora
         
         Call BKUPI.BUSCA.LIMITE.TV(Gr.BnKupi.Tvp$)													! Ajusta controles pago
         GoTo UE.FIN.BONOKUPI                                               ! Sale del proceso
        EndIf																																!
  Wend 																																		  ! Fin recorrido archivo
  UE.FIN.BONOKUPI: 
    Close 94 
    If Gr.BnKupi.Act% = -1 Then                                            \! Proyecto Activo
      Call U.IMPRIME("MODULO BONOS KUPI      ON 22/Sep/2021",2100H) Else   \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO BONOS KUPI     OFF 22/Sep/2021",2100H)         ! Msg Proyecto Cargado
EndIf

If Gr.BnKupi.Act% = 0 Then Exit Sub 																				! Si proyecto apagado

!--- EAMTSU02.J86
If XOPT% = 02 Then Begin																										! Al inicio de una trx
   Gr.BnKupi.PinKupi$ = ""
   Gr.BnKupi.Pin$  = ""
   Gr.BnKupi.Tmp1$ = ""
   Gr.BnKupi.Tmp2$ = ""
   Gr.BnKupi.Tmp3$ = ""
   Gr.BnKupi.AplAn% = 0
   Gr.BnKupi.Apl%   = 0
   To.TENDLIMITS(Gr.BnKupi.TVPOS%,0) = Gr.BnKupi.MaxTv%											! Retorna valores default
   To.TENDLIMITS(Gr.BnKupi.TVPOS%,1) = Gr.BnKupi.ChgTv%											! Retorna valores default
   TS.TEMP1I1 = 0
   For TS.TEMP1I1 = 0 To 7
       TO.NEGAMT(TS.TEMP1I1) = GR.BKNEGAMT(TS.TEMP1I1)
       TO.NEGCNT(TS.TEMP1I1) = GR.BKNEGCNT(TS.TEMP1I1)
   Next TS.TEMP1I1

EndIf																																				! Fin eamtsu02

!--- EAMTSU14.J86
If XOPT% = 14 Then Begin																										! En entrada de datos
! If (TS.IO.DATA$(3) = Right$(Gr.BnKupi.Tvp$,1)) And                        \!
!    (TS.IO.KEYS(7)  = Val("9"+Left$(Gr.BnKupi.Tvp$,1))) Then Begin          ! Si pago bono kupi

 If TS.IO.MOTORKEY = Val(Gr.BnKupi.Motor$) Then Begin
    If TS.IO.KEYS(1) = 70 Then Gr.BnKupi.AplAn% = -1                        ! Anulacion pago
    Gr.BnKupi.Tmp1$ = Asic.Datos$("INGRESE ID CLIENTE  "," ")
    If Gr.BnKupi.Tmp1$ = "E" Or Val(Gr.BnKupi.Tmp1$) <= 0 Then Begin  			! Proceso cancelado   
   	   Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  		! Msg de Alerta
       Exit Sub       																							    		!
    EndIf 																														    	!
    Gr.BnKupi.Tmp3$ = ASIC.DATOS$("INGRESE VALOR DEL ","BONO KUPI         ") !
    If Gr.BnKupi.Tmp3$ = "E" Then BEGIN															        ! Proceso cancelado
 	       Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								    !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Exit Sub 																											    !
    EndIf 																														      !
    If Val(Gr.BnKupi.Tmp3$) <= 0 Then Begin
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
         If Gr.BnKupi.AplAn% = -1 Then TS.IO.KEYS(1) = 70                   ! Anulacion pago
         TS.IO.DATA$(3) = Right$(Gr.BnKupi.Tvp$,1)              		        ! Variedad de Pago
         TS.IO.KEYS(3)  = 78                                     		        ! Tecla Slash
         TS.IO.DATA$(7) = Gr.BnKupi.Tmp3$ 													        ! Valor a entregar
         TS.IO.DATA$(9) = Gr.BnKupi.Tmp1$														        ! Id del cliente
         TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.BnKupi.Tvp$,1))   		          ! Tipo de Pago
         TS.IO.MOTORKEY = TS.IO.KEYS(7)															        !
         To.TENDLIMITS(Gr.BnKupi.TVPOS%,0) = 9999999   										  ! Modifica controles entrega efectivo
         To.TENDLIMITS(Gr.BnKupi.TVPOS%,1) = 9999999									      ! Modifica controles entrega efectivo
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
 If Not TS.TRAINING Then Begin                       												! Si no en entrenamiento
  If TS.INTRX Then                                  											 \! Si esta en una compra
    If TS.LINETYPE = 6 And                                                 \! Store Data y Transacc
       TS.LINEDATA = 1 Then Begin                    											  ! Fecha, Hora, etc
        If TS.TENDERED (0) <> 0 Or                                         \! Si hay pagos
           TS.TRX.STATUS <> 100 Then Begin                                  ! y NO hay VOID en curso
           If Gr.BnKupi.Apl% = -1	Then Begin																! Init variable
           	   
           	   !Call CONFIRMAR.BONOKUPI                                      ! Almacena bono KUPI
           	   
           	   Gr.BnKupi.Apl% = 0                                           ! proceso confirmado
           EndIf																														!
        EndIf
     EndIf
  EndIf
EndIf 																																			! Fin Eamtsu02

!--- EAMTSU32.J86
If XOPT% = 32 Then Begin																										! Validacion forma de pago
 If (TS.IO.DATA$(3) = Right$(Gr.BnKupi.Tvp$,1)) And                        \!
    (TS.IO.KEYS(7)  = Val("9"+Left$(Gr.BnKupi.Tvp$,1))) Then Begin          ! Si pago bono kupi
 	  TS.TEMP1I1 = 0																													! Control de errores
    Call Validando.BonoKupi   																							! Validando forma de pago
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
       Gr.BnKupi.PinKupi$ = ""
       Gr.BnKupi.Pin$  = ""
       Gr.BnKupi.Tmp1$ = ""
       Gr.BnKupi.Tmp2$ = ""
       Gr.BnKupi.Tmp3$ = ""
       Gr.BnKupi.AplAn% = 0
       Gr.BnKupi.Apl%   = 0
       Exit Sub                                                             !
   	EndIf Else Begin																							          !
   		 TS.TEMP2$ = "00"
   		 If TS.IO.KEYS(1) = 70 Then TS.TEMP2$ = "01"
       TS.TEMP1$ = Pack$(Gr.BnKupi.Tmp1$)  + ":" +                         \! Numero cliente
                   Pack$(Gr.BnKupi.PinKupi$)+ ":" +                        \! Numero pin reportado por el cliente
                   Pack$(Gr.BnKupi.Tmp3$)  + ":" +                         \! Valor compra
                   Pack$(Gr.BnKupi.Pin$)   + ":" +                         \! Pin Autorizacion de trx
                   Pack$(TS.TEMP2$)                                         ! Signo Operacion  00.Pago 01. Anulacion
      Call Grabacion.Cadena.Usuario("20210922",TS.TEMP1$)                   ! Almacena UD bono recompra pago
      Gr.BnKupi.Apl%   = -1
    EndIf
 EndIf																																			! fin pago
EndIf																																				! Fin Eamtsu32

End Sub																																			! Fin Modulo control cambio precio
