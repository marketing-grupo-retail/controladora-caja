!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMBNDEV.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Pago Bonos Devolucion            *
!**************************************************
!Observaciones:
! Version 1.0 22 Julio 2024        
!--------------------------------------------------
! Mod 11Abr2025
! Se permite intercambio medios de pago para anulacion
! Bonos devolucion.
! Solicitado por Comfandi - Julian Hidalgo
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%ENVIRON T		                          																		! Ambiente de terminal

Integer*1 Global Gr.BonDev.Ok%,          																	 \! Control del proyecto
                 Gr.BonDev.TrxBon%,                                        \! Proceso pago bono
                 Gr.BonDev.InTrx%,                                         \! Bono en la trx
                 Gr.BonDev.AplAn%,                                         \! Proceso de anulacion
                 Gr.BonDev.Captura%,                                       \! Forma captura bono
                 Gr.BonDev.Capt%                                            ! Bono capturado
Integer*1 Global TS.RECOVERY           																			! terminal recovery flag                 
Integer*2 Global GR.BDNEGCNT(1)                                             !
Integer*4 Global Gr.BonDev.Motora%,                                        \! Tecla procedimiento
                 Gr.BonDev.Slend%,                                         \!
                 Asc.Pay.Impr%,                                            \!
                 Gr.BonDev.TVPOS%,                                         \!
                 Gr.BonDev.MaxTv%,                                         \! Maximo pago   tipo y variedad
                 Gr.BonDev.ChgTv%,                                         \! Maximo cambio tipo y variedad
                 GR.BDNEGAMT(1),                                           \!
                 Asc.Tmp.Apun%                                              ! Apuntador String
String    Global Gr.BonDev.TipVar$,                                        \! Tipo y variedad de pago
                 Gr.BonDev.Captura$,                                       \!
                 Gr.BonDev.Valor$,                                         \! Valor del bono
                 Gr.BonDev.Pref$, 																				 \! Prefijo bono
                 Gr.BonDev.DatTmp$,                                        \! Temporal bono
                 Gr.Lcl.Clte$,                                             \! Id del cliente
                 Hora.Mundial$,                                            \! Fecha y hora para confirmacion
                 Gr.BonDev.Nro$                                             ! Numero bono capturado
Integer  Global  USER.FBACT.READ                                            ! SUCCESSFUL READ OF FILE
Integer  Global  SL.END                                                     ! NUMBER TOTAL STRINGS

%INCLUDE EAMTSWKG.J86          																			        ! working storage
%INCLUDE EAMTRANS.J86          																			        ! working storage
%INCLUDE EAMITEMR.J86          																			        ! Maestra productos
%INCLUDE EAMTOPTS.J86          																			        ! working storage
%INCLUDE EAMTSXHC.J86                                              					! Rutinas de Assembler            
%INCLUDE EAMASMRT.J86                                              					! Rutinas de Assembler            

%INCLUDE RECATSSU.011          																			        ! Rutinas Genericas Grupo Retail

Function FORMAT.AMOUNT(AMT1) External   																		! Formateo de valores
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub TSHIECET EXTERNAL
End Sub

Sub  TSCSEC03 External
End Sub 

Sub TSTDEC01 EXTERNAL          																							! tendering
End Sub 																																		!

Sub TSBDEC01 EXTERNAL          																							! balance due (normal/foodstamp)
End Sub                        																							!

Function VALIDA.COMPRA.UVT External 
Integer*1 VALIDA.COMPRA.UVT
End Function 

Sub BNDV.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)       								! Auditoria FECO
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$,      \!
       X.RTA$, X.BUFF$																											!
Integer*4 X.Len%																														!
			TS.ER.RETURN = -1																											!
			X.FILE$ = "R::ADX_UDT1:BD" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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

Sub BNDEV.BUSCA.LIMITE.TV(UE.TIPO.VAR$)																			! Control de limites pago
  Integer*2 UE.I%																														!
  Integer*1 UE.FOUND%																												!
  String UE.TIPO.VAR$																												!
  UE.I% = 1																																	!
  UE.FOUND% = 0 																														!
  Gr.BonDev.MaxTv% = 0																											!
  Gr.BonDev.ChgTv% = 0 																											!
  While UE.I% <= TO.NUMTNDR AND NOT UE.FOUND%																!
    If TO.TENDOPTS(UE.I%,0) = VAL(LEFT$(UE.TIPO.VAR$,1)) AND               \!
       TO.TENDOPTS(UE.I%,1) = VAL(RIGHT$(UE.TIPO.VAR$,1)) Then Begin 				!
       Gr.BonDev.MaxTv% = TO.TENDLIMITS(UE.I%,0)														!
       Gr.BonDev.ChgTv% = TO.TENDLIMITS(UE.I%,1)														!
       UE.FOUND%  = -1																											!
       Gr.BonDev.TVPOS% = UE.I%																							!
    EndIf Else UE.I% = UE.I% + 1																						!
  Wend    																																	!
  UE.I% = 0																																	!
  For UE.I% = 0 To 7																												!
      GR.BDNEGCNT(UE.I%) = To.NEGCNT(UE.I%)    															!
      GR.BDNEGAMT(UE.I%) = To.NEGAMT(UE.I%)    															!
  Next UE.I%																																!
End Sub																																			! fin control limites pago

Function BUSQUEDA.PAGO.BNDEVOL(Xfnd$)																				! Validacion pago 
Integer*1 BUSQUEDA.PAGO.BNDEVOL
String XR$, Xtemp4$, Xtrama$,Xsnd$, Xfin$, Xfnd$														!
String Xbuffer$, Xtmp$, xbon$, Xvlr$, Xid$, Xsgn$								            !
String xenvia$, xsale$, xrta$, xllega$, xmov$														    !
Integer*4 Xpos%, Xloc%, Xj%, Xi%																						!
   Xpos% = 0 : Xloc% = 0																										!
   For XI% = 1 TO SL.END                                  									! FOR ALL StringS
       H$ = READ.SL.STR$(XI%)                              									! GET String
       If LEN(H$) > 5 Then Begin                          									! ASSURE GOOD String
        If ASC(H$) = 153 Then Begin                        									! User Data String
     	     Asc.Tmp.Apun% = 3 																							  ! Init apuntador String 
     	     Xtmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													  ! numero del proyecto
     	     If Xtmp$ = "20240916" Then Begin																  ! Pago bono devolucion
     	     	  Xbon$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! numero del bono
     	     	  Xvlr$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! valor del bono
     	     	  Xid$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! cliente
              xmov$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! aammddhhmmss movimiento
              Xsgn$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! signo operacion
              If Gr.BonDev.AplAn% = -1 Then Begin														! Si es anulacion bono
               If (Xfnd$ = XBON$) And (Xsgn$ = "00") Then Begin             ! Encuentra y positivo
    	     	    	Xloc% = -1 																								! Lo encontro
    	     	    	TS.TEMP3$ = XMOV$                                         ! Retorna datos de uso
    	     	   EndIf																												! Fin encontrado
               If (Xfnd$ = XBON$) And (Xsgn$ = "01") Then Begin             ! Encuentra y Anulado
    	     	    	Xloc% = 0  																								! Lo encontro
    	     	    	TS.TEMP3$ = ""                                            ! Retorna datos de uso
    	     	   EndIf																												! Fin encontrado
    	     	  EndIf																													! Fin en anulacion
              If Gr.BonDev.AplAn% = 0  Then Begin														! Si ya aplicado
               If (Xfnd$ = XBON$) And (Xsgn$ = "00") Then Begin             ! Encuentra y positivo
    	     	    	Xloc% = -1 																								! Lo encontro
    	     	   EndIf																												! Fin encontrado
               If (Xfnd$ = XBON$) And (Xsgn$ = "01") Then Begin             ! Encuentra y Anulado
    	     	    	Xloc% = 0  																								! Lo encontro
    	     	   EndIf																												! Fin encontrado
    	     	  EndIf																													! Fin en anulacion
        	 EndIf																														! fin String pago bono
        EndIf																																! Fin User data
       EndIf																																! Fin String tlog
   Next XI%																																	! Fin barrido tlog
   If Xsgn$ = "00" Then                                                    \! Si encuentra bono en pago 
      BUSQUEDA.PAGO.BNDEVOL = Xloc% Else                                   \!
      BUSQUEDA.PAGO.BNDEVOL = 0																							!
End Function 																																!

Sub VALIDA.PAGO.BNDEV(Xbono$,Xfunc$)																				! Validacion bono
String Xbono$, Xfunc$, Xbuffer$, XTemp4$, xenvia$, xsale$, xrta$, xllega$   !
String XTrama$, XMonto$																											!
Integer*1 Xrta%																															!
 TS.TEMP1I1 = 0																															! Control proceso
 If Gr.BonDev.AplAn% = -1 Then Begin																				! Si es anulacion bono
    Call VISORES4690(1,"ANULANDO  BONO","ESPERE POR FAVOR..",0,"L")					! Msg Operador
 	  Xrta% = BUSQUEDA.PAGO.BNDEVOL(XBONO$)                         					! Busca bono capturado
    If Xrta% = 0 Then Begin 																								! No encontrado en trx
       Call VISOR.AND.BORRAR("BONO A ANULAR NO    EXITE EN LA COMPRA")			!
       Exit Sub																															!
    EndIf																																		!
    Xbuffer$ = Right$(String$(30," ")+Xbono$,30) +                         \!
               Right$("0000"+TS.STORE$,4)              +                   \! Numero del almacen
               Right$("000000"+Str$(SL.HD.TERMINAL),6) +                   \! Numero de terminal
               Right$("000000"+Str$(SL.HD.TRANSNUM+1),6) +                 \! Numero de transaccion
               TS.TEMP3$    																				        ! Trama de anulacion
 EndIf Else Begin																														! Fin anulacion bono
 	  Call VISORES4690(1,"PROCESA BONO  ","DEVOLUCION ESPERE.",0,"L")				  ! Msg Operador
 	  Xrta% = BUSQUEDA.PAGO.BNDEVOL(XBONO$)                         					! Busca bono capturado
    If Xrta% <> 0 Then Begin 																								! si encontrado en trx
       Call VISOR.AND.BORRAR("BONO A REGISTRAR YA EXITE EN LA COMPRA")			!
       TS.TEMP1I1 = 0																												! Control proceso
       Exit Sub																															!
    EndIf																																		!
    Xbuffer$ = Right$(String$(30," ")+Xbono$,30)         +                 \! Nro del bono
               Right$("00000000000"+Gr.BonDev.Valor$,11)                    ! Valor del bono
 EndIf																																			!
 Asc.Pay.Impr% = 1																													!
 XTemp4$ = Armar.Trama.Msg("22",Xfunc$,(Xbuffer$),"00","0001","123456")     ! Armar trama MSG
 Xenvia$ = XTemp4$																											    !
 Xsale$  = DATE$ +":"+ Time$																							  ! Hora salida requerimiento
 XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)           ! Ejecuta Requerimiento
 XTEMP4$   = Valida.Rta(XTrama$)																			      ! Valida rta entregada
 Xrta$     = DATE$ +":"+ Time$																						  ! Hora llegada requerimiento
 Xllega$   = Xtrama$																										  	!
 Call BNDV.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                     	  ! Rastro auditoria
 If Xtemp4$ = "00" Then Begin 																						  ! Proceso satisfactorio
    If Mid$(XTrama$,12,2) <> "00" Then Begin 																! Con Error de ejecucion 
    	 Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))														! Presenta mensaje de Error 
    	 TS.TEMP1I1 = 0																												! Control proceso
   	   Exit Sub      																												! Sale del proceso
    EndIf	  																																!
 EndIf Else Begin																														! Falla proceso
 	  Call VISOR.AND.BORRAR("ERROR EN VALIDACION BONO DEVOL /Borra")					!
 	  TS.TEMP1I1 = 0																													! Control proceso
 	  Exit Sub 																																!
 EndIf																																	    !
 If Gr.BonDev.AplAn% = 0 Then Begin																				  ! Si es consumo bono
    TS.TEMP1$ = MID$(XTrama$,64,8)                                          ! Fecha validez bono
    If Val(DATE$) >= Val(TS.TEMP1$) Then Begin												      ! 
       Call VISOR.AND.BORRAR("FECHA VIGENCIA DEL  BONO NO AUTORIZADA")      !
       TS.TEMP1I1 = 0																												! Control proceso
       Exit Sub 																														!
    EndIf																																	  !
 EndIf																																			!
 Gr.BonDev.Nro$ = Xbono$																										! Nro bono procesado
 TS.TEMP6$ = Left$(XTrama$,58)
 TS.TEMP1I1 = -1																														! Proceso exitoso
End Sub 																																		! Fin validacion bono

Function CONFIRMA.CONSUMO.BNDEV																							! Confirma consumo Bono
String Xbono$, Xfunc$, Xbuffer$, XTemp4$, xenvia$, xsale$, xrta$, xllega$   !
String XTrama$
Integer*1 CONFIRMA.CONSUMO.BNDEV
 CONFIRMA.CONSUMO.BNDEV = 0
 If Gr.BonDev.AplAn% = -1 Then Begin
 	  CONFIRMA.CONSUMO.BNDEV = -1  																						! Control proceso
 	  Exit Function 
 EndIf
 Call VISORES4690(1,"CONFIRMANDO CONSUMO","ESPERE POR FAVOR",2500,"L")         ! 
 Xbuffer$ = Right$(String$(30," ")+Gr.BonDev.Nro$,30)                       ! Nro del bono
 Asc.Pay.Impr% = 1
 XTemp4$ = Armar.Trama.Msg("22","05",(Xbuffer$),"00","0001","123456")       ! Armar trama MSG
 Xenvia$ = XTemp4$																											    !
 Xsale$  = DATE$ +":"+ Time$																							  ! Hora salida requerimiento
 XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)           ! Ejecuta Requerimiento
 
 !Xtrama$ = "2205046200000BONO DEVOLUCION BLOQUEADO SATISFACTORIAM"
 
 XTEMP4$   = Valida.Rta(XTrama$)																			      ! Valida rta entregada
 Xrta$     = DATE$ +":"+ Time$																						  ! Hora llegada requerimiento
 Xllega$   = Xtrama$																										  	!
 Call BNDV.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                        ! Rastro auditoria
 If Xtemp4$ = "00" Then Begin 																						  ! Proceso satisfactorio
    If Mid$(XTrama$,12,2) <> "00" Then Begin 																! Con Error de ejecucion 
    	 Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))														! Presenta mensaje de Error 
    	 CONFIRMA.CONSUMO.BNDEV = 0																						! Control proceso
   	   Exit Function 																												! Sale del proceso
    EndIf	  																																!
 EndIf Else Begin																														! Falla proceso
 	  Call VISOR.AND.BORRAR("ERROR CONFIRMANDO BONO")												  !
 	  CONFIRMA.CONSUMO.BNDEV = 0																							! Control proceso
 	  Exit Function																														!
 EndIf																																	    !
 CONFIRMA.CONSUMO.BNDEV = -1  																							! Control proceso
End Function																																! Fin confirma consumo bono

Sub CONFIRMAR.PAGO.BNDEV
String XR$, Xtemp4$, Xtrama$,Xsnd$, Xfin$												            !
String Xbuffer$, Xbonos$(2), Xtmp$, xbon$, Xvlr$, Xid$, Xsgn$								!
String xenvia$, xsale$, xrta$, xllega$, xmov$														    !
Integer*4 Xpos%, Xloc%, Xj%, Xi%
   Dim Xbonos$(100,2)                                                       !
   Xpos% = 0
   For XI% = 1 TO Gr.BonDev.Slend%                        									! FOR ALL StringS
       H$ = READ.SL.STR$(XI%)                              									! GET String
       If LEN(H$) > 5 Then Begin                          									! ASSURE GOOD String
        If ASC(H$) = 153 Then Begin                        									! User Data String
     	     Asc.Tmp.Apun% = 3 																							  ! Init apuntador String 
     	     Xtmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)													  ! numero del proyecto
     	     If Xtmp$ = "20240916" Then Begin																  ! Pago bono devolucion
     	     	  Xbon$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! numero del bono
     	     	  Xvlr$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! valor del bono
     	     	  Xid$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! cliente
              xmov$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! aammddhhmmss movimiento
              Xsgn$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)											  ! signo operacion
     	     	  Xloc% = 0
     	     	  For Xj% = 1 To Xpos%																					! Busqueda de bono
     	     	    If Xbonos$(Xj%,0) = Xbon$ Then                             \! Bono encontrado
     	     	    	 Xloc% = Xj%																							!
     	     	  Next Xj%																											!
              If Xloc% = 0 Then Begin                                       ! No Existe
              	 Xpos% = Xpos% + 1
              	 Xbonos$(Xpos%,0) = Xbon$
              	 If Xsgn$ = "01" Then Xvlr$ = Str$(Val(Xvlr$) * -1)         ! Si anulacion
              	 Xbonos$(Xpos%,1) = Xvlr$                                   !
                 Xbonos$(Xpos%,2) = Right$("0000"+TS.STORE$,4)  +          \! Numero del almacen
                                    Right$("000000"+Str$(SL.HD.TERMINAL),6) + \! Numero de terminal
                                    Right$("000000"+Str$(SL.HD.TRANSNUM),6) + \! Numero de transaccion
                                    Xmov$                                   ! Fecha y Hora de la operacion AAMMDDHHMMSS
              EndIf Else Begin																							! Bono ya existe
              	 If Xsgn$ = "01" Then Xvlr$ = Str$(Val(Xvlr$) * -1)         ! Si anulacion
              	 Xbonos$(Xloc%,1) = Str$(Val(Xbonos$(Xloc%,1)) + Val(Xvlr$))!
                 Xbonos$(Xloc%,2) = Right$("0000"+TS.STORE$,4)  +          \! Numero del almacen
                                    Right$("000000"+Str$(SL.HD.TERMINAL),6) + \! Numero de terminal
                                    Right$("000000"+Str$(SL.HD.TRANSNUM),6) + \! Numero de transaccion
                                    Xmov$                                   ! Fecha y Hora de la operacion AAMMDDHHMMSS
              EndIf																													!
        	 EndIf																														! fin String pago bono
        EndIf																																! Fin User data
       EndIf																																! Fin String tlog
   Next XI%																																	! Fin barrido tlog
   Xj% = 0
   If Xpos% > 0 Then \
   	  Call Visores4690(1,"ACTUALIZANDO BONOS    ","ESPERE POR FAVOR..",2500,"L")
   For Xj% = 1 To Xpos%																											! Total bonos para actualizar
    If Val(Xbonos$(XJ%,1)) > 0 Then Begin																	  ! Bono para actualizar
     Xsnd$ = DATE$ +":"+ Time$                                              ! Fecha y hora del requerimiento     
     Xbuffer$ = Right$(String$(30," ")+Xbonos$(XJ%,0),30) + Xbonos$(XJ%,2)  ! numero del bono + datos origen
     Asc.Pay.Impr% = 2                                                      ! Ajusta numero trx para seguimiento 
     XTemp4$ = Armar.Trama.Msg("22","08",Xbuffer$,"00","0001","123456")     ! Armar trama MSG CC              
     XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)       ! Ejecuta Requerimiento              
     Xfin$ = DATE$ +":"+ Time$                                              ! Fecha y hora rta del requerimiento 
     Call BNDV.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                   	! Rastreo movimiento
     XTEMP4$ = Valida.Rta(XTrama$)																			    ! Valida rta entregada
     If Xtemp4$ <> "00" Then Begin																					!
        Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									        ! Presenta Msg Error                 
     EndIf                                                                  !
    EndIf																																		! Fin Bono actualizar                 
   Next Xj%
End Sub 

Function CONSUMO.CAMBIO.PAGO																								! Valida bono devol cambio medio pago
Integer*1 CONSUMO.CAMBIO.PAGO, Xrta1%
String    Xbuffer$, XTemp4$, Xenvia$, Xsale$, Xtrama$, Xrta$, Xllega$
  CONSUMO.CAMBIO.PAGO = -1																									! Proceso aceptado
  If (TS.IO.DATA$(3) = Right$(Gr.BonDev.TipVar$,1)) And                    \! Si usa forma de pago 
     (TS.IO.KEYS(7)  = Val("9"+Left$(Gr.BonDev.TipVar$,1))) Then Begin      ! bono devolucion
     Gr.BonDev.Valor$   = TS.IO.DATA$(7)																		! Valor del bono
     Gr.BonDev.Captura$ = TS.IO.DATA$(9)																		! Nro del bono
     If Len(Gr.BonDev.Captura$) <> 20 Then Begin														! Error longitud del bono
     	  Call VISOR.AND.BORRAR("DATO ESCANEADO NO ES UN BONO DEVOL.")		    ! Msg Alerta
     	  CONSUMO.CAMBIO.PAGO = 0 																						! Proceso fallido
     	  Exit Function 																											! Sale del proceso
     EndIf  																																! 
     Gr.BonDev.Captura$ = Right$(Gr.BonDev.Captura$,18)											! Toma numero bono para validar
     Gr.BonDev.Nro$ = Gr.BonDev.Captura$																		! Para procesos de bloqueo y confirmación
     Xbuffer$ = Right$(String$(30," ")+Gr.BonDev.Captura$,30)  +           \! Nro del bono
               Right$("00000000000"+Gr.BonDev.Valor$,11)                    ! Valor del bono
     XTemp4$ = Armar.Trama.Msg("22","04",(Xbuffer$),"00","0001","123456")   ! Armar trama MSG            
     Xenvia$ = XTemp4$																										  !                            
     Xsale$  = DATE$ +":"+ Time$																					  ! Hora salida requerimiento  
     XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)       ! Ejecuta Requerimiento      
     
     !XTrama$ = "2204065200000NOTA DEVO CONSULTADO SATISFACTORIAMENTE         20260314"
     
     XTEMP4$   = Valida.Rta(XTrama$)																			  ! Valida rta entregada       
     Xrta$     = DATE$ +":"+ Time$																				  ! Hora llegada requerimiento 
     Xllega$   = Xtrama$																									 	!                            
     Call BNDV.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                    ! Rastro auditoria           
               
     If Xtemp4$ = "00" Then Begin 																				  ! Proceso satisfactorio
      If Mid$(XTrama$,12,2) <> "00" Then Begin 															! Con Error de ejecucion 
    	 Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))														! Presenta mensaje de Error 
    	 CONSUMO.CAMBIO.PAGO = 0																							! Control proceso
   	   Exit Function 																												! Sale del proceso
      EndIf	  																															!
     EndIf Else Begin																												! Falla proceso
 	     Call VISOR.AND.BORRAR("ERROR EN VALIDACION BONO DEVOL /Borra")				!
    	 CONSUMO.CAMBIO.PAGO = 0																							! Control proceso
   	   Exit Function 																												! Sale del proceso
     EndIf																															    !
     TS.TEMP1$ = MID$(XTrama$,64,8)                                         ! Fecha validez bono
     If Val(DATE$) >= Val(TS.TEMP1$) Then Begin												      ! 
       Call VISOR.AND.BORRAR("FECHA VIGENCIA DEL  BONO NO AUTORIZADA")      !
    	 CONSUMO.CAMBIO.PAGO = 0																							! Control proceso
   	   Exit Function 																												! Sale del proceso
     EndIf																																  !
     Xrta1% = CONFIRMA.CONSUMO.BNDEV																				! Bloqueo del bono
     
     If Xrta1% = -1 Then Begin
        TS.TEMP1$ = Pack$(Gr.BonDev.Nro$)   + ":" +                        \! Numero bono
                    Pack$(Gr.BonDev.Valor$) + ":" +                        \! Valor del bono
                    Pack$("222222222222")   + ":" +                        \! Cliente asociado
                    Pack$(Hora.Mundial$)    + ":" +                        \! ammddhhmmss para confirmar
                    Pack$("00")             + ":" +                        \! Signo Operacion  00.Pago 01. Anulacion
                    Pack$("01")                                             ! Reporta consumo en intercambio medio pago
        Call Grabacion.Cadena.Usuario("20240916",TS.TEMP1$)                 ! Almacena UD bono devolucion 
        TS.TEMP1$ = Left$("BONO:"+Gr.BonDev.Nro$+String$(37," "),37)        !
        Call U.IMPRIME(TS.TEMP1$,6100H)                                     !
      EndIf
     
     CONSUMO.CAMBIO.PAGO = Xrta1%																						! Rta del bloqueo
     
 EndIf																																			!
     
End Function 


Sub BONODEVPAGO(XOPT%) Public			 																				  ! Bono devolucion pago
Integer*4 Xopt%                                                             !
Integer*1 Xrta%
String    Xtmp$

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.BonDev.Ok%  = 0                                                        ! Proyecto Apagado
  TS.ER.RETURN = -1																													! Ctrl Errores
  Dim GR.BDNEGCNT(8)
  Dim GR.BDNEGAMT(8)
  Open "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL               							! Apertura archivo parametrizacion
  If TS.ER.RETURN <> -1 Then BEGIN                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA BONOS DEVOL. ")									! MSg alerta
  	 Exit Sub 																															! Sale del proceso
  EndIf 																																		!
  IF END #94 THEN UE.FIN.BONDEV         																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        IF TS.TEMP1$ = "[BONO DEVOLUCION]" Then Begin		          				  ! Bono devolucion 
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.BonDev.Ok%   = Val(Mid$(TS.TEMP1$,30,2))   				    					! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.BonDev.Pref$ = Mid$(TS.TEMP1$,30,02)          				          ! Prefijo bonos
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.BonDev.TipVar$ = Mid$(TS.TEMP1$,30,02)          				        ! Tipo y variedad pago registro
         Call BNDEV.BUSCA.LIMITE.TV(Gr.BonDev.TipVar$)
         GoTo UE.FIN.BONDEV  																								! Sale del ciclo de carga          
       EndIf                                                                !
   Wend                                                                     !
   UE.FIN.BONDEV:                                                           !
     Close 94																																! Cierra archivo
   If Gr.BonDev.Ok% = -1 Then                                              \! Proyecto Activo
      Call U.IMPRIME("MODULO BONO DEVOLUC. ON  11-Abr-2025",2100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO BONO DEVOLUC. OFF 11-Abr-2025",2100H)          ! Msg Proyecto Cargado
EndIf 																																			! Fin carga opciones

If Gr.BonDev.Ok% <> -1 Then Exit Sub                                        ! Si proyecto apagado

!--- EAMTSU02.J86
If Xopt% = 02 Then Begin                                                    ! Inicio de trx
	 Call CONFIRMAR.PAGO.BNDEV
   Gr.BonDev.Nro$ = ""
   Gr.BonDev.Capt% = 0
   Gr.BonDev.TrxBon% = 0
   Gr.BonDev.Captura$ = ""
   Gr.BonDev.AplAn% = 0
   Gr.BonDev.Valor$ = ""
   Gr.BonDev.Slend% = 0
   Gr.BonDev.InTrx% = 0
   Gr.BonDev.Captura% = 0
   Gr.BonDev.DatTmp$ = ""
   To.TENDLIMITS(Gr.BonDev.TVPOS%,0) = Gr.BonDev.MaxTv%											! Retorna valores default
   To.TENDLIMITS(Gr.BonDev.TVPOS%,1) = Gr.BonDev.ChgTv%											! Retorna valores default
   TS.TEMP1I1 = 0
   For TS.TEMP1I1 = 0 To 7
       TO.NEGAMT(TS.TEMP1I1) = GR.BDNEGAMT(TS.TEMP1I1)
       TO.NEGCNT(TS.TEMP1I1) = GR.BDNEGCNT(TS.TEMP1I1)
   Next TS.TEMP1I1
EndIf																																				! Inicio de trx 

!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                    ! Secuencias de tecleo
  
	If Gr.BonDev.InTrx% = -1 Then                                            \! Si bono en proceso
	 If	TS.IO.KEYS(1) = 82 And TS.IO.MOTORKEY = 81 Then Begin                 ! Si Suspension de trx
 			 Call Visor.And.Borrar("SUSPENSION NO AUTOR.CON BONO DEVOLUC.  ")     ! Msg de alerta
       Call Visor.And.Borrar("ANULE BONO DEVOLUC. PARA SUSPENDER TRX ")     ! Msg de alerta
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
       Exit Sub 																														!
   EndIf
	
	If Gr.BonDev.InTrx% = -1 Then                                            \! Si bono en proceso
    If	TS.IO.KEYS(1) = 70 And TS.IO.MOTORKEY = 81 Then Begin               ! Si anulacion total
 	   If (TS.TOTALS(0,0,0) <> TS.BALDUE(0)) Then Begin												! Si pagos capturados
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
       TS.IO.MOTORKEY = 73																									!
   	   TS.GUIDANCE = 1069																										! Msg Error appl nativa
	     Exit Sub 
	   EndIf
    EndIf
	
   If LEN(TS.IO.DATA$(2)) > 14 And                                         \! Escaneo un bono 
      Left$(TS.IO.DATA$(2),2) = (Gr.BonDev.Pref$) Then Begin								! devolucion
      If Not(TS.INTRX) Then BEGIN 																					! No esta en trx de venta
	 	  	 Call VISOR.AND.BORRAR("PROCEDIMIENTO NO PERMITIDO FUERA DE VENTA") ! Msg Alerta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Exit Sub 																											    ! Termina proceso
      EndIf
  	  If TS.TRX.STATUS <> 1 And TS.PROCEDURE = -1 THEN BEGIN			  	      ! No se ha ingresado total en venta
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 0 																									! 
       TS.GUIDANCE = 1020																										! Solicita total a la trx
       Exit Sub 																														! Sale del proceso
      EndIf 																																	!

      TS.TEMP1I1 = VALIDA.COMPRA.UVT																			  ! Si cumple parametros compra minima UVT
      If TS.TEMP1I1 <> -1 Then Begin 																			  ! Si no pasa validacion Feco
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 0 																									! 
       TS.GUIDANCE = 1020																										! Solicita total a la trx
       Exit Sub 																														! Sale del proceso
      EndIf

	 	  If TS.TRAINING Then Begin																							! Si en entrenamiento
	 	  	 Call VISOR.AND.BORRAR("PROCESO NO PERMITIDO EN ENTRENAMIENTO")     ! Msg Alerta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Exit Sub 																											    ! Termina proceso
	 	  EndIf																																	! Fin Salida
      If TS.STANDALONE Then Begin 						 	                            ! Proceso fuera de linea
	 	  	 Call VISOR.AND.BORRAR("PROCESO NO PERMITIDO FUERA DE LINEA  ")     ! Msg Alerta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Exit Sub 																											    ! Termina proceso
      EndIf
      
  	  Gr.BonDev.Captura$ = TS.IO.DATA$(2)																		! Nro del bono
  	  If Len(Gr.BonDev.Captura$) <> 20 Then Begin														! Error longitud bono
	 	     Call VISOR.AND.BORRAR("DATO ESCANEADO NO ES UN BONO DEVOL.")		    !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Gr.BonDev.TrxBon% = 0 																						  ! Cancela proceso
         Gr.BonDev.Captura$ = ""
         Gr.BonDev.AplAn% = 0
         Exit Sub 																											    ! Termina proceso
  	  EndIf																																	!
  	  Gr.BonDev.Captura$ = Right$(Gr.BonDev.Captura$,18)										! Ajusta dato 
  	  If TS.IO.KEYS(1) = 70 Then Gr.BonDev.AplAn% = -1                      ! Anulacion pago
      Gr.BonDev.Valor$ = ASIC.DATOS$("INGRESE VLR. BONO.","DEVOLUCION        ") !
      If Gr.BonDev.Valor$ = "E" Then BEGIN															    ! Proceso cancelado
 	       Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								    !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Exit Sub 																											    !
      EndIf 																														    !
      If Val(Gr.BonDev.Valor$) <= 0 Then Begin
 	       Call VISOR.AND.BORRAR("VALOR INGRESADO NO VALIDO")			            !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Exit Sub 																											    !
      EndIf

      If Gr.BonDev.AplAn% = 0 Then                                         \!
         If Val(Gr.BonDev.Valor$) > ( TS.BALDUE(0) ) Then Begin             ! Si dato supera total compra
    	       Call VISOR.AND.BORRAR("MONTO BONO SUPERA ELVALOR DE LA COMPRA")!
             Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!
             TS.IO.MOTORKEY = 73																					  !
             Exit Sub 																											!
         EndIf																															!
	 	  	 If Gr.BonDev.AplAn% = 0 Then                                      \!
            Call VALIDA.PAGO.BNDEV(Gr.BonDev.Captura$,"04") Else           \! Proceso de pago
            Call VALIDA.PAGO.BNDEV(Gr.BonDev.Captura$,"06")                 ! Proceso de anulacion pago
         If TS.TEMP1I1 = 0 Then Begin																				! Proceso fallido
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!
            TS.IO.MOTORKEY = 73																							!
            Gr.BonDev.TrxBon% = 0
            Gr.BonDev.Captura$ = ""
            Gr.BonDev.AplAn% = 0
            Gr.BonDev.Valor$ = ""
            Exit Sub 
         EndIf 
         Xrta% = CONFIRMA.CONSUMO.BNDEV
   	     If Xrta% = 0 Then Begin 																		       	! Si falla el proceso
   	     	  Call VISOR.AND.BORRAR("FORMA DE PAGO NO   AUTORIZADA /Borrar ") !
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!
            TS.IO.MOTORKEY = 73																							!
            Gr.BonDev.TrxBon% = 0
            Gr.BonDev.Captura$ = ""
            Gr.BonDev.AplAn% = 0
            Gr.BonDev.Valor$ = ""
           Exit Sub 
         EndIf																															! fin falla proceso
         
         TS.BD.DSPPARM = -1                     														! SET FOR NORMAL BD
         Call TSBDEC01                          														! FIRST TAKE BAL DUE
         TS.IO.STATE = 10
         Dim TS.IO.PREV.KEYS(10)
         Dim TS.IO.PREV.DATA$(10)
         TS.IO.PREV.MOTORKEY = 0
         Dim Ts.Io.Keys(10)																									! Inicializa Vectores
         Dim Ts.Io.Data$(10)																								! 
         If Gr.BonDev.AplAn% = -1 Then TS.IO.KEYS(1) = 70                   ! Anulacion pago
         TS.IO.DATA$(3) = Right$(Gr.BonDev.TipVar$,1)           		        ! Variedad de Pago
         TS.IO.KEYS(3)  = 78                                     		        ! Tecla Slash
         TS.IO.DATA$(7) = Gr.BonDev.Valor$													        ! Valor a entregar
         !TS.IO.DATA$(9) = Gr.BonDev.Nro$														        ! Nro bono capturado
         TS.IO.DATA$(9) = Gr.BonDev.Nro$
         TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.BonDev.TipVar$,1))   		        ! Tipo de Pago
         TS.IO.MOTORKEY = TS.IO.KEYS(7)															        !
         To.TENDLIMITS(Gr.BonDev.TVPOS%,0) = 9999999   									    ! Modifica controles entrega efectivo
         To.TENDLIMITS(Gr.BonDev.TVPOS%,1) = 9999999									      ! Modifica controles entrega efectivo
         TS.TEMP1I1 = 0
         For TS.TEMP1I1 = 0 To 7
             TO.NEGAMT(TS.TEMP1I1) = 9999999																!
             TO.NEGCNT(TS.TEMP1I1) = 999																		!
         Next TS.TEMP1I1           																				  !
         Gr.BonDev.TrxBon% = -1
         Gr.BonDev.Capt% = -1
	 EndIf																																		! Fin escaneo Bono
EndIf 																																			! Fin secuencias de tecleo

!--- EAMTSU32.J86
If XOPT% = 32 Then Begin																										! En validacion de pago
 If TS.PROCEDURE = 2 Then Begin																							! En proceos intercambio medios de pago
 	  TS.TEMP5I4 =  CONSUMO.CAMBIO.PAGO																				! Valida bono devolucion
 	  If TS.TEMP5I4 = 0 Then Begin																						! Proceso fallido
      Call VISOR.AND.BORRAR("BONO DEVOL RECHAZADO       / BORRAR")
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)                              ! Init vectores entrada
      TS.IO.MOTORKEY = 73                                         
      Gr.BonDev.TrxBon% = 0
      Gr.BonDev.Captura$ = ""
      Gr.BonDev.AplAn% = 0
      Gr.BonDev.Valor$ = ""
      Gr.BonDev.Capt% = 0
      TS.USER.RETURN = 99																									  !
      TS.LINEDATA = 51                 																      ! "TARJETA INVALIDA"            
      TS.STACKERR(0) = 0                                                    ! manager's override required   
      TS.STACKERR(3) = 0                                                    ! item descriptor               
      TS.STACKERR(6) = 0                                                    ! put "M" on last display       
      TS.STACKERR(7) = -1                                                   ! indicate no printing required 
      TS.MO.REASON = 0                                                      ! invalid key with department   
      Call TSCSEC03       																							    !                               
      TS.IO.MOTORKEY = 0 																									  !
      Call Gr.Init.Trx																								      ! Init Trx
      TS.IO.STATE    = 10            																        ! PREPARE FOR NORMAL SALES STATE
 	  EndIf Else Begin
      Gr.BonDev.InTrx% = -2
 	  EndIf
 	  Exit Sub 
 EndIf
 
 If (TS.IO.DATA$(3) = Right$(Gr.BonDev.TipVar$,1)) And                     \! Si usa forma de pago 
    (TS.IO.KEYS(7)  = Val("9"+Left$(Gr.BonDev.TipVar$,1))) And             \!
    Gr.BonDev.TrxBon% <> -1 Then Begin 
      Call VISOR.AND.BORRAR("ERROR SECUENCIA DE  TECLEO / BORRAR")
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)                              ! Init vectores entrada
      TS.IO.MOTORKEY = 73                                         
       Gr.BonDev.TrxBon% = 0
       Gr.BonDev.Captura$ = ""
       Gr.BonDev.AplAn% = 0
       Gr.BonDev.Valor$ = ""
       Gr.BonDev.Capt% = 0
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
 EndIf

EndIf																																				! Fin validacion forma de pago

!-- EAMTSU20
If Xopt% = 20 Then Begin																										! En la impresion del CR
	
  If TS.PROCEDURE = 2 Then BEGIN
    	If Gr.BonDev.InTrx% = -2 Then Begin																		!
    		 Gr.BonDev.Slend% = SL.END																					! Total de strings
    		 Gr.BonDev.InTrx% = 0    																						!
    	EndIf																																	!
  EndIf  
  
  If TS.PROCEDURE <> 2 Then                                                \!
   If (TS.INTRX) AND (TS.LINETYPE = 6 AND TS.LINEDATA = 1) And  					 \!
      (TS.TENDERED (0) <> 0 Or TS.TRX.STATUS <> 100) Then Begin							!
    	If Gr.BonDev.InTrx% = -1 Then Begin
    		 Gr.BonDev.Slend% = SL.END																					! Total de strings
    		 Gr.BonDev.InTrx% = 0
    	EndIf
   EndIf
   
EndIf 

!-- EAMTSU60
If Xopt% = 60 Then Begin																										! En la impresion del CR

  If TS.LINETYPE = 2 And Gr.BonDev.Capt% = -1 THEN BEGIN										! Impresion pago 
      TS.TEMP1$ = Left$("BONO:"+Gr.BonDev.Nro$+String$(37," "),37)
      Call U.IMPRIME(TS.TEMP1$,6100H)
      Gr.BonDev.Capt% = 0
      Gr.BonDev.InTrx% = -1
      If TS.PROCEDURE = 2 Then Gr.BonDev.InTrx% = -2 
      If Not(TS.RECOVERY) Then Begin
      	  TS.TEMP2$ = "00"
      	  If Gr.BonDev.AplAn% = -1 Then Begin 
      	  	 TS.TEMP2$ = "01"
      	  	 Gr.BonDev.InTrx% = 0
      	  Endif
          TS.TEMP1$ = Pack$(Gr.BonDev.Nro$)   + ":" +                      \! Numero bono
                      Pack$(Gr.BonDev.Valor$) + ":" +                      \! Valor del bono
                      Pack$(Gr.Lcl.Clte$)     + ":" +                      \! Cliente asociado
                      Pack$(Hora.Mundial$)    + ":" +                      \! ammddhhmmss para confirmar
                      Pack$(TS.TEMP2$)        + ":" +                      \! Signo Operacion  00.Pago 01. Anulacion
                      Pack$("00")                                           ! Filler 
          Call Grabacion.Cadena.Usuario("20240916",TS.TEMP1$)               ! Almacena UD bono devolucion 
       EndIf
       Gr.BonDev.AplAn% = 0

       To.TENDLIMITS(Gr.BonDev.TVPOS%,0) = Gr.BonDev.MaxTv%									! Retorna valores default
       To.TENDLIMITS(Gr.BonDev.TVPOS%,1) = Gr.BonDev.ChgTv%									! Retorna valores default
       TS.TEMP1I1 = 0
       For TS.TEMP1I1 = 0 To 7
          TO.NEGAMT(TS.TEMP1I1) = GR.BDNEGAMT(TS.TEMP1I1)
          TO.NEGCNT(TS.TEMP1I1) = GR.BDNEGCNT(TS.TEMP1I1)
       Next TS.TEMP1I1
   EndIf
EndIf																																				! Fin EAMTSU60

!-- EAMTSU53
If Xopt% = 53 Then Begin																										! En recuperacion trx
 If Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "99" Then Begin   	    	            ! Si DATA/ENTRY
  If Unpack$(MID$(SL.STR.ENTRY$,3,2)) = "20240916" Then Begin               ! Bono devolución
    Asc.Tmp.Apun% = 3																												! Apuntador String
    TS.TEMP1$      = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	   	 	 		  ! Numero del proyecto
    If TS.TEMP1$ = "20240916" Then Begin                                    ! Pago bono devolucion
       Gr.BonDev.InTrx% = -1																								!
       Gr.BonDev.Capt%  = -1
       Gr.BonDev.Nro$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	   	 	 		! Numero del bono
    EndIf
  EndIf
 EndIf
EndIf																																				! Fin EAMTSU53

End Sub 
