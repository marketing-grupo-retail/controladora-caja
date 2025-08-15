!**************************************************
!Empresa       : Grupo Retail Ltda - Colombia     *
!Programa      : GRFOSFEC.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Subsidio Fosfec           *
!**************************************************
! Modificaciones:
!
! Mod 26ene2018
! Se modifica el ingreso por credencial y se cambia
! por ingreso de cedula de extranjeria.
! ajustado por requerimiento de Comfandi.
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 1Abr2019
! Se desarrolla el pago de subsidio monetario 
! dentro de la secuencia de pago fosfec, según los 
! requerimientos entregados por Comfandi 
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 22Nov2019
! Se ajusta el proceso de items no validos en pago
! de bono alimentos para que realice el reintegro
! de los productos.
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 26Nov2019
! Se cambia la validación de los excluidos no por 
! estructura comercial, se valida por plu.
! solicitado por Comfandi 
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 27Jul2021
! Se adiciona la captura de cedula digital por 
! teclado.
! Solicitado por Comfandi 
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%ENVIRON T

Integer*1 Global Gr.Fosfec.Act%,  		                                     \! Si proyecto activo        
                 Gr.Fosfec.TrxCed%,                                        \! 
                 Gr.Fosfec.TipEnt%,                                        \!
                 Gr.Fosfec.Anul%,                                          \! Anulacion pago
                 Gr.Fosfec.Consulta%,                                      \!
                 Asc.Pay.Impr%,                                            \!
                 Asic.Detalle%,                                            \!
                 Gr.Fosfec.InAnul%,                                        \!
                 Gr.Fosfec.Trx%    		                                      ! Si pago en registro    
                                                                                                           
Integer*2 Global Gr.Fosfec.Sec%,    		                                   \! Tecla para Secuencia     
                 GR.NEGCNT(1)                                               !
Integer*4 Global Gr.Fosfec.Ndoc%,                                          \! Numero documentos
                 Gr.RsmItm.Xcont%,																				 \!
                 Gr.RsmItm.Ycont%,																				 \!
                 Gr.Fosfec.TVPOS%,                                         \!
                 Gr.Fosfec.MaxTv%,                                         \! Maximo pago   tipo y variedad
                 Gr.Fosfec.ChgTv%,                                         \! Maximo cambio tipo y variedad
                 Gr.Fosfec.Cont%,                                             \!
                 GR.NEGAMT(1),                                             \!
                 Asc.Tmp.Apun%
String    Global Gr.Fosfec.Benef$,                                         \! ID del beneficiario       
                 Gr.Fosfec.TipVar$,                                        \! Tipo y variedad definida  
                 Gr.Fosfec.Naut$,                                          \! Numero autorizacion       
                 Gr.Fosfec.Trama$,                                         \! Trama original mensaje    
                 Gr.Fosfec.TipDoc$,                                        \! Tipo de documento
                 Gr.Fosfec.IdBene$,                                        \! ID del beneficiario
                 Gr.Fosfec.ViaPago$,                                       \! Via de Pago
                 Gr.Fosfec.Appl$,                                          \! Codigo aplicacion
                 Gr.Fosfec.Func$,                                          \! Codigo funcion
                 Gr.Fosfec.Captura$,                                       \!
                 Gr.Fosfec.RsmItm$(2),                                     \!
                 Gr.Fosfec.ExcItm$(2),                                     \!
                 IR.USERDATA$,                                             \!
                 Gr.Fosfec.Vlr$                                             ! Valor del subsidio        

Integer   Global CHAIN.COUNT        																				! Access base variable

%INCLUDE ADX_UPGM:EAMTSCVA.J86
%INCLUDE ADX_UPGM:EAMTSWKG.J86                                              ! working storage                 
%INCLUDE ADX_UPGM:EAMTRANS.j86                                              !                                 
%INCLUDE ADX_UPGM:EAMTERMS.J86                                              ! 
%INCLUDE ADX_UPGM:EAMITEMR.J86                                              ! 
%INCLUDE ADX_UPGM:EAMTOPTS.J86	                                            ! Variables de la terminal options
%INCLUDE ADX_UPGM:EAMTSXHC.J86                                              ! Rutinas de Assembler            
%INCLUDE ADX_UPGM:EAMASMRT.J86                                              ! Rutinas de Assembler            
                                                                                                              
%INCLUDE RECATSSU.011          					                                    ! RUTINAS GENERICAS APLICACION    

Sub TSPREC01 EXTERNAL          																							! print routine
End Sub

Sub TSTCEC01 EXTERNAL                                                       ! tender cashing            
End Sub                                                                     !                           
                                                                                                        
Sub TSCSEC10 EXTERNAL                                                       ! non sales initialization  
End Sub                                                                     !                           

Sub  TSCSEC03 External
End Sub 

Sub TSTDEC01 EXTERNAL          																							! tendering
End Sub 

Sub TSBDEC01 EXTERNAL            							! take a balance due
End Sub

Sub TSIEEC01 EXTERNAL          ! item sales
End Sub                        !

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

Sub TSHIECET External 
End Sub 

Function CEDULA.PDF417 External
String   CEDULA.PDF417
End Function 

Sub CREAR.PIPE.PDF External
End Sub 

Sub FOSFEC.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:FF" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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

Sub FOSFEC.BUSCA.LIMITE.TV(UE.TIPO.VAR$)
  Integer*2 UE.I%
  Integer*1 UE.FOUND%
  String UE.TIPO.VAR$
  UE.I% = 1
  UE.FOUND% = 0 
  Gr.Fosfec.MaxTv% = 0
  Gr.Fosfec.ChgTv% = 0 
  While UE.I% <= TO.NUMTNDR AND NOT UE.FOUND%
    If TO.TENDOPTS(UE.I%,0) = VAL(LEFT$(UE.TIPO.VAR$,1)) AND  \
       TO.TENDOPTS(UE.I%,1) = VAL(RIGHT$(UE.TIPO.VAR$,1)) Then Begin 
       Gr.Fosfec.MaxTv% = TO.TENDLIMITS(UE.I%,0)
       Gr.Fosfec.ChgTv% = TO.TENDLIMITS(UE.I%,1)
       UE.FOUND%  = -1
       Gr.Fosfec.TVPOS% = UE.I%
    EndIf Else UE.I% = UE.I% + 1
  Wend    
  UE.I% = 0
  For UE.I% = 0 To 7
      GR.NEGCNT(UE.I%) = To.NEGCNT(UE.I%)    
      GR.NEGAMT(UE.I%) = To.NEGAMT(UE.I%)    
  Next UE.I%
End Sub

Sub Voucher.Fosfec																													! Generacion vouchers pago fosfec
Integer*4 XJ%
String    Xbuff$
 If Gr.Fosfec.TipEnt% = 1 Then Begin    
    To.USEREXIT(20) = 0																									   	! Activa  lineas impresion
    To.USEREXIT(60) = 0																									   	!
    Call U.IMPRIME(TO.HEADERLINE1$,4100H)																		!
    Call U.IMPRIME(TO.HEADERLINE2$,4100H)																		!
    Call U.IMPRIME(" ",4100H)
    Call U.IMPRIME("     *** PAGO SUBSIDIO FOSFEC ***",4100H)
    Call Format.Amount(Val(Gr.Fosfec.Vlr$))																!
    Xbuff$ = "VALOR ENTREGADO   :$"+TS.TEMP1$
    Xbuff$ = Left$(Xbuff$+String$(38," "),38)
    Call U.IMPRIME(Xbuff$,4100H)									!
    Xbuff$ = "ID BENEFICIARIO   :"+Gr.Fosfec.IdBene$
    Xbuff$ = Left$(Xbuff$+String$(38," "),38)
    Call U.IMPRIME(Xbuff$,4100H)									!
    
    CALL U.Imprime(" ",44400h)
    CALL U.Imprime("Firma:______________________________",4100H)
    Asic.Detalle% = 1																												!
    TS.TEMP1$ = Linea.Detalle(38)  																					! Trailer del movimiento
    Call U.IMPRIME(TS.TEMP1$,4200H)                                       	!
    TS.TEMP1$ = String$(37,"-")																							! Linea cierre voucher
    Call U.IMPRIME(TS.TEMP1$,4900H)																			  	! avance de papel
    Call U.CORTACR																													! Corta Papel
    To.USEREXIT(20) = -1																								  	! Activa  lineas impresion
    To.USEREXIT(60) = -1																								  	!
    TS.LINETYPE = 18																										  	! Genera cabecera 
    TS.LINEDATA = 1																											  	! en proximo tiquete
    Call TSPREC01																												  	!
 EndIf
 If Gr.Fosfec.TipEnt% = 2 Then Begin    
    To.USEREXIT(20) = 0																									   	! Activa  lineas impresion
    To.USEREXIT(60) = 0																									   	!
    Call U.IMPRIME(TO.HEADERLINE1$,4100H)																		!
    Call U.IMPRIME(TO.HEADERLINE2$,4100H)																		!
    Xj% = 0																																	!
    For Xj% = 1 To Gr.RsmItm.Xcont%           															! Recorre vector venta itms
     If Val(Gr.Fosfec.RsmItm$(xj%,1)) <> 0 Then Begin                       ! Tiene Cantidd
      Call Format.Amount(Val(Gr.Fosfec.RsmItm$(xj%,2)))                     ! Ajusta dato impresion
      Xbuff$ = Right$("       "+Gr.Fosfec.RsmItm$(xj%,0),7)		+	" " +			 \! Item
               Gr.Fosfec.RsmItm$(xj%,5)                       + " " +      \! Descriptor
               Right$("          "+TS.TEMP1$,10)                            ! Precio de venta total
      Xbuff$ = Left$(Xbuff$+String$(38," "),38)                             ! Ajusta dato impresion
      Call U.IMPRIME(XBUFF$,4100H)                                          ! Imprime items comprados
     EndIf
    Next Xj%																																!
    Call Format.Amount(Val(Gr.Fosfec.Vlr$))																  !
    Call U.IMPRIME(" ",4100H)
    Call U.IMPRIME("     *** PAGO SUBSIDIO FOSFEC ***",4100H)
    Call Format.Amount(Val(Gr.Fosfec.Vlr$))																!
    Xbuff$ = "VALOR ENTREGADO   :$"+TS.TEMP1$
    Xbuff$ = Left$(Xbuff$+String$(38," "),38)
    Call U.IMPRIME(Xbuff$,4100H)									!
    Xbuff$ = "ID BENEFICIARIO   :"+Gr.Fosfec.IdBene$
    Xbuff$ = Left$(Xbuff$+String$(38," "),38)
    Call U.IMPRIME(Xbuff$,4100H)									!
    CALL U.Imprime(" ",44400h)
    CALL U.Imprime("Firma:______________________________",4100H)
    Asic.Detalle% = 1																												!
    TS.TEMP1$ = Linea.Detalle(38)  																					! Trailer del movimiento
    Call U.IMPRIME(TS.TEMP1$,4200H)                                       	!
    TS.TEMP1$ = String$(37,"-")																							! Linea cierre voucher
    Call U.IMPRIME(TS.TEMP1$,4900H)																			  	! avance de papel
    Call U.CORTACR																													! Corta Papel
    To.USEREXIT(20) = -1																								  	! Activa  lineas impresion
    To.USEREXIT(60) = -1																								  	!
    TS.LINETYPE = 18																										  	! Genera cabecera 
    TS.LINEDATA = 1																											  	! en proximo tiquete
    Call TSPREC01																												  	!
    
 EndIf

End Sub 

Function COBRO.SUBSIDIO.FOSFEC
Integer*1 COBRO.SUBSIDIO.FOSFEC																							!
String XR$, Xtemp4$, Xtrama$,Xsnd$, Xfin$, Xcedulas$												!
String Xbuffer$
   If Gr.Fosfec.TipEnt% = 1 Then 																				   \! Pago monetario
      Call VISORES4690(1,"VALIDANDO FOSFEC  ","ESPERE POR FAVOR ...",0,"L")	! Msg Operador
   If Gr.Fosfec.TipEnt% = 2 Then 																				   \! Pago monetario
      Call VISORES4690(1,"VALIDANDO ALIMENTO","ESPERE POR FAVOR ...",0,"L")	! Msg Operador
   
   Asc.Pay.Impr% = 0																												! Aumenta contador de trx
   COBRO.SUBSIDIO.FOSFEC = 0  																				      !
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora del requerimiento
   Xbuffer$ = Right$("    "+Gr.Fosfec.TipDoc$,4)    +                      \! Tipo documento
              Right$("                    "+Gr.Fosfec.IdBene$,20) +        \! Numero de documento
              Right$("                   "+Gr.Fosfec.Benef$,20)             ! Numero de la credencial
   XTemp4$ = Armar.Trama.Msg(Gr.Fosfec.Appl$,Gr.Fosfec.Func$,Xbuffer$,"00","0001","123456")       ! Armar trama MSG
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)         ! Ejecuta Requerimiento
   
   !If Gr.Fosfec.TipEnt% = 1 Then 																				   \! Pago monetario
   ! Xtrama$ = "1191201200000CONSULTA              SATISFACTORIA     172890      66994788            5  205200001194205200001195205200001196205200001197205200001198                                                            "
   !If Gr.Fosfec.TipEnt% = 2 Then 																				   \! Pago Alimentos
   ! Xtrama$ = "1181201200000CONSULTA              SATISFACTORIA     976555      66994788            5  202400000267202400000267202400000267202400000267202400000267                                                            "
   
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call FOSFEC.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                     ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   	  COBRO.SUBSIDIO.FOSFEC = 0 		 																        ! Error del proceso
   	  Gr.Fosfec.Trx% = 0
   	  Exit Function
   EndIf
   XTEMP4$ = Mid$(XTrama$,12,2)	           																  ! Valida rta entregada
   If XTemp4$ = "00" Then Begin 																					  ! Rta Satisfactoria
       Gr.Fosfec.Vlr$ = Str$(Val(Mid$(XTrama$,54,12)))                      ! Valor del subsidio
       If Val(Gr.Fosfec.Vlr$) <= 0 Then Begin															  ! Error en Valor Pago
   		    Call VISOR.AND.BORRAR("VALOR DEL SUBSIDIO  NO VALIDO")		        ! Presenta Msg Error
   		    COBRO.SUBSIDIO.FOSFEC = 0																				  !
   		    Gr.Fosfec.Trx% = 0
          Exit Function 
       EndIf
       Gr.Fosfec.IdBene$ = Mid$(XTrama$,66,20) 														  ! ID beneficiario
       Gr.Fosfec.Ndoc% = Val(Mid$(XTrama$,86, 3))														! Numero documentos
       Gr.Fosfec.Trama$ = Mid$(XTrama$,89,(Gr.Fosfec.Ndoc%*12))             ! Extrae lista documentos
       Xbuffer$  = Gr.Fosfec.TipDoc$                         +             \! Tipo documento
                   ":"+Gr.Fosfec.IdBene$                     +             \! ID beneficiario
                   ":"+Gr.Fosfec.Benef$                      +             \! Credencial beneficiario
                   ":"+Pack$(Gr.Fosfec.Vlr$)                 +             \! Valor subsidio entregado
                   ":"+Pack$(str$(Gr.Fosfec.TipEnt%))        +             \! Tipo de subsidio pagado
                   ":"+Pack$("00")                                          ! Filler 
       Call Grabacion.String.Usuario2("20160122",Xbuffer$)                  ! Graba String usuario  
   	   COBRO.SUBSIDIO.FOSFEC = -1		 																	      !
   EndIf Else Begin 																												!
   		 Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
   		 COBRO.SUBSIDIO.FOSFEC = 0																				    !
   		 Gr.Fosfec.Trx% = 0
   EndIf 																																		!
End Function

Function CONFIRMAR.FOSFEC
Integer*1 CONFIRMAR.FOSFEC																							    !
String XR$, Xtemp4$, Xtrama$,Xsnd$, Xfin$, Xdocs$     										  !
String Xbuffer$, Xvpago$, Xtbenef$, Xopt$
Integer*4 Xs%
 Xvpago$ = Gr.Fosfec.ViaPago$
 Xtbenef$ = "03"																														! 
 Asc.Pay.Impr% = 0																												  ! Aumenta contador de trx
 If Gr.Fosfec.TipEnt% = 1 Then Begin																				! Pago Monetario
    Gr.Fosfec.Appl$ = "11"                                           	      ! Codigo aplicacion
    !Gr.Fosfec.Func$ = "52"                                                  ! Codigo funcion
    
    Gr.Fosfec.Func$ = "92"                                                  ! Codigo funcion New 10Abr2019
   Xdocs$ = ""																															! Lista documentos
   Xdocs$ = Left$((Gr.Fosfec.Trama$+String$(120," ")),120)                  ! Ajusta dato 
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora del requerimiento
   Xbuffer$ =  Right$("    "+Gr.Fosfec.TipDoc$,4)    +                     \! Tipo documento
               Xvpago$             +																			 \! Via de pago
               Xtbenef$            +																			 \! Tipo de beneficio
               Right$("000000000000"+Gr.Fosfec.Vlr$,12)  +                 \! Valor de subsidio
               Right$("                    "+Gr.Fosfec.IdBene$,20) +       \! ID beneficiario
               Right$("000"+Str$(Gr.Fosfec.Ndoc%),3)     +                 \! Numero de documentos
               Xdocs$                                                       ! Lista documentos            
 EndIf Else Begin																														! Pago Alimentacion
    Gr.Fosfec.Appl$ = "11"                                           	      ! Codigo aplicacion
    Gr.Fosfec.Func$ = "82"                                                  ! Codigo funcion
    Xvpago$ = "I"																														! Via de pago
    Xtbenef$ = "06"																													! Tipo de beneficio
    Xdocs$ = ""																															! Lista documentos
    Xdocs$ = Left$((Gr.Fosfec.Trama$+String$(120," ")),120)                 ! Ajusta dato 
    Xsnd$ = DATE$ +":"+ Time$                                               ! Fecha y hora del requerimiento
    Xbuffer$ = Right$("    "+Gr.Fosfec.TipDoc$,4)    +                     \! Tipo documento
               Xvpago$             +																			 \! Via de pago
               Xtbenef$            +																			 \! Tipo de beneficio
               Right$("000000000000"+Gr.Fosfec.Vlr$,12)  +                 \! Valor de subsidio
               Right$("                    "+Gr.Fosfec.IdBene$,20) +       \! ID beneficiario
               Right$("000"+Str$(Gr.Fosfec.Ndoc%),3)     +                 \! Numero de documentos
               Xdocs$                                                       ! Lista documentos            
 EndIf																																			!
 proc.pago.fosfec:
   XTemp4$ = Armar.Trama.Msg(Gr.Fosfec.Appl$,Gr.Fosfec.Func$,Xbuffer$,"00","0001","123456")       ! Armar trama MSG
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)         ! Ejecuta Requerimiento
   
   !Xtrama$ = "1182046200000ACTUALIZACION SE REALIZO DE MANERA SATIF"
   
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call FOSFEC.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                     ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
    If Gr.Fosfec.TipEnt% = 2 Then Begin  																		! Si pago de alimento
      FOSFEC.REINTENTA:     
      Xopt$ = ASIC.DATOS$("REINTENTA PROCESO ","1.Si 0.No          ")       ! Mod 07Mzo2018 GR
      If Val(Xopt$) < 0 Or Val(Xopt$) > 1 Then BEGIN 					  						! Proceso cancelado   
         GoTo FOSFEC.REINTENTA
      EndIf 																																!                     			
      If Xopt$ = "0" Then Begin
       	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")										!                     			
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!                     			
         TS.IO.MOTORKEY = 73																								!         
         CONFIRMAR.FOSFEC = 0 		 																                ! Error del proceso
         Gr.Fosfec.Trx% = 0
         Exit Function 																											!                     	
      EndIf
      If Xopt$ = "1" Then Begin
      	 Call VISORES4690(1,"PROCESANDO NUEVAMENTE","ESPERE POR FAVOR",0,"L")
         Gr.Fosfec.Func$ = "86"
         GoTo proc.pago.fosfec
      EndIf
    Endif Else Begin																												! 
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!                     			
         TS.IO.MOTORKEY = 73																								!                     			
         Gr.Fosfec.Trx% = 0
         CONFIRMAR.FOSFEC = 0 		 																          ! Error del proceso
         Exit Function 																											!                     	
    EndIf
   EndIf
   XTEMP4$ = Mid$(XTrama$,12,2)	           																  ! Valida rta entregada
   If XTemp4$ = "00" Then Begin 																					  ! Rta Satisfactoria
   	  Gr.Fosfec.Trama$ = XTrama$																						! Trama para anulaciones
      CONFIRMAR.FOSFEC = -1 																			          !
      Exit Function 																												! 
   EndIf 
   Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									              ! Presenta Msg Error
   CONFIRMAR.FOSFEC = 0 		 																                ! Error del proceso
   Gr.Fosfec.Trx% = 0
End Function 

Sub Fosfec.Acum.Items(Itm$, Qty%, Pvt%, Dscto%, Opr%, Xprices$,Xdcom%)
String Itm$, Opr$, Xprices$, x$, Xcomer$
Integer*4 Qty%, Pvt%, Dscto%, Xi%, Xpos%, Gnro%, Xdcom%
Integer*1 Fnd%, XSgn%, Opr%
Fnd% = 0
For Xi% = 1 To Gr.RsmItm.Xcont%																							! Lista de productos
    If Val(Gr.Fosfec.RsmItm$(Xi%,0)) = Val(Itm$) Then  										 \! Encontrado
    	 Fnd% = -1 : Xpos% = Xi%																							!
Next Xi%																																		!
If Fnd% = 0 Then Begin																											! No existe
	 Gr.RsmItm.Xcont% = Gr.RsmItm.Xcont% + 1																	! Aumenta contador
	 Gnro% = Gr.RsmItm.Xcont%																									!
	 If Opr% = -1 Then Xsgn% = -1 Else Xsgn% = 1															! Signo Movimiento
   TS.TEMP2$ = Pack$(Right$("000000000000"+Itm$,12))				  				      ! Armo dato para busqueda
   Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)                					! Lectura Datos Itemr
   Call IRRFEC.SPLIT1( TS.TEMP1$ ) 			                           				  ! Entrega datos al eamitemr.j86
   Xcomer$ = Left$(IR.USERDATA$,16)																				  ! Estructura comercial
	 Gr.Fosfec.RsmItm$(Gnro%,0) = Itm$																				! Almacena Item
	 Gr.Fosfec.RsmItm$(Gnro%,1) = Str$(Qty% * Xsgn%)													! Almacena Qty
	 Gr.Fosfec.RsmItm$(Gnro%,2) = Str$(Pvt% * Xsgn%)												  ! Almacena Precio Total
	 Gr.Fosfec.RsmItm$(Gnro%,3) = Xcomer$ 																		! estructura comercial
	 Gr.Fosfec.RsmItm$(Gnro%,4) = Xprices$																		! Almacena PvP
	 Gr.Fosfec.RsmItm$(Gnro%,5) = IR.ITEMNAME$                                ! Descriptor 
EndIf Else Begin																														! Si Existe
	 If Opr% = -1 Then Xsgn% = -1 Else Xsgn% = 1															! Signo Movimiento
	 Gr.Fosfec.RsmItm$(Xpos%,1) = Str$( Val(Gr.Fosfec.RsmItm$(Xpos%,1)) + (Qty% * Xsgn%))	! Actualiza cantidades
	 Gr.Fosfec.RsmItm$(Xpos%,2) = Str$( Val(Gr.Fosfec.RsmItm$(Xpos%,2)) + (Pvt% * Xsgn%))	! Actualiza Totales
	 Gr.Fosfec.RsmItm$(Xpos%,4) = Xprices$																		! Almacena PvP
EndIf
End Sub 

Function Valida.Itms.Subsidio																								!
  Integer*1 Valida.Itms.Subsidio, Xrta%																			!
  Integer*4 XI%, XInd2%, X.J%, Xqty%, Xfind%, Xdct%, Xpric%, XInd1%, Xj%		!
  Integer*2 Xsgn%, Xpos%, Xtax%																							!
  String    XTMP$, Xitm$, XPric$, Xdummy$, Xkey$, Xbasura$, Xiva$, Xsave$, \! 
            XTar$, Xind$, Xpir$, Xpuv$, XCct$, XCnt$, Xvta$                 !
  Gr.RsmItm.Xcont% = 0 																											! Numero de Items
  Gr.RsmItm.Ycont% = 0                                                      ! Numero de items excluidos
  Dim Gr.Fosfec.RsmItm$(700,5)		 															    				! Lista de Items 
  Dim Gr.Fosfec.ExcItm$(700,5)																							! Lista items excluidos
  FOR X.J% = 1 TO SL.END                    																! FOR ALL StringS
    H$ = READ.SL.STR$(X.J%)                 																! GET String
    If LEN(H$) > 5 Then  Begin                                              ! ASSURE GOOD String
     If ASC(H$)  = 1 Then Begin             																! ITEM ENTRY String
     	  Asc.Tmp.Apun% = 3																										!
        XItm$   = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Item Vendido
        Xpric$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Precio Venta
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Departamento
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Family 
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Indicat 1
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Indicat 2
        XInd2%  = Val(Xdummy$)																							! Vlr numerico Indicat 2
        Xsgn% = 1 																													! Trx de compra
        Xqty% = 0                                                           ! Sin cantidad o peso
        If (Xind2% And 0080H) Then Xsgn% = -1                               ! Item anulado
        If (Xind2% And 0040H) Then Xsgn% = -1                               ! Item anulado
        If (Xind2% And 0004H) Then                                         \! Reduccion de precio
           Xpric% = (Val(Xpric$) *-1) Else                                 \!
           Xpric% = Val(Xpric$)																							!
        Call Fosfec.Acum.Items(XItm$, 1, Xpric%, 0, Xsgn%, Xpric$,0)				!
     EndIf                                  																! ITEM ENTRY String
     Asc.Tmp.Apun% = 3																										  !     
     If ASC(H$)  = 2 Then Begin             																! ITEM ENTRY String Extention
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! MPGROUP       
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! DEALQUAN      
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! METODO PRECIO 
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! PRECIO        
        XPric$   = Right$(Xdummy$,5)																				!
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! CANTIDAD      
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! CANTIDAD      
        Xqty% = Val(Xdummy$)																								!
        If Xsgn% = 1 Then                                                  \!
           Call Fosfec.Acum.Items(XItm$, 1, 0, 0,-1,"0",0) Else 				   \! Retira el Item del S1
           Call Fosfec.Acum.Items(XItm$, 1, 0, 0,1,"0",0)      							! Retira el Item del S1
        Call Fosfec.Acum.Items(XItm$, Xqty%, 0, 0, Xsgn%, Xpric$,0)         !
     EndIf																																	! ITEM ENTRY String Extention
    EndIf																																		!
  NEXT X.J%   																															!
  X.J% = 0
  TS.ER.RETURN = -1
  Open "R::TF:ECFOSF" KEYED RECL 8 AS 94																	  ! Estructuras excluidas fosfec
  If TS.ER.RETURN <> -1 Then Begin																					! Error de apertura
  	 Valida.Itms.Subsidio = 0																								! No permite subsidio
  	 Exit Function 																													!
  EndIf																																			!
  XRta% = 0 																																! Respuesta
  For X.J% = 1 To Gr.RsmItm.Xcont%           																! Recorre vector venta itms
    If Val(Gr.Fosfec.RsmItm$(X.J%,1)) <> 0 Then Begin                       ! Hay venta registrada +/-
      !Xitm$ = Gr.Fosfec.RsmItm$(X.J%,3)																		! Estructura a validar
      Xitm$ = Gr.Fosfec.RsmItm$(X.J%,0)																			! Plu a validar Add 26Nov2019
      Xitm$ = "1" + Right$(String$(15,"0")+Xitm$,15)                        ! Ajusta dato para busqueda
      Xitm$ = Pack$(Xitm$)																									! Arma llave 
      !Call VISORES4690(1,"VALIDANDO..",UNPACK$(XITM$),500,"L")
      
      TS.ER.RETURN = -1
      Read Form "C8";#94 Key Xitm$; Xtmp$            												! Realiza consulta
      If TS.ER.RETURN = -1 Then Begin                                       ! Reporta Item Excluido      
      	 XRta% = -1																													! Estructura excluida de subsidio
      	 Gr.RsmItm.Ycont% = Gr.RsmItm.Ycont% + 1                            ! Total Excluidos
	       Gr.Fosfec.ExcItm$(Gr.RsmItm.Ycont%,0) = Gr.Fosfec.RsmItm$(x.j%,0)  ! Almacena Item
	       Gr.Fosfec.ExcItm$(Gr.RsmItm.Ycont%,1) = Gr.Fosfec.RsmItm$(x.j%,1)  ! Almacena Qty
      EndIf
    EndIf
  Next X.J%																																	! Fin recorrido venta itms
  Close 94  
  Valida.Itms.Subsidio = Xrta%																							! Reporta validacion
End Function 																																!

Sub REVERSAR.ITMS.NO.VALIDOS																								! Reversa Items no validos
String Xqty$
 If Gr.RsmItm.Ycont% <> 0 Then Begin
     Call U.IMPRIME("*** DEVOLUCION ITEMS NO AUTOR.***",4101H)              !
     Call U.IMPRIME("*** DEVOLUCION ITEMS NO AUTOR.***",2100H)              !
     Dim TS.IO.DATA$(10)
     Dim TS.IO.KEYS(10)
     Gr.Fosfec.InAnul% = -1
     Gr.Fosfec.Cont% = 1
     TS.IO.KEYS(2)  = 80
     TS.IO.DEVICE   = 1
     TS.IO.DATA$(2) = Gr.Fosfec.ExcItm$(1,0)																! Item a devolver
     Xqty$ = Gr.Fosfec.ExcItm$(1,1)																          ! Qty a devolver
     If Val(Xqty$) < 0 Then Begin																						! Hay reintegro fuera tiquete
     	  Xqty$ = Str$( Val(Xqty$) * -1)                                      ! Invierte signo
     	  TS.IO.KEYS(1)  = 0
     EndIf																																	!
     TS.IO.MOTORKEY = 80
     IR.ITEMCODE$   = PACK$(Right$("000000000000"+Gr.Fosfec.ExcItm$(1,0),12)) !
     %INCLUDE EAMIRRD1.J86
     If IR.INDICAT0 AND 40H Then Begin																			!
        TS.IO.KEYS(6)  = 72																									!
        TS.IO.DATA$(6) = Xqty$																							! Peso capturado
     EndIf Else Begin																												!
        TS.IO.KEYS(6)  = 75																									!
        TS.IO.DATA$(6) = Xqty$																							! Unidades vendidas
     EndIf
 EndIf
End Sub 																																		! Fin reverso items no validos

Sub Reclamo.Fosfec  																												! Cobro subsidio
Integer*2 Xcont%, XK%, XCant%
String Xmsg1$, Xmsg2$, Xdata$, XvlrTrx$
                                                                                                    
 If Gr.Fosfec.TipEnt% = 1 Then Begin																				! Pago monetario

          Gr.Fosfec.Appl$ = "11"                                           	! Codigo aplicacion
          !Gr.Fosfec.Func$ = "51"                                            ! Codigo funcion
          
          Gr.Fosfec.Func$ = "91"                                            ! Codigo funcion New 10Abr2019
          TS.TEMP5I2 = 0                    													      ! Si proceso correcto
          TS.TEMP5I2 = COBRO.SUBSIDIO.FOSFEC																! Cobro subsidio
      	 IF TS.TEMP5I2 <> 0 Then Begin																			! Si proceso correcto
      	    If Gr.Fosfec.Benef$ = "" Then TS.TEMP5$ = Gr.Fosfec.IdBene$ Else \!
      	    	 TS.TEMP5$ = Gr.Fosfec.Benef$
      	    Call VISORES4690(1,"CONFIRMANDO SUBSIDIO","ESPERE POR FAVOR",0,"L")
            TS.TEMP5I2 = CONFIRMAR.FOSFEC                                   ! Confirma movimiento pago subsidio
            If TS.TEMP5I2 <> -1 Then Begin																	! Falla confirmacion pago
               Call Gr.Init.Trx																							! Init Trx
               TS.IO.STATE    = 10         																  ! PREPARE FOR NORMAL SALES STATE
               Gr.Fosfec.Trx% = 0
               Exit Sub 
            EndIf
      	 	  Call Format.Amount( Val(Gr.Fosfec.Vlr$) )                       ! Formatea valor a entregar
      	    Call TSHIECET
      	    Call VISORES4690(2,"VALOR A ENTREGAR","$"+TS.TEMP1$,0,"L")
      	    Call VISORES4690(1,"VALOR A ENTREGAR","$"+TS.TEMP1$,1500,"L")
            Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 												! Init vectores de carga
            TS.IO.DATA$(3) = Right$(Gr.Fosfec.TipVar$,1)                    ! Variedad de Pago
            TS.IO.KEYS(3)  = 78                                             ! Tecla Slash
            TS.IO.DATA$(7) = Gr.Fosfec.Vlr$																	! Valor a entregar
            TS.IO.DATA$(9) = TS.TEMP5$      																! ID del Beneficiario
            TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.Fosfec.TipVar$,1))           ! Tipo de Pago
            TS.IO.MOTORKEY = TS.IO.KEYS(7)																	!
         EndIf Else Begin 																									! Falla Confirmacion
            Call Gr.Init.Trx																								! Init Trx
            TS.IO.STATE    = 10            																  ! PREPARE FOR NORMAL SALES STATE
            Gr.Fosfec.Trx% = 0
            Gr.Fosfec.TrxCed% = 0
         EndIf																															!
 EndIf																																			!
        
 If Gr.Fosfec.TipEnt% = 2 Then Begin																				! Pago Alimentos
    TS.TEMP5I2 = 0                    													      			! Si proceso correcto
    TS.TEMP5I2 = VALIDA.ITMS.SUBSIDIO        																! Analiza compra 
    If TS.TEMP5I2 = -1 Then Begin																						!
       Call VISOR.AND.BORRAR("ITEMS NO AUTORIZADOSEN LA COMPRA /Borrar")		!
       Call VISOR.AND.BORRAR("SUBSIDIO NO FUE     APLICADO     /Borrar")		!
       Gr.Fosfec.Vlr$	= ""																									! Valor a entregar
       Gr.Fosfec.Benef$ = ""																								! ID del Beneficiario
       Gr.Fosfec.TRama$ = ""                                           			! Trama del mensaje
       Gr.Fosfec.Trx% = 0																										!
       Gr.Fosfec.TrxCed% = 0																								!
       Gr.Fosfec.Captura$ = ""																							!
       Gr.Fosfec.TipEnt% = 0
       Call REVERSAR.ITMS.NO.VALIDOS                                        !
       Exit Sub                                                             !
     EndIf 	        																												!
     Gr.Fosfec.Appl$ = "11"                                           	    ! Codigo aplicacion
     Gr.Fosfec.Func$ = "81"                                                 ! Codigo funcion
     TS.TEMP5I2 = 0                    													            ! Si proceso correcto
     TS.TEMP5I2 = COBRO.SUBSIDIO.FOSFEC																      ! Cobro subsidio
   	 If TS.TEMP5I2 <> 0 Then Begin																			    ! Si proceso correcto
        If (Val(Gr.Fosfec.Vlr$) > ( TS.BALDUE(0) )) And                    \! Si dato supera total compra
           Gr.Fosfec.Anul% = 0 Then Begin                                   ! y no esta en anulacion
    	     Call VISOR.AND.BORRAR("SUBSIDIO SUPERA EL  VALOR DE LA COMPRA")  !
    	     Call Format.Amount(Val(Gr.Fosfec.Vlr$))                          ! 
    	     Call VISOR.AND.BORRAR("MONTO SUBSIDIO AUTOR$"+TS.TEMP1$)
           Call VISOR.AND.BORRAR("NO APLICA SUBSIDIO  EN LA COMPRA/Borra")  !
           TS.TEMP5I2 = 0																										!
           Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 											  ! Init vectores de carga
           TS.IO.MOTORKEY = 73																						  !
           Gr.Fosfec.Vlr$	= ""																						  ! Valor a entregar
           Gr.Fosfec.Benef$ = ""																					  ! ID del Beneficiario
           Gr.Fosfec.TRama$ = ""                                            ! Trama del mensaje
           Gr.Fosfec.Trx% = 0																						  	!
           Gr.Fosfec.TrxCed% = 0																					  !
           Gr.Fosfec.Captura$ = ""																				  !
           Exit Sub 																												!
        EndIf																																! 
 	      If Gr.Fosfec.Benef$ = "" Then TS.TEMP5$ = Gr.Fosfec.IdBene$ Else   \! 
 	    	   TS.TEMP5$ = Gr.Fosfec.Benef$																		  !
 	      Call VISORES4690(1,"CONFIRMANDO SUBSIDIO","ESPERE POR FAVOR",0,"L") !
 	      TS.TEMP5I2 = 0																											!
        TS.TEMP5I2 = CONFIRMAR.FOSFEC                                       ! Confirma movimiento pago subsidio
     	  IF TS.TEMP5I2 <> 0 Then Begin																			  ! Si proceso correcto
           TS.TEMP5I1 = -1																									! Reporta aplicacion del pago 
           Exit Sub 																												!
        EndIf Else Begin 																									  ! Falla Confirmacion
        	 Call VISOR.AND.BORRAR("SUBSIDIO NO FUE     APLICADO     /Borrar")!
           TS.TEMP5I2 = 0																										!
           Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 											  ! Init vectores de carga
           TS.IO.MOTORKEY = 73																						  !
           Gr.Fosfec.Vlr$	= ""																						  ! Valor a entregar
           Gr.Fosfec.Benef$ = ""																					  ! ID del Beneficiario
           Gr.Fosfec.TRama$ = ""                                            ! Trama del mensaje
           Gr.Fosfec.Trx% = 0																						  	!
           Gr.Fosfec.TrxCed% = 0																					  !
           Gr.Fosfec.Captura$ = ""																				  !
           Exit Sub 																												!
        EndIf																																! Fin confirmación
     EndIf Else Begin																												! Proceso fallido pago
        Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 													  ! Init vectores de carga
        TS.IO.MOTORKEY = 73																							    !
        Gr.Fosfec.Vlr$	= ""																						    ! Valor a entregar
        Gr.Fosfec.Benef$ = ""																						    ! ID del Beneficiario
        Gr.Fosfec.TRama$ = ""                                               ! Trama del mensaje
        Gr.Fosfec.Trx% = 0																						    	!
        Gr.Fosfec.TrxCed% = 0																						    !
        Gr.Fosfec.Captura$ = ""																					    !
     EndIf																																	! Fin cobro subsidio
 EndIf																																			!

End Sub 

Function Anular.Pago.Fosfec																									! Anulacion pago en compra
Integer*1 Anular.Pago.Fosfec
String XR$, Xtemp4$, Xtrama$,Xsnd$, Xfin$, Xdocs$     										  !
String Xbuffer$

 Gr.Fosfec.Appl$ = "11"                                           	      	! Codigo aplicacion
 Gr.Fosfec.Func$ = "85"                                                  		! Codigo funcion

   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora del requerimiento
   Xbuffer$ =  Gr.Fosfec.TipDoc$                                   +       \! Tipo de documento
               Right$("                    "+Gr.Fosfec.IdBene$,20) +       \! ID beneficiario
               Gr.Fosfec.TRama$																							! Trama de pago 
   XTemp4$ = Armar.Trama.Msg(Gr.Fosfec.Appl$,Gr.Fosfec.Func$,Xbuffer$,"00","0001","123456")       ! Armar trama MSG
   XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)         ! Ejecuta Requerimiento
   
   !Xtrama$ = "1182046200000ACTUALIZACION SE REALIZO DE MANERA SATIF"
   
   
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call FOSFEC.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                     ! Rastreo movimiento
   XTEMP4$ = Valida.Rta(XTrama$)																			      ! Valida rta entregada
   If Xtemp4$ <> "00" Then Begin 																						!
      Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									          ! Presenta Msg Error
      Anular.Pago.Fosfec = 0
   	  Exit Function
   EndIf
   XTEMP4$ = Mid$(XTrama$,12,2)	           																  ! Valida rta entregada
   If XTemp4$ = "00" Then Begin 																					  ! Rta Satisfactoria
      Anular.Pago.Fosfec = -1


      Xbuffer$  = Gr.Fosfec.TipDoc$                          +             \! Tipo documento
                   ":"+Gr.Fosfec.IdBene$                     +             \! ID beneficiario
                   ":"+Gr.Fosfec.Benef$                      +             \! Credencial beneficiario
                   ":"+Pack$(Gr.Fosfec.Vlr$)                 +             \! Valor subsidio entregado
                   ":"+Pack$(str$(Gr.Fosfec.TipEnt%))        +             \! Tipo de subsidio pagado
                   ":"+Pack$("00")                                          ! Filler 
      Call Grabacion.String.Usuario2("20190409",Xbuffer$)                   ! Graba String usuario  

      Exit Function 																												! 
   EndIf 
   Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									              ! Presenta Msg Error
   Anular.Pago.Fosfec = 0 	 																                ! Error del proceso

End Function 																																! Fin anulacion pago compra

Sub SUBSIDIO.FOSFEC(Xopt%) Public
Integer*4 Xopt%

!--- EAMTSU07.J86
If XOPT% = 07 Then Begin																										! Carga de opciones
  Gr.Fosfec.Act% = 0 																										    ! Init proyecto activo
  Dim GR.NEGCNT(8)
  Dim GR.NEGAMT(8)
  TS.TS11WERR$ = ""																													! Control errores
  OPEN "R::$SCNTRL" AS 94 UNLOCKED NOWRITE NODEL												    ! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR APERTURA PARAMETROS MPAC   ")             !
	   Gr.Fosfec.Act% = 0																										  ! Init proyecto activo  	 
  	 Exit Sub																																!
  EndIf 
  If End #94 Then UE.FIN.MPAC           																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[SUBSIDIO FOSFEC]" Then Begin		     	 					    ! Parametros Fosfec
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Fosfec.Act% = Val(Mid$(TS.TEMP1$,30,2))      			    					! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Fosfec.Sec% = Val(Mid$(TS.TEMP1$,30,3))      			    					! Tecla Motora Asignada
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Fosfec.TipVar$ = Mid$(TS.TEMP1$,30,2)       			    				  ! Tipo y variedad definida
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Fosfec.ViaPago$ = Ucase$(Mid$(TS.TEMP1$,30,1))		    				  ! Via de pago
         Call FOSFEC.BUSCA.LIMITE.TV(Gr.Fosfec.TipVar$)                     ! Ajuste limites pago         
         GoTo UE.FIN.MPAC                                                   ! Sale del proceso
        EndIf																																!
  Wend 																																		  ! Fin recorrido archivo
  UE.FIN.MPAC:
    Close 94 
    If Gr.Fosfec.Act% = -1 Then                                            \! Proyecto Activo
      Call U.IMPRIME("MODULO SUBS. FOSFEC    ON 27/Jul/2021",2100H) Else   \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO SUBS. FOSFEC   OFF 27/Jul/2021",2100H)         ! Msg Proyecto Cargado
    
EndIf

If Gr.Fosfec.Act% = 0 Then Exit Sub 																				! Si proyecto apagado

!--- EAMTSU02.J86
If XOPT% = 02 Then Begin																										! Inicio de transaccion
	 If Gr.Fosfec.Trx% = -1 Then Begin																				! Pago subsidio
	 	  Call Voucher.Fosfec																										! Genera voucher
	 EndIf																																		!
   Gr.Fosfec.Vlr$	= ""																											! Valor a entregar
   Gr.Fosfec.Benef$ = ""																										! ID del Beneficiario
   Gr.Fosfec.TRama$ = ""                                                    ! Trama del mensaje
   Gr.Fosfec.Trx% = 0
   Gr.Fosfec.TrxCed% = 0
   Gr.Fosfec.Captura$ = ""
   Gr.Fosfec.Consulta% = 0
   Gr.Fosfec.InAnul% = 0
   Gr.Fosfec.Cont% = 0
   To.TENDLIMITS(Gr.Fosfec.TVPOS%,0) = Gr.Fosfec.MaxTv%										! Retorna valores default
   To.TENDLIMITS(Gr.Fosfec.TVPOS%,1) = Gr.Fosfec.ChgTv%										! Retorna valores default
   TS.TEMP1I1 = 0
   For TS.TEMP1I1 = 0 To 7
       TO.NEGAMT(TS.TEMP1I1) = GR.NEGAMT(TS.TEMP1I1)
       TO.NEGCNT(TS.TEMP1I1) = GR.NEGCNT(TS.TEMP1I1)
   Next TS.TEMP1I1
EndIf


!--- EAMTSU14.J86
If XOPT% = 14 Then Begin																										! En secuencia de tecleo
	
	 If TS.IO.MOTORKEY <> 69 And Gr.Fosfec.TrxCed% = -1 Then Begin            ! 
  	  Call VISOR.AND.BORRAR("SECUENCIA REGISTRO   NO AUTORIZADA...")     		! Msg Alerta
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															!
      TS.IO.MOTORKEY = 73																										!
      Gr.Fosfec.TrxCed% = 0
      Gr.Fosfec.Captura$ = ""
      Exit Sub 																											    		! Termina proceso
	 EndIf

	 If TS.IO.MOTORKEY = 69 And Gr.Fosfec.TrxCed% = -1 Then Begin             ! 
	 	  TS.TEMP1$ = TS.IO.DATA$(10)																						! Longitd dato
	 	  If Val(TS.TEMP1$) >= 531 Then Begin																		! Si cc Colombia
	 	     Gr.Fosfec.Captura$ = CEDULA.PDF417
         Gr.Fosfec.TipDoc$ = "CO1C"																					!
         Gr.Fosfec.IdBene$ = Gr.Fosfec.Captura$															! Numero id beneficiario
         Gr.Fosfec.Benef$  = ""																							!
         Gr.Fosfec.TrxCed% = 0
	 	     GoTo Pago.Fosfec
	 	  EndIf Else Begin
	 	     Call VISOR.AND.BORRAR("DATO ESCANEADO NO ES UNA CEDULA VALIDA")		!
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Gr.Fosfec.TrxCed% = 0 																							! Cancela proceso
         Gr.Fosfec.Captura$ = ""
         Exit Sub 																											    ! Termina proceso
	 	  EndIf
   EndIf

  If TS.IO.KEYS(1) = 82 AND \                       											! Proceso de suspension de
     TS.IO.KEYS(6) = 81 AND \                       											! Transaccion con una subsidio
     TS.PROCEDURE < 1 Then Begin                    											! Capturado
    If Gr.Fosfec.Trx% = -1 Then Begin        															! Subsidio aplicado
      Call Visor.And.Borrar("NO SE PERMITE SUSPENDER TRANSACCION")				!
      TS.IO.MOTORKEY = 0                            											! Inicializa secuencia de 
      Dim TS.IO.KEYS(10) : Dim TS.IO.DATA$(10)      											! Tecleo
      Exit Sub 																														!
    EndIf      
  EndIf
 
	 If TS.IO.MOTORKEY = Gr.Fosfec.Sec% Then Begin														! Si tecla de secuencia
	 	 IF TS.INTRX Then                                                      \! Si esta dentro de una trx venta
  	  If TS.TRX.STATUS <> 1 And TS.PROCEDURE = -1 THEN BEGIN				  	    ! No se ha ingresado total en venta
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 0 																									! 
       TS.GUIDANCE = 1020																										! Solicita total a la trx
       Exit Sub 																														! Sale del proceso
      EndIf 																																!

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
	 	
	 	  If (Gr.Fosfec.Trx% = -1) And (TS.IO.KEYS(1) <> 70) Then Begin					! Existe un pago en proceso
	 	  	 Call VISOR.AND.BORRAR("EXISTE UN SUBSIDIO   EN PROCESO      ")     ! Msg Alerta
         Call VISOR.AND.BORRAR("PROCESO NO AUTORIZADO         /Borrar")     ! Msg Alerta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Exit Sub 																											    ! Termina proceso
	 	  EndIf	
	 	  If (TS.IO.KEYS(1) = 70) And (Gr.Fosfec.Trx% = -1) Then Begin 				  ! Anulando movimiento
	 	  	Call VISORES4690(1,"ANULANDO PAGO FOSFEC","ESPERE POR FAVOR",500,"L")
 	      TS.TEMP5I2 = 0																											!
        TS.TEMP5I2 = Anular.Pago.Fosfec																			! Anulación del pago
     	  IF TS.TEMP5I2 <> 0 Then Begin																			  ! Si proceso correcto
           Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 												! Init vectores de carga
           TS.IO.KEYS(1) = 70								  															! Anulando movimiento
           TS.IO.DATA$(3) = Right$(Gr.Fosfec.TipVar$,1)                     ! Variedad de Pago
           TS.IO.KEYS(3)  = 78                                              ! Tecla Slash
           TS.IO.DATA$(7) = Gr.Fosfec.Vlr$																	! Valor a entregar
           TS.IO.DATA$(9) = TS.TEMP5$      																  ! ID del Beneficiario
           TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.Fosfec.TipVar$,1))            ! Tipo de Pago
           TS.IO.MOTORKEY = TS.IO.KEYS(7)																	  !

           Gr.Fosfec.Vlr$	= ""																						  ! Valor a entregar
           Gr.Fosfec.Benef$ = ""																						! ID del Beneficiario
           Gr.Fosfec.TRama$ = ""                                            ! Trama del mensaje
           Gr.Fosfec.Trx% = 0																						   	!
           Gr.Fosfec.TrxCed% = 0																						!
           Gr.Fosfec.Captura$ = ""																					!
!--- Falta grabacion String de anulacion           
           Exit Sub 																												!
        EndIf Else Begin 																									  ! Falla Confirmacion
        	 Call VISOR.AND.BORRAR("SUBSIDIO NO FUE     ANULADO      /Borrar")!
           Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
           TS.IO.MOTORKEY = 73																								!
           Exit Sub 																											    ! Termina proceso
        EndIf        
        
   	    Exit Sub  
	 	  EndIf																																  ! 

	 	  If (Gr.Fosfec.Trx% <> -1) And (TS.IO.KEYS(1) = 70) Then Begin					! Existe un pago en proceso
	 	  	 Call VISOR.AND.BORRAR("NO EXISTE UN SUBSIDIOEN PROCESO      ")     ! Msg Alerta
         Call VISOR.AND.BORRAR("PROCESO NO AUTORIZADO         /Borrar")     ! Msg Alerta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Exit Sub 																											    ! Termina proceso
	 	  EndIf																																	!
	 	

      TS.TEMP5$ = ASIC.DATOS$("SUBSIDIO A PAGAR  ","1.MONET. 2.ALIMENT.")   ! Mod 07Mzo2018 GR
      If Val(TS.TEMP5$) <= 0 Or Val(TS.TEMP5$) > 2 Then BEGIN 							! Proceso cancelado   
       	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")										!                     			
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!                     			
         TS.IO.MOTORKEY = 73																								!                     			
         Exit Sub 																													!                     			
      EndIf 																																!                     			
      Gr.Fosfec.TipEnt% = Val(TS.TEMP5$)																		! Tipo de subsidio
      If Val(TS.TEMP5$) = 1 Then GoTo Captura.Fosfec
      If Val(TS.TEMP5$) = 2 Then GoTo Captura.Fosfec.Alimento

Captura.Fosfec.Alimento:
	 	  IF NOT TS.INTRX THEN BEGIN                      											! si no esta en una trx
	 	  	 Gr.Fosfec.Consulta% = -1																						! Consulta cupo subsidio
!	 	  	 Call VISOR.AND.BORRAR("SUBSIDIO SOLO USAR  EN COMPRAS   /Borrar")  ! Msg Alerta
!         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
!         TS.IO.MOTORKEY = 73																								!
!         Exit Sub 																											    ! Termina proceso
	 	  EndIf
	 	  
      Gr.Fosfec.Anul% = 0																										! 
      !TS.TEMP5$ = ASIC.DATOS$("TIPO DOC. 1.CEDULA","2.EXTRAN 3.PERMANEN")   ! Mod 07Mzo2018 GR
      
      TS.TEMP5$ = ASIC.DATOS$("DOC 1.CEDULA 2.EXTR.","3.PERMANEN 4.DIGITAL")! Mod 27jUL2021 GR
      
      If Val(TS.TEMP5$) <= 0 Or Val(TS.TEMP5$) > 4 Then BEGIN 							! Proceso cancelado   
       	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")										!                     			
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!                     			
         TS.IO.MOTORKEY = 73																								!                     			
         Exit Sub 																													!                     			
      EndIf 																																!                     			
      If TS.TEMP5$ = "1" Then BEGIN 																				! Captura por cedula
         TS.ER.RETURN = -1
         Call CREAR.PIPE.PDF
         If TS.ER.RETURN = -1 Then Begin 
           Call VISORES4690(1,"ESCANEE CEDULA","DEL BENEFICIARIO..",0,"L")  ! 
           Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  !
           TS.IO.MOTORKEY = 0 																						  !
           Gr.Fosfec.TrxCed% = -1
           If (TS.IO.KEYS(1) = 70) Then Begin 															! Anulando movimiento
           	  Gr.Fosfec.Anul% = -1																					! 
           EndIf																														!
           Exit Sub 																											  !
         EndIf Else Begin
         	Call VISOR.and.borrar("FALLA DEL PIPE")  ! 
           Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  !
           TS.IO.MOTORKEY = 73 																						  !
           Exit Sub 																											  !
         EndIf
      EndIf

      If TS.TEMP5$ = "2" Then BEGIN 																				! Captura por cedula de extranjeria 

         TS.TEMP2I4  = 0																										! Control proceso
         Call SALVAR.SECUENCIA
         Call Entrada.Autorizacion																					! Entrada autorizacion al proceso	
         If TS.TEMP2I4  = 0 Then Begin																			!
       	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								!
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!
            TS.IO.MOTORKEY = 73																						  !
            Exit Sub 																											  !
         EndIf																															!
         Call RESTAURAR.SECUENCIA

         TS.TEMP5$ = ASIC.DATOS$("INGRESE CED. EXTR.","DEL BENEFICIARIO..")	!
         If TS.TEMP5$ = "E" Then BEGIN 																			! Proceso cancelado   
       	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								!                     			
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!                     			
            TS.IO.MOTORKEY = 73																							!                     			
            Exit Sub 																												!                     			
         EndIf 																															!                     			
         Gr.Fosfec.TipDoc$ = "CO1E"																					!
         Gr.Fosfec.IdBene$ = TS.TEMP5$																			! Numero id beneficiario
         Gr.Fosfec.Benef$  = ""																							!
      EndIf

      If TS.TEMP5$ = "3" Then BEGIN 																				! Captura por permiso permanencia
         TS.TEMP2I4  = 0																										! Control proceso
         Call SALVAR.SECUENCIA
         Call Entrada.Autorizacion																					! Entrada autorizacion al proceso	
         If TS.TEMP2I4  = 0 Then Begin																			!
       	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								!
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!
            TS.IO.MOTORKEY = 73																						  !
            Exit Sub 																											  !
         EndIf																															!
         Call RESTAURAR.SECUENCIA

         TS.TEMP5$ = ASIC.DATOS$("INGRESE PERMISO DE","PERMANENCIA BENEF.")	!
         If TS.TEMP5$ = "E" Then BEGIN 																			! Proceso cancelado   
       	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								!                     			
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!                     			
            TS.IO.MOTORKEY = 73																							!                     			
            Exit Sub 																												!                     			
         EndIf 																															!                     			
         If (TS.IO.KEYS(1) = 70) Then Begin 															  ! Anulando movimiento
         	  Gr.Fosfec.Anul% = -1																					  ! 
         EndIf																													  	!
         Gr.Fosfec.TipDoc$ = "CO1V"																					!
         Gr.Fosfec.IdBene$ = TS.TEMP5$																			! Numero id beneficiario
         Gr.Fosfec.Benef$  = ""																							!
      EndIf

      If TS.TEMP5$ = "4" Then BEGIN 																				! Captura por cedula digital
         TS.TEMP2I4  = 0																										! Control proceso
         Call SALVAR.SECUENCIA
         Call Entrada.Autorizacion																					! Entrada autorizacion al proceso	
         If TS.TEMP2I4  = 0 Then Begin																			!
       	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								!
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!
            TS.IO.MOTORKEY = 73																						  !
            Exit Sub 																											  !
         EndIf																															!
         Call RESTAURAR.SECUENCIA

         TS.TEMP5$ = ASIC.DATOS$("INGRESE NRO CEDULA","DIGITAL           ")	!
         If TS.TEMP5$ = "E" Then BEGIN 																			! Proceso cancelado   
       	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								!                     			
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!                     			
            TS.IO.MOTORKEY = 73																							!                     			
            Exit Sub 																												!                     			
         EndIf 																															!                     			
         If (TS.IO.KEYS(1) = 70) Then Begin 															  ! Anulando movimiento
         	  Gr.Fosfec.Anul% = -1																					  ! 
         EndIf																													  	!
         Gr.Fosfec.TipDoc$ = "CO1C"																					!
         Gr.Fosfec.IdBene$ = TS.TEMP5$																			! Numero id beneficiario
         Gr.Fosfec.Benef$  = ""																							!
      EndIf

      GoTo Pago.Fosfec

Captura.Fosfec:

	 	  If TS.INTRX Then Begin  																							! Esta en plena trx de venta
	 	  	 Call VISOR.AND.BORRAR("NO SE PERMITE PAGO SUBSIDIO EN UNA TRX")    ! Msg Alerta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!
         TS.IO.MOTORKEY = 73																								!
         Exit Sub 																											    ! Termina proceso
	 	  EndIf

      If (TE.TR.AMT(1) + 50000) >= To.CDLIMIT2 Then Begin                   ! PROCESO CERCA A RECOGIDA
   	      Call VISOR.AND.BORRAR("REALICE PROCESO DE RECOGIDA CAJERO AHORA") ! Msg de alerta
   	      Call VISOR.AND.BORRAR("PAGO SUBSIDIO   CANCELADO")								! Msg de alerta
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													!
          TS.IO.MOTORKEY = 73																								!
          Exit Sub 																											    ! Termina proceso
      EndIf 	

      If (TS.IO.KEYS(1) = 70) Then Begin                                    ! Si secuencia anulacion
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)													!
          TS.IO.MOTORKEY = 0 																								!
          TS.GUIDANCE    = 1003                                             ! mensaje para revisar secuencia
          Exit Sub 																											    ! Termina proceso
      EndIf
      Gr.Fosfec.Anul% = 0
      !TS.TEMP5$ = ASIC.DATOS$("TIPO DOC. 1.CEDULA","2.EXTRAN 3.PERMANEN")   ! Mod 07Mzo2018 GR
      TS.TEMP5$ = ASIC.DATOS$("DOC 1.CEDULA 2.EXTR.","3.PERMANEN 4.DIGITAL")! Mod 27jUL2021 GR      
      
      If Val(TS.TEMP5$) <= 0 Or Val(TS.TEMP5$) > 4 Then BEGIN 							! Proceso cancelado   
       	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")										!                     			
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														!                     			
         TS.IO.MOTORKEY = 73																								!                     			
         Exit Sub 																													!                     			
      EndIf 																																!                     			
      
      If TS.TEMP5$ = "1" Then BEGIN 																				! Captura por cedula
         TS.ER.RETURN = -1
         Call CREAR.PIPE.PDF
         If TS.ER.RETURN = -1 Then Begin 
           Call VISORES4690(1,"ESCANEE CEDULA","DEL BENEFICIARIO..",0,"L")  ! 
           Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  !
           TS.IO.MOTORKEY = 0 																						  !
           Gr.Fosfec.TrxCed% = -1
           Exit Sub 																											  !
         EndIf Else Begin
         	Call VISOR.and.borrar("FALLA DEL PIPE")  ! 
           Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  !
           TS.IO.MOTORKEY = 73 																						  !
           Exit Sub 																											  !
         EndIf

      EndIf

      If TS.TEMP5$ = "2" Then BEGIN 																				! Captura por cedula de extranjeria 

         TS.TEMP2I4  = 0																										! Control proceso
         Call SALVAR.SECUENCIA
         Call Entrada.Autorizacion																					! Entrada autorizacion al proceso	
         If TS.TEMP2I4  = 0 Then Begin																			!
       	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								!
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!
            TS.IO.MOTORKEY = 73																						  !
            Exit Sub 																											  !
         EndIf																															!
         Call RESTAURAR.SECUENCIA

         TS.TEMP5$ = ASIC.DATOS$("INGRESE CED. EXTR.","DEL BENEFICIARIO..")	!
         If TS.TEMP5$ = "E" Then BEGIN 																			! Proceso cancelado   
       	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								!                     			
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!                     			
            TS.IO.MOTORKEY = 73																							!                     			
            Exit Sub 																												!                     			
         EndIf 																															!                     			
         Gr.Fosfec.TipDoc$ = "CO1E"																					!
         Gr.Fosfec.IdBene$ = TS.TEMP5$																			! Numero id beneficiario
         Gr.Fosfec.Benef$  = ""																							!
      EndIf

      If TS.TEMP5$ = "3" Then BEGIN 																				! Captura por permiso permanencia
         TS.TEMP2I4  = 0																										! Control proceso
         Call SALVAR.SECUENCIA
         Call Entrada.Autorizacion																					! Entrada autorizacion al proceso	
         If TS.TEMP2I4  = 0 Then Begin																			!
       	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								!
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!
            TS.IO.MOTORKEY = 73																						  !
            Exit Sub 																											  !
         EndIf																															!
         Call RESTAURAR.SECUENCIA

         TS.TEMP5$ = ASIC.DATOS$("INGRESE PERMISO DE","PERMANENCIA BENEF.")	!
         If TS.TEMP5$ = "E" Then BEGIN 																			! Proceso cancelado   
       	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								!                     			
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!                     			
            TS.IO.MOTORKEY = 73																							!                     			
            Exit Sub 																												!                     			
         EndIf 																															!                     			
         Gr.Fosfec.TipDoc$ = "CO1V"																					!
         Gr.Fosfec.IdBene$ = TS.TEMP5$																			! Numero id beneficiario
         Gr.Fosfec.Benef$  = ""																							!
      EndIf

      If TS.TEMP5$ = "4" Then BEGIN 																				! Captura por cedula digital
         TS.TEMP2I4  = 0																										! Control proceso
         Call SALVAR.SECUENCIA
         Call Entrada.Autorizacion																					! Entrada autorizacion al proceso	
         If TS.TEMP2I4  = 0 Then Begin																			!
       	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								!
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!
            TS.IO.MOTORKEY = 73																						  !
            Exit Sub 																											  !
         EndIf																															!
         Call RESTAURAR.SECUENCIA

         TS.TEMP5$ = ASIC.DATOS$("INGRESE NRO CEDULA","DIGITAL BENEFIC.  ")	!
         If TS.TEMP5$ = "E" Then BEGIN 																			! Proceso cancelado   
       	    Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								!                     			
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												!                     			
            TS.IO.MOTORKEY = 73																							!                     			
            Exit Sub 																												!                     			
         EndIf 																															!                     			
         Gr.Fosfec.TipDoc$ = "CO1C"																					!
         Gr.Fosfec.IdBene$ = TS.TEMP5$																			! Numero id beneficiario
         Gr.Fosfec.Benef$  = ""																							!
      EndIf


Pago.Fosfec:      
      Gr.Fosfec.Trx% = -1
      If Gr.Fosfec.TipEnt% = 1 Then Begin																		! Pago monetario
         Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 									 				! Init vectores de carga        
         TS.IO.DATA$(5) = "1"																								! Secuencia cambio a efectivo          
         TS.IO.KEYS(5)  = 61																								!                                        
         TS.IO.MOTORKEY = 61																								!                                        
         Call TSTCEC01                                                      !                        	     
         Call Reclamo.Fosfec                                         				! Reclamo subsidio
         Exit Sub                                                           !                         	     
      EndIf Else Begin																											! Pago Alimenticio
       If Gr.Fosfec.Consulta% = -1 Then Begin	 
         Gr.Fosfec.Appl$ = "11"                                             ! Codigo aplicacion
         Gr.Fosfec.Func$ = "81"                                             ! Codigo funcion
         TS.TEMP5I2 = 0                    													        ! Si proceso correcto
         TS.TEMP5I2 = COBRO.SUBSIDIO.FOSFEC															    ! Cobro subsidio
   	     If TS.TEMP5I2 <> 0 Then Begin																	    ! Si proceso correcto
            Call Format.Amount(Val(Gr.Fosfec.Vlr$))
   	     	  Call VISOR.AND.BORRAR("CUPO DISPONIBLE     $"+TS.TEMP1$)
         EndIf																															!
         Gr.Fosfec.Vlr$	= ""																								! Valor a entregar
         Gr.Fosfec.Benef$ = ""																							! ID del Beneficiario
         Gr.Fosfec.TRama$ = ""                                              ! Trama del mensaje
         Gr.Fosfec.Trx% = 0
         Gr.Fosfec.TrxCed% = 0
         Gr.Fosfec.Captura$ = ""
         Gr.Fosfec.Consulta% = 0
         Dim Ts.Io.Keys(10)																									! Inicializa Vectores
         Dim Ts.Io.Data$(10)																								! 
         TS.IO.MOTORKEY = 73
         Exit Sub          
       EndIf
      	 
      	 
         TS.TEMP5I1 = 0
         Call Reclamo.Fosfec                                         				! Reclamo subsidio
         If TS.TEMP5I1 = -1 Then BEGIN 
         	 TS.BD.DSPPARM = -1                     													! SET FOR NORMAL BD
           Call TSBDEC01                    																! FIRST TAKE BAL DUE
           TS.IO.STATE = 10
           Dim TS.IO.PREV.KEYS(10)
           Dim TS.IO.PREV.DATA$(10)
           TS.IO.PREV.MOTORKEY = 0
           Dim Ts.Io.Keys(10)																								! Inicializa Vectores
           Dim Ts.Io.Data$(10)																							! 
           TS.IO.DATA$(3) = Right$(Gr.Fosfec.TipVar$,1)                     ! Variedad de Pago
           TS.IO.KEYS(3)  = 78                                              ! Tecla Slash
           TS.IO.DATA$(7) = Gr.Fosfec.Vlr$																	! Valor a entregar
           TS.IO.KEYS(7)  = Val("9"+ Left$(Gr.Fosfec.TipVar$,1))            ! Tipo de Pago
           TS.IO.DATA$(9) = Gr.Fosfec.IdBene$															  ! ID del Beneficiario
           TS.IO.MOTORKEY = TS.IO.KEYS(7)																	  !
           Gr.Fosfec.TrxCed% = 0																						!
           To.TENDLIMITS(Gr.Fosfec.TVPOS%,0) = 9999999   										! Modifica controles entrega efectivo
           To.TENDLIMITS(Gr.Fosfec.TVPOS%,1) = 9999999									    ! Modifica controles entrega efectivo
           TS.TEMP1I1 = 0
           For TS.TEMP1I1 = 0 To 7
               TO.NEGAMT(TS.TEMP1I1) = 9999999
               TO.NEGCNT(TS.TEMP1I1) = 999
           Next TS.TEMP1I1           
           Call TSTDEC01																										! Ejecuta secuencia pago
           Dim Ts.Io.Keys(10)																								! Inicializa Vectores
           Dim Ts.Io.Data$(10)																							! 
           TS.IO.MOTORKEY = 73

         EndIf 
         Exit Sub                                                           !                         	          	
      EndIf
   EndIf																																		! Fin secuencia remesas
   
EndIf																																				! Fin Eamtsu14

!--- EAMTSU32.J86
If XOPT% = 32 Then Begin																										! En validacion de pago
 If (TS.IO.DATA$(3) = Right$(Gr.FosFec.TipVar$,1)) And                     \! Si usa forma de pago 
   (TS.IO.KEYS(7)  = Val("9"+Left$(Gr.FosFec.TipVar$,1))) Then Begin        !	fosfec
   	If Gr.Fosfec.Trx% = 0 Then Begin 																		    ! Si no esta en una operacion con Fosfec
       	  Call VISOR.AND.BORRAR("FORMA DE PAGO NO AUTORIZADA      /Borrar")	!
           TS.USER.RETURN = 99
           TS.LINEDATA = 51                 																! "TARJETA INVALIDA"            
           TS.STACKERR(0) = 0                                               ! manager's override required   
           TS.STACKERR(3) = 0                                               ! item descriptor               
           TS.STACKERR(6) = 0                                               ! put "M" on last display       
           TS.STACKERR(7) = -1                                              ! indicate no printing required 
           TS.MO.REASON = 0                                                 ! invalid key with department   
           Call TSCSEC03       																							!
           TS.IO.MOTORKEY = 0 							
   	EndIf																																		! 
 EndIf																																			! Fin validacion forma de pago pes  	
EndIf																																				! Fin validacion forma de pago

!--- EAMTSU08.J86
If XOPT% = 08 Then Begin																										! En registro de items
	 If Gr.Fosfec.InAnul% = -1 Then Begin																			! En simulacion
	 	  If Gr.Fosfec.Cont% <= Gr.RsmItm.Ycont% Then Begin											! 
	 	  	 CHAIN.COUNT = 1                                             				! Contador de item linkeados
	 	  	 If Gr.Fosfec.Cont% < Gr.RsmItm.Ycont% Then Begin										! Recorre lista de itemns no validos
            IR.LINKEDTO$ = Pack$(Gr.Fosfec.ExcItm$(Gr.Fosfec.Cont%+1,0))    ! Siguiente Item
	 	  	 EndIf Else Begin																										!
	 	  	 	  IR.LINKEDTO$ = Pack$("0000")																	  ! Termina ciclo
	 	  	 	  Gr.Fosfec.InAnul% = -2																					!
	 	  	 EndIf																															!
	 	  	 TS.IO.KEYS(1) = 70																									! Anulacion Item
	 	  	 TS.TEMP1$ = Gr.Fosfec.ExcItm$(Gr.Fosfec.Cont%,1)										! Toma Cantidad reversar
	 	  	 If Val(Gr.Fosfec.ExcItm$(Gr.Fosfec.Cont%,1)) < 0 Then Begin        ! Qty reversar
	 	  	 	  TS.TEMP1$ = Str$(Val(Gr.Fosfec.ExcItm$(Gr.Fosfec.Cont%,1)) * -1)!
	 	  	 	  TS.IO.KEYS(1) = 0																							  ! Reporta venta Item
	 	  	 EndIf																															!
         If IR.INDICAT0 AND 40H Then Begin        													! Si el Item es pesable 
            TS.IO.KEYS(6)  = 72																							! Simula secuencia registro
            TS.IO.DATA$(6) = TS.TEMP1$           														!
         EndIf Else Begin																										! Item por cantidad
            TS.IO.KEYS(6)  = 75																							! Simula cantidad
            TS.IO.DATA$(6) = TS.TEMP1$           														!
         EndIf																															!
         Gr.Fosfec.Cont% = Gr.Fosfec.Cont% + 1                              !
	 	  EndIf 
	 EndIf																																		! Fin simulacion

EndIf

!--- EAMTSU60.J86
If XOPT% = 60 Then Begin																										! En registro de items
	
 If TS.LINETYPE = 1 Then                                                   \! Es un articulo
  If Gr.Fosfec.InAnul% = -2 Then Begin             													! Ultimo Item devuelto
     Gr.Fosfec.InAnul% = 0 																								  ! Reset Variable
     Gr.Fosfec.Cont% = 0
     Gr.RsmItm.Ycont% = 0
     Call U.Imprime(String$(38,"-"),6100H)                                 !
  EndIf																																		  !

EndIf
End Sub 
