 !**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMNTACR.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   *
!Observaciones : GENERACION NOTAS CREDITO SISTEMA *
!                FECO COLOMBIA                    *
!**************************************************
! Version 1.0 22Jul2024
! Mod 06/Feb/2025
! Se ajuste bug en recuperación de NC si Item trae
! descuento sea sumado y aplicado en la trx 
! Desarrollado por GR - OVS
!--------------------------------------------------
! Mod 21/Feb/2025
! Se ajusta calculo precio unitario items si tiene 
! descuentos para retornar un precio neto y evitar
! descuadres en plantilla financiera.
! Desarrollado por GR - OVS
!--------------------------------------------------

%Environ T                 																									! Ambiente de terminal

%INCLUDE GRFTSUVA.011	                                                      ! Variables desarrollo                

String    Global ItmsNtCrd$(2), 																					 \! Lista Items Nta Crd
                 Gr.NtaCrd.Msg$(1),																				 \! Msg tkt nota credito
                 Gr.NtaCrd.Tipo$,                                          \! Tipo Nota 1.Total 2.Parcial
                 Gr.NtaCrd.Tori$,																					 \! Nro trx origen
                 Gr.NtaCrd.Nlin$, 																				 \!
                 Gr.NtaCrd.NItm$, 																				 \!
                 Gr.Ntacrd.Bono$,																					 \! nro bono dev generado
                 Gr.NtaCrd.Monto$,																				 \! Monto bono a genera
                 Gr.NtaCrd.Preori$,                                        \! Prefijo origen
                 Gr.NtaCrd.Facori$,                                        \! factura origen
                 Gr.NtaCrd.Cliori$,                                        \! cliente origen
                 Gr.NtaCrd.Cufe$,                                          \! Nro cufe origen
                 Gr.NtaCrd.TipClte$,                                       \! Tipo cliente fiscal
                 Gr.NtaCrd.Motor$,																				 \! Tecla motora Nta Crd
                 Gr.NtaMnf.Motor$																						! Tecla motora reintegro mf
Integer*4 Global Gr.NtaCrd.Itm%, 																					 \! Total Items recuperados
                 Gr.NtaCrd.Discount%,                                      \! Total descuento anular
                 Gr.NtaCrd.Cont%																						! Total items registrados
Integer*1 Global Gr.NtaCrd.Proc%, 																				 \! Recuperacion de items
                 Gr.NtaCrd.InTrx%,                                         \! Nta Crd en proceso
                 Gr.NtaCrd.NmLin%,                                         \! Nro lineas mensaje tkt bono
                 Asic.Detalle%,                                            \!
                 Gr.NtaCrd.Ok%																							! Proyecto activo
Integer*1 Global Asc.Pay.Impr%																							!
Integer*1 Global Gr.RetMf.Proc%																							! Reintegro minifacturas
Integer*4 Global Asc.Tmp.Apun2%       																			!
String    Global Gr.BonDev.Pref$,  																				 \! Prefijo bono
                 Gr.Fis.Numer$(2), 																				 \!
                 Gr.Fis.Direc$,                                            \!
                 Gr.Fis.City$,                                             \!
                 Gr.BonDev.TipVar$,																				 \!
                 Gr.Motor2.GrpDscto$,                                      \!
                 IR.USERDATA$

%INCLUDE EAMTSWKG.J86                                                       ! working storage                   
%INCLUDE EAMTRANS.j86                                                       ! Variables procesos de transaccion 
%INCLUDE EAMTERMS.J86                                                       ! Variables procesos terminal       
%INCLUDE EAMTOPTS.J86                                                       ! Variables Terminal Options        
%INCLUDE EAMTSEVA.J86                                                       ! Variables Cliente Frecuente       
%INCLUDE EAMITEMR.J86                                                       ! Variables ITEMR
%INCLUDE LCLTSUVA.011	         																							! Variables Lealtad

%INCLUDE EAMTSXHC.J86																												!
%INCLUDE EAMASMRT.J86																												!

%INCLUDE RECATSSU.011          																			        ! Rutinas Genericas Grupo Retail

Sub TSPREC01 EXTERNAL          																							! print routine
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

Function FORMAT.AMOUNT(AMT1) External   																		! Formateo de valores
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub NTCR.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)        							! Auditoria FECO
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$,      \!
       X.RTA$, X.BUFF$																											!
Integer*4 X.Len%																														!
			TS.ER.RETURN = -1																											!
			X.FILE$ = "R::ADX_UDT1:NC" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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

Sub Graba.Itm.FeNatural External																						! Almacena UD Items
End Sub 																																		!

Sub Impresion.Detalle.Iva External
End Sub 

Sub FECO.OBSERVACIONES External
End Sub 

Sub Actualiza.Num.Fiscal External																						! Grabacion numeracion fiscal
End Sub 

Sub Flag.Item.Iva(Xindi1%) External 																				! Calculo flag de iva
Integer*1 Xindi1%																														!
End Sub 

Function Format.Fecha$ External
String Format.Fecha$
End Function 

Sub CARGAR.ITMS.NTACRD(Xlista$)																							! Recuperacion Items nota cred
String Xean$, XQty$, Xpup$, Xlista$, Xitm$, Xdct$, Xqty2$, Xcomer$					!
Integer*4 Xi%, Xprecio%, Xpun%																							!
  Asc.Tmp.Apun% = 135																							          !
  !Dim ItmsNtCrd$(800,6)																										!
  Dim ItmsNtCrd$(800,10)                                                    ! Add 12Mzo2025
  Gr.NtaCrd.Itm% = 0																												!
  TS.TEMP1I1  = -1 																													! Control del proceso 
  While Asc.Tmp.Apun% < Len(Xlista$)																				!
   XItm$ = Asic.Getunpk5(Xlista$,Asc.Tmp.Apun%)															! Lista items
   If XItm$ <> "" Then Begin																								! Lista recuperada
   	  Asc.Tmp.Apun2% = 1																										! Apuntador recuperacion
      Xean$  = Asic.Getunpk6(Xitm$,Asc.Tmp.Apun2%)													! Plu registro
      XQty2$ = Asic.Getunpk6(Xitm$,Asc.Tmp.Apun2%)												  ! Qty Vendida
      XQty$  = Asic.Getunpk6(Xitm$,Asc.Tmp.Apun2%)												  ! Qty Saldo
      Xpup$  = Asic.Getunpk6(Xitm$,Asc.Tmp.Apun2%)													! Precio venta unitario
      Xdct$  = Asic.Getunpk6(Xitm$,Asc.Tmp.Apun2%)													! Dscto aplicado
      TS.TEMP2$ = Pack$(Right$("000000000000"+Xean$,12))                    ! Item a buscar
      TS.ER.RETURN = -1																											!
      Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)              				  ! Lectura Datos Itemr
      Call IRRFEC.SPLIT1( TS.TEMP1$ ) 			                         				! Entrega datos al eamitemr.j86
      If TS.ER.RETURN <> -1 Then Begin																			! Not Found
   	    Call VISOR.AND.BORRAR("PLU NO EXITE MAESTRA"+Xean$)                 !
   	    Call VISOR.AND.BORRAR("NOTA CREDITO NO     PERMITIDA /Borra")       !
  	    TS.TEMP1I1  =  0 																									  ! Control del proceso 
        Exit Sub 																														! Sale del proceso
      EndIf																																	!
      Call Flag.Item.Iva(IR.INDICAT1)																			  ! Retorna flag iva
      If Val(XQty$) > 0 Then Begin																					! Tiene existencia
        Gr.NtaCrd.Itm% = Gr.NtaCrd.Itm% + 1																	! Total Items 
        ItmsNtCrd$(Gr.NtaCrd.Itm%,0) = Xean$																! Plu registrado
        ItmsNtCrd$(Gr.NtaCrd.Itm%,1) = XQty$																! Cantidad peso
        ItmsNtCrd$(Gr.NtaCrd.Itm%,2) = Xpup$																! Precio unitario neto
        ItmsNtCrd$(Gr.NtaCrd.Itm%,3) = "0"															    ! Control qty vendidas
        ItmsNtCrd$(Gr.NtaCrd.Itm%,4) = Xdct$															  ! Total dscto aplicado
        ItmsNtCrd$(Gr.NtaCrd.Itm%,5) = XQty2$															  ! Cantidad ori vendida
        ItmsNtCrd$(Gr.NtaCrd.Itm%,6) = "0"															    ! Dscto apicado en la trx de nc
!--- Datos adicionales items para grabacion dsctos trx  Mod 12Mzo20251
        ItmsNtCrd$(Gr.NtaCrd.Itm%,7) = UnPack$(IR.DEPARTME$)                ! Dpto venta
        Xcomer$ = LEFT$(IR.USERDATA$,16)																		! Estructura comercial
        ItmsNtCrd$(Gr.NtaCrd.Itm%,8) = Right$(Xcomer$,15)                   ! Estructura comercial
        ItmsNtCrd$(Gr.NtaCrd.Itm%,9) = Str$(TS.TEMP1I1)                     ! Flag de IVA
        ItmsNtCrd$(Gr.NtaCrd.Itm%,10) = "0"                                 ! Total venta Item
        TS.TEMP1I1  = -1 																										! Control del proceso 
      EndIf																																	!
   EndIf Else Begin																													! Termina recuperacion
   	If Gr.NtaCrd.Itm% > 0 Then Begin																				! Hay itms recuperados
   		Gr.NtaCrd.Itm% = Gr.NtaCrd.Itm% + 1																		! Total Items 
      ItmsNtCrd$(Gr.NtaCrd.Itm%,0) = "999999999999"													! Registro cierre recuperacion
      ItmsNtCrd$(Gr.NtaCrd.Itm%,1) = "1"  																	!
      ItmsNtCrd$(Gr.NtaCrd.Itm%,2) = "0"  																	!
      ItmsNtCrd$(Gr.NtaCrd.Itm%,3) = "0"																	  !
      ItmsNtCrd$(Gr.NtaCrd.Itm%,4) = "0"																	  !
      ItmsNtCrd$(Gr.NtaCrd.Itm%,5) = "0"																	  !
      ItmsNtCrd$(Gr.NtaCrd.Itm%,6) = "0"																	  !
      ItmsNtCrd$(Gr.NtaCrd.Itm%,7) = "0"																	  !
      ItmsNtCrd$(Gr.NtaCrd.Itm%,8) = "0"																	  !
      ItmsNtCrd$(Gr.NtaCrd.Itm%,9) = "0"																	  !
      ItmsNtCrd$(Gr.NtaCrd.Itm%,10) = "0"																	  !
   	  Asc.Tmp.Apun% = Len(Xlista$)																					!
   	Endif Else Begin																												!
   		Asc.Tmp.Apun% = Len(Xlista$)																					! Cierra ciclo
   	EndIf																																		!
   EndIf																																		!
  Wend 																																			!
  If Gr.NtaCrd.Itm% = 0 Then Begin																					! No hay items de carga
  	 TS.TEMP1I1  =  0 																											! Control del proceso 
     Exit Sub 																															!
  EndIf																																			!

!-- Lista Itms
 XI% = 0
! For XI% = 1 To (Gr.NtaCrd.Itm%)
!    Call U.IMPRIME("PLU :"+ItmsNtCrd$(XI%,0),2100H)
!    Call U.IMPRIME("QTY :"+ItmsNtCrd$(XI%,1),2100H)
!    Call U.IMPRIME("PVP :"+ItmsNtCrd$(XI%,2),2100H)
!    Call U.IMPRIME("QVT :"+ItmsNtCrd$(XI%,3),2100H)
!    Call U.IMPRIME("DSC :"+ItmsNtCrd$(XI%,4),2100H)
!    Call U.IMPRIME("QIN :"+ItmsNtCrd$(XI%,5),2100H)
!    Call U.IMPRIME("DAP :"+ItmsNtCrd$(XI%,6),2100H)
! Next XI%

End Sub 																																		! Fin recuperacion items

Sub ListaItmsNtaCrd        																								  ! Carga lista items Nta Crd
String Xsnd$, Xtemp4$, Xtrama$, Xfin$
String Xtqt$, Xnrot$, Xnromt$, XSndMov$, Xmsg$
 TS.TEMP1I1  = 0 																														! Control del proceso
 Xnrot$ = "0"
 XSndMov$ = ""
 Xmsg$ = "VALIDANDO TIQUETE"
 Call VISORES4690(1,Xmsg$,Gr.NtaCrd.Tori$,0,"L")
 Tramas.NtaCrd:
 Xnrot$ = Right$("00"+Str$(Val(Xnrot$) + 1),2)
 Xnromt$ = "1" 
 Xtqt$  = Right$("0000"+Mid$(Gr.NtaCrd.Tori$, 1, 3),4)   +                 \! Centro costo
          Right$("000000"+Mid$(Gr.NtaCrd.Tori$, 4, 3),6) +                 \! Numero de terminal
          Right$("000000"+Mid$(Gr.NtaCrd.Tori$, 7, 4),6) +                 \! Numero de transaccion
          Right$("0000000000"+Mid$(Gr.NtaCrd.Tori$,11,10),10) + "00"  +    \! Fecha y hora trx
          Xnrot$                                                            ! Trama inicial 
 Call VISORES4690(1,"VALIDANDO "+Xnrot$,Gr.NtaCrd.Tori$,0,"L")
 Asc.Pay.Impr% = 1																													! Toma numero trx correcta de cierre
 Xsnd$ = DATE$ +":"+ Time$                                                  ! Fecha y hora del requerimiento
 XTemp4$ = Armar.Trama.Msg("22","00",Xtqt$,"00","0001","123456")        		! Armar trama MSG NtaCrd
 !Xtrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)           ! Ejecuta Requerimiento
 
 Xtrama$ = "2200399200000TRANSACCION RECUPERADA SATISFACTORIAMENT1 1           3115932001SETT  02300207    ad6031b7c28f093dae00ad3dfc749f677a0a9b58565b2b9de392e824c04114a97835f"
 Xtrama$ = Xtrama$ + "de897bf330acd93d8bf871d22a0734:1:1:57700:11540:,788:1:1:122500:24500:,20:10:10:3400:6800:                                                                 "


 Xfin$ = DATE$ +":"+ Time$                                                  ! Fecha y hora rta del requerimiento
 Call NTCR.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                      		! Rastreo movimiento
 XTEMP4$ = Valida.Rta(XTrama$)																			        ! Valida rta entregada
 If Xtemp4$ <> "00" Then Begin 																							! Falla de Comunicaciones
    Exit Sub      																													! Termina proceso
 EndIf																																			!
 XTEMP4$ = Mid$(XTrama$,12,2)	           																    ! Valida rta entregada
 If XTemp4$ <> "00" Then Begin 																					    ! Falla recup trx
 	  Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))									            ! Presenta Msg Error
    Exit Sub      																													! Termina proceso
 EndIf
 Xnromt$ = Mid$(Xtrama$,56,2)																								! Total de tramas
 Xmsg$ = "RECUPERA  "+Xnrot$+"/"+Xnromt$
 Call VISORES4690(1,Xmsg$,Gr.NtaCrd.Tori$,0,"L")
 If Val(Xnrot$) < Val(Xnromt$) Then Begin																		! Proceso tramas
	  XSndMov$ = XSndMov$ + Mid$(Xtrama$,58,(Len(Xtrama$) - 58) + 1)          ! Toma la trama
 	  GoTo Tramas.NtaCrd																											! Valida otra trama
 EndIf
 If Val(Xnrot$) = Val(Xnromt$) Then Begin																		! Proceso tramas
 	  XSndMov$ = XSndMov$ + Mid$(Xtrama$,58,(Len(Xtrama$) - 58) + 1)			 	  ! Toma la trama
 EndIf
 TS.TEMP1I1  = 00 																													! Control del proceso 
 XSndMov$ = XSndMov$ + ",      "
 Call CARGAR.ITMS.NTACRD(XSndMov$)																					!
 If TS.TEMP1I1 = -1 Then Begin																							!
 	 Gr.NtaCrd.Cliori$  = AllTrim$(Mid$(XSndMov$, 1,18))                      ! cliente origen
 	 Gr.NtaCrd.TipClte$ = AllTrim$(Mid$(XSndMov$,19, 2))                      ! tipo fiscal cliente
   Gr.NtaCrd.Preori$  = AllTrim$(Mid$(XSndMov$,21, 4))                      ! Prefijo origen
   Gr.NtaCrd.Facori$  = AllTrim$(Mid$(XSndMov$,25,10))                      ! Factura origen
 	 Gr.NtaCrd.Cufe$    = AllTrim$(Mid$(XSndMov$,35,100))											! Cufe original
   Call Visor.And.Borrar("# ITEMS RECUPERADOS "+STR$(Gr.NtaCrd.Itm%)+" /Borra")	! Total Items cargados
 EndIf Else Begin																														! falla recuperacion
 	 Call Visor.And.Borrar("FACTURA YA ANULADA  TOTALMENTE /Borrar")          ! Msg alerta 
 EndIf																																			! 
 
End Sub 																																		! Fin carga lista simulacion

Sub ValidaItmsNraCrd																												! Validacion items nota parcial
Integer*2 XI%
Integer*4 Xqty%
 TS.TEMP1I2 = 0																															! Control del proceso
 XI% = 0
 For XI% = 1 to Gr.NtaCrd.Itm%																							! Recorre venta
  If Val(ItmsNtCrd$(XI%,0)) =  Val(SL.IT.ITEMCODE$) Then Begin							! Si Item existe
     TS.TEMP1I2 = XI%               																				! Posicion de la venta
  	 Xqty% = Val(ItmsNtCrd$(XI%,1)) - Val(ItmsNtCrd$(XI%,3))                ! Cantidad Item
  	 If Val(TS.IO.DATA$(6)) > Xqty% Then Begin															! Cantidad mayor a la comprada
 	 	    Call Visor.And.Borrar("SUPERA LA CANT/PESO VENTA ORIGEN /Borra")    ! Reporta Error al cajero
        Call Visor.And.Borrar("CANT. MAXIMA ANULAR "+Str$(XQTY%)+" /Borra") ! Msg Alerta
 	 	    TS.TEMP1I2 = 0               																				! Posicion de la venta
  	 	  Exit Sub 																														!
  	 EndIf																																	!
  EndIf																																			!
 Next XI%																																		!
End Sub 																																		! Fin items notas parciales


Sub ENVIO.TRX.SALDOS																												! Actualizacion saldos Nta Crd
String Xtmp$, Xbuff$, XTemp4$, Xsnd$, Xfin$, Xtrama$												! 
 Xbuff$ = Right$("000"+TS.STORE$,3)                      +                 \! Centro costo
          Right$("000000"+Str$(SL.HD.TERMINAL),6)        +                 \! Numero de terminal
          Right$("000000"+Str$(Sl.Hd.Transnum + 1),6)    +                 \! Numero de transaccion
          UNPACK$(SL.HD.DATETIME$) + "00   "                                ! Fecha y hora trx
 Xbuff$ = Xbuff$ + Right$("0000"+Mid$(Gr.NtaCrd.Tori$, 1, 3),4)   +        \! Centro costo origen
          Right$("000000"+Mid$(Gr.NtaCrd.Tori$, 4, 3),6) +                 \! Numero de terminal origen
          Right$("000000"+Mid$(Gr.NtaCrd.Tori$, 7, 4),6) +                 \! Numero de transaccion origen
          Right$("0000000000"+Mid$(Gr.NtaCrd.Tori$,11,10),10) + "00"        ! Fecha y hora trx origen
 Xbuff$ = Xbuff$ + TS.TEMP6$																								! Adiciona Lista items
 Asc.Pay.Impr% = 1																													! Toma numero trx correcta de cierre
 Xsnd$ = DATE$ +":"+ Time$                                                  ! Fecha y hora del requerimiento
 XTemp4$ = Armar.Trama.Msg("22","02",Xbuff$,"00","0001","123456")        		! Armar trama almacena nta dev
 !Xtrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)           ! Ejecuta Requerimiento
 
 Xtrama$ = "2202046200000DETALLE BONO DEVO INSERTADO SATISFACTORI"
 
 Xfin$ = DATE$ +":"+ Time$                                                  ! Fecha y hora rta del requerimiento
 Call NTCR.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                      		! Rastreo movimiento
 XTEMP4$ = Valida.Rta(XTrama$)																			        ! Valida rta entregada

End Sub 

Sub ALMACENA.NOTA.CREDITO																										! Almacenamiento nota credito
String Xtmp$, Xbuff$
 Call Graba.Itm.FeNatural																										! Almacena UD nota crd
 Gr.Fis.NotaCrd% = Gr.Fis.NotaCrd% + 1																			! Aumenta consecutivo Nta Crd
 Xbuff$ = Right$("000"+Mid$(Gr.NtaCrd.Tori$, 1, 3),3)    +                 \! Centro costo                 Asc 
          Right$("000000"+Mid$(Gr.NtaCrd.Tori$, 4, 3),6) +                 \! Numero de terminal
          Right$("000000"+Mid$(Gr.NtaCrd.Tori$, 7, 4),6) +                 \! Numero de transaccion
          Right$("0000000000"+Mid$(Gr.NtaCrd.Tori$,11,10),10) + "00" +     \! Fecha y hora trx
          ":" + Gr.Fis.Numer$(3,1) +																			 \! Prefijo fiscal               Asc
          ":" + Left$(Str$(Gr.Fis.NotaCrd%)+"           ",11) +            \! Nro de nota                  Asc
          ":" + Gr.Fis.Numer$(3,2) +                                       \! Rango nota inicial           PD
          ":" + Gr.Fis.Numer$(3,3) +                                       \! Rango nota final             PD
          ":" + Gr.NtaCrd.Preori$  +                                       \! Prefijo factura origen       Asc
          ":" + Pack$(Gr.NtaCrd.Facori$)  +                                \! Numero factura origen        PD
          ":" + Pack$(Gr.Lcl.Clte$)                                         ! ID cliente origen            PD
 Call Grabacion.Cadena.Usuario2("67",Xbuff$)                       					! 
 Call Actualiza.Num.Fiscal																									! Actualiza numeracion fiscal
 Call ENVIO.TRX.SALDOS
End Sub 

Sub Detalle.Nota.Crd
String Xtmp$
Integer*4 Xi%
  Gr.Fis.Doc% = 0
  !Call Impresion.Detalle.Iva
  Gr.Fis.Doc% = -1

  TS.TEMP1$ = "NOTA DEVOLUCION:"+Gr.Fis.Numer$(3,1)+Str$(Gr.Fis.NotaCrd%)   ! Nro Nota Devol.
  TS.TEMP1$ = Left$(TS.TEMP1$ + STRING$(38," "),38)                					! Ajuste impresion
  Call U.IMPRIME(TS.TEMP1$,4101H)																						!
  Call U.IMPRIME(TS.TEMP1$,2100H)																						!
  TS.TEMP1$ = "CLIENTE:"+Gr.Lcl.Clte$																				!
  TS.TEMP1$ = Left$(TS.TEMP1$ + STRING$(38," "),38)                					! Ajuste impresion
  Call U.IMPRIME(TS.TEMP1$,6100H)																						!
  TS.TEMP1$ = "NOMBRE :"+CA.CUSTOMER$																				!
  TS.TEMP1$ = Left$(TS.TEMP1$ + STRING$(38," "),38)                					! Ajuste impresion
  Call U.IMPRIME(TS.TEMP1$,6100H)																						!
  TS.TEMP1$ = "DATOS TRANSACCION ORIGEN DEVOLUCION"											    !
  TS.TEMP1$ = Left$(TS.TEMP1$ + STRING$(38," "),38)                					! Ajuste impresion
  Call U.IMPRIME(TS.TEMP1$,4101H)																						!
  Call U.IMPRIME(TS.TEMP1$,2100H)																						!
  TS.TEMP2$ = "DOC ORIGEN:"+Gr.NtaCrd.Preori$+Gr.NtaCrd.Facori$							!
  TS.TEMP2$ = LEFT$(TS.TEMP2$ + STRING$(38," "),38)                         ! 
  Call U.IMPRIME(TS.TEMP2$,6100H)
  TS.TEMP2$ = Mid$(Gr.NtaCrd.Tori$,11,2) + "/" + 													 \! a#0
              Mid$(Gr.NtaCrd.Tori$,13,2) + "/" + 													 \! Mes
              Mid$(Gr.NtaCrd.Tori$,15,2) + " " + 										       \! Dia
              Mid$(Gr.NtaCrd.Tori$,17,2) + ":" + 													 \! Hora
              Mid$(Gr.NtaCrd.Tori$,19,2) + ":" + "00" 											! Min y Seg
  TS.TEMP2$ = "FECHA DOC: "+TS.TEMP2$
  TS.TEMP2$ = LEFT$(TS.TEMP2$ + STRING$(38," "),38)                         ! 
  Call U.IMPRIME(TS.TEMP2$,6100H)
  TS.TEMP2$ = "IUT ORI: "+Gr.NtaCrd.Tori$
  TS.TEMP2$ = LEFT$(TS.TEMP2$ + STRING$(38," "),38)                         ! 
  Call U.IMPRIME(TS.TEMP2$,6100H)
 
End Sub 

Sub GENERAR.BONO.DEVOL																											! Generacion bono devolucion
String Xtmp$, Xvlr$, Xnro$, Xsnd$, Xtemp4$, Xtrama$, Xfin$, Xbuff$					! 
Integer*2 Xnt%

 Call VISORES4690(1,"ALMACENANDO NOTA","DEVOLUCION ESPERE..",0,"L")					! Msg Alerta
 Gr.NtaCrd.Monto$ = Str$(TS.TOTALS(0,0,0) * -1)															! Monto de la trx
 Gr.Ntacrd.Bono$ = Right$("000"+TS.STORE$,3) + 														 \! Nro tienda             Asc  3
                   Right$("00"+Str$(Val(TS.TERMINAL$)),2)      +           \! Nro terminal           Asc  2
                   Right$("000"+Str$(Sl.Hd.Transnum + 1),3)    +           \! Nro Transaccion        Asc  3
                   UNPACK$(SL.HD.DATETIME$)																	! Fecha y hora           Asc 10
 Xbuff$ = Pack$(Gr.Ntacrd.Bono$) +																				 \! Nro bono generado
          ":" + Pack$(Gr.NtaCrd.Monto$) +																	 \! Valor del bono
          ":" + Pack$(Gr.NtaCrd.Tori$)  +                                  \! IUT Original 
          ":" + Pack$("00")																									! Filler
 Call Grabacion.Cadena.Usuario2("69",Xbuff$)                       					! 

 Xnt% = Val(Left$(DATE$,2)) + 1																							! Aumenta 1 año validez
 Xfin$ = "20"+Right$("00"+Str$(Xnt%),2) + Mid$(DATE$,3,2) + Right$(DATE$,2) ! Fecha vigencia
 Xtmp$ = Left$(Gr.Lcl.Clte$+"              ",15)        +                  \! ID cliente
         Right$("0000"+Mid$(Gr.NtaCrd.Tori$, 1, 3),4)   +                  \! Centro costo
         Right$("000000"+Mid$(Gr.NtaCrd.Tori$, 4, 3),6) +                  \! Numero de terminal
         Right$("000000"+Mid$(Gr.NtaCrd.Tori$, 7, 4),6) +                  \! Numero de transaccion
         Right$("0000000000"+Mid$(Gr.NtaCrd.Tori$,11,10),10) + "00" +      \! Fecha y hora trx
         Right$("00000000000"+Gr.NtaCrd.Monto$,11)      +                  \! Valor Bono
         Right$(String$(30," ")+Gr.Ntacrd.Bono$,30)     +     						 \! Nro Bono
         Left$((Alltrim$(Gr.Fis.Numer$(3,1))+Str$(Gr.Fis.NotaCrd%))+       \! Nro interno nta devol
                String$(13," "),14)                     +                  \!
         Right$("00000000"+Xfin$,8)																				  ! Fecha vencimiento bono

 Asc.Pay.Impr% = 1																													! Toma numero trx correcta de cierre
 Xsnd$ = DATE$ +":"+ Time$                                                  ! Fecha y hora del requerimiento
 XTemp4$ = Armar.Trama.Msg("22","01",Xtmp$,"00","0001","123456")        		! Armar trama almacena nta dev
 !Xtrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)           ! Ejecuta Requerimiento
 
 Xtrama$ = "2201046200000NOTA DEVOLUCION CREADO SATISFACTORIAMENT"
 
 Xfin$ = DATE$ +":"+ Time$                                                  ! Fecha y hora rta del requerimiento
 Call NTCR.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                      		! Rastreo movimiento
 XTEMP4$ = Valida.Rta(XTrama$)																			        ! Valida rta entregada

End Sub 

Sub IMPRESION.NOTA.DEVOL																										! Impresion nota devol
String Xnro$, BARCODE$, ENDBCODE$, XDATE1$, XDATE2$, CPI17$
Integer*2 X2%, Xctl%, Xnxt%, Xi%

 If Gr.NtaCrd.InTrx% = -2 Then Begin																				! Generacion bono devol
     
     TS.TEMP1$ = Left$(Gr.Fis.Empresa$+String$(38," "),38)
     Call U.Imprime(TS.TEMP1$,6100H)
     TS.TEMP1$ = Left$(Gr.Fis.Nit$+String$(38," "),38)
     Call U.Imprime(TS.TEMP1$,6100H)
     TS.TEMP1$ = Left$(Gr.Fis.Direc$+String$(38," "),38)
     Call U.Imprime(TS.TEMP1$,6100H)
     TS.TEMP1$ = Left$(Gr.Fis.City$+String$(38," "),38)
     Call U.Imprime(TS.TEMP1$,6100H)

     TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(11H)
     TS.TEMP1$ = TS.TEMP1$ + "      BONO DEVOLUCION COMPRA       "          !
     Call U.Imprime(TS.TEMP1$,4211H)		                                    ! Impresion Bono recompra
     TS.TEMP1$ = "FECHA GENERACION : 20"+Left$(DATE$,2) + "-" + Mid$(DATE$,3,2) + "-" + \!
                 Right$(DATE$,2)
     TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												! Ajusta Dato impresion
     Call U.IMPRIME(TS.TEMP1$,4110H)																			  ! Imprime dato
     Xnxt% = Val(Left$(DATE$,2)) + 1
     TS.TEMP1$ = "VALID HASTA      : 20"+Right$("00"+Str$(Xnxt%),2) + "-" + Mid$(DATE$,3,2) + "-" + \!
                 Right$(DATE$,2)
     TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												! Ajusta Dato impresion
     Call U.IMPRIME(TS.TEMP1$,4110H)																			  ! Imprime dato
     TS.TEMP2$ = "IUT ORI: "+Gr.NtaCrd.Tori$																! 
     TS.TEMP2$ = LEFT$(TS.TEMP2$ + STRING$(38," "),38)                      ! 
     Call U.IMPRIME(TS.TEMP2$,4110H)
     Xnro$ = "" : Xctl% = 0
     Xnro$ = Gr.BonDev.Pref$ + Gr.Ntacrd.Bono$															! Nro bono generado
!-- Impresion EAN 128
    BARCODE$  = Chr$(1DH) + Chr$(6BH) 					                            ! Manejo de impresion
    ENDBCODE$ = Chr$(00H) 						                                      ! codigos de barra
    TS.TEMP1$ = Chr$(1DH)+Chr$(77H)+Chr$(02H)+  		                       \! Horizontal barcode Size
                Chr$(1DH)+Chr$(68H)+Chr$(50)+                              \! Vertical barcode Size 
                Chr$(1DH)+Chr$(48H)+Chr$(02H)+                             \! Human Read characters
  	            Chr$(1DH)+Chr$(66H)+Chr$(01H)    	                          ! activado
     Call U.IMPRIME(TS.TEMP1$,4000H)				                                !
     TS.TEMP2$ = Right$("0000000000"+Str$(TS.TEMP5I4),10)                   ! Dato Impresion
     TS.TEMP1$ = (BARCODE$+Chr$(07H)+(XNRO$)+ENDBCODE$)           					! Arma String de impresion			
     Write #34 ; TS.TEMP1$          					                              ! Imprime cupon en PDF-417
     TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(0)				                        ! Ajusta impresion a la Izquierda
     Call U.IMPRIME(TS.TEMP1$,4000H)				                                !                    
!-- Fin impresion EAN 128
     TS.TEMP5I4 = Val(Gr.NtaCrd.Monto$)																			! Monto del bono
     CALL FORMAT.AMOUNT(TS.TEMP5I4)																					! formateo del valor
     TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(11H)+"VALOR BONO :$"+TS.TEMP1$  	! Valor del Bono
     Call U.IMPRIME(TS.TEMP1$,4200H)																			  ! Imprime dato
     TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(00H)+" "
     Call U.IMPRIME(TS.TEMP1$,4000H)																			  ! Imprime dato

     TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(00H)+" "
     Call U.IMPRIME(TS.TEMP1$,4400H)																			  ! Imprime dato
     TS.TEMP1$ = "FIRMA:______________________________"											! Firma
     TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												! Ajusta Dato impresion
     Call U.IMPRIME(TS.TEMP1$,4101H)																			  ! Imprime dato
    
     TS.TEMP1$ = Gr.Lcl.Name$                         											! Nombre del cliente
     TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)												! Ajusta Dato impresion
     Call U.IMPRIME(TS.TEMP1$,4100H)																			  ! Imprime dato
!--- Add 3/May/2022
     For XI% = 1 To Gr.NtaCrd.NmLin%																				! Imprime texto bono
       TS.TEMP1$ = Gr.NtaCrd.Msg$(Xi%)
       TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)											! Ajusta Dato impresion
       Call U.IMPRIME(TS.TEMP1$,4100H)																		  ! Imprime dato
     Next Xi%
     Asic.Detalle% = 1																											! Dato de la trx actual
     TS.TEMP1$ = Linea.Detalle(38)  																				! Trailer del movimiento
     Call U.IMPRIME(TS.TEMP1$,4200H)                                        !
     TS.TEMP1$ = String$(37,"*")																						! Linea cierre voucher
     Call U.IMPRIME(TS.TEMP1$,4900H)																			  ! avance de papel
     Call U.CORTACR																												  ! Corta Papel

     Call visor.and.borrar("BONO DEVOLUCION POR VALOR "+Str$(TS.TEMP5I4))		!
     Call U.Imprime("*----   BONO DEVOLUCION COMPRA   ---*",2100H)		      ! Impresion Bono recompra
     CALL FORMAT.AMOUNT(TS.TEMP5I4)																					!
     Call U.IMPRIME("BONO GENERADO :",2100H)																!
     Call U.IMPRIME(XNRO$,2100H)																	          !
     Call U.IMPRIME("VALOR :"+TS.TEMP1$,2100H)															!
     Call U.Imprime("*-----------------------------------*",2100H)		      ! Impresion Bono recompra
     TS.LINETYPE = 18																											  ! Genera cabecera 
     TS.LINEDATA = 1																											  ! en proximo tiquete
     Call TSPREC01																												  !
 	  
 EndIf																																			!
End Sub 																																		! Fin impresion nota devol

Sub Aplica.Dscto.NotaCr
Integer*4 Xi%
 If Gr.NtaCrd.Discount% > 0 Then Begin																			! Si Dscto aplicar

    SL.DI.AMTDISCO = Gr.NtaCrd.Discount%																		! Total Descuento
    SL.DI.DISRATE = 0																												! Tasa descuento
    TS.BALDUE(0) = TS.BALDUE(0) + Gr.NtaCrd.Discount%												! Actualiza variables aplicacion
    SL.HD.GROSSNEG  = SL.HD.GROSSNEG - Gr.NtaCrd.Discount%									!
    TS.GROSSNEG = TS.GROSSNEG - Gr.NtaCrd.Discount%													!
    TS.TOTALS(0,0,1) = TS.TOTALS(0,0,1) - Gr.NtaCrd.Discount%								!
    TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) - Gr.NtaCrd.Discount%							!
    TS.TOTALS(0,0,0) = TS.TOTALS(0,0,0) + Gr.NtaCrd.Discount%								!
 	  SL.END = SL.END + 1                                                   	! Aumenta apuntador de String
    SL.STR$(SL.END) = Pack$("04")+":"+Pack$(Gr.Motor2.GrpDscto$) + ":" +   \! Almacena String de descuento
		                  Pack$("00")+ ":" + Pack$(Str$(Gr.NtaCrd.Discount%))  + \!
		                  ":"+Pack$("00") + ":" 														  	!
		If Gr.NtaCrd.Tipo$ = "1" Then Begin																			! Anulacion total
      For XI% = 1 To Gr.NtaCrd.Itm%																					! Total Items
         TS.TEMP1$ =  Pack$(ItmsNtCrd$(Xi%,0))         +                   \! Codigo del producto
                ":" + Pack$(ItmsNtCrd$(Xi%,4))         +                   \! Valor del descuento
                ":" + Pack$("9999")                    +                   \! Codigo de la promocion
                ":" + Pack$(ItmsNtCrd$(Xi%,9))         +                   \! Flag de iva
                ":" + Pack$(ItmsNtCrd$(Xi%,7))         +                   \! Codigo del departamento
                ":" + Pack$(ItmsNtCrd$(Xi%,8))         +                   \! Estructura comercial
                ":" + Pack$("01")                      +                   \! Signo de la operacion
                ":" + Pack$(ItmsNtCrd$(Xi%,10))        +                   \! Total venta items
                ":" + Pack$(ItmsNtCrd$(Xi%,5))                              ! Cantidad vendida items
         Call Grabacion.String.Usuario2("97",TS.TEMP1$)            					! Graba String usuario
      Next XI%																															! Fin grabacion UD dscto
    EndIf																																		! Fin Nc total
		If Gr.NtaCrd.Tipo$ = "2" Then Begin                                     ! Si nota crd parcial
      For XI% = 1 To Gr.NtaCrd.Itm%																					! Total Items
       If Val(ItmsNtCrd$(XI%,6)) <> 0 Then Begin                            ! Anulacion parcial
         TS.TEMP1$ =  Pack$(ItmsNtCrd$(Xi%,0))         +                   \! Codigo del producto
                ":" + Pack$(ItmsNtCrd$(Xi%,4))         +                   \! Valor del descuento
                ":" + Pack$("9999")                    +                   \! Codigo de la promocion
                ":" + Pack$(ItmsNtCrd$(Xi%,9))         +                   \! Flag de iva
                ":" + Pack$(ItmsNtCrd$(Xi%,7))         +                   \! Codigo del departamento
                ":" + Pack$(ItmsNtCrd$(Xi%,8))         +                   \! Estructura comercial
                ":" + Pack$("01")                      +                   \! Signo de la operacion
                ":" + Pack$(ItmsNtCrd$(Xi%,10))        +                   \! Total venta items
                ":" + Pack$(ItmsNtCrd$(Xi%,5))                              ! Cantidad vendida items
         Call Grabacion.String.Usuario2("97",TS.TEMP1$)            					! Graba String usuario
       EndIf
      Next XI%																															! Fin grabacion UD dscto
    EndIf																																		! Fin Nc parcial
     
 EndIf
  
End Sub 

Sub Aplica.Pago.NCrd																												! Aplica pago NC
String Xbono$
 Xbono$ = Right$("000"+TS.STORE$,3) + 														         \! Nro tienda             Asc  3
          Right$("00"+Str$(Val(TS.TERMINAL$)),2)      +                    \! Nro terminal           Asc  2
          Right$("000"+Str$(Sl.Hd.Transnum + 1),3)    +                    \! Nro Transaccion        Asc  3
          UNPACK$(SL.HD.DATETIME$)																	        ! Fecha y hora           Asc 10

 Dim Ts.Io.Keys(10)																													! Inicializa Vectores
 Dim Ts.Io.Data$(10)																												! 
 TS.IO.DATA$(3) = Right$(Gr.BonDev.TipVar$,1)                               ! Toma la variedad de pago
 TS.IO.KEYS(3)  = 78																												! Simula la tecla slash
 TS.IO.DATA$(7) = "0"																												! Precio de la compra
 TS.IO.KEYS(7)  = 90 + Val(Left$(Gr.BonDev.TipVar$,1))                      ! Arma secuencia tecleo motor
 TS.IO.MOTORKEY = 90 + Val(Left$(Gr.BonDev.TipVar$,1))                      ! Arma secuencia tecleo motor
 TS.IO.DATA$(9) = Xbono$ 
End Sub 																																		!

Sub Carga.Msg.Bondev																												! Carga mensajes tkt bono devol
String Xdata$																																!
 TS.ER.RETURN = -1																													!
 Gr.NtaCrd.NmLin% = 0																												!
 Open "R::TF:DEFBON" As 94 NoWrite Nodel																		! Archivo mensajes
 If TS.ER.RETURN <> -1 Then Begin																						! Falla apertura
  	 Call VISOR.AND.BORRAR("ERROR APERTURA MSG BONOS DEV")									! MSg alerta
     Call U.Imprime("ERROR APERTURA MSG BONOS DEV",2100H)										! rastro en el sj
  	 Exit Sub 																															! Sale del proceso
 EndIf																																			!
If End # 94 Then CONT.BONDEV																								! Control archivo
LEER.BONDEV:																																! Barrido archivo
   	 READ # 94 ;  Line Xdata$																								! Lectura data
     Gr.NtaCrd.NmLin% = Gr.NtaCrd.NmLin% + 1																! Aumenta apuntador
     If Gr.NtaCrd.NmLin% > 50 Then GoTo CONT.BONDEV													! Maximo 50 lineas
     Gr.NtaCrd.Msg$(Gr.NtaCrd.NmLin%) = Xdata$															! Carga mensaje
GoTo LEER.BONDEV:																														! Continua proceso
CONT.BONDEV:																																! Termina proceso
  Close 94

End Sub 

Sub NOTASCREDITOPOS(Xopt%) Public																						! Modulo Notas Credito
Integer*4 Xopt%, Gri%

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.NtaCrd.Ok%  = 0                                                        ! Proyecto Apagado
  Dim Gr.NtaCrd.Msg$(50)
  TS.ER.RETURN = -1																													! Ctrl Errores
  Open "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL               							! Apertura archivo parametrizacion
  If TS.ER.RETURN <> -1 Then BEGIN                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA NOTAS CREDITO")									! MSg alerta
  	 Exit Sub 																															! Sale del proceso
  EndIf 																																		!
  IF END #94 THEN UE.FIN.NTACRD         																	  ! Si es EOF
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        IF TS.TEMP1$ = "[NOTAS CREDITO POS]" Then Begin	         				    ! Notas Credito
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.NtaCrd.Ok%   = Val(Mid$(TS.TEMP1$,30,2))   				    					! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.NtaCrd.Motor$ = Mid$(TS.TEMP1$,30,03)          				          ! Tecla Motora 
         Read #94; TS.TEMP1$     																						! Lectura registro                 
         Gr.NtaMnf.Motor$ = Mid$(TS.TEMP1$,30,03)          				          ! Tecla Motora 
         
         GoTo UE.FIN.NTACRD  																								! Sale del ciclo de carga          
       EndIf                                                                !
   Wend                                                                     !
   UE.FIN.NTACRD:                                                           !
     Close 94																																! Cierra archivo
     Call Carga.Msg.Bondev																									! Mensaje bono devolucion
   If Gr.NtaCrd.Ok% = -1 Then                                              \! Proyecto Activo
      Call U.IMPRIME("MODULO NOTAS CREDITO ON  21-Feb-2025",2100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO NOTAS CREDITO OFF 21-Feb-2025",2100H)          ! Msg Proyecto Cargado
EndIf 																																			! Fin carga opciones

If Gr.NtaCrd.Ok% <> -1 Then Exit Sub                                        ! Si proyecto apagado

!-- EAMTSU02
If Xopt% = 02 Then Begin																										! En eamtsu02
	 Call IMPRESION.NOTA.DEVOL																								! Impresion nota devol
   Gr.NtaCrd.Proc% = 0 																									    !
   Gr.NtaCrd.InTrx% = 0 																									  !
   Gr.NtaCrd.Cont% = 0																											!
   Gr.NtaCrd.Tipo$ = ""
   Gr.NtaCrd.Tori$ = ""   
   Gr.NtaCrd.Nlin$ = "0"
   Gr.NtaCrd.NItm$ = "0"
   Gr.Ntacrd.Bono$ = ""
   Gr.NtaCrd.Cliori$ = ""
   Gr.NtaCrd.Preori$ = ""
   Gr.NtaCrd.Facori$ = "" 
   Gr.NtaCrd.Cufe$   = ""
   Gr.NtaCrd.TipClte$ = ""
   Gr.NtaCrd.Discount% = 0   
EndIf																																				! Fin eamtsu02

!-- EAMTSU08
If Xopt% = 08 Then Begin																										! En eamtsu08
	 If Gr.RetMf.Proc% = -1 Then Begin																				! En reintegro MF
	 	  TS.IO.KEYS(8) = 67																									  ! Reporta devol Item
	 	  Exit Sub 																															!
	 EndIf																																		!
	 If Gr.NtaCrd.Proc% = 0 And Gr.NtaCrd.InTrx% = -1 Then Begin              ! En procedo de Nta Crd
	 	  If Gr.NtaCrd.Tipo$ = "1" Then Begin																		! Nota Total
	 	  	 Call VISOR.AND.BORRAR("NO SE PERMITE REGISTRO MAS ITEMS")					! Msg Alerta
	 	  	 IR.INDICAT0 = IR.INDICAT0 OR 04H			                							! No permite venta
	 	  	 Exit Sub 																													! Sale del proceso
	 	  EndIf																																	!
	 	  If Gr.NtaCrd.Tipo$ = "2" Then Begin																		! Nota Parcial
	 	  	 TS.IO.KEYS(8) = 67																									! Reporta devol Item
	 	  EndIf																																	!
	 EndIf
	 If Gr.NtaCrd.Proc% = -1 And Gr.NtaCrd.Tipo$ = "1" Then Begin							! En Recuperacion nota total
	 	  If Gr.NtaCrd.Cont% <= Gr.NtaCrd.Itm% Then Begin												! 
	 	  	 CHAIN.COUNT = 1                                             				! Contador de item linkeados
	 	  	 Gr.NtaCrd.Nlin$ = Str$(Gr.NtaCrd.Itm%-1)                           ! Total de Items
	 	  	 If Gr.NtaCrd.Cont% = 1 Then Begin
            SL.IT.ITEMCODE$ = ItmsNtCrd$(1,0)
            TS.IO.DATA$(2)  = SL.IT.ITEMCODE$																!
	 	  	 EndIf
	  	 	 TS.IO.KEYS(8) = 67 																								! Anulacion items
         If IR.INDICAT0 AND 40H Then Begin        													! Si el Item es pesable 
            TS.IO.KEYS(6)  = 72																							! Simula secuencia registro
            TS.IO.DATA$(6) = ItmsNtCrd$(Gr.NtaCrd.Cont%,1)
            Gr.NtaCrd.NItm$ = Str$(Val(Gr.NtaCrd.NItm$)+1)
         	  If Val(ItmsNtCrd$(Gr.NtaCrd.Cont%,4)) <> 0 Then Begin  						! Valor Dscto
               ItmsNtCrd$(Gr.NtaCrd.Cont%,6) = Str$(((Val(ItmsNtCrd$(Gr.NtaCrd.Cont%,4))*1000) /  \!
               Val(ItmsNtCrd$(Gr.NtaCrd.Cont%,5)))*Val(TS.IO.DATA$(6)))  ! Valor dscto
               Gr.NtaCrd.Discount% = Gr.NtaCrd.Discount% + Val(ItmsNtCrd$(Gr.NtaCrd.Cont%,6))
         	  EndIf
         EndIf Else Begin																										! Item por cantidad
            TS.IO.KEYS(6)  = 75																							! Simula cantidad
            TS.IO.DATA$(6) = ItmsNtCrd$(Gr.NtaCrd.Cont%,1)
            Gr.NtaCrd.NItm$ = Str$(Val(Gr.NtaCrd.NItm$) + Val(TS.IO.DATA$(6)))
         	  If Val(ItmsNtCrd$(Gr.NtaCrd.Cont%,4)) <> 0 Then Begin  						! Valor Dscto
               ItmsNtCrd$(Gr.NtaCrd.Cont%,6) = Str$(((Val(ItmsNtCrd$(Gr.NtaCrd.Cont%,4))) /  \!
               Val(ItmsNtCrd$(Gr.NtaCrd.Cont%,5)))*Val(TS.IO.DATA$(6)))  ! Valor dscto
               Gr.NtaCrd.Discount% = Gr.NtaCrd.Discount% + Val(ItmsNtCrd$(Gr.NtaCrd.Cont%,6))
         	  EndIf
         EndIf																															!
	 	  	 If ItmsNtCrd$(Gr.NtaCrd.Cont%+1,0) = "999999999999" Then Begin  	  ! Ultimo del proceso
	 	  	 	  IR.LINKEDTO$ = Pack$("0000")																  	! Termina ciclo
	 	  	 	  Gr.NtaCrd.Proc% = 0 
	 	  	 	  Call Aplica.Dscto.NotaCr
	 	  	 EndIf Else Begin
	 	  	 	  TS.TEMP4$ = Right$("000000000000"+ItmsNtCrd$(Gr.NtaCrd.Cont%+1,0),12) 
            IR.LINKEDTO$ = Pack$(TS.TEMP4$)	          											!
         EndIf
         IR.PRICE1 = Val(ItmsNtCrd$(Gr.NtaCrd.Cont%,2))
         IR.PRICE2 = IR.PRICE1
	 	  	 Gr.NtaCrd.Cont% = Gr.NtaCrd.Cont% + 1															! 
	 	  EndIf 																																! Fin Nota total
	 EndIf																																		! Fin simulacion
!-- Para manejo nota parcial
   If Gr.NtaCrd.Proc% = 0 And Gr.NtaCrd.Tipo$ = "2" Then Begin					    ! En Recuperacion nota parcial
	    	 If (TS.IO.KEYS(1) = 70) Then Begin
	    	 	  Call Visor.And.Borrar("PROCESO NO AUTORIZAD /Borrar")						! Msg Alerta
	 	  	    IR.INDICAT0 = IR.INDICAT0 OR 04H			                					! No permite venta
	 	  	    Exit Sub 																												! Sale del proceso
 	    	 EndIf
 	    	 If TS.IO.DATA$(6) = "" Then Begin																	! Si qty no capturada
	    	 	  Call Visor.And.Borrar("ENTRE CANTIDAD/PESO  /Borrar")						! Msg Alerta
	 	  	    IR.INDICAT0 = IR.INDICAT0 OR 04H			                					! No permite venta
	 	  	    Exit Sub 																												! Sale del proceso
 	    	 EndIf																															!
         TS.TEMP1I2 = 0																											! Control de errores
         Call ValidaItmsNraCrd																							! Valida Item capturado
         If TS.TEMP1I2 <> 0 Then Begin 																			! Si Item OK
            Gr.NtaCrd.Nlin$ = Str$(Val(Gr.NtaCrd.Nlin$) + 1 )								!
            Gr.NtaCrd.Cont% = TS.TEMP1I2																		! Posicion Item a devolver
         	  If IR.INDICAT0 AND 40H Then Begin        												! Si el Item es pesable 
         	     Gr.NtaCrd.NItm$ = Str$(Val(Gr.NtaCrd.NItm$) + 1)             ! Qty de items
               If Val(ItmsNtCrd$(TS.TEMP1I2,4)) <> 0 Then Begin  						! Valor Dscto
                ItmsNtCrd$(TS.TEMP1I2,6) = Str$(((Val(ItmsNtCrd$(TS.TEMP1I2,4))*1000) /  \!
                Val(ItmsNtCrd$(TS.TEMP1I2,5)))*Val(TS.IO.DATA$(6)))         ! dscto aplicado reportar nc
                Gr.NtaCrd.Discount% = Gr.NtaCrd.Discount% + Val(ItmsNtCrd$(TS.TEMP1I2,6))
         	     EndIf
         	  EndIf Else Begin																								!
         	  	 Gr.NtaCrd.NItm$ = Str$(Val(Gr.NtaCrd.NItm$) +               \! 
         	  	                        Val(TS.IO.DATA$(6)))                  !
               If Val(ItmsNtCrd$(TS.TEMP1I2,4)) <> 0 Then Begin  						! Valor Dscto
                ItmsNtCrd$(TS.TEMP1I2,6) = Str$(((Val(ItmsNtCrd$(TS.TEMP1I2,4))) /  \!
                Val(ItmsNtCrd$(TS.TEMP1I2,5)))*Val(TS.IO.DATA$(6)))         ! dscto aplicado reportar nc
                Gr.NtaCrd.Discount% = Gr.NtaCrd.Discount% + Val(ItmsNtCrd$(TS.TEMP1I2,6))
         	     EndIf
         	  	                        
         	  EndIf																														!
         	  IR.PRICE1 = Val(ItmsNtCrd$(TS.TEMP1I2,2))            						! Reporta precio unitario
         	  ItmsNtCrd$(TS.TEMP1I2,3) = Str$(Val(ItmsNtCrd$(TS.TEMP1I2,3)) + \!
         	                                  Val(TS.IO.DATA$(6)))            !
         EndIf Else Begin																										! Falla validacion Item
	 	  	   IR.INDICAT0 = IR.INDICAT0 OR 04H			                						! No permite venta
	 	  	   Exit Sub 																												! Sale del proceso
         EndIf																															! 
   EndIf                                                                    ! Fin nota parcial
	 
EndIf																																				! Fin Eamtsu08

!-- EAMTSU14
If Xopt% = 14 Then Begin																										! En eamtsu14

   TS.TEMP5$ = ";"+Str$(TS.IO.MOTORKEY) + ";"																! Tecla motora presionada
   If (TS.IO.KEYS(7) > 90 AND TS.IO.KEYS(7) < 97) Then Begin                ! Valida si en forma de pago 
    If Gr.RetMf.Proc% = -1 Then Begin																				! Si en proceso
     If TS.IO.KEYS(7) <> 93 Then Begin																			! Si pago diferente a MF
     	Call VISOR.AND.BORRAR("FORMA PAGO NO AUTOR.SOLO CREDITO /Borrar")  	  !
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Init secuencias registro
      TS.IO.MOTORKEY = 73																										!
   	  Exit Sub 																															!
     EndIf																																	! 
    EndIf																																		!
   EndIf																																		!
	
	 If TS.IO.MOTORKEY = Val(Gr.NtaMnf.Motor$) Then Begin											! Reintegro mf
      If Gr.RetMf.Proc% = -1 Then Begin																			! Si en proceso
    	   Call VISOR.AND.BORRAR("REINTEGRO MF ESTA   EN PROCESO /Borrar")  	!
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														! Init secuencias registro
         TS.IO.MOTORKEY = 73																								!
      	 Exit Sub 
      EndIf																																	!
	 	  Gr.RetMf.Proc% = -1																										! Activa proceso
    	Call VISOR.AND.BORRAR("REGISTRE ITEMS M.F. A DEVOLVER /Borrar")    		!
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Init secuencias registro
      TS.IO.MOTORKEY = 73																										!
      Exit Sub 																															!
	 EndIf 
	 If Gr.NtaCrd.InTrx% = -1 Then Begin																			! En proceso de Ntas Crd
      If TS.IO.KEYS(1) = 82 AND \                                           ! Proceso de suspension de
         TS.IO.KEYS(6) = 81 Then Begin                                      ! Capturada
	 	  	 Call VISOR.AND.BORRAR("PROCESO NO PERMITIDO PARA NOTAS CRD")       ! Msg Alerta
         Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)                						!
         TS.IO.MOTORKEY = 73 																								!
         Exit Sub      																											!
      EndIf																																	! Fin suspension
      If Val(Gr.NtaCrd.Cliori$) <> Val(Gr.Lcl.Clte$) Then Begin             ! Cliente no similar
	 	  	 Call VISOR.AND.BORRAR("CLIENTE CAPTURADO NO SIMILAR FACT ORIGEN")  ! Msg Alerta
         Call VISOR.AND.BORRAR("REPORTAR NOTA A CLTE "+Str$(Val(Gr.NtaCrd.Cliori$)))
         Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)                						!
         TS.IO.MOTORKEY = 73 																								!
         Exit Sub      																											!
      	
      EndIf
      If Val(Gr.NtaCrd.Tipo$) = 2 Then Begin																! Nota parcial
        If (TS.IO.KEYS(7) > 90 AND TS.IO.KEYS(7) < 97) Then Begin           ! Valida si en forma de pago 
        	 TS.IO.DATA$(7)= "0"																							! Para cerrar trx 
        	 Call SALVAR.SECUENCIA																						!
      	   Call Aplica.Dscto.NotaCr																					! Aplica Dscto trx 
      	   Call RESTAURAR.SECUENCIA																					!
        EndIf
      EndIf
      
	 	  If TS.IO.KEYS(7) > 90 And TS.IO.KEYS(7) < 97 Then BEGIN               ! Simular pago
         Call Aplica.Pago.NCrd	 	  	 
	 	  EndIf 
      
      
	 EndIf																																		!
	 If TS.IO.MOTORKEY = Val(Gr.NtaCrd.Motor$) Then Begin											! Tecla Motora Notas Credito
      If USER.FBACT.READ = 0 Then Begin 																		! Cliente no capturado
 	       Call VISOR.AND.BORRAR("DATOS DE CLIENTE NO  CAPTURADO   /Borrar")	! Msg de alerta
 	       Call VISOR.AND.BORRAR("NO SE GENERA NOTA    CREDITO     /Borrar")	!
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Gr.NtaCrd.Tipo$ = ""
         Exit Sub 																											    !
      EndIf																																	!
	 	  If TS.INTRX Then Begin																								! Esta en una trx
     	   Call VISOR.AND.BORRAR("PROCESO NOTA CREDITOSOLO FUERA DE VENTAS")  !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Gr.NtaCrd.Tipo$ = ""
         Exit Sub 																											    !
	 	  EndIf
	 	
	 	  Gr.NtaCrd.Proc% = 0
      Gr.NtaCrd.Tipo$ = ASIC.DATOS$("TIPO DE OPERACION","1.TOTAL 2.PARCIAL ")
      If Gr.NtaCrd.Tipo$ = "E" Then BEGIN 																  ! Proceso cancelado	
      	 Call VISOR.AND.BORRAR("PROCESO CANCELADO PORUSUARIO   /Borrar")
      	 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														! Init secuencias registro
      	 TS.IO.MOTORKEY = 73																								! Simula tecla borrar
      	 Gr.NtaCrd.Tipo$ = ""
      	 Exit Sub 																													!
      EndIf
      If Val(Gr.NtaCrd.Tipo$) < 1 Or Val(Gr.NtaCrd.Tipo$) > 2 Then Begin		! Dato Capturado no valido
      	 Call VISOR.AND.BORRAR("TIPO DE NOTA CRD  NO VALIDO    /Borra")     !
      	 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														! Init secuencias registro
      	 TS.IO.MOTORKEY = 73																								! Simula tecla borrar
      	 Gr.NtaCrd.Tipo$ = ""
      	 Exit Sub 																													!
      EndIf
      Gr.NtaCrd.Tori$ = ASIC.DATOS$("# TRANSACCION ORIGEN","")							! 
      If Gr.NtaCrd.Tori$ = "E" Then BEGIN 																  ! Proceso cancelado	
      	 Call VISOR.AND.BORRAR("PROCESO CANCELADO PORUSUARIO   Borrar")
      	 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														! Init secuencias registro
      	 TS.IO.MOTORKEY = 73																								! Simula tecla borrar
      	 Gr.NtaCrd.Tori$ = ""
      	 Gr.NtaCrd.Tipo$ = ""
      	 Exit Sub 																													!
      EndIf
      If Len(Gr.NtaCrd.Tori$) <> 20 Then Begin															! Dato incorrecto
      	 Call VISOR.AND.BORRAR("# TIQUETE NO VALID0 OPERACION CANCELADA ")  ! Msg Alerta
      	 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														! Init secuencias registro
      	 TS.IO.MOTORKEY = 73																								! Simula tecla borrar
      	 Gr.NtaCrd.Tori$ = ""
      	 Gr.NtaCrd.Tipo$ = ""
      	 Exit Sub 																													!
      EndIf																																	! Fin data
	 	  TS.TEMP1I1 = -1																												! Control proceso
      Call ListaItmsNtaCrd																									! Carga items nota credito
      If TS.TEMP1I1 <> -1 Then Begin																				! Error apertura
      	 Call VISOR.AND.BORRAR("FALLO RECUP. TRANSACORIGEN    /Borrar")     !
      	 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														! Init secuencias registro
      	 TS.IO.MOTORKEY = 73																								! Simula tecla borrar
      	 Gr.NtaCrd.Tori$ = ""
      	 Gr.NtaCrd.Tipo$ = ""
      	 Exit Sub 																													!
      EndIf																																	!
      Gr.NtaCrd.Proc% = 0 																									! Ntas parciales
      Gr.NtaCrd.InTrx% = -1																									!
      Gr.NtaCrd.Cont% = 1
      If Gr.NtaCrd.Tipo$ = "1" Then Begin																		! Nota crd total
         Gr.NtaCrd.Proc% = -1																								! Ntas totales
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														! Init secuencias registro
         TS.IO.KEYS(2)  = 80																								!
         TS.IO.DATA$(2) = ItmsNtCrd$(1,0)																		! Entrega plu
         TS.IO.DEVICE   = 1																									!
         TS.IO.MOTORKEY = 80																								!
      EndIf Else Begin																											!
      	 Call VISOR.AND.BORRAR("REGISTRE LOS ITEMS  A DEVOLVER /Borrar")    !
      	 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														! Init secuencias registro
      	 TS.IO.MOTORKEY = 73																								!
      	 Gr.NtaCrd.Discount% = 0																						! Init total dsctos recuperados
      EndIf
    EndIf  
EndIf																																				! Fin eamtsu14

!-- EAMTSU20
If Xopt% = 20 Then Begin																										! En eamtsu20
  If MATCH(TS.SDESC$(59),TS.PRTBUF$,1) > 0 THEN BEGIN												! Si anulacion total de la trx
  	 Gr.NtaCrd.InTrx% = 0																										! no creacion bono devol
  EndIf																																			!

If (TS.PROCEDURE < 1 AND TS.INTRX) Then Begin       												! En una trx
 If(TS.LINETYPE = 6 And TS.LINEDATA = 1) Then Begin                         ! Personalized message
  If Gr.NtaCrd.InTrx% = -1 Then Begin	    																	! Es una nota credito
     Call Visores4690(1,"ALMACENANDO NOTA","CREDITO ESPERE ...  ",0,"L")    ! 
     Call ALMACENA.NOTA.CREDITO                                             !
     Call DETALLE.NOTA.CRD																									! Detalle NC
     Call GENERAR.BONO.DEVOL																								! Generar Bono devol
     Gr.NtaCrd.InTrx% = -2
   EndIf
 EndIf
EndIf

EndIf																																				! Fin Eamtsu20

If Xopt% = 60 Then Begin																										! En eamtsu60
 If TS.LINETYPE = 2 And Gr.NtaCrd.Discount% <> 0 Then Begin
  	 Call Format.Amount(Gr.NtaCrd.Discount%)
  	 TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)
  	 TS.TEMP2$ = " DESCUENTO ANULADO:"+TS.TEMP1$
  	 TS.TEMP2$ = RIGHT$(STRING$(38," ")+TS.TEMP2$,38)  
  	 Call U.IMPRIME(TS.TEMP2$,4101H)																			  ! Imprime dato
  	 Call U.IMPRIME(TS.TEMP2$,2100H)																			  ! Imprime dato
 EndIf
EndIf																																				! Fin Eamtsu60

!--- EAMTSU23.J86
If XOPT% = 23 Then Begin 																										! En Visores de cliente
 Exit Sub 
 If Gr.NtaCrd.Discount% <> 0 Then                   											 \! Dscto Anul trx 	
  If Match("** TOTAL",TS.DISP2$,1) > 0 Then Begin													  ! Presentacion linea total
   If Gr.NtaCrd.Discount% > 0 Then                                         \!	
 		  Call Format.AMOUNT(Gr.NtaCrd.Discount%) Else 							     \!
 		  Call Format.AMOUNT(Gr.NtaCrd.Discount% * -1) 
	 TS.DISP1$ = "DESCUENTO:"+Right$("          "+TS.TEMP1$,10)							  !
   If Gr.NtaCrd.Discount% > 0 Then                                         \!
 		  TS.TEMP1I4 = (TS.BALDUE(0) + Gr.NtaCrd.Discount%)  Else              \!
 		  TS.TEMP1I4 = (TS.BALDUE(0) - (Gr.NtaCrd.Discount%*-1)) 
     TS.TEMP1I4 = (TS.BALDUE(0))
     Call Format.Amount(TS.TEMP1I4)																					!
     TS.DISP2$ = Left$(TS.SDESC$(68),10)+Right$("          "+TS.TEMP1$,10)  ! 
     TS2.DISP1$ = TS.DISP1$																									!
     TS2.DISP2$ = TS.DISP2$																									!
  EndIf																																			! Fin Linea Total

EndIf

!--- EAMTSU43.J86
If XOPT% = 43 Then Begin 																										! En calculo precio Item
 If Gr.NtaCrd.InTrx% = -1 Then Begin																				! 
  If Gr.NtaCrd.Tipo$ = "1" Then Begin 																			! Nota total

 ! 	If Val(ItmsNtCrd$(Gr.NtaCrd.Cont%-1,4)) <> 0 Then Begin
 ! 		 TS.TEMP5I4 = Val(ItmsNtCrd$(Gr.NtaCrd.Cont%-1,4)) /                 \! Dscto unitario
 ! 		              Val(ItmsNtCrd$(Gr.NtaCrd.Cont%-1,5))
 !   EndIf Else TS.TEMP5I4 = 0                                               ! No hay dscto Item
 	
 	  !SL.IE.SALEPRIC = Val(ItmsNtCrd$(Gr.NtaCrd.Cont%-1,2)) - TS.TEMP5I4			! precio unitario
 	  
 	  SL.IE.SALEPRIC = Val(ItmsNtCrd$(Gr.NtaCrd.Cont%-1,2))             			! precio unitario
 	  If IR.INDICAT0 AND 40H Then Begin        																! Si el Item es pesable 
 	  	SL.IT.XPRICE = (SL.IE.SALEPRIC * SL.IE.QTYORWGT) / 1000								! Precio pesable total
 	  EndIf Else Begin																												!
 	    SL.IT.XPRICE = SL.IE.SALEPRIC * SL.IE.QTYORWGT												! Precio unitario total
 	  EndIf																																		!
 	  ItmsNtCrd$(Gr.NtaCrd.Cont%-1,10) = Str$(SL.IT.XPRICE)
 	EndIf

  If Gr.NtaCrd.Tipo$ = "2" Then Begin 																			! Nota Parcial
!  	If Val(ItmsNtCrd$(Gr.NtaCrd.Cont%,4)) <> 0 Then Begin
!  		 TS.TEMP5I4 = Val(ItmsNtCrd$(Gr.NtaCrd.Cont%,4)) /                   \! Dscto unitario
!  		              Val(ItmsNtCrd$(Gr.NtaCrd.Cont%,5))
!    EndIf Else TS.TEMP5I4 = 0                                               ! No hay dscto Item
	
 	  !SL.IE.SALEPRIC = Val(ItmsNtCrd$(Gr.NtaCrd.Cont%,2)) - TS.TEMP5I4	  		! precio unitario
  	
 	  SL.IE.SALEPRIC = Val(ItmsNtCrd$(Gr.NtaCrd.Cont%,2))										  ! precio unitario
 	  
 	  If IR.INDICAT0 AND 40H Then Begin        																! Si el Item es pesable 
 	  	SL.IT.XPRICE = (SL.IE.SALEPRIC * SL.IE.QTYORWGT) / 1000								! Precio pesable total
 	  EndIf Else Begin																												!
 	    SL.IT.XPRICE = SL.IE.SALEPRIC * SL.IE.QTYORWGT												! Precio unitario total
 	  EndIf																																		!
 	EndIf

 	
 EndIf																																			!
EndIf																																				!

End Sub

