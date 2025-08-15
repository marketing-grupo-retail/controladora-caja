!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRITMCTL.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Articulos Controlados            *
!**************************************************
!Observaciones:
!
!

%ENVIRON T		                          																			! Ambiente de terminal

Integer*1 Global Gr.Ict.Ok%, Gr.Ict.Cap%, Gr.Ict.Stm%, Gr.Ict.Open%   				! Variables de control
String    Global Gr.Ict.Proy$, Gr.Ict.Ente$, Gr.Ict.Msg$(2)                   ! 
Integer*1 Global Gr.Ict.Dlg1%, Gr.Ict.Dlg2%, Gr.Ict.Dlg3%, Gr.Ict.Tkt%        !
String    Global Gr.Ict.Dlg1$, Gr.Ict.Dlg2$, Gr.Ict.Dlg3$, Gr.Ict.Lista$(1)   !
Integer*4 Global Gr.Ict.Msg%(2), Gr.Ict.Qtyente%
Integer*2 Global Gr.Ict.Slend%
Integer*4 Global Asc.Tmp.Apun%
Integer*1 Global Gr.Ict.ConPrc%

%INCLUDE EAMTSCVA.J86
%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          !
%INCLUDE EAMTERMS.J86          !
%INCLUDE EAMTOPTS.J86	         !
%INCLUDE EAMP4VAJ.J86          !
%INCLUDE EAMITEMR.J86          !
%INCLUDE NTAMTSSU.011 				 ! Subrutinas Grupo Retail 
%INCLUDE EAMTSXHC.J86     

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                      *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                         *!
  STRING    DATA$      !* String to hold the data read from file.            *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                               *!
End Sub 

SUB IRRFEC.SPLIT1 (RECORD$) EXTERNAL
  STRING    RECORD$    !* The record (including the key) read from file      *!
End Sub 

Function VTA.ITM.CTL(Xitm$)
Integer*1 VTA.ITM.CTL
String Xitm$, Xente$, Xqty$
  TS.TEMP2$ = Pack$(Right$("000000000000"+Xitm$,12)) 
  Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)              								! Lectura Datos Itemr
  Call IRRFEC.SPLIT1( TS.TEMP1$ ) 			                       								! Entrega datos al eamitemr.j86
  If UNPACK$(IR.INDICAT2$) = "05" Then BEGIN 																	! Si metodo precio 5
     Xitm$ = UNPACK$(IR.SALEQUAN$) + UNPACK$(IR.SALEPRIC$)                  	! Toma PLU asignado a la barra
  EndIf																																				!
  Xitm$ = Pack$(Right$("000000000000"+Xitm$,12))															! Arma llave de busqueda
  TS.ER.RETURN = -1																														! Ctrl de lectura
  READ FORM "C6 C1 C5";#58 KEY XITM$;                							           \! Lee Reg Cabecera 
        XITM$, XENTE$, XQTY$							  																	!	
  VTA.ITM.CTL = TS.ER.RETURN																									! Retorna respuesta
  Gr.Ict.Ente$ = Xente$ 																											! Retorna Ente de control
End Function

Sub Captura.Dlgo.ItmCtl
String  ENTE.COD$,  ENTE.NAME$, ENTE.DLG1$,  ENTE.DLGX$, 										 \! 
        ENTE.DLG2$, ENTE.DLGY$, ENTE.DLG3$,  ENTE.DLGZ$,  									 \! 
        ENTE.DLG4$, ENTE.DLGT$, Lec$, Ente.Monto$, Ente.Sgn$									! 
Integer*1 Xfound%
Integer*2 GJ%        
  TS.ER.RETURN = -1																														!
  Open "R::TF:ENTES" KEYED RECL 91 AS Gr.Ict.Stm%                  						! Abre archivo de Entes de control
  If TS.ER.RETURN <> -1 Then Begin 																						!
  	 Call VISOR.AND.BORRAR("ERROR VALIDACION ENTE CONTROL")					  				!
  	 Exit Sub 																																!
  Endif																																				!
  TS.ER.RETURN = -1																														!
  ENTE.COD$ = PACK$(RIGHT$("00"+Gr.Ict.Ente$,2))            									!
  LEC$="C1 C20 C1 C20 C1 C20 C1 C20 2C1 C5"                   										!
  READ FORM LEC$;#Gr.Ict.Stm% KEY ENTE.COD$;             							       \! Lee Reg Cabecera 
        ENTE.COD$,  ENTE.NAME$, ENTE.DLG1$,  ENTE.DLGX$, 										 \! 
        ENTE.DLG2$, ENTE.DLGY$, ENTE.DLG3$,  ENTE.DLGZ$,  									 \! 
        ENTE.DLG4$, ENTE.DLGT$, Ente.Monto$                										! 

  If TS.ER.RETURN <> -1 Then Begin																						! No existe registro
  	 Call VISORES4690(1,"ENTE NO ENCONTRADO",UNPACK$(ENTE.COD$),1200,"L")
  	 Close Gr.Ict.Stm%																											  !
  	 Exit Sub 																																!
  EndIf Else Begin																														! Registro encontrado
  	
  	If Gr.Ict.Dlg1% = 0 Then Begin																						! Primer Dialogo no capturado
  	 If ENTE.DLG1$ = "1" Then Begin																						! Si captura primer dialogo
        Gr.Ict.Dlg1$ = ASIC.DATOS$(ENTE.DLGX$,"")                             ! Captura dialogo
        If Gr.Ict.Dlg1$ = "E" Then Begin																			! Dialogo Cancelado
        	 IR.INDICAT0 = IR.INDICAT0 OR 04H								                    ! No permite venta
        	 Close Gr.Ict.Stm%
        	 Exit Sub 
        EndIf 
        Gr.Ict.Dlg1% = -1
  	 EndIf 																																		! 
    EndIf 																																		!

  	If Gr.Ict.Dlg2% = 0 Then Begin																						! Segundo Dialogo no capturado
  	 If ENTE.DLG2$ = "1" Then Begin																						! Si captura segundo dialogo
        Gr.Ict.Dlg2$ = ASIC.DATOS$(ENTE.DLGY$,"")                             ! Captura dialogo
        If Gr.Ict.Dlg2$ = "E" Then Begin																			! Dialogo Cancelado
        	 IR.INDICAT0 = IR.INDICAT0 OR 04H								                    ! No permite venta
        	 Close Gr.Ict.Stm%
        	 Exit Sub 
        EndIf 
        Gr.Ict.Dlg2% = -1
  	 EndIf 																																		! 
    EndIf 																																		!

  	If Gr.Ict.Dlg3% = 0 Then Begin																						! Tercer Dialogo no capturado
  	 If ENTE.DLG3$ = "1" Then Begin																						! Si captura Tercer dialogo
        Gr.Ict.Dlg3$ = ASIC.DATOS$(ENTE.DLGZ$,"")                             ! Captura dialogo
        If Gr.Ict.Dlg3$ = "E" Then Begin																			! Dialogo Cancelado
        	 IR.INDICAT0 = IR.INDICAT0 OR 04H								                    ! No permite venta
        	 Close Gr.Ict.Stm%
        	 Exit Sub 
        EndIf 
        Gr.Ict.Dlg3% = -1
  	 EndIf 																																		! 
    EndIf 																																		!
  EndIf 																																			!
  FIN.ICTDIALOGO:
   Close Gr.Ict.Stm%																													!
   Ente.Sgn$ = "00"																														! Trx positiva Venta
  If TS.IO.DATA$(6) = "" Then TS.TEMP2$ = "1" Else TS.TEMP2$ = TS.IO.DATA$(6) ! Cantidad Vendida
  If (TS.IO.KEYS(1) = 70) OR (TS.IO.KEYS(8) = 67) Then  Ente.Sgn$ = "01"      ! Si es una devolucion / anulacion
  TS.TEMP1$ = ENTE.COD$                     +                                \! Codigo del ente
              ":" + PACK$(SL.IT.ITEMCODE$)  +                                \! Itm Vendido y controlado
              ":" + PACK$(Gr.Ict.Dlg1$)     +                                \! Primer Dialogo
              ":" + PACK$(Gr.Ict.Dlg2$)     +                                \! Segundo Dialogo
              ":" + PACK$(Gr.Ict.Dlg3$)     +                                \! Tercer dialogo
              ":" + PACK$(TS.TEMP2$)        +                                \! Cantidad vendida
              ":" + PACK$(Ente.Sgn$)        +                                \! Signo operacion 
              ":" + PACK$("00")                                               ! Filler
  Call Grabacion.String.Usuario2(Gr.Ict.Proy$,TS.TEMP1$)
  Gr.Ict.Tkt% = -1
  Xfound% = 0 																																! Init Variable
  GJ% = 0  																																		! Init Variable
  For GJ% = 0 To Gr.Ict.Qtyente%																							! Busca Entes
    If Gr.Ict.Lista$(GJ%) = UNPACK$(ENTE.COD$) Then Xfound% = -1              ! Si lo encontro 
  Next GJ%																																		!
  If Xfound% = 0 Then Begin 																									! No esta en lista
  	 Gr.Ict.Qtyente% = Gr.Ict.Qtyente% + 1																		! Aumenta contador
  	 Gr.Ict.Lista$(Gr.Ict.Qtyente%) = UNPACK$(ENTE.COD$)      								! Ente para proceso de impresion
  EndIf
End Sub 

Sub LST.ITEM.ENTE(Yente$)
Integer*4 GI%
String Yente$, Yplu$
  For GI% = 1 To TS.TEMP1I4																										! Total de Items
     If Gr.Ict.Msg%(GI%,1) = Val(Yente$) Then Begin														! Si articulo del Ente
     	  Yplu$ = Str$(Gr.Ict.Msg%(GI%,0))																			! Item a imprimir
      	TS.TEMP2$ = Pack$(Right$("000000000000"+Yplu$,12)) 
        Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)              					! Lectura Datos Itemr
        Call IRRFEC.SPLIT1( TS.TEMP1$ ) 			                       					! Entrega datos al eamitemr.j86
     	  TS.TEMP1$ = Right$("       "+Str$(Gr.Ict.Msg%(GI%,0)),7) + " " +     \! Item Vendido
     	              IR.ITEMNAME$                                 + " " +     \! Descripcion
     	              Right$("       "+Str$(Gr.Ict.Msg%(GI%,2)),7)              ! Qty Vendido
        TS.TEMP1$ = Left$(TS.TEMP1$+String$(37," "),37)     									!	              
        Call U.IMPRIME(TS.TEMP1$,4100H)																				!
     EndIf																																		!
  Next GI%																																		!
End Sub 																																			!

Sub PROCESO.ENTES																															! Impresion de entes
Integer*2 GK%, GL%, GX%, GJ%																									!
String Xitm$, Xente$, Xqty$, XMSG$
  For GK% = 1 To Gr.Ict.Qtyente%																							! Nro entes Capturados
     GX% = 0 				  																												! Ctrl de impresion voucher
     TS.TEMP2I4 = 0
     For GL% = 1 To TS.TEMP1I4   																							! Analiza Entes
      If Gr.Ict.Msg%(GL%,1) = Val(Gr.Ict.Lista$(GK%)) Then Begin              ! Si Venta pertenece al ente
        TS.ER.RETURN = -1																											!
        Xitm$ = Str$(Gr.Ict.Msg%(GL%,0))																	    ! Item a Validar
        
        TS.TEMP2$ = Pack$(Right$("000000000000"+Xitm$,12)) 
        Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)              					! Lectura Datos Itemr
        Call IRRFEC.SPLIT1( TS.TEMP1$ ) 			                       					! Entrega datos al eamitemr.j86
        If UNPACK$(IR.INDICAT2$) = "05" Then BEGIN 														! Si metodo precio 5
           Xitm$ = UNPACK$(IR.SALEQUAN$) + UNPACK$(IR.SALEPRIC$)             	! Toma PLU asignado a la barra
        EndIf																																	!
        Xitm$ = PACK$(RIGHT$("000000000000"+XItm$,12))         							  !
        TS.ER.RETURN = -1																											! Ctrl de lectura
        READ FORM "C6 C1 C5";#58 KEY XITM$;                 				         \! Lee Reg Cabecera 
        XITM$, XENTE$, XQTY$																							    !	
        If TS.ER.RETURN = -1 Then BEGIN																				! Encontro Ente
   	     If Gr.Ict.Msg%(GL%,2) >= Val(Unpack$(XQTY$)) Then Begin              ! Cumple con venta controlada
            GX% = -1 
   	     EndIf																																!
        EndIf 
      EndIf																																		! Ente de Impresion      
     Next GL%
     If GX% = -1 Then Begin																										! Imprime Ente
         Xente$ = Gr.Ict.Lista$(GK%)																					! Ente a Imprimir
         TS.TEMP1$ = "Ced./Nit:"+Gr.Ict.Dlg1$
         TS.TEMP1$ = Left$(TS.TEMP1$+String$(37," "),37)
         Call U.IMPRIME(TS.TEMP1$,4100H)
         For GJ% = 1 TO Val(Gr.Ict.Msg$(Val(Xente$),0))		 								    ! Total de lineas a imprimir
             XMSG$ = Gr.Ict.Msg$ (Val(Xente$),GJ%)		  										  ! Toma linea de impresion
             Call U.IMPRIME(XMSG$,4100H)			           											! Imprime Texto
         Next GJ%                                        											!
         TS.TEMP1$ = "CODIGO  DESCRIPCION       CANTIDAD  "
         TS.TEMP1$ = Left$(TS.TEMP1$+String$(37," "),37)
         Call U.IMPRIME(TS.TEMP1$,4100H)
         Call LST.ITEM.ENTE(Xente$)                                           ! Lista los productos del Ente
         TS.TEMP1$ = Linea.Detalle(38)																				!
         Call U.IMPRIME(TS.TEMP1$,4100H)
         Call U.IMPRIME(" ",4900H)																						! Avanza Papel
         Call U.CORTACR																												! Guillotina Papel
         TS.TEMP2I4 = -1
     EndIf
  Next GK%																																		! Fin Entes Capturados
End Sub 																																			!

Sub Ticket.Controlados
Integer*4 XI%, XJ%, Xfnd%
String    XMSG$, X.Tmp$, XITM$, XENTE$, XQTY$
  TS.TEMP1I4 = 0
  For XI% = 1 TO Gr.Ict.Slend%                    														! Recorre todos los String
    H$ = READ.SL.STR$(XI%)                 																	  ! Toma el String
    If LEN(H$) > 5 Then                    			                             \! Si el String es OK
     If ASC(H$)  = 153 Then Begin            																	! Si es un User Data 
        Asc.Tmp.Apun% = 3																											! Asigna apuntador
        X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  														! Verifica el numero de proyecto
        If Val(X.TMP$) = Val(Gr.Ict.Proy$) Then Begin    											! Si articulo controlado
        	 TS.TEMP1$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  											! Codigo del Ente
        	 TS.TEMP2$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  											! Item Vendido
        	 TS.TEMP3$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  											! Dialogo 1
        	 TS.TEMP3$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  											! Dialogo 2
        	 TS.TEMP3$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  											! Dialogo 3
        	 TS.TEMP3$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  											! Cantidad Vendida
        	 TS.TEMP4$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  											! Signo Operacion
        	 Xfnd% = 0 																													! Init Busqueda
        	 For XJ% = 1 To TS.TEMP1I4																					! Busca Item vendido
        	   If Gr.Ict.Msg%(XJ%,0) = Val(TS.TEMP2$) Then Begin                ! Si lo encontro
        	   	  Xfnd% = XJ%																										! Sitio donde lo encontro
        	   	  XJ% = TS.TEMP1I4 + 10																					! Sale del ciclo
        	   EndIf																														! Fin encontrado
        	 Next XJ%																														! Fin busqueda
        	 If Xfnd% = 0 Then Begin																						! No lo Encontro
        	    TS.TEMP1I4 = TS.TEMP1I4 + 1                                     ! Carga vector
        	    Gr.Ict.Msg%(TS.TEMP1I4,0)= Val(TS.TEMP2$)												! Item Articulo
              Gr.Ict.Msg%(TS.TEMP1I4,1)= Val(TS.TEMP1$)												! Codigo del Ente
              If TS.TEMP4$ = "00" Then Begin 																	! Si venta
                Gr.Ict.Msg%(TS.TEMP1I4,2)= Val(TS.TEMP3$)											! Qty Vendida
              EndIf Else Begin 																								! Si anulacion
              	Gr.Ict.Msg%(TS.TEMP1I4,2)= Val(TS.TEMP3$) * -1								! Qty Vendida
              EndIf																														!
        	 EndIf Else Begin																										! Lo Encontro
              If TS.TEMP4$ = "00" Then Begin 																	! Si venta
               Gr.Ict.Msg%(Xfnd%,2)= Val(TS.TEMP3$)		+							         \! Qty Vendida
                                     Gr.Ict.Msg%(Xfnd%,2)                     !
              EndIf Else Begin 																								! Si anulacion
               Gr.Ict.Msg%(Xfnd%,2)= Gr.Ict.Msg%(Xfnd%,2) -                  \! Resta cantidad
                                     Val(TS.TEMP3$)									          ! Qty Vendida
              EndIf																														!
        	 EndIf																															! Fin grabacion acumulado
        EndIf																																	! Fin articulo controlado
     EndIf																																		! Fin User Data
   Next XI%																																		! Final barrido trx
   XI% = 0 																																		! Init Variable
   Call PROCESO.ENTES                                                         ! IMPRESION DE ENTES
   If TS.TEMP2I4 = -1 Then Begin 																							! Genera cabecera trx
     Call U.IMPRIME(TO.HEADERLINE1$,4100H)																		! Imprime cabecera tiquete
     Call U.IMPRIME(TO.HEADERLINE2$,4100H)																		!
   EndIf 																																			!
End Sub 

Sub GRITMCTL(XOPT%) Public																							    	! Captura tarjeta corona
Integer*4 Xopt%                                                               !

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                       ! Carga de parametros
  Gr.Ict.Ok%   = 0                                                            ! Proyecto Apagado
  Gr.Ict.Cap%  = 0 
  Gr.Ict.Dlg1% = 0 
  Gr.Ict.Dlg2% = 0 
  Gr.Ict.Dlg3% = 0
  Dim Gr.Ict.Lista$(20)
  Gr.Ict.Qtyente% = 0
  Dim Gr.Ict.Msg$(99,30)				
  Dim Gr.Ict.Msg%(200,2)																				              ! Articulos Vendidos
  TS.ER.RETURN = -1																													  ! Ctrl Errores                     
  OPEN "R::ASCNTRL" AS 94					  		  																	  ! Apertura archivo parametrizacion 
  If TS.ER.RETURN <> -1 Then BEGIN                                            !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA ARTICULOS CTRL")
  	 Exit Sub 
  ENDIF 
  IF END #94 THEN UE.FIN.ITMCTL         																	    ! Si es EOF                        
  While (1)															  																    ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																				! Lectura registro                 
        IF TS.TEMP1$ = "[ENTES CONTROL]" Then Begin		  	    		   					! ENTES DE CONTROL
         Read #94; TS.TEMP1$																									! Lectura registro                 
         Gr.Ict.Ok%   = Val(Mid$(TS.TEMP1$,30,2))      				    					  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						  ! Lectura registro                 
         Gr.Ict.Stm%  = Val(Mid$(TS.TEMP1$,30,02))         				            ! Sesion Temporal
         Read #94; TS.TEMP1$     																						  ! Lectura registro                 
         Gr.Ict.Proy$ = Mid$(TS.TEMP1$,30,08)          				            		! Numero de proyecto
         If Gr.Ict.Open% = 0 Then Begin 																		  ! Control apertura archivo items controlados
            TS.ER.RETURN = -1																									! Control de errores aplicacion 
         	  Open "R::TF:ITMCTL" KEYED RECL 12 As 58								   					! Apertura Articulos controlados
            If TS.ER.RETURN <> -1 Then BEGIN                                  ! Error APERTURA
               Call VISOR.AND.BORRAR("ERROR APERTURA ARTICULOS CONTROLADOS")  ! Msg de alerta
               Call VISOR.AND.BORRAR("PROYECTO CANCELADO")										!
               Gr.Ict.Ok% = 00                                                ! Cierra proyecto
            ENDIF Else Gr.Ict.Open% = -1                                      ! Apertura correcta
         Endif 
         GoTo UE.FIN.ITMCTL 																								  ! Sale del ciclo de carga          
       Endif                                                                  !
   Wend                                                                       !
   UE.FIN.ITMCTL:                                                             !
     Close 94																																  ! Cierra archivo
     TS.ER.RETURN = -1 
     OPEN "R::TF:ITMMSG" AS 94                    	  												! Apertura archivo formato mensajes
     IF TS.ER.RETURN = -1 THEN BEGIN 																					!
        K% = 0																																!
        IF END #94 THEN UE.ITM.MSGLINEA		       												  		! Si es EOF
        While (1)																															!
	        Read #94;A$																													! Lectura registro hasta eof()
	        If Left$(A$,1) = "<" Then Begin																			! Carga los mensajes 
		         I% = Match(">",A$,1)																				  		! que van a utilizar 
		         J% = Val(MID$(A$,2,I%-2))																				! c/u de los articulos 
		         K% = 1																														! controlados
	        Endif Else Begin																										!
		         Gr.Ict.Msg$ (J%,K%) = A$																					!
		         Gr.Ict.Msg$ (J%,0) = STR$(K%)																		!
		         K% = K% + 1																											!
	        EndIf																																!
        Wend																																	!         
        UE.ITM.MSGLINEA:
        Close 94        
     ENDIF ELSE BEGIN 
      Call VISOR.AND.BORRAR("ERROR CARGA MENSAJE ITEM CONTROLADOS")		        ! Msg Alerta
      Call U.Imprime("ERROR CARGA MSG ITMS CONTROL ",2100H)	    							! Rastro en SJ
      Gr.Ict.Ok%         = 00 																								! Proyecto desactivado
     ENDIF 
     
   If Gr.Ict.Ok% = -1 Then                                                   \! Proyecto Activo
      Call U.IMPRIME("MODULO ENTES CONTROL   ON 16/Jun/2011",6100H) Else     \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO ENTES CONTROL  OFF 16/Jun/2011",6100H)           ! Msg Proyecto Cargado
EndIf 

If Gr.Ict.Ok% <> -1 Then Exit Sub                                             ! Si proyecto apagado


!--- EAMTSU02.J86
If Xopt% = 2 Then Begin                                                      	! Inicializacion de trx
   If Gr.Ict.Tkt% = -1 Then Begin 
   	 Call Ticket.Controlados
   EndIf        
   Gr.Ict.Dlg1% = 0 
   Gr.Ict.Dlg2% = 0 
   Gr.Ict.Dlg3% = 0
   Gr.Ict.Tkt%  = 0
   Dim Gr.Ict.Lista$(20) 
   Gr.Ict.Qtyente% = 0
   Dim Gr.Ict.Msg%(200,2)
EndIf 

!--- EAMTSU08.J86
If Xopt% = 08 Then Begin                                                       	! Secuencias de tecleo
	
!	To.USEREXIT(20) = 0
!	To.USEREXIT(60) = 0

!	If TS.IO.KEYS(4) <> 89 Then Begin																							!
	
	 TS.TEMP1I1 = 0 																															!
   TS.TEMP1I1 = VTA.ITM.CTL(SL.IT.ITEMCODE$)																	  ! Verifica si producto es recarga
   If TS.TEMP1I1 = -1 Then Begin 																								! Articulo controlado
   	  Call Captura.Dlgo.ItmCtl                                                  ! Captura Dialogos Item Controlado
   EndIf
 
!  EndIf

!	To.USEREXIT(20) = -1
!	To.USEREXIT(60) = -1


EndIf 

!--- EAMTSU20.J86
If Xopt% = 20 Then Begin
  If (TS.LINETYPE = 18) And (TS.LINEDATA = 99) Then Begin
   If (TS.TOTALS(0,0,0) > 0) Then Begin
     Gr.Ict.Slend% = SL.END																										! Total de strings
   EndIf
  EndIf

 If MATCH(TS.SDESC$(59),TS.PRTBUF$,1) > 0 Then Begin			       							! Mensaje de Anulacion Total
    Gr.Ict.Slend% = 0 																												!
    Gr.Ict.Tkt%   = 0 																												!
 EndIf																																				!

 If MATCH("SUSPEND",TS.PRTBUF$,1) > 0 Then Begin 							    						! Si mensaje de Suspension Total
    Gr.Ict.Slend% = 0 																												!
    Gr.Ict.Tkt%   = 0 																												!
 EndIf																																				!
 
EndIf


!--- EAMTSU53.J86
If Xopt% = 53 Then Begin                                                       	! Recuperacion de trx
   If UnPack$(LEFT$(SL.STR.ENTRY$,1)) = "99" Then Begin                 	    	! Si DATA/ENTRY
    If UnPack$(MID$(SL.STR.ENTRY$,3,4)) = Gr.Ict.Proy$ Then Begin               ! Tarjeta corona capturada
    	 Gr.Ict.Cap% = -1 
    EndIf    
   EndIf 
EndIf 

!--- EAMTSU56.J86
If Xopt% = 56 Then Begin                                                       	! Suspender una trx
   Gr.Ict.Dlg1% = 0 
   Gr.Ict.Dlg2% = 0 
   Gr.Ict.Dlg3% = 0
   Gr.Ict.Tkt%  = 0
   Dim Gr.Ict.Lista$(20)
   Gr.Ict.Qtyente% = 0
   Dim Gr.Ict.Msg%(200,2)
EndIf 

End Sub 
