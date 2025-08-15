!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRPFIZER.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Dscto Escaloando Pfizer   *
!**************************************************
!Observaciones:
! Mod 10Dic2012
! Se retira el control de validacion de cliente al
! cierre de la transacción, modificacion solicitada
! por comfandi.
! Ajuste desarrollado por grupo retail - OVS
!--------------------------------------------------
! Mod 22Abr2013
! Se adiciona manejo de log de proceso de la aplicacion
! para revisar posibles problemas de actualizacion 
! o de comunicación del sistema.
! Ajuste desarrollado por Grupo Retail - OVS
!-----------------------------------------------
! Mod 14Sep2015
! Se coloca al codigo del afiliado de Pfizer el prefijo
! COM segun requerimiento suministrado por 
! Freddy Guerrero
! Ajuste desarrollado por Grupo Retail - OVS
!-----------------------------------------------

%ENVIRON T		                          																			! Ambiente de terminal

Integer*1 Global  Gr.Pfizer.Ok%, Gr.Pfizer.Stmp%, Gr.Pfizer.CtlPago%, Gr.Pfizer.Clte%
Integer*2 Global  Gr.Pfizer.Motor%
Integer*4 Global  Gr.Pfizer.Dscto%, Asc.Tmp.Apun%, Gr.Pfizer.Xcont%
String    Global  Gr.Pfizer.Cadena$, Gr.Pfizer.Gdsc$, Gr.Pfizer.Itm$(2), Gr.Lcl.Clte$, Gr.Pfizer.Cotiza$, \
                  Gr.Pfizer.Tm$

%INCLUDE EAMTSWKG.J86          																								! working storage
%INCLUDE EAMTOPTS.J86          																								! Terminal Options
%INCLUDE EAMTRANS.J86          																								! Variables de transancion
%INCLUDE EAMTSEVA.J86          																								! Variables Cliente Frecuente
%INCLUDE EAMITEMR.J86          																								! Variables EamItemr
%INCLUDE EAMTSXHC.J86          																								! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          																								! Rutinas de Assembler
%INCLUDE RECATSSU.011        																									! Rutinas Genericas Grupo Retail

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

Sub IRRFEC.SPLIT1 (RECORD$) External
  String    RECORD$    !* The record (including the key) read from file      *!
End Sub 


Sub PFIZER.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%

			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:LP" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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
			X.Len% = Len(X.BUFF$)						  				  								            ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						            ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					          ! Graba registro
			X.BUFF$ = "["+X.RTA$+"]"+"RTA:"+X.LLEGA$                                !
			X.Len% = Len(X.BUFF$)						  				  								            ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						            ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					          ! Graba registro      
			Close 56
End Sub 



Function Pfizer.Digito.Chequeo(Xplu$) Public
Integer*4 DATO(2), XTOTAL%, XH%, XT%, XJ%, XI%
String XDATO$, XDG$, Xplu$, Pfizer.Digito.Chequeo

DIM DATO(12,1)
DATO(01,0)=1:DATO(02,0)=3:DATO(03,0)=1:DATO(04,0)=3
DATO(05,0)=1:DATO(06,0)=3:DATO(07,0)=1:DATO(08,0)=3
DATO(09,0)=1:DATO(10,0)=3:DATO(11,0)=1:DATO(12,0)=3
XTOTAL% = 0
XDATO$ = Xplu$
TS.TEMP1I4 = 0

  For XI% = 1 to 12
      DATO(XI%,1) = VAL(MID$(XDATO$,XI%,1))
  Next XI%
  For XJ% = 1 to 12
      XTOTAL% = XTOTAL% + (DATO(XJ%,1)*DATO(XJ%,0))
  Next XJ%
  XH% = (10*INT(XTOTAL%/10))+10
  XT% = XH% - XTOTAL%
  If XT% > 9 Then XT% = 0
  Pfizer.Digito.Chequeo = Str$(XT%)

End Function 

Sub Find.Items.Pfizer
  Integer*4 PRIC%, TOT4%, XI%, XInd2%, X.J%, Xqty%, Xfind%				  !
  Integer*2 Xsgn%, Xpos%
  String    XTMP$, Xitm$, XPric$, Xdummy$, Xkey$, Xbasura$, X.Ean$, Xcot$   !
  
  TS.ER.RETURN = -1
  Open "R::$F:ITMPFZ" KEYED RECL 13 AS Gr.Pfizer.Stmp% UNLOCKED NOWRITE NODEL ! Apertura Articulos controlados
  If TS.ER.RETURN <> -1 Then BEGIN 																					! Error Apertura
  	 Call U.Imprime("Error Validacion Pfizer",2100H)                        !
     TS.TEMP1I1 = 0																													!
     Gr.Pfizer.Dscto% = 0																										! 
  	 Exit Sub  																															!
  EndIf																																			!
  Gr.Pfizer.Xcont% = 0 																											! Numero de Items
  Dim Gr.Pfizer.Itm$(20,2)					 																			  ! Lista de Items para Pfizer
  FOR X.J% = 1 TO SL.END                    																! FOR ALL StringS
    H$ = READ.SL.STR$(X.J%)                 																! GET String
    If LEN(H$) > 5 Then  Begin                                              ! ASSURE GOOD String
     If ASC(H$)  = 1 Then Begin             																! ITEM ENTRY String
     	  Asc.Tmp.Apun% = 3
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
        Xkey$ = Pack$( Right$("000000000000"+Xitm$,12) )                    ! Llave de busqueda
        TS.ER.RETURN = -1																										! Control Errores
        Xfind% = 0																													! Encontrado
        Xpos% = 0 
        Read Form "C6 C7"; #Gr.Pfizer.Stmp% KEY XKey$; Xdummy$, X.Ean$      !
        If TS.ER.RETURN = -1 Then BEGIN 																		! Encontrado en lista Pfizer
        	 Xpos% = 0																												! Ctrl busqueta Item 
        	 For XI% = 1 To Gr.Pfizer.Xcont%																	! Busca Item
        	     If Val(Unpack$(X.Ean$)) = Val(Gr.Pfizer.Itm$(Xi%,0)) Then   \! Lo encontro	
        	        Xpos% = Xi%																								! Asigna posicion
        	 Next XI%																													!
        	 If Xpos% = 0 Then Begin                                          ! Nuevo Item
        	    Gr.Pfizer.Xcont% = Gr.Pfizer.Xcont% + 1												! Posicion Almacenar
        	    Gr.Pfizer.Itm$(Gr.Pfizer.Xcont%,0) = Str$(Val(Unpack$(X.Ean$)))! Ean del Producto
        	    Gr.Pfizer.Itm$(Gr.Pfizer.Xcont%,1) = Str$( Xsgn% * 1 )        ! Cantidad Vendida
        	    Xpos% = Gr.Pfizer.Xcont%                                      ! Asigna posicion de vector
        	 EndIf Else Begin																									! Item Encontrado
        	 	  Gr.Pfizer.Itm$(Xpos%,1) = Str$(Val(Gr.Pfizer.Itm$(Xpos%,1)) + \!
        	 	                                 (Xsgn% * 1))                   ! Cantidad Vendida
        	 EndIf
        EndIf	                  																				    ! Encontrado en Pfizer
     EndIf                                  																! ITEM ENTRY String
     Asc.Tmp.Apun% = 3																										  !     
     If ASC(H$)  = 2 Then Begin             																! ITEM ENTRY String Extention
     	 If Xpos% <> 0 Then Begin 																						! Extention Item Pfizer
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! MPGROUP       
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! DEALQUAN      
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! METODO PRECIO 
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! PRECIO        
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! CANTIDAD      
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! CANTIDAD      
        Xqty% = Val(Xdummy$)																								!
        Xqty% = Xqty% * Xsgn%									      												! Movimiento
        Gr.Pfizer.Itm$(Xpos%,1) = Str$(Xqty% + Val(Gr.Pfizer.Itm$(Xpos%,1)) - (Xsgn% * 1))! Cantidad Vendida
       EndIf 																																!
     EndIf																																	! ITEM ENTRY String Extention
    EndIf
  NEXT X.J%   
  Close Gr.Pfizer.Stmp%																											! Cierre validacion 

End Sub 

Sub APLICA.DSCTO.PFIZER
  Integer*4 PRIC%, TOT4%, XI%, XInd2%, X.J%, Xqty%, Xfind%				  !
  Integer*2 Xsgn%, Xpos%
  String    XTMP$, Xitm$, XPric$, Xdummy$, Xkey$, Xbasura$, X.Ean$, Xcot$,  \!
            XLitm$, Xplu$
  XI% = 0 
  If Gr.Pfizer.Xcont% > 0 Then Begin 																				! Productos Pfizer en venta
  	Proc.Pfizer:
  	 Xtmp$ = Right$("            "+Str$(Val(Gr.Lcl.Clte$)),12)              ! Cliente Lealtad Add prefijo COM 14sep2015
     For XI% = 1 To Gr.Pfizer.Xcont%																				! Barre Lista Items 
         Xtmp$ = Xtmp$ + Right$("0000000000000"+Gr.Pfizer.Itm$(XI%,0),13) + \! Lista Items a Validar
                 Right$("0000"+Gr.Pfizer.Itm$(XI%,1),4)                     !
     Next XI%                                                               ! Fin lista de Items
     
     XI% = 0 
     For Xi% = (Gr.Pfizer.Xcont% + 1) To 11
         Xtmp$ = Xtmp$ + String$(17," ")
     Next XI%
     
     TS.TEMP2$ = Armar.Trama.Msg("15","26",Xtmp$,"00","0001","123456")      ! Armar trama MSG
     TS.TEMP4$ = DATE$ +":"+ Time$
     Xdummy$  = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)  	! Ejecuta Requerimiento          	
     TS.TEMP5$ = DATE$ +":"+ Time$
     TS.TEMP1$ = Valida.Rta(Xdummy$)																				! Valida rta entregada 
     
     Call PFIZER.AUDITORIA(TS.TEMP2$,TS.TEMP1$, TS.TEMP4$, TS.TEMP5$)       ! Log de auditoria Pfizer     
     
     
     If TS.TEMP1$ <> "00" Then Begin   																			! Rta No Exitosa
     	  Call VISOR.AND.BORRAR("ERROR EN VALIDACION ITEMS PFIZER")           !
        TS.TEMP1$ = ASIC.DATOS$("REINTENTA PROCESO?","1.SI 9.NO")           ! Valida si reintenta proceso
        If Val(TS.TEMP1$) = 1 Then BEGIN																	  ! Si reintenta proceso
        	 XI% = 0                                                          ! Init Proceso
        	 Call VISORES4690(1,"VALIDANDO DESCUENTOS","PFIZER, ESPERE..",0,"L")
         	 GoTo Proc.Pfizer                                                 ! Repite invocacion
        EndIf                                                               !
        TS.TEMP1I1 = 0 													  												  ! No retorna
        Gr.Pfizer.Dscto% = 0    																				    ! Descuentos para aplicar
        Exit Sub 																														! Sale del proceso
     EndIf Else Begin																												! Valida Rta Proceso
       TS.TEMP1$ = MID$(Xdummy$,12,2)                                    	  ! Toma status de la Operacion
       If TS.TEMP1$ <> "00" Then Begin					 														! Si presenta un error, verif. local
          TS.TEMP1$ = MID$(Xdummy$,14,40)                                		! Toma mensaje del Error 
          Call VISOR.And.BORRAR(Left$(TS.TEMP1$,20)+Right$(TS.TEMP1$,20)) 	! Msg de Error
          TS.TEMP1I1 = 0													  												! No retorna
          Gr.Pfizer.Dscto% = 0																							! Descuentos para aplicar
          Exit Sub 																													! Sale del proceso
       EndIf																																! 
       Xcot$ = Mid$(Xdummy$,14,19)                                          ! Numero Cotizacion
       Xcot$ = Str$(Val(Xcot$))                                             ! Retira basura
       Gr.Pfizer.Cotiza$ = Xcot$                                            ! Almacena numero cotizacion
       XI% = 0 
       XLitm$ = ""
       For XI% = 33 To Len(Xdummy$) Step 21                                 ! Lista de Items Descontados
           Xitm$ = Mid$(Xdummy$,XI%,13)                                     ! Item descontado
           XPric$ = Mid$(Xdummy$,XI%+13,8)                                  ! Dscto Aplicado 
           If Xpric$ = "       0" Then Xpric$ = "00"                        ! Si no hay dscto
           If Val(Xitm$) > 0 Then Begin                                     ! Item con dscto 
              TS.TEMP1$  = PACK$(Right$("0000"+TS.STORE$,4))        +      \! Numero de tienda
                ":"+PACK$(Gr.Lcl.Clte$)                             +	     \! Cliente
                ":"+PACK$(Xitm$)                                    +	     \! Item vendido
                ":"+PACK$(Xpric$)                                   +      \! Dscto Obtenido
                ":"+Pack$(Xcot$)                                    +      \! Numero de cotización
                ":"+Pack$("00")                                             ! Relleno registro
              Call Grabacion.Cadena.Usuario2("23082012",TS.TEMP1$)          ! Almacena user data              
              Gr.Pfizer.Dscto% = Gr.Pfizer.Dscto% + Val(Xpric$)   					! Descuentos para aplicar
           EndIf
       Next Xi%
       TS.TEMP1I1 = -1																											! No retorna
     EndIf																																	! Fin Valida Rta Proceso
  EndIf Else Begin 																													! No hay Items de Pfizer
    TS.TEMP1I1 = 0																													! No retorna
    Gr.Pfizer.Dscto% = 0																										! Descuentos para aplicar
  EndIf																																			! Fin Productos Pfizer Venta
End Sub 

Sub Detalle.Pfizer																												!
Integer*4 PRIC%, TOT4%, X.I%																							!
String    X.TMP$, Xplu$, Xpric$, XLF$, Xbuff$															!
Integer*1 Xcab%																														!
Xcab% = 0																																  ! Impresion Cabecera
XLF$ = CHR$(0DH)                                                             ! Salto de linea
Xbuff$ = ""																																!
For X.I% = 1 TO SL.END                    																! FOR ALL StringS
    H$ = READ.SL.STR$(X.I%)                 															! GET String
    If LEN(H$) > 5 Then                                                  \! ASSURE GOOD String
     If ASC(H$)  = 153 Then Begin             														! ITEM ENTRY String
        Asc.Tmp.Apun% = 3																									! Asigna apuntador
        X.TMP$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	 												  ! Verifica el numero de proyecto
        If X.TMP$ = "23082012" Then Begin                               	! Si Dscto Pfizer
        	 If Xcab% = 0 Then Begin																				!
        	 	 Xbuff$ = "********  ITEMS DSCTO PFIZER ********" + XLF$      !
        	 	 Xcab% = -1																										! 
        	 EndIf																													!
        	 X.TMP$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	 										  ! Numero de tienda
        	 X.TMP$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	 										  ! Numero de cliente
        	 XPlu$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)	 										  ! Item Pfizer
        	 XPric$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	 										  ! Dscto otorgado
           TS.ER.RETURN = -1																							! Ctrl errores
           Xplu$ = Left$(Xplu$,12)																				! Ajusta dato busqueda
           TS.TEMP2$ = Pack$(Right$("000000000000"+Xplu$,12))             ! Arma llave busqueda eamitemr
           Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)           			! Lectura Datos Itemr
           Call IRRFEC.SPLIT1 ( TS.TEMP1$ ) 			                   		  ! Entrega datos al eamitemr.j86
           If TS.ER.RETURN <> -1 Then IR.ITEMNAME$ = "ITEM NO ENCONTRADO" ! Descripcion del Item
       	   Call Format.Amount(Val(XPric$))                                ! Formatea Valor Dscto
           TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)                  ! Ajusta dato
           Xbuff$ = Xbuff$ + IR.ITEMNAME$+" Dcto:$"+TS.TEMP1$ + XLF$      ! Almacena lista en SJ        	
     	  EndIf
     EndIf                                 																! ITEM ENTRY String
NEXT X.I% 
If Xcab% = -1 Then Begin                                                  ! Hay Items de Impresion
   Xbuff$ = Xbuff$ + "*************************************" + XLF$
   Write #34; Xbuff$
   Write #36; Xbuff$
EndIf
  
End Sub 

Sub GRPFIZER(XOPT%) Public																							   	! Dscto Escalonado Pfizer
Integer*4 Xopt%

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                       ! Carga de parametros
  Gr.Pfizer.Ok%   = 0                                                         ! Proyecto Apagado
  Gr.Pfizer.Clte% = 0
  Gr.Pfizer.Tm$  = ""
  Gr.Pfizer.CtlPago% = 0                                                      !
  TS.ER.RETURN = -1																													  ! Ctrl Errores                     
  OPEN "R::ASCNTRL" AS 94					  		  																	  ! Apertura archivo parametrizacion 
  If TS.ER.RETURN <> -1 Then BEGIN                                            !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA ARCHIVO CONTROL PFIZER")
  	 Exit Sub 
  ENDIF 
  IF END #94 THEN UE.FIN.GRPFIZER        																	    ! Si es EOF                        
  While (1)															  																    ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																				! Lectura registro                 
        IF TS.TEMP1$ = "[DSCTO PFIZER]" Then Begin		   		   					      ! Bonos Premio 
         Read #94; TS.TEMP1$																									! Lectura registro                 
         Gr.Pfizer.Ok%   = Val(Mid$(TS.TEMP1$,30,2))      			    				  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																									! Lectura registro                 
         Gr.Pfizer.Cadena$  = Mid$(TS.TEMP1$,30,3)         			    				  ! Codigo Cadena CCC
         Read #94; TS.TEMP1$																									! Lectura registro                 
         Gr.Pfizer.Gdsc$ = (Mid$(TS.TEMP1$,30,2))         			    				  ! Grupo de descuento
         Read #94; TS.TEMP1$																									! Lectura registro                 
         Gr.Pfizer.Motor% = Val(Mid$(TS.TEMP1$,30,3))      			    				  ! Tecla Motora
         Read #94; TS.TEMP1$																									! Lectura registro                 
         Gr.Pfizer.Stmp%  = Val(Mid$(TS.TEMP1$,30,2))      			    				  ! Sesion Temporal
         GoTo UE.FIN.GRPFIZER																							    ! Sale del ciclo de carga          
       Endif                                                                  !
   Wend                                                                       !
   UE.FIN.GRPFIZER:                                                           !
     Close 94																																  ! Cierra archivo
   If Gr.Pfizer.Ok% = -1 Then                                                \! Proyecto Activo
      Call U.IMPRIME("MODULO DSCTO PFIZER    ON 22/Abr/2013",2100H) Else     \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO DSCTO PFIZER   OFF 22/Abr/2013",2100H)           ! Msg Proyecto Cargado
EndIf 																																				! Final Eamtsu07

If Gr.Pfizer.Ok% <> -1 Then Exit Sub                                          ! Si proyecto apagado

!--- EAMTSU02.J86
If Xopt% = 2 Then Begin                                                       ! Inicializacion de trx
  Gr.Pfizer.Dscto% = 0                                                        ! Total dscto pfizer
  Gr.Pfizer.CtlPago% = 0																											! Ctrl Aplicacion dscto
  Gr.Pfizer.Clte% = 0 																												! Ctrl captura de cliente
  Gr.Pfizer.Cotiza$ = ""                                                      ! Numero de cotizacion
  Gr.Pfizer.Tm$  = ""                                                         ! Temporal
EndIf 																																				! Final Eamtsu02


!--- EAMTSU08.J86
If Xopt% = 8 Then Begin                                                       						 ! Validacion Venta Items
 If (Gr.Pfizer.CtlPago% = -2) Then Begin   																								 ! Si dscto Aplicado Pfizer
 	   Call VISOR.AND.BORRAR("EXISTE DSCTO PFIZER NO PERMITE VENTA")                         ! Msg al operador
 	   IR.INDICAT0 = IR.INDICAT0 OR 04H			                                                 ! No permite venta Item
 EndIf
EndIf 																																				             ! Final Eamtsu02


!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                                   ! Secuencias de tecleo
 
 If (Gr.Pfizer.CtlPago% = 0 And USER.FBACT.READ <> 0 ) Then \				  											 ! No hay dscto aplicado
  If TS.IO.KEYS(7) > 90 AND TS.IO.KEYS(7) < 97 Then Begin                                  ! Si forma de pago 
    Gr.Pfizer.Xcont% = 0																																	 ! Reset contador
    Call Find.Items.Pfizer																																 ! Analiza lista de Items Pfizer
    If Gr.Pfizer.Xcont% > 0 Then Begin 																										 ! Si hay Items Pfizer
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																				     !
     	 TS.IO.MOTORKEY = Gr.Pfizer.Motor%																									 ! Aplica dscto Pfizer
    	 Gr.Pfizer.CtlPago% = -1
    EndIf
  EndIf

 !--- Lineas comentareadas 10 dic 2012
 !If Gr.Pfizer.Clte% = 0 Then \                                                             ! En proceso de cliente
 ! If (TS.IO.MOTORKEY = Gr.Pfizer.Motor%) And (USER.FBACT.READ = 0) Then Begin              ! Si tecla Pfizer y no cliente
 !   Call VISOR.AND.BORRAR("NO SE HA CAPTURADO  DATO DE CLIENTE")                           !
 !   TS.TEMP1$ = ASIC.DATOS$("DESEA INGRESAR CLIENTE?","1.SI 9.NO")                         ! Valida si reintenta proceso
 !   If TS.TEMP1$ = "1" Then BEGIN 																												 ! Captura cliente
 !      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																				     ! Init Vectores
 !      TS.IO.MOTORKEY = 73																														     ! Secuencia captura de cliente
 !      Gr.Pfizer.CtlPago% = 0                                                              !
 !      Exit Sub 
 !   EndIf 
 !   Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																				       !
 !   TS.IO.MOTORKEY = 73																														         !
 !   Gr.Pfizer.CtlPago% = 0                                                                 !
 !   If TS.TEMP1$ = "9" Then BEGIN 																												 ! No desea cliente
 !      Gr.Pfizer.Clte% = -1																																 ! 
 !	  EndIf 
 ! EndIf

 If USER.FBACT.READ <> 0 Then \
 If (TS.IO.MOTORKEY = Gr.Pfizer.Motor%) And (Gr.Pfizer.CtlPago% = -1)  Then Begin          ! Dscto Pfizer
 	      Call VISORES4690(1,"VALIDANDO DESCUENTOS","PFIZER, ESPERE..",0,"L")
 	      TS.TEMP1I1 = 0                                                                     ! Ctrl Aplicacion Dscto
 	      Call APLICA.DSCTO.PFIZER																													 ! Valida contra WSDL de Pfizer
 	      If TS.TEMP1I1 <> -1 Then Begin 																										 !
 	      	 Call VISOR.AND.BORRAR("NO HAY ITEMS PFIZER  PARA LA COMPRA")                    !
           Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																				 !
           TS.IO.MOTORKEY = 73																														 !
 	      	 Exit Sub 																								                       ! No hay dscto aplicado
 	      EndIf
        If Gr.Pfizer.Dscto% <= 0 Then Begin																								 ! Si no entrego Dsctos
           Call VISOR.AND.BORRAR("NO HAY DSCTO PFIZER  PARA LA COMPRA")                    !
           Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																				 !
           TS.IO.MOTORKEY = 73																														 !
           Gr.Pfizer.CtlPago% = -2                                                         ! Dscto en aplicacion 
 	      	 Exit Sub 																								                       ! No hay dscto aplicado
        EndIf
 	      SL.END = SL.END + 1                                                                ! Aumenta apuntador de String 
	      SL.STR$(SL.END) = Pack$("03")+":"+Pack$(Gr.Pfizer.Gdsc$) + ":" +	\                ! Almacena String de descuento
			    	              Pack$("00")+ ":" + Pack$(Str$(Gr.Pfizer.Dscto%))+":"+Pack$("00") + ":" ! 
        TS.BALDUE(0)      = TS.BALDUE(0) - Gr.Pfizer.Dscto%                                ! Actualiza variables de la 
        SL.HD.GROSSNEG    = SL.HD.GROSSNEG + Gr.Pfizer.Dscto%                              ! Aplicacion y de total de la 
        TS.GROSSNEG       = TS.GROSSNEG + Gr.Pfizer.Dscto%                                 ! Transaccion eliminando el descuento
        TS.TOTALS(0,0,1)  = TS.TOTALS(0,0,1) + Gr.Pfizer.Dscto%                            !
        TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) + Gr.Pfizer.Dscto%                           !
        TS.TOTALS(0,0,0)  = TS.TOTALS(0,0,0) - Gr.Pfizer.Dscto%                            !
        Call Format.Amount(Gr.Pfizer.Dscto% * -1)                                          !
        TS.TEMP1$ = "DESCUENTO PFIZER $"+TS.TEMP1$                                         !
        TS.TEMP1$ = Right$(String$(37," ")+TS.TEMP1$,37)                                   !
        Call U.IMPRIME(TS.TEMP1$,4101H)                                                    ! Presenta informacion
        Call U.IMPRIME(TS.TEMP1$,2100H)                                                    ! Presenta informacion
        Call Format.Amount(Gr.Pfizer.Dscto%)                                               !
        Call VISOR.AND.BORRAR("DESCUENTO PFIZER DE $"+TS.TEMP1$)                           !
        Gr.Pfizer.CtlPago% = -2                                                            ! Dscto en aplicacion 
        Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
        TS.IO.MOTORKEY = 0 
        TS.GUIDANCE = 1020																																 ! Solicite totalizar Trx 
 EndIf
   
EndIf 																																				! Final Eamtsu14
  
!--- EAMTSU20.J86
If Xopt% = 20 Then Begin                                                      ! Secuencias de tecleo
 If MATCH(TS.SDESC$(59),TS.PRTBUF$,1) > 0 THEN BEGIN												  ! Si anulacion total de la trx
 	If Gr.Pfizer.CtlPago% = -2 Then Begin                                       ! Dscto aplicado
    Call VISORES4690(1,"ANULANDO    COMPRA","PFIZER, ESPERE",0,"L")
    TS.TEMP1$ = Right$("            "+Str$(Val(Gr.Lcl.Clte$)),12)       +    \! Identificacion del cliente
 	              Right$("                   "+Gr.Pfizer.Cotiza$,19)            ! Numero de cotizacion    
   If Val(Gr.Pfizer.Cotiza$) <> 0 Then Begin 
    TS.TEMP2$ = Armar.Trama.Msg("15","28",TS.TEMP1$,"00","0001","123456")     ! Armar trama MSG     
    TS.TEMP4$ = DATE$ +":"+ Time$           	
    TS.TEMP1$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)  	  ! Ejecuta Requerimiento          	
    TS.TEMP5$ = DATE$ +":"+ Time$           	
    Call PFIZER.AUDITORIA(TS.TEMP2$,TS.TEMP1$, TS.TEMP4$, TS.TEMP5$)          ! Log de auditoria Pfizer     
   EndIf  
  EndIf 
  Gr.Pfizer.Dscto% = 0                                                        ! Total dscto pfizer
  Gr.Pfizer.CtlPago% = 0																										  ! Ctrl Aplicacion dscto
  Gr.Pfizer.Cotiza$ = ""                                                      ! Numero de cotizacion
 EndIf																																				!

If TS.LINEDATA = 1 And TS.LINETYPE = 29 And TS.PROCEDURE < 0 Then Begin       ! Si cierre de trx
 If Gr.Pfizer.CtlPago% = -2 Then Begin                                        ! Dscto aplicado
 	  Call Detalle.Pfizer                                                       ! Detalle impresion pfizer
    Call VISORES4690(1,"CONFIRMANDO COMPRA","PFIZER, ESPERE",0,"L")           !
    If Val(Gr.Pfizer.Cotiza$) <> 0 Then Begin                                 ! 
 	   TS.TEMP1$ = Right$("            "+Str$(Val(Gr.Lcl.Clte$)),12)       +   \! Identificacion del cliente
 	               Right$("                   "+Gr.Pfizer.Cotiza$,19)           ! Numero de cotizacion
     TS.TEMP2$ = Armar.Trama.Msg("15","27",TS.TEMP1$,"00","0001","123456")    ! Armar trama MSG                	
     TS.TEMP4$ = DATE$ +":"+ Time$           	
     TS.TEMP1$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)  	  ! Ejecuta Requerimiento          	
     TS.TEMP5$ = DATE$ +":"+ Time$           	
     Call PFIZER.AUDITORIA(TS.TEMP2$,TS.TEMP1$, TS.TEMP4$, TS.TEMP5$)         ! Log de auditoria Pfizer     
    EndIf																																			!
     Gr.Pfizer.CtlPago% = 0                                                   !
 EndIf																																				! Fin confirmacion
EndIf

EndIf																									    										! Final Eamtsu20

!-- EAMTSU53.J86
If Xopt% = 53 Then Begin                                                                   ! Recupearacion de trx
 IF Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "03" THEN BEGIN   	    	                           ! Si DESCUENTO
    Asc.Tmp.Apun% = 3																													             ! Apuntador String
    TS.TEMP1$      = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	   	 	 		    					   ! Grupo de descuento
    If Val(TS.TEMP1$) = Val(Gr.Pfizer.Gdsc$) Then Begin                       						 ! Si es dscto Pfizer
        TS.TEMP1$         = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	   	 	 		           ! Ptg de descuento
        TS.TEMP1$         = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	   	 	 		           ! Monto del descuento
        Gr.Pfizer.Dscto%  = Val(TS.TEMP1$)																								 !
        TS.BALDUE(0)      = TS.BALDUE(0) - Gr.Pfizer.Dscto%                                ! Actualiza variables de la 
        SL.HD.GROSSNEG    = SL.HD.GROSSNEG + Gr.Pfizer.Dscto%                              ! Aplicacion y de total de la 
        TS.GROSSNEG       = TS.GROSSNEG + Gr.Pfizer.Dscto%                                 ! Transaccion eliminando el descuento
        TS.TOTALS(0,0,1)  = TS.TOTALS(0,0,1) + Gr.Pfizer.Dscto%                            !
        TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) + Gr.Pfizer.Dscto%                           !
        TS.TOTALS(0,0,0)  = TS.TOTALS(0,0,0) - Gr.Pfizer.Dscto%                            !
        Call Format.Amount(Gr.Pfizer.Dscto% * -1)                                          !
        TS.TEMP1$ = "DESCUENTO PFIZER $"+TS.TEMP1$                                         !
        TS.TEMP1$ = Right$(String$(37," ")+TS.TEMP1$,37)                                   !
        Call U.IMPRIME(TS.TEMP1$,4101H)                                                    ! Presenta informacion
        Call U.IMPRIME(TS.TEMP1$,2100H)                                                    ! Presenta informacion
        Call Format.Amount(Gr.Pfizer.Dscto%)                                               !
        Gr.Pfizer.CtlPago% = -2                                                            ! Dscto en aplicacion 
        Gr.Pfizer.Tm$ = ""
    EndIf 
 ENDIF 
 
 IF Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "99" THEN BEGIN   	    	                           ! Si User Data
    Asc.Tmp.Apun% = 3																													             ! Apuntador String
    TS.TEMP1$      = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	   	 	 		    					   ! Codigo del proyecto
    If TS.TEMP1$ = "23082012" Then Begin                       						                 ! Si es dscto Pfizer
    	 TS.TEMP1$      = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	   	 	 		    				   ! Codigo de tienda
    	 TS.TEMP1$      = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	   	 	 		    				   ! Cliente
    	 TS.TEMP3$      = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	   	 	 		    				   ! Item
    	 TS.TEMP4$      = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	   	 	 		    				   ! Dscto
       TS.ER.RETURN = -1                                                                   ! Control errores
       TS.TEMP3$ = Left$(TS.TEMP3$,12)                                                     ! Toma 12 caracteres Item
       TS.TEMP3$ = Pack$(Right$("000000000000"+TS.TEMP3$,12))                              ! Llave de busqueda
       Gr.Pfizer.Dscto% = Gr.Pfizer.Dscto% + Val(TS.TEMP4$)   			                       ! Descuentos para aplicar
    EndIf 
 EndIf
EndIf																																					             ! Final Eamtsu53

!-- EAMTSU56.J86
If Xopt% = 56 Then Begin                                                      ! Suspension de trx
   Gr.Pfizer.Dscto% = 0                                                       ! Total dscto pfizer
   Gr.Pfizer.CtlPago% = 0																											! Ctrl Aplicacion dscto
   Gr.Pfizer.Cotiza$ = ""                                                     ! Numero de cotizacion
   Gr.Pfizer.Tm$  = ""                                                        ! Temporal
EndIf 																																				! Final Eamtsu56

!-- EAMTSU60.J86
If Xopt% = 60 Then Begin                                                      ! Secuencias de tecleo

EndIf																																					! Final Eamtsu60


End Sub 
