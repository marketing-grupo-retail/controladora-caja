!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMDEVOL.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Control Devoluciones Mercancia   *
!**************************************************
!Observaciones:
!
!

%ENVIRON T		                          																			! Ambiente de terminal

Integer*1 Global Gr.Anmc.Ok%, Gr.Anmc.Cap%, Gr.Anmc.Trx%, Gr.Anmc.Ses%   			! Control del proyecto
Integer*1 Global Gr.Anmc.CtlAnul%
String    Global Gr.Anmc.Proy$, Gr.Anmc.Itm$(2), Gr.Anmc.Ter$, Gr.Anmc.Trx$, Gr.Anmc.Time$
Integer*4 Global Gr.Anmc.Itm%, IR.PRICE1, IR.PRICE2
                 
%INCLUDE ADX_UPGM:EAMTSWKG.J86          																			! working storage
%INCLUDE ADX_UPGM:EAMTRANS.J86          																			! working storage
%INCLUDE ADX_UPGM:EAMTOPTS.J86          																			! working storage
%INCLUDE ADX_UPGM:EAMITEMR.J86          																			! Item Record 
%INCLUDE ADX_UPGM:NTAMTSSU.011         																				! Subrutinas Grupo Retail 

SUB TSCSEC03 EXTERNAL          ! stack display guidance
End SUB

Function PRSTCRT(IONUM,PIPESIZE,PIPEID) EXTERNAL
 INTEGER*2 PRSCCRT
 INTEGER*2 IONUM,PIPESIZE
 STRING PIPEID
 END FUNCTION

Function PRSTINT EXTERNAL
 INTEGER*4 PRSCINT
End Function

Function PRSTWRT(PRSNUM,DEST,BUFFER) EXTERNAL
 Integer*4 PRSCWRC
 INTEGER*4 PRSNUM
 STRING    DEST, BUFFER
End Function

Function GRABA.PIPES(UE.PIPE.SESION%,UE.PIPE.FORM$,UE.PIPE.DATA$) Public
   String  UE.PIPE.FORM$, UE.PIPE.DATA$
   Integer UE.PIPE.SESION%
   Write Form UE.PIPE.FORM$;#UE.PIPE.SESION% AUTOUNLOCK;UE.PIPE.DATA$
End Function

Function LEER.PIPES(UE.PIPE.SESION%,UE.PIPE.FORM$) Public
   String  UE.PIPE.FORM$, LEER.PIPES,UE.PIPE.DATA$
   Integer UE.PIPE.SESION%
   TS.ER.RETURN = -1
   READ FORM UE.PIPE.FORM$;#UE.PIPE.SESION%;UE.PIPE.DATA$
   If TS.ER.RETURN <> -1 Then UE.PIPE.DATA$ = "0"
   LEER.PIPES = UE.PIPE.DATA$
End Function 
!--- Fin de la funcion de lectura de pipes


Sub Valida.Temp.Anul																													! validacion archivo temporal
String Xfile$, Xrta$, XA$, XB$																								!
Integer*4 XPRSNUM, RTA%
TS.TEMP4I1 = -1																																! Ctrl Errores
If Gr.Anmc.Trx% = 0 Then Begin   		  																				! Trx Cargada y lista para anulaciones
  TS.ER.RETURN = -1																														! Control de errores
  Dim Gr.Anmc.Itm$(100,3)																											! Hasta 100 Productos
  Gr.Anmc.Trx% = -1																														! Ctrl creacion temporal
  XA$ = Right$("0000"+Gr.Anmc.Ter$,4)																				  ! Terminal a buscar
  XB$ = Right$("0000"+Gr.Anmc.Trx$,4)																				  !	Trx a buscar
  XA$ = XA$ + XB$																														  ! Arma Trama busqueda
  XRTA$ = "0"																																	! Rta Items
  Rta% = 0 																																		!
  TS.ER.RETURN = -1  																													!
  Open "R::PI:N" As Gr.Anmc.Ses%																							!
  If TS.ER.RETURN <> -1 Then BEGIN 
     Call VISOR.and.borrar("ERROR APERTURA DEL PIPE")
     Gr.Anmc.Trx% = 0																													! Ctrl creacion temporal
     TS.TEMP4I1 = 0 
     Exit Sub 																													  		! Sale rutina
  ENDIF																																				!
  TS.ER.RETURN = -1																														! Ctrl de errores
  Call GRABA.PIPES(Gr.Anmc.Ses%,"C8",XA$)																			! Coloca req. al controlador
  If TS.ER.RETURN = -1 Then Begin																							! Si proceso OK
  	 XRTA$ = LEER.PIPES(Gr.Anmc.Ses%,"C8")																		! Toma Rta del PIPE
  	 Rta% = Val(Xrta$)
  EndIf Else Begin 																														! Error de grabacion Pipe
  	Gr.Anmc.Trx% = 0																													! Ctrl creacion temporal
  	TS.TEMP4I1 = 0 
  EndIf																																				!
  Close Gr.Anmc.Ses%																													! Cierre del Pipe
  If Rta% > 0 Then Begin																						          ! Trx con Items
    	 TS.ER.RETURN = -1																											! Ctrl Errores
    	 Open "R::C:\EAMDA"+Right$(TS.TERMINAL$,3) As Gr.Anmc.Ses%							! Apertura Items Vendidos
    	 If TS.ER.RETURN <> -1 Then Begin																				! Error de apertura 
  	      Gr.Anmc.Trx% = 0																										! Ctrl creacion temporal
  	      Exit Sub 																													  ! Sale rutina
    	 EndIf																																	!
  	   IF END #Gr.Anmc.Ses% THEN UE.FIN.ITMVTA  													    ! Si es EOF
       While (1)												  																    ! Recorre archivo
        Gr.Anmc.Itm% = Gr.Anmc.Itm% + 1																				! 
        Read #Gr.Anmc.Ses%; Gr.Anmc.Itm$(Gr.Anmc.Itm%,0), Gr.Anmc.Itm$(Gr.Anmc.Itm%,1), \! Carga items vendidos
                  Gr.Anmc.Itm$(Gr.Anmc.Itm%,2), Gr.Anmc.Itm$(Gr.Anmc.Itm%,3)  !
       Wend 																																	!
       UE.FIN.ITMVTA:																													! Fin del archivo
         Delete Gr.Anmc.Ses%																									! Borra archivo
    	   Gr.Anmc.Trx% = -1																										! Ctrl creacion temporal         
   EndIf Else Begin 
 	      Gr.Anmc.Trx% = 0																										! Ctrl creacion temporal
 	      TS.TEMP4I1 = 0 
 	      Exit Sub 																													  ! Sale rutina
   EndIf
EndIf																																					!
End Sub 																																			! Fin validacion temporal

Sub Find.Mov.Sale Public																				              ! Busca movimiento para anulacion
String Ycaj$, Ytrx$
String Yitm$, Yprc$, Yqty$, Ydsc$
Integer*4 XI%, Xpos%, Xpric%,XQty%
TS.TEMP4I1 = -1																																! Control de creacion temporal
Call Valida.Temp.Anul																													! Valida si trx cargada 
If TS.TEMP4I1 <> -1 Then Begin																							  ! Error en carga de trx
	 Call Visor.And.Borrar("ERROR EN VALIDACION PRODUCTO")											! Msg de alerta
	 Gr.Anmc.CtlAnul% = 0																															! Reporta Error en proceso
	 Exit Sub 																																	! Sale del proceos
EndIf																																					!
Call visores4690(1,"VALIDANDO ITEM",SL.IT.ITEMCODE$,1200,"l")
Yitm$ = SL.IT.ITEMCODE$             																					! Empaqueta Item a buscar
TS.ER.RETURN = 0 																															! Ctrl de lectura
For XI% = 1 To Gr.Anmc.Itm% 																									! Recorre vector de venta
  If Val(Gr.Anmc.Itm$(XI%,0)) = Val(Yitm$) Then Begin													! Si lo encontro
  	 TS.ER.RETURN = -1																												! Proceso OK
  	 Xpos% = XI%
  EndIf 																																			!
Next XI%																																			! Fin recorrido vector de venta
If TS.ER.RETURN <> -1 Then Begin 																							! Si Item no encontrado
	 Call Visor.And.Borrar("ARTICULO ANULAR NO  ENCONTRADO")										!
	 Gr.Anmc.CtlAnul% = 0																															! Reporta Error en proceso
	 Exit Sub
EndIf
If TS.IO.DATA$(6) = "" Then XQty% = 1 Else                                   \! Valida Qty capturada
    XQty% = VAL(TS.IO.DATA$(6))				                                        !
If XQty% > Val(Gr.Anmc.Itm$(Xpos%,2)) Then Begin															! Si supera Cantidad Vendida
	 Call Visor.And.Borrar("CANTIDAD ANULAR SUPERA LA VENDIDA")									!
	 Gr.Anmc.CtlAnul% = 0																												! Reporta Error en proceso
	 Exit Sub
EndIf
Xpric% = Val(Gr.Anmc.Itm$(Xpos%,1)) - Val(Gr.Anmc.Itm$(Xpos%,3))              ! Total - Dscto
Xpric% = Xpric% / Val(Gr.Anmc.Itm$(Xpos%,2))																	! Retorna precio unitario
Gr.Anmc.Itm$(Xpos%,2) = Str$(Val(Gr.Anmc.Itm$(Xpos%,2)) - Xqty%)              ! Resta cantidad anulada
IR.PRICE1 = Xpric%  																													! Retorna Precio con descuento
IR.PRICE2 = IR.PRICE1																													!

End Sub 

Sub Revision.Hora.Trx Public 																									! Control de tiempo de autorizacion 
String XTIME$, TSTRING$
Integer*4 XDIFF%
  XTIME$ = LEFT$(TIME$,4)																											! Toma hora del sistema
  Tstring$ = Mid$(TS.TEMP1$,13,4)																							! Toma hora de la trx
  XDIFF% =                        																					 \! Calculate time left for exec in min.
          ((Val(Left$(XTIME$,2))*60) +                                       \! Convert hours to minutes and
          Val(Right$(XTIME$,2)))    -                                        \! Add the minutes and subtract
          ((Val(Left$(TSTRING$,2))*60) +                                     \! Convert hours to minutes and
          Val(Right$(TSTRING$,2)))                                            ! Add the minutes
          
  If XDIFF% > VAL(Gr.Anmc.Time$) Then Begin 																  ! If tiempo sobrepasado al permitido
   TS.ER.RETURN = 0 																													! No autoriza proceso
 EndIf																																				!
End Sub 

Sub Anul.Itm.Tirilla Public 																									! Validacion Item Devuelto
String XCaj$, Xtrx$
If Gr.Anmc.Trx% = 0 Then Begin 																								! Trx Origen no cargada
  TS.TEMP1$ = ASIC.DATOS$("ESCANEE LA TIRILLA","ORIGEN DE VENTA")      				! Captura tirilla seguridad
  If TS.TEMP1$ = "E" Or Val(TS.TEMP1$) = 0 Then Begin                  				! Si tecla borrar o dato invalido
  	 Call VISOR.AND.BORRAR("PROCESO CANCELADO   POR EL USUARIO")				      ! Msg Alerta
     IR.INDICAT0 = IR.INDICAT0 OR 04H					         												! No permite venta
     Gr.Anmc.Cap% = 0																													!
     Exit Sub																									  					    ! Sale del Procedimiento
  EndIf 																																			!
  If Len(TS.TEMP1$) <> 18 Then Begin 																					! Error de longitud de dato
     Call VISOR.AND.BORRAR("ERROR EN LECTURA    TIRILLA ORIGEN VENTA")				! Msg Alerta
     IR.INDICAT0 = IR.INDICAT0 OR 04H					         												! No permite venta
     Gr.Anmc.Cap% = 0																													!
     Exit Sub																									  					    ! Sale del Procedimiento
  EndIf 																																			!
  If Mid$(TS.TEMP1$,9,4) <> Right$(Date$,4) Then Begin 												! Tirilla fuera de la fecha del sistema
     Call VISOR.AND.BORRAR("TIRILLA ESTA FUERA  FECHA DE REGISTRO")						! Msg Alerta
     IR.INDICAT0 = IR.INDICAT0 OR 04H					         												! No permite venta
     Gr.Anmc.Cap% = 0 																												!
     Exit Sub																									  					    ! Sale del Procedimiento
  EndIf
  TS.ER.RETURN = -1
  Call Revision.Hora.Trx																											! Revisa si esta en el tiempo permitido
  If TS.ER.RETURN <> -1 Then Begin 																						! Fuera del tiempo calculado
     Call VISOR.AND.BORRAR("MOVIMIENTO FUERA DEL TIEMPO PERMITIDO")						! Msg Alerta
     IR.INDICAT0 = IR.INDICAT0 OR 04H					         												! No permite venta
     Gr.Anmc.Cap% = 0 																												!
     Exit Sub																									  					    ! Sale del Procedimiento
  EndIf
  Gr.Anmc.Ter$ = Mid$(TS.TEMP1$,1,4)																					! Numero de la terminal
  Gr.Anmc.Trx$ = Mid$(TS.TEMP1$,5,4)																					! Numero de la transaccion
  Gr.Anmc.CtlAnul% = -1
  Call Find.Mov.Sale                                													! Busca Movimiento en log de trx
  If Gr.Anmc.CtlAnul% <> -1 Then Begin																							! No encontrado
     IR.INDICAT0 = IR.INDICAT0 OR 04H					         												! No permite venta
     Gr.Anmc.Cap% = 0 																												!
     Exit Sub																									  					    ! Sale del Procedimiento
  EndIf 
EndIf Else Begin 																															! Movimiento cargado para validacion
  Gr.Anmc.CtlAnul% = -1
  Call Find.Mov.Sale                                											    ! Busca Movimiento en log de trx
  If Gr.Anmc.CtlAnul% <> -1 Then Begin
     IR.INDICAT0 = IR.INDICAT0 OR 04H					         												! No permite venta
     Gr.Anmc.Cap% = 0 																												!
     Exit Sub																									  					    ! Sale del Procedimiento
  EndIf 
EndIf 
End Sub 


Sub ANULMCIA(XOPT%, Xaut%) Public																				    	! Control anulacion mercancia
Integer*4 Xopt%, Xaut%                                                        !

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                       ! Carga de parametros
  Gr.Anmc.Ok%  = 0                                                            ! Proyecto Apagado
  Gr.Anmc.Cap% = 0 																														!
  Gr.Anmc.Trx% = 0 																														! Transaccion Cargada
  Gr.Anmc.Ter$ = ""
  Gr.Anmc.Trx$ = ""
  Dim Gr.Anmc.Itm$(100,3)
  TS.ER.RETURN = -1																													  ! Ctrl Errores                     
  OPEN "R::ASCNTRL" AS 94					  		  																	  ! Apertura archivo parametrizacion 
  If TS.ER.RETURN <> -1 Then BEGIN                                            !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA CTRL ANULACIONES")
  	 Exit Sub 
  ENDIF 
  IF END #94 THEN UE.FIN.ANMCIA         																	    ! Si es EOF                        
  While (1)															  																    ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																				! Lectura registro                 
        IF TS.TEMP1$ = "[ANULACION MERCANCIA]" Then Begin		 		   		  			! ANULACIONES MERCANCIA
         Read #94; TS.TEMP1$																									! Lectura registro                 
         Gr.Anmc.Ok%   = Val(Mid$(TS.TEMP1$,30,2))     				    					  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						  ! Lectura registro                 
         Gr.Anmc.Proy$ = Mid$(TS.TEMP1$,30,08)             				            ! Codigo del proyecto a almacenar
         Read #94; TS.TEMP1$     																						  ! Lectura registro                 
         Gr.Anmc.Ses% = Val(Mid$(TS.TEMP1$,30,02))         				            ! Numero sesion temporal
         Read #94; TS.TEMP1$     																						  ! Lectura registro                 
         Gr.Anmc.Time$ = Mid$(TS.TEMP1$,30,02)         				                ! Minutos de espera 
         GoTo UE.FIN.ANMCIA 																								  ! Sale del ciclo de carga          
       Endif                                                                  !
   Wend                                                                       !
   UE.FIN.ANMCIA:                                                             !
     Close 94																																  ! Cierra archivo
   If Gr.Anmc.Ok% = -1 Then                                                  \! Proyecto Activo
      Call U.IMPRIME("MODULO ANUL MERCANCIA  ON 13/Abr/2011",6100H) Else     \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO ANUL MERCANCIA OFF 13/Abr/2011",6100H)           ! Msg Proyecto Cargado
EndIf 

If Gr.Anmc.Ok% <> -1 Then Exit Sub                                             ! Si proyecto apagado

!--- EAMTSU02.J86
If Xopt% = 2 Then Begin                                                       ! Inicializacion de trx
   Gr.Anmc.Cap% = 0 
   Gr.Anmc.Trx% = 0
   Dim Gr.Anmc.Itm$(100,3)
   Gr.Anmc.Ter$ = ""
   Gr.Anmc.Trx$ = ""
EndIf 																																				! Fin inicializacion de trx

!--- EAMTSU08.J86
If Xopt% =  8 Then Begin
	If Gr.Anmc.Cap% = -2 Then Begin
	   Call Anul.Itm.Tirilla																										! Analiza anulacion Item
	EndIf
EndIf

!--- EAMTSU20.J86
If Xopt% = 20 Then Begin 
   IF TS.LINETYPE < 4 AND 																									 \! Acceptable line type
      MID$(TS.PRTBUF$,37,1) = "-" THEN BEGIN 																	! with minus sign 
      If Gr.Anmc.Cap% = -2 Then Begin 																        ! Si en anul Item No Vendido
         TS.TEMP1$ = PACK$(TS.TEMP1$)       +                                \! Nro Terminal Origen
                     ":" + PACK$(TS.TEMP2$) +                                \! Nro Trx Origen
                     ":" + PACK$("00")                  											! Almacena String de Anulacion
         Call Grabacion.String.Usuario2(Gr.Anmc.Proy$,TS.TEMP1$)						  ! de articulo
         Gr.Anmc.Cap% = 0																											! Inicializa Variable
      EndIf 
   EndIf 
EndIf 																																				! Fin Control Proceso 

!--- EAMTSU23.J86
If Xopt% = 23 Then Begin 
   If Left$(TS.DISP1$,4) = "B067" Then Begin 									                ! Si anul itm no vendido
   	  Gr.Anmc.Cap% = -1	
   EndIf 
EndIf 																																				! Fin Control Proceso 

!--- EAMTSU56.J86
If Xopt% = 56 Then Begin 
   Gr.Anmc.Cap% = 0 
   Gr.Anmc.Trx% = 0
   Dim Gr.Anmc.Itm$(100,3)
   Gr.Anmc.Ter$ = ""
   Gr.Anmc.Trx$ = ""
EndIf 

!--- EAMTSU66.J86
If Xopt% = 66 Then Begin 
   If (Gr.Anmc.Cap% = -1 And XAUT% = -1)  Then Begin                          ! Si autorizacion proceso anulada
   	  Gr.Anmc.Cap% = -2																												! Si entro en dialogo
   EndIf Else Gr.Anmc.Cap% = 0
EndIf 																																				! Fin Control Proceso 


End Sub 
