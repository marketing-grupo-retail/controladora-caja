!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMDOMIC.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Domiciliarios             *
!**************************************************
! Modificaciones:
! ----- Version 1.0 10 Noviembre 2023 -------------
! Mod 09Jul2024
! Se adiciona para la captura de domiciiarios diferentes
! a rappi, la captura de un numero de pedido.
! Desarrollado por Grupo Retail Ltda - OVS
!--------------------------------------------------
! Mod 31Jul2024
! Se adicionan 2 parametros a domiciliarios reportando
! los codigos de tarjeta de credito institucional.
! Desarrollado por Grupo Retail Ltda - OVS
!--------------------------------------------------
! Mod 24Jul2025
! Se define una variable global donde se reportan 
! las tarjetas de crd institucional usadas por los 
! domiciliarios para evitar su uso en operaciones 
! fuera de domicilio.
! Desarrollado por Grupo Retail Ltda - OVS
!--------------------------------------------------

%Environ T																																	! Ambiente terminal

Integer*1 Global Gr.Domi.Ok%,                                              \! Proyecto Activo
                 Gr.Domi.Intrx%,                                           \! Proceso en trx 
                 Gr.Domi.Delivery%,                                        \! Delivery capturado
                 Gr.Domi.Graba%                                             ! GRabacion UD 
String    Global Gr.Domi.Motora$,                                          \! Tecla motora 
                 Gr.Domi.Lista$(2)                                          ! Lista domicialios
Integer*2 Global Gr.Domi.Ptg%                                               ! Ptg domiciliario
Integer*4 Global IR.PRICE1, IR.PRICE2   																		! Precios producto
Integer*4 Global Asc.Tmp.Apun%																							! Apuntador strings 

String    Global Gr.Clqr.Term$,                                            \! Lista terminales QR
                 Gr.Clqr.Url$,                                             \! URL a generar 
                 Gr.Clqr.Msg1$,                                            \! Mensaje 1
                 Gr.Clqr.Msg2$,                                            \! Mensaje 2
                 Gr.Clqr.Msg3$,                                            \! Mensaje 3
                 Gr.Domi.Pedido$,                                          \! Numero de pedido
                 Gr.Domi.LstaTarj$                                          ! Lista tarjetas crd inst. 24Jul2025
                 
%INCLUDE EAMTSWKG.J86          																							! working storage
%INCLUDE EAMTRANS.J86          																							! Variables de transacciones
%INCLUDE EAMTERMS.J86          																							! Variables de terminales
%INCLUDE EAMTOPTS.J86	         																							! Variables Terminal Options
%INCLUDE EAMITEMR.J86          																							! Variables EAMITEMR
%INCLUDE JAVAGUIV.J86          																							! Variables java
%INCLUDE EAMTSXHC.J86          																							! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          																							! Rutinas de Assembler

%INCLUDE RECATSSU.011          					      															! RUTINAS GENERICAS APLICACION     
%INCLUDE JAVAGUIC.J86

Sub TSHIECET EXTERNAL
End Sub

!------------------------------------------------------------------------------
! Recibe datos desde desde teclado en modo alfanumerico
! msg1$, msg2$ mensaje para linea 1 y linea 2 del visor
! retorna una cadena donde el primer caracter indica
! "E" en caso de error, "P" dato ingresado (tecla PLU),
! "A" aborta el ingreso (tecla BORRAR o ANULAR)
! el segundo caracter "G" si la llave esta girada
! del tercer en adelante el valor ingresado
! si esta activo el indicador teclaAnular
! la tecla BORRAR limpia el texto ingresado y la tecla
! ANULAR aborta el ingreso
!------------------------------------------------------------------------------
Function ingresoTexto$( msg1$, msg2$ ) External
   String ingresoTexto$, msg1$, msg2$
End Function 

Sub grabatrx99(num, trx99$) External
   string trx99$,s$
   integer*2 num
End Sub

Sub CARGA.DOMICILIOS																												! Carga lista domiciliarios
Integer*2 XI%
String Xdata1$, Xdata2$, Xdata3$, Xdata4$, Xdata5$
Dim Gr.Domi.Lista$(10,3)
Gr.Domi.LstaTarj$ = ""
TS.ER.RETURN = -1
Open "R::TF:DOMICI" AS 94 UNLOCKED NOWRITE NODEL 
If TS.ER.RETURN NE -1 Then Begin
   Call VISORES4690(1,"ERROR APERTURA ","DOMICILIOS",1500,"C")
   Gr.Domi.Ok% = 0
   Exit Sub 
EndIf  
If End # 94 Then SALIDA.DOMIC
LEER.DOMIC:
   READ # 94 ; Xdata1$, Xdata2$, Xdata3$, Xdata4$, Xdata5$
   XI% = Val(Xdata1$)																												! Nro domicilario
   If (XI% > 0 And XI% <= 10) Then Begin																		! Maximo 10 posiciones
     Gr.Domi.Lista$(XI%,0) = Xdata2$																				! Descriptor
     Gr.Domi.Lista$(XI%,1) = Xdata3$																				! Porcentaje
     Gr.Domi.Lista$(XI%,2) = Xdata4$																				! tarjeta 1
     Gr.Domi.Lista$(XI%,3) = Xdata5$																				! tarjeta 2
     Gr.Domi.LstaTarj$ = Gr.Domi.LstaTarj$ + ";" + Xdata4$ + ";" + Xdata5$ + ";"
   EndIf
   GoTo LEER.DOMIC
   
SALIDA.DOMIC:
  Close 94

End Sub 																																		!

Sub GRDOMICILIOS(Xopt%) Public 																							! Modulo de domicilios
Integer*4 Xopt%            																								  ! UE ejecutar
String Xtmp$, Xdato$

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.Domi.Ok%  = 0                                                          ! Proyecto Apagado
  TS.ER.RETURN = -1																												  ! Control errores
  Open "R::$ARGENER" As 94 UNLOCKED NOWRITE NODEL 												  ! Apertura archivo parametrizacion para TCxSky
  If TS.ER.RETURN <> -1 Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR APERTURA DOMICILIOS   ")									! MSg alerta
  	 Call U.IMPRIME("ERROR APERTURA DOMICILIOS    ",6100H)                  !
  	 Exit Sub 																															! Sale del proceso
  EndIf  																																		!
  If End #94 Then UE.FIN.DOMICILIOS      																	  ! Si es EOF
  While (1)															  																  ! Recorre archivo parametros
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[DOMICILIARIOS]" Then Begin	   	     				        ! Domiciliarios
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Domi.Ok%     = Val(Mid$(TS.TEMP1$,30,2)) 		    					      ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Domi.Motora$ = Mid$(TS.TEMP1$,30,3)  		    					          ! Tecla Motora
        EndIf																																! Fin carga
        If TS.TEMP1$ = "[IMPRESION QR]" Then Begin	   	     				        ! Impresion QR clientes
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Clqr.Term$ = Mid$(TS.TEMP1$,30,50)															! Lista terminales
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Clqr.Url$ = Mid$(TS.TEMP1$,30,80)  															! URL imprimir
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Clqr.Msg1$ = Mid$(TS.TEMP1$,30,37)															! Lista Mensaje 1
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Clqr.Msg2$ = Mid$(TS.TEMP1$,30,37)															! Lista Mensaje 2
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Clqr.Msg3$ = Mid$(TS.TEMP1$,30,37)															! Lista Mensaje 3
        EndIf
  Wend 																																			! Fin carga
  UE.FIN.DOMICILIOS:                                                        !
     Close 94																																! Cierra archivo
     Call CARGA.DOMICILIOS																							    ! Carga lista domiciliarios
   If Gr.Domi.Ok% = -1 Then                                                \! Proyecto Activo
      Call U.IMPRIME("MODULO DOMICILIARIOS ON  31-Jul-2024",6100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO DOMICILIARIOS OFF 31-Jul-2024",2100H)          ! Msg Proyecto Cargado
EndIf 																																			! Fin carga de parametros 

If Gr.Domi.Ok% <> -1  Then Exit Sub                                         ! Si proyecto apagado
If TS.STANDALONE       Then Exit Sub	                                      ! Si en operacion TOF

!--- EAMTSU02.J86
If Xopt% = 02 Then Begin                                                    ! Al inicio de trx
   Gr.Domi.Intrx% = 0
   Gr.Domi.Ptg% = 0
   Gr.Domi.Delivery% = 0
   Gr.Domi.Graba% = 0 
EndIf																																				!


!--- EAMTSU08.J86
If Xopt% = 08 Then Begin                                                    ! En validacion Items
   If Gr.Domi.Intrx% = -1 Then Begin																				! Si proceso domiciliario
   	  If Gr.Domi.Ptg% = 0 Then Exit Sub 																		! No realiza calculos Mod 05Ago2024
   	  TS.TEMP5$ = UNPACK$(IR.SALEPRIC$)                                     ! Desempaqueta precio de venta
   	  If Unpack$(IR.INDICAT2$) = "03" Then Begin														! Precios Mayoristas
   	  	 IR.PRICE1 = Val(Left$(TS.TEMP5$,5))
   	  	 IR.PRICE2 = Val(Right$(TS.TEMP5$,5))
   	  EndIf
   	  If Unpack$(IR.INDICAT2$) = "00" Then Begin														! Precios unitario
   	  	 IR.PRICE1 = Val(TS.TEMP5$)
   	  	 IR.PRICE2 = 0
   	  EndIf
   	  TS.TEMP1I4 = ROUND(FLOAT(IR.PRICE1) * (1.+FLOAT(Gr.Domi.Ptg%)/10000.),0,0) !
   	  TS.TEMP2I4 = ROUND(FLOAT(IR.PRICE2) * (1.+FLOAT(Gr.Domi.Ptg%)/10000.),0,0) !
   	  IR.PRICE1 = TS.TEMP1I4 : IR.PRICE2 = TS.TEMP2I4												! Asigna nuevo precio
   EndIf																																		!
EndIf																																				!

!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                    ! En secuencias de teclado
   If TS.IO.MOTORKEY = Val(Gr.Domi.Motora$) Then Begin											! Si tecla domicilios
   	  If Gr.Domi.Intrx% = -1 Then Begin
   	  	 Call VISOR.AND.BORRAR("PROCESO YA CAPTURADOEN TRANSACC. /Borrar")  ! Msg Alerta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Exit Sub 
   	  EndIf
   	  If TS.INTRX Then Begin																								! En transaccion 
   	  	 Call VISOR.AND.BORRAR("YA EXISTE UNA COMPRAEN PROCESO   /Borrar")  ! Msg Alerta
         Call VISOR.AND.BORRAR("PROCESO NO ESTA     AUTORIZADO   /Borrar")  ! Msg Alerta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Exit Sub 
   	  EndIf																																	!
   	  
      !jGuiSubState = 80538																									! Captura dato Format valor
      !jGuiSubState = 80501																									! Captura dato numerico
      If java.init = -1 Then begin																					! Si ambiente gráfico
         jGuiSubState = 88001																								! Dialogo domicilarios      
         Call javaEvent(terminalSubStateMsg)																! numerico
   	     TS.TEMP1$ = ASIC.DATOS$("INGRESE CODIGO DE   ","DELIVERY           ") !
   	  EndIf Else Begin   
         I% = 0 : TS.TEMP2$ = ""
         For I% = 1 To 5
          If (Gr.Domi.Lista$(I%,2)) <> "" Then                             \!
           TS.TEMP2$ = TS.TEMP2$ + ( Str$(I%) + "."+ Gr.Domi.Lista$(I%,0) + " " )
         Next I%  
         TS.TEMP1$ = ASIC.DATOS$(Mid$(TS.TEMP2$,1,20),Mid$(TS.TEMP2$,21,20))    
      EndIf 
      If TS.TEMP1$ = "E" Then BEGIN 													    			    ! Proceso cancelado   
      	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO /Borrar")				    !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Exit Sub      																											!
      EndIf 																														    !
      If Val(TS.TEMP1$) < 0 Or Val(TS.TEMP1$) > 10 Then BEGIN               !
      	 Call VISOR.AND.BORRAR("DATO INVALIDO /Borrar  ")								    !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Exit Sub      																											!
      EndIf																																	!
      TS.TEMP1I1 = Val(TS.TEMP1$)																						!
      If Val(Gr.Domi.Lista$(TS.TEMP1I1,1)) < 0 Then Begin
      	 Call VISOR.AND.BORRAR("ERROR DEFINICION    DELIVERY     /Borrar")  !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Exit Sub      																											!
      EndIf																																	!
      if java.init = -1 Then Begin																					! Habilita estado 101
         jGuiSubState = 80670																								! para entrada de datos
         call javaEvent(terminalSubStateMsg)																! alfanumericos
      endif																																	!
      
      Gr.Domi.Intrx% = -1																										! Domicilio en proceso
      Gr.Domi.Ptg% = Val(Gr.Domi.Lista$(TS.TEMP1I1,1))                      ! Asigna ptg domiciliario
      Gr.Domi.Delivery% = TS.TEMP1I1                                        ! ID domiciliario
      Gr.Domi.Graba% = -1																										! Ordena grabar UD 
      Gr.Domi.Pedido$ = "000000"                                            ! Numero de pedido
      !jGuiSubState = 80501																									! Teclado numerico
      !jGuiSubState = 80509																									  ! Teclado qwerty
      
      jGuiSubState = 80508																								  ! Teclado qwerty
      Call javaEvent(terminalSubStateMsg)																		!
      Gr.Domi.Pedido$ = ingresoTexto$("INGRESE NUMERO DEL","PEDIDO /Borrar")! 
      If Left$( Gr.Domi.Pedido$, 1 ) = "A"  Then Gr.Domi.Pedido$ = "E"			!
      If Left$( Gr.Domi.Pedido$, 1 ) <> "P" Then Gr.Domi.Pedido$ = "E"			!
      If Gr.Domi.Pedido$ = "E" Then BEGIN														     		! Proceso cancelado
         Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								    !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						   	!
         Gr.Domi.Intrx% = 0																									! Domicilio en proceso
         Gr.Domi.Ptg% = 0                                                   ! Asigna ptg domiciliario
         Gr.Domi.Delivery% = 0                                              ! ID domiciliario
         Gr.Domi.Graba% = 0																								  ! Ordena grabar UD 
         Gr.Domi.Pedido$ = "000000"                                         ! Numero de pedido
         Exit Sub 																											   	!
      EndIf 																													     	!
      Gr.Domi.Pedido$ = Right$( Gr.Domi.Pedido$, len( Gr.Domi.Pedido$ ) - 2 ) ! toma dato capturado
      TS.TEMP5$ = "OPERADOR DELIVERY   " + Gr.Domi.Lista$(TS.TEMP1I1,0) +  \!
                  " /Borrar"																								!
      Call VISOR.AND.BORRAR(TS.TEMP5$)																			!
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      !
      TS.IO.MOTORKEY = 73																						    		!
      Exit Sub      																												!
   EndIf																																		! Fin tecla motora
EndIf																																				!

!--- EAMTSU53.J86
If Xopt% = 53 Then Begin                                                    ! En recuperacion de trx 
 If Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "99" THEN BEGIN   	   	              ! Si user data
  If Unpack$(MID$(SL.STR.ENTRY$,3,4)) = "1003" Then Begin                   ! Proyecto Domiciliarios
    Asc.Tmp.Apun% = 3																												!
    Gr.Domi.Intrx% = -1																											! Domicilio en proceso
    TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                   ! Numero de proyecto
    TS.TEMP1$ = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)           				! Id del domiciliario
    TS.TEMP1I1 = Val(TS.TEMP1$)
    Gr.Domi.Delivery% = TS.TEMP1I1
    TS.TEMP1$ = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)           				! Porcentaje domicilio
    Gr.Domi.Ptg% = Val(TS.TEMP1$)																						! Asigna variable
    Call VISOR.AND.BORRAR("OPER.DELIVERY RECUP."+Gr.Domi.Lista$(TS.TEMP1I1,0))
    Gr.Domi.Graba% = -1
  EndIf																																			! 
 EndIf 
EndIf																																			  ! Fin Eamtsu53

!--- EAMTSU43.J86
If Xopt% = 43 Then Begin                                                    ! En suspension de trx 
 If Gr.Domi.Graba% = -1 Then Begin
    TS.TEMP1$  = (Str$(Gr.Domi.Delivery%))                    +            \! ID domiciliario
                 ":"+(Str$(Gr.Domi.Ptg%))                     +            \! Porcentaje domicilio
                 ":"+ Gr.Domi.Pedido$                         +            \! Numero del pedido
                 ":"+Pack$("00")                                            ! Filler 
    Call Grabacion.Cadena.Usuario("1003",TS.TEMP1$)                         ! Graba String usuario  
 EndIf 
EndIf																																				! Fin Eamtsu43.j86

!--- EAMTSU56.J86
If Xopt% = 56 Then Begin                                                    ! En suspension de trx 
   Gr.Domi.Intrx% = 0
   Gr.Domi.Ptg% = 0
   Gr.Domi.Delivery% = 0
   Gr.Domi.Graba% = 0 
EndIf																																				! Fin Eamtsu56
 
End Sub 																																		! Fin libreria
