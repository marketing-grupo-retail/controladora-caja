!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMREMAT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Remates                   *
!**************************************************
! Modificaciones:
! ----- Version 1.0 10 Marzo 2023 -----------------

%ENVIRON T																																	! Ambiente terminal

Integer*1 Global Gr.Remate.Ok%,                                            \!
                 Gr.Remate.Intrx%,                                         \!
                 Gr.Remate.PLiq%
String    Global Gr.Remate.Pref$,                                          \!
                 Gr.Remate.Ean$,                                           \!
                 Gr.Remate.Pmae$,                                          \!
                 Gr.Remate.Precio$,                                        \!
                 Gr.Remate.Pean$,                                          \!
                 Gr.Remate.Motora$                                          !
String    Global IR.USERDATA$																								! Datos adicionales IR
Integer*4 Global IR.PRICE1, IR.PRICE2   																		! Precios producto

%INCLUDE EAMTSWKG.J86          																							! working storage
%INCLUDE EAMTRANS.J86          																							! Variables de transacciones
%INCLUDE EAMTERMS.J86          																							! Variables de terminales
%INCLUDE EAMTOPTS.J86	         																							! Variables Terminal Options
%INCLUDE EAMITEMR.J86          																							! Variables EAMITEMR
%INCLUDE EAMTSXHC.J86          																							! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          																							! Rutinas de Assembler

%INCLUDE RECATSSU.011          					      															! RUTINAS GENERICAS APLICACION     

Sub TSHIECET EXTERNAL
End Sub

Function VALIDA.EAN128(Xdata$)
Integer*1 VALIDA.EAN128

VALIDA.EAN128 = -1

End Function 

Sub GRREMATES(Xopt%) Public 																								! Modulo de remates
Integer*4 Xopt%            																								  ! UE ejecutar
String Xtmp$ 
!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.Remate.Ok%  = 0                                                        ! Proyecto Apagado
  TS.ER.RETURN = -1																												  ! Control errores
  Open "R::$ARGENER" As 94 UNLOCKED NOWRITE NODEL 												  ! Apertura archivo parametrizacion para TCxSky
 !Open "R::$SCNTRL" As 94 UNLOCKED NOWRITE NODEL 												    ! Apertura archivo parametrizacion para V6R5
  
  If TS.ER.RETURN <> -1 Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR APERTURA REMATES      ")									! MSg alerta
  	 Call U.IMPRIME("ERROR APERTURA REMATES       ",6100H)                  !
  	 Exit Sub 																															! Sale del proceso
  EndIf  																																		!
  If End #94 Then UE.FIN.REMATES         																	  ! Si es EOF
  While (1)															  																  ! Recorre archivo parametros
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[REMATES ITEMS]" Then Begin	   	     				        ! FE Naturales
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Remate.Ok%     = Val(Mid$(TS.TEMP1$,30,2)) 		    					    ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Remate.Pref$   = Mid$(TS.TEMP1$,30,2)  		    					        ! Prefijo barra 
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Remate.Motora$ = Mid$(TS.TEMP1$,30,3)  		    					        ! Tecla Motora

         GoTo UE.FIN.REMATES																							  ! Sale del ciclo de carga                   
        EndIf																																! Fin caga
  Wend 																																			! Fin carga
  UE.FIN.REMATES:                                                           !
     Close 94																																! Cierra archivo
   If Gr.Remate.Ok% = -1 Then                                              \! Proyecto Activo
      Call U.IMPRIME("MODULO REMATES ITEMS ON  04-Myo-2023",6100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO REMATES ITEMS OFF 04-Myo-2023",2100H)          ! Msg Proyecto Cargado
EndIf 																																			! Fin carga de parametros 

If Gr.Remate.Ok% <> -1  Then Exit Sub                                       ! Si proyecto apagado
If TS.STANDALONE       Then Exit Sub	                                      ! Si en operacion TOF

!--- EAMTSU02.J86
If Xopt% = 02 Then Begin                                                    ! Nueva transaccion
   Gr.Remate.Ean$ = ""
   Gr.Remate.Precio$ = "0"
   Gr.Remate.Pmae$ = "0"
   Gr.Remate.Pean$ = ""
   Gr.Remate.PLiq% = 0
EndIf																																				! Fin EAMTSU02.J86

!--- EAMTSU08.J86
If Xopt% = 08 Then Begin                                                    ! Nueva transaccion
	If Gr.Remate.Intrx% = -1 Then Begin																				! Venta de remate
   If Val(SL.IT.ITEMCODE$) = Val(Gr.Remate.Ean$) Then Begin                 ! Item de remate
   	 Gr.Remate.Pmae$ = Str$(IR.PRICE1)																			! Precio Maestra

!     IR.PRICE1 = Val(Gr.Remate.Precio$)																			! Retorna precio del item
!     IR.PRICE2 = IR.PRICE1

   EndIf Else Begin
   	 Gr.Remate.Intrx% = 0
     Gr.Remate.Ean$ = ""
     Gr.Remate.Precio$ = "0"
     Gr.Remate.Pmae$ = "0"
   EndIf
  EndIf																																

!--- Control preliquidados 22
	If Gr.Remate.PLiq% = -1 Then      																				\! Venta de preliquidado 22
   If Val(SL.IT.ITEMCODE$) <> Val(Gr.Remate.Pean$) Then Begin                ! Item de remate
   	 Gr.Remate.PLiq% = 0
     Gr.Remate.PEan$ = ""
   EndIf

EndIf																																				! Fin EAMTSU08


!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                    ! validando forma de pago
	
!-- Informacion de bandejas pesables
 If LEN(TS.IO.DATA$(2)) >= 12 And Mid$(TS.IO.DATA$(2),1,2) = "22" Then Begin! Preliquidados 
 	  Gr.Remate.PLiq% = -1																										! venta preliquidado
 	  Gr.Remate.Pean$ = Mid$(TS.IO.DATA$(2),3,5)                              ! Plu pesable
 EndIf																																			! Fin preliquidados

 If TS.IO.MOTORKEY = Val(Gr.Remate.Motora$) Then Begin                      ! Captura remates
  TS.TEMP1I1 = 0
  If (TS.IO.KEYS(1) = 70) Then TS.TEMP1I1 = -1			  									    ! Si anulacion del movimiento
 	Xtmp$ = Entrada.Datos.Varios$( "INGRESE CODIGO  ","PRODUCTO REMATE" )     ! Entrada de dato
  If XTMP$ = "E" Then Begin 																		        		! Proceso cancelado   
 	   Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								        !
     Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												        !
     TS.IO.MOTORKEY = 73																						        !
     TS.TEMP1I1 = 0																													!
     Exit Sub 																											        !
  EndIf																																			!

  If LEN(XTMP$) >= 20 And Mid$(Xtmp$,3,2) = Gr.Remate.Pref$ Then Begin			! validacion barras remate
    TS.TEMP1I4 = VALIDA.EAN128(Xtmp$)														  					! Valida digito chequeo
    If TS.TEMP1I4 = -1 Then Begin																						! Codigo OK
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
       TS.IO.KEYS(2)  = 80
       TS.IO.MOTORKEY = 80
    	 TS.IO.DATA$(2)    = Mid$(Xtmp$,5,12)																	! EAN a facturar
    	 If TS.TEMP1I1 = -1 Then TS.IO.KEYS(1) = 70
    	 TS.IO.DEVICE = 1																											! Simula entrada digitada
    	 Gr.Remate.Ean$    = Mid$(Xtmp$,5,12)                                 ! EAN
    	 Gr.Remate.Precio$ = Mid$(Xtmp$,17,6)                                 ! Precio del remate
    	 Gr.Remate.Intrx% = -1 																								! 
    EndIf Else Begin
    	 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
    	 TS.IO.MOTORKEY = 0
    EndIf																																		!
  EndIf Else Begin																													! Dato no valido
       Call VISOR.AND.BORRAR("DATO NO VALIDO")  	
    	 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
    	 TS.IO.MOTORKEY = 73  	
  	   Exit Sub 
  EndIf																																			!
 EndIf																																			! Fin tecla motora
EndIf																																				! Fin EAMTSU14


!--- EAMTSU20.J86
If Xopt% = 20 Then Begin                                                    ! Nueva transaccion
 If TS.LINETYPE = 1 Then                                                   \! Es un articulo
	If Gr.Remate.Intrx% = -1 Then Begin																				! Venta de remate
   	 Xtmp$ = "00"																														! Default Venta
   	 If TS.IO.KEYS(1) = 70 Then Xtmp$ = "01"																! Anulacion venta
     TS.TEMP1I4 = GETN4( IR.USERDATA$, 16 )																	! Codigo interno
     TS.TEMP2$  = Str$( TS.TEMP1I4 )																				!
     TS.TEMP3$  = "00"																				              ! NO PESABLE
     If (IR.INDICAT0 And 40H) Then TS.TEMP3$  = "01"										    ! PESABLE
     TS.TEMP1$  = PACK$(SL.IT.ITEMCODE$)                            +	     \! Item vendido
                ":"+PACK$(Str$(SL.IE.SALEPRIC))                  	  +    	 \! Precio unitario Vendido Articulo
                ":"+PACK$(Str$(SL.IT.XPRICE))                  	    +    	 \! Precio total Vendido Articulo
                ":"+PACK$(Str$(SL.IE.QTYORWGT))                	    +    	 \! Cantidad - peso vendido
                ":"+PACK$(TS.TEMP2$)                                +      \! Codigo interno
                ":"+PACK$(TS.TEMP3$)                                +      \! Indicador de pesable
                ":"+PACK$(XTMP$)                                            ! Signo operacion 00.Venta 01.Anul
     Call Grabacion.Cadena.Usuario("1001",TS.TEMP1$)                        ! Almacena user data
     Gr.Remate.Ean$ = ""
     Gr.Remate.Precio$ = "0"
     Gr.Remate.Pmae$ = "0"
     Gr.Remate.Intrx% = 0
   EndIf																																		! Fin UD remates

!-- UD preliquidados 22
 If TS.LINETYPE = 1 Then                                                   \! Es un articulo
	If Gr.Remate.PLiq% = -1 Then Begin																				! Venta preliquidados 22
		 Xtmp$ = "00"																														! Default Venta
   	 If TS.IO.KEYS(1) = 70 Then Xtmp$ = "01"																! Anulacion venta
     TS.TEMP1I4 = GETN4( IR.USERDATA$, 16 )																	! Codigo interno
     TS.TEMP2$  = Str$( TS.TEMP1I4 )
     TS.TEMP3$  = "00"																				              ! NO PESABLE
     If (IR.INDICAT0 And 40H) Then TS.TEMP3$  = "01"										    ! PESABLE
     TS.TEMP1$  = PACK$(SL.IT.ITEMCODE$)                            +	     \! Item vendido
                ":"+PACK$(Str$(SL.IE.SALEPRIC))                  	  +    	 \! Precio unitario Vendido Articulo
                ":"+PACK$(Str$(SL.IT.XPRICE))                  	    +    	 \! Precio total Vendido Articulo
                ":"+PACK$(Str$(SL.IE.QTYORWGT))                	    +    	 \! Cantidad - peso vendido
                ":"+PACK$(TS.TEMP2$)                                +      \! Codigo interno
                ":"+PACK$(TS.TEMP3$)                                +      \! Indicador de pesable
                ":"+PACK$(XTMP$)                                            ! Signo operacion 00.Venta 01.Anul
     Call Grabacion.Cadena.Usuario("1002",TS.TEMP1$)                        ! Almacena user data
     Gr.Remate.PEan$ = ""
     Gr.Remate.PLiq% = 0
   EndIf																																		! Fin UD remates
   
EndIf																																				! Fin EAMTSU20

!--- EAMTSU43.J86
If Xopt% = 43 Then Begin                                                    ! En calculo de precio

	If Gr.Remate.Intrx% = -1 Then Begin																				! Venta de remate
   If Val(SL.IT.ITEMCODE$) = Val(Gr.Remate.Ean$) Then Begin                 ! Item de remate
      SL.IE.SALEPRIC = Val(Gr.Remate.Precio$)																! Retorna precio del Item
      SL.IT.XPRICE = (SL.IE.QTYORWGT * SL.IE.SALEPRIC)                      ! Total compra
   EndIf
  EndIf																																

	
EndIf																																				! fin EAMTSU43

End Sub 																																		!
