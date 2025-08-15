!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : SIMULAR.BAS                      *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   *
!Fecha         : Julio 14 del 2.004               *
!Observaciones : SIMULACION REGISTRO PARA         *
!                PRUEBAS DE REGISTRO              *
!**************************************************
!

%Environ T                 ! Ambiente de terminal

String    Global ListaItems$(2)
Integer*4 Global Gr.Lst.Itm%, Gr.Lst.Cont%
Integer*1 Global Gr.Lst.Proc%

%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          ! Variables procesos de transaccion
%INCLUDE EAMTERMS.J86          ! Variables procesos terminal
%INCLUDE EAMTOPTS.J86          ! Variables Terminal Options
%INCLUDE EAMTSEVA.J86          ! Variables Cliente Frecuente
%INCLUDE EAMITEMR.J86          ! Variables ITEMR
%INCLUDE EAMTSXHC.J86
%INCLUDE EAMASMRT.J86

%INCLUDE RECATSSU.011          																			          ! Rutinas Genericas Grupo Retail

Sub Carga.Lista.Simulacion																									! Carga lista items simulacion
String Xean$, XQty$, Xtip$, Xacc$, Xname$
  Xname$ = "R::ADX_UDT1:SIMULA"+Right$("00"+TS.TEMP1$,2)+".TXT"
  Dim ListaItems$(700,3)																										! 
  TS.ER.RETURN = -1
  Open Xname$ As 94 NOWRITE NODEL																						! Archivo lista Items
  If TS.ER.RETURN <> -1 Then Begin
  	 TS.TEMP1I1 = 0 																												! Control proceso
  	 Exit Sub 
  EndIF 
  If End #94 Then GR.ITM.FINAL
  Gr.Lst.Itm% = 0
  While (1)
   Read # 94 ; Xean$, XQty$, Xtip$, Xacc$																		! Lectura items 
   Gr.Lst.Itm% = Gr.Lst.Itm% + 1
   ListaItems$(Gr.Lst.Itm%,0) = Xean$
   ListaItems$(Gr.Lst.Itm%,1) = XQty$
   ListaItems$(Gr.Lst.Itm%,2) = XTip$
   ListaItems$(Gr.Lst.Itm%,3) = XAcc$																			
  Wend 
  GR.ITM.FINAL:
   Close 94
   Call VISOR.AND.BORRAR("TOTAL ITEMS "+Str$(Gr.Lst.Itm%))
End Sub 																																		! Fin carga lista simulacion

Sub SIMULAR.VENTA.POS(Xopt%) Public
Integer*4 Xopt%, Gri%

!-- EAMTSU14
If Xopt% = 14 Then Begin																										! En eamtsu14
	 If TS.IO.MOTORKEY = 215 Then Begin																				! Tecla simulacion venta
	 	  Gr.Lst.Proc% = 0
	 	  Call VISOR.AND.BORRAR("GENERANDO SIMULACIONDE VENTA  Borrar")
	 	  TS.TEMP1I1 = -1																												! Control proceso
      TS.TEMP1$ = ASIC.DATOS$("NUMERO DE SIMULACION","")
      If TS.TEMP1$ = "E" Then BEGIN 																			  ! Proceso cancelado	
      	 Call VISOR.AND.BORRAR("PROCESO CANCELADO PORUSUARIO   Borrar")
      	 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														! Init secuencias registro
      	 TS.IO.MOTORKEY = 73																								! Simula tecla borrar
      	 Exit Sub 																													!
      EndIf
      If Val(TS.TEMP1$) <= 0 Or Val(TS.TEMP1$) > 99 Then Begin							! 
      	 Call VISOR.AND.BORRAR("ERROR EN NUMERO   DE SIMULACION Borra")
      	 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														! Init secuencias registro
      	 TS.IO.MOTORKEY = 73																								! Simula tecla borrar
      	 Exit Sub 																													!
      EndIf
      Call Carga.Lista.Simulacion																						! Carga items simulacion venta
      If TS.TEMP1I1 <> -1 Then Begin																				! Error apertura
      	 Call VISOR.AND.BORRAR("LISTA DE  SIMULACIONNO EXISTE  Borrar")
      	 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														! Init secuencias registro
      	 TS.IO.MOTORKEY = 73																								! Simula tecla borrar
      	 Exit Sub 																													!
      EndIf																																	!
      Gr.Lst.Proc% = -1																											!
      Gr.Lst.Cont% = 1
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Init secuencias registro
      TS.IO.KEYS(2)  = 80
      TS.IO.DATA$(2) = ListaItems$(1,0)																			! Entrega plu
      TS.IO.DEVICE   = 1
      TS.IO.MOTORKEY = 80
    EndIf  
EndIf																																				! Final secuencia

!-- EAMTSU08
If Xopt% = 08 Then Begin																										! En eamtsu14
	 If Gr.Lst.Proc% = -1 Then Begin																					! En simulacion
	 	  If Gr.Lst.Cont% <= Gr.Lst.Itm% Then Begin															! 
	 	  	 CHAIN.COUNT = 1                                             				! Contador de item linkeados
	 	  	 If Gr.Lst.Cont% = 1 Then Begin
            SL.IT.ITEMCODE$ = ListaItems$(1,0)
            TS.IO.DATA$(2)  = SL.IT.ITEMCODE$																!
	 	  	 EndIf
	 	  	 If Ucase$(ListaItems$(Gr.Lst.Cont%,3)) = "A" Then                 \! Si un Item anulado
	 	  	 	  TS.IO.KEYS(8) = 67 Else TS.IO.KEYS(8) = 0
	 	  	 If ListaItems$(Gr.Lst.Cont%+1,0) = "999999999999" Then  Begin   	  ! Ultimo del proceso
	 	  	 	  IR.LINKEDTO$ = Pack$("0000")																  	! Termina ciclo
	 	  	 	  Gr.Lst.Proc% = 0 
	 	  	 EndIf Else Begin 
            IR.LINKEDTO$ = Pack$(ListaItems$(Gr.Lst.Cont%+1,0))	            !
         EndIf
         If IR.INDICAT0 AND 40H Then Begin        													! Si el Item es pesable 
            TS.IO.KEYS(6)  = 72																							! Simula secuencia registro
            TS.IO.DATA$(6) = ListaItems$(Gr.Lst.Cont%,1)
         EndIf Else Begin																										! Item por cantidad
            TS.IO.KEYS(6)  = 75																							! Simula cantidad
            TS.IO.DATA$(6) = ListaItems$(Gr.Lst.Cont%,1)
         EndIf																															!
	 	  	 Gr.Lst.Cont% = Gr.Lst.Cont% + 1																		! 
	 	  EndIf 
	 EndIf																																		! Fin simulacion
EndIf

!-- EAMTSU08
If Xopt% = 43 Then Begin																										! En eamtsu14

EndIf

End Sub

