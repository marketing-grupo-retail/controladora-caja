!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : GPRESCAN.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   *
!Fecha         : Julio 14 del 2.004               *
!Observaciones : MODULO ROMPE FILAS - PRESCAN     *
!**************************************************
!

%Environ T                 ! Ambiente de terminal

String    Global Gr.Psc.LstItm$(2), Gfile$, Gr.Psc.Pref$,                     \!
                 Gr.Psc.File$																								!
Integer*4 Global Gr.Psc.Itm%, Gr.Psc.Cont%
Integer*1 Global Gr.Psc.Proc%, Gr.Psc.Ok%

%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          ! Variables procesos de transaccion
%INCLUDE EAMTERMS.J86          ! Variables procesos terminal
%INCLUDE EAMTOPTS.J86          ! Variables Terminal Options
%INCLUDE EAMTSEVA.J86          ! Variables Cliente Frecuente
%INCLUDE EAMITEMR.J86          ! Variables ITEMR
%INCLUDE EAMTSXHC.J86
%INCLUDE EAMASMRT.J86

%INCLUDE RECATSSU.011          																			        ! Rutinas Genericas Grupo Retail

Sub Carga.Lista.Prescan																											! Carga lista items simulacion
String Xean$, XQty$, Xtip$, Xacc$, Xname$
  Gr.Psc.File$ = TS.IO.DATA$(2)
  Xname$ = "R::PS:"+MID$(TS.IO.DATA$(2),4,8)+".TXT"                         ! Archivo para busqueda
  Dim Gr.Psc.LstItm$(700,3)																										! 
  TS.TS11WERR$ = "" 
  Open Xname$ As 94 NOWRITE NODEL																						! Archivo lista Items
  If TS.TS11WERR$ <> "" Then Begin
  	 Call VISOR.AND.BORRAR("ERROR "+TS.TS11WERR$)
  	 TS.TEMP1I1 = 0 																												! Control proceso
  	 Exit Sub 
  EndIF 
  If End #94 Then GR.ITM.FINAL
  Gr.Psc.Itm% = 0
  TS.TEMP1I1 = -1
  While (1)
   Read # 94 ; Xean$, XQty$, Xtip$, Xacc$																		! Lectura items 
   Gr.Psc.Itm% = Gr.Psc.Itm% + 1
   Gr.Psc.LstItm$(Gr.Psc.Itm%,0) = Xean$
   Gr.Psc.LstItm$(Gr.Psc.Itm%,1) = XQty$
   Gr.Psc.LstItm$(Gr.Psc.Itm%,2) = XTip$
   Gr.Psc.LstItm$(Gr.Psc.Itm%,3) = XAcc$																			
  Wend 
  GR.ITM.FINAL:
   Close 94
End Sub 																																		! Fin carga lista simulacion

Sub PRESCAN.VENTA(Xopt%) Public																							! Prescan compra
Integer*4 Xopt%, Gri%

!-- EAMTSU07
If XOPT% = 07 Then Begin																										! Carga de opciones
  Gr.Psc.Ok% = 0 																									    			! Init proyecto activo
  TS.TS11WERR$ = ""																													! Control errores
  OPEN "R::$SCNTRL" AS 94 UNLOCKED NOWRITE NODEL												    ! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR APERTURA PARAMETROS PRESCAN")             !
	   Gr.Psc.Ok% = 0 																								   			! Init proyecto activo
  	 Exit Sub																																!
  EndIf 
  If End #94 Then UE.FIN.PRESCAN         																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[ROMPE FILAS]" Then Begin	     	 					    			! Parametros SAP CORE
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Psc.Ok%      = Val(Mid$(TS.TEMP1$,30,2))      			    			  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Psc.Pref$    = Mid$(TS.TEMP1$,30,2)       			    			      ! Prefijo Prescan
         GoTo UE.FIN.PRESCAN                                                ! Sale del proceso
        EndIf																																!
  Wend 																																		  ! Fin recorrido archivo
  UE.FIN.PRESCAN:
    Close 94 
    If Gr.Psc.Ok% = -1 Then                                                \! Proyecto Activo
      Call U.IMPRIME("MODULO ROMPE FILAS  ON    13/May/2019",6100H) Else   \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO ROMPE FILAS  OFF   13/May/2019",6100H)         ! Msg Proyecto Cargado
    
EndIf

If Gr.Psc.Ok% = 00 Then Exit Sub                                            ! Proyecto No Activo

!-- EAMTSU02
If Xopt% = 02 Then Begin
	 Gr.Psc.File$ = ""
	 Gr.Psc.Proc% = 0 
EndIf

!-- EAMTSU14
If Xopt% = 14 Then Begin																										! En eamtsu14
	 
	 If TS.IO.KEYS(2)  = 80 And Len(TS.IO.DATA$(2)) = 11 And                 \! Se capturo una 
      Left$(TS.IO.DATA$(2),3) = "0"+Gr.Psc.Pref$ Then Begin                ! barra prescan
      Gr.Psc.Proc% = 0
      TS.TEMP1I1 = 0
      Call Carga.Lista.Prescan   																						! Carga items simulacion venta
      If TS.TEMP1I1 <> -1 Then Begin																				! Error apertura
      	 Call VISOR.AND.BORRAR("LISTA DE  PRESCAN   NO EXISTE  Borrar")
      	 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														! Init secuencias registro
      	 TS.IO.MOTORKEY = 73																								! Simula tecla borrar
      	 Exit Sub 																													!
      EndIf																																	!
      Gr.Psc.Proc% = -1																											!
      Gr.Psc.Cont% = 1
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Init secuencias registro
      TS.IO.KEYS(2)  = 80
      TS.IO.DATA$(2) = Gr.Psc.LstItm$(1,0)																			! Entrega plu
      TS.IO.DEVICE   = 1
      TS.IO.MOTORKEY = 80
	 EndIf
	
EndIf																																				! Final secuencia

!-- EAMTSU08
If Xopt% = 08 Then Begin																										! En eamtsu14
	 If Gr.Psc.Proc% = -1 Then Begin																					! En simulacion
	 	  If Gr.Psc.Cont% <= Gr.Psc.Itm% Then Begin															! 
	 	  	 CHAIN.COUNT = 1                                             				! Contador de item linkeados
	 	  	 If Gr.Psc.Cont% = 1 Then Begin
            SL.IT.ITEMCODE$ = Gr.Psc.LstItm$(1,0)
            TS.IO.DATA$(2)  = SL.IT.ITEMCODE$																!
	 	  	 EndIf
	 	  	 If Ucase$(Gr.Psc.LstItm$(Gr.Psc.Cont%,3)) = "A" Then              \! Si un Item anulado
	 	  	 	  TS.IO.KEYS(8) = 67 Else TS.IO.KEYS(8) = 0
	 	  	 If Gr.Psc.LstItm$(Gr.Psc.Cont%+1,0) = "999999999999" Then  Begin   ! Ultimo del proceso
	 	  	 	  IR.LINKEDTO$ = Pack$("0000")																	  ! Termina ciclo
	 	  	 	  Gr.Psc.Proc% = 0 
	 	  	 EndIf Else Begin 
            IR.LINKEDTO$ = Pack$(Gr.Psc.LstItm$(Gr.Psc.Cont%+1,0))	        !
         EndIf
         If IR.INDICAT0 AND 40H Then Begin        													! Si el Item es pesable 
            TS.IO.KEYS(6)  = 72																							! Simula secuencia registro
            TS.IO.DATA$(6) = Gr.Psc.LstItm$(Gr.Psc.Cont%,1)
         EndIf Else Begin																										! Item por cantidad
            TS.IO.KEYS(6)  = 75																							! Simula cantidad
            TS.IO.DATA$(6) = Gr.Psc.LstItm$(Gr.Psc.Cont%,1)
         EndIf																															!
	 	  	 Gr.Psc.Cont% = Gr.Psc.Cont% + 1																		! 
	 	  EndIf 
	 EndIf																																		! Fin simulacion
EndIf

!-- EAMTSU08
If Xopt% = 43 Then Begin																										! En eamtsu14

EndIf

End Sub

