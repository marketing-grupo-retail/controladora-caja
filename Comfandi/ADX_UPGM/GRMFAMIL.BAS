!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMFAMIL.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Registro Famlias en Accion       *
!**************************************************
!Observaciones:

%ENVIRON T		                          																			! Ambiente de terminal

Integer*1 Global  Gr.Famil.Ok%, Gr.Famil.InTrx%
String    Global  Gr.Famil.Key$, Gr.Famil.Plu$

%INCLUDE EAMTSWKG.J86          																								! working storage
%INCLUDE EAMTRANS.J86          																								! working storage
%INCLUDE EAMITEMR.J86          																								! working storage
%INCLUDE RECATSSU.011        																									! Rutinas Genericas Grupo Retail

Sub GRFAMILIAS(XOPT%) Public																						    	! Familias en Accion
Integer*4 Xopt%

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                       ! Carga de parametros
  Gr.Famil.Ok%  = 0                                                           ! Proyecto Apagado
  TS.ER.RETURN = -1																													  ! Ctrl Errores                     
  OPEN "R::ASCNTRL" AS 94					  		  																	  ! Apertura archivo parametrizacion 
  If TS.ER.RETURN <> -1 Then BEGIN                                            !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA ARCHIVO PARAMETROS")
  	 Exit Sub 
  ENDIF 
  IF END #94 THEN UE.FIN.GRFAMI         																	    ! Si es EOF                        
  While (1)															  																    ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																				! Lectura registro                 
        IF TS.TEMP1$ = "[FAMILIAS EN ACCION]" Then Begin		   					      ! Bonos Premio 
         Read #94; TS.TEMP1$																									! Lectura registro                 
         Gr.Famil.Ok%   = Val(Mid$(TS.TEMP1$,30,2))      			    					  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						  ! Lectura registro                 
         Gr.Famil.Key$  = Mid$(TS.TEMP1$,30,03)        			                  ! Tecla Motora Registro
         Read #94; TS.TEMP1$     																						  ! Lectura registro                 
         Gr.Famil.Plu$  = Mid$(TS.TEMP1$,30,12)        			                  ! Plu registro familias en accion
         GoTo UE.FIN.GRFAMI 																								  ! Sale del ciclo de carga          
       Endif                                                                  !
   Wend                                                                       !
   UE.FIN.GRFAMI:                                                             !
     Close 94																																  ! Cierra archivo
   If Gr.Famil.Ok% = -1 Then                                                 \! Proyecto Activo
      Call U.IMPRIME("MODULO FAMIL. ACCION   ON 30/May/2013",2100H) Else     \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO FAMIL. ACCION  OFF 30/May/2013",2100H)           ! Msg Proyecto Cargado
EndIf 																																				! Final Eamtsu07

If Gr.Famil.Ok% <> -1 Then Exit Sub                                           ! Si proyecto apagado

!--- EAMTSU02.J86
If Xopt% = 2 Then Begin                                                       ! Inicializacion de trx
	 Gr.Famil.InTrx% = 0
EndIf 																																				! Final Eamtsu02

!--- EAMTSU08.J86
If Xopt% = 8 Then Begin                                                       ! Validacion datos Item
	IF Gr.Famil.InTrx% = -1 THEN \                                              ! En pago familias en accion
   If VAL(SL.IT.ITEMCODE$) <> Val(Gr.Famil.Plu$) Then Begin                   ! No es el plu de pago
      IR.INDICAT0 = IR.INDICAT0 OR 04H					         											! No permite venta
      Exit Sub																									  					  ! Sale del Procedimiento
   EndIf
   
  IF TS.INTRX THEN \                                                          ! En transaccion
   If VAL(SL.IT.ITEMCODE$) = Val(Gr.Famil.Plu$) Then Begin                    ! Es el plu de pago familias accion
      IR.INDICAT0 = IR.INDICAT0 OR 04H					         											! No permite venta
      Gr.Famil.InTrx% = 0
      Exit Sub																									  					  ! Sale del Procedimiento
   EndIf
  	 
EndIf 																																				! Final Eamtsu08

!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                      ! Secuencias de tecleo
   IF TS.TOTALS(0,0,0) = 1 And Gr.Famil.InTrx% = -1 THEN \                    ! Si es solo subsidio
    If TS.IO.KEYS(7) > 90 AND TS.IO.KEYS(7) < 97 Then Begin                  	! Si forma de pago 
      TS.TEMP3$ = Str$(TS.IO.KEYS(7) - 90)                                  	! Toma Tipo de pago
      If TS.IO.DATA$(3) = "" Then TS.TEMP3$ = TS.TEMP3$ + "1" Else           \! Toma variedad de pago 
         TS.TEMP3$ = TS.TEMP3$ + TS.IO.DATA$(3)                             	! 
      If TS.TEMP3$ <> "41" Then Begin                                         ! Diferente de TEF
      	 CALL VISOR.AND.BORRAR("FORMA DE PAGO NO    AUTORIZADA")              ! Msj al operador
      	 DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) : TS.IO.MOTORKEY = 73       ! Cancela secuencia de tecleo
      	 EXIT SUB                                                             ! Sale del proceso
      EndIf
    EndIf 
    
    IF NOT(TS.INTRX) THEN \                                                   ! No esta en una trx
    If TS.IO.MOTORKEY = VAL(Gr.Famil.Key$) THEN BEGIN                         ! Si tecla Motora Familias en accion
   	   DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10)                               ! Cancela secuencia de tecleo
   	   TS.IO.MOTORKEY = 80
       TS.IO.DATA$(2) = Gr.Famil.Plu$
       TS.IO.KEYS(2) = 80
       TS.IO.KEYS(6) = 75                      
       TS.IO.DATA$(6) = "1"
       Gr.Famil.InTrx% = -1
    ENDIF

EndIf 																																				! Final Eamtsu14
  
!--- EAMTSU20.J86
If Xopt% = 20 Then Begin                                                      ! Secuencias de tecleo

If MATCH(TS.SDESC$(59),TS.PRTBUF$,1) > 0 THEN BEGIN														! Si anulacion total de la trx
   Gr.Famil.InTrx% = 0
EndIf																																					!

EndIf																																					! Final Eamtsu20

!--- EAMTSU56.J86
If Xopt% = 56 Then Begin                                                      ! Al suspender una trx
	 Gr.Famil.InTrx% = 0
EndIf 																																				! Final Eamtsu56


End Sub 
