!************************************************** 
!Empresa       : Grupo Retail Ltda                 
!Programa      : AJUMEXIT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Ajuste al cambio                 *
!**************************************************

%ENVIRON T		                 ! Ambiente de terminal

%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          ! Variables procesos de transaccion
%INCLUDE EAMTERMS.J86          ! Variables procesos terminal
%INCLUDE EAMITEMR.J86          ! Variables procesos terminal
%INCLUDE EAMTOPTS.J86          ! Variables Terminal Options
%INCLUDE EAMTSEVA.J86          ! Variables Cliente Frecuente
%INCLUDE AJUSTSVA.011	         ! Variables desarrollo
%INCLUDE EAMTSXHC.J86          !
%INCLUDE EAMASMRT.J86          !
%INCLUDE RECATSSU.011   			 ! Rutinas Genericas

Sub TSPREC01  External				 ! Rutina de impresion SMA
End Sub

Sub TSPREC03  External									! Rutima de MSG SMA
End Sub

Sub TSHIECET External								    !  Tono de alerta
End Sub

Function Format.Amount(AMT1) External 	! Formateo de valores
 Integer*1 FORMAT.AMOUNT
 Integer*4 AMT1
End Function

Sub AJUCAMBIO(Xopt%) Public
Integer*4 Xopt%

!EAMTSU07.J86
If Xopt% = 7 Then Begin
TS.ER.RETURN = -1																													! Ctrl Errores
OPEN "R::ASCNTRL" AS 94																										! Apertura archivo parametrizacion
If TS.ER.RETURN <> -1 THEN BEGIN																					! Error en la apertura
 Call TSHIECET																														! Tono de Alerta
 Call Visores4690(1,"Error Parametros","Ajuste al Cambio   ",1500,"L")		! Msg Alerta
 Call Visores4690(1,"Ajuste al Cambio   ","No Operativo",1500,"L")				! Msg Alerta
 Call U.Imprime("Error Carga Ajuste al Cambio    ",2100H)									! Rastro en SJ
 Gr.Ajuste.Proy% = 0																											! Proyecto cancelado
EndIf Else Begin																													! 
 If End #94 THEN UE.AJU.FINAL		       																	  ! Si es EOF
  While (1)															  																! Recorre archivo
    Read #94; TS.TEMP1$			       																				! Lectura registro
    If TS.TEMP1$ = "[AJUSTE AL CAMBIO]" Then Begin						   					! Proyecto de Lealtad
     Read #94; TS.TEMP1$																									! Lectura registro
     Gr.Ajuste.Proy%      = Val(Mid$(TS.TEMP1$,30,2))          				    ! Proyecto activo
     Read #94; TS.TEMP1$																									! Lectura registro
     UE.AJUS.REDO%      = Val(Mid$(TS.TEMP1$,30,2))            				    ! Valor base ajuste
     Read #94; TS.TEMP1$																									! Lectura registro
     UE.MNTO.DPT$       = Str$(Val(Mid$(TS.TEMP1$,30,2)))				          ! Grupo de Descuento
     UE.MNTO.DPT$       = Right$("00"+UE.MNTO.DPT$,2)                     ! Ajuste numero dpto
     Read #94; TS.TEMP1$																									! Lectura registro
	   UE.MNTO.DESC$      = Mid$(TS.TEMP1$,30,20)									          ! Descriptor
	   UE.MNTO.DESC$ = LEFT$(UE.MNTO.DESC$+STRING$(20," "),20)              ! Ajusta descriptor
     GoTo UE.AJU.FINAL																									  ! Sale del ciclo de cargau
   EndIf
 WEnd
 UE.AJU.FINAL:
   Close 94																																! Cierra archivo
   If Gr.Ajuste.Proy% = -1 Then                                          \! Proyecto Activo
   Call U.IMPRIME("MOD. AJUSTE AL CAMBIO  ON 24/Abr/2012",6100H) Else    \! Msg Proyecto Cargado
   Call U.IMPRIME("MOD. AJUSTE AL CAMBIO OFF 24/Abr/2012",6100H)          ! Msg Proyecto Cargado
EndIf

EndIf

If Gr.Ajuste.Proy% <> -1 Then Exit Sub                                    ! Proyecto Cancelado


!EAMTSU20.J86
If Xopt% = 20 Then Begin
 If (UE.AJUS.MONTO% > 0) AND (TS.LINETYPE = 2)  THEN BEGIN
  Call Format.AMOUNT((UE.AJUS.MONTO% * -1))
 	TS.TEMP1$ = STRING$(8," ")+UE.MNTO.DESC$+"      "+TS.TEMP1$
  Call U.Imprime(TS.TEMP1$,6100H)
  UE.AJUS.MONTO% = 0
 EndIf
	
EndIf

!EAMTSU30.J86
If Xopt% = 30 Then Begin
	
If TS.BALDUE(0) <= 0 THEN BEGIN
   TS.TEMP4I4 = 0
   For J% = 1 TO 6
	    TS.TEMP4I4 = TS.TEMP4I4 + TS.TEndERED(J%)
   Next J%
   If (TS.TOTALS(0,0,0) <> TS.TEMP4I4) THEN BEGIN
    If UE.AJUS.REDO% <= 0 THEN GOTO SALIDA.AJUSTE.CAMBIO
    If MOD(TS.TOTALS(0,0,0),UE.AJUS.REDO%) > 0 THEN	BEGIN
	     TS.TEMP4I4 = TS.TEMP4I4 - TS.TOTALS(0,0,0)
       UE.AJUS.MONTO% = MOD(TS.TEMP4I4,UE.AJUS.REDO%)
	     If UE.AJUS.MONTO% <> 0 THEN BEGIN
		      UE.AJUS.MONTO% = UE.AJUS.REDO% - UE.AJUS.MONTO%
		      SL.STR$(SL.End+1) = PACK$("03")+":"+PACK$(UE.MNTO.DPT$)+":"+	\
				                      PACK$("00")+ ":"+PACK$(STR$(UE.AJUS.MONTO%))+":"
		      TS.BALDUE(0) = TS.BALDUE(0) - UE.AJUS.MONTO%
		      SL.HD.GROSSNEG = SL.HD.GROSSNEG + UE.AJUS.MONTO%
		      TS.GROSSNEG = TS.GROSSNEG + UE.AJUS.MONTO%
		      SL.End = SL.End + 1
		   EndIf
	   EndIf
   EndIf
EndIf

SALIDA.AJUSTE.CAMBIO:

EndIf

End Sub 
