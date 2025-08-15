!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : REMISION.BAS                     *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Diciembre de 2.009               *
!Observaciones : Modulo Generacion consecutivo    *
!                de remisiones                    *
!**************************************************

%ENVIRON T		                          ! Ambiente de terminal

Integer*1 Global  Gr.Crd.Fiscal%,      \! Si maneja numeracion fiscal
                  Ue.Cnv.Fiscal%,      \! Si maneja numeracion fiscal
                  Gr.Crd.ValData%,     \! Validacion de datos 
                  Com.Fde.Sesion%
                  
Integer*4 Global  Gr.Crd.Remision%      ! Numero de remision por terminal

String    Global  NITW$

%INCLUDE ADX_UPGM:EAMTSWKG.J86          ! working storage
%INCLUDE ADX_UPGM:EAMTRANS.j86          ! Variables procesos de transaccion
%INCLUDE ADX_UPGM:EAMTERMS.J86          ! Variables procesos terminal

%INCLUDE RECATSSU.011        						! Rutinas Genericas

Sub Asignacion.Consecutivo.Remision Public 
String XKey$, Xnull$, XStat$
TS.ER.RETURN = -1
Gr.Crd.Remision% = 1
Open "R::$F:NUMREM" Keyed Recl 7 As Com.Fde.Sesion% 
If TS.ER.RETURN <> -1 Then Begin
   Gr.Crd.Remision% = 1 
   Exit Sub 
EndIf
Xkey$ = Pack$(Right$("0000"+TS.TERMINAL$,4))
Read Form "C2 C5";#Com.Fde.Sesion% Key XKey$ ; Xnull$, XStat$
If TS.ER.RETURN = -1 Then Begin 
	 Gr.Crd.Remision% = Val(UnPack$(XStat$)) + 1  ! Aumenta consecutivo
	 Xstat$ = Pack$(Right$("0000000000"+Str$(Gr.Crd.Remision%),10))
   Write Form "C2 C5";#Com.Fde.Sesion%;  \! Almacena consecutivo de remision
            XKEY$, XStat$
EndIf Else Begin 
	Call VISOR.AND.BORRAR("TERMINAL SIN CONSECUTIVO DE REMISION")
  Gr.Crd.Remision% = 1 
EndIf 
Close Com.Fde.Sesion%
End Sub 

Sub VALIDAR.EMPRESA.CREDITO(XDATA$)
String XDATA$, Xkey$, Xnull$, XStat$
TS.ER.RETURN = -1
Gr.Crd.Fiscal% = -1
Open "R::$F:CTLCRD" Keyed Recl 9 As Com.Fde.Sesion% UNLOCKED NOWRITE NODEL
If TS.ER.RETURN <> -1 Then Begin 
	 Call VISORES4690(1,"FALLA OPEN TFCTLCRD","",1200,"L")
	 Exit Sub 
EndIf
Xkey$ = Pack$(Right$("0000000000000000"+Xdata$,16))
Read Form "C8 C1";#Com.Fde.Sesion% Key XKey$ ; Xnull$, XStat$
If TS.ER.RETURN = -1 Then Begin ! Nit encontrado
	If Xstat$ = "0" Then Begin 
		 Gr.Crd.Fiscal% = 0 
		 Ue.Cnv.Fiscal% = -1 
  EndIf 
EndIf 
Close Com.Fde.Sesion%
End Sub

Sub REMISION(XUE%) Public
Integer*2 Xue%
String Xtmp$

If Xue% = 2 Then Begin 
	Gr.Crd.ValData% = 0 
	Ue.Cnv.Fiscal%  = 0
	Gr.Crd.Fiscal%  = -1
EndIf 

If Xue% = 7 Then Begin 
	Gr.Crd.ValData% = 0 
	Ue.Cnv.Fiscal%  = 0
	Gr.Crd.Fiscal%  = -1
  Call U.IMPRIME("MODULO REMISIONES      ON 30/Ago/2011",2100H)           ! Msg Proyecto Cargado

EndIf 


If Xue% = 14 Then Begin 
 If Gr.Crd.ValData% = 0 Then Begin 
 	If Val(NITW$) <> 0 Then Begin 
    Xtmp$ = NITW$
    Call VALIDAR.EMPRESA.CREDITO(XTMP$)
    Gr.Crd.ValData% = -1
    If Ue.Cnv.Fiscal% = -1 Then Begin 
    	 TS.TEMP1$ = Pack$("01")     +   \! Numero remision generada
                   ":"+Pack$("00")      ! Relleno de registro
       Call Grabacion.String.Usuario2("48",TS.TEMP1$) ! Almacena User Data 
    EndIf 
  EndIf 
 EndIf 
EndIf 


!--- Ejecucion en EAMTSU20
If Xue% = 20 Then Begin 
   If Gr.Crd.Fiscal% = -1 Then Exit Sub         ! Si numeracion fiscal sale del proceso	
   If Not TS.TRAINING Then Begin                ! Si no en entrenamiento  
    If TS.INTRX Then                           \! Si esta en una compra   
      If TS.LINETYPE = 6 And                   \! Store Data y Transacc   
         TS.LINEDATA = 1 Then Begin             ! Fecha, Hora, etc        
          If TS.TENDERED (0) <> 0 Or           \! Si hay pagos            
             TS.TRX.STATUS <> 100 Then Begin    ! y NO hay VOID en curso  
             Call U.IMPRIME("  CONSTANCIA DE ENTREGA DE CREDITOS  ",6100H)
             Call Asignacion.Consecutivo.Remision
             TS.TEMP1$ = Right$("0000000"+STR$(Gr.Crd.Remision%),07)
             TS.TEMP2$ = "   REMISION NRO: "+TS.STORE$+"-"+TS.TERMINAL$+"-"+TS.TEMP1$
             TS.TEMP2$ = Left$(TS.TEMP2$ + STRING$(40," "),40)
             Call U.Imprime(TS.TEMP2$,6100H)                
             
!             TS.TEMP1$ = Pack$(Str$(Gr.Crd.Remision%)) +   \! Numero remision generada
!		                     ":"+Pack$("00")                    ! Relleno de registro
!		         Call Grabacion.String.Usuario2("45",TS.TEMP1$) ! Almacena User Data 
		         
		         TS.TEMP1$ = Right$("0000000000000000"+STR$(Gr.Crd.Remision%),16)
		         
             TS.TEMP2$ = "TR+"                            + \! Arma Str Usuario con
                 "REMI"+Pack$(TS.TEMP1$)+":"+    + \!
                 STR$(ABS(TS.GROSSPOS + TS.GROSSNEG))     + \! #Fact, Vr/Vta Neto
                 ":" + Pack$("00") + ":" + Pack$("00")    + \! y Vrs de Iva
                 ":" + Pack$("00") + ":" + Pack$("00")    + \!
                 ":" + Pack$("00") + ":" + Pack$("00")    + \!
                 ":" + Pack$("00") + ":" + Pack$("00")       !

             Call Grabacion.Cadena.Usuario("21",TS.TEMP2$)
		         
		         
		         
          EndIf 
      EndIf 
   EndIf        
   	                                                                             
EndIf 

End Sub 
