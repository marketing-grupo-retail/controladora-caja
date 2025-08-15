!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : CLTMEXIT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   *
!Fecha         : Septiembre 25 de 2.007           *
!Observaciones : Modulo Registro de Puntos y Dscto*
!                por tipo de Cliente              *
!**************************************************

%ENVIRON T		                 																						! Ambiente de terminal

Integer*1   Global   GR.CLT.CTRL%        																	! Control del Proyecto
Integer*4   Global   GR.OP.PPAMT     																			! POINTS PER AMT
String      Global   Gr.Lcl.TpClte$                                       ! Tipo de Cliente Modulo Lealtad OnLine

%INCLUDE EAMTSEVA.J86          																						! Variables EMS 
%INCLUDE EAMTSWKG.J86          																						! Variables EMS 


Sub TSHIECET External								   																		!  Tono de alerta
End Sub

Sub TSU07.FB External          																						! Llamado Parametros EMS
End Sub

Function  VISORES4690(U.D.VISOR%, U.D.LINEA1$, U.D.LINEA2$,U.D.TIEMPO%,U.D.POSICION$) External ! Msg Display
 String U.d.Linea1$, U.d.Linea2$, U.d.Posicion$						! Variables de
 Integer*2 U.d.visor%, U.d.tiempo%   							! trabajo
 String U.D.Linea1$, U.D.Linea2$		
End Function
!--- Fin manejo de visores

Function U.IMPRIME(U.CADENA$,U.STATION) External
   String U.CADENA$
   Integer*2 U.STATION
End Function 

Sub TIPO.CLIENTE(STATE$)
String STATE$, FINI$, FFIN$, TAPPL$, TPTOS$, TGRPO$, LLAVE$
 TS.ER.RETURN = -1 																									! Control Errores
 CA.DISCG = 00         																			        ! ASOCIO GRUPO DESCUENTO 
 OP.PPAMT = GR.OP.PPAMT																			        ! Dato Original
 Open "R::$IPOCLTE" KEYED RECL 10 AS 94 UNLOCKED NOWRITE NODEL      ! Open Tipo de clientes
 If TS.ER.RETURN = -1 Then Begin 																	  ! Si apertura OK
    LLAVE$ = Ucase$(Left$(STATE$,1)) 															  ! Toma tipo de cliente
    Read Form "C1 2C3 3C1";#94 KEY LLAVE$;                         \! Lee Tipo de Cliente
    LLAVE$, FINI$, FFIN$, TAPPL$, TPTOS$, TGRPO$                    !
    If TS.ER.RETURN = -1 Then Begin 																! Encontro Tipo de Cliente
 		   If Val(Unpack$(FINI$)) <= Val(DATE$) And	\                   ! Si se encuentra entre el rango
		      Val(Unpack$(FFIN$)) >= Val(DATE$) Then Begin              ! de fechas parametrizado
		      If TAPPL$ = "1" Then Begin 																! Si aplicacion de Puntos
		         OP.PPAMT = Val(TPTOS$)																	! Aplicacion de puntos 
		      EndIf Else Begin 																					! Si aplicacion de descuento
		         CA.DISCG = Val(UNPACK$(TGRPO$))												! Asigna grupo de descuento
		      EndIf 																										! Fin tipo de aplicacion 
       EndIf 
    EndIf 
    Close 94 																												! Cierra Tipo de Cliente
    Exit Sub 																												! Regresa Control Programa
 EndIf
End Sub 

Sub LECTURA.CLIENTE
String    GR.DATA$, STATE$, LLAVE$

 CA.DISCG = 00         																  						! ASOCIO GRUPO DESCUENTO 
 OP.PPAMT = GR.OP.PPAMT																	  					! Dato Original          
 TS.ER.RETURN = -1 																									! 
 STATE$ = Gr.Lcl.TpClte$																						! Entrega tipo de cliente
 Call TIPO.CLIENTE(STATE$)																					! Tipo de cliente
 Exit Sub 

!--- Se anula la ejecucion de estas lineas
!--- por implementacion del modulo de lealtad en linea
  
 Open "R::$AMFBCUS" KEYED RECL 254 AS 94 UNLOCKED NOWRITE NODEL     ! Open enrollment file
 If TS.ER.RETURN = -1 Then Begin 												            ! Apertura OK
    Read Form "C254";#94 KEY CA.CUSTNUM$; GR.DATA$
     If TS.ER.RETURN = -1 Then Begin 																			! Lectura OK
        Close 94 																													! Cierre archivo FBCUS
        STATE$  = MID$(GR.DATA$,111,2)																		! Toma tipo de cliente
        Call TIPO.CLIENTE(STATE$)																					! Tipo de cliente
     EndIf Else Begin 																										! Fallo Lectura
        Close 94 																													! Cierre archivo FBCUS
     EndIf 
  EndIf 
End Sub 


!-------
!-- Control de Ejecucion User Exits
!-------

Sub CONTROL.CLIENTE(U.EXIT%) Public																				! Rutinas Control de Clientes
Integer*4 U.Exit%

If U.Exit% = 2 Then Begin 																								! Ejecucion User Exit 2
 If GR.CLT.CTRL% = -1 Then Begin																					! Proyecto Activo
   Call TSU07.FB              				 																		! Read options EMS
   GR.OP.PPAMT = OP.PPAMT																								  ! Dato Original
 EndIf 																																		! Fin proyecto activo
EndIf 																																		! Fin UE 2

If U.Exit% = 7 Then Begin 																								! Ejecucion User Exit 7
TS.ER.RETURN = -1																													! Ctrl Errores
OPEN "R::ASCNTRL" AS 94																										! Apertura archivo parametrizacion
If TS.ER.RETURN <> -1 Then Begin																					! Error en la apertura
 Call TSHIECET																														! Tono de Alerta
 Call Visores4690(1,"Error Parametros","Control Clientes   ",1500,"L")		! Msg Alerta
 Call Visores4690(1,"Control Clientes   ","No Operativo",1500,"L")				! Msg Alerta
 Call U.Imprime("Error Control Clientes  ",2100H)		        							! Rastro en SJ
 GR.CLT.CTRL%   = 00 																									    ! Proyecto desactivado
 GoTo Salida.CtrlCltes																										! Salida de la carga
EndIf Else Begin									      																	! Apertura OK
 If End #94 Then Ctrl.Carga.Clte      																	  ! Si es EOF
  While (1)															  																! Recorre archivo
    Read #94; TS.TEMP1$			       																				! Lectura registro
    If TS.TEMP1$ = "[CONTROL CLIENTES]" Then Begin			  		   					! Proyecto de Control Clientes
     Read #94; TS.TEMP1$																									! Lectura registro
     GR.CLT.CTRL%   = Val(Mid$(TS.TEMP1$,30,2))				    	    					! Proyecto activo
     GR.OP.PPAMT = OP.PPAMT																								! Dato Original
     GoTo Ctrl.Carga.Clte																									! Sale de la carga 
    EndIf																																	!
 Wend																																			!
EndIf 																																		! Fin Apertura Control
Ctrl.Carga.Clte:																													!
   Close 94																																! Cierre Archivo de Carga
Salida.CtrlCltes:																													!
   If GR.CLT.CTRL% = -1 Then                                                   \! Proyecto Activo
      Call U.IMPRIME("MODULO PTOS/DSCT CLTE  ON 30-Ago-2011",2100H) Else     \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO PTOS/DSCT CLTE OFF 30/Ago/2011",2100H)           ! Msg Proyecto Cargado

EndIf 																																		! Fin UE 7

If U.Exit% = 101 Then Begin 																							! Ejecucion User Exit 1 EMS
If GR.CLT.CTRL% = -1 Then \       																			  ! Proyecto Activo
 If USER.FBACT.READ Then Begin                    												! Si CLF capturado
    Call LECTURA.CLIENTE																									! Busqueda Cliente en EAMFBCUS
 EndIf 																																		! Fin lectura cliente
EndIf 																																		! Fin UE EMS 1


End Sub

