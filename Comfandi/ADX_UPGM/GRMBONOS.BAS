!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMBONOS.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Manejo bonos Dinero Comfandi     *
!**************************************************
!Observaciones:
!Mod 3 May 2012
!Se adiciona la funcion para calculo de digito de 
!chequeo del bono capturado
!desarrollado por Grupo Retail
!--------------------------------------------------
!Mod 13 Ago 2012
!Se ajusta el modulo para permitir lo siguiente:
!1. Permitir un maximo de 30 bonos por trx
!2. Controlar la anulacion del bono dentro de la trx
!   validando correctamente la cedula del cliente
!Desarrollado por Grupo Retail - OVS 
!--------------------------------------------------
! Mod 27 Feb 2014
!Se ajusta el modulo para controlar el proceso de 
!registro del bono validando que el cajero antes 
!validar el bono haya presionado la tecla TOTAL a la
!aplicacion e caja
!Desarrollado por Grupo Retail - OVS 
!--------------------------------------------------
! Mod 18/Feb/2020
! Se ajusta aplicación de pago para evitar errores
! en procesos de autorización de supervisor.
!Desarrollado por Grupo Retail - OVS 
!--------------------------------------------------

%ENVIRON T		                          																			! Ambiente de terminal

Integer*1 Global  Gr.Bonos.Ok%, Gr.Bono.Apl%, Asic.Device%, Gr.Bonos.Sgn%			!
Integer*4 Global  GR.BONOS.SALDO%, GR.BONOS.CUPO%, Asc.Tmp.Apun%				  		! Numero de remision por terminal
Integer*4 Global  Gr.Bono.Slend%, Gr.Bonos.Qty%
String    Global  Gr.Bonos.Key$, Gr.Bonos.Num$, Gr.Bonos.Id$, Gr.Bonos.Estado$, \
                  Gr.Bonos.Vlr$, Gr.Bonos.Lista$(2)
Integer*2 Global GR.BLNEGCNT(1)                                               !
Integer*4 Global Gr.Bonos.TVPOS%,                                            \!
                 Gr.Bonos.MaxTv%,                                            \! Maximo pago   tipo y variedad
                 Gr.Bonos.ChgTv%,                                            \! Maximo cambio tipo y variedad
                 GR.BLNEGAMT(1) 					  																	!

%INCLUDE EAMTSWKG.J86          																								! working storage
%INCLUDE EAMTOPTS.J86          																								! Terminal Options
%INCLUDE EAMITEMR.J86          																								! Maestro de articulos 
%INCLUDE EAMTRANS.J86          																								! Variables de transancion
%INCLUDE EAMTSEVA.J86          																								! Variables Cliente Frecuente
%INCLUDE EAMTSXHC.J86          																								! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          																								! Rutinas de Assembler
%INCLUDE RECATSSU.011        																									! Rutinas Genericas Grupo Retail

Function FORMAT.AMOUNT(AMT1) External 																				! Formateo de valores
 Integer*1 FORMAT.AMOUNT
 Integer*4 AMT1
End Function

Sub TSBDEC01 EXTERNAL          ! balance due (normal/foodstamp)
End Sub                        !

Sub Bono.Digito.Chequeo Public
Integer*4 DATO(2), XTOTAL%, XH%, XT%, XJ%, XI%
String XDATO$, XDG$

DIM DATO(12,1)
DATO(01,0)=1:DATO(02,0)=3:DATO(03,0)=1:DATO(04,0)=3
DATO(05,0)=1:DATO(06,0)=3:DATO(07,0)=1:DATO(08,0)=3
DATO(09,0)=1:DATO(10,0)=3:DATO(11,0)=1:DATO(12,0)=3
XTOTAL% = 0
XDATO$ = Left$(Gr.Bonos.Num$,12)
XDG$   = Right$(Gr.Bonos.Num$,1)
TS.TEMP1I4 = 0
If Len(XDATO$) = 12 Then BEGIN
  For XI% = 1 to 12
      DATO(XI%,1) = VAL(MID$(XDATO$,XI%,1))
  Next XI%
  For XJ% = 1 to 12
      XTOTAL% = XTOTAL% + (DATO(XJ%,1)*DATO(XJ%,0))
  Next XJ%
  XH% = (10*INT(XTOTAL%/10))+10
  XT% = XH% - XTOTAL%
  If XT% > 9 Then XT% = 0
  If Val(XDG$) = XT% Then TS.TEMP1I4 = -1
EndIf

End Sub


Sub Consulta.BonoPremio Public
String XClte$, Xbono$
Integer*1 Xfound%
Integer*4 XI%, Xpos%

 Xfound% = 0 
 For XI% = 1 To Gr.Bonos.Qty%																									! Buca bonos capturados
   If Gr.Bonos.Lista$(XI%,0) = Gr.Bonos.Num$ Then \                           ! Si lo encontro
   	  Xfound% = -1 : Xpos% = XI%
 Next XI%
 
 If (TS.IO.KEYS(1) = 70) And Xfound% = -1 Then Begin                          ! Anulando y encuentra bono
 	  If Val(Gr.Bonos.Lista$(Xpos%,1)) <> Val(Gr.Bonos.Id$) Then Begin          ! Si no corresponde al cliente
 	     Call VISOR.AND.BORRAR("DATO CLIENTE BONO   NO CORRESPONDE")
       TS.TEMP1I4    = 0 				  																						! Proceso Fallido
       Exit Sub 																															! Sale del proceso
 	  EndIf
    GR.BONOS.CUPO%   = Val(Gr.Bonos.Lista$(XPOS%,2))				  								! Retorna Valor del Bono
    GR.BONOS.SALDO%  = GR.BONOS.CUPO%																					! Valor del Bono
    Gr.Bonos.Estado$ = "E"																										! Estado del Bono
    Gr.Bonos.Lista$(XPOS%,0) = "ANULADO"
    Gr.Bonos.Lista$(XPOS%,1) = "ANULADO"
    Gr.Bonos.Lista$(XPOS%,2) = "0"
    TS.TEMP1I4    = -1 				  																						  ! Proceso Fallido
    Exit Sub 																																	! Sale del proceso
 EndIf
 
 If (TS.IO.KEYS(1) = 70) And Xfound% = 0 Then Begin                           ! Anulando y no encuentra bono
 	 Call VISOR.AND.BORRAR("BONO ANULAR NO EXISTE VENTA")
   TS.TEMP1I4    = 0 				  																						    ! Proceso Fallido
   Exit Sub 																																	! Sale del proceso
 EndIf
 
 If Xfound% <> 0 Then Begin																										! Si lo encontro
 	 Call VISOR.AND.BORRAR("BONO YA CAPTURADO EN COMPRA")
   TS.TEMP1I4    = 0 				  																						    ! Proceso Fallido
   Exit Sub 																																	! Sale del proceso
 EndIf

 Xclte$    = Right$("               "+Gr.Bonos.Id$,15)                				! Numero de Cliente
 Xbono$    = Right$("                    "+Gr.Bonos.Num$,20)                  ! Numero del Bono
 Xbono$    = Xclte$ + Xbono$																									!

 TS.TEMP1$ = Armar.Trama.Msg("15","05",XBONO$,"00","0001","123456")           ! Armar trama MSG
 TS.TEMP2$ = Rutina.Java("com.appl.ApplKernel","threader",TS.TEMP1$)     	    ! Ejecuta Requerimiento

 !TS.TEMP2$ = "1504069200000CONSULTA DE PUNTOS SATISFACTORIA        10000      10000      E"

 
 TS.TEMP1$ = Valida.Rta(TS.TEMP2$)	  																        ! Valida rta entregada 
 If TS.TEMP1$ = "00" Then Begin 																							! Si Ok Proceso
    TS.TEMP1$ = MID$(TS.TEMP2$,12,2)                                          ! Toma status de la Operacion
    If TS.TEMP1$ <> "00" Then Begin		 				   														  ! Si presenta un error, verif. local
    	 Call Visor.And.Borrar(Mid$(TS.TEMP2$,14,40))
       TS.TEMP1I4    = 0 				  																						! Proceso Fallido
       Exit Sub 
    EndIf
    GR.BONOS.CUPO%   = Val(Mid$(TS.TEMP2$,54,11))
    GR.BONOS.SALDO%  = Val(Mid$(TS.TEMP2$,65,11))
    Gr.Bonos.Estado$ = Mid$(TS.TEMP2$,76,1)
    
    !Call U.IMPRIME("CUPO   : "+STR$(GR.BONOS.CUPO%),4100H)
    !Call U.IMPRIME("SALDO  : "+STR$(GR.BONOS.CUPO%),4100H)
    !Call U.IMPRIME("ESTADO : "+GR.BONOS.ESTADO$,4100H)
    !Call U.IMPRIME("Totals : "+STR$(TS.TOTALS(0,0,0)),4100H)
    !Call U.IMPRIME("Baldue : "+STR$(TS.BALDUE(0)),4100H)
    
    If GR.BONOS.SALDO% > TS.BALDUE(0) Then Begin															! vLR Bono superior al subtotal trx
       Call Visor.And.Borrar("NO SE PERMITE CAMBIO CON BONOS")
       GR.BONOS.CUPO%   = 0
       GR.BONOS.SALDO%  = 0
       Gr.Bonos.Estado$ = ""
       TS.TEMP1I4    = 0 				  																						! Proceso Fallido
       Exit Sub 
    ENDIF 
    TS.TEMP1I4    = -1				  																							! Proceso Satisfactorio
 EndIf Else TS.TEMP1I4 = 0 
 
End Sub 

Sub Aplica.Pago.BonoPremio(Xdata1%)																						!
Integer*2 Xdata1%
      TS.BD.DSPPARM = -1                     																	! SET FOR NORMAL BD
      Call TSBDEC01                          																	! FIRST TAKE BAL DUE
      TS.IO.STATE = 10
      Dim Ts.Io.Keys(10)																											! Inicializa Vectores
      Dim Ts.Io.Data$(10)																											! 
      TS.IO.STATE    = 10 																										! En Estado Main
      TS.IO.KEYS(1)  = Xdata1%																								! Para manejo de anulaciones
      TS.IO.DATA$(3) = Right$(Gr.Bonos.Key$,1)                                ! Toma la variedad de pago
      TS.IO.KEYS(3)  = 78																											! Simula la tecla slash
      TS.IO.DATA$(7) = STR$(GR.BONOS.SALDO%)																	! Valor del bono
      TS.IO.KEYS(7)  = 90 + Val(Left$(Gr.Bonos.Key$,1))                       ! Arma secuencia tecleo motor
      TS.IO.MOTORKEY = 90 + Val(Left$(Gr.Bonos.Key$,1))                       ! Arma secuencia tecleo motor
      TS.IO.DATA$(9) = Gr.Bonos.Num$        																	! Numero del Bono
      Gr.Bonos.Sgn%  = 1
      If Xdata1% = 70 Then Gr.Bonos.Sgn% = 0
      Gr.Bono.Apl%     = -1
      If Xdata1% <> 70 Then Begin 
        Gr.Bonos.Qty%    = Gr.Bonos.Qty% + 1 																	! Cantidad de Bonos
        Gr.Bonos.Lista$(Gr.Bonos.Qty%,0) = Gr.Bonos.Num$                      ! Numero del Bono
        Gr.Bonos.Lista$(Gr.Bonos.Qty%,1) = Gr.Bonos.Id$                       ! ID Propietario del Bono
        Gr.Bonos.Lista$(Gr.Bonos.Qty%,2) = STR$(GR.BONOS.SALDO%)              ! Valor del Bono
      EndIf
      To.TENDLIMITS(Gr.Bonos.TVPOS%,0) = 9999999   										        ! Modifica controles forma de pago
      To.TENDLIMITS(Gr.Bonos.TVPOS%,1) = 9999999									            ! Modifica controles forma de pago
      TS.TEMP1I1 = 0
      For TS.TEMP1I1 = 0 To 7
        TO.NEGAMT(TS.TEMP1I1) = 9999999
        TO.NEGCNT(TS.TEMP1I1) = 999
      Next TS.TEMP1I1           
      
End Sub 

Sub Reportar.Bono.Usado
  Integer*4 Venta.Art.Trx, PRIC%, Xvta%                												!
  String X.TMP$, Xpago$                                												!
  String XClte$, Xbono$, Xvlr$																								!
  For I% = 1 TO Gr.Bonos.Qty%                         												! Recorre Bonos
   If Val(Gr.Bonos.Lista$(I%,2)) > 0 Then Begin																! 
   	      
   	      Call VISORES4690(1,"ACTUALIZANDO BONO",Gr.Bonos.Lista$(I%,0),1200,"L")
          Gr.Bonos.Id$  = Gr.Bonos.Lista$(I%,1)																! Id del Cliente
          Gr.Bonos.Num$ = Gr.Bonos.Lista$(I%,0)																! Numero del Bono
          Gr.Bonos.Vlr$ = Gr.Bonos.Lista$(I%,2)					  					     			! Valor del bono
          Xclte$    = Right$("               "+Gr.Bonos.Id$,15)               ! Numero de Cliente
          Xbono$    = Right$("                    "+Gr.Bonos.Num$,20)         ! Numero del Bono
          Xvlr$     = Right$("           "+Gr.Bonos.Vlr$,11)                  ! Valor del Bono
          Xbono$    = Xclte$ + Xbono$	+ Xvlr$																	!
          TS.TEMP1$ = Armar.Trama.Msg("15","06",XBONO$,"00","0001","123456")  ! Armar trama MSG
          TS.TEMP2$ = Rutina.Java("com.appl.ApplKernel","threader",TS.TEMP1$) ! Ejecuta Requerimiento
          
   EndIf																																			!
  Next I%																																			! Fin del ciclo
End Sub 																																			! Fin Reportar Bono

Sub BL.BUSCA.LIMITE.TV(UE.TIPO.VAR$)
  Integer*2 UE.I%
  Integer*1 UE.FOUND%
  String UE.TIPO.VAR$
  UE.I% = 1
  UE.FOUND% = 0 
  Gr.Bonos.MaxTv% = 0
  Gr.Bonos.ChgTv% = 0 
  While UE.I% <= TO.NUMTNDR AND NOT UE.FOUND%
    If TO.TENDOPTS(UE.I%,0) = VAL(LEFT$(UE.TIPO.VAR$,1)) AND  \
       TO.TENDOPTS(UE.I%,1) = VAL(RIGHT$(UE.TIPO.VAR$,1)) Then Begin 
       Gr.Bonos.MaxTv% = TO.TENDLIMITS(UE.I%,0)
       Gr.Bonos.ChgTv% = TO.TENDLIMITS(UE.I%,1)
       UE.FOUND%  = -1
       Gr.Bonos.TVPOS% = UE.I%
    EndIf Else UE.I% = UE.I% + 1
  Wend    
  UE.I% = 0
  For UE.I% = 0 To 7
      GR.BLNEGCNT(UE.I%) = To.NEGCNT(UE.I%)    
      GR.BLNEGAMT(UE.I%) = To.NEGAMT(UE.I%)    
  Next UE.I%
End Sub



Sub GRMBONOS(XOPT%) Public																							    	! Captura remisiones
Integer*4 Xopt%, Xpos%
Integer*1 Xfound%                                                             !

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                       ! Carga de parametros
  Gr.Bonos.Ok%  = 0                                                           ! Proyecto Apagado
  Dim Gr.Bonos.Lista$(100,2)																									!  Maximo 100 bonos x trx
  Gr.Bonos.Qty% = 0
  TS.ER.RETURN = -1																													  ! Ctrl Errores
  Dim GR.BLNEGCNT(8)
  Dim GR.BLNEGAMT(8)
  Open "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL               							  ! Apertura archivo parametrizacion
  If TS.ER.RETURN <> -1 Then BEGIN                                            !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA ARCHIVO CONTROL REMISION")
  	 Exit Sub 
  ENDIF 
  IF END #94 THEN UE.FIN.GRBONO         																	    ! Si es EOF                        
  While (1)															  																    ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																				! Lectura registro                 
        IF TS.TEMP1$ = "[BONOS PREMIO]" Then Begin		   		   					      ! Bonos Premio 
         Read #94; TS.TEMP1$																									! Lectura registro                 
         Gr.Bonos.Ok%   = Val(Mid$(TS.TEMP1$,30,2))      			    					  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						  ! Lectura registro                 
         Gr.Bonos.Key$  = Mid$(TS.TEMP1$,30,02)        			                  ! Tipo y Variedad Pago Bonos
         Call BL.BUSCA.LIMITE.TV(Gr.Bonos.Key$)                               ! Parametros Bono Lealtad
         GoTo UE.FIN.GRBONO 																								  ! Sale del ciclo de carga          
       Endif                                                                  !
   Wend                                                                       !
   UE.FIN.GRBONO:                                                             !
     Close 94																																  ! Cierra archivo
   If Gr.Bonos.Ok% = -1 Then                                                 \! Proyecto Activo
      Call U.IMPRIME("MODULO CONTROL BONOS   ON 18/Feb/2020",2100H) Else     \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO CONTROL BONOS  OFF 18/Feb/2020",2100H)           ! Msg Proyecto Cargado
EndIf 																																				! Final Eamtsu07

If Gr.Bonos.Ok% <> -1 Then Exit Sub                                           ! Si proyecto apagado

!--- EAMTSU02.J86
If Xopt% = 2 Then Begin                                                       ! Inicializacion de trx
   Call Reportar.Bono.Usado
   GR.BONOS.SALDO%  = 0 
   GR.BONOS.CUPO%   = 0 
   Gr.Bonos.Num$    = ""
   Gr.Bonos.Id$     = ""
   Gr.Bonos.Estado$ = ""
   Gr.Bono.Apl%     = 0 
   Gr.Bono.Slend%   = 0
   Gr.Bonos.Qty%    = 0 
   Dim Gr.Bonos.Lista$(100,2)																									! Maximo 100 bonos x trx
   
   To.TENDLIMITS(Gr.Bonos.TVPOS%,0) = Gr.Bonos.MaxTv%										! Retorna valores default
   To.TENDLIMITS(Gr.Bonos.TVPOS%,1) = Gr.Bonos.ChgTv%										! Retorna valores default
   TS.TEMP1I1 = 0
   For TS.TEMP1I1 = 0 To 7
       TO.NEGAMT(TS.TEMP1I1) = GR.BLNEGAMT(TS.TEMP1I1)
       TO.NEGCNT(TS.TEMP1I1) = GR.BLNEGCNT(TS.TEMP1I1)
   Next TS.TEMP1I1
   
   
EndIf 																																				! Final Eamtsu02

!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                      ! Secuencias de tecleo

   If TS.IO.KEYS(7) > 90 AND TS.IO.KEYS(7) < 97 Then Begin                  	! Si forma de pago 
      TS.TEMP3$ = Str$(TS.IO.KEYS(7) - 90)                                  	! Toma Tipo de pago
      If TS.IO.DATA$(3) = "" Then TS.TEMP3$ = TS.TEMP3$ + "1" Else           \! Toma variedad de pago 
         TS.TEMP3$ = TS.TEMP3$ + TS.IO.DATA$(3)                             	! 
   EndIf Else TS.TEMP3$ = "00"                                                ! No es pago 
   
   If TS.TEMP3$ = Gr.Bonos.Key$ Then Begin																		! Si registro de Bonos
   	
   	If TS.TRX.STATUS <> 1 THEN BEGIN				  							                  ! No se ha ingresado total
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 0 																										! 
       TS.GUIDANCE = 1020																											! Solicita total a la trx
       Exit Sub 
    EndIf 

    If Gr.Bonos.Qty% > 29 Then Begin                                          ! Maximo 30 bonos
    	 Call Visor.And.Borrar("MAXIMO SE PERMITEN  30 BONOS POR COMPRA")
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 73																										! 
   		 Exit Sub 																															!
    EndIf
    	
    TS.TEMP1$ = ASIC.DATOS$("ENTRE CEDULA CLIENTE","")                        ! 
    If TS.TEMP1$ = "E" Then Begin 			  																    ! Si cancela proceso
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 73																										! 
   		 Exit Sub 																															!
    EndIf																																			!
    Gr.Bonos.Id$ = TS.TEMP1$																									! Numero del cliente
    TS.TEMP1$ = ASIC.DATOS$("ESCANEE BONO LEALTAD","")                        ! 
    If TS.TEMP1$ = "E" Then Begin 			  																    ! Si cancela proceso
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 73																										! 
   		 Exit Sub 																															!
    EndIf																																			!
!-- Pendiente activar control Bonos no escanean correctamente
!-- por tipo de impresora utilizada, 
!    If Asic.Device% <> 3 Then Begin 																					! Dato no entro Escaneado
!    	 Call Visor.And.Borrar("NO SE PERMITE INGRESO DEL BONO DIGITADO")
!   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)														  ! Inicializa secuencia de teclado
!   		 TS.IO.MOTORKEY = 73																									! 
!  		 Exit Sub 																															!
!    EndIf

    Gr.Bonos.Num$ = TS.TEMP1$           																			! Numero del Bono
    If Len(Gr.Bonos.Num$) <> 13 Then Begin 																		! Dato Escaneado Invalido
    	 Call Visor.And.Borrar("DOCUMENTO CAPTURADO NO VALIDO PARA BONOS")
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 73																										! 
   		 Exit Sub 																															!
    EndIf
    TS.TEMP1I4 = 0																														! Control de validacion
    Call Bono.Digito.Chequeo																									! Valida digito de chequeo

!-- Para Emulación    
!-- TS.TEMP1I4 = -1																														! Control de validacion
    
    If TS.TEMP1I4 = 0 Then Begin																						  ! Bono no valido
    	 Call Visor.And.Borrar("NUMERO DE BONO ES INVALIDO")                    ! Msg Alerta 
   		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																! Inicializa secuencia de teclado
   		 TS.IO.MOTORKEY = 73																										! 
   		 Exit Sub 																															!
    EndIf																																			!
    TS.TEMP1I4 = 0																														! Control de validacion
    Gr.Bonos.Num$ = Left$(TS.TEMP1$,12)                                       ! Elimina digito de chequeo
    Call Visores4690(1,"VALIDANDO BONO","ESPERE POR FAVOR...",0,"L")          ! Msg de alerta cajero
    Call Consulta.BonoPremio																									! Consulta bono 
    If TS.TEMP1I4 = -1 Then Begin																							! Proceso Satisfactorio
    	 If Ucase$(Gr.Bonos.Estado$) <> "T" And                                \! Si bono parcial o 
    	    Ucase$(Gr.Bonos.Estado$) <> "A" Then Begin 													! Emitido
    	    TS.TEMP1I4 = TS.IO.KEYS(1)
          Call Aplica.Pago.BonoPremio(TS.TEMP1I4)		     											! Aplica premio
       EndIf Else Begin 																											! Si falla en estado de Bono
       	If Ucase$(Gr.Bonos.Estado$) =  "A" Then 														 \! Si esta Anulado el Bono
       		 Call Visor.And.Borrar("BONO TIENE ESTADO ANULADO")
       	If Ucase$(Gr.Bonos.Estado$) =  "T" Then                              \! Usado Totalmente
       		 Call Visor.And.Borrar("BONO USADO EN TOTALIDAD")
    		Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencia de teclado
   		  TS.IO.MOTORKEY = 73																										! 
   		  Exit Sub 																															!
       EndIf 
    EndIf Else Begin																													! Si falla en proceso
    		Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Inicializa secuencia de teclado
   		  TS.IO.MOTORKEY = 73																										! 
   		  Exit Sub 																															!
    EndIf																																			!
   EndIf																																			!
   
EndIf 																																				! Final Eamtsu14
  
!--- EAMTSU20.J86
If Xopt% = 20 Then Begin                                                      ! Secuencias de tecleo
 If (TS.LINETYPE = 18) AND (TS.LINEDATA = 99) THEN BEGIN
  IF (TS.TOTALS(0,0,0) > 0)  THEN BEGIN
     Gr.Bono.Slend% = SL.END																									! Total de strings
  EndIf
 EndIf	

If MATCH(TS.SDESC$(59),TS.PRTBUF$,1) > 0 THEN BEGIN														! Si anulacion total de la trx
   GR.BONOS.SALDO%  = 0 
   GR.BONOS.CUPO%   = 0 
   Gr.Bonos.Num$    = ""
   Gr.Bonos.Id$     = ""
   Gr.Bonos.Estado$ = ""
   Gr.Bono.Apl%     = 0 
   Gr.Bono.Slend%   = 0
   Gr.Bonos.Qty%    = 0 
   Dim Gr.Bonos.Lista$(100,2)																									! Maximo 100 bonos x trx
EndIf																																					!


EndIf																																					! Final Eamtsu20

!-- EAMTSU53.J86
If Xopt% = 53 Then Begin                                                      ! Recupearacion de trx
	
 IF Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "99" THEN BEGIN   	    	              ! Si DATA/ENTRY
  IF Unpack$(MID$(SL.STR.ENTRY$,3,2)) = "4001" THEN BEGIN                     ! Bono Regalo 
    Asc.Tmp.Apun% = 3																													! Apuntador String
    TS.TEMP1$      = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	   	 	 		    ! Numero del proyecto
    Gr.Bonos.Id$   = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	  	 	 		    ! Id del cliente
    Gr.Bonos.Num$  = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	  	 	 		    ! Numero del bono
    Gr.Bonos.Vlr$  = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	  	 	 		    ! Valor del Bono
    Gr.Bonos.Sgn%  = Val(ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%))	 	 		    ! Signo del movimiento
    If Gr.Bonos.Sgn% = 1 Then Begin 																					! Venta de Bono
        Gr.Bonos.Qty%    = Gr.Bonos.Qty% + 1 																	! Aumenta Apuntador
        Gr.Bonos.Lista$(Gr.Bonos.Qty%,0) = Gr.Bonos.Num$                      ! Numero del Bono
        Gr.Bonos.Lista$(Gr.Bonos.Qty%,1) = Gr.Bonos.Id$                       ! ID Propietario del Bono
        Gr.Bonos.Lista$(Gr.Bonos.Qty%,2) = Gr.Bonos.Vlr$                      ! Valor del Bono
    EndIf Else Begin 																													! Anulacion del Bono
        Xfound% = 0 																													! Ctrl de busqueda
        For I% = 1 To Gr.Bonos.Qty%						  															! Buca bonos capturados
          If Gr.Bonos.Lista$(I%,0) = Gr.Bonos.Num$ Then \                     ! Si lo encontro
   	         Xfound% = -1 : Xpos% = I%																				! Encontrado y posicion
        Next I%																															  !
        If Xfound% = -1 Then Begin                          									! Anulando y encuentra bono
           Gr.Bonos.Lista$(XPOS%,0) = "ANULADO"
           Gr.Bonos.Lista$(XPOS%,1) = "ANULADO"
           Gr.Bonos.Lista$(XPOS%,2) = "0"
        EndIf																																	!
    EndIf 																																		!

  Endif
 EndIf	
EndIf																																					! Final Eamtsu53

!-- EAMTSU56.J86
If Xopt% = 56 Then Begin                                                      ! Suspension de trx
   GR.BONOS.SALDO%  = 0 
   GR.BONOS.CUPO%   = 0 
   Gr.Bonos.Num$    = ""
   Gr.Bonos.Id$     = ""
   Gr.Bonos.Estado$ = ""
   Gr.Bono.Apl%     = 0 
   Gr.Bono.Slend%   = 0
   Gr.Bonos.Qty%    = 0 
   Dim Gr.Bonos.Lista$(100,2)																									! Maximo 100 bonos x trx
EndIf 																																				! Final Eamtsu56

!-- EAMTSU60.J86
If Xopt% = 60 Then Begin                                                      ! Secuencias de tecleo
	
 If (TS.LINETYPE = 2 AND TS.LINEDATA = 60) And (Gr.Bono.Apl% = -1) Then Begin ! Si en impresion de forma de pago y dscto aplicado	
      TS.TEMP2$ = Pack$(Gr.Bonos.Id$)                 + 										 \! Id del cliente
                 ":" + Pack$(Gr.Bonos.Num$)           +                      \! Numero del bono
                 ":" + Pack$(Str$(GR.BONOS.SALDO%))   +                      \! Valor del bono
                 ":" + Pack$(Right$("00"+Str$(Gr.Bonos.Sgn%),2)) +           \! Signo del movimiento
                 ":" + Pack$("00")                                            ! Relleno
      Call Grabacion.Cadena.Usuario2("4001",TS.TEMP2$)					  				  	! Almacena User Data
      Gr.Bono.Apl%     = 0 																										! 
      To.TENDLIMITS(Gr.Bonos.TVPOS%,0) = Gr.Bonos.MaxTv%								      ! Retorna valores default
      To.TENDLIMITS(Gr.Bonos.TVPOS%,1) = Gr.Bonos.ChgTv%									    ! Retorna valores default
      TS.TEMP1I1 = 0
      For TS.TEMP1I1 = 0 To 7
         TO.NEGAMT(TS.TEMP1I1) = GR.BLNEGAMT(TS.TEMP1I1)
         TO.NEGCNT(TS.TEMP1I1) = GR.BLNEGCNT(TS.TEMP1I1)
      Next TS.TEMP1I1
 EndIf

EndIf																																					! Final Eamtsu60


End Sub 
