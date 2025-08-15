!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMIMEI.BAS                      *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Captura Imei venta celulares     *
!**************************************************
!Observaciones:
! Version inicial 13/Feb/2018
!-------------------------------------------------
! Mod 25Jun2020
! Se adiciona control para no captura de IEMI si esta
! procesando una venta del dia Sin iva
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%ENVIRON T		                          																			! Ambiente de terminal

Integer*1 Global  Gr.Imei.Ok%, Gr.Imei.Trx%
String    Global  Gr.Imei.Dpto$, Gr.Imei.Capt$, IR.USERDATA$
Integer*4 Global  Asc.Tmp.Apun%
Integer*1 Global  Gr.NoTax.Proc%
Integer*1 Global  Gr.NtaCrd.InTrx%																					! Control Nta Crd

%INCLUDE EAMTSWKG.J86          																							! working storage
%INCLUDE EAMTOPTS.J86          																							! Terminal Options
%INCLUDE EAMITEMR.J86          																							! Maestro de articulos 
%INCLUDE EAMTRANS.J86          																							! Variables de transancion
%INCLUDE EAMTSEVA.J86          																							! Variables Cliente Frecuente

%INCLUDE RECATSSU.011        																								! Rutinas Genericas Grupo Retail

Sub GRMIMEI(XOPT%) Public		  																					    ! Captura Imei Telefono
Integer*4 Xopt%                                                             !
String    Xdpto$

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.Imei.Ok%  = 0                                                          ! Proyecto Apagado
  TS.ER.RETURN = -1																													! Ctrl Errores                     
  OPEN "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL														! Apertura archivo parametrizacion 
  If TS.ER.RETURN <> -1 Then BEGIN                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA ARCHIVO PARAMETROS SISTEMA")
  	 Exit Sub 
  ENDIF 
  IF END #94 THEN UE.FIN.CAPIMEI        																    ! Si es EOF                        
  While (1)															  															    ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        IF TS.TEMP1$ = "[CAPTURA IMEI]" Then Begin		   		   							! Captura Imei
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Imei.Ok%   = Val(Mid$(TS.TEMP1$,30,2))      				    			  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						! Lectura registro
         Gr.Imei.Dpto$ = Mid$(TS.TEMP1$,30,100)         				            ! Lista departamentos para captura 
         GoTo UE.FIN.CAPIMEI																								! Sale del ciclo de carga          
       Endif                                                                !
   Wend                                                                     !
   UE.FIN.CAPIMEI:                                                          !
     Close 94																																! Cierra archivo
   If Gr.Imei.Ok% = -1 Then                                                \! Proyecto Activo
      Call U.IMPRIME("MODULO CAPTURA IMEI    ON 25/Jun/2020",2100H) Else   \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO CAPTURA IMEI   OFF 25/Jun/2020",2100H)         ! Msg Proyecto Cargado
EndIf 

If Gr.Imei.Ok% <> -1 Then Exit Sub                                          ! Si proyecto apagado

!--- EAMTSU02.J86
If Xopt% = 2 Then Begin                                                     ! Inicializacion de trx
	 Gr.Imei.Capt$ = ""
	 Gr.Imei.Trx% = 0 
EndIf 

!--- EAMTSU08.J86
If Xopt% = 8 Then Begin                                                     ! En validacion de producto
   Xdpto$ = LEFT$(IR.USERDATA$,16)        																	! DEPARTAMENTO COMERCIAL
   Xdpto$ = ";"+Right$(Xdpto$,15)+";"
   If match(Xdpto$,Gr.Imei.Dpto$,1) > 0 Then Begin 											  	! Es un Item con captura imei
      If Gr.NoTax.Proc% <> 0 Then Begin																			! En simulacion
      	 Exit Sub 																													! No captura de nuevo el imei
      EndIf
      If Gr.NtaCrd.InTrx% <> 0 Then Exit Sub                                ! En proceso nota crd
   	  If Val(TS.IO.DATA$(6)) > 1 Then Begin																	! Reporta mas de una unidad vendida
   	  	 Call Visor.and.borrar("NO SE PERMITE VENTA POR CANTIDAD")          !
   	  	 IR.INDICAT0 = IR.INDICAT0 OR 04H			                              ! No permite venta
   	  	 Exit Sub 
   	  EndIf 
   	
      TS.TEMP5$ = ASIC.DATOS$("CAPTURE IMEI DEL  ","DISPOSITIVO ...   ")    !
      If TS.TEMP5$ = "E" Then BEGIN 																		    ! Proceso cancelado   
         Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								    !
         IR.INDICAT0 = IR.INDICAT0 OR 04H												           	! No permite venta
         Gr.Imei.Capt$ = ""																									!
 	       Exit Sub 																													! Sale del proceso
      EndIf																																	!
      
      If Len(TS.TEMP5$) <> 15 Then Begin																		! Si longitud erronea
   	  	 Call Visor.and.borrar("IMEI CAPTURADO NO ESVALIDO       /Borrar")  !
         IR.INDICAT0 = IR.INDICAT0 OR 04H												           	! No permite venta
         Gr.Imei.Capt$ = ""																									!
 	       Exit Sub 																													! Sale del proceso
      EndIf																																	!
      
      Gr.Imei.Capt$ = TS.TEMP5$																							! Imei Capturado
   EndIf																																		! Fin captura Imei
EndIf 

!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                    ! En validacion de producto
	
  If TS.IO.KEYS(1) = 82 AND \                       												! Proceso de suspension de
     TS.IO.KEYS(6) = 81 AND \                       												! Transaccion con una NP
     TS.PROCEDURE < 1 Then Begin                    												! Capturada
    If Gr.Imei.Trx% = -1 Then Begin        																	! Imei Capturado
      Call Visor.And.Borrar("NO SE PERMITE SUSPENDER TRANSACCION")					!
      TS.IO.MOTORKEY = 0                            												! Inicializa secuencia de 
      Dim TS.IO.KEYS(10) : Dim TS.IO.DATA$(10)      												! Tecleo
      Exit Sub 																															!
    EndIf      
  EndIf
EndIf


!--- EAMTSU60.J86
If Xopt% = 60 Then Begin                                                    ! Impresion CR
 If TS.LINETYPE = 29 Then Begin                                             \! Es un articulo
 	  If Len(Gr.Imei.Capt$) > 0 Then Begin																		! Imprime Imei en el tiquete
       To.USEREXIT(20) = 0 
       To.USEREXIT(60) = 0 
 	  	 TS.TEMP2$ = Left$("IMEI:"+Gr.Imei.Capt$+String$(37," "),37)          ! Ajusta dato 
 	  	 Call U.Imprime(TS.TEMP2$,6100H)													            ! Dato en el tiquete
       TS.TEMP1$ = Pack$(SL.IT.ITEMCODE$)                  + ":" + 				 \! SKU Vendido
                   Gr.Imei.Capt$                           + ":" +         \! Imei Reportado
                   "00"																											!
       Call Grabacion.Cadena.Usuario2("20180213", TS.TEMP1$)                ! Almacena String
 	  	 Gr.Imei.Capt$ = ""																										! Limpia Variable
 	  	 Gr.Imei.Trx% = -1																										! En trx 
       To.USEREXIT(20) = -1
       To.USEREXIT(60) = -1
 	  EndIf																																		! 
 EndIf
EndIf 																																			! Fin Impresion CR

!--- EAMTSU53.J86
If Xopt% = 53 Then Begin                                                   	! Recuperacion de trx
   If UnPack$(LEFT$(SL.STR.ENTRY$,1)) = "99" Then Begin              	    	! Si DATA/ENTRY
    If UnPack$(MID$(SL.STR.ENTRY$,3,4)) = "20180213" Then Begin             ! Imei Capturado
       Asc.Tmp.Apun% = 3
       TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                ! codigo proyecto
       TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                ! Sku vendido
       TS.TEMP1$ = ASIC.GETUNPK4(SL.STR.ENTRY$,Asc.Tmp.Apun%)               ! Imei reportado
       Gr.Imei.Capt$ = TS.TEMP1$
       Gr.Imei.Trx% = -1
    EndIf
   EndIf
EndIf 

!--- EAMTSU56.J86
If Xopt% = 56 Then Begin                                                   	! En suspension de trx
   Gr.Imei.Capt$ = ""
   Gr.Imei.Trx%  = 0
EndIf


End Sub 
