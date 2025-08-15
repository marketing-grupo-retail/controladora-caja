!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMDOMIC.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Domiciliarios             *
!**************************************************
! Modificaciones:
! ----- Version 1.0 10 Noviembre 2023 -------------

%Environ T																																	! Ambiente terminal

Integer*1 Global Gr.Domi.Ok%,                                              \! Proyecto Activo
                 Gr.Domi.Intrx%                                             ! Proceso en trx 
String    Global Gr.Domi.Motora$,                                          \! Tecla motora 
                 Gr.Domi.Lista$(2)                                          ! Lista domicialios
Integer*2 Global Gr.Domi.Ptg%                                               ! Ptg domiciliario
Integer*4 Global IR.PRICE1, IR.PRICE2   																		! Precios producto
Integer*4 Global Asc.Tmp.Apun%																							! Apuntador strings 

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

Sub CARGA.DOMICILIOS																												! Carga lista domiciliarios
Integer*2 XI%
String Xdata1$, Xdata2$, Xdata3$
Dim Gr.Domi.Lista$(10,1)
TS.ER.RETURN = -1
Open "R::$FDOMICI" AS 94 UNLOCKED NOWRITE NODEL 
If TS.ER.RETURN NE -1 Then Begin
   Call VISORES4690(1,"ERROR APERTURA ","DOMICILIOS",1500,"C")
   Gr.Domi.Ok% = 0
   Exit Sub 
EndIf  
If End # 94 Then SALIDA.DOMIC
LEER.DOMIC:
   READ # 94 ; Xdata1$, Xdata2$, Xdata3$
   XI% = Val(Xdata1$)																												! Nro domicilario
   If (XI% > 0 And XI% <= 10) Then Begin																		! Maximo 10 posiciones
     Gr.Domi.Lista$(XI%,0) = Xdata2$																				! Descriptor
     Gr.Domi.Lista$(XI%,1) = Xdata3$																				! Porcentaje
   EndIf
   GoTo LEER.DOMIC
   
SALIDA.DOMIC:
  Close 94

End Sub 																																		!

Sub GRDOMICILIOS(Xopt%) Public 																							! Modulo de domicilios
Integer*4 Xopt%            																								  ! UE ejecutar
String Xtmp$ 
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
        EndIf																																! Fin caga
  Wend 																																			! Fin carga
  UE.FIN.DOMICILIOS:                                                        !
     Close 94																																! Cierra archivo
     Call CARGA.DOMICILIOS																							    ! Carga lista domiciliarios
   If Gr.Domi.Ok% = -1 Then                                                \! Proyecto Activo
      Call U.IMPRIME("MODULO DOMICILIARIOS ON  11-Nov-2023",6100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO DOMICILIARIOS OFF 11-Nov-2023",2100H)          ! Msg Proyecto Cargado
EndIf 																																			! Fin carga de parametros 

If Gr.Domi.Ok% <> -1  Then Exit Sub                                         ! Si proyecto apagado
If TS.STANDALONE       Then Exit Sub	                                      ! Si en operacion TOF

!--- EAMTSU02.J86
If Xopt% = 02 Then Begin                                                    ! Al inicio de trx
   Gr.Domi.Intrx% = 0
   Gr.Domi.Ptg% = 0
EndIf																																				!


!--- EAMTSU08.J86
If Xopt% = 08 Then Begin                                                    ! En validacion Items
   If Gr.Domi.Intrx% = -1 Then Begin																				! Si proceso domiciliario 
   	  TS.TEMP1I4 = ROUND(FLOAT(IR.PRICE1) * (1.+FLOAT(Gr.Domi.Ptg%)/1000.),0,0) !
   	  TS.TEMP2I4 = ROUND(FLOAT(IR.PRICE2) * (1.+FLOAT(Gr.Domi.Ptg%)/1000.),0,0) !
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
      TS.TEMP1$ = ASIC.DATOS$("INGRESE CODIGO DE   ","DOMICILIARIO       ") !
      If TS.TEMP1$ = "E" Then BEGIN 													    			    ! Proceso cancelado   
      	 Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								    !
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
      If Val(Gr.Domi.Lista$(TS.TEMP1I1,1)) <= 0 Then Begin
      	 Call VISOR.AND.BORRAR("ERROR DEFINICION    DOMICILIARIO /Borrar")  !
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
         TS.IO.MOTORKEY = 73																						    !
         Exit Sub      																											!
      EndIf																																	!
      Gr.Domi.Intrx% = -1																										! Domicilio en proceso
      Gr.Domi.Ptg% = Val(Gr.Domi.Lista$(TS.TEMP1I1,1))                      ! Asigna ptg domiciliario
      TS.TEMP1$  = Pack$(Str$(TS.TEMP1I1))                         +       \! ID domiciliario
                   ":"+Pack$(Str$(Gr.Domi.Ptg%))                   +       \! Porcentaje domicilio
                   ":"+Pack$("00")                                          ! Filler 
      Call Grabacion.String.Usuario2("20231111",TS.TEMP1$)                  ! Graba String usuario  
      
      Call VISOR.AND.BORRAR("OPERADOR DOMICILIO  "+Gr.Domi.Lista$(TS.TEMP1I1,0))
      Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      !
      TS.IO.MOTORKEY = 73																						    		!
      Exit Sub      																												!
   EndIf																																		! Fin tecla motora
EndIf																																				!


!--- EAMTSU53.J86
If Xopt% = 53 Then Begin                                                    ! En recuperacion de trx 
 If Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "99" THEN BEGIN   	   	              ! Si user data
  If Unpack$(MID$(SL.STR.ENTRY$,3,4)) = "20231111" Then Begin               ! Proyecto Domicioliarios
    Asc.Tmp.Apun% = 3																												!
    Gr.Domi.Intrx% = -1																											! Domicilio en proceso
    TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                   ! Numero de proyecto
    TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)           				! Id del domiciliario
    TS.TEMP1I1 = Val(TS.TEMP1$)
    TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)           				! Porcentaje domicilio
    Gr.Domi.Ptg% = Val(TS.TEMP1$)																						! Asigna variable
    Call VISOR.AND.BORRAR("OPER. DOMICI. RECUP."+Gr.Domi.Lista$(TS.TEMP1I1,0))
  EndIf																																			! 
 EndIf 
EndIf																																			  ! Fin Eamtsu53
 
End Sub 																																		! Fin libreria
