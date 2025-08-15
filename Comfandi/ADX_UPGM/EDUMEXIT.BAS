!************************************************** 
!Empresa       : ASIC S.A. UNIDAD DE RETAIL       *
!Programa      : EDUMEXIT.BAS                     *
!Autor         : Oscar Valencia                   *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Junio del 2005                   *
!Observaciones : Modulo Recaudo Siempre Educando  *
!**************************************************
!Modificaciones
!Mod19Sept2005
! Se ajusta la validacion de fechas entregando estas
! a variables de tipo numerico, el compilador
! no controla correctamente condiciones con la 
! funcion Val
!--------------------------------------------------

%ENVIRON T

Integer   Global SL.END, Asc.Edu.InVlr%

%INCLUDE EAMP4VAJ.J86        				!4610 Printer
%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          !
%Include EAMTOPTS.J86	         !
%Include EAMITEMR.J86	         !
%Include EDUTSUVA.011          ! Definicion de variables
%Include EAMCUSTA.J86	         !
%INCLUDE EAMTSXHC.J86          !
%INCLUDE EAMASMRT.J86          !

%Include EDUTSUSU.011          ! Subrutinas 

Sub TSHIECET External 				 ! BEP
End Sub 

Sub TSPREC01 External					 ! RUT. IMP SMA
End Sub 

Sub TSCSECRK EXTERNAL     		 ! READ KEYBOARD
End Sub

Sub TSCSEC08 External
End Sub 

Function FORMAT.AMOUNT(AMT1) External
  INTEGER*1 FORMAT.AMOUNT
  INTEGER*4 AMT1
End Function

Function Linea.Detalle(X.Ses%)   External                  ! Imprime Detalle de la operacion
  STRING Linea.Detalle
  Integer*1 X.Ses%
End Function

Sub Mensaje.Error(X.M1$, X.M2$) External
End Sub

Function Anulacion.Recibos.Educando(X.Data$)
String X.Data$
Integer*4 X.Pos%, X.FEC1%, X.FEC2%
    TS.TEMP1$ = X.Data$
    If Len(X.Data$) <> 30 Then BEGIN																			! No es recibo colegio
       TS.TEMP2$ = Asic.Datos$(" Recibo No Valido ","Presione BORRAR")      !
       DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 													  ! Init vectores de carga
       TS.IO.MOTORKEY = 73																									! Tecla Borrar
       Exit Function  																														! Sale de la rutina       
    EndIf 																																	!
    X.Pos% = MATCH(X.Data$,Asc.Edu.Lista$,1)  															! Busca Recibo
    If X.Pos% <= 0 Then Begin 																							! Ya Registrado
       Call TSHIECET																												! Tono de alerta
       TS.TEMP2$ = Asic.Datos$("Recibo No Registrado","Presione BORRAR")    !
       DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 													  ! Init vectores de carga
       TS.IO.MOTORKEY = 73																									! Tecla Borrar
       Exit Function  																														! Sale de la rutina
    EndIf																																		!
    X.Pos% = 0																															!
    X.Pos% = MATCH(X.Data$,Asc.Edu.NotaNul$,1)  							  				  ! Valida si ya no fue anulado antes
    If X.Pos% > 0 Then Begin 					  																    ! Si ya esta anulado
       TS.TEMP2$ = Asic.Datos$(" Recibo Ya Anulado ","Presione BORRAR")     !
       DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 													  ! Init vectores de carga
       TS.IO.MOTORKEY = 73																									! Tecla Borrar
       Exit Function  																														!      
    EndIf
       Asc.Edu.NotaNul$ = Asc.Edu.NotaNul$ +(","+X.Data$+",")			  				! Adiciona a la lista Anulados
       TS.TEMP1$ = X.DATA$
       !TS.TEMP1$ = Asc.Edu.NotaNul$
       
       X.fecha$=Right$("20"+Date$,8)
       Asc.Edu.NoRec$ = 	mid$(TS.TEMP1$,1,8)															  ! Numero del recibo   
       Asc.Edu.VlrPago1$=mid$(TS.TEMP1$,9,7)																! Captura valor con descuento
       Asc.Edu.Fecha$=mid$(TS.TEMP1$,16,8)																	! Captura fecha limite de pago
       Asc.Edu.VlrPago2$=mid$(TS.TEMP1$,24,7)																! Captura valor Sin descuento
       Asc.Edu.InVlr% = 0
       
      X.FEC1% = Val(Asc.Edu.Fecha$)                                         ! Add OVS 19 Sept 2005
      X.FEC2% = Val(X.fecha$)																					      ! Control validacion de fechas

      If X.FEC1% < X.FEC2% Then Begin                                       ! Si fecha vencida
             Asc.Edu.VlrPago$ = Asc.Edu.VlrPago2$ 
             Asc.Edu.InVlr% = 2
      EndIf Else Begin      ! Asigna valor Sin dscto sino 
             Asc.Edu.VlrPago$ = Asc.Edu.VlrPago1$														! Asigna valor CON dscto
             Asc.Edu.InVlr% = 1
      EndIf       
       
       TS.TEMP3$ = PACK$(RIGHT$("00"+STR$(ASC.EDU.INVLR%),2))
       Call Grabacion.Cadena.Usuario("20050603",Pack$(X.Data$)+":"+TS.TEMP3$)    ! Graba String Usuario anulacion
       Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 												  	     ! Init vectores de carga
       TS.IO.KEYS(1) = 70                                                        ! Anulacion Item
       TS.IO.DATA$(2) = Asc.Edu.Item$																		  	     ! Item Miscelaneo
       TS.IO.KEYS(2)  = 80																							 	       ! Tecla ENTER
       TS.IO.MOTORKEY = 80																								       ! Tecla ENTER
       Asc.Edu.Impr% = -1                                                        ! Impresion de Bonos
       Asc.Edu.Devol% = -1																		   		             ! Indicador devolucion    
End Function
!--- Fin anulacion de recibos

Sub Validacion.Recibos.Educando
String X.Fill$, X.fecha$, X.Data$
Integer*4 X.Pos%, X.Fec1%, X.Fec2%
    If Len(TS.TEMP1$) <> 30 Then BEGIN																			! No es recibo educando
       TS.TEMP2$ = Asic.Datos$(" Recibo No Valido ","Presione BORRAR")      !
       DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 													  ! Init vectores de carga
       TS.IO.MOTORKEY = 73																									! Tecla Borrar
       Exit Sub  																														! Sale de la rutina       
    EndIf 
    X.Data$ = Ts.Temp1$
    X.Pos% = MATCH(Ts.Temp1$,Asc.Edu.Lista$,1)															! Busca Recibo
    If X.Pos% > 0 Then Begin 																								! Ya Registrado
       Call TSHIECET																												! Tono de alerta
       TS.TEMP2$ = Asic.Datos$("Recibo Ya Capturado","Presione BORRAR")     !
       DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 													  ! Init vectores de carga
       TS.IO.MOTORKEY = 73																									! Tecla Borrar
       Exit Sub  																														! Sale de la rutina
    EndIf Else Begin 																												!
      X.Fill$ = Pack$("00")																									! Relleno
      X.fecha$=Right$("20"+Date$,8)																			    ! Fecha del sistema en formato aaaammdd
      Asc.Edu.NoRec$=Left$(TS.TEMP1$,8)					   													! Captura Num recibo
      Asc.Edu.VlrPago1$=mid$(TS.TEMP1$,9,7)																	! Captura valor con descuento
      Asc.Edu.Fecha$=mid$(TS.TEMP1$,16,8)																		! Captura fecha limite de pago
      Asc.Edu.VlrPago2$=Right$(TS.TEMP1$,7)						   										! Captura valor Sin descuento
!      Call VISORES4690(1,Asc.Edu.Fecha$,X.fecha$,1800,"L")

      X.FEC1% = Val(Asc.Edu.Fecha$)                                         ! Add OVS 19 Sept 2005
      X.FEC2% = Val(X.fecha$)																					      ! Control validacion de fechas

      If X.FEC1% < X.FEC2% Then Begin                                       ! Si fecha vencida
             Asc.Edu.VlrPago$ = Asc.Edu.VlrPago2$ 
             Asc.Edu.InVlr% = 2
      EndIf Else Begin      ! Asigna valor Sin dscto sino 
             Asc.Edu.VlrPago$ = Asc.Edu.VlrPago1$														! Asigna valor CON dscto
             Asc.Edu.InVlr% = 1
      EndIf

      Asc.Edu.Lista$ = Asc.Edu.Lista$ +(","+X.Data$+",")						      	! Adiciona a la lista
      TS.TEMP3$ = PACK$(RIGHT$("00"+STR$(ASC.EDU.INVLR%),2))
      Call Grabacion.Cadena.Usuario("20050602",Pack$(X.Data$)+":"+TS.TEMP3$)            ! Graba String Usuario
      Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 													    ! Init vectores de carga
      TS.IO.DATA$(2) = Asc.Edu.Item$																			  ! Item Miscelaneo
      TS.IO.KEYS(2)  = 80																										! Tecla ENTER
      TS.IO.MOTORKEY = 80																										! Tecla ENTER
      TS.IO.DEVICE   = 1
      Asc.Edu.Impr% = -1                                                    ! Impresion de Bonos
      Asc.Edu.Devol% = 0 																		   		          ! Indicador devolucion
      Asc.Edu.IndPrice% = -1																								! Cambio de precio
    EndIf 
End Sub
!--- Fin validacion Recibos

Sub imprime.franqueo.Educando
String X.Bon$, X.Vlr$, X.WKG$, X.Recibo$, X.Costo$, X.Tmp$, X.vlr2$
Integer*4 X.POS%, X.Vuso%, X.Nfr%, X.J%, X.Veces%
  Asc.Tmp.Apun% = 3																												! Apuntador String
  X.BON$      = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)														! Ajuste del String
  X.BON$      = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)														! Ajuste del String
  X.vlr2$     = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)														! Ajuste del String
  ts.temp1$ = x.bon$
  Asc.Edu.NoRec$=Left$(TS.TEMP1$,8)					   													! Captura Num recibo
  Asc.Edu.VlrPago1$=mid$(TS.TEMP1$,9,7)																	! Captura valor con descuento
  Asc.Edu.Fecha$=mid$(TS.TEMP1$,16,8)																		! Captura fecha limite de pago
  Asc.Edu.VlrPago2$=Right$(TS.TEMP1$,7)						   										! Captura valor Sin descuento
  
!  If PRT4610.ENABLE = 0 Then X.Veces% = 2 Else 

  X.Veces% = 5
  
  if val(x.vlr2$) = 2 then \!
         Asc.Edu.VlrPago$ = Asc.Edu.VlrPago2$ Else                     \! Asigna valor Sin dscto sino 
         Asc.Edu.VlrPago$ = Asc.Edu.VlrPago1$														! Asigna valor CON dscto  

  X.WKG$ = ","+X.BON$+","										            									! Arma Str busqueda
  X.Nfr% = 1 																															! Numero de Franqueos
  X.POS% = MATCH(X.WKG$,Asc.Edu.NotaNul$,1)			  												! Busca Nota
  IF (X.POS% >  0) THEN BEGIN														    							! No lo encontro
    Exit Sub 
  EndIf Else begin 
   Call TSHIECET
   Call VISORES4690(1,"Inserte Bono No.",Asc.Edu.NoRec$,1500,"L")			  	! Franquea el documento
   Call SALVAR.VARIABLES																									!
   Impresion.Franqueo.Bono.Educando:
   Call Format.Amount(Val(Asc.Edu.VlrPago$))
  
  If PRT4610.ENABLE = 0 Then Begin 
     CALL U.IMPRIME("*****  SIEMPRE EDUCANDO   *****",1100H)
     CALL U.IMPRIME("NUMERO RECIBO   : "+Asc.Edu.NoRec$,1100H)
     CALL U.IMPRIME("VALOR PAGADO    : "+Ts.Temp1$,1100H)
     CALL U.IMPRIME(STRING$(37,"-"),1100H)
     X.Tmp$ = Linea.Detalle(Asc.Edu.Sesion%)                                ! Store Line Trx      
     CALL U.IMPRIME(X.Tmp$,1100H)																            !
  EndIf Else Begin 
	   TS.PRT.PARM = 1000H																										!DOC INSERT
  	 TS.LINETYPE = 9																												!
  	 TS.LINEDATA = Asc.Edu.Fran%																						! Formato de franqueo   
	   TO.FRANK.TXT$(Asc.Edu.Fran%,01) = "*****  SIEMPRE EDUCANDO   *****"
	   TO.FRANK.TXT$(Asc.Edu.Fran%,02) = "NUMERO RECIBO   : "+Asc.Edu.NoRec$
	   TO.FRANK.TXT$(Asc.Edu.Fran%,03) = "VALOR PAGADO    : "+Ts.Temp1$
     TO.FRANK.TXT$(Asc.Edu.Fran%,04) = STRING$(37,"-")
     X.Tmp$ = Linea.Detalle(Asc.Edu.Sesion%)                                ! Store Line Trx      
     TO.FRANK.TXT$(Asc.Edu.Fran%,05) = X.Tmp$																!
     Call TSPREC01 
  EndIf 
   X.Tmp$ = " "
   If X.NFR% < 2 Then begin 
      For X.J% = 1 to X.Veces%
         Call U.Imprime(X.Tmp$,1900H)
      Next X.J%
      
   EndIf    
   
   X.NFR% = X.NFR% + 1
   If X.NFR% < 3 Then GoTo Impresion.Franqueo.Bono.Educando 
   Call RESTAURA.VARIABLES
  EndIf 
  
End Sub 
!--- Fin franqueo Bonos

Function FRANQUEO.BONOS.EDUCANDO Public    															  !
 Integer*4 PRIC%, TOT4%, X.I%																							!
 String X.TMP$																														!
  For X.I% = 1 TO Asc.Edu.Slend%                  												! Recorre todos los String
    H$ = READ.SL.STR$(X.I%)                 															! Toma el String
    If LEN(H$) > 5 Then Begin              														    ! Si el String es OK
     If ASC(H$)  = 153 Then Begin            															! Si es un User Data 99
        Asc.Tmp.Apun% = 3																									! Asigna apuntador
        X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  												! Verifica el numero de proyecto
        If X.TMP$ = "20050602" Then Begin                                 ! 
           Call Imprime.Franqueo.Educando                                 ! Impresion del franqueo del documento 
           Call U.EXPLUSADI                                               ! Expulda Documento
        EndIf 																														!
     EndIf                                  															!
   EndIf 
  Next X.I%                                 															!
End Function
!--- Fin recorrido String de la transaccion

Sub EDUMTS02.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Recaudo de colegios                                  **
!   **    User Exit  :  EDUMTS02.011                                         **
!   **    Incluir en :  EAMTSU02.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Edu.Line% Then Begin 
   %Include EDUTSU02.011
EndIf 

End Sub 

Sub EDUMTS07.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Devoluciones en Linea                                **
!   **    User Exit  :  EDUMTS07.011                                         **
!   **    Incluir en :  EAMTSU07.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

   %Include EDUTSU07.011

End Sub 

Sub EDUMTS14.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Devoluciones en Linea                                **
!   **    User Exit  :  DVLMTS14.011                                         **
!   **    Incluir en :  EAMTSU14.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Edu.Line% Then Begin 
   %Include EDUTSU14.011
EndIf 

End Sub 

Sub EDUMTS08.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Recaudo de colegios                                  **
!   **    User Exit  :  EDUTSU08.011                                         **
!   **    Incluir en :  EAMTSU02.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Edu.Line% Then Begin 
   %Include EDUTSU08.011
EndIf 

End Sub

Sub EDUMTS20.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Devoluciones en Linea                                **
!   **    User Exit  :  EDUMTS20.011                                         **
!   **    Incluir en :  EAMTSU20.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Edu.Line% Then Begin 
   %Include EDUTSU20.011
EndIf 

End Sub 

Sub EDUMTS43.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Devoluciones en Linea                                **
!   **    User Exit  :  EDUTSU43.011                                         **
!   **    Incluir en :  EAMTSU43.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Edu.Line% Then Begin 
   %Include EDUTSU43.011
EndIf 

End Sub 


Sub EDUMTS53.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Devoluciones en Linea                                **
!   **    User Exit  :  EDUMTS53.011                                         **
!   **    Incluir en :  EAMTSU53.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Edu.Line% Then Begin 
   %Include EDUTSU53.011
EndIf 

End Sub 
