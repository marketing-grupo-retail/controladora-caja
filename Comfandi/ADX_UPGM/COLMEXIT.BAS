!************************************************** 
!Empresa       : ASIC S.A. UNIDAD DE RETAIL       *
!Programa      : COLMEXIT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Junio del 2004                   *
!Observaciones : Modulo Recaudo de Colegios       *
!**************************************************
!

%ENVIRON T
Integer   Global SL.END 

%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          !
%Include EAMTOPTS.J86	         !
%Include EAMITEMR.J86	         !
%Include COLTSUVA.011          ! Definicion de variables
%Include EAMCUSTA.J86	         !
%INCLUDE EAMTSXHC.J86          !
%INCLUDE EAMASMRT.J86          !

%Include COLTSUSU.011          ! Subrutinas 

Sub TSHIECET External 
End Sub 

Sub TSPREC01 External
End Sub 

Function FORMAT.AMOUNT(AMT1) External
  INTEGER*1 FORMAT.AMOUNT
  INTEGER*4 AMT1
End Function

Function Linea.Detalle(X.Ses%)   External                  ! Imprime Detalle de la operacion
  STRING Linea.Detalle
  Integer*1 X.Ses%
End Function

Function Anulacion.Recibos(X.Data$)
String X.Data$
Integer*4 X.Pos%
    TS.TEMP1$ = X.Data$
    If Len(X.Data$) <> 18 Then BEGIN																			! No es recibo colegio
       TS.TEMP2$ = Asic.Datos$(" Recibo No Valido ","Presione BORRAR")      !
       DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 													  ! Init vectores de carga
       TS.IO.MOTORKEY = 73																									! Tecla Borrar
       Exit Function  																														! Sale de la rutina       
    EndIf 																																	!
    X.Pos% = MATCH(X.Data$,Asc.Col.Lista$,1)															! Busca Recibo
    If X.Pos% <= 0 Then Begin 																							! Ya Registrado
       Call TSHIECET																												! Tono de alerta
       TS.TEMP2$ = Asic.Datos$("Recibo No Registrado","Presione BORRAR")    !
       DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 													  ! Init vectores de carga
       TS.IO.MOTORKEY = 73																									! Tecla Borrar
       Exit Function  																														! Sale de la rutina
    EndIf																																		!
    X.Pos% = 0																															!
    X.Pos% = MATCH(X.Data$,Asc.Col.NotaNul$,1)  							  				  ! Valida si ya no fue anulado antes
    If X.Pos% > 0 Then Begin 					  																    ! Si ya esta anulado
       TS.TEMP2$ = Asic.Datos$(" Recibo Ya Anulado ","Presione BORRAR")     !
       DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 													  ! Init vectores de carga
       TS.IO.MOTORKEY = 73																									! Tecla Borrar
       Exit Function  																														!      
    EndIf
       Asc.Col.NotaNul$ = Asc.Col.NotaNul$ +(","+X.Data$+",")							! Adiciona a la lista Anulados
       Call Grabacion.String.Usuario("20041103",Pack$(X.Data$))           ! Graba String Usuario
       TS.TEMP1$ = X.DATA$
       TS.TEMP1$ = Asc.Col.NotaNul$
       Asc.Col.VlrBono$ = Right$(X.Data$,7)                               ! Valor del Bono
       Asc.Col.VlrBono$ = Str$(Val(Asc.Col.VlrBono$))                       ! 
       Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 													  ! Init vectores de carga
       TS.IO.KEYS(1) = 70                                                   ! Anulacion Item
       TS.IO.DATA$(2) = Asc.Col.Item$																			  ! Item Miscelaneo
       TS.IO.KEYS(2)  = 80																									! Tecla ENTER
       TS.IO.MOTORKEY = 80																								  ! Tecla ENTER
       Ue.CENCOS$ = mid$(x.Data$,1,5)				   																! Toma centro de costo
       Ue.RECIVE$ = mid$(X.Data$,6,6)																					! Numero del recibo       
       Asc.Col.Impr% = -1                                                   ! Impresion de Bonos
       Asc.Col.Devol% = -1																		   		        ! Indicador devolucion    
End Function
!--- Fin anulacion de recibos

Sub Validacion.Recibos
String X.Fill$
Integer*4 X.Pos%
    If Len(TS.TEMP1$) <> 18 Then BEGIN																			! No es recibo colegio
       TS.TEMP2$ = Asic.Datos$(" Recibo No Valido ","Presione BORRAR")      !
       DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 													  ! Init vectores de carga
       TS.IO.MOTORKEY = 73																									! Tecla Borrar
       Exit Sub  																														! Sale de la rutina       
    EndIf 
    X.Pos% = MATCH(Ts.Temp1$,Asc.Col.Lista$,1)															! Busca Recibo
    If X.Pos% > 0 Then Begin 																								! Ya Registrado
       Call TSHIECET																												! Tono de alerta
       TS.TEMP2$ = Asic.Datos$("Recibo Ya Capturado","Presione BORRAR")     !
       DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 													  ! Init vectores de carga
       TS.IO.MOTORKEY = 73																									! Tecla Borrar
       Exit Sub  																														! Sale de la rutina
    EndIf Else Begin 
      X.Fill$ = Pack$("00")
      Asc.Col.Lista$ = Asc.Col.Lista$ +(","+TS.TEMP1$+",")									! Adiciona a la lista
      Call Grabacion.String.Usuario("20041102",Pack$(TS.TEMP1$))            ! Graba String Usuario

      Asc.Col.VlrBono$ = Right$(TS.TEMP1$,7)                                ! Valor del Bono
      Asc.Col.VlrBono$ = Str$(Val(Asc.Col.VlrBono$))
      Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 													    ! Init vectores de carga
      TS.IO.DATA$(2) = Asc.Col.Item$																			  ! Item Miscelaneo
      TS.IO.KEYS(2)  = 80																										! Tecla ENTER
      TS.IO.MOTORKEY = 80																										! Tecla ENTER
      Ue.CENCOS$ = mid$(TS.TEMP1$,1,5)	    																! Toma centro de costo
      Ue.RECIVE$ = mid$(TS.TEMP1$,6,6)																			! Numero del recibo
      Asc.Col.Impr% = -1                                                    ! Impresion de Bonos
      Asc.Col.Devol% = 0 																		   		          ! Indicador devolucion    
    EndIf 
End Sub
!--- Fin validacion Recibos

Sub imprime.franqueo
String X.Bon$, X.Vlr$, X.WKG$, X.Recibo$, X.Costo$, X.Tmp$
Integer*4 X.POS%, X.Vuso%, X.Nfr%, X.J%
  Asc.Tmp.Apun% = 3																												! Apuntador String
  X.BON$      = ASIC.GETUNPK2(H$,Asc.Tmp.Apun%)														! Ajuste del String
  X.BON$      = ASIC.GETUNPK2(H$,Asc.Tmp.Apun%)														! Numero del bono
  X.Costo$ = mid$(X.bon$,1,5)																							! Toma centro de costo
  X.Recibo$ = mid$(X.bon$,6,6)																						! Numero del recibo
  X.Vlr$ = Right$(X.bon$,7)																								! Valor del recibo
  Call Format.Amount(Val(X.vlr$))                           							! Formatea valor
  X.Vlr$ = TS.TEMP1$																											! Retorna dato
  X.WKG$ = ","+X.BON$+","										            									! Arma Str busqueda
  X.Nfr% = 1 																															! Numero de Franqueos
  X.POS% = MATCH(X.WKG$,Asc.Col.NotaNul$,1)			  												! Busca Nota
  TS.TEMP1$ = Asc.Col.NotaNul$
  IF (X.POS% >  0) THEN BEGIN														    							! No lo encontro
    Exit Sub 
  EndIf Else begin 
   Call TSHIECET
   Call VISORES4690(1,"Inserte Bono No.",X.Bon$,1500,"L")									! Franquea el documento
   Impresion.Franqueo.Bono:
   Call SALVAR.VARIABLES																									!
	 TS.PRT.PARM = 1000H																										!
	 TS.LINETYPE = 9																												!
	 TS.LINEDATA = Asc.Col.Fran%																						! Formato de franqueo   
	 TO.FRANK.TXT$(Asc.Col.Fran%,01) = " *****   RECAUDO   COLEGIOS     ***** "
	 TO.FRANK.TXT$(Asc.Col.Fran%,02) = "CENTRO DE COSTO : "+X.Costo$
	 TO.FRANK.TXT$(Asc.Col.Fran%,03) = "NUMERO RECIBO   : "+X.Recibo$
	 TO.FRANK.TXT$(Asc.Col.Fran%,04) = "VALOR RECAUDADO : "+X.Vlr$     
   TO.FRANK.TXT$(Asc.Col.Fran%,05) = STRING$(38,"-")
   X.Tmp$ = Linea.Detalle(Asc.Col.Sesion%)                                ! Store Line Trx      
   TO.FRANK.TXT$(Asc.Col.Fran%,06) = X.Tmp$																!
   Call TSPREC01 
   X.Tmp$ = ""
   If X.NFR% < 3 Then begin 
      For X.J% = 1 to 25
          X.Tmp$ = X.Tmp$ + Chr$(0DH)
      Next X.J%
      Call U.Imprime(X.Tmp$,1100H)
   EndIf    
   Call RESTAURA.VARIABLES
   X.NFR% = X.NFR% + 1
   If X.NFR% < 4 Then GoTo Impresion.Franqueo.Bono 
  EndIf 
  
End Sub 
!--- Fin franqueo Bonos

Function FRANQUEO.BONOS Public             															  !
 Integer*4 PRIC%, TOT4%, X.I%																							!
 String X.TMP$																														!
  For X.I% = 1 TO Asc.Col.Slend%                  												! Recorre todos los String
    H$ = READ.SL.STR$(X.I%)                 															! Toma el String
    If LEN(H$) > 5 Then                    														   \! Si el String es OK
     If ASC(H$)  = 17 Then Begin            															! Si es un Data Entry
        Asc.Tmp.Apun% = 3																									! Asigna apuntador
        X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  												! Verifica el numero de proyecto
        If X.TMP$ = "20041102" Then Begin                                 ! Proyecto devolucion en linea
           Call Imprime.Franqueo                                          ! Impresion del franqueo del documento 
           Call U.EXPLUSADI                                               ! Expulda Documento
        EndIf 																														!
     EndIf                                  															!
  Next X.I%                                 															!
End Function
!--- Fin recorrido String de la transaccion

Sub COLMTS02.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Recaudo de colegios                                  **
!   **    User Exit  :  COLMTS02.011                                         **
!   **    Incluir en :  EAMTSU02.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Col.Line% Then Begin 
   %Include COLTSU02.011
EndIf 

End Sub 

Sub COLMTS07.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Devoluciones en Linea                                **
!   **    User Exit  :  COLMTS07.011                                         **
!   **    Incluir en :  EAMTSU07.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

   %Include COLTSU07.011

End Sub 

Sub COLMTS14.011  Public 
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

If Asc.Col.Line% Then Begin 
   %Include COLTSU14.011
EndIf 

End Sub 

Sub COLMTS08.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Recaudo de colegios                                  **
!   **    User Exit  :  COLMTS08.011                                         **
!   **    Incluir en :  EAMTSU02.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Col.Line% Then Begin 
   %Include COLTSU08.011
EndIf 

End Sub

Sub COLMTS20.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Devoluciones en Linea                                **
!   **    User Exit  :  COLMTS20.011                                         **
!   **    Incluir en :  EAMTSU20.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Col.Line% Then Begin 
   %Include COLTSU20.011
EndIf 

End Sub 

Sub COLMTS43.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Devoluciones en Linea                                **
!   **    User Exit  :  COLMTS43.011                                         **
!   **    Incluir en :  EAMTSU43.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Col.Line% Then Begin 
   %Include COLTSU43.011
EndIf 

End Sub 


Sub COLMTS53.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Devoluciones en Linea                                **
!   **    User Exit  :  COLMTS53.011                                         **
!   **    Incluir en :  EAMTSU53.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Col.Line% Then Begin 
   %Include COLTSU53.011
EndIf 

End Sub 
