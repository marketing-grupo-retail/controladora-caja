!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : CNVMEXIT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Control Convenios         *
!**************************************************
! 04May2005
! Se adiciona control para manejo de capitación por
! planes, y se modifica la impresion del tiquete de 
! venta
! Desarrollado por Oscar Valencia Sarmiento
!--------------------------------------------------
! 08May2006
! Se ajusta el modulo para que permita utilizar los 
! formatos de la minifactura y formula medica que 
! usan Comfandi, desarrollado por Oscar Valencia
! Asic.
!-------------------------------------------------
! 07Feb2007
! Se ajusta el modulo para el manejo de capitaciones
! segun el requerimiento de Comfandi.
!---------------------------------------------------
! 10Abr2007
! Se ajusta modulo de capitacion para manejo de % de
! descuentos segun requerimiento de Comfandi.
!---------------------------------------------------
! 24Jul2008
! Se ajusta modulo para control de copia tiquete segun
! parametro matriculado en el plan.
! desarrollado por Grupo Retail - OVS
!-----------------------------------------------------
! 28Jul2008
! Se adiciona los proceos de validacion en linea de los
! procesos:
! -- Validacion de PLU/PLAN para productos no existentes 
! -- Validacion de Beneficiarios
! Desarrollado por Grupo Retail - OVS
!------------------------------------------------------
! Mod8Sep2008
! Se adiciona la rutina Factura.Automatica
! para la generacion de un consecutivo para la generacion
! documentos por almacen, el consecutivo consta de 2 partes
! el numero de la tienda (3 caracteres) y un consecutivo de
! 6 digitos.
!------------------------------------------------------
! Mod 22Abr2013
! Se adiciona en la impresion de la cabecera del tiquete
! de venta y copia del cliente el numero del nit y razon
! social del convenio.
! Desarrollado por Grupo Retail - OVS
!------------------------------------------------------
! Mod 22Abr2013
! Se modifica validación modulo de convenios para que todo
! producto sea validado en línea contra el As/400, segun
! requerimiento de comfandi, en caso de falla reporta Error
! y no permite la venta del producto.
! Desarrollado por Grupo Retail - OVS
!------------------------------------------------------

%ENVIRON T

Integer*4 global IR.price1, IR.price2
Integer*1 Global IR.ITEMTYPE, Asc.Cnv.Formula%, Asic.Detalle%, ASIC.LLAVE%

String    Global vend.medica$, nom.oper$, XERRFX$


%Include EAMTSWKG.J86          ! working storage
%Include EAMTRANS.j86          !
%Include EAMTERMS.J86          !
%Include EAMITEMR.J86          !
%Include EAMTOPTS.J86	       !
%Include EAMSOPTS.J86	       !
%Include CNVTSUVA.011	       ! Variables del programa
%Include CTRLTSVA.011	       ! Lectura de Control
%INCLUDE GRITEMR.J86        				  ! Variables Itemr
%Include EAMP4VGJ.J86          ! Variables rutinas 4610
%INCLUDE EAMTSXHC.J86          !
%Include EAMASMRT.J86          !

Sub TSPREC01  EXTERNAL
End Sub

Sub TSHIECET EXTERNAL
End Sub

%INCLUDE PEFMTSSU.011           ! Rutinas Genericas

Function Entrada.Datos.Varios$(Umsg1$,Umsg2$) External   		! Captura De Datos
String Entrada.Datos.Varios$, Umsg1$, Umsg2$, Udata$
Integer*1  Ui%
Integer*4 X.GETLONG
End Function

Function FORMAT.AMOUNT(AMT1) EXTERNAL
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                      *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                         *!
  STRING    DATA$      !* String to hold the data read from file.            *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                               *!
End Sub 

Sub SPLIT.ITEMFILE (RECORD$) External
  STRING    RECORD$    !* The record (including the key) read from file      *!
End Sub 

Sub TRADUCE.ERROR.CNV                                       !
String XZ$
Integer*4 XS%, XSUM%, XHX%, XSX%
    XHX% = ERRN                                               ! Traduccion de los
    XERRFX$=""                                                ! codigos de error
    FOR XS% = 28 TO 0 STEP -4                                 ! del sistema operativo
        XSX% = SHIFT(XHX%,XS%)                                  !
        XSUM% = XSX% AND 000FH                                 !
        IF XSUM% > 9 THEN XSUM% = XSUM% + 55                   \!
        ELSE XSUM% = XSUM% + 48                                !
        XZ$ = CHR$(XSUM%)                                      !
        XERRFX$ = XERRFX$ + XZ$                                 !
    NEXT XS%         
End Sub 


Sub VALIDE.ERROREPS(OVRKEY%) Public
Integer*1 OVRKEY%
  If Mid$(IOPDATA$,14,1) <> " " Then Begin         ! Si hay Errror
       Call Visores4690(1,"DATO ERRADO..!", "",1500,"C")
       UE.ERROR% = -1
  EndIf                                            
If NOT (OVRKEY%) Then Begin
	If (UE.LLAVE.PUESTA%) Then Begin
	    Call Visores4690(1,"RETIRE LLAVE !!", "",1500,"C")
        UE.ERROR% = -1
	EndIf
 EndIf

If OVRKEY% Then Begin
	If NOT (UE.LLAVE.PUESTA%) Then Begin
       Call Visores4690(1,"ESTE PROCEDIMIENT0", "NECESITA LLAVE !!",1500,"C")
       UE.ERROR% = -1
	EndIf
  EndIf
End Sub 

Sub CAPTURE.DATOEPS(LINEA1$,LINEA2$,OVRKEY%) Public
String LINEA1$, LINEA2$
Integer*1 OVRKEY%
  ASIC.LLAVE% = 0 
  IOPDATA$ = Entrada.Datos.Varios$(LINEA1$,LINEA2$)
End Sub

Sub INICIALICE.EPS
Integer*2 XI%
  If UE.EPS.CAPITACION$ = "1" Then Begin 
     TO.TRAILERLINE1$ = Asc.Cnv.Line1$
     TO.TRAILERLINE2$ = Asc.Cnv.Line2$
  EndIf 
  Asc.Cnv.Line2$  = "F" : Asc.Cnv.Line1$ = ""
	UE.EPS.CCOD$    = ""  :UE.EPS.CNOMB$   = "" :UE.EPS.CFECHI$ = "" :UE.EPS.CFECHF$  = ""
	UE.EPS.CTOT$    = ""  :UE.EPS.CMAX$    = "" :UE.EPS.CVMED$  = "" :UE.EPS.CVMERCA$ = ""	
	UE.EPS.CVIPS$   = ""  :UE.EPS.CCALCULO$= "" :DIM UE.EPS.CMP$(5)
	UE.EPSS.DIAG$   = ""  :UE.EPSS.NoDIAG$ = ""
	UE.EPSS.INOMB$  = ""  :UE.EPS.CODALT$  = ""
	UE.EPSS.MNOMB$  = ""  :UE.EPSS.DIAG$   = ""
	UE.EPS.CNIT$    = ""  :DIM UE.EPS.DSCTLESS$(5)
	UE.EPS.BCOD$    = ""  :UE.EPS.BTIPO$   = "" :UE.EPS.BCONVENIO$ = "" :UE.EPS.BNOMB$      = ""
	UE.EPS.BFECHN$  = ""  :UE.EPS.BCOTIZ$  = "" :UE.EPS.BPAREN$    = "" :UE.EPS.BESTRATO$   = ""
	UE.EPS.BFECHI$  = ""  :UE.EPS.BFECHF$  = "" :UE.EPS.BPLAN$     = "" :UE.EPS.BEMPRESA$   = ""
	UE.EPS.BESTADO$ = ""  :UE.EPS.BPREST$  = "" :UE.EPS.BCODPREST$ = "" :UE.EPS.CAPITACION$ = ""
	UE.EPSS.COOP% = 0     :UE.EPS.LINE% = 0     :UE.EPS.FOTO% = 0       :UE.EPSS.FORMULA$   = ""
  UE.EPS.CVDIAGNOS$ = "" : UE.EPS.NRMLoCATAL$ = "" : UE.EPS.COPAGOLESS$ = "" 
	UE.EPS.DSCTOTRX$  = "" : UE.EPS.ValFAC$ = ""  : UE.EPS.ValBEN$ = "" : UE.EPS.CAPITACION$ = ""
	UE.EPS.NCONVE$ = "" :  UE.EPS.DATO$ = ""
	UE.EPSS.ON = 0
        UE.EPS.GRB% = -1
	UE.EPS.VLRCOPAGO$ = "0"
	UE.EPS.DSCTOTRX$ = "0"
        UE.EPS.ERROR$ = ""
        UE.EPS.COPIA% = 0
        UE.EPS.DATO% = 0
	UE.EPS.COPAGOAPLICADO% = 0
	UE.EPS.GRABAString% = 0 
	UE.EPS.INDPAGO% = 0
	UE.CTRL.DEVO.EPS% = 0
	Ue.Cnv.AplicaCopago% = 0 
	Ue.Cnv.Fiscal% = 0 
	Asc.Cnv.NumItm% = 0 
	Asc.Cnv.EpCop$ = "00"
	Asc.Cnv.PtgCuota$ = "0"
	UE.EPS.DEVO% = 0 
	Asc.Cnv.PagoParcial$ = "0"
	Asc.Cnv.ApplCobro% = 0 
	Asc.Cnv.CuotaMod$ = "0"
	Asc.Cnv.NumNit$  = "0"
	For XI% = 0 To 7	! Control  Montos devoluciones
            TO.NEGAMT(XI%) = Asc.NEGAMT(XI%) 
	    TO.NEGCNT(XI%) = Asc.NEGCNT(XI%)
        Next XI%
End Sub

Function VALIDE.MEDICO(NO.MEDICO$)
String NO.MEDICO$
Integer*1 VALIDE.MEDICO, MEDICO.OK%
TVISOR$ = NO.MEDICO$
NO.MEDICO$ = PACK$(Right$(String$(12,"0")+NO.MEDICO$,12))
MEDICO.OK% = -1

If Val(UE.EPS.CVMED$) = 2 Then Begin     ! Si no captura diagnostico
   UE.EPSS.MEDICO$ = String$(12,"0")	   ! Asigna Ceros
   VALIDE.MEDICO  = -1                   ! retorno dato a la funcion
   Exit Function
EndIf

If Val(UE.EPS.CVMED$) NE 0 Then Begin
   TS.ER.RETURN = -1
   Read Form "C6 C40 C12";#UE.OPEN.MEDICO% KEY NO.MEDICO$;	\
	 NO.MEDICO$,UE.EPSS.MNOMB$, UE.EPS.CODALT$
   If TS.ER.RETURN = -1 Then Begin
   	   Call Visores4690(1,Left$(UE.EPSS.MNOMB$,20),Right$(UE.EPSS.MNOMB$,20),800,"C")   	
	    UE.EPSS.MEDICO$ = UNPACK$(NO.MEDICO$)
	 EndIf Else \
		UE.EPSS.MEDICO$ = String$(12,"0")
    VALIDE.MEDICO = TS.ER.RETURN
EndIf Else Begin
   UE.EPSS.MEDICO$ = UNPACK$(NO.MEDICO$)
   !Call Visores4690(1,"MEDICO GENERICO", ".....",500,"C")
   
   UE.EPSS.MNOMB$ = "MEDICO GENERICO"
   VALIDE.MEDICO = -1
EndIf 

End Function
!--- Fin de la funcion de Validacion de medico

Function VALIDE.DIAGNOSTICO(NO.DIAG$)                           						! Vld diagnostico
String NO.DIAG$																															!
Integer*1 VALIDE.DIAGNOSTICO, DIAG.OK%																			!
TVISOR$ = NO.DIAG$																												  !
NO.DIAG$ = PACK$(Right$(String$(6,"0")+NO.DIAG$,6))													!
DIAG.OK% = -1																																!
TS.ER.RETURN = -1																														!
If Val(UE.EPS.CVDIAGNOS$) = 2 Then Begin     																! Si no captura diagnostico
   UE.EPSS.NoDIAG$ = "000000"                																! asigno ceros
   VALIDE.DIAGNOSTICO  = -1                  																! retorno dato a la funcion
   Exit Function																														!
EndIf																																				!
If Val(UE.EPS.CVDIAGNOS$) <> 0 Then Begin																		!
   Read Form "C3 C40 C12";#UE.OPEN.DIAGNOS% KEY NO.DIAG$;                  \!
           NO.DIAG$,UE.EPSS.DIAG$,UE.EPS.CODALT$														!
   If TS.ER.RETURN = -1 Then Begin																					!
      Call Visores4690(1,Left$(UE.EPSS.DIAG$,20),Right$(UE.EPSS.DIAG$,20),800,"C")
	    UE.EPSS.NoDIAG$ = UNPACK$(NO.DIAG$)																		!
	EndIf Else 																														   \!
		UE.EPSS.NoDIAG$ = String$(6,"0")																				!
EndIf Else Begin																														!
   UE.EPSS.NoDIAG$ = UNPACK$(NO.DIAG$)																			!
   !Call Visores4690(1,"DIAGNOSTICO GENERICO","....",500,"C")   						!
   
EndIf																																				!
VALIDE.DIAGNOSTICO = TS.ER.RETURN																						!
End Function																																				!
!--- Fin de la Validacion de diagnosticos

Function VALIDE.IPS(NO.IPS$)
String NO.IPS$
Integer*1 VALIDE.IPS, IPS.OK%

If Val(UE.EPS.CVIPS$) = 2 Then Begin         ! Si no captura diagnostico
   UE.EPSS.IPS$ = "0000"                     ! asigno ceros
   VALIDE.IPS  = -1                          ! retorno dato a la funcion
   Exit Function
EndIf
TVISOR$ = NO.IPS$
NO.IPS$ = PACK$(Right$(String$(4,"0")+NO.IPS$,4))
IPS.OK% = -1
TS.ER.RETURN = -1
If Val(UE.EPS.CVIPS$) <> 0 Then Begin
   Read Form "C2 C40 C12";#UE.OPEN.IPS% KEY NO.IPS$;       \
	      NO.IPS$,UE.EPSS.INOMB$,UE.EPS.CODALT$
   If TS.ER.RETURN = -1 Then Begin
      Call Visores4690(1,Left$(UE.EPSS.INOMB$,20),Right$(UE.EPSS.INOMB$,20),800,"C")
      UE.EPSS.IPS$ = UNPACK$(NO.IPS$)
	EndIf Else \
		UE.EPSS.IPS$ = String$(12,"0")
EndIf Else Begin
   UE.EPSS.IPS$ = UNPACK$(NO.IPS$)
   !Call Visores4690(1,"IPS GENERICO","....",500,"C")
   
EndIf
VALIDE.IPS = TS.ER.RETURN
End Function
!--- Fin de la Validacion de IPS

Function VALIDE.CONVENIO(NO.CONVENIO$)
String NO.CONVENIO$, Gr.Doc$
Integer*4 VALIDE.CONVENIO
NO.CONVENIO$ = PACK$(Right$(String$(16,"0")+NO.CONVENIO$,16))
TVISOR$ = NO.CONVENIO$
UE.OPEN.LIGCONVE% = 23
UE.EPS.ACTFECHA$ = DATE$
Gr.Doc$ = UnPack$(No.Convenio$)
TS.ER.RETURN = -1


Read Form "C8 C40 2C3 C5 C4 4C1 4C3 C5 3C1 4C3 C3 4C1";#UE.OPEN.CONVENIO% KEY NO.CONVENIO$;	\
	UE.EPS.CCOD$,UE.EPS.CNOMB$,UE.EPS.CFECHI$,UE.EPS.CFECHF$,	\
	UE.EPS.CTOT$,UE.EPS.CMAX$,UE.EPS.CVMED$,UE.EPS.CVMERCA$,	\
	UE.EPS.CVIPS$,UE.EPS.CCALCULO$,UE.EPS.CMP$(1),UE.EPS.CMP$(2),	\
	UE.EPS.CMP$(3),UE.EPS.CMP$(4),UE.EPS.CNIT$, UE.EPS.CVDIAGNOS$,	\
	UE.EPS.NRMLoCATAL$,UE.EPS.COPAGOLESS$,				\
	UE.EPS.DSCTLESS$(1), UE.EPS.DSCTLESS$(2),			\
	UE.EPS.DSCTLESS$(3), UE.EPS.DSCTLESS$(4),			\
	UE.EPS.DSCTOTRX$, UE.EPS.ValFAC$, UE.EPS.ValBEN$, UE.EPS.CAPITACION$, \
	UE.EPS.COPIATIQUETE$

If TS.ER.RETURN <> -1 Then Begin 
   UEDLG.LEIDO% = 0 
   Call VISOR.AND.BORRAR("PLAN INGRESADO NO   EXISTE         ")
   VALIDE.CONVENIO = 0 
   Exit Function 
EndIf Else Begin
   
   If Val(UNPACK$(UE.EPS.CMAX$)) = 0 Then UE.EPS.CMAX$ = PACK$("00000000")
   
   UE.EPS.NCONVE$ = " FALTA ACTUALIZAR."
   TS.ER.RETURN = -1
   Asc.Cnv.Line1$ = TO.TRAILERLINE1$
   Asc.Cnv.Line2$ = TO.TRAILERLINE2$
   TO.TRAILERLINE1$ = ""
   TO.TRAILERLINE2$ = ""
   Open "R::$IGCONVE" KEYED RECL 57 AS UE.OPEN.LIGCONVE% UNLOCKED NOWRITE NODEL
   If TS.ER.RETURN = -1 Then Begin
      Read Form "C5 C40 C12";#UE.OPEN.LIGCONVE% KEY UE.EPS.CNIT$;     \
                              UE.EPS.CNIT$,UE.EPS.NCONVE$,UE.EPS.CODALT$
      If UE.OPEN.LIGCONVE% = 23 Then CLOSE 23
      UE.EPS.NCONVE$ = Left$(UE.EPS.NCONVE$+String$(38," "),38)
      UEDLG.LEIDO% = TS.ER.RETURN
   EndIf Else UEDLG.LEIDO% = 0
EndIf
   
If UEDLG.LEIDO% <> 0 Then Begin
	If Val(UE.EPS.ACTFECHA$) >= Val(UNPACK$(UE.EPS.CFECHI$)) AND \
	   Val(UE.EPS.ACTFECHA$) <= Val(UNPACK$(UE.EPS.CFECHF$)) Then Begin
		UEDLG.OK% = 0
		Call Visores4690(1,Left$(UE.EPS.CNOMB$,20),Right$(UE.EPS.CNOMB$,20),1200,"C")
    If Asc.Cnv.Formula% = -1 Then GoTo Pasar.Resto.Datos
    If UE.EPS.CVMED$ NE "2" Then Begin
  		   While (UEDLG.OK% = 0)
		       UE.ERROR% = 0       
		       IOPDATA$ = ""
		       Call CAPTURE.DATOEPS("Ingrese Cod. Medico.","",0)
		       
		        If IOPDATA$ <> "E" Then Begin         	! si presiono ENTER
			         UE.EPSS.LEIDO% = Val(IOPDATA$)
			         UE.EPSS.LEIDO$ = IOPDATA$
			         UEDLG.OK% = VALIDE.MEDICO(UE.EPSS.LEIDO$)
			         If UEDLG.OK% = 0 Then Begin
			            IOPDATA$ = ""
			            Call CAPTURE.DATOEPS("Medico "+UE.EPSS.LEIDO$,"1.(OK)  9.(REPITE)  ",0)
		               If IOPDATA$ <> "E" Then Begin         	! si presiono ENTER
			               TS.TEMP1$ = IOPDATA$
			               If Val(TS.TEMP1$) = 1 Then Begin 
                       UEDLG.OK% = -1
                       UE.EPSS.MEDICO$ = Right$("000000000000"+UE.EPSS.LEIDO$,12)
                       UE.EPSS.MNOMB$ = "MEDICO GENERICO"                      
                     EndIf Else UEDLG.OK% = 0
                   EndIf 			            
			         EndIf 
		       EndIf
		       
		       
		       
		     Wend
     EndIf Else UEDLG.OK% = VALIDE.MEDICO(UE.EPSS.LEIDO$)
     
		 UEDLG.OK% = 0
     If UE.EPS.CVDIAGNOS$ NE "2" Then Begin
			While (UEDLG.OK% = 0)
			   UE.ERROR% = 0
			   IOPDATA$ = ""
			   Call CAPTURE.DATOEPS("Ingrese Cod. Diagnos","",0)
			   
			   
			      If IOPDATA$ <> "E" Then Begin         	! si presiono ENTER
				     UE.EPSS.LEIDO% = Val(IOPDATA$)
				     UE.EPSS.LEIDO$ = IOPDATA$
				     UEDLG.OK% = VALIDE.DIAGNOSTICO(UE.EPSS.LEIDO$)
				     If UEDLG.OK% = 0 Then Begin
				       Call VISOR.AND.BORRAR("DIAGNOSTICO NO      EXISTE         ")
				     EndIf
			      EndIf
			   
			Wend
     EndIf Else UEDLG.OK% = VALIDE.DIAGNOSTICO(UE.EPSS.LEIDO$)
		 UEDLG.OK% = 0
     If UE.EPS.CVIPS$ NE "2" Then Begin 
			 While (UEDLG.OK% = 0)
			   UE.ERROR% = 0        
			    IOPDATA$ = ""
			    Call CAPTURE.DATOEPS("Ingrese Cod. I.P.S. ","",0)

			      If IOPDATA$ <> "E" Then Begin         	! si presiono ENTER
				       UE.EPSS.LEIDO% = Val(IOPDATA$)
				       UE.EPSS.LEIDO$ = IOPDATA$
				       UEDLG.OK% = VALIDE.IPS(UE.EPSS.LEIDO$)
				       If UEDLG.OK% = 0 Then Begin
				          Call VISOR.AND.BORRAR("IPS  INGRESADO NO   EXISTE         ")
				       EndIf
			      EndIf

			 Wend
     EndIf Else UEDLG.OK% = VALIDE.IPS(UE.EPSS.LEIDO$)
     Pasar.Resto.Datos:
		  UEDLG.OK% = 0
	EndIf Else Begin
	    Call VISOR.AND.BORRAR("LA FECHA DEL PLAN   EXPIRO         ")
	    UEDLG.LEIDO% = 0
	EndIf
	     
EndIf

VALIDE.CONVENIO = UEDLG.LEIDO%
End Function
!--- Fin de la Validacion de convenios

Function VALIDE.BENEFIC(No.BENEFIC$)
String No.BENEFIC$, Gr.Doc$
Integer*1 BENEFIC.OK%
Integer*4 VALIDE.BENEFIC
TVISOR$ = No.BENEFIC$
No.BENEFIC$ = Pack$(Right$(String$(16,"0")+UE.EPSS.DAT(1),16)) +\
	      Pack$(Right$(String$(14,"0")+No.BENEFIC$,14))
TVISOR$ = No.BENEFIC$
If UE.EPS.ValBEN$ = "0" Then Begin		 ! Si no Valida beneficiario
 VALIDACION.LOCAL.BENEFICIARIO:      
   VALIDE.BENEFIC = UEDLG.LEIDO%
   BENEFIC.OK% = -1				 ! Init variable
   UE.EPS.BNOMB$ = "BENEFICIARIO GENERICO"
   No.BENEFIC$ = UNPACK$(No.BENEFIC$)
   UE.EPS.BCOD$  = PACK$(Right$("000000000000000000000000000000"+No.BENEFIC$,30))
  EPS.LOOP9:							! Control de ciclo
  KQ$ = ""							! Inite Var.
  While(Val(KQ$)=0)						! Captura categoria
   IOPDATA$ = ""
   Call CAPTURE.DATOEPS("Categoria ...... ?","",0)		! del afiliado
    If IOPDATA$ = "E" Then Begin			        ! Si presiono borrar
       GOTO EPS.SALIDA						! Sale del dialogo
    EndIf							!
   KQ$ = STR$(Val(IOPDATA$))					! Tomo dato capturado
   If Val(KQ$) < 1 OR Val(KQ$) > 4 Then GOTO EPS.LOOP9
   GOTO EPS.DATOOK
  WEnd								!
  EPS.SALIDA:
     UE.EPS.BESTRATO$ = "00"					! Retorno Ceros
     VALIDE.BENEFIC   = 0					! Valor funcion cero
     Exit Function						! Fin de la funcion
  EPS.DATOOK:  
   Call Visores4690(1,"INICIE A INGRESAR","PRODUCTOS",0,"C")
   UE.EPS.BESTRATO$ = Right$("00"+KQ$,2)
   VALIDE.BENEFIC = UEDLG.LEIDO%
   Exit Function				 ! Salgo de la funcion
EndIf
BENEFIC.OK% = -1
TS.ER.RETURN = -1
Gr.Doc$   = UnPack$(No.BENEFIC$)                                                    ! Dato Capturado
Gr.Doc$   = Left$(Gr.Doc$,13) + Right$(Gr.Doc$,14)                                  ! Nit + Id beneficiario
Call Visores4690(1,"Consultando Afiliado",Right$(Gr.Doc$,14),0,"L")
TS.TEMP1$ = Armar.Trama.Msg(GR.CNV.APPL$,"03",GR.DOC$,"00",GR.CNV.CADENA$,"123456") ! Armar trama MSG
TS.TEMP2$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP1$)                ! Ejecuta Requerimiento
TS.TEMP3$ = Valida.Rta(TS.TEMP2$)				                    ! Valida rta entregada
If TS.TEMP3$ = "00" Then Begin                                                      ! Si Proceso OK
   If MID$(TS.TEMP2$,12,2) <> "00" Then Begin                                       ! Si falla de consulta en BD
      Call Visor.And.Borrar(Mid$(TS.TEMP2$,14,40)) 				    ! Reporta Error
      UEDLG.LEIDO% = 0								    ! Falla de consulta
      Exit Function 
   EndIf 
   UE.EPS.BESTRATO$ = Mid$(TS.TEMP2$,84,1)					    ! Asigna Estrato
   UE.EPS.BESTRATO$ = Right$("00"+UE.EPS.BESTRATO$,2)
   UE.EPS.BFECHI$   = Date$
   UE.EPS.BFECHF$   = Date$ 
   UE.EPS.BNOMB$    = Mid$(TS.TEMP2$,54,30)
   UE.EPS.BCOD$     = Pack$(Right$("000000000000000000000000000000"+Right$(Gr.Doc$,14),30))
   If Val(UE.EPS.ACTFECHA$) >= Val(UE.EPS.BFECHI$) AND \
      Val(UE.EPS.ACTFECHA$) <= Val(UE.EPS.BFECHF$) Then Begin
      Call Visores4690(1,Left$(UE.EPS.BNOMB$,20),Right$(UE.EPS.BNOMB$,20),1300,"C")   
      EPS.LOOPX:
       IOPDATA$ = ""
       Call CAPTURE.DATOEPS("Categoria "+UE.EPS.BESTRATO$+" ?","1.Confirma  9.Otra",0)  ! Valida categoria
       If IOPDATA$ = "E" Then Begin			        ! Si presiono borrar
         GOTO EPS.SALIDA						! Sale del dialogo
       EndIf							!
       KQ$ = STR$(Val(IOPDATA$))				! Tomo dato capturado
       If MATCH(KQ$,"19",1) <= 0  Then GoTo EPS.LOOPX           ! Si dato invalido captura nuevamente dialogo
       If Val(KQ$) = 9 Then GoTo EPS.LOOP9                      ! Captura tipo de cuota
       Call Visores4690(1,"INICIE A INGRESAR","PRODUCTOS",0,"C")
   EndIf Else Begin
      Call VISOR.AND.BORRAR("FECHA DEL AFILIADO  EXPIRO")
      UEDLG.LEIDO% = 0
      UEPG.BORRA% = 3
   EndIf
   VALIDE.BENEFIC = UEDLG.LEIDO%
   Exit Function 
EndIf Else Begin 								    ! Falla de comunicaciones
   GoTo VALIDACION.LOCAL.BENEFICIARIO                                               ! Captura Cuota Manual 
EndIf 


End Function

Function VALIDE.ANTERIOR			  ! Validar si cobra coopago
Integer*1 VALIDE.ANTERIOR		          ! 
VALIDE.ANTERIOR = 0
  If ( (Val(UNPACK$(UE.EPS.BCOD$)) = Val(UE.OLD.BENEF$))  AND \   ! Si se cumple la condicion
       (Val(UE.EPSS.MEDICO$) = Val(UE.OLD.MEDICO$)) AND \         ! la funcion retorna 1, si 
       (Val(UNPACK$(UE.EPS.CCOD$)) = Val(UE.OLD.IPS$))    AND \   ! no la cumple retorna 0
       (Val(UE.EPSS.IPS$)    = Val(UE.OLD.IPS$)))   Then \        ! OVS Dic. 11 2001
    VALIDE.ANTERIOR = -1		       \  !
  Else VALIDE.ANTERIOR = 0			  !
End Function
!--- Fin de la funcion de Validacion de beneficiarios/medicos

Function COOPAGO
Integer*1 COOPAGO
ARC% = 0
ARC% = VALIDE.ANTERIOR			! Valida datos anteriores de formula
If ARC% = 0 Then Begin			! Realiza cobro coopago
If UE.EPS.CCALCULO$ = "0" Then Begin
  VISOR% = TS.TOTALS(0,0,0)
  UE.EPS.INDPAGO% = -1
  If Val(UNPACK$(UE.EPS.CMP$(Val(UE.EPS.BESTRATO$)))) <= ABS(TS.TOTALS(0,0,0)) Then Begin

  If UE.EPS.CAPITACION$ = "0" Then \
	   TS.IO.KEYS(7)  = Val("9"+UE.EPS.TIPO$) Else \
	   TS.IO.KEYS(7)  = Val("9"+UE.EPS.TIPOC$)
	   
	If (TS.TOTALS(0,0,0) >= 0) Then \
  	   UE.EPS.VLRCOPAGO$ = STR$(TS.TOTALS(0,0,0)-Val(UNPACK$(UE.EPS.CMP$(Val(UE.EPS.BESTRATO$)))))	\
	Else	\
	   UE.EPS.VLRCOPAGO$ = STR$(TS.TOTALS(0,0,0)+Val(UNPACK$(UE.EPS.CMP$(Val(UE.EPS.BESTRATO$)))))


    If (Val(UE.EPS.VLRCOPAGO$) = 0 And Val(UE.EPS.CAPITACION$) = 0) Then Begin 
	     UE.CTRL.DEVO.EPS% = -1
	     Call Visores4690(1,"NO APLICA COOPAGO EN","ESTA TRANSACCION !!",1500,"L")   	
       UE.EPS.INDPAGO% = -1
	     UE.EPS.GRB% = 0  ! Ind de grabacion String
	     COOPAGO = -1
       DIM TS.IO.KEYS(10):DIM TS.IO.DATA$(10):TS.IO.MOTORKEY = 0	   
	     Exit Function
    EndIf Else Begin 
  	   TS.IO.DATA$(7) = UE.EPS.VLRCOPAGO$
	     TS.IO.KEYS(3)  = 78
	     
	     !TS.IO.DATA$(9) = Unpack$(UE.EPS.CCOD$)
	     
	     If UE.EPS.CAPITACION$ = "0" Then Begin 
	        TS.IO.DATA$(3) = UE.EPS.VARI$
	        TS.IO.MOTORKEY = Val("9"+UE.EPS.TIPO$)
	     EndIf Else Begin 
	        TS.IO.DATA$(3) = UE.EPS.VARIC$
	        TS.IO.MOTORKEY = Val("9"+UE.EPS.TIPOC$)
	        If Val(UE.EPS.VLRCOPAGO$) = 0 And Asc.Cnv.ApplCobro% = -1 Then Begin       ! Si no coopago
	           TS.IO.DATA$(3) = "1"                        ! Reporta todo al efectivo
	           TS.IO.MOTORKEY = 91                         ! y no afectar las formas de pago 
	           TS.IO.KEYS(7)  = 91                         !
	        EndIf  	        
	        If Val(Asc.Cnv.PagoParcial$) <> 0 Then Begin 
	           TS.IO.DATA$(7) = Asc.Cnv.PagoParcial$
	           
	           !TS.IO.DATA$(9) = Asc.Cnv.NumNit$
	           
	           UE.EPS.VLRCOPAGO$ = Asc.Cnv.PagoParcial$
	        Endif 
	     EndIf 
	EndIf
	EndIf Else Begin
	 If (Val(UE.EPS.COPAGOLESS$) = 0) And UE.EPS.CAPITACION$ = "0" Then Begin
                Call Visores4690(1,"NO APLICA COOPAGO EN","ESTA TRANSACCION !!",1500,"C")   	
		UE.EPS.GRB% = 0  ! Ind de grabacion String
		UE.EPS.INDPAGO% = -1
		UE.CTRL.DEVO.EPS% = -1
		COOPAGO = -1
                DIM TS.IO.KEYS(10):DIM TS.IO.DATA$(10):TS.IO.MOTORKEY = 0	   		
		Exit Function
	 EndIf Else Begin
!*******************************************************************************
! SI DSCTO * < Valor DE COPAGO

  If UE.EPS.CAPITACION$ = "0" Then \
	  TS.IO.KEYS(7)  = Val("9"+UE.EPS.TIPO$) Else \
	  TS.IO.KEYS(7)  = Val("9"+UE.EPS.TIPOC$)
	  TS.TEMP2I4 = ROUND(Val(UNPACK$(UE.EPS.CMP$(Val(UE.EPS.BESTRATO$))))/FLOAT(1000)*FLOAT(TS.TOTALS(0,0,0)-TS.MISCTRX.AMT),0,0)
	  UE.EPS.VLRCOPAGO$ = STR$(ROUND(Val(UE.EPS.DSCTLESS$(Val(UE.EPS.BESTRATO$)))/FLOAT(1000)*FLOAT(TS.TOTALS(0,0,0)-TS.MISCTRX.AMT),0,0))
	  TS.IO.DATA$(7) = UE.EPS.VLRCOPAGO$
	  TS.IO.KEYS(3)  = 78
	  If UE.EPS.CAPITACION$ = "0" Then Begin 
	   TS.IO.DATA$(3) = UE.EPS.VARI$
	   TS.IO.MOTORKEY = Val("9"+UE.EPS.TIPO$)
  	  EndIf Else Begin 
	   TS.IO.DATA$(3) = UE.EPS.VARIC$
	   TS.IO.MOTORKEY = Val("9"+UE.EPS.TIPOC$)	
	   If Val(UE.EPS.VLRCOPAGO$) = 0 And Asc.Cnv.ApplCobro% = -1 Then Begin       ! Si no coopago
	      TS.IO.DATA$(3) = "1"                        ! Reporta todo al efectivo
	      TS.IO.MOTORKEY = 91                         ! y no afectar las formas de pago 
	      TS.IO.KEYS(7)  = 91                         !
	   EndIf  	        
          EndIf 
	  UE.EPS.INDPAGO% = -1
	  If Val(UE.EPS.VLRCOPAGO$) = 0 Then UE.CTRL.DEVO.EPS% = -1
!*******************************************************************************
   EndIf
 EndIf
EndIf Else Begin
  If UE.EPS.CAPITACION$ = "0" Then \
	   TS.IO.KEYS(7)  = Val("9"+UE.EPS.TIPO$) Else \
	   TS.IO.KEYS(7)  = Val("9"+UE.EPS.TIPOC$)
	TS.TEMP2I4 = ROUND(Val(UNPACK$(UE.EPS.CMP$(Val(UE.EPS.BESTRATO$))))/FLOAT(1000)*FLOAT(TS.TOTALS(0,0,0)-TS.MISCTRX.AMT),0,0)
	UE.EPS.VLRCOPAGO$ = STR$(ROUND( (1000.0-Val(UNPACK$(UE.EPS.CMP$(Val(UE.EPS.BESTRATO$)))))/FLOAT(1000)*FLOAT(TS.TOTALS(0,0,0)-TS.MISCTRX.AMT),0,0))
	TS.IO.DATA$(7) = UE.EPS.VLRCOPAGO$
	TS.IO.KEYS(3)  = 78
	If UE.EPS.CAPITACION$ = "0" Then Begin 
	   TS.IO.DATA$(3) = UE.EPS.VARI$
	   TS.IO.MOTORKEY = Val("9"+UE.EPS.TIPO$)
	EndIf Else Begin 
	   TS.IO.DATA$(3) = UE.EPS.VARIC$
	   TS.IO.MOTORKEY = Val("9"+UE.EPS.TIPOC$)	
	   If Val(UE.EPS.VLRCOPAGO$) = 0 And Asc.Cnv.ApplCobro% = -1 Then Begin       ! Si no coopago
	      TS.IO.DATA$(3) = "1"                        ! Reporta todo al efectivo
	      TS.IO.MOTORKEY = 91                         ! y no afectar las formas de pago 
	      TS.IO.KEYS(7)  = 91                         !
	   EndIf  	        
     If Val(Asc.Cnv.PagoParcial$) <> 0 Then Begin 
        TS.IO.DATA$(7) = Asc.Cnv.PagoParcial$
        
        !TS.IO.DATA$(9) = Asc.Cnv.NumNit$
        
        UE.EPS.VLRCOPAGO$ = Asc.Cnv.PagoParcial$
     Endif 
  EndIf 
	UE.EPS.INDPAGO% = -1
	If Val(UE.EPS.VLRCOPAGO$) = 0 Then UE.CTRL.DEVO.EPS% = -1	
EndIf
EndIf Else Begin
     Call Visores4690(1,"FACTURA EXCENTA DE","COBRO BENEFICIARIO",1500,"C")   	
     UE.EPS.COPAGOLESS$ = "0"
     If UE.EPS.CAPITACION$ = "0" Then \
        TS.IO.KEYS(7)  = Val("9"+UE.EPS.TIPO$) Else \
        TS.IO.KEYS(7)  = Val("9"+UE.EPS.TIPOC$)
     TS.IO.DATA$(7) = STR$(TS.TOTALS(0,0,0))
	   TS.IO.KEYS(3)  = 78
	   If UE.EPS.CAPITACION$ = "0" Then Begin 
	      TS.IO.DATA$(3) = UE.EPS.VARI$
	      TS.IO.MOTORKEY = Val("9"+UE.EPS.TIPO$)
	   EndIf Else Begin 
	      TS.IO.DATA$(3) = UE.EPS.VARIC$
	      TS.IO.MOTORKEY = Val("9"+UE.EPS.TIPOC$)
	      If Val(UE.EPS.VLRCOPAGO$) = 0 And Asc.Cnv.ApplCobro% = -1 Then Begin       ! Si no coopago
	         TS.IO.DATA$(3) = "1"                        ! Reporta todo al efectivo
	         TS.IO.MOTORKEY = 91                         ! y no afectar las formas de pago 
	         TS.IO.KEYS(7)  = 91                         !
	      EndIf  	        
	      If Val(Asc.Cnv.PagoParcial$) <> 0 Then Begin 
	         TS.IO.DATA$(7) = Asc.Cnv.PagoParcial$
	         
	         !TS.IO.DATA$(9) = Asc.Cnv.NumNit$
	         
	         UE.EPS.VLRCOPAGO$ = Asc.Cnv.PagoParcial$
	      Endif 	      
	   EndIf 
	   UE.EPS.INDPAGO% = -1
EndIf
COOPAGO = 0   ! Retorna de la funcion
End Function
!--- Fin de aplicacion de coopagos

Function FACTURA.AUTOMATICA
Integer*4 X.Num%
String X.Llave$, X.Dat1$, X.Dat2$, X.Dat3$, FACTURA.AUTOMATICA

X.Llave$ = Pack$("9999999999999999")
Ts.Er.Return = -1
Read Form "C8 C3 C95";#UE.OPEN.CONVENIO% KEY X.Llave$;	\
     X.Dat1$, X.Dat2$, X.Dat3$
If Ts.Er.Return = -1 Then Begin 
   X.Dat2$ = UnPack$(X.Dat2$)
   X.Dat2$ = Str$( Val (X.Dat2$) + 1 )			! Aumenta consecutivo
   X.Num% = Val(X.Dat2$)				! Asigna numero calculado
   X.Dat2$ = Pack$(Right$("000000"+X.Dat2$,6))
   Write Form "C8 C3 C95";#UE.OPEN.CONVENIO%; X.Llave$, \
         X.Dat2$, X.Dat3$
EndIf Else Begin 
   X.Num% = 1 
   X.Dat2$ = Pack$("000001")
   X.Dat3$ = String$(95," ")
   Write Form "C8 C3 C95";#UE.OPEN.CONVENIO%; X.Llave$, \
         X.Dat2$, X.Dat3$
EndIf 
Factura.Automatica = Str$(X.Num%)			! Retorna dato de la funcion
End Function 


Sub LEA.EPS
String X.Convenio$, X.Factura$, X.IPS$, X.Coopago$, X.tmp$
 LOCKDEV 32,PURGE                                ! Bloquea Teclado
 Asc.Cnv.Formula% = 0                            ! Si dato capturado es una formula medica
 UEPG.BORRA% = 0          
 UE.EPSS.ASK% = 1
 UEDLG.OK% = 0
 UE.EPSS.MEDICO$ = PACK$(String$(12,""))
 UE.EPSS.IPS$    = PACK$(String$(12,""))
 UE.EPS.DATO$    = "" 
 X.Convenio$ = ""
 X.Factura$ = ""
 X.IPS$ = ""
 X.Coopago$ = ""
 While (UEDLG.OK% = 0)
  CICLO.CAPTURA.CNV:
   UE.ERROR% = 0
   If UE.EPSS.ASK% = 2 Then Begin                                   ! Numero documento
    If X.factura$ <> "" Then Begin                                  ! Dato escaneado - Comfandi
       UE.EPS.DATO$ = X.FACTURA$                                    ! Asigna documento
       UEDLG.LEIDO% = -1                                            ! Reporta lectura correcta
       GoTo Valida.Entrada                                          ! Omite ingreso del dato
    EndIf 
   EndIf 
   
   If UE.EPSS.ASK% = 3 Then Begin                                   ! Captura de beneficiario
    If Asc.Cnv.Formula% = -1 Then \                                 ! Si ya paso formula medica - Comfandi
       GoTo Valida.Entrada                                          ! No captura dato
   EndIf 
   IOPDATA$ = ""
   UE.EPS.DATO$ = ""
   Call CAPTURE.DATOEPS(UE.EPSS.DESC(UE.EPSS.ASK%),"",0)						! Captura Dialogo
      
   If (IOPDATA$ <> "E") And (Val(IOPDATA$) <> 0) Then Begin 
      If Len(IOPDATA$) = 13 Then Begin 
        If Left$(IOPDATA$,1) = "1" Then \
           IOPDATA$ = Right$(IOPDATA$,12)
      EndIf    
      UEDLG.LEIDO% = Val(IOPDATA$)
      UE.EPS.DATO$ = IOPDATA$

      
   VALIDA.ENTRADA:
!--- Lineas Adicionadas para manejo formatos de Comfandi - OVS 09 May 2006      
      If (UE.EPSS.ASK% = 1 AND Len(UE.EPS.DATO$) = 20) Then Begin             ! Datos Mini Factura - Comfandi
         X.Convenio$ = Mid$(UE.EPS.DATO$,9,12)    ! Codigo del Convenio
         X.Factura$  = Mid$(UE.EPS.DATO$,1, 7)    ! Numero de la factura
      EndIf 
!--- Se modifica la longitud capturada del codigo de barras pasando de 24 a 28 posiciones
!--- Mod el 11Abr2007 OVS

      If (UE.EPSS.ASK% = 1 AND Len(UE.EPS.DATO$) = 24) Then Begin             ! Datos Formula Medica
         UE.EPS.DATO$ = "0000" + UE.EPS.DATO$															  ! Si formato antiguo lo ajusta al nuevo
      Endif 

      If (UE.EPSS.ASK% = 1 AND Len(UE.EPS.DATO$) = 28) Then Begin             ! Datos Formula Medica
         Asc.Cnv.Epcop$       = Mid$(UE.EPS.DATO$, 1,02)                    ! Entidad pago convenio
         Asc.Cnv.PtgCuota$    = Mid$(UE.EPS.DATO$, 3,03)                    ! Porcentaje cuota moderadora         
         X.Convenio$ = Mid$(UE.EPS.DATO$,16,12)                             ! Codigo del Convenio    
         X.Factura$  = Mid$(UE.EPS.DATO$, 9, 7)                             ! Numero de la factura   
         X.IPS$      = Mid$(UE.EPS.DATO$, 5, 4)                             ! Codigo de la IPS       
         X.Coopago$  = Mid$(UE.EPS.DATO$,28, 1)                             ! Codigo Coopago         
         If Val(X.Coopago$) = 0 Then X.Coopago$ = "4"                       ! Si plan excento de pago
         Asc.Cnv.Formula% = -1
      EndIf 
      
!--- Fin de lineas adicionadas
      
!--- Validacion del convenio
      If UE.EPSS.ASK% = 1 Then Begin                                        ! Captura del convenio

       If Len(X.CONVENIO$) > 2 Then UE.EPS.DATO$ = X.CONVENIO$              ! Si capturado por scanner - Comfandi 

	     UEDLG.LEIDO% = VALIDE.CONVENIO(UE.EPS.DATO$)                         ! Valida Convenio 
	                           
	     If UEDLG.LEIDO% = 0 Then \                                           ! Si dato errado                        
	        UE.EPSS.ASK% = UE.EPSS.ASK% - 1  Else \                           !                                       
	       Begin                                                              !                                       
	        If Asc.Cnv.Formula% = -1 Then Begin                                                                      
	    UE.EPS.BNOMB$ = "BENEFICIARIO GENERICO"                         ! Nombre beneficiario                   
            UE.EPS.BCOD$  = Pack$(X.CONVENIO$ + String$(14,"0"))            ! Codigo Beneficiario                   
            UE.EPS.BESTRATO$ = Right$("00"+X.Coopago$,2)                    ! Coopago a cobrar                      
            UE.EPSS.IPS$ = X.IPS$                                           ! Asigna IPS                            
            UE.EPSS.MEDICO$ = String$(12,"0")                               ! Medico generico                       
            UE.EPSS.MNOMB$  = "MEDICO GENERICO"                             !                                       
            UE.EPSS.NoDIAG$ = String$(6,"0")                                ! Diagnostico generico                  
            UE.EPSS.DIAG$   = "DIAGNOSTICO GENERICO"                        !                                       
            UE.EPSS.INOMB$  = "IPS FORMULA"                                 !                                       
          EndIf 
	       EndIf 
      EndIf
      
!--- Validacion numero de documento/formula

      If (UE.EPSS.ASK% = 2) Then Begin                           ! Captura formuna
	If UEDLG.LEIDO% = 0 Then Begin                           ! SI NUMERO FACTURA ES 0 ASIGNACION AUTOMATICA FECHA+HORA
	 ! UEDLG.LEIDO% = Val(DATE$+Left$(TIME$,4))              !
	 
           X.TMP$ = FACTURA.AUTOMATICA				 ! Numero consecutivo del almacen
           X.Tmp$ = TS.STORE$ + Right$("000000"+X.tmp$,6)        ! Consecutivo Armando
           UEDLG.LEIDO% = Val(X.Tmp$)                            ! Asigna numero consecutivo
        EndIf 
      EndIf
      
!--- Validacion de beneficiario
      If (UE.EPSS.ASK% = 3) Then Begin
       If Asc.Cnv.Formula% = 0 Then Begin 
	        UEDLG.LEIDO% = VALIDE.BENEFIC(UE.EPS.DATO$)
	        If UEDLG.LEIDO% = 0 Then UE.EPSS.ASK% = 3 : GoTo CICLO.CAPTURA.CNV:  ! UE.EPSS.ASK% = UE.EPSS.ASK% - 1 Linea comentariada OVS 12 FEb 2008
	     EndIf Else Begin 
	       UE.EPS.DATO$ = UE.EPS.BCOD$
	       Call TSHIECET
         Call Visores4690(1,"INICIE A INGRESAR","PRODUCTOS",0,"C")
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
         TS.IO.MOTORKEY = 0 
	     EndIf 
      EndIf

      UE.EPSS.DAT(UE.EPSS.ASK%) = UE.EPS.DATO$		! Almacena datos capturados
      A$ = UE.EPSS.DAT(UE.EPSS.ASK%)			        ! 
      UE.EPSS.ASK% = UE.EPSS.ASK% + 1			        ! Siguiente	
      
      If UE.EPSS.ASK% = 2 Then Begin			        ! Si captura de documento
         If UE.EPS.ValFAC$ = "0" Then Begin     	! Si no captura documento
            UE.EPSS.ASK% = UE.EPSS.ASK% + 1		    ! Aumento contador
  	        UE.EPSS.DAT(2) = DATE$+Left$(TIME$,4)	! Asigna Nro Automatico
         EndIf         
      EndIf
      If UE.EPSS.ASK% = 4 Then UEDLG.OK% = 1:UE.EPSS.ON = -1

   EndIf Else UEPG.BORRA% = UEPG.BORRA% + 1
   If UEPG.BORRA% >= 3 Then UEDLG.OK% = 1:UE.EPSS.ON = 0

Wend

End Sub


Function Valida.Tipo.Articulo(X.Code$) 	Public  									! Valida tipo de Articulo
Integer*1 Valida.Tipo.Articulo																		!
String    X.Code$, X.Tmp$																					!
!%Include CNVITEMR.J86																							! Variables temporales

TS.ER.RETURN = -1
!%Include CNVIRRD1.J86          																	! Lectura maestro de articulos

 Call IRRFEC.READ01 (X.Code$, 4, TS.TEMP1$, 0)                    ! Lectura Datos Itemr
 Call SPLIT.ITEMFILE( TS.TEMP1$ ) 			                          ! Entrega datos al eamitemr.j86

If TS.ER.RETURN = -1 Then BEGIN 
   Asc.Cnv.Name$ = GR.ITEMNAME$
EndIF Else BEGIN
   Asc.Cnv.Name$ = "NO DEFINIDO"
EndIF
 
X.Tmp$ = Right$(Unpack$(GR.INDICAT2$),1)													! Toma el tipo de articulo
If Val(X.tmp$) = 5 Then Begin  																		! Si metodo de precio 5
   Valida.Tipo.Articulo = -1
   X.Tmp$ = Unpack$(GR.SaleQuan$)																	! Toma Primera parte del PLU
   X.Tmp$ = X.Tmp$ + Unpack$(GR.Salepric$)                        ! Toma segunda parte del PLU
   Ue.Eps.Cod5$ = Pack$(X.Tmp$)                                   ! Empaqueta el dato
EndIf Else Valida.Tipo.Articulo = 0                               ! Item Normal

End Function 
!--- Fin validacion de Item


!-- Se modifica duplicado tiquete segun requerimiento de Colsanitas
!-- Desarrollado el 11 de Nov de 2004 por Oscar Valencia
!
Sub COPIA.CLIENTE                                            				! Duplicado tiquete para Colsanitas
Integer*1 X.Ajuste%
Integer ARC2%, AA%, BB%
String UELF$, LETRA$, HOLD$,RELB$, XX$, X.Tmp$, X.Alto$, X.Bajo$, X.Signo$, X.LF$, X.Str$
String XPrecio$
Integer*4 X.I%, X.Qty%, X.Indicat2%
Integer*1 X.J%, X.IndArt%
 X.LF$ = ""
 If Prt4610.Enable Ne 0 Then Begin									! Impresoras 4610
    X.LF$ = Chr$(0Dh)
 EndIf	
 
 If UE.EPS.COPIATIQUETE$ = "0" Then Exit Sub            ! No Genera copia tiquete
 											!
TO.USEREXIT(20) = 0
TO.USEREXIT(60) = 0

If UE.EPS.COPIA% = 2 Then Begin

  If PRT4610.ENABLE = 0 Then X.Ajuste% = 38 Else X.Ajuste% = 40
  !Call U.Imprime(TO.HEADERLINE1$,4100H)																		!
  !Call U.Imprime(TO.HEADERLINE2$,4100H) 																	!
  
  !Call U.Imprime("    ***** COPIA CLIENTE *****         ",4100H)					!
!-- Modificado OVS 19 Junio 2008 solicitado por Comfandi
  

  If UE.EPS.CAPITACION$ = "0" Then BEGIN
   If Prt4610.Enable Ne 0 Then Begin		! Impresoras 4610
    TS.TEMP2$ = CHR$(1BH)+CHR$(21H)+CHR$(10H) + "* USUARIO *" ! IMPRESORA 4
   EndIf Else TS.TEMP2$ = CHR$(27) + CHR$(14) + "* USUARIO *"
  Call U.IMPRIME(TS.TEMP2$,6100H)
  
    ENDIF ELSE BEGIN   
   If Prt4610.Enable Ne 0 Then Begin		! Impresoras 4610
  TS.TEMP2$ = CHR$(1BH)+CHR$(21H)+CHR$(10H) + "* COMFANDI * "
   EndIf Else TS.TEMP2$ = CHR$(27) + CHR$(14) + "* COMFANDI *" !IMPRESORA 4
   
  Call U.IMPRIME(TS.TEMP2$,6100H)
  ENDIF 

  !Call U.Imprime(String$(38,"*"),4100H)
  Call U.Imprime("--   IDENTIFICACION DEL CONVENIO    --",4100H)

  X.Str$ = Str$(Val(unpack$(UE.EPS.CNIT$)))
  X.Str$ = Left$("Numero NIT     : "+X.Str$+String$(X.Ajuste%," "),X.Ajuste%)
  Call U.Imprime(X.Str$,6100H)

  X.Str$ = Left$("Razon Social   : "+Left$(UE.EPS.NCONVE$,21)+String$(X.Ajuste%," "),X.Ajuste%)
  Call U.Imprime(X.Str$,4100H)

  X.Str$ = Left$("Codigo del Plan: "+UNPACK$(UE.EPS.CCOD$)+String$(X.Ajuste%," "),X.Ajuste%)
  Call U.Imprime(X.Str$,4100H)
  X.Str$ = Left$("Nombre del Plan: "+Left$(UE.EPS.CNOMB$,21)+String$(X.Ajuste%," "),X.Ajuste%)
  Call U.Imprime(X.Str$,4100H)
  
!  X.Str$ = Left$("Beneficiario   : "+Left$(UE.EPS.BNOMB$,21)+String$(X.Ajuste%," "),X.Ajuste%)
!  Call U.Imprime(X.Str$,4100H)

  X.Str$ = Left$("Solicitud Nro. : "+Right$(UE.EPSS.FORMULA$,12)+String$(X.Ajuste%," "),X.Ajuste%)
  Call U.Imprime(X.Str$,4100H)


!-- Linea ajustada 24 Jul 2008
  If UE.EPS.CAPITACION$ = "0" Then Begin   ! Si no es capitacion 
     X.Str$ = Left$("Documento Nro.: "+Right$(UNPACK$(UE.EPS.BCOD$),14)+String$(X.Ajuste%," "),X.Ajuste%)
     Call U.Imprime(X.Str$,4100H)
  EndIf 
!-- Fin lineas ajustadas

  
  X.Str$ = Left$("Codigo Medico  : "+Right$(UE.EPSS.MEDICO$,12)+String$(X.Ajuste%," "),X.Ajuste%)
  Call U.Imprime(X.Str$,4100H)
  X.Str$ = Left$("Codigo I.P.S   : "+Right$(UE.EPSS.IPS$,12)+String$(X.Ajuste%," "),X.Ajuste%)
  Call U.Imprime(X.Str$,4100H)
  X.Str$ = Left$("Categoria      : "+UE.EPS.BESTRATO$+String$(X.Ajuste%," "),X.Ajuste%)
  Call U.Imprime(X.Str$,4200H)
  !Call U.Imprime(String$(38,"*"),4100H)  
  !Call U.Imprime("   CODIGO           DESCRIPCION",4140H)

  X.IndArt% = 0
  For X.I% = 1 TO Asc.Cnv.SlEnd% + 1               												! Recorre todos los String
    H$ = READ.SL.STR$(X.I%)                 															! Toma el String
    If LEN(H$) > 5 Then     Begin          														    ! Si el String es OK
     If ASC(H$)  = 1 Then Begin            															  ! Si es un articulo
        If X.IndArt% = 1 Then Begin                                       !
            Call U.Imprime("Cantidad : "+Str$(X.Qty%)+" $"+Xprecio$,4100H)! Imprime dato de copia
        EndIf Else X.IndArt% = 1
        X.TMP$ = ASIC.GETUNPK(H$,3)	              												! Articulo vEndido
        X.Tmp$ = Pack$(Right$("000000000000"+X.Tmp$,12))
        X.J% = Valida.Tipo.Articulo(X.Tmp$)                               ! Tipo de articulo
        X.TMP$ = ASIC.GETUNPK(H$,3)	              												! Articulo vEndido
        X.Tmp$ = Right$("            "+Str$(Val(X.Tmp$)),12)              ! Ajusta formato Item
        X.Tmp$ = X.Tmp$ + "        "+ Asc.Cnv.Name$                       ! Arma String impresion
        Call U.Imprime(X.Tmp$,4100H)              												! Imprime dato de copia
        X.SIGNO$ = ""
        Asc.Tmp.Apun% = 3																				          ! Posiciona apuntador  
        X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)                  					! CODIGO PLU O EAN
        X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)                 					! PRECIO DEL ARTICULO
        Call Format.Amount(Val(X.tmp$))                                    ! Formatea precio
        XPrecio$ = TS.TEMP1$																							! Dato Formateado
        X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)              	 	  			  ! DEPARTAMENTO DEL ARTICULO
        X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)														! Cupon Family 3 y 3 Long 6
        X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%) 											    ! INDICAT1  PARA IMPUESTOS, PESABLES, ALIAS  ETC...
        X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)                           ! X.Indicat2%   QUE SE OPRIMIO ?  KEY CANCEL, KEY DEVOLUC, KEY REFOUND
        X.Indicat2% = VAL(X.Tmp$) 
        If (X.Indicat2% AND 0080H)  Then X.Signo$ = "-"    
        If (X.Indicat2% AND 0040H)  Then X.Signo$ = "-"    
        X.Qty% = 1
        If X.SIGNO$ = "-" Then Begin 
           X.Qty% = -1
        EndIf         
        GoTo Next.Lectura
     EndIf                                  															!

     If ASC(H$)  = 2 Then Begin            															  ! Si es una extencion de articulo
         Asc.Tmp.Apun% = 3																								! Posiciona apuntador
         X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)													! NO USADO   MPGROUP= Val(A$)
         X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)													! NO USADO   DEALQUAN= Val(A$)
         X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)													! NO USADO   PRICE METH= Val(A$)
         X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)													! NO USADO   SALEQUAN= Val(A$)
         X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)													! NO USADO   SALEPRIC= Val(A$)
         X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)													! CANTIDAD O PESO VEndIDO = QTYORWGT        
         If X.Qty% > 0 Then \ 																					  ! Si venta
            X.Qty% = Val(X.Tmp$) Else																		 \! Retorna Valor Sino Reporta devolucion
            X.Qty% = Val(X.Tmp$) * -1																			! Retorna Valor 
            Call U.Imprime("Cantidad : "+Str$(X.Qty%)+" $"+Xprecio$,4100H)! Imprime dato de copia
         X.IndArt% = 2																										! Control Impresion Dato
         GoTo Next.Lectura
     EndIf 
    EndIf 
    Next.Lectura:
     
  Next X.I%
  If X.IndArt% = 1 Then Begin                                             !
     Call U.Imprime("Cantidad : "+Str$(X.Qty%)+" $"+Xprecio$,4100H) 		  ! Imprime dato de copia
  EndIf 
!     Call U.Imprime(String$(19,"- "),4100H)

!-- Se adiciona impresion del vendedor y nombre del vendedor
!-- segun requerimiento de Comfandi 28 Mzo 2007 

     X.tmp$ = "Vendedor : "+vend.medica$ + " " + nom.oper$ 
     X.Tmp$ = Left$(X.Tmp$,38)
     
     Call U.Imprime(X.tmp$,6100H)
     
     Asic.Detalle% = -1 
     X.Tmp$ = Linea.Detalle(23)                                              ! Store Line
     Asic.Detalle% = 0 
     Call U.Imprime(X.Tmp$,4100H)                     											  ! Impresion Store Line
     Call U.Imprime(" ",4900H)																								!
     If PRT4610.ENABLE = 0 Then \                                           ! Si la impresora es una modelo 4
       Call U.Imprime(" ",4500H)																						!
     Call U.CORTACR																													! Corta Papel
     Call U.Imprime(TO.HEADERLINE1$,4100H)																		!
     Call U.Imprime(TO.HEADERLINE2$,4100H) 																	!
  UE.EPSS.ON = 0
  UE.EPS.COPIA% = 0
ENDIF   
TO.USEREXIT(20) = -1
TO.USEREXIT(60) = -1

End Sub
!--- Fin de la copia del cliente


Sub VALIDACION.LINEA.PLU(TKEY$) 						! Validacion en linea del plu-convenio
String Tkey$, GR.DOC$, TTmp$
    Ttmp$ = Str$(Val(Right$(UnPack$(Tkey$),12)))
    Call TSHIECET								! Tono de alerta
    Call VISORES4690(1,"VALIDANDO EN LINEA",Ttmp$,0,"L")		        ! Presenta dato en visor
    GR.DOC$ = Right$(String$(26," ")+UnPack$(TKEY$),26)	    		        ! Numero de plu-convenio a consultar

    TS.TEMP1$ = Armar.Trama.Msg(GR.CNV.APPL$,"01",GR.DOC$,"00",GR.CNV.CADENA$,"123456") ! Armar trama MSG
    TS.TEMP2$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP1$)        ! Ejecuta Requerimiento
    TS.TEMP3$ = Valida.Rta(TS.TEMP2$)				                ! Valida rta entregada
    If TS.TEMP3$ <> "00" Then Begin 
       TS.ER.RETURN = 0 						        ! Error de comunicacion
       Exit Sub 
    EndIf Else Begin
       If MID$(TS.TEMP2$,12,2) <> "00" Then Begin 
          Call Visor.And.Borrar(Mid$(TS.TEMP2$,14,40)) 				! Reporta Error
          TS.ER.RETURN = 0 						        ! 
          Exit Sub 
       EndIf 
    EndIf 
    UE.EPS.CIPRECIO$ = Str$(Val(Mid$(TS.TEMP2$,54,9)))				! Asigna el valor
    UE.EPS.CIfECHI$  = Pack$(Mid$(TS.TEMP2$,65,6))     				! Fecha Inicial
    UE.EPS.CIfECHF$  = Pack$(Mid$(TS.TEMP2$,73,6))     				! Fecha Final Precio
    UE.EPS.CIPRECIO$ = Pack$(Right$("00000000"+UE.EPS.CIPRECIO$,8))             ! Empaqueta dato

!    Write Form "C14 C4 C3 C3";#UE.OPEN.PLU% ; TKEY$,                           \! Almacena registro localmente
!          UE.EPS.CIPRECIO$,UE.EPS.CIfECHI$,UE.EPS.CIfECHF$

    TS.ER.RETURN = -1               
End Sub 
!--- Fin validacion en linea plu-convenio

Sub Genera.Codigo.CUM	
String Xcum$, Xkey$, XDUMMY$
  TS.ER.RETURN = -1 
  Open "R::$F:ITMCUM" KEYED RECL 78 AS 87	UNLOCKED NOWRITE NODEL								! Abre codigos cum items
  If TS.ER.RETURN <> -1 Then Begin											! Error en la apertura
     Exit Sub 	 
  EndIf
  
  Xkey$ = Pack$(Right$("000000000000"+SL.IT.ITEMCODE$,12))   ! ITM A BUSCAR
  TS.ER.RETURN = -1 
  READ FORM "C6 C12 C60";#87 KEY Xkey$;    \! SESSION NUMBER AND RECORD KEY
            XDUMMY$, XCUM$, XDUMMY$

  If TS.ER.RETURN <> -1 Then \
  	 Xcum$ = "           0"
  	
  Xcum$ = "CUM: "+Xcum$
  TS.TEMP1$ = Left$(XCUM$+STRING$(37," "),37)
  Call U.IMPRIME(TS.TEMP1$,6100H)
  Close 87
 
End Sub 

!---------------------------------
!--- User Exit Modulo Terminal
!---------------------------------

Sub EPSMTS01.011 Public                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Sept de 2004                                        **
!   **    Proyecto   :  Grupo Retail Ltda                            **
!   **    User Exit  :  EPSMTS01.011                                         **
!   **    Inclu¡r en :  EAMTSU01.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura Valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
String X.Str$, X.Tipo$, X.Tmp$
Integer*1 X.Ajuste%, XI%

If UE.EPS.EPSS% = -1 Then \
Begin
	%Include CNVTSU01.011				! CONVENIOS Y  E.P.S
EndIf
End Sub                                                 !
!------------------------------------------------------------------------------
 
Sub EPSMTS02.011 Public                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Sept de 2004                                        **
!   **    Proyecto   :  Grupo Retail Ltda                            **
!   **    User Exit  :  EPSMTS02.011                                         **
!   **    Inclu¡r en :  EAMTSU02.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura Valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
If UE.EPS.EPSS% = -1 Then \
Begin
	%Include CNVTSU02.011				! CONVENIOS Y  E.P.S
EndIf
End Sub                                                 !
!------------------------------------------------------------------------------
  
Sub EPSMTS07.011 Public                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Sept de 2004                                        **
!   **    Proyecto   :  Grupo Retail Ltda                            **
!   **    User Exit  :  EPSMTS07.011                                         **
!   **    Inclu¡r en :  EAMTSU07.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura Valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
Integer*2 XI%

	%Include CNVTSU07.011				! CONVENIOS Y  E.P.S

End Sub                                               !
!------------------------------------------------------------------------------

Sub EPSMTS08.011 Public                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Sept de 2004                                        **
!   **    Proyecto   :  Grupo Retail Ltda                            **
!   **    User Exit  :  EPSMTS08.011                                         **
!   **    Inclu¡r en :  EAMTSU08.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura Valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
String XTmp$, XKEY$

If UE.EPS.EPSS% = -1 Then \
Begin
	%Include CNVTSU08.011				! CONVENIOS Y  E.P.S
EndIf
End Sub                                                 !
!------------------------------------------------------------------------------

Sub EPSMTS14.011 Public                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Sept de 2004                                        **
!   **    Proyecto   :  Grupo Retail Ltda                            **
!   **    User Exit  :  EPSMTS14.011                                         **
!   **    Inclu¡r en :  EAMTSU14.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura Valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
If UE.EPS.EPSS% = -1 Then \
Begin
	%Include CNVTSU14.011				! CONVENIOS Y  E.P.S
EndIf
End Sub                                                 !
!------------------------------------------------------------------------------

Sub EPSMTS20.011 Public                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Sept de 2004                                        **
!   **    Proyecto   :  Grupo Retail Ltda                            **
!   **    User Exit  :  EPSMTS20.011                                         **
!   **    Inclu¡r en :  EAMTSU20.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura Valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
If UE.EPS.EPSS% = -1 Then \
Begin
	%Include CNVTSU20.011				! CONVENIOS Y  E.P.S
EndIf
End Sub                                                 !
!------------------------------------------------------------------------------

Sub EPSMTS20.012 Public                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Sept de 2004                                        **
!   **    Proyecto   :  Grupo Retail Ltda                            **
!   **    User Exit  :  EPSMTS20.012                                         **
!   **    Inclu¡r en :  EAMTSU20.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura Valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
If UE.EPS.EPSS% = -1 Then \
Begin
	%Include CNVTSU20.012				! CONVENIOS Y  E.P.S
EndIf
End Sub                                                 !
!------------------------------------------------------------------------------


Sub EPSMTS23.011 Public                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Sept de 2004                                        **
!   **    Proyecto   :  Grupo Retail Ltda                            **
!   **    User Exit  :  EPSMTS23.011                                         **
!   **    Inclu¡r en :  EAMTSU23.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura Valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
!If UE.EPS.EPSS% = -1 Then \
!Begin

	%Include CNVTSU23.011				! CONVENIOS Y  E.P.S

!EndIf
End Sub                                                 !
!------------------------------------------------------------------------------

Sub EPSMTS30.011 Public                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Sept de 2004                                        **
!   **    Proyecto   :  Grupo Retail Ltda                            **
!   **    User Exit  :  EPSMTS30.011                                         **
!   **    Inclu¡r en :  EAMTSU30.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura Valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
If UE.EPS.EPSS% = -1 Then \
Begin
	%Include CNVTSU30.011				! CONVENIOS Y  E.P.S
EndIf
End Sub                                                 !
!------------------------------------------------------------------------------
 
Sub EPSMTS43.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Sept de 2004                                        **
!   **    Proyecto   :  Grupo Retail Ltda                            **
!   **    User Exit  :  EPSMTS43.011                                         **
!   **    Inclu¡r en :  EAMTSU43.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura Valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
If UE.EPS.EPSS% = -1 Then \
Begin
	%Include CNVTSU43.011				! CONVENIOS Y  E.P.S
EndIf

End Sub
!------------------------------------------------------------------------------

Sub EPSMTS53.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Sept de 2004                                        **
!   **    Proyecto   :  Grupo Retail Ltda                            **
!   **    User Exit  :  EPSMTS53.011                                         **
!   **    Inclu¡r en :  EAMTSU53.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura Valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
If UE.EPS.EPSS% = -1 Then \
Begin
	%Include CNVTSU53.011				! CONVENIOS Y  E.P.S
EndIf
End Sub

Sub EPSMTS66.011(X.Callparm%) Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Sept de 2004                                         **
!   **    Proyecto   :  Grupo Retail Ltda                                **
!   **    User Exit  :  EPSMTS66.011                                         **
!   **    Inclu¡r en :  EAMTSU66.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
Integer*1 X.Callparm%

If UE.EPS.EPSS% = -1 Then \
Begin
	%Include CNVTSU66.011				! CONVENIOS Y  E.P.S
EndIf
End Sub

Sub EPSMTS60.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Sept de 2004                                         **
!   **    Proyecto   :  Grupo Retail Ltda                                **
!   **    User Exit  :  EPSMTS60.011                                         **
!   **    Inclu¡r en :  EAMTSU60.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
Integer*1 X.Callparm%

If UE.EPS.EPSS% = -1 Then \
Begin
	%Include CNVTSU60.011				! CONVENIOS Y  E.P.S
EndIf
End Sub
