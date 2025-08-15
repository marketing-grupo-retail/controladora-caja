!**************************************************
!Empresa       : ASIC CONSULTING GROUP S.A. RETAIL*
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

%ENVIRON T

Integer*4 global IR.price1, IR.price2
Integer*1 Global IR.ITEMTYPE, Asc.Cnv.Formula%, Asic.Detalle%

%Include EAMTSWKG.J86          ! working storage
%Include EAMTRANS.j86          !
%Include EAMTERMS.J86          !
%Include EAMITEMR.J86          !
%Include EAMTOPTS.J86	       !
%Include CNVTSUVA.011	       ! Variables del programa
%Include CTRLTSVA.011	       ! Lectura de Control
%Include EAMP4VGJ.J86          ! Variables rutinas 4610
%INCLUDE EAMTSXHC.J86          !
%Include EAMASMRT.J86          !

Sub TSPREC01  EXTERNAL
End Sub

Sub TSHIECET EXTERNAL
End Sub

Function  VISORES4690(U.D.VISOR%, U.D.LINEA1$, U.D.LINEA2$,U.D.TIEMPO%,U.D.POSICION$) External ! Msg Display
String U.d.Linea1$, U.d.Linea2$, U.d.Posicion$							! Variables de
Integer*2 U.d.visor%, U.d.tiempo%   							        	! trabajo
String U.D.Linea1$, U.D.Linea2$		
End Function
!--- Fin manejo de visores

Function U.IMPRIME(UE.String$,UE.STATION) External					! Rutina de Impresion
String UE.String$      																			! Variables temporales
Integer*2 UE.STATION																				!
End Function
!--- Rutina de Impresion

Function ASIC.DATOS$(UMSG1$,UMSG2$) External   							! Captura de datos
String UMSG1$, UMSG2$, ASIC.DATOS$
End Function
!--- Fin captura de datos desde el teclado

Function U.CORTACR External																	! Cortal papel impresora pos
End Function
!--- Fin U.CortarCr

Function U.Explusadi External 															! Expulsa Papel En El 
End Function

Sub VISOR.AND.BORRAR(X.MSG$) External
  STRING X.MSG$
End Sub

Function FORMAT.AMOUNT(AMT1) EXTERNAL
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub SALVAR.VARIABLES EXTERNAL
End Sub

Sub RESTAURA.VARIABLES EXTERNAL
End Sub

Function Asic.Getunpk(X.B$,X.J%) External
Integer*2 X.J%, X.K%
String    X.B$, Asic.Getunpk
End Function

Function Linea.Detalle(X.Ses%)  External                 ! Imprime Detalle De La Operacion
String Linea.Detalle, X.Oper$, X.A$
Integer*1 X.Ses%
End Function 

Function Grabacion.Cadena.Usuario(X.clave$,X.datos$) External
String X.clave$, X.datos$
End Function
!--- Fin grabacion de datos

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
	F$ = LINEA1$
	G$ = LINEA2$
	UE.LLAVE.PUESTA% = 0
	Call Visores4690(1,LINEA1$,LINEA2$,0,"L")
  UNLOCKDEV 32,7                               ! Desbl Teclado y pasa
  Wait 32;500                                   ! al Estado ACCT NO
  Read #32;IOPDATA$,BQ$,CQ$,DQ$,               \! Lectura Teclado y Status
           EQ$,FQ$,GQ$,HQ$,IQ$,JQ$,KQ$              ! teclado con 10 Datos
  If Mid$(IOPDATA$,11,1) = "1" Then UE.LLAVE.PUESTA% = -1
  Call VALIDE.ERROREPS(OVRKEY%)
End Sub

Sub INICIALICE.EPS
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
	UE.EPSS.COOP% = 0     :UE.EPS.LINE% = 0     :UE.EPS.FOTO% = 0
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
   	   Call Visores4690(1,Left$(UE.EPSS.MNOMB$,20),Right$(UE.EPSS.MNOMB$,20),1500,"C")   	
	    UE.EPSS.MEDICO$ = UNPACK$(NO.MEDICO$)
	 EndIf Else \
		UE.EPSS.MEDICO$ = String$(12,"0")
    VALIDE.MEDICO = TS.ER.RETURN
EndIf Else Begin
   UE.EPSS.MEDICO$ = UNPACK$(NO.MEDICO$)
   Call Visores4690(1,"MEDICO GENERICO", ".....",1000,"C")
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
      Call Visores4690(1,Left$(UE.EPSS.DIAG$,20),Right$(UE.EPSS.DIAG$,20),1500,"C")
	    UE.EPSS.NoDIAG$ = UNPACK$(NO.DIAG$)																		!
	EndIf Else 																														   \!
		UE.EPSS.NoDIAG$ = String$(6,"0")																				!
EndIf Else Begin																														!
   UE.EPSS.NoDIAG$ = UNPACK$(NO.DIAG$)																			!
   Call Visores4690(1,"DIAGNOSTICO GENERICO","....",1500,"C")   						!
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
      Call Visores4690(1,Left$(UE.EPSS.INOMB$,20),Right$(UE.EPSS.INOMB$,20),1500,"C")
      UE.EPSS.IPS$ = UNPACK$(NO.IPS$)
	EndIf Else \
		UE.EPSS.IPS$ = String$(12,"0")
EndIf Else Begin
   UE.EPSS.IPS$ = UNPACK$(NO.IPS$)
   Call Visores4690(1,"IPS GENERICO","....",1500,"C")
EndIf
VALIDE.IPS = TS.ER.RETURN
End Function
!--- Fin de la Validacion de IPS

Function VALIDE.CONVENIO(NO.CONVENIO$)
String NO.CONVENIO$
Integer*4 VALIDE.CONVENIO
NO.CONVENIO$ = PACK$(Right$(String$(16,"0")+NO.CONVENIO$,16))
TVISOR$ = NO.CONVENIO$
UE.OPEN.LIGCONVE% = 23
UE.EPS.ACTFECHA$ = DATE$
TS.ER.RETURN = -1
Read Form "C8 C40 2C3 C5 C4 4C1 4C3 C5 3C1 4C3 C3 3C1";#UE.OPEN.CONVENIO% KEY NO.CONVENIO$;	\
	UE.EPS.CCOD$,UE.EPS.CNOMB$,UE.EPS.CFECHI$,UE.EPS.CFECHF$,	\
	UE.EPS.CTOT$,UE.EPS.CMAX$,UE.EPS.CVMED$,UE.EPS.CVMERCA$,	\
	UE.EPS.CVIPS$,UE.EPS.CCALCULO$,UE.EPS.CMP$(1),UE.EPS.CMP$(2),	\
	UE.EPS.CMP$(3),UE.EPS.CMP$(4),UE.EPS.CNIT$, UE.EPS.CVDIAGNOS$,	\
	UE.EPS.NRMLoCATAL$,UE.EPS.COPAGOLESS$,				\
	UE.EPS.DSCTLESS$(1), UE.EPS.DSCTLESS$(2),			\
	UE.EPS.DSCTLESS$(3), UE.EPS.DSCTLESS$(4),			\
	UE.EPS.DSCTOTRX$, UE.EPS.ValFAC$, UE.EPS.ValBEN$, UE.EPS.CAPITACION$

If TS.ER.RETURN NE -1 Then UEDLG.LEIDO% = 0 Else Begin
   If Val(UNPACK$(UE.EPS.CMAX$)) = 0 Then UE.EPS.CMAX$ = PACK$("99999999")
   UE.EPS.NCONVE$ = " FALTA ACTUALIZAR."
   TS.ER.RETURN = -1
   Asc.Cnv.Line1$ = TO.TRAILERLINE1$
   Asc.Cnv.Line2$ = TO.TRAILERLINE2$
   TO.TRAILERLINE1$ = ""
   TO.TRAILERLINE2$ = ""
   Open "R::LIGCONVE" KEYED RECL 57 AS UE.OPEN.LIGCONVE%
   If TS.ER.RETURN NE -1 Then UEDLG.LEIDO% = 0 Else \
    Begin
      Read Form "C5 C40 C12";#UE.OPEN.LIGCONVE% KEY UE.EPS.CNIT$;     \
                              UE.EPS.CNIT$,UE.EPS.NCONVE$,UE.EPS.CODALT$
      If UE.OPEN.LIGCONVE% = 23 Then CLOSE 23
      UE.EPS.NCONVE$ = Left$(UE.EPS.NCONVE$+String$(38," "),38)
    EndIf
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
		       Call CAPTURE.DATOEPS("Ingrese Cod. Medico.","",0)
		       If (Mid$(IOPDATA$,13,1) <> "I") AND (UE.ERROR% = 0) Then Begin
		        If Mid$(IOPDATA$,13,1) = "P" Then Begin         	! si presiono ENTER
			         UE.EPSS.LEIDO% = Val(Right$(KQ$,LEN(KQ$)-1))
			         UE.EPSS.LEIDO$ = Right$(KQ$,LEN(KQ$)-1)
			         UEDLG.OK% = VALIDE.MEDICO(UE.EPSS.LEIDO$)
			         If UEDLG.OK% = 0 Then Begin
			            Call CAPTURE.DATOEPS("Medico "+UE.EPSS.LEIDO$,"1.(OK)  9.(REPITE)  ",0)
		              If (Mid$(IOPDATA$,13,1) <> "I") AND (UE.ERROR% = 0) Then Begin
		               If Mid$(IOPDATA$,13,1) = "P" Then Begin         	! si presiono ENTER
			               TS.TEMP1$ = Right$(KQ$,LEN(KQ$)-1)			
			               If Val(TS.TEMP1$) = 1 Then Begin 
                       UEDLG.OK% = -1
                       UE.EPSS.MEDICO$ = Right$("000000000000"+UE.EPSS.LEIDO$,12)
                       UE.EPSS.MNOMB$ = "MEDICO GENERICO"                      
                     EndIf Else UEDLG.OK% = 0
                   EndIf 			            
			            EndIf 
			         EndIf 
		        EndIf
		       EndIf
		     Wend
     EndIf Else UEDLG.OK% = VALIDE.MEDICO(UE.EPSS.LEIDO$)
     
		 UEDLG.OK% = 0
     If UE.EPS.CVDIAGNOS$ NE "2" Then Begin
			While (UEDLG.OK% = 0)
			   UE.ERROR% = 0
			   Call CAPTURE.DATOEPS("Ingrese Cod. Diagnos","",0)
			   If (Mid$(IOPDATA$,13,1) <> "I") AND (UE.ERROR% = 0) Then Begin
			      If Mid$(IOPDATA$,13,1) = "P" Then Begin         	! si presiono ENTER
				     UE.EPSS.LEIDO% = Val(Right$(KQ$,LEN(KQ$)-1))
				     UE.EPSS.LEIDO$ = Right$(KQ$,LEN(KQ$)-1)
				     UEDLG.OK% = VALIDE.DIAGNOSTICO(UE.EPSS.LEIDO$)
				     If UEDLG.OK% = 0 Then Begin
				       Call VISOR.AND.BORRAR("DIAGNOSTICO NO      EXISTE         ")
				     EndIf
			      EndIf
			   EndIf
			Wend
     EndIf Else UEDLG.OK% = VALIDE.DIAGNOSTICO(UE.EPSS.LEIDO$)
		 UEDLG.OK% = 0
     If UE.EPS.CVIPS$ NE "2" Then Begin 
			 While (UEDLG.OK% = 0)
			   UE.ERROR% = 0
			    Call CAPTURE.DATOEPS("Ingrese Cod. I.P.S. ","",0)
			    If (Mid$(IOPDATA$,13,1) <> "I") AND (UE.ERROR% = 0) Then Begin
			      If Mid$(IOPDATA$,13,1) = "P" Then Begin         	! si presiono ENTER
				       UE.EPSS.LEIDO% = Val(Right$(KQ$,LEN(KQ$)-1))
				       UE.EPSS.LEIDO$ = Right$(KQ$,LEN(KQ$)-1)
				       UEDLG.OK% = VALIDE.IPS(UE.EPSS.LEIDO$)
				       If UEDLG.OK% = 0 Then Begin
				          Call VISOR.AND.BORRAR("IPS  INGRESADO NO   EXISTE         ")
				       EndIf
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
	     
EndIf Else Begin
    Call VISOR.AND.BORRAR("PLAN INGRESADO NO   EXISTE         ")
    UEDLG.LEIDO% = 0
EndIf
VALIDE.CONVENIO = UEDLG.LEIDO%
End Function
!--- Fin de la Validacion de convenios

Function VALIDE.BENEFIC(No.BENEFIC$)
String No.BENEFIC$
Integer*1 BENEFIC.OK%
Integer*4 VALIDE.BENEFIC
TVISOR$ = No.BENEFIC$
No.BENEFIC$ = PACK$(Right$(String$(16,"0")+UE.EPSS.DAT(1),16)) +\
		PACK$(Right$(String$(14,"0")+No.BENEFIC$,14))
TVISOR$ = No.BENEFIC$
If UE.EPS.ValBEN$ = "0" Then Begin		 ! Si no Valida beneficiario
   VALIDE.BENEFIC = UEDLG.LEIDO%
   BENEFIC.OK% = -1				 ! Init variable
   Call Visores4690(1,"BENEFICIARIO GENERICO","....",1200,"C")   
   UE.EPS.BNOMB$ = "BENEFICIARIO GENERICO"
   No.BENEFIC$ = UNPACK$(No.BENEFIC$)
   UE.EPS.BCOD$  = PACK$(Right$("000000000000000000000000000000"+No.BENEFIC$,30))
  EPS.LOOP9:							! Control de ciclo
  KQ$ = ""							! Inite Var.
  While(Val(KQ$)=0)						! Captura categoria
   Call CAPTURE.DATOEPS("Categoria ...... ?","",0)		! del afiliado
    If Mid$(IOPDATA$,13,1)="I" Then Begin			! Si presiono borrar
       GOTO EPS.SALIDA						! Sale del dialogo
    EndIf							!
   If Mid$(IOPDATA$,10,1)="1" AND KQ$ <> "" Then \		! Capturo dato
      KQ$ = Right$(KQ$,LEN(KQ$)-1) Else \			!
      KQ$ = Mid$(KQ$,4,10)					!
   KQ$ = STR$(Val(KQ$))						! Tomo dato capturado
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
   Exit Function				 ! Salgo de la funcion
EndIf
BENEFIC.OK% = -1
TS.ER.RETURN = -1
Read Form "C15 C1 C6 C40 C6 3C1 2C6 C1 C12 C2 C1 C2";#UE.OPEN.BENEFIC% KEY No.BENEFIC$;	\
	UE.EPS.BCOD$,UE.EPS.BTIPO$,UE.EPS.BCONVENIO$,UE.EPS.BNOMB$,	\
	UE.EPS.BFECHN$,UE.EPS.BCOTIZ$,UE.EPS.BPAREN$,UE.EPS.BESTRATO$,	\
	UE.EPS.BFECHI$,UE.EPS.BFECHF$,UE.EPS.BPLAN$,UE.EPS.BEMPRESA$,	\
	UE.EPS.BESTADO$,UE.EPS.BPREST$,UE.EPS.BCODPREST$
	
If TS.ER.RETURN = -1 Then Begin
	If Val(UE.EPS.ACTFECHA$) >= Val(UE.EPS.BFECHI$) AND \
	   Val(UE.EPS.ACTFECHA$) <= Val(UE.EPS.BFECHF$) Then Begin
	    Call Visores4690(1,Left$(UE.EPS.BNOMB$,20),Right$(UE.EPS.BNOMB$,20),1500,"C")   
		  Call Visores4690(1,"INICIE A INGRESAR","PRODUCTOS",0,"C")
	EndIf Else Begin
    	Call VISOR.AND.BORRAR("FECHA DEL AFILIADO  EXPIRO         ")
	    UEDLG.LEIDO% = 0
	    UEPG.BORRA% = 3
	EndIf
EndIf Else Begin
    	Call VISOR.AND.BORRAR("BENEFICIARIO   NO   EXISTE         ")
      UEDLG.LEIDO% = 0
EndIf
VALIDE.BENEFIC = UEDLG.LEIDO%

End Function

Function VALIDE.ANTERIOR			  ! Validar si cobra coopago
Integer*1 VALIDE.ANTERIOR		          ! 
VALIDE.ANTERIOR = 0
  If ( (Val(UNPACK$(UE.EPS.BCOD$)) = Val(UE.OLD.BENEF$))  AND \   ! Si se cumple la condicion
       (Val(UE.EPSS.MEDICO$) = Val(UE.OLD.MEDICO$)) AND \   ! la funcion retorna 1, si 
       (Val(UNPACK$(UE.EPS.CCOD$)) = Val(UE.OLD.IPS$))    AND \   ! no la cumple retorna 0
       (Val(UE.EPSS.IPS$)    = Val(UE.OLD.IPS$)))   Then \  ! OVS Dic. 11 2001
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
	     TS.IO.DATA$(9) = Unpack$(UE.EPS.CCOD$)
	     If UE.EPS.CAPITACION$ = "0" Then Begin 
	        TS.IO.DATA$(3) = UE.EPS.VARI$
	        TS.IO.MOTORKEY = Val("9"+UE.EPS.TIPO$)
	     EndIf Else Begin 
	        TS.IO.DATA$(3) = UE.EPS.VARIC$
	        TS.IO.MOTORKEY = Val("9"+UE.EPS.TIPOC$)	  
	     EndIf 
	EndIf
	EndIf Else Begin
	 If (Val(UE.EPS.COPAGOLESS$) = 0) Then Begin
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
	   EndIf 
	   UE.EPS.INDPAGO% = -1
EndIf
COOPAGO = 0   ! Retorna de la funcion
End Function
!--- Fin de aplicacion de coopagos


Sub LEA.EPS
String X.Convenio$, X.Factura$, X.IPS$, X.Coopago$
 LOCKDEV 32,PURGE                                ! Bloquea Teclado
 Asc.Cnv.Formula% = 0                            ! Si dato capturado es una formula medica
 UEPG.BORRA% = 0          
 UE.EPSS.ASK% = 1
 UEDLG.OK% = 0
 UE.EPSS.MEDICO$ = PACK$(String$(12,""))
 UE.EPSS.IPS$    = PACK$(String$(12,""))
 While (UEDLG.OK% = 0)
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
   
   Call CAPTURE.DATOEPS(UE.EPSS.DESC(UE.EPSS.ASK%),"",0)
   If (Mid$(IOPDATA$,13,1)<>"I") AND (UE.ERROR% = 0) Then Begin     ! si presiono borrar
      If Mid$(IOPDATA$,13,1)="P" Then Begin         	              ! si presiono ENTER
	        UEDLG.LEIDO% = Val(Right$(KQ$,LEN(KQ$)-1))
	        UE.EPS.DATO$ = Right$(KQ$,LEN(KQ$)-1)
      EndIf  
   VALIDA.ENTRADA:
!--- Lineas Adicionadas para manejo formatos de Comfandi - OVS 09 May 2006      
      If UE.EPSS.ASK% = 1 AND Len(UE.EPS.DATO$) = 20 Then Begin   ! Datos Mini Factura - Comfandi
         X.Convenio$ = Mid$(UE.EPS.DATO$,9,12)    ! Codigo del Convenio
         X.Factura$  = Mid$(UE.EPS.DATO$,1, 7)    ! Numero de la factura
         
      EndIf 

      If UE.EPSS.ASK% = 1 AND Len(UE.EPS.DATO$) = 24 Then Begin             ! Datos Formula Medica
         X.Convenio$ = Mid$(UE.EPS.DATO$,12,12)        ! Codigo del Convenio
         X.Factura$  = Mid$(UE.EPS.DATO$, 5, 7)        ! Numero de la factura
         X.IPS$      = Mid$(UE.EPS.DATO$, 1, 4)        ! Codigo de la IPS    
         X.Coopago$  = Mid$(UE.EPS.DATO$,24, 1)        ! Codigo Coopago
         If Val(X.Coopago$) = 0 Then X.Coopago$ = "4"  ! Si plan excento de pago
         Asc.Cnv.Formula% = -1
      EndIf 
      
!--- Fin de lineas adicionadas
      
!--- Validacion del convenio
      If UE.EPSS.ASK% = 1 Then Begin                             ! Captura del convenio
       If X.CONVENIO$ <> "" Then UE.EPS.DATO$ = X.CONVENIO$      ! Si capturado por scanner - Comfandi
	     UEDLG.LEIDO% = VALIDE.CONVENIO(UE.EPS.DATO$)              ! Valida Convenio
	     If UEDLG.LEIDO% = 0 Then \                                ! Si dato errado
	        UE.EPSS.ASK% = UE.EPSS.ASK% - 1  Else \                !
	       Begin                                                   !
	        If Asc.Cnv.Formula% = -1 Then Begin 
	          UE.EPS.BNOMB$ = "BENEFICIARIO GENERICO"              ! Nombre beneficiario
            UE.EPS.BCOD$  = Pack$(X.CONVENIO$ + String$(14,"0")) ! Codigo Beneficiario
            UE.EPS.BESTRATO$ = Right$("00"+X.Coopago$,2)         ! Coopago a cobrar
            UE.EPSS.IPS$ = X.IPS$                                ! Asigna IPS
            UE.EPSS.MEDICO$ = String$(12,"0")                    ! Medico generico
            UE.EPSS.MNOMB$  = "MEDICO GENERICO"                  !
            UE.EPSS.NoDIAG$ = String$(6,"0")                     ! Diagnostico generico
            UE.EPSS.DIAG$   = "DIAGNOSTICO GENERICO"             !
            UE.EPSS.INOMB$  = "IPS FORMULA"                      ! 
          EndIf 
	       EndIf 
      EndIf
      
!--- Validacion numero de documento/formula
      If (UE.EPSS.ASK% = 2) Then Begin                           ! Captura formuna
	      If UEDLG.LEIDO% = 0 Then \                               ! SI NUMERO FACTURA ES 0 ASIGNACION AUTOMATICA FECHA+HORA
	         UEDLG.LEIDO% = Val(DATE$+Left$(TIME$,4))              !
      EndIf
!--- Validacion de beneficiario
      If (UE.EPSS.ASK% = 3) Then Begin
       If Asc.Cnv.Formula% = 0 Then Begin 
	      UEDLG.LEIDO% = VALIDE.BENEFIC(UE.EPS.DATO$)
	      If UEDLG.LEIDO% = 0 Then UE.EPSS.ASK% = UE.EPSS.ASK% - 1 
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

Function Valida.Tipo.Articulo(X.Code$) 														! Valida tipo de Articulo
Integer*1 Valida.Tipo.Articulo																		!
String    X.Code$, X.Tmp$																					!
%Include CNVITEMR.J86																							! Variables temporales

TS.ER.RETURN = -1
%Include CNVIRRD1.J86          																		! Lectura maestro de articulos
If TS.ER.RETURN = -1 Then BEGIN 
   Asc.Cnv.Name$ = AR.ITEMNAME$
EndIF Else BEGIN
   Asc.Cnv.Name$ = "NO DEFINIDO"
EndIF
 
X.Tmp$ = Right$(Unpack$(AR.INDICAT2$),1)													! Toma el tipo de articulo
If Val(X.tmp$) = 5 Then Begin  																		! Si metodo de precio 5
   Valida.Tipo.Articulo = -1
   X.Tmp$ = Unpack$(AR.SaleQuan$)																	! Toma Primera parte del PLU
   X.Tmp$ = X.Tmp$ + Unpack$(AR.Salepric$)                        ! Toma segunda parte del PLU
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
 EndIf												!

If UE.EPS.COPIA% = 2 Then Begin
 If UE.EPS.CAPITACION$ <> "1" Then Begin																		! Proceso de capitacion
  If PRT4610.ENABLE = 0 Then X.Ajuste% = 38 Else X.Ajuste% = 52
  !Call U.Imprime(TO.HEADERLINE1$,4100H)																		!
  !Call U.Imprime(TO.HEADERLINE2$,4100H) 																	!
  
  Call U.Imprime("    ***** COPIA CLIENTE *****         ",4100H)					!
  !Call U.Imprime(String$(38,"*"),4100H)
  Call U.Imprime("--   IDENTIFICACION DEL CONVENIO    --",4100H)
!  X.Str$ = Left$("Nombre Convenio: "+Left$(UE.EPS.NCONVE$,21)+String$(X.Ajuste%," "),X.Ajuste%)
!  Call U.Imprime(X.Str$,4100H)

  X.Str$ = Left$("Codigo del Plan: "+UNPACK$(UE.EPS.CCOD$)+String$(X.Ajuste%," "),X.Ajuste%)
  Call U.Imprime(X.Str$,4100H)
  X.Str$ = Left$("Nombre del Plan: "+Left$(UE.EPS.CNOMB$,21)+String$(X.Ajuste%," "),X.Ajuste%)
  Call U.Imprime(X.Str$,4100H)
  
!  X.Str$ = Left$("Beneficiario   : "+Left$(UE.EPS.BNOMB$,21)+String$(X.Ajuste%," "),X.Ajuste%)
!  Call U.Imprime(X.Str$,4100H)
!  X.Str$ = Left$("Codigo / C.C.  : "+Right$(UNPACK$(UE.EPS.BCOD$),14)+String$(X.Ajuste%," "),X.Ajuste%)
!  Call U.Imprime(X.Str$,4100H)

  X.Str$ = Left$("Solicitud Nro. : "+Right$(UE.EPSS.FORMULA$,12)+String$(X.Ajuste%," "),X.Ajuste%)
  Call U.Imprime(X.Str$,4100H)
  X.Str$ = Left$("Codigo Medico  : "+Right$(UE.EPSS.MEDICO$,12)+String$(X.Ajuste%," "),X.Ajuste%)
  Call U.Imprime(X.Str$,4100H)
  X.Str$ = Left$("Codigo I.P.S   : "+Right$(UE.EPSS.IPS$,12)+String$(X.Ajuste%," "),X.Ajuste%)
  Call U.Imprime(X.Str$,4100H)
  X.Str$ = Left$("Categoria      : "+UE.EPS.BESTRATO$+String$(X.Ajuste%," "),X.Ajuste%)
  Call U.Imprime(X.Str$,4100H)
  !Call U.Imprime(String$(38,"*"),4100H)  
  !Call U.Imprime("   CODIGO           DESCRIPCION",4140H)
  
EndIf Else Begin 
  Call VISOR.AND.BORRAR("INSERTE COPIA DE LA FORMULA     BORR")
  Write #35;(String$(X.AJUSTE%,"-")+X.LF$)
  Write #35;(" --   IDENTIFICACION DEL CONVENIO    --"+X.LF$)
!  Write #35;(" Nombre Convenio: "+Left$(UE.EPS.NCONVE$,21)+X.LF$)

  Write #35;(" Codigo del Plan: "+UNPACK$(UE.EPS.CCOD$)+X.LF$)
  Write #35;(" Nombre del Plan: "+Left$(UE.EPS.CNOMB$,21)+X.LF$)
!  Write #35;(" Beneficiario   : "+Left$(UE.EPS.BNOMB$,21)+X.LF$)
!  Write #35;(" Codigo / C.C.  : "+Right$(UNPACK$(UE.EPS.BCOD$),14)+X.LF$)

  Write #35;(" Solicitud Nro. : "+Right$(UE.EPSS.FORMULA$,12)+X.LF$)
!  Write #35;(" Codigo Medico  : "+Right$(UE.EPSS.MEDICO$,12)+X.LF$)
  Write #35;(" Codigo I.P.S   : "+Right$(UE.EPSS.IPS$,12)+X.LF$)
  Write #35;(" Categoria      : "+UE.EPS.BESTRATO$+X.LF$)
!  Write #35;(String$(X.AJUSTE%,"-")+X.LF$)  
!  Write #35;("   CODIGO           DESCRIPCION"+X.LF$)
  
EndIf 
  X.IndArt% = 0
  For X.I% = 1 TO Asc.Cnv.SlEnd%                  												! Recorre todos los String
    H$ = READ.SL.STR$(X.I%)                 															! Toma el String
    If LEN(H$) > 5 Then     Begin          														    ! Si el String es OK
     If ASC(H$)  = 1 Then Begin            															  ! Si es un articulo
        If X.IndArt% = 1 Then Begin                                       !
           If UE.EPS.CAPITACION$ <> "1" Then \
            Call U.Imprime("Cantidad : "+Str$(X.Qty%)+" $"+Xprecio$,4100H) Else \! Imprime dato de copia
            Write #35;" Cantidad : "+Str$(X.Qty%)+" $"+Xprecio$+X.LF$     ! Imprime dato de copia
        EndIf Else X.IndArt% = 1
        X.TMP$ = ASIC.GETUNPK(H$,3)	              												! Articulo vEndido
        X.Tmp$ = Pack$(Right$("000000000000"+X.Tmp$,12))
        X.J% = Valida.Tipo.Articulo(X.Tmp$)                               ! Tipo de articulo
        X.TMP$ = ASIC.GETUNPK(H$,3)	              												! Articulo vEndido
        X.Tmp$ = Right$("            "+Str$(Val(X.Tmp$)),12)              ! Ajusta formato Item
        X.Tmp$ = X.Tmp$ + "        "+ Asc.Cnv.Name$                       ! Arma String impresion
        If UE.EPS.CAPITACION$ <> "1" Then \
           Call U.Imprime(X.Tmp$,4100H) Else \														! Imprime dato de copia
           Write #35;X.Tmp$+X.LF$    																			! Imprime dato de copia
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
         X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)													! NO USADO   MPGROUP= ",VAL(A$)
         X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)													! NO USADO   DEALQUAN= ",VAL(A$)
         X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)													! NO USADO   PRICE METH= ",VAL(A$)
         X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)													! NO USADO   SALEQUAN= ",VAL(A$)
         X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)													! NO USADO   SALEPRIC= ",VAL(A$)
         X.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)													! CANTIDAD O PESO VEndIDO = QTYORWGT        
         If X.Qty% > 0 Then \ 																					  ! Si venta
            X.Qty% = Val(X.Tmp$) Else																		 \! Retorna Valor Sino Reporta devolucion
            X.Qty% = Val(X.Tmp$) * -1																			! Retorna Valor 
         If UE.EPS.CAPITACION$ <> "1" Then \            
            Call U.Imprime("Cantidad : "+Str$(X.Qty%)+" $"+Xprecio$,4100H) Else \			  ! Imprime dato de copia
            Write #35;" Cantidad : "+Str$(X.Qty%)+" $"+Xprecio$+X.LF$     ! Imprime dato de copia
         X.IndArt% = 2																										! Control Impresion Dato
         GoTo Next.Lectura
     EndIf 
    EndIf 
    Next.Lectura:
     
  Next X.I%
  If X.IndArt% = 1 Then Begin                                             !
    If UE.EPS.CAPITACION$ <> "1" Then \            
     Call U.Imprime("Cantidad Entregada : "+Str$(X.Qty%),4100H)Else \     !
     Write #35;" Cantidad : "+Str$(X.Qty%)+" $"+Xprecio$+X.LF$            ! Imprime dato de copia
  EndIf 
  If UE.EPS.CAPITACION$ <> "1" Then Begin 
!     Call U.Imprime(String$(19,"- "),4100H)
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
  EndIf Else Begin
     Write #35;(String$(19,"- ")+X.LF$)
     Asic.Detalle% = -1 
     X.Tmp$ = Linea.Detalle(23)                                              ! Store Line
     Asic.Detalle% = 0
     Write #35;(" "+X.Tmp$+X.LF$)                     											  ! Impresion Store Line
     Call U.Explusadi     
  EndIf 
  UE.EPSS.ON = 0
  UE.EPS.COPIA% = 0
EndIf
End Sub
!--- Fin de la copia del cliente


Sub EPSMTS01.011 Public                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Sept de 2004                                        **
!   **    Proyecto   :  ASIC CONSULTING GROUP                            **
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
String X.Str$
Integer*1 X.Ajuste%
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
!   **    Proyecto   :  ASIC CONSULTING GROUP                            **
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
!   **    Proyecto   :  ASIC CONSULTING GROUP                            **
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

	%Include CNVTSU07.011				! CONVENIOS Y  E.P.S

End Sub                                               !
!------------------------------------------------------------------------------

Sub EPSMTS08.011 Public                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Sept de 2004                                        **
!   **    Proyecto   :  ASIC CONSULTING GROUP                            **
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
!   **    Proyecto   :  ASIC CONSULTING GROUP                            **
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
!   **    Proyecto   :  ASIC CONSULTING GROUP                            **
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
!   **    Proyecto   :  ASIC CONSULTING GROUP                            **
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
!   **    Proyecto   :  ASIC CONSULTING GROUP                            **
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
!   **    Proyecto   :  ASIC CONSULTING GROUP                            **
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
!   **    Proyecto   :  ASIC CONSULTING GROUP                            **
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
!   **    Proyecto   :  ASIC CONSULTING GROUP                            **
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
