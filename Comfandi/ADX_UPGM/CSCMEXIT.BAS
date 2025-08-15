!************************************************** 
!Empresa       : Grupo Retail Ltda  		          *
!Programa      : CSCMEXIT.BAS                     *
!Autor         : Oscar Valencia                   *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Mayo 28 de 2009                  *
!Observaciones : Control Claves Supervisor Cajeros*
!**************************************************

%ENVIRON T

Integer*1 GLOBAL UE.CTLCSC.ACTIVO%	! SI PROYECTO ACTIVO
String    Global Gr.CTLCSC.Superv$(2)

%INCLUDE EAMTSCVA.J86
%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          !
%INCLUDE EAMTERMS.J86          !
%INCLUDE EAMTOPTS.J86	       !
%INCLUDE EAMP4VAJ.J86          !
%INCLUDE EAMTSXHC.J86     

%INCLUDE GRMTSUSU.011          ! Invocacion de rutinas externas genericas

Sub TSHIECET External 
End Sub 

Function Java.Msg.Encryption(Java.Class$, Java.Method$, Java.Message1$, Java.Message2$) Public 		! Inicializacion Clases De Java
String Java.Method$, Java.Request$, Java.Exception$, Java.Class$,             			       \! Def. Variables Tmp
       Java.Returnvalue$, Java.Message1$, Java.Message2$,Java.Msg.Encryption           			!

Java.Request$     = "C$"										! Init De Variables
Java.Exception$   = ""											! Para La Ejecucion
Java.Returnvalue$ = ""											! Del Modulo
Java.Exception$   = "" 											! Init Variable
Call Javacall.Initialize.Request(Java.Class$,Java.Method$,Java.Request$)  				! Inicializacion De Clases
Call Javacall.Addparameter.String(Java.Request$,Java.Message1$)						! Adicion De Parametros
Call Javacall.Addparameter.String(Java.Request$,Java.Message2$)						! Adicion De Parametros
Call Javacall.Invokemethod.Returnstring(Java.Request$,Java.Returnvalue$,                               \! Ejecucion De La Clase
        				Java.Exception$)		
Java.Msg.Encryption = Java.Returnvalue$

End Function 


Sub Carga.Claves.Supervisor
Integer*2 GRI%
Integer*4 GRREC.NO, GRBLK.NUM, GRREC.LEN, GRKEY.LEN, GRMAX.R.SEC, GRX, GRR.S, GR
String    GRH$, GRPZERO$, GRKEY$, GRUSER$, GRFEC$, GRPWD$, GRUSR$(1)
 TS.ER.RETURN = -1 
 Dim Gr.CTLCSC.Superv$(25,3)				         ! Vector de control  
 Open "R::TF:CLVSEG" KEYED RECL 6 AS 94 NOWRITE NODEL            ! Open Archivo Claves Supervisores
 If TS.ER.RETURN <> -1 Then Begin 				 ! Error de apertura
    Call VISOR.AND.BORRAR("ERROR APERTURA TF:CLVSEG")		 ! Msg al operador
    UE.CTLCSC.ACTIVO% = 0
    Exit Sub 							 ! Salida de la rutina
 EndIf 
 Dim GRUSR$(25)
 For GRI% = 1 to 20 						 ! Recorre las claves
     GRKEY$ = Pack$(Right$("00"+Str$(GRI%),2))
     TS.ER.RETURN = -1 
     Read Form "C1 C5";#94 KEY GRKEY$; GRH$, GRUSER$
     If TS.ER.RETURN = -1 Then Begin 
        GRUSR$(GRI%) = UnPack$(GRUSER$)
     EndIf Else GRUSR$(GRI%) = "00000"
 Next GRI%
 Close 94 :  GRI% = 0 
 TS.ER.RETURN = -1 
 Open "R::TF:SUPERV" KEYED RECL 15 AS 94 NOWRITE NODEL           ! Open Archivo Claves Supervisores
 If TS.ER.RETURN <> -1 Then Begin 				 ! Error de apertura
    Call VISOR.AND.BORRAR("ERROR APERTURA TF:SUPERV")            ! Msg al operador
    UE.CTLCSC.ACTIVO% = 0
    Exit Sub                                                     ! Salida de la rutina 
 EndIf 
  
 For GRI% = 1 to 20 						 ! Asigna claves a la aplicacion de SMA
     If Val(GRUSR$(GRI%)) <> 0 Then Begin 			 ! Usuario asignado
        GRKEY$ = Pack$(Right$("0000000000"+GRUSR$(GRI%),10))     ! Arma llave de busqueda
        TS.ER.RETURN = -1  					 ! Ctrl de errores
        Read Form "C5 C4 C5 C1";#94 KEY GRKEY$; GRUSER$, GRFEC$, GRPWD$, GRH$
        If TS.ER.RETURN = -1 Then Begin                          ! Si lo encontro
      	   Gr.CTLCSC.Superv$(GRI%,0) = UnPack$(GRUSER$)          ! Usuario
  	   Gr.CTLCSC.Superv$(GRI%,1) = UnPack$(GRFec$)           ! Fecha de Vencimiento
  	   Gr.CTLCSC.Superv$(GRI%,2) = UnPack$(GRPWD$)           ! Password
           TO.OVRIDNUM(GRI%) = Val(Gr.CTLCSC.Superv$(GRI%,2))    ! Almacena en vector control de claves
        EndIf Else Begin 					 ! No existe usuario matriculado
          TO.OVRIDNUM(GRI%) = 0         			 ! Asigna datos en ceros
          Gr.CTLCSC.Superv$(GRI%,0) = "0"			 ! 
          Gr.CTLCSC.Superv$(GRI%,1) = "0"			 ! 
          Gr.CTLCSC.Superv$(GRI%,2) = "0"			 !
       EndIf    						 !
     EndIf Else Begin 						 ! Si posicion no definida
        TO.OVRIDNUM(GRI%) = 0					 ! Borra posicion 
     EndIf 
 Next GRI%
 Close 94
 Call U.IMPRIME("CTRL CLAVES SUPERVISORES Ver 1.00 OK!",6100H)
 GRI% = 0 
 
! For GRI% = 1 TO 20
!   Call U.IMPRIME("POS "+Str$(GRI%)+" ="+Str$(TO.OVRIDNUM(GRI%)),4100H)
! Next GRI%
 
End Sub 

Sub Valida.Clave.Supervisor
Integer*2 GRI%, GrFound%
String    GRTMP$, GRCLV$, GRUSER$
 GrFound% = 0
 For GRI% = 1 to 20						 ! Analiza si fecha activa
     GRCLV$ = Str$(TS.TEMP1I4)                                   ! Clave capturada
     GRUSER$ = Gr.CTLCSC.Superv$(GRI%,0)                         ! Usuario
     GRCLV$ = Java.Msg.Encryption("com.grpretail.utils.Encryption","encryption",GRCLV$,GRUSER$) ! Ejecuta encryption
     If Val(GRCLV$) = Val(Gr.CTLCSC.Superv$(GRI%,2)) Then Begin  ! Encontrado
        GRTMP$ = Right$(Gr.CTLCSC.Superv$(GRI%,1),6)             ! Fecha de validacion
        If Val(GRTMP$) > Val(GRTMP$) Then Begin                  ! Fecha Expiro
           Call VISOR.AND.BORRAR("CLAVE SUPERVISOR    EXPIRO!")  ! Msg al Operador
           TS.TEMP1I4 = 9999999999				 ! Si Error retorna clave invalida
           GRI% = 30
        EndIf Else Begin                                         ! Clave Correcta y no vencida
           TS.TEMP1I4 = Val(GRCLV$)				 ! Entrega clave 
           GRI% = 30						 ! Sale del ciclo	
           GrFound% = -1					 ! Lo encontro
           GRTMP$ = Pack$(GRUSER$)                         +    \! ID Supervisor que autorizo proceso
		    ":"+Pack$("00")			         ! Fin de entrada de usuario
           Call Grabacion.String.Usuario("7001",GRTMP$)          ! Almacena User Data
        EndIf 
     EndIf                                                       !
 Next GRI%                                                    ! 
 If GrFound% <> -1 Then \
    TS.TEMP1I4 = 9999999999

End Sub 

Sub CLSUPERV(XOPTION%)  Public
Integer*4 XOPTION%

!   ***************************************************************************
!   **    Proyecto   :  Modulo Control Claves Supervisor Cajeros             **
!   **    Incluir en :  EAMTSU07.J86                                         **
!   ***************************************************************************

If XOPTION% = 7 Then Begin 
   UE.CTLCSC.ACTIVO% = 0 
   TS.ER.RETURN = -1																											  ! Ctrl Errores
   Open "R::TF:CSC001" AS 94					  											              ! Apertura archivo parametrizacion
   If TS.ER.RETURN <> -1 Then Begin																					! Error en la apertura
      Call U.IMPRIME("Error Apertura TFCSC001",6100H)
      Call U.IMPRIME("ERROR="+TS.TS11WERR$+" COD="+TS.DISPERR$+" FILE="+\
                     STR$(TS.ERRF),6100H)
      Call U.IMPRIME("ERROR CTRL CLAVE SUPERV.",6100H)	        ! Rastro en SJ
      Exit Sub 																															! Sale del procedimiento
   EndIf Else Begin 
      Read #94; TS.TEMP1$			       																				! Proyecto Activo
      UE.CTLCSC.ACTIVO%  = Val(Mid$(TS.TEMP1$,30,2))
   EndIf 
   Close 94
   If UE.CTLCSC.ACTIVO% = -1 Then Begin ! Proyecto Activo
      Call Carga.Claves.Supervisor
   EndIf 
EndIf

If UE.CTLCSC.ACTIVO% <> -1 Then Exit Sub 	 		! SI MODULO APAGADO 

!   ***************************************************************************
!   **    Proyecto   :  Modulo Control Claves Supervisor Cajeros             **
!   **    Incluir en :  EAMTSU36.J86                                         **
!   ***************************************************************************

If XOPTION% = 36 Then Begin 
   Call Valida.Clave.Supervisor
EndIf 

End Sub 


