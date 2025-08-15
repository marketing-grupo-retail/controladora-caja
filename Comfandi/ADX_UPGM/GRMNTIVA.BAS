!************************************************** 
!Empresa       : GRUPO RETAIL LTDA                *
!Programa      : GRMNTIVA.BAS                     *
!Autor         : OSCAR VALENCIA SARMIENTO         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Oct 5 de 2.005                   *
!Observaciones : Definicion tasas de impuestos    *
!**************************************************

!--- Definicion de Variables de trabajo

String    Global U.SERIAL$, U.TIPO$, U.PREF$, U.AUT$, U.FEC$, U.TMP$, U.HOY$, U.RESOL$
String    Global U.OLD$, U.NEW$, U.CAJERO$
Integer*4 Global U.INI%, U.FIN%, U.ACT% ,U.NOTA%, U.NC%
Integer*1 Global U.STATUS%
String    Global TIVA$(1)
Integer*1 Global TS.ER.RETURN

%INCLUDE FISPVARI.BAS				  	   ! Variables programa
%INCLUDE ADX_UPGM:DMEXTR.J86       ! Inclucion Libreria Display Manager
%INCLUDE FISPRUTI.BAS				  	   ! Rutinas Comunes

Function ADXERROR(TERM,GRP,NUM,SVRTY,EVENT,UNQ$) EXTERNAL
 Integer*2 TERM,NUM
 Integer*1 SVRTY,GRP,EVENT
 String    UNQ$
End Function

Function QUIT2     					       ! Funcion salida programa
    Call SETF("0000000")				   !
    Call CLRSCR						         !
    RET.ERR%= CLSDIS					     !
    Call DM.ERR(RET.ERR%,CLSDIS$)	 !
    Stop
End Function
!--- Fin de la funcion de salida

Function INICIO01
   Call.ORDER% = 20                                           ! Llamado Primera Pantalla D.M
   RET.ERR% = DISPD(Call.ORDER%)                              ! Llamado de la pantalla en DM
   Call DM.ERR(RET.ERR%,DISPD$)
End Function
!--- Fin de la funcion de inicio

Sub FISCAL.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "ADX_UDT1:NF" + Left$(DATE$,6) + ".TXT"
			Open X.FILE$ AS 56 Append
			If TS.ER.RETURN <> -1 Then Begin    ! Si no existe
			   TS.ER.RETURN = -1
				 CREATE X.FILE$ AS 56
         If TS.ER.RETURN <> -1 Then Begin 
            CALL MSG.ERR(05,"Error: Falla Creación Auditoria")
            WAIT;2000
            Exit Sub 
         EndIf 
			EndIf 
			X.Finr$ = Chr$(13) + Chr$(10)
			X.BUFF$ = "["+X.SALE$+"]"+"MSG:"+X.ENVIA$
			X.Len% = Len(X.BUFF$)						  				  								            ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						            ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					          ! Graba registro
			X.BUFF$ = "["+X.RTA$+"]"+"RTA:"+X.LLEGA$                                !
			X.Len% = Len(X.BUFF$)						  				  								            ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						            ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					          ! Graba registro      
			Close 56
End Sub 

Sub CAPTURA.DATOS
Integer*2 X.I%
String FINR$
      FINR$ = CHR$(13)+CHR$(10)				                                      ! Fin de registro
      U.STATUS% = 0
      MSG$="Tasa Numero Uno de Impuesto" 		   															!
      Call MSG.ERR(10,MSG$)				   																				! Mensaje 
      RET.ERR% = NXTF(-20) 			           																	! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)			   																		!
      ATTR$ = SETF(PRM.ON$)                                									!
      INP$ = UPDF                                          									! Captura dato en pantalla
      TIVA$(1) = INP$                                          									! Asigna valor capturado
      IF (ENDF = F1.AYUDA) Then Begin \
         Call HELP("Tasas de Impuestos","TASAIMPT.TXT")   									!
         Call INICIO01
      EndIf
      IF (ENDF = F3.SALIR) Then Exit Sub                   ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  Then Exit Sub                   ! Si presiona tecla F3
      WHILE TIVA$(1) = " " 
        RET.ERR% = NXTF(-20)
        Call DM.ERR(RET.ERR%,NXTF$)
        ATTR$ = SETF(PRM.ON$)                                
        INP$ = UPDF                                        									! Captura dato en pantalla
        TIVA$(1) = INP$                                        									! Asigna valor capturado
        IF (ENDF = F1.AYUDA) Then Begin 
           Call HELP("Tasas de Impuestos","TASAIMPT.TXT")   							  !
           Call INICIO01
        EndIf
        IF (ENDF = F3.SALIR) Then Exit Sub                 ! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  Then Exit Sub                      ! Si presiona tecla F3
      Wend

      For X.I% = 2 TO 8
        Call MSG.ERR(10,"Tasa "+Str$(X.I%)+" de Impuesto")
        TIVA$(X.I%) = GET.DATOS
        IF (ENDF = F3.SALIR) Then Exit Sub                      ! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  Then Exit Sub                      ! Si presiona tecla ESC
        WHILE TIVA$(X.I%) = " " 
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         TIVA$(X.I%) = GET.DATOS                                                  ! Asigna valor capturado
         IF (ENDF = F3.SALIR) Then Exit Sub                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  Then Exit Sub                   ! Si presiona tecla ESC
        Wend
      Next X.I%
      
      CREATE POSFILE "TABLAIVA" AS AREA1% COMPOUND PERUPDATE
      
      For X.I% = 1 TO 8 
        TIVA$(X.I%) = Right$("00"+TIVA$(X.I%),2)
        Write Form "2C2" ;#AREA1%; TIVA$(X.I%), FINR$
        U.New$ = U.New$ + TIVA$(X.I%)
      Next X.I%
      U.STATUS% = 1
End Sub

SUB VALIDA.DATOS
STRING U.KEY$, LEC$, UE.PORCENTAJE
INTEGER*1 UE.LINEA

Dim TIVA$(9)
For UE.LINEA = 1 TO 9 
    TIVA$(UE.LINEA) = "00"
Next UE.LINEA

UE.LINEA = 0
IF END # AREA1% Then  CONT.IVA
LEER.IVA:
   READ # AREA1% ;  UE.PORCENTAJE
   UE.LINEA = UE.LINEA + 1    
   TIVA$(UE.LINEA) = UE.PORCENTAJE
   GOTO LEER.IVA 
CONT.IVA:
  Close AREA1%
  Call INICIO01					   																							!
  Call MSG.ERR(2,TIVA$(1))
  Call MSG.ERR(3,TIVA$(2))
  Call MSG.ERR(4,TIVA$(3))
  Call MSG.ERR(5,TIVA$(4))
  Call MSG.ERR(6,TIVA$(5))
  Call MSG.ERR(7,TIVA$(6))
  Call MSG.ERR(8,TIVA$(7))
  Call MSG.ERR(9,TIVA$(8))

For UE.LINEA = 1 TO 8
    U.OLD$ = U.OLD$ + TIVA$(UE.LINEA)
Next UE.LINEA
     
End SUB

Sub CTRL.ACCESO
String DM.USER$, DM.PASWD$,DM.MODELO$,DM.RESTO$, DM.OPER$, CLAVE.MEM$
ACCESO:
      Call.ORDER% = 250                                    ! Llamado Primera Pantalla D.M
      RET.ERR% = DISPD(Call.ORDER%)                        ! Llamado de la pantalla en DM
      Call DM.ERR(RET.ERR%,DISPD$)
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      Call MSG.ERR(05,"Digite C¢digo de Usuario ....         ")
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.USER$ = INP$
      If (ENDF = F3.SALIR) Then Call QUIT                  ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  Then Call QUIT                  ! Si presiona tecla ESC
      If (ENDF = F1.AYUDA) Then Begin                     \! Si presiona tecla F3
        Call HELP("ACCESO APLICATIVO CONTROL","HLPP999.TXT")
        GOTO ACCESO 
      EndIf
      While DM.USER$ = ""
         Call MSG.ERR(05,"Error: Entre C¢digo Usuario ...       ")
         RET.ERR% = NXTF(-20)
         Call DM.ERR(RET.ERR%,NXTF$)
         INP$ = UPDF                                       ! Captura dato en pantalla
         DM.USER$ = INP$
         If (ENDF = F3.SALIR) Then Call QUIT               ! Si presiona tecla F3
         If (ENDF = ESC.KEY)  Then Call QUIT               ! Si presiona tecla ESC
         If (ENDF = F1.AYUDA) Then Begin                  \! Si presiona tecla F3
            Call HELP("ACCESO APLICATIVO CONTROL","HLPP999.TXT")
            GOTO ACCESO 
         EndIf
      Wend
      BAN.PRG$ = "0"
      DM.USER$=PACK$(Right$("0000000000"+DM.USER$,10))     ! Creacion llave usuario
      LEC$="C5 C4 C1 C22 C20 C20"
      Read ForM LEC$;#AREA2% KEY DM.USER$;                \! Lee Reg usuario
        DM.USER$,DM.PASWD$,DM.MODELO$,DM.RESTO$,          \!
        DM.OPER$,DM.RESTO$				   !
      If BAN.PRG$ = "0" Then Begin                        \!
         Call MSG.ERR(03,DM.OPER$)
         If Val(UNPACK$(DM.MODELO$)) <> 9 AND             \!
            Val(UNPACK$(DM.MODELO$)) <> 5 Then Begin      \!
            Call MSG.ERR(05,"Error: Nivel de Usuario No Autorizado ")
            WAIT;2000
            Call QUIT
	 EndIf  
         Call MSG.ERR(05,"Digite Clave de Usuario  ....         ")
         CLAVE.MEM$ = GET.DATOS				   ! Captura la clave 
         If (ENDF = F3.SALIR) Then Call QUIT               ! Si presiona tecla F3
         If (ENDF = ESC.KEY)  Then Call QUIT               ! Si presiona tecla ESC
         If (ENDF = F1.AYUDA) Then Begin                  \! Si presiona tecla F3
            Call HELP("ACCESO APLICATIVO CONTROL","HLPP999.TXT")
            GOTO ACCESO 
         EndIf
         While CLAVE.MEM$ = ""
            Call MSG.ERR(05,"Error: Entre Clave  Usuario ...       ")
            RET.ERR% = NXTF(-2)
            Call DM.ERR(RET.ERR%,NXTF$)
            CLAVE.MEM$ = GET.DATOS
            If (ENDF = F3.SALIR) Then Call QUIT            ! Si presiona tecla F3
            If (ENDF = ESC.KEY)  Then Call QUIT            ! Si presiona tecla ESC
            If (ENDF = F1.AYUDA) Then Begin                  \! Si presiona tecla F3
               Call HELP("ACCESO APLICATIVO CONTROL","HLPP999.TXT")
               GOTO ACCESO 
            EndIf
         Wend
         CLAVE.MEM$=PACK$(Right$("00000000"+CLAVE.MEM$,8)) ! Arma clave capturada
         If CLAVE.MEM$ <> DM.PASWD$ Then  Begin           \!
            Call MSG.ERR(05,"Error: Clave de Acceso Erronea ...    ") 
            WAIT;2000
            Call QUIT2
         EndIf
      EndIf
      If BAN.PRG$ = "1" Then Begin			  \!
         Call MSG.ERR(05,"Error: Usuario No Autorizado o No Existe ")
         WAIT;2000
         Call QUIT2
      EndIf
      U.CAJERO$ = Unpack$(DM.USER$)  ! Usuario ingreso sistema
END Sub
!--- Fin del control de acceso

!----
!----
!---- Bloque Principal 
!----
!----

On Error GoTo E0101
Call INICIADM 				                   																		! Inicializacion Variables Display Manager
AREA1% = 1
AREA2% = 11
AREA3% = 12
ARCH1$ = "TABLAIVA" 
Open ARCH1$ AS AREA1%                                     									! Abre archivo de IVA´s
Open "EAMOPERA" KEYED RECL 72 AS AREA2% NODEL              									! Abre archivo de operadores
TERM$ = " "					                     																		! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					       																			!
Call DM.ERR(RET.ERR%,INITDM$)				   																			!
RET.ERR% = OPNDIS("ADX_UPGM:FISCAL.PNT")	           												! Apertura de la forma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
!Call CTRL.ACCESO
FIN$="0"
Call VALIDA.DATOS
Call CAPTURA.DATOS
Call QUIT2
Stop 


!----
!---- Fin del bloque principal
!----

E0101:
         IF ERRF% = AREA1% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") Then Begin          ! el archivo de control
            CREATE ARCH1$ AS AREA1% 			                 ! 
            RESUME                                         ! retorna ejecucion
         EndIf                                             ! del programa
         IF ERRF% = AREA2% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") Then Begin          ! el archivo de control
            Call MSG.ERR(05,"Error Apertura Archivo Operadores")
            wait ; 2000
            Stop
         EndIf                                             ! del programa         
         If ERRF% = 56 AND                                    \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN              ! el archivo de control
            TS.ER.RETURN = 0
            Resume 
         ENDIF                          
         
         IF ERRF% = AREA1% AND ERR = "EF" Then Begin      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         EndIf
         IF ERRF% = AREA2% AND ERR = "EF" Then Begin      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         EndIf
         IF ERRF% = AREA3% AND ERR = "EF" Then Begin      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         EndIf
         IF ERRF% = 19 AND ERR = "EF" OR ERR="OE" Then Begin  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         EndIf						       !
         Call TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla
!--- Fin despliegue de errores

!--- Fin del programa NUMFIS.BAS
