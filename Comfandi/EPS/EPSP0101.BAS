!************************************************** 
!Empresa       : Asic Consulting Group S.A Retail *
!Programa      : EPSP0101.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Mantenimiento Beneficiarios      *
!**************************************************

!--- Definicion de Variables de trabajo

%INCLUDE EPSPVARI.BAS				  	   ! Variables programa
%INCLUDE ADX_UPGM:DMEXTR.J86          				   ! Inclucion Libreria Display Manager
%INCLUDE EPSPRUTI.BAS				  	   ! Rutinas Comunes

FUNCTION QUIT2     					   ! Funcion salida programa
    CLOSE AREA1%					   !
    CLOSE AREA2%					   !
    CALL SETF("0000000")				   !
    CALL CLRSCR						   !
    RET.ERR%= CLSDIS					   !
    CALL DM.ERR(RET.ERR%,CLSDIS$)			   !
    STOP
END FUNCTION
!--- Fin de la funcion de salida

FUNCTION BUSQ.CONV					   ! Busqueda de convenios
  BAN.PRG$ = "0"					   !
  LEC$="C8 C40 2C3 C5 C4 4C1 4C3 C5 3C1 4C3 C3 4C1"	   !
  READ FORM LEC$;#AREA2% KEY DM.CONVEN$;                  \! Lee Reg Cabecera 
       DM.LLAVE$,  DM.RAZON$,  DM.FECINI$, DM.FECFIN$,    \!
       DM.VLRCON$, DM.VLRDES$, DM.VALMED$, DM.VALMCA$,    \!
       DM.VALIPS$, DM.VALMET$, DM.METPRA$, DM.METPRB$,    \!
       DM.METPRC$, DM.METPRD$, DM.NIT$,    DM.VALDIA$,    \!
       DM.FACTURA$, DM.DSCVLR$, DM.PTG1$,  DM.PTG2$,      \!
       DM.PTG3$,   DM.PTG4$,  DM.DSCTO$,   DM.VALFAC$,    \!
       DM.VALBEN$, DM.CAPITACION$, DM.COPIATIQ$
  IF BAN.PRG$ = "0" THEN CALL MSG.ERR(3,DM.RAZON$)	   !
  IF BAN.PRG$ = "1" THEN BEGIN 
     CALL MSG.ERR(3,"No Definido ...") 			   !
     WAIT;1800
  ENDIF
END FUNCTION						   !	
!--- Fin de la funcion busqueda de convenios

FUNCTION BUSQ.REDP					   ! Busqueda de red prestadora
  BAN.PRG$ = "0"					   !
  DM.REDPRE$ = PACK$(RIGHT$("0000"+DM.RDPR$,4))            !
  LEC$="C2 C40 C12"                                        !
  READ FORM LEC$;#AREA3% KEY DM.REDPRE$;                  \! Lee Reg Cabecera 
       DM.LLAVE$,  DM.RAZON$, DM.CODALT$                   !
  IF BAN.PRG$ = "0" THEN CALL MSG.ERR(15,DM.RAZON$)	   !
  IF BAN.PRG$ = "1" THEN CALL MSG.ERR(15,"No Definido ...")!
END FUNCTION						   !	
!--- Fin de la funcion busqueda de convenios

FUNCTION BUSQ.ESAF        				   ! Busqueda de red prestadora
  BAN.PRG$ = "0"					   !
  DM.REDPRE$ = PACK$(RIGHT$("00"+DM.ESAF$,2))              !
  LEC$="C1 C40 C12"                                        !
  READ FORM LEC$;#AREA4% KEY DM.REDPRE$;                  \! Lee Reg Cabecera 
       DM.LLAVE$,  DM.RAZON$, DM.CODALT$                   !
  IF BAN.PRG$ = "0" THEN CALL MSG.ERR(31,DM.RAZON$)	   !
  IF BAN.PRG$ = "1" THEN CALL MSG.ERR(31,"No Definido ...")!
END FUNCTION						   !	
!--- Fin de la funcion busqueda de convenios

FUNCTION BUSQ.AFIL					   ! Busqueda afiliados
  BAN.PRG$ = "0"					   ! Inicializa bandera 
  DM.CONVEN$ = UNPACK$(DM.CONVEN$)		           ! Desempaqueta convenio
  TMP1$ = PACK$(RIGHT$("00000000000000"+DM.CODIGO$,14))	   ! Arma codigo asociado
  TMP1$ = UNPACK$(TMP1$)
  DM.CODIGO$ = DM.CONVEN$ + TMP1$			   ! Arma llave para busqueda
  KEY$=PACK$(RIGHT$("000000000000000000000000000000"+DM.CODIGO$,30))         ! Creacion llave proyecto
  LEC$="C15 C1 C6 C40 C6 3C1 2C6 C1 C12 C2 C1 C2"	   !
  READ FORM LEC$;#AREA1% KEY KEY$; DM.CODIGO$,            \! Lee Reg Cabecera 
        DM.TIPO$,   DM.CONVEN$, DM.NOMBRE$,  DM.FECNAC$,  \! maestro de 
        DM.TIPB$,   DM.PAREN$,  DM.ESTO$,    DM.FECINI$,  \! beneficiarios
        DM.FECFIN$, DM.PLAN$,   DM.EMPRES$,  DM.ESAF$,    \!
        DM.TPRE$,   DM.RDPR$				   !
  IF BAN.PRG$ = "0" THEN BEGIN				  \! Presenta Informacion
     DM.RDPR$ = Unpack$(DM.RDPR$)
     CALL MSG.ERR(05,DM.TIPO$)  : CALL MSG.ERR(06,DM.NOMBRE$)
     CALL MSG.ERR(08,DM.FECNAC$)
     CALL MSG.ERR(09,DM.TIPB$)  : CALL MSG.ERR(10,DM.PAREN$)
     CALL MSG.ERR(11,DM.ESTO$)  : CALL MSG.ERR(12,DM.RDPR$)
     CALL MSG.ERR(13,DM.TPRE$)  : CALL MSG.ERR(14,DM.PLAN$)
     CALL MSG.ERR(16,DM.ESAF$)  : CALL MSG.ERR(17,DM.FECINI$)
     CALL MSG.ERR(18,DM.FECFIN$): CALL MSG.ERR(19,DM.EMPRES$)
     CALL BUSQ.REDP                                        ! Busqueda red prestadora
     CALL BUSQ.ESAF                                        ! Busqueda estado afiliado
  ENDIF
END FUNCTION
!--- Fin de la funcion de busqueda afiliados		   

FUNCTION INICIO01
   MSG$ = "MANTENIMIENTO DE BENEFICIARIOS"
   CALL.ORDER% = 202                                          ! Llamado Primera Pantalla D.M
   RET.ERR% = DISPD(CALL.ORDER%)                              ! Llamado de la pantalla en DM
   CALL DM.ERR(RET.ERR%,DISPD$)
   CALL MSG.ERR(1,MSG$)				              ! Mensaje cabecera pantalla
END FUNCTION
!--- Fin de la funcion de inicio

FUNCTION CAPTURA.DATOS					      ! Funcion captura datos
STRING PLANES$
      CALL MSG.ERR(20,"Identificaci¢n del cliente ")
      DM.CODIGO$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
      WHILE DM.CODIGO$ = " " OR VAL(DM.CODIGO$) = 0
         CALL MSG.ERR(20,"Identificaci¢n del cliente ")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.CODIGO$ = GET.DATOS                               ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
      WEND
      DM.CODIGO$ = ALLTRIM(DM.CODIGO$)			      ! Elimino espacios en blanco al final
      CALL BUSQ.AFIL			        	      ! Busqueda Afiliado
!      CALL MSG.ERR(20,"1.NIT  2.C.C  3.C‚d.Ext.  4.T.I  5.Menor 7 A¤os  6.Carnet 7.Otro")
      CALL MSG.ERR(20,"1.Ced.Ext. 2.TI  3.CC 4.NIT 5.Reg.Civ. 6.Pasap. 7.Menor 8.Carnet Empresa") 
      DM.TIPO$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
      WHILE (MATCH(DM.TIPO$,"1234567",1) = 0 OR DM.TIPO$ = " ")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.TIPO$ = GET.DATOS
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
      WEND
      CALL MSG.ERR(20,"Nombre del Afiliado                                             ")
      DM.NOMBRE$ = GET.DATOS				   
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
      WHILE DM.NOMBRE$ = " "
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.NOMBRE$ = GET.DATOS                            ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
      WEND
      CALL MSG.ERR(20,"Fecha de Nacimiento del Afiliado AAMMDD                         ")
      DM.FECNAC$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
      CALL VAL.FECHA(DM.FECNAC$)
      WHILE DM.FECNAC$ = " " OR BAN.PRG$ = "1"
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.FECNAC$ = GET.DATOS                               ! Asigna valor capturado
         CALL VAL.FECHA(DM.FECNAC$)
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
      WEND
      CALL MSG.ERR(20,"1. Afiliado  2. Beneficiario")
      DM.TIPB$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
      WHILE (MATCH(DM.TIPB$,"12",1) = 0 OR DM.TIPB$ = " ")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.TIPB$ = GET.DATOS
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
      WEND
      IF DM.TIPB$ <> "1" THEN BEGIN
         CALL MSG.ERR(20,"1. Conyuge  2. Compa¤ero(a)  3. Hijos  4. Padres  5. Otros      ")
         DM.PAREN$ = GET.DATOS
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
         WHILE (MATCH(DM.PAREN$,"12345",1) = 0 OR DM.PAREN$ = " ")
            RET.ERR% = NXTF(-2)
            CALL DM.ERR(RET.ERR%,NXTF$)
            DM.PAREN$ = GET.DATOS
            IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
            IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
         WEND
      ENDIF ELSE BEGIN 						\! Si el tipo es el cotizante
            RET.ERR% = NXTF(2)					 ! no solicita el parentezco
            CALL DM.ERR(RET.ERR%,NXTF$)				 !
            DM.PAREN$ = "0"					 ! Asigna parentezco en cero
            CALL MSG.ERR(10,DM.PAREN$)				 ! presenta dato en pantalla
      ENDIF							 !
      IF VAL(DM.VALMET$) = 1 THEN PLANES$ = "Porcentaje " ELSE  \!
         PLANES$ = "Valor "
!      CALL MSG.ERR(20,"1. A o 0    2. B o 1   3. C o 2   4. D o 3                     ")
      PLANES$ = PLANES$+" 1."+UNPACK$(DM.METPRA$)+" 2."+        \!
                UNPACK$(DM.METPRB$)+" 3."+UNPACK$(DM.METPRC$)+  \!
                " 4."+UNPACK$(DM.METPRD$)
      CALL MSG.ERR(20,PLANES$)
      DM.ESTO$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                    ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                    ! Si presiona tecla ESC
      WHILE (MATCH(DM.ESTO$,"1234",1) = 0 OR DM.ESTO$ = " ")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.ESTO$ = GET.DATOS
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
      WEND
      CALL MSG.ERR(20,"C¢digo IPS primaria")
      BAN.PRG$ = "0"
      DM.RDPR$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                    ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                    ! Si presiona tecla ESC
      CALL BUSQ.REDP
      WHILE (DM.RDPR$ = " " OR BAN.PRG$ = "1")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.RDPR$ = GET.DATOS
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
         CALL BUSQ.REDP
      WEND
      DM.RDPR$ = RIGHT$("0000"+DM.RDPR$,4)                       ! 
      CALL MSG.ERR(20,"1. POS  2. Complementario  3. Otros                            ")
      DM.PLAN$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                    ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                    ! Si presiona tecla ESC
      WHILE (MATCH(DM.PLAN$,"123",1) = 0 OR DM.PLAN$ = " ")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.PLAN$ = GET.DATOS
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
      WEND
      CALL MSG.ERR(20,"1. Contributivo  2. Subsidiado  3. No Aplica                   ")
      DM.TPRE$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                    ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                    ! Si presiona tecla ESC
      WHILE (MATCH(DM.TPRE$,"12",1) = 0 OR DM.TPRE$ = " ")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.TPRE$ = GET.DATOS
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
      WEND
      CALL MSG.ERR(20,"Digite el Estado del Afiliado                                ")
      BAN.PRG$ = "0"
      DM.ESAF$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                    ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                    ! Si presiona tecla ESC
      CALL BUSQ.ESAF
      WHILE (BAN.PRG$ = "1" OR DM.ESAF$ = " ")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.ESAF$ = GET.DATOS
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
         CALL BUSQ.ESAF
      WEND
      DM.ESAF$ = (RIGHT$("00"+DM.ESAF$,2))                    !
      CALL MSG.ERR(20,"Fecha Inicio de Afiliaci¢n   AAMMDD                            ")
      DM.FECINI$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
      CALL VAL.FECHA(DM.FECINI$)			      ! Valida fecha
      WHILE (DM.FECINI$ = " " OR BAN.PRG$ = "1")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.FECINI$ = GET.DATOS
         CALL VAL.FECHA(DM.FECINI$)			      ! Valida fecha
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
      WEND
      CALL MSG.ERR(20,"Fecha Final de  Afiliaci¢n   AAMMDD                            ")
      DM.FECFIN$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
      CALL VAL.FECHA(DM.FECFIN$)			      ! Valida fecha
      WHILE (DM.FECFIN$ = " " OR (VAL(DM.FECFIN$) < VAL(DM.FECINI$)) OR \
             BAN.PRG$ = "1")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.FECFIN$ = GET.DATOS
         CALL VAL.FECHA(DM.FECFIN$)			      ! Valida fecha
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
      WEND
      CALL MSG.ERR(20,"Nit Empresa del Afiliado                                       ")
      DM.EMPRES$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
      WHILE (DM.EMPRES$ = " ")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.EMPRES$ = GET.DATOS
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
      WEND
      DM.RDPR$ = PACK$(DM.RDPR$)			   ! Empaqueta IPS Primaria OVS 18/May/01
      LEC$="C15 C1 C6 C40 C6 3C1 2C6 C1 C12 C2 C1 C2"
      WRITE FORM LEC$;#AREA1%;KEY$,                       \! Grabacion registro 
        DM.TIPO$,   DM.CONVEN$, DM.NOMBRE$,  DM.FECNAC$,  \! maestro de 
        DM.TIPB$,   DM.PAREN$,  DM.ESTO$,    DM.FECINI$,  \! beneficiarios
        DM.FECFIN$, DM.PLAN$,   DM.EMPRES$,  DM.ESAF$,    \!
        DM.TPRE$,   DM.RDPR$				  \!
END FUNCTION
!--- Fin de la funcion captura de datos

!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0101
CALL INICIADM 				                   ! Inicializacion Variables Display Manager
AREA1% = 11: AREA2% = 12: AREA3% = 13: AREA4% = 14         ! Definicion area de trabajo archivo
ARCH1$="BNEFICIA"		                       	   ! Asignar nombre archivo 
ARCH2$="CONVENIO"	        	               	   ! Asignar nombre archivo 
ARCH3$="IPS"		                	       	     ! Asignar nombre archivo 
ARCH4$="ESTAFILI"	                       		   ! Asignar nombre archivo 
OPEN ARCH1$ KEYED RECL 101 AS AREA1%                       ! Abre archivo de Beneficiarios
OPEN ARCH2$ KEYED RECL 106 AS AREA2%                       ! Abre archivo de Convenios
OPEN ARCH3$ KEYED RECL 54 AS AREA3%                        ! Abre archivo de IPS
OPEN ARCH4$ KEYED RECL 53 AS AREA4%                        ! Abre archivo de Estado afiliados
TERM$ = " "					           ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   !
CALL DM.ERR(RET.ERR%,INITDM$)				   !
RET.ERR% = OPNDIS("ADX_UPGM:EPSPANTA.PNT")	           ! Apertura de la forma de pantalla
CALL DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
WHILE (FIN$ = "0") 					   ! Mientras que no F3/Esc
      DM.OPCION$   = "  "				   !
      CALL INICIO01					   !
      MSG$="Digite C¢digo del Plan"			   !
      CALL MSG.ERR(20,MSG$)				   ! Mensaje 
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)			   !
      ATTR$ = SETF(PRM.ON$)                                !
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.CONVEN$ = INP$                                    ! Asigna valor capturado
      IF (ENDF = F1.AYUDA) THEN BEGIN \
         CALL HELP("DEFINICION BENEFICIARIOS ","EPSP0101.TXT") !
         CALL INICIO01
      ENDIF
      IF (ENDF = F3.SALIR) THEN CALL QUIT2                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN CALL QUIT2                 ! Si presiona tecla ESC
      WHILE DM.CONVEN$ = " " OR VAL(DM.CONVEN$) = 0
        RET.ERR% = NXTF(-20)
        CALL DM.ERR(RET.ERR%,NXTF$)
        ATTR$ = SETF(PRM.ON$)                                
        INP$ = UPDF                                        ! Captura dato en pantalla
        DM.CONVEN$ = INP$                                  ! Asigna valor capturado
        IF (ENDF = F1.AYUDA) THEN BEGIN \
           CALL HELP("DEFINICION BENEFICIARIOS ","EPSP0101.TXT") !
           CALL INICIO01
        ENDIF
        IF (ENDF = F3.SALIR) THEN CALL QUIT2               ! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN CALL QUIT2               ! Si presiona tecla ESC
      WEND
      DM.CONVEN$=PACK$(RIGHT$("0000000000000000"+DM.CONVEN$,16))  ! Creacion llave convenio
      CALL BUSQ.CONV			                   ! Busqueda convenio
      IF BAN.PRG$ = "0" THEN CALL CAPTURA.DATOS	           ! Captura datos 
WEND

!----
!---- Fin del bloque principal
!----

E0101:
         IF ERRF% = AREA1% AND                              \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN            ! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 15,,,25000 RECL 101 \! si no existe lo crea.
                        AS AREA1% compound perupdate 		     ! 
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa
         IF ERRF% = AREA2% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            CREATE POSFILE ARCH2$ KEYED 8,,,5000 RECL 106 \! si no existe lo crea.
                        AS AREA2% compound perupdate 		   ! 
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa
         IF ERRF% = AREA3% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            CREATE POSFILE ARCH3$ KEYED 2,,,150 RECL 54   \! si no existe lo crea.
                        AS AREA3% compound perupdate 		   ! 
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa
         IF ERRF% = AREA4% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            CREATE POSFILE ARCH4$ KEYED 1,,,150 RECL 53   \! si no existe lo crea.
                        AS AREA4% 	compound perupdate 	   ! 
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa
         IF ERRF% = AREA1% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         ENDIF
         IF ERRF% = AREA2% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo              
            RESUME
         ENDIF
         IF ERRF% = AREA3% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo              
            RESUME
         ENDIF
         IF ERRF% = AREA4% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo              
            RESUME
         ENDIF
         IF ERRF% = 19 AND ERR = "EF" OR ERR="OE" THEN BEGIN  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         ENDIF						       !
         CALL TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         Locate 24,1: Print String$(78," ")
         LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla
!--- Fin despliegue de errores

!--- Fin del programa EPSP0101.BAS
