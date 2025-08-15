!************************************************** 
!Empresa       : ASIC CONSULTING GROUP S.A Retail *
!Programa      : EPSP0401.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Reporte de Afiliados             *
!**************************************************
!
!--- Definicion de Variables de trabajo

%INCLUDE EPSPVARI.BAS				  	   ! Variables programa
%INCLUDE ADX_UPGM:DMEXTR.J86          				   ! Inclucion Libreria Display Manager
%INCLUDE EPSPRUTI.BAS				  	   ! Rutinas Comunes

FUNCTION INICIO01
   CALL.ORDER% = 221                                          ! Llamado Primera Pantalla D.M
   RET.ERR% = DISPD(CALL.ORDER%)                              ! Llamado de la pantalla en DM
   CALL DM.ERR(RET.ERR%,DISPD$)
   CALL MSG.ERR(5,"Digite Codigo del Convenio")
END FUNCTION
!--- Fin de la funcion de inicio

FUNCTION QUIT2     					   ! Funcion salida programa
    CLOSE AREA1%					   !
    CALL SETF("0000000")				   !
    CALL CLRSCR						   !
    RET.ERR%= CLSDIS					   !
    CALL DM.ERR(RET.ERR%,CLSDIS$)			   !
    STOP
END FUNCTION
!--- Fin de la funcion de salida

FUNCTION LEER.CABECERA
     DM.NOMBRE$   = Left$("COMFAMILIAR ANDI"+String$(30," "),30)
     DM.SIGLA$   = Left$("COMFANDI        "+String$(30," "),30)
     DM.ALMACEN$ = "0000"
End FUNCTION
!--- Fin de la funcion de lectura

FUNCTION BUSQ.CONV					   ! Busqueda de convenios
  BAN.PRG$ = "0"					   !
  LEC$="C8 C40 2C3 C5 C4 4C1 4C3 C5 3C1 4C3 C3 4C1"        !
  KEY$=PACK$(RIGHT$("0000000000000000"+DM.OPCION$,16))     ! Creacion llave proyecto
  READ FORM LEC$;#AREA3% KEY KEY$;                        \! Lee Reg Cabecera 
       DM.LLAVE$,  DM.RAZON$,  DM.FECINI$, DM.FECFIN$,    \!
       DM.VLRCON$, DM.VLRDES$, DM.VALMED$, DM.VALMCA$,    \!
       DM.VALIPS$, DM.VALMET$, DM.METPRA$, DM.METPRB$,    \!
       DM.METPRC$, DM.METPRD$, DM.NIT$,    DM.VALDIA$,	  \!
       DM.FACTURA$, DM.DSCVLR$, DM.PTG1$,  DM.PTG2$,      \!
       DM.PTG3$,   DM.PTG4$,  DM.DSCTO$,   DM.VALFAC$,    \!
       DM.VALBEN$, DM.CAPITACION$, DM.COPIATIQ$
  IF BAN.PRG$ = "1" THEN DM.RAZON$ = "CONVENIO NO DEFINIDO"
END FUNCTION						   !	
!--- Fin de la funcion busqueda de convenios

FUNCTION PROCESE.REPORTE
  IF MID$(DATE$,1,2) = "99" THEN        \  ! Manejo Y2K
   DM.FECINI$ = "19"+DATE$  ELSE        \
   DM.FECINI$ = "20"+DATE$
 IF CONLI% > 55 OR PAGI% = 0 THEN BEGIN  			  ! Final de Hoja o Inicio pagina
    IF CONLI% > 55 THEN BEGIN
       PRINT LINEA$					          ! Impresion linea
       PRINT CHR$(12)    					  ! Salto de Pagina
    ENDIF
     PAGI%=PAGI%+1						  ! Inicializa numero de pagina
     PRINT LET.CON$					          ! Activa la letra condensada
     PRINT LINEA$						  ! Imprime linea continua
     LISTA$ =DM.NOMBRE$+STRING$(33," ")+"LISTA BENEFICIARIOS "+STRING$(33," ")+\
     "Pagina No. "+RIGHT$(STRING$(04,"0")+STR$(PAGI%),04)
     PRINT LISTA$
     PRINT DM.SIGLA$+" - "+DM.ALMACEN$+STRING$(79," ")+"Fecha :"+DM.FECINI$
     PRINT LINEA$
     PRINT "Plan Listado  :"+UNPACK$(DM.LLAVE$)+"  "+DM.RAZON$
     PRINT " "
!                    1         2         3         4         5         6         7         8         9        10        11        12        13
!           123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012
     PRINT "   CODIGO               NOMBRE BENEFICIARIO                INICIAL      FINAL   ESTRATO"
     PRINT "                                                            AAMMDD     AAMMDD"
!           xxxxxxxxxxxx   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx     xxxxxx     xxxxxx      x
     CONLI%=10             					  ! Inicializa Contador de lineas
 ENDIF
     PRINT DM.BUFFER$
     CONLI% = CONLI% + 1
END FUNCTION
!--- Fin del proceso del reporte

FUNCTION IMPRIME
   CALL MSG.ERR(05,"Generando Reporte, Espere Por Favor ...")
   LPRINTER							! Salida Impresora
   LINEA$=STRING$(131,"Ä")        				! Linea Continua en la impresion
   CONLI% = 0: PAGI% = 0: I%=1					! Inicializo contadores
   LET.CON$ = CHR$(15)            				! Condensar  letra de impresion
   LET.NOR$ = CHR$(18)            				! Normalizar letra de impresion 
   READ FORM "T43 I4 I2 T55 I2 C456"; #AREA1%,I%;              \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$                              !      
   PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
   MAX.R.SEC = 508/REC.LEN                                      ! Length record
   BAN.PRG$ = "0"					        ! Program Control
   FOR I% = 2 TO BLK.NUM                                        ! Cicle to read all blocks
     READ FORM "T5 C508"; #AREA1%, I%;  H$                      ! H$ contains block
     X = 1 : R.S = 0 : KEY$ = MID$(H$,X,KEY.LEN)                ! Extract First key
     WHILE KEY$  NE  PZERO$                                     ! Inside sector loop
       R.S = R.S + 1                                            ! Records On This Sector 
       C01$ = UNPACK$(MID$(H$,X+0,15))                          ! Convenio/Beneficiario 
       C02$ = Right$(C01$,14)                                   ! Cedula beneficiario
       C01$ = Left$(C01$,16)                                    ! Codigo del plan
       IF VAL(C01$) = VAL(DM.OPCION$) THEN BEGIN                ! Si es convenio escogido
        IF VAL(C02$) >= VAL(DM.TIPO$) AND                      \! Si esta dentro del
           VAL(C02$) <= VAL(DM.TIPB$) THEN Begin               \! rango seleccionado
          DM.NOMPRO$ = MID$(H$,X+21,40)                         ! Nombre Beneficiario 
          DM.ESTO$   = MID$(H$,X+70,01)                         ! Estrato social   
          DM.FECINI$ = MID$(H$,X+71,06)                         ! Fecha Inicial contrato
          DM.FECFIN$ = MID$(H$,X+77,06)                         ! Fecha Final Contrato
          C02$ = Str$(Val(C02$))
          C02$ = Right$("              "+C02$,14)
          DM.BUFFER$ = C02$+"   "+DM.NOMPRO$+"   "+DM.FECINI$+\!
                       "     "+DM.FECFIN$+"      "+DM.ESTO$
          CALL PROCESE.REPORTE      				! Generaci¢n del Plano
        ENDIF
       ENDIF
       X = X + REC.LEN                                          ! Index to next key
       KEY$ = MID$(H$,X,KEY.LEN)                                ! Pick up next key
       IF R.S = MAX.R.SEC THEN KEY$ = PZERO$                    ! If EOF() record or file
     WEND                                                       !
   NEXT I%                                                      !
   PRINT LINEA$                                                 ! Imprime linea continua
   PRINT "Fin del Reporte               "                       ! Imprime numero de registros
   PRINT LET.NOR$  	                        	        ! Normaliza Letra impresora
   PRINT CHR$(12)   				                ! Realiza salto de pagina
   CONSOLE
END FUNCTION
!--- Fin de la funcion de impresion

FUNCTION CAPTURA.DATOS					      ! Funcion captura datos
      CALL MSG.ERR(05,"Beneficiario Inicial")
      DM.TIPO$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
      WHILE DM.TIPO$ = " "
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.TIPO$ = GET.DATOS
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
      WEND
      CALL MSG.ERR(05,"Beneficiario Final")
      DM.TIPB$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
      WHILE DM.TIPB$ = " " OR (VAL(DM.TIPB$) < VAL(DM.TIPO$))
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.TIPB$ = GET.DATOS
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
      WEND
      CALL IMPRIME
END FUNCTION
!--- Fin de la funcion captura de datos

!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0201
CALL INICIADM 				                   ! Inicializacion Variables Display Manager
AREA1% = 11: AREA2% = 12: AREA3% = 13: AREA4% = 14         ! Definicion area de trabajo archivo
EOF$="0"
ARCH1$="BNEFICIA"		                       	   ! Asignar nombre archivo convenios
ARCH3$="CONVENIO"	                	       	   ! Asignar nombre archivo convenios
OPEN ARCH1$ DIRECT RECL 512 AS AREA1%                      ! Abre archivo de Beneficiarios
Open ARCH3$ KEYED  RECL 106 AS AREA3%                      ! Abre archivo convenios
TERM$ = " "					           ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   !
CALL DM.ERR(RET.ERR%,INITDM$)				   !
RET.ERR% = OPNDIS("ADX_UPGM:EPSPANTA.PNT")	           ! Apertura de la forma de pantalla
CALL DM.ERR(RET.ERR%,OPNDIS$)			           !
FIN$="0"						   !
CALL LEER.CABECERA					   ! Lectura registro control
WHILE (FIN$ = "0") 					   ! Mientras que no F3/Esc
      DM.OPCION$   = "  "				   !
      CALL INICIO01					   !
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)			   !
      ATTR$ = SETF(PRM.ON$)                                !
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.OPCION$ = INP$                                    ! Asigna valor capturado
      IF (ENDF = F1.AYUDA) THEN BEGIN \
         CALL HELP("REPORTE DE BENEFICIARIOS  ","EPSP0401.TXT") !
         CALL INICIO01
      ENDIF
      IF (ENDF = F3.SALIR) THEN CALL QUIT2                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN CALL QUIT2                 ! Si presiona tecla ESC
      WHILE DM.OPCION$ = " "				   !
        CALL MSG.ERR(5,"Digite Codigo del Convenio")
        RET.ERR% = NXTF(-20)				   !
        CALL DM.ERR(RET.ERR%,NXTF$)			   !
        ATTR$ = SETF(PRM.ON$)                              ! 
        INP$ = UPDF                                        ! Captura dato en pantalla
        DM.OPCION$ = INP$                                  ! Asigna valor capturado
        IF (ENDF = F1.AYUDA) THEN BEGIN 		  \!
           CALL HELP("REPORTE DE BENEFICIARIOS  ","EPSP0401.TXT") !
           CALL INICIO01				   !
        ENDIF						   !
        IF (ENDF = F3.SALIR) THEN CALL QUIT2               ! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN CALL QUIT2               ! Si presiona tecla ESC
      WEND
      CALL BUSQ.CONV					   ! Busqueda convenio
      CALL CAPTURA.DATOS				   ! captura de datos
WEND

!----
!---- Fin del bloque principal
!----

E0201:
         IF ERRF% = AREA1% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 12,,,950 RECL 98  \! si no existe lo crea.
                        AS AREA1% 			   ! 
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa
         IF ERRF% = AREA3% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            MSG$="Error, Archivo "+ARCH3$+" No Existe ¢ No Se Puede Abrir"
            CALL MSG.ERR(03,MSG$)			   ! Mensaje 
            WAIT;3000					   !
            STOP       
         ENDIF                                             ! del programa
         IF ERRF% = AREA1% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         ENDIF
         IF ERRF% = AREA3% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         ENDIF
         IF ERRF% = 19 AND ERR = "EF" OR ERR="OE" THEN BEGIN  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         ENDIF						       !
         CONSOLE					   ! Control a pantalla
         CALL TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla
         WAIT;3000
         STOP
!--- Fin despliegue de errores

!--- Fin del programa EPSP0401.BAS
