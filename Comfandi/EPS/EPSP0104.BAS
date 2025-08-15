!************************************************** 
!Empresa       : Asic Consulting Group S.A Retail *
!Programa      : EPSP0104.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Mantenimiento Plu Convenios      *
!**************************************************
! Se modifica la longitud del convenio pasa de 12 a 14
! y la llave queda de 38 caracteres
! ajustado el 13 julio 2006 por OVS - ASIC
!---------------------------------------------------

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
  LEC$="C8 C40 2C3 C5 C4 4C1 4C3 C5 3C1 4C3 C3 4C1"        !
  READ FORM LEC$;#AREA2% KEY KEY$;                        \! Lee Reg Cabecera 
       DM.LLAVE$,  DM.RAZON$,  DM.FECINI$, DM.FECFIN$,    \!
       DM.VLRCON$, DM.VLRDES$, DM.VALMED$, DM.VALMCA$,    \!
       DM.VALIPS$, DM.VALMET$, DM.METPRA$, DM.METPRB$,    \!
       DM.METPRC$, DM.METPRD$, DM.NIT$,    DM.VALDIA$,    \!
       DM.FACTURA$, DM.DSCVLR$, DM.PTG1$,  DM.PTG2$,      \!
       DM.PTG3$,   DM.PTG4$,   DM.DSCTO$,  DM.VALFAC$,    \!
       DM.VALBEN$, DM.CAPITACION$, DM.COPIATIQ$
  IF BAN.PRG$ = "0" THEN CALL MSG.ERR(3,DM.RAZON$)         !
  IF BAN.PRG$ = "1" THEN CALL MSG.ERR(3,"Convenio No Definido ...")
END FUNCTION						   !	
!--- Fin de la funcion busqueda de convenios

FUNCTION CLEAN.PANTA
  CALL.ORDER% = 210                                        ! Llamado Primera Pantalla D.M
  RET.ERR% = DISPD(CALL.ORDER%)                            ! Llamado de la pantalla en DM
  CALL DM.ERR(RET.ERR%,DISPD$)                             !
  CALL MSG.ERR(1,MSG$)				           ! Mensaje cabecera pantalla
  RET.ERR% = NXTF(-20) 				           !
  CALL DM.ERR(RET.ERR%,NXTF$)				   !
  ATTR$ = SETF(PRM.ON$)                                	   !
  CALL MSG.ERR(2,DM.CONVEN$)
  CALL MSG.ERR(3,DM.RAZON$)                                !
  CAMPO% = POSF(0)					   !
  WHILE CAMPO% < 4 					   !
     RET.ERR% = NXTF(01)                                   !
     CALL DM.ERR(RET.ERR%,NXTF$)			   !
     ATTR$ = SETF(PRM.ON$)                                 !
     CAMPO% = POSF(0)					   !
  WEND							   !
END FUNCTION						   !
!--- Fin funcion limpieza de pantalla

FUNCTION PLUCONVE
  BAN.PRG$ = "0"					   !
  LEC$="C14 C4 2C3"					   !
  TMP.KEY$=(RIGHT$("0000000000000000"+DM.CONVEN$,16))          ! Creacion llave proyecto
  KEY$=(RIGHT$("000000000000"+DM.CODIGO$,12))              ! Creacion llave proyecto
  KEY$=PACK$(RIGHT$("0000000000000000000000000000"+TMP.KEY$+KEY$,28)) ! Creacion llave proyecto
  READ FORM LEC$;#AREA1% KEY KEY$;                        \! Lee Reg Cabecera 
       DM.LLAVE$,DM.TIPB$,DM.FECINI$,DM.FECFIN$            !
  IF BAN.PRG$ = "0" THEN BEGIN
     CALL MSG.ERR(CAMPO%+2,UNPACK$(DM.TIPB$))
     CALL MSG.ERR(CAMPO%+3,UNPACK$(DM.FECINI$))
     CALL MSG.ERR(CAMPO%+4,UNPACK$(DM.FECFIN$))
  ENDIF
END FUNCTION
!--- Fin del procedimiento de busqueda

FUNCTION GRABA.REG
  LEC$="C14 C4 2C3"
  TMP.KEY$=(RIGHT$("0000000000000000"+DM.CONVEN$,16))      ! Creacion llave proyecto
  KEY$=(RIGHT$("000000000000"+DM.CODIGO$,12))              ! Creacion llave proyecto
  KEY$=PACK$(RIGHT$("0000000000000000000000000000"+TMP.KEY$+KEY$,28)) ! Creacion llave proyecto
  DM.TIPB$   = PACK$(RIGHT$("00000000"+DM.TIPB$,8))        !
  DM.FECINI$ = PACK$(RIGHT$("000000"+DM.FECINI$,6))        !
  DM.FECFIN$ = PACK$(RIGHT$("000000"+DM.FECFIN$,6))        !
  WRITE FORM LEC$;#AREA1%;KEY$,DM.TIPB$,DM.FECINI$,DM.FECFIN$ !
END FUNCTION
!--- Fin grabacion de registro

FUNCTION BUSQ.PLU
   BAN.PRG$ = "0"
   KEY$=PACK$(RIGHT$("000000000000"+DM.CODIGO$,12))        ! Creacion llave proyecto
   LEC$ = "C6 2C18 C4"                                     !
   READ FORM LEC$;#AREA3% KEY KEY$;                       \! Lee EAMITEMR
     TMP.KEY$,DM.BASURA$,DM.NOMPRO$,DM.BASURA$             !
END FUNCTION
!--- Fin de la funcion busqueda de productos
                                            
FUNCTION CAPTURA.DATOS					      ! Funcion captura datos
STRING A$
CAMPO% = POSF(0)
WHILE CAMPO% < 72
     BAN.PRG$ = "1"
     WHILE BAN.PRG$ = "1"
      IF DM.VALMCA$ = "1" THEN \
         CALL MSG.ERR(72,"Digite PLU del Producto ") ELSE \   ! Msg en pantalla
         CALL MSG.ERR(72,"Digite C¢digo Departamento ")
      DM.CODIGO$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
      WHILE DM.CODIGO$ = " " OR VAL(DM.CODIGO$) = 0
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.CODIGO$ = GET.DATOS                               ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
      WEND
      CAMPO% = POSF(0)
      IF DM.VALMCA$ = "1" THEN BEGIN
         CALL PLUCONVE
         CALL BUSQ.PLU
         IF BAN.PRG$ = "1" THEN BEGIN \
            CALL MSG.ERR(CAMPO%+1,"Producto No Existe")
            RET.ERR% = NXTF(-2)
            CALL DM.ERR(RET.ERR%,NXTF$)
         ENDIF
         IF BAN.PRG$ = "0" THEN   \
            CALL MSG.ERR(CAMPO%+1,DM.NOMPRO$)
      ENDIF ELSE CALL PLUCONVE: BAN.PRG$ = "0"
     WEND
     CAMPO% = POSF(0)
     IF DM.VALMCA$ = "1" THEN BEGIN                         ! Control x Articulo
        CALL MSG.ERR(72,"Digite el Precio del PLAN ")
        DM.TIPB$ = GET.DATOS
        IF (ENDF = F3.SALIR) THEN EXIT FUNCTION             ! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION             ! Si presiona tecla ESC
        WHILE (DM.TIPB$ = " ")
           RET.ERR% = NXTF(-2)
           CALL DM.ERR(RET.ERR%,NXTF$)
           DM.TIPB$ = GET.DATOS
           IF (ENDF = F3.SALIR) THEN EXIT FUNCTION          ! Si presiona tecla F3
           IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION          ! Si presiona tecla ESC
        WEND
     ENDIF ELSE BEGIN
           A$ = "Error: No Definido"
           FOR I% = 1 TO T%
               IF VAL(DEPTOS$(I%,1)) = VAL(DM.CODIGO$) THEN  \!
                  A$ = MID$(DEPTOS$(I%,2),2,20)  	      ! Nombre del departamento
           NEXT I%
           CALL MSG.ERR(CAMPO%+1,A$)
           CALL MSG.ERR(CAMPO%+2,"00000000")
           DM.TIPB$ = "00000000"
           RET.ERR% = NXTF(2)				    ! Salta campo siguiente
           CALL DM.ERR(RET.ERR%,NXTF$)
     ENDIF
      CALL MSG.ERR(72,"Fecha Inicial del Precio AAMMDD                                 ")
      DM.FECINI$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
      BAN.PRG$ = "0"
      CALL VAL.FECHA(DM.FECINI$)
      WHILE (DM.FECINI$ = " " OR BAN.PRG$ = "1")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.FECINI$ = GET.DATOS
         BAN.PRG$ = "0"
         CALL VAL.FECHA(DM.FECINI$)
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
      WEND
      CALL MSG.ERR(72,"Fecha Final   del Precio AAMMDD                                 ")
      DM.FECFIN$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
      BAN.PRG$ = "0"
      CALL VAL.FECHA(DM.FECFIN$)
      WHILE (DM.FECFIN$ = " " OR (VAL(DM.FECINI$) > VAL(DM.FECFIN$)) OR \
             BAN.PRG$ = "1")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         DM.FECFIN$ = GET.DATOS
         BAN.PRG$ = "0"
         CALL VAL.FECHA(DM.FECFIN$)
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
      WEND
      CALL GRABA.REG
      CAMPO% = POSF(0)
      IF CAMPO% >= 71 THEN CALL CLEAN.PANTA
WEND
END FUNCTION
!--- Fin de la funcion captura de datos

FUNCTION INICIO4
  CALL.ORDER% = 210                                          ! Llamado Primera Pantalla D.M
  RET.ERR% = DISPD(CALL.ORDER%)                              ! Llamado de la pantalla en DM
  CALL DM.ERR(RET.ERR%,DISPD$)
  CALL MSG.ERR(1,MSG$)				             ! Mensaje cabecera pantalla
END FUNCTION
!--- Fin de la funcion de inicio

FUNCTION CARGAR.DEPTIVA
STRING AUX1$, A$, MSG$
EXIT FUNCTION
OPEN "DEPTIVA" AS 5 NOWRITE NODEL
IF END #5 THEN FIN
DIM DEPTOS$(600,5)
WHILE (1)
  T% = T% + 1
  READ #5; DEPTOS$(T%,1), AUX1$, DEPTOS$(T%,2) !PARA CAFAM O CARULLA
WEND
FIN:
T% = T% - 1
CLOSE 5

!STRING AUX1$, A$, MSG$
!OPEN "DEPTIVA" AS 5 NOWRITE NODEL
!IF END #5 THEN FIN
!DIM DEPTOS$(200,2)
!T% = 1
!WHILE (1)
!  READ #5;LINE AUX1$
!  J% = 1				! LLAVE DIFERENCIADORA ENTRE IVAS EN MISMO DEPTO
!  GOSUB GETFIELD
!  GOSUB GETFIELD
!  GOSUB GETFIELD
!  MSG$ = A$                             ! DESCRIPCION GRUPO
!  WHILE (J% < LEN(AUX1$)) 
!         GOSUB GETFIELD
!         BAN.PRG$ = "0"
!         FOR I% = 1 TO 200
!            IF VAL(DEPTOS$(I%,1)) = VAL(A$) THEN BEGIN
!               BAN.PRG$ = "1"
!               I% = 200
!            ENDIF
!         NEXT I%
!         IF BAN.PRG$ = "0" THEN BEGIN
!            EOF$ = A$
!            DEPTOS$(T%,1) = A$
!            EOF$ = MSG$
!            DEPTOS$(T%,2) = MSG$
!            T% = T% + 1
!         ENDIF 
!  WEND
!WEND
!FIN:
!T% = T% - 1
!CLOSE 5

EXIT FUNCTION

GETFIELD:
  K% = MATCH(",",AUX1$,J%)  ! SEARCH FOR FIELD SEPERATOR
  A$ = MID$(AUX1$,J%,K%-J%) ! FIELD
  J% = K% + 1               ! POINT TO BEGINNING OF NEXT FIELD
RETURN
END FUNCTION
!--- Fin de la carga del deptiva

!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0101
CALL INICIADM 				                   ! Inicializacion Variables Display Manager
CALL CARGAR.DEPTIVA
AREA1% = 11: AREA2% = 12: AREA3% = 13   	                 ! Definicion area de trabajo archivo
ARCH1$="PLUCONVE"		                                       ! Asignar nombre archivo beneficiarios
ARCH2$="CONVENIO"	        		       	                     ! Asignar nombre archivo convenios
ARCH3$="EAMITEMR"	                       		               ! Asignar nombre archivo convenios
OPEN ARCH1$ KEYED RECL 24 AS AREA1%                        ! Abre archivo plu/convenio
OPEN ARCH2$ KEYED RECL 106 AS AREA2%                       ! Abre archivo convenios
OPEN ARCH3$ KEYED RECL 46 AS AREA3%                        ! Abre archivo Eamitemr
TERM$ = " "					           ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   !
CALL DM.ERR(RET.ERR%,INITDM$)				   !
RET.ERR% = OPNDIS("ADX_UPGM:EPSPANTA.PNT")	           ! Apertura de la forma de pantalla
CALL DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
MSG$ = "MANTENIMIENTO DE PLU/PLAN"
CALL INICIO4 
WHILE (FIN$ = "0") 
      MSG$="Digite C¢digo del Plan"
      CALL MSG.ERR(72,MSG$)                                ! Mensaje
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.CONVEN$ = INP$                                    ! Asigna valor capturado
      IF (ENDF = F1.AYUDA) THEN BEGIN \
         CALL HELP("DEFINICION PLU/PLAN      ","EPSP0104.TXT") !
         CALL INICIO4
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
           CALL HELP("DEFINICION PLU/PLAN      ","EPSP0104.TXT") !
           CALL INICIO4
        ENDIF
        IF (ENDF = F3.SALIR) THEN CALL QUIT2               ! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN CALL QUIT2               ! Si presiona tecla ESC
      WEND
      KEY$=PACK$(RIGHT$("0000000000000000"+DM.CONVEN$,16))     ! Creacion llave proyecto
      CALL BUSQ.CONV                                       ! Busqueda convenio
      IF BAN.PRG$ = "0" THEN CALL CAPTURA.DATOS            ! Captura datos
WEND

!----
!---- Fin del bloque principal
!----

E0101:
         IF ERRF% = AREA1% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 14,,,100000 RECL 24 \! si no existe lo crea.
                        AS AREA1% 	compound perupdate 	   ! 
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa
         IF ERRF% = AREA2% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            CREATE POSFILE ARCH2$ KEYED 8,,,5000 RECL 106  \! si no existe lo crea.
                        AS AREA2% compound perupdate 			   ! 
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa
         IF ERRF% = AREA3% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            MSG$ = "Error: Archivo Productos EAMITEMR No Existe"   
            LOCATE 22,6:PRINT MSG$                         ! Presenta Error pantalla
            WAIT;2000
            CHAIN "EPS.286"
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
         IF ERRF% = 19 AND ERR = "EF" OR ERR="OE" THEN BEGIN  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         ENDIF						       !
         CALL TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla
!--- Fin despliegue de errores

!--- Fin del programa EPSP0104.BAS
