!************************************************** 
!Empresa       : Asic Consulting Group S.A Retail *
!Programa      : EPSP0402.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Reporte de Convenios             *
!**************************************************

!--- Definicion de Variables de trabajo

%INCLUDE EPSPVARI.BAS				  	   ! Variables programa
%INCLUDE ADX_UPGM:DMEXTR.J86  	   ! Inclucion Libreria Display Manager
%INCLUDE EPSPRUTI.BAS				  	   ! Rutinas Comunes

FUNCTION LEER.CABECERA
     DM.NOMBRE$   = Left$("COMFAMILIAR ANDI"+String$(30," "),30)
     DM.SIGLA$   = Left$("COMFANDI        "+String$(30," "),30)
     DM.ALMACEN$ = "0000"
End FUNCTION
!--- Fin de la funcion de lectura

FUNCTION QUIT2     					   ! Funcion salida programa
    CLOSE AREA1%					   !
    STOP
END FUNCTION
!--- Fin de la funcion de salida

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
     LISTA$ =DM.NOMBRE$+STRING$(33," ")+"LISTA   DE  PLANES  "+STRING$(33," ")+\
     "P gina No. "+RIGHT$(STRING$(04,"0")+STR$(PAGI%),04)
     PRINT LISTA$  
     PRINT DM.SIGLA$+" - "+DM.ALMACEN$+STRING$(79," ")+"Fecha :"+DM.FECINI$
     PRINT LINEA$
!                       1         2         3         4         5         6         7         8         9        10        11        12        13
!              123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012
        PRINT "COD. PLAN                  NOMBRE DEL PLAN                   INICIO    FINAL   VLR CONTRATO  VLR DESPACHO"
        PRINT "                                                             AAMMDD   AAMMDD"
!              xxxxxxxxxxxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx   xxxxxx   xxxxxx     xxxxxxxxxx      xxxxxxxx
     CONLI% = 09             					! Inicializa Contador de lineas
 ENDIF
     PRINT DM.BUFFER$
     CONLI% = CONLI% + 1
END FUNCTION
!--- Fin del proceso del reporte

FUNCTION IMPRIME
   LPRINTER
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
       DM.CONVEN$ = UNPACK$(MID$(H$,X+0,08))                    ! Convenio
       DM.CONVEN$ = Str$(Val(DM.CONVEN$))                       !
       DM.CONVEN$ = Right$("              "+DM.CONVEN$,16)      ! 
       DM.RAZON$  = MID$(H$,X+08,40)                            ! Descripcion
       DM.FECINI$ = UNPACK$(MID$(H$,X+48,3))                    ! Fecha inicial
       DM.FECFIN$ = UNPACK$(MID$(H$,X+51,3))                    ! Fecha inicial
       DM.VLRCON$ = STR$(VAL(UNPACK$(MID$(H$,X+53,5))))         ! Valor Contrato
       DM.VLRCON$ = RIGHT$("          "+DM.VLRCON$,10)		!      
       DM.VLRDES$ = STR$(VAL(UNPACK$(MID$(H$,X+59,4))))         ! Valor Despacho
       DM.VLRDES$ = RIGHT$("        "+DM.VLRDES$,08)
       DM.BUFFER$ = DM.CONVEN$+"  "+DM.RAZON$+"   "+DM.FECINI$+\!
                    "   "+DM.FECFIN$+"     "+DM.VLRCON$+       \!
                    "      "+DM.VLRDES$
       CALL PROCESE.REPORTE      				! Generaci¢n del Plano
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

!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0201
AREA1% = 11: AREA2% = 12: AREA3% = 13: AREA4% = 14         ! Definicion area de trabajo archivo
ARCH1$="CONVENIO"	        	               	   ! Asignar nombre archivo convenios
OPEN ARCH1$ DIRECT RECL 512 AS AREA1%                      ! Abre archivo de Convenios
Call LEER.CABECERA				           ! Lectura archivo control
CALL IMPRIME      					   ! Proceso del reporte
CALL QUIT2						   ! Fin ejecucion programa

!----
!---- Fin del bloque principal
!----

E0201:
         IF ERRF% = AREA1% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            LOCATE 24,1
            PRINT "Error, Archivo Convenios No Existe ¢ No Se Puede Abrir"
            WAIT;3000					   !
            STOP
         ENDIF                                             ! del programa
         IF ERRF% = AREA1% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
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

!--- Fin del programa EPSP0402.BAS
