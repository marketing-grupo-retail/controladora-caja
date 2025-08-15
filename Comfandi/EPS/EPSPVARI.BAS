!************************************************** 
!Empresa       : Asic Consulting Group S.A Retail *
!Programa      : EPSPVARI.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Definicion de variables programa *
!**************************************************

!--- Definicion de Variables de trabajo

STRING   GLOBAL       \
         DM.OPCION$,  \         ! Variable Captura de opciones
         DM.OPCION2$, \         ! Variable Captura de opciones
         DM.NIT$,     \  	! Nit de la empresa
         DM.RAZON$,   \		! Razon social empresa
         DM.SIGLA$,   \ 	! Nombre Comercial
         DM.ALMACEN$, \	        ! Codigo del punto de venta
         DM.BASURA$,  \		! Resto del registro
         DM.LLAVE$,   \         ! Llave registro de control
         DM.NOMPRO$,  \         ! Nombre del proyecto
         DM.CODIGO$,  \         ! Codigo del afiliado
         DM.NOMBRE$,  \         ! Nombre del afiliado
         DM.ESTPRO$,  \         ! Estado del proyecto
         DM.FECNAC$,  \         ! Fecha de nacimiento 
         DM.PAREN$,   \         ! Parentesco           
         DM.CODALT$,  \         ! Codigo alterno interfaces
         DM.VALFAC$,  \         ! Codigo alterno interfaces
         DM.EMPRES$,  \
         DM.REDPRE$,  \
         DM.FECFIN$,  \
         DM.ITEM$,    \
         DM.ACCION$,  \
         EOF$,        \
         REGISTRO$,   \
         DM.FECINI$

STRING   GLOBAL       \
         DM.PLAN$,    \
         DM.ESAF$,    \
         DM.TPRE$,    \
         DM.RDPR$,    \ 
         DM.TIPB$,    \         ! Tipo de beneficiario     
         DM.ESTO$,    \         ! Estrato
         DM.TIPO$,    \         ! Tipo de identificacion
         DM.DATO$,    \         ! Captura de dato en pantalla
         TOT.CAM$,    \         ! Captura de dato en pantalla
         MSG$                   ! Mensajes temporales

INTEGER*2 GLOBAL        \
          DM.NOPROY%,   \	! Numero de proyectos definidos
          DM.NOCAMPOS%, \	! Numero de campos en proyecto
          RET.ERR%,     \       ! Numero de rutina para Display Manager
          CALL.ORDER%,  \       ! Numero de orden de llamado de pantalla
          DM.NOREG%,    \       ! Numero de registros
          DM.NOCAMP%,   \       ! Numero de campos
          INI.CAMPO%    \       ! Control despliege de proyectos en pantalla

     
STRING GLOBAL ARCH1$,   \	! Variable nombre arch.
              ARCH2$,   \	! Variable nombre arch.        
              ARCH3$,   \	! Variable nombre arch. 
              ARCH4$,   \	! Variable nombre arch.          
              ARCH5$,   \	! Variable nombre arch.          
              BAN.PRG$, \	! Bandera del programa
              LEC$,    	\	! Definicion formato registro
              FIN$,     \	! Variable de control programa
              TMP1$,    \       ! Variable temporal de trabajo
              FIN2$,    \	! Variable de control programa
              CICLO$,   \	! Variable de control programa
              TMP.MSG$, \       ! Variable temporal de trabajo
              TMP.KEY$, \	! Almacena temporalmente la llave del registro
              NOREG$,   \       ! Numero de registros
              NOCAMP$,  \       ! Numero de campos
              CADENA$   \       ! Cadena de caracteres parametrizacion

INTEGER   GLOBAL     \
          AREA1%,    \          ! Definicion area de trabajo
          AREA2%,    \          ! Definicion area de trabajo
          AREA3%,    \          ! Definicion area de trabajo
          AREA4%,    \          ! Definicion area de trabajo
          AREA5%,    \          ! Definicion area de trabajo
          PNT.ANT%,  \          ! Captura de la pantalla anterior
          CURR.FLD%, \          ! Campo actual 
          ENTER.KEY, \         	! Definicion de la tecla Enter
          TAB.KEY,   \          ! Definicion de la tecla TAB
          ESC.KEY,   \        	! Definicion de la tecla ESCAPE
          F1.AYUDA,  \         	! Deficion de la tecla de funcion F1 
          F2.KEY  ,  \         	! Deficion de la tecla de funcion F2 
          F3.SALIR,  \      	! Deficion de la tecla de funcion F3
          F4.KEY  ,  \         	! Deficion de la tecla de funcion F4 
          F5.KEY  ,  \         	! Deficion de la tecla de funcion F5 
          F6.KEY  ,  \         	! Deficion de la tecla de funcion F6 
          F7.KEY  ,  \         	! Deficion de la tecla de funcion F7 
          F8.KEY  ,  \         	! Deficion de la tecla de funcion F8 
          F9.KEY  ,  \         	! Deficion de la tecla de funcion F9 
          FA.KEY  ,  \         	! Deficion de la tecla de funcion F10
          HELP%   ,  \        	! Llamado Segunda Pantalla Display Manager
          J%,        \		! Control de ciclos
          K%,        \		! Control de ciclos
          L%,        \		! Control de ciclos
          T%,        \          ! Control de ciclos
          CAMPO%,    \          ! Captura posicion en pantalla del cursor
          F%,        \          ! Variable de control ciclico
          LONGITUD%, \          ! Logitud cadena de caracteres
          NOREG%, NOCAMP%

STRING  GLOBAL  INITDM$, OPNDIS$, DISPD$,  POSF$,   NXTF$,     \
                PUTF$,   CURS$,   CLSDIS$, PRM.ON$, PRM.OFF$,  \
                ON$,     OFF$,    TABS$,   TERM$,   INP$,      \
                ATTR$,   RET$,    KEY$,    KEY2$,   ERRFX$,    \
                Z$

STRING GLOBAL  DM.VLRCON$, DM.VLRDES$, DM.VALMED$, DM.VALMCA$, \
               DM.VALIPS$, DM.VALMET$, DM.METPRA$, DM.METPRB$, \
               DM.METPRC$, DM.METPRD$, DM.CONVEN$, HLP.PRG$,   \
               HLP.FILE$,  MSG1$,      REG.HLP$,   INP2$,      \
               DM.VALBEN$, DM.VALDIA$, DM.FACTURA$, DM.PTG1$,  \
               DM.PTG2$,   DM.PTG3$,   DM.PTG4$,    DM.DSCVLR$,\
               DM.DSCTO$

INTEGER*4 GLOBAL HX%, S%, SX%, SUM%, NRG%, CNTI%, BLK.NUM, REC.LEN, KEY.LEN, \
          I%

INTEGER*2 GLOBAL CONLI%, PAGI%, MAX.R.SEC, X, R.S

STRING    GLOBAL LINEA$, LET.CON$, LET.NOR$, LISTA$, DM.BUFFER$, H$, PZERO$, \
          C01$, C02$, C03$, C04$, C05$, DEPTOS$(2), DM.CAPITACION$, DM.NEWKEY$, DM.SUCURSAL$,  \
          DM.COPIATIQ$

!--- Fin del programa EPSPVARI.BAS
