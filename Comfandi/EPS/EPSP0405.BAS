!************************************************** 
!Empresa       : Asic Consulting Group S.A Retail *
!Programa      : EPSP0405.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Reporte de Plu/Convenios         *
!**************************************************

!--- Definicion de Variables de trabajo

%INCLUDE EPSPVARI.BAS				  	   !  Variables programa 
%INCLUDE ADX_UPGM:DMEXTR.J86       !  Inclucion  Libreria  Display  Manager  
%INCLUDE EPSPRUTI.BAS				  	   !  Rutinas Comunes

Function INICIO01
   Call.ORDER% = 225                                          ! Llamado Primera Pantalla D.M
   RET.ERR% = DISPD(Call.ORDER%)                              ! Llamado de la pantalla en DM
   Call DM.ERR(RET.ERR%,DISPD$)
   Call MSG.ERR(5,"Digite C¢digo del Plan")
End Function
!--- Fin de la funcion de inicio

Function LEER.CABECERA
     DM.NOMBRE$   = Left$("COMFAMILIAR ANDI"+String$(30," "),30)
     DM.SIGLA$   = Left$("COMFANDI        "+String$(30," "),30)
     DM.ALMACEN$ = "0000"
End Function
!--- Fin de la funcion de lectura


Function BUSQ.CONV					   ! Busqueda de convenios
  BAN.PRG$ = "0"					   !
  LEC$="C8 C40 2C3 C5 C4 4C1 4C3 C5 3C1 4C3 C3 4C1"        !
  KEY$=PACK$(RIGHT$("0000000000000000"+DM.OPCION$,16))     ! Creacion llave proyecto
  Read Form LEC$;#AREA4% KEY KEY$;                        \! Lee Reg Cabecera 
       DM.LLAVE$,  DM.RAZON$,  DM.FECINI$, DM.FECFIN$,    \!
       DM.VLRCON$, DM.VLRDES$, DM.VALMED$, DM.VALMCA$,    \!
       DM.VALIPS$, DM.VALMET$, DM.METPRA$, DM.METPRB$,    \!
       DM.METPRC$, DM.METPRD$, DM.NIT$,    DM.VALDIA$,    \!
       DM.FACTURA$, DM.DSCVLR$, DM.PTG1$,  DM.PTG2$,      \!
       DM.PTG3$,   DM.PTG4$,   DM.DSCTO$,  DM.VALFAC$,    \!
       DM.VALBEN$, DM.CAPITACION$, DM.COPIATIQ$
  If BAN.PRG$ = "1" Then DM.RAZON$ = "PLAN NO DEFINIDO"
End Function						   !	
!--- Fin de la funcion busqueda de convenios

Function BUSQ.PLU
   BAN.PRG$ = "0"
   KEY$=PACK$(RIGHT$("000000000000"+MID$(C01$,17,12),12))  ! Creacion llave proyecto
   LEC$ = "C6 2C18 C4"                                     !
   Read Form LEC$;#AREA3% KEY KEY$;                       \! Lee EAMITEMR
     TMP.KEY$,DM.BASURA$,DM.NOMPRO$,DM.BASURA$             !
   If BAN.PRG$ = "1" Then DM.NOMPRO$ = "PLU NO DEFINIDO   "
End Function
!--- Fin de la funcion busqueda de productos

Function QUIT2     					   ! Funcion salida programa
    Close AREA1%					   !
    Call SETF("0000000")				   !
    Call CLRSCR						   !
    RET.ERR%= CLSDIS					   !
    Call DM.ERR(RET.ERR%,CLSDIS$)			   !
    Stop
End Function
!--- Fin de la funcion de salida

Function PROCESE.REPORTE
  If MID$(DATE$,1,2) = "99" Then        \  ! Manejo Y2K
   DM.FECINI$ = "19"+DATE$  Else        \
   DM.FECINI$ = "20"+DATE$
 If CONLI% > 55 OR PAGI% = 0 Then Begin  			  ! Final de Hoja o Inicio pagina
    If CONLI% > 55 Then Begin
       Print LINEA$					          ! Impresion linea
       Print CHR$(12)    					  ! Salto de Pagina
    EndIf
     PAGI%=PAGI%+1						  ! Inicializa numero de pagina
     Print LET.CON$					          ! Activa la letra condensada
     Print LINEA$						  ! Imprime linea continua
     LISTA$ =DM.NOMBRE$+STRING$(33," ")+"LISTA PLU/CONVENIOS "+STRING$(33," ")+\
     "P gina No. "+RIGHT$(STRING$(04,"0")+STR$(PAGI%),04)
     Print LISTA$  
     Print DM.SIGLA$+" - "+DM.ALMACEN$+STRING$(79," ")+"Fecha :"+DM.FECINI$
     Print LINEA$
     Print LET.NOR$					  ! Activa la letra normal
!                       1         2         3         4         5         6         7         8         9        10        11        12        13
!              123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012
     Print "Plan Listado  :"+MID$(C01$,1,16)+"  "+DM.RAZON$
     Print " "
!                       1         2         3         4         5         6         7         8         9        10        11        12        13
!              123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012
        Print "   P.L.U       DESCRIPCION ARTICULO   PRECIO     INICIAL      FINAL"
        Print "                                                  AAMMDD     AAMMDD"
     CONLI% = 10             					! Inicializa Contador de lineas
 EndIf
     Print DM.BUFFER$
     CONLI% = CONLI% + 1
End Function
!--- Fin del proceso del reporte

Function IMPRIME
String PLU$
   Call MSG.ERR(05,"Generando Reporte, Espere Por Favor ...")
   LPrintER
   LINEA$=STRING$(131,"-")        				! Linea Continua en la impresion
   CONLI% = 0: PAGI% = 0: I%=1					! Inicializo contadores
   LET.CON$ = CHR$(15)            				! Condensar  letra de impresion
   LET.NOR$ = CHR$(18)            				! Normalizar letra de impresion 
   Read Form "T43 I4 I2 T55 I2 C456"; #AREA1%,I%;              \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$                              !      
   PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
   MAX.R.SEC = 508/REC.LEN                                      ! Length record
   BAN.PRG$ = "0"					        ! Program Control
   FOR I% = 2 TO BLK.NUM                                        ! Cicle to read all blocks
     Read Form "T5 C508"; #AREA1%, I%;  H$                      ! H$ contains block
     X = 1 : R.S = 0 : KEY$ = MID$(H$,X,KEY.LEN)                ! Extract First key
     WHILE KEY$  NE  PZERO$                                     ! Inside sector loop
       R.S = R.S + 1                                            ! Records On This Sector 
       C01$=UNPACK$(MID$(H$,X+0,14))                            ! Convenio/Plu
       If VAL(MID$(C01$,1,16)) = VAL(DM.OPCION$) Then Begin     ! Si es convenio escogido
        If VAL(MID$(C01$,17,12)) >= VAL(DM.TIPO$) AND          \! Si esta dentro del
           VAL(MID$(C01$,17,12)) <= VAL(DM.TIPB$) Then Begin   \! rango seleccionado
          C02$=UNPACK$(MID$(H$,X+14,4))                         ! Precio
          C02$=STR$(VAL(C02$))					!
          C02$=RIGHT$("        "+C02$,8)			! Formateo salida
          C03$=UNPACK$(MID$(H$,X+18,3))                         ! Fecha inicial
          C04$=UNPACK$(MID$(H$,X+21,3))                         ! Fecha inicial
          If DM.VALMCA$ = "1" Then Call BUSQ.PLU	             \! Busqueda articulo
           Else DM.NOMPRO$ = "CTRL. DEPARTAMENTO"		            ! Movimiento x depto
          PLU$ = Str$(Val(Mid$(C01$,17,12)))
          PLU$ = Right$("            "+PLU$,12)
          DM.BUFFER$ = PLU$+"   "+DM.NOMPRO$+"   "+\!
                       C02$+"      "+C03$+"     "+C04$	        !
          Call PROCESE.REPORTE      				! Generaci¢n del Plano
        EndIf
       EndIf
       X = X + REC.LEN                                          ! Index to next key
       KEY$ = MID$(H$,X,KEY.LEN)                                ! Pick up next key
       If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
     WEnd                                                       !
   NEXT I%                                                      !
   Print LET.CON$					        ! Condensa la letra
   Print LINEA$                                                 ! Imprime linea continua
   Print "Fin del Reporte               "                       ! Imprime numero de registros
   Print LET.NOR$  	                        	        ! Normaliza Letra impresora
   Print CHR$(12)   				                ! Realiza salto de pagina
   CONSOLE
End Function
!--- Fin de la funcion de impresion

Function CAPTURA.DATOS					      ! Funcion captura datos
      Call MSG.ERR(05,"Plu Inicial         ")
      DM.TIPO$    = GET.DATOS
      If (EndF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
      If (EndF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
      WHILE DM.TIPO$ = " "
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         DM.TIPO$ = GET.DATOS
         If (EndF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
         If (EndF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
      WEnd
      Call MSG.ERR(05,"Plu Final         ")
      DM.TIPB$ = GET.DATOS
      If (EndF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
      If (EndF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
      WHILE DM.TIPB$ = " " OR (VAL(DM.TIPB$) < VAL(DM.TIPO$))
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         DM.TIPB$ = GET.DATOS
         If (EndF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
         If (EndF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
      WEnd
      Call BUSQ.CONV					      ! Busco convenio
      Call IMPRIME
End Function
!--- Fin de la funcion captura de datos

!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0201
Call INICIADM 				                   ! Inicializacion Variables Display Manager
AREA1% = 11: AREA2% = 12: AREA3% = 13: AREA4% = 14         ! Definicion area de trabajo archivo
EOF$="0"
ARCH1$="PLUCONVE"	                	       	   ! Asignar nombre archivo plu convenios
ARCH3$="EAMITEMR"		                       	   ! Asignar nombre archivo articulos
ARCH4$="CONVENIO"		                       	   ! Asignar nombre archivo convenios
Open ARCH1$ DIRECT RECL 512 AS AREA1%                      ! Abre archivo de Beneficiarios
Open ARCH3$ KEYED  RECL 46 AS AREA3%                       ! Abre archivo Eamitemr
Open ARCH4$ KEYED  RECL 106 AS AREA4%                      ! Abre archivo convenios
TERM$ = " "					           ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   !
Call DM.ERR(RET.ERR%,INITDM$)				   !
RET.ERR% = OPNDIS("ADX_UPGM:EPSPANTA.PNT")	           ! Apertura de la forma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)			           !
FIN$="0"						   !
Call LEER.CABECERA				           ! Lectura archivo control
While (FIN$ = "0") 					   ! Mientras que no F3/Esc
      DM.OPCION$   = "  "				   !
      Call INICIO01					   !
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)			   !
      ATTR$ = SETF(PRM.ON$)                                !
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.OPCION$ = INP$                                    ! Asigna valor capturado
      If (EndF = F1.AYUDA) Then Begin \
         Call HELP("REPORTE DE PLU/PLANES     ","EPSP0405.TXT") !
         Call INICIO01
      EndIf
      If (EndF = F3.SALIR) Then Call QUIT2                 ! Si presiona tecla F3
      If (EndF = ESC.KEY)  Then Call QUIT2                 ! Si presiona tecla ESC
      While DM.OPCION$ = " "				   !
        Call MSG.ERR(5,"Digite C¢digo del Plan")
        RET.ERR% = NXTF(-20)				   !
        Call DM.ERR(RET.ERR%,NXTF$)			   !
        ATTR$ = SETF(PRM.ON$)                              ! 
        INP$ = UPDF                                        ! Captura dato en pantalla
        DM.OPCION$ = INP$                                  ! Asigna valor capturado
        If (EndF = F1.AYUDA) Then Begin 		  \!
           Call HELP("REPORTE DE PLU/PLANES     ","EPSP0405.TXT") !
           Call INICIO01				   !
        EndIf						   !
        If (EndF = F3.SALIR) Then Call QUIT2               ! Si presiona tecla F3
        If (EndF = ESC.KEY)  Then Call QUIT2               ! Si presiona tecla ESC
      WEnd
      Call CAPTURA.DATOS
WEnd

!----
!---- Fin del bloque principal
!----

E0201:
         If ERRF% = AREA1% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") Then Begin          ! el archivo de control
            MSG$="Error, Archivo PLUCONVE No Existe ¢ No Se Puede Abrir"
            Call MSG.ERR(03,MSG$)			   ! Mensaje 
            WAIT;3000					   !
            Call QUIT2
         EndIf                                             ! del programa
         If ERRF% = AREA1% AND ERR = "EF" Then Begin      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         EndIf
         If ERRF% = AREA3% AND ERR = "EF" Then Begin      \! Valida Lectura
            BAN.PRG$ = "1"                                 ! temporal de carga
            RESUME				           ! 
         EndIf						   !
         If ERRF% = AREA4% AND ERR = "EF" Then Begin      \! Valida Lectura
            BAN.PRG$ = "1"                                 ! temporal de carga
            RESUME				           ! 
         EndIf						   !
         If ERRF% = 19 AND ERR = "EF" OR ERR="OE" Then Begin  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         EndIf						       !
         CONSOLE					   ! Control a pantalla
         Call TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:Print MSG$                          ! Presenta Error pantalla
         WAIT;3000
         STOP
!--- Fin despliegue de errores

!--- Fin del programa EPSP0405.BAS
