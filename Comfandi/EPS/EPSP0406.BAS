!************************************************** 
!Empresa       : Asic Consulting Group S.A Retail *
!Programa      : EPSP0406.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Reporte Consolidado de Formulas  *
!**************************************************

%ENVIRON C						   												! Ambiente de controlador

%INCLUDE POSPVARI.BAS					   ! Variables del programa

String global DM.VALDIA$,      \!
       DM.FACTURA$, DM.DSCVLR$, DM.PTG1$,  DM.PTG2$,        \!
       DM.PTG3$,   DM.PTG4$, DM.DSCTO$, DM.VALFAC$,         \!
       DM.VALBEN$, DM.CAPITACION$

Integer*4 Global TOT.UNI%, TOT.VTAS%, CONTADOR%,  ACUMMEDIO(1), Vlr.Venta%

String Global DM.VALOR$, SALIDA$, \
              UE.CUOTA$, UE.COOPAGO$, ARTICULOS$(2), \
              DM.TRX$, DM.CAJA$, UE.INDTRX$

String    Global Nro.Convenio$, Nro.Medico$, Nro.Eps$, Nro.Formula$, \
                 Nro.Beneficiario$,  Nro.Diagnostico$, Nro.Estrato$, Vlr.Coopago$


%INCLUDE ADX_UPGM:EAMITEMR.J86			           ! Variable EAMITEMR
%INCLUDE DMEXTR.J86          				   ! Inclucion Libreria Display Manager
%INCLUDE PORPRUTI.BAS				  	   ! Rutinas Comunes
%INCLUDE BASROUT.J86					   ! 

Function INICIO1
  Call.ORDER% = 213                                        ! Llamado Primera Pantalla D.M
  RET.ERR% = DISPD(Call.ORDER%)                            ! Llamado de la pantalla en DM
  Call DM.ERR(RET.ERR%,DISPD$)
  Call MSG.ERR(1,"  REPORTE CONSOLIDADO DE FORMULAS  ")
End Function
!--- Fin de la funcion de inicio

Function TERMINE.PROG
  Call SETF("0000000")				   !
  Call CLRSCR					   !
  RET.ERR%= CLSDIS				   !
  Call DM.ERR(RET.ERR%,CLSDIS$)			   !
  STOP
End Function
!--- Fin de la ejecucion del programa

Function LEER.CABECERA
     DM.NOMBRE$   = Left$("COMFAMILIAR ANDI"+String$(30," "),30)
     DM.SIGLA$   = Left$("COMFANDI        "+String$(30," "),30)
     DM.ALMACEN$ = "0000"
End Function
!--- Fin de la funcion de lectura

Function LEE.ITEMR(DATO1$)
String DATO1$
  IR.ITEMCODE$ = PACK$(RIGHT$("000000000000"+STR$(VAL(DATO1$)),12))
  BAN.PRG$ = "0"					      !
  %INCLUDE ADX_UPGM:EAMIRRD1.J86                           ! Lectura del EAMITEMR     
End Function
!--- Fin de lectura del EAMITEMR

Function BUSQ.CONV(DATO$)				   ! Busqueda de planes
String DATO$, DM.NEWKEY$               
  BAN.PRG$ = "0"					   !
  LEC$="C8 C40 2C3 C5 C4 4C1 4C3 C5 3C1 4C3 C3 3C1"
  DM.CONVEN$=(Right$("0000000000000"+DM.CONVEN$,13))         ! Creacion llave proyecto
  DM.PLAN$=(Right$("000"+DM.PLAN$, 3))                       ! Creacion llave proyecto
  DM.NEWKEY$ = Pack$(DM.CONVEN$ + DM.PLAN$)
  Read Form LEC$;#AREA1% KEY DM.NEWKEY$;                    \! Lee Reg Cabecera 
       DM.LLAVE$,  DM.RAZON$,  DM.FECINI$, DM.FECFIN$,      \!
       DM.VLRCON$, DM.VLRDES$, DM.VALMED$, DM.VALMCA$,      \!
       DM.VALIPS$, DM.VALMET$, DM.METPRA$, DM.METPRB$,      \!
       DM.METPRC$, DM.METPRD$, DM.NIT$,    DM.VALDIA$,      \!
       DM.FACTURA$, DM.DSCVLR$, DM.PTG1$,  DM.PTG2$,        \!
       DM.PTG3$,   DM.PTG4$, DM.DSCTO$, DM.VALFAC$,         \!
       DM.VALBEN$, DM.CAPITACION$
  If BAN.PRG$ = "1" Then DM.NIT$ = "000000000000"	   !
  If BAN.PRG$ = "0" Then DM.NIT$ = RIGHT$(UNPACK$(DATO$),12)
End Function						   !	
!--- Fin de la funcion busqueda de convenios

Function BUSQ.AFIL(DATO1$,DATO2$)			   ! Busqueda afiliados
String DATO1$,DATO2$
  BAN.PRG$ = "0"					   ! Inicializa bandera 
  DATO2$ = RIGHT$("00000000000000"+DATO2$,14)
  DATO1$ = PACK$(DATO2$ + DATO1$)
!  VISOR$ = DATO1$
  LEC$="C13 C1 C52 C1 C32"
  READ FORM LEC$;#AREA2% KEY DATO1$;			  \! Busca beneficiario
        DM.LLAVE$, DM.TIPO$, DM.BASURA$,DM.TIPB$,         \!
        DM.BASURA$					   !
  If BAN.PRG$ = "1" Then Begin
     DM.TIPO$ = "0"
     DM.TIPB$ = "0"
  EndIf
End Function
!--- Fin busqueda de beneficiarios

Function IMP.LINEA
  If MID$(DATE$,1,2) = "99" Then        \  ! Manejo Y2K
   DM.FECINI$ = "19"+DATE$  Else        \
   DM.FECINI$ = "20"+DATE$
 If CONLI% > 55 OR PAGI% = 0 Then Begin  			  ! Final de Hoja o Inicio pagina
    If CONLI% > 55 Then Begin
       PRINT LINEA$					          ! Impresion linea
       PRINT CHR$(12)    					  ! Salto de Pagina
    EndIf
     PAGI%=PAGI%+1						  ! Inicializa numero de pagina
     PRINT LET.CON$					          ! Activa la letra condensada
     PRINT LINEA$						  ! Imprime linea continua
     LISTA$ =DM.RAZON$+String$(33," ")+" DETALLE MOVIMIENTOS"+String$(33," ")+\
     "Pagina No. "+RIGHT$(String$(04,"0")+STR$(PAGI%),04)
     PRINT LISTA$  
     PRINT DM.SIGLA$+" - "+DM.ALMACEN$+String$(79," ")+"Fecha :"+DM.FECINI$
     PRINT LINEA$
     PRINT LET.NOR$
     PRINT "DETALLE DE FORMULAS MEDICAS"
     PRINT "FECHA MOVIMIENTO AAMMDD : "+FECHA.CIERRE$+"       ALMACEN : "+DM.ALMACEN$
     PRINT " "
!                    1         2         3         4         5         6         7         8         9        10        11        12        13
!           123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012
!     PRINT "   Codigo Convenio                    Vlr.Formula  Vlr.Cuota  Vlr.Dscto  Vlr.Cobro EPS"
     PRINT "   Codigo Convenio                    Vlr.Formula  Vlr.Cuota             Vlr.Cobro EPS"	 
     PRINT " "
     CONLI%=8              					  ! Inicializa Contador de lineas
 EndIf
 PRINT DM.BUFFER$
 CONLI% = CONLI% + 1
End Function
!--- Fin procedimiento impresion linea

Function CAMBIO.CONVENIO
      DM.DIA02$ = RIGHT$("        "+STR$(INDICAT1),8)		  !
      DM.DIA03$ = RIGHT$("        "+STR$(INDICAT2),8)		  !
      DM.DIA04$ = RIGHT$("         "+STR$(INDICAT3),9)		  !
	  DM.DIA01$ = "         "

      DM.BUFFER$ = "          "+DM.CONVEN$+String$(19," ")+DM.DIA02$+\!
                   "   "+DM.DIA04$+"  "+DM.DIA01$+"     "+DM.DIA03$   !
				   
      Call IMP.LINEA						  ! Imprime Linea
      TOT.UNI%  = TOT.UNI%  + INDICAT3		  ! Total Coopagos
      TOT.VTAS% = TOT.VTAS% + INDICAT1        ! Total facturas
      CONTADOR% = CONTADOR% + INDICAT2		  ! Total Cobros EPS
      	  
      INDICAT1   = 0						  ! Total cuota moderadora
      INDICAT2   = 0						  ! Total Cobrar EPS
      INDICAT3   = 0						  ! Total descuentos
End Function
!--- Fin cambio convenio

Function REPORTE
 CONLI% = 0: PAGI% = 0: I%=0					  ! Inicializacion contadores
 LET.CON$ = CHR$(15)            				  ! Condensar  letra de impresion
 LET.NOR$ = CHR$(18)            				  ! Normalizar letra de impresion 
 LET.GRA$ = CHR$(14)            				  ! Agrandar   letra de impresion
 LINEA$=String$(131,"-")        				  ! Linea Continua en la impresion
 OPEN "ADX_UDT1:TMPOK.TXT" AS 32 				  ! Apertura archivo ordenado
 If End #32 Then EOF.REPORTE                                      ! Si encuentra final del archivo
 LPRINTER
 READ #32;LINE H$					          ! Lectura primer registro
 DM.CONVEN$ = MID$(H$,1,24)					  ! Numero de convenio
 DM.VALOR$  = MID$(H$,1,24)					  ! Numero de convenio 
 DM.VALMCA$ = MID$(H$,25,9)				    ! Numero de la formula 
 DM.VALIPS$ = MID$(H$,34,9)					  ! Valor de la trx     
 DM.DATO$   = MID$(H$,43,9)					  ! Valor de la cuota moderadora
 DM.BUFFER$ = DM.CONVEN$+"    "+DM.VALMCA$+"   "+DM.VALIPS$+"  "+DM.DATO$   !
 Call IMP.LINEA
 GRAN.TOTAL = 0             					  								! Total de las formulas
 INDICAT1   = 0							  										! Total cuota moderadora
 INDICAT2   = 0							  										! Total Cobrar EPS
 INDICAT3   = 0							  										! Total descuentos
 While(1)
   If DM.CONVEN$ <> DM.VALOR$ Then Begin
     Call CAMBIO.CONVENIO
     DM.CONVEN$ = MID$(H$,1,24)					  ! Numero de convenio   
  EndIf
  
  !INDICAT1 = INDICAT1 + VAL(DM.VALMCA$)		  ! Total de formulas  
  !INDICAT2 = INDICAT2 + VAL(DM.VALIPS$)     ! Total de cobro a EPS
  !INDICAT3 = INDICAT3 + VAL(DM.DATO$)       ! Total de cobro COOPAGO
  
  READ #32;LINE H$
  DM.VALOR$  = MID$(H$,1,24)					  ! Numero de convenio 
  DM.VALMCA$ = MID$(H$,25,9)				    ! Numero de la formula 
  DM.VALIPS$ = MID$(H$,34,9)					  ! Valor de la trx     
  DM.DATO$   = MID$(H$,43,9)					  ! Valor de la cuota moderadora
  DM.BUFFER$ = DM.CONVEN$+"    "+DM.VALMCA$+"   "+DM.VALIPS$+"  "+DM.DATO$   !
  Call IMP.LINEA
 Wend

EOF.REPORTE:
 Call CAMBIO.CONVENIO
 DM.BUFFER$ = String$(38," ")+ 				 \!
              "-----------   ---------            --------------" !
 Call IMP.LINEA
 DM.DIA01$ = RIGHT$("        "+STR$(TOT.VTAS%),8)
 DM.DIA02$ = RIGHT$("        "+STR$(TOT.UNI%),8)
 DM.DIA03$ = RIGHT$("        "+STR$(CONTADOR%),8)
 DM.DIA04$ = "         "
 
 DM.BUFFER$ = "TOTAL GENERAL"+String$(28," ")+DM.DIA01$+\!
              "    "+DM.DIA02$+"  "+DM.DIA04$+"     "+DM.DIA03$
 Call IMP.LINEA
 DM.BUFFER$ = String$(30," ")
 Call IMP.LINEA
 PRINT LINEA$						  	  										! Impresion de linea
 PRINT CHR$(12)    					  	  										! Salto de Pagina
 CONSOLE
End Function
!--- Fin procedimiento impresion

Function PLANO
String TMP1$

If FIN$ = "0" Then Begin					     								! Crea archivo salida CCCCaammdd.IPS
  FECHA.CIERRE$ = UNPACK$(FECHA.CIER$)					     							!
  If MID$(FECHA.CIERRE$,1,2) = "99" Then        		          		       \! Manejo Y2K
   FECHA.CIERRE$ = "19"+FECHA.CIERRE$  Else     		    				   \!
   FECHA.CIERRE$ = "20"+FECHA.CIERRE$				     						!
  SALIDA$ = "ADX_UDT1:TMPORD.TXT"
  CREATE POSFILE SALIDA$ AS 32					     							!
  FIN$ = "1"						             !
EndIf								     !
!Call BUSQ.CONV(DM.VLRCON$)				             							! Retorna valor convenio

DM.VALOR$    = Right$("         "+Str$(Vlr.Venta%),9)
Vlr.Coopago$ = Right$("         "+Vlr.Coopago$,9)
Nro.Formula$ = Right$("         "+Nro.Formula$,9)
Nro.Convenio$ = Right$(String$(24," ")+Nro.Convenio$,24)


LEC$="C24 C9 C9 C9 C2" ! Formato salida 
WRITE FORM LEC$;#32;Nro.Convenio$,	  \! Codigo convenio
                    Nro.Formula$, 		\! formula
				            DM.VALOR$ ,				\! Cobro a la EPS
				            Vlr.Coopago$,			\! Valor coopago
				            FINR$			     		 ! Fin de registro
INP2$ = "0"							     					 !                            				    
End Function
!--- Fin de la generacion del plano

Function INI.VAR.PROG															! Inicializa Variables del Programa en GRAL
  DM.CAJA$=""
  DM.TRX$=""
  INP2$="0"
  CONTADOR% = 0				! Inicializo contador
  NRO.ERROR = 0
  BARRA% = 1
  TOTAL.REG = 0
  CLIENTE.FREC = 0
  GRAN.TOTAL = 0
  ACUMULADOR% = 0
  TRANS.AUTONOMO = 0
  TRANS.NORMAL = 0
  CNTREG% = 1
  TOT.DEPTOS = 0
  NETMSC = 0
  TOT.TERM = 0
  TERM.INI = 0
  NUM.SEP$ = ","
  PRIMERA.VEZ = 1
  FINR$=CHR$(13)+CHR$(10)
		     !  NUMEROS   DE  SESIONES
  TLOG = 25		:CONTROLFILE = 29	:INTERFAZ = 28
  LISTRAN  = 34		:OUTFIL = 36		:LOGINTERFAZ = 20
  SERDIAN = 27		:NUM.ALMACEN = 38
  !ITEM.REC    = 32
End Function

Function INICIALICE.VAR.TRANSACCION		! Para cada Transacci¢n Nueva
Q=1
CONTADOR% = 0				! Inicializo contador
NRO.StringS.FOUND = 0


NO.VENTA = 0




NRO.REG = 0      ! Control del header a cero 

End Function

Function APERTURA.ARCHIVOS.PRINC
  Call Msg.Err(05,"Abriendo... "+B$)
  wait ; 1800
  OPEN B$ AS TLOG 
  TOT.TAMANO = SIZE(B$)
  B$ = ""
End Function

Function LEA.REG.TRANS.SUM.LOG	    ! DEVUELVE EL ARCHIVO PREVIO  
String LLAVE$
!+++++++++++++++++++++++ Leemos cual transaction previo se procesa 
OPEN "EAMCSCF1" KEYED RECL 36 AS 37 NOWRITE NODEL	! Busca Eamtran Activo 
LLAVE$ = PACK$("9998")
READ FORM "C2 C8 C8 I4 C5 C9";#37  KEY LLAVE$; TERM$, \
       SLOGNAME$,OSLOGNAME$,CLOSEPNT,FECHA.CIER$,RESERVED$  
CLOSE 37
If DM.CODIGO$ = "1" Then B$ = SLOGNAME$
If DM.CODIGO$ = "2" Then Begin 
   If OSLOGNAME$="        "   Then Begin 
  	MEN$="No Existe EAMTRAN? Previo. Procesando el Actual ... "
    Call MSG.ERR(5,MEN$)
    WAIT;1800
	  B$ = SLOGNAME$ 
   EndIf Else B$ = OSLOGNAME$ 			! CARGA EAMTRAN PREVIO
EndIf
TOT.TAMANO = SIZE(B$)
End Function

Function PANTALLA.PRINCIPAL
Call INICIADM 				                    								! Inicializacion Variables Display Manager
AREA1% = 11: AREA2% = 12: AREA3%=13: AREA4%=14              					! Definicion area de trabajo archivo

FIN$="0"						    											! Variable control creacion archivo
ARCH2$="CONVENIO"					    										! Archivo de Convenios
ARCH3$="BNEFICIA"					    										! Archivo de Beneficiarios
ARCH4$="EAMITEMR"					    										! Maestro de articulos

Open ARCH3$ KEYED RECL  99 AS AREA2%                        					! Abre archivo de Beneficiarios
Open ARCH2$ KEYED RECL 106 AS AREA3%                        					! Abre archivo de Planes
Open ARCH4$ KEYED RECL  46 AS 4				    								! Abre maestro articulos
Call LEER.CABECERA				            									!
TERM$ = " "					            										! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					    									!
Call DM.ERR(RET.ERR%,INITDM$)				    								!
RET.ERR% = OPNDIS("ADX_UPGM:EPSPANTA.PNT")	            						! Apertura de la forma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
Call INICIO1 
      Call MSG.ERR(05,"Digite Origen de la informacion")
      RET.ERR% = NXTF(-20) 			           									! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          						! Captura dato en pantalla
      DM.CODIGO$ = INP$                                    						! Asigna valor capturado
      If (EndF = F1.AYUDA) Then Begin \
         Call HELP("Reporte Formulas Medicas ","EPSP0404.TXT") !
         Call INICIO1
      EndIf
      If (EndF = F3.SALIR) Then Call TERMINE.PROG                 				! Si presiona tecla F3
      If (EndF = ESC.KEY)  Then Call TERMINE.PROG                 				! Si presiona tecla ESC
      WHILE (MATCH(DM.CODIGO$,"123",1) = 0 OR DM.CODIGO$ = " ")
         Call MSG.ERR(05,"Digite Origen de la informacion")
         RET.ERR% = NXTF(-20) 			                  						! Primer campo
         Call DM.ERR(RET.ERR%,NXTF$)
         ATTR$ = SETF(PRM.ON$)                                
         INP$ = UPDF                                              				! Captura dato en pantalla
         DM.CODIGO$ = INP$                                        				! Asigna valor capturado
         If (EndF = F1.AYUDA) Then Begin \
            Call HELP("Reporte Formulas Medicas ","EPSP0404.TXT") 				!
            Call INICIO1
         EndIf
         If (EndF = F3.SALIR) Then Call TERMINE.PROG              				! Si presiona tecla F3
         If (EndF = ESC.KEY)  Then Call TERMINE.PROG              				! Si presiona tecla ESC
      WEnd
      If DM.CODIGO$ = "3" Then Begin \
         Call MSG.ERR(05,"Digite ruta y nombre del log de transacciones")
         DM.NOMPRO$ = GET.DATOS
         If (EndF = F3.SALIR) Then Call TERMINE.PROG               ! Si presiona tecla F3
         If (EndF = ESC.KEY)  Then Call TERMINE.PROG               ! Si presiona tecla ESC
         While DM.NOMPRO$ = " " 
            Call MSG.ERR(05,"Digite ruta y nombre del log de transacciones")
            RET.ERR% = NXTF(-2)
            Call DM.ERR(RET.ERR%,NXTF$)
            DM.NOMPRO$ = GET.DATOS
            If (EndF = F3.SALIR) Then Call TERMINE.PROG            ! Si presiona tecla F3
            If (EndF = ESC.KEY)  Then Call TERMINE.PROG            ! Si presiona tecla ESC
         Wend
      EndIf
  If DM.CODIGO$ = "1" Then Call LEA.REG.TRANS.SUM.LOG    ! DEVUELVE ARCHIVO PREVIO  
  If DM.CODIGO$ = "2" Then Call LEA.REG.TRANS.SUM.LOG    ! DEVUELVE ARCHIVO PREVIO  
  If DM.CODIGO$ = "3" Then B$ = Alltrim(DM.NOMPRO$)      ! Busqueda de log digitado
End Function

Function ENTRADA.LOG
  Call MSG.ERR(5,MEN$)
  WAIT;2000
End Function

Function CUENTE.StringS		! Esta funcion cuenta primero el #String y lo
Integer*2 POS1%,CUENTE.StringS	! compara con el #reportado por SMA
String CARACTER
POS1% = 1
NRO.StringS.FOUND = 0
CARACTER = CHR$(34)+CHR$(44)+CHR$(34)
WHILE (POS1% <> 0)
	POS1% = MATCH(CARACTER,INAREA$,POS1%+1)
	NRO.StringS.FOUND = NRO.StringS.FOUND + 1
WEnd
NRO.StringS.FOUND = NRO.StringS.FOUND - 1
If (NRO.StringS.FOUND < NRO.REG) Then Begin
	MEN$ = "FALTA String'S TERM : "+TERMINAL$+" TRX: "+NRO.TRANS$
	Call ENTRADA.LOG
EndIf
If (NRO.StringS.FOUND > NRO.REG) Then Begin
	!MEN$ = "SOBRAN String'S TERM : "+TERMINAL$+" TRX: "+NRO.TRANS$
	!Call ENTRADA.LOG
EndIf
If (NRO.StringS.FOUND = NRO.REG) Then CUENTE.StringS = 1	\ Retorna 1 si es
Else CUENTE.StringS = 0				! satisfactorio el resultado y 0
End Function						! si el resultado es fallido.


Function NEGATIVO(CADENA.NEG)	! CONVIERTE UN String A NEGATIVO
Integer*1 IL
String CADENA.NEG,NEGATIVO
IL=LEN(CADENA.NEG)
NEGATIVO="-"+RIGHT$(CADENA.NEG,IL-1)
End Function

Function BARRA.ESTADO
  Integer*1 N, TOT.PORC.LEIDO
  TOT.PORC.LEIDO = TOT.LEIDO * 100 / TOT.TAMANO
  N = TOT.PORC.LEIDO / 10
  BARRA$ = " 0% ¯"+String$(N,CHR$(219))+String$(10-N,CHR$(177))+"® "+STR$(TOT.PORC.LEIDO)+"%"
End Function

Function GRABE.IDENTIfICADOR

 String C$, DINNER.POS$, DINNER.NEG$
 PROCESO$ = " INSERTANDO IDENT "
 DUPLICADA = 0

TOTAL.REG = TOTAL.REG + 1
LOCATE 20,25
Call BARRA.ESTADO
TOTAL.REG = TOTAL.REG + 1
MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
Call MSG.ERR(5,MEN$)
End Function

Function INSERTE.ARTICULO

End Function

!--------------
!-------------- Programa Principal
!--------------

  ON ERROR GOTO IO.ARCHIVOS
  Call INI.VAR.PROG						    ! Inicializamos Variables
  Call PANTALLA.PRINCIPAL				    ! Captura de datos
  Call APERTURA.ARCHIVOS.PRINC				    ! Apertura de archivos

!********************** LECTURA SUCESIVA DE NUEVA TRANSACCION *******************

  NXTRCD:
  Call INICIALICE.VAR.TRANSACCION		! Inicializamos las Var de Trans.
  PROCESO$ = " PROCESO PRINCIPAL "
  READ #TLOG; LINE INAREA$                      ! Lectura log de transacciones
  If End #TLOG Then REPORTE.FINAL               ! Si encuentra final del log
  TOT.LEIDO = TOT.LEIDO+LEN(INAREA$)+2		! PARA CALCULAR BYTES LEIDOS DEL TLOG
  If LEN(INAREA$) < 12 Then Begin		! SI HAY ALGO EN REGISTRO
	  GOTO NXTRCD
  EndIf
  INAREA$ = INAREA$ + ","
  WHILE (Q < LEN(INAREA$))			! SI HAY ALGO EN REGISTRO
    P = MATCH (",",INAREA$,Q) 			! ENCONTRAR DELIMITADOR, COMA
    If (P-Q) < 3 Then \ 			! REVISAR FALTA DE String
    	Begin
	  P=0
	  !MEN$ ="FALTA String ID TRANSACCION"
	  !Call ENTRADA.LOG
	  Q=P+1 				! PREPARANDO POSICION PARA PROX. String
	  NO.VENTA = -1
	  GOTO AGAIN		   		! VUELVA A LEER OTRO REGISTRO
	EndIf
    B$ = MID$(INAREA$,Q+1,(P-Q)-2) 		! CAPTURA CADENA SIN COMILLAS.	MENOS 13 Y "
    B$ = B$+":" 				! SUMA UN SEMI-COLON
    Q = P + 1 					! PREPARA POSC. PARA NUEVO DATO
    A = VAL(UNPACK$(LEFT$(B$,1))) 		! DETERMINA TIPO DE String
    If A = 0 Then GOSUB S0:GOTO AGAIN		! SI ES CHECKOUT TRANSACCION
    If A = 99 Then GOSUB S99:GOTO AGAIN
    If (A < 0) OR (A > 21) Then GOTO AGAIN
    If (SW.ITEM = 1) AND (A <> 2) AND (A <> 3) Then \	! SI YA HUBO ITEM Y NO EXTENSION NI DESCUENTO
	If (TIPO.TRANS = 0) OR (TIPO.TRANS = 18) Then Begin
		CANT.PESO$ = "000000000" 
		Call INSERTE.ARTICULO
          	SW.ITEM = 0
       	EndIf
    If (A > 3) AND (SW.ITEM = 1) Then \		!SI PASO CANTIDAD Y DESCUENTO Y NO GRABO
	If (TIPO.TRANS = 0) OR (TIPO.TRANS = 18) Then Begin
		Call INSERTE.ARTICULO
          	SW.ITEM = 0
       	EndIf
    ON A GOSUB S1,S2,S3,S3,S5,S5,S7,S7,S9,S10, \
               S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21,S99
  AGAIN:
  If (A=0) AND (StringS.COMPLETOS=0) Then NRO.ERROR = NRO.ERROR + 1:GOTO NXTRCD
  If NO.VENTA Then GOTO NXTRCD			! Si no es una venta leemos otro registro
  WEnd
  If DM.CAJA$ <> TERMINAL$ OR DM.TRX$ <> NRO.TRANS$ Then Begin
     DM.CAJA$ = TERMINAL$			
     DM.TRX$  = NRO.TRANS$
     Call PLANO
  EndIf
GOTO NXTRCD					! VUELVA A LEER NUEVO REGISTRO 

S0:
  ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
  J = 3
  GOSUB GETUNPK				! SIGUE AL PROXIMO CAMPO
  TERMINAL$=RIGHT$(A$,4)		! CAPTURA NUMERO DE TERMINAL
  GOSUB GETUNPK				! SIGUE AL PROXIMO CAMPO Y ASI.......
  NRO.TRANS$=A$				! CAPTURA NUMERO DE TRANSACCION
  GOSUB GETUNPK
  FECHA$=LEFT$(A$,6)			! CAPTURA FECHA DE TRANSACCION
  If DM.CODIGO$ <> "2" Then FECHA.CIER$ = PACK$(FECHA$)
  HORA$ = RIGHT$(A$,4)+"00"		! CAPTURA HORA DE LA TRANSACCION
  GOSUB GETUNPK
  TIPO.TRANS=VAL(A$)			! CAPTURA EL TIPO DE LA TRANSACCION
  GOSUB GETUNPK
  NRO.REG=VAL(A$)  			! NUMERO DE StringS EN TODA LA TRANSACCION
  StringS.COMPLETOS = 0
  If (TIPO.TRANS <> 0) AND (TIPO.TRANS <> 18) AND (TIPO.TRANS <> 2) AND 	\
	(TIPO.TRANS <> 1) Then Begin 	! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...

		NO.VENTA = 1
  EndIf Else	\
	StringS.COMPLETOS = CUENTE.StringS	! FUNCION PARA CORROBORAR INTEGRIDAD DE StringS
  GOSUB GETUNPK
  COD.OPERA$=RIGHT$(String$(8,"0")+A$,8) 	! NUMERO DEL OPERADOR
  GOSUB GETUNPK
  			!  PASSWORD DEL OPERADOR
  GOSUB GETUNPK		! Valor de la transaccion
  GROSS.POSITIVO$ = A$
  GOSUB GETUNPK
  GROSS.NEGATIVO$ = A$
  
  Vlr.Venta% = Val(GROSS.POSITIVO$) - Val(GROSS.NEGATIVO$)
  
  GOSUB GETUNPK
  !	RING.TIME = VAL(A$)	! TIEMPO DURACION DE TRANSACCION
  GOSUB GETUNPK
				! TEndER TIME= ",VAL(A$) TIEMPO DE PAGO
  GOSUB GETUNPK
  !	SPECIAL.TIME  = VAL(A$)	! TIEMPO EN SING-OFF
  GOSUB GETUNPK
  !	INACTIVE.TIME = VAL(A$)	! TIEMPO INACTIVA
  GOSUB GETUNPK
  If StringS.COMPLETOS AND (TIPO.TRANS = 0 OR TIPO.TRANS = 18 OR \
	TIPO.TRANS = 2 OR TIPO.TRANS = 1) Then  Begin	! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
  !++++++++++++++++++++++++++++ CAMPOS LISTOS PARA GRABAR  ++++++++++++++
     Call GRABE.IDENTIfICADOR
     If DUPLICADA Then GOTO NXTRCD	! 
  EndIf
RETURN

S1:
  J = 3
 
RETURN

S2:
  J = 3

RETURN

S3:
  J = 3
 
RETURN

S5:
  J = 3
 
RETURN

S7:
  J = 3!TAX
RETURN

S9:
  J = 3
  
RETURN

S10:
  J = 3
RETURN

S11:
  J = 3					! DATA ENTRY
Return 

S12:
  J = 3!CHANGE PRICE
RETURN

S13:
  J = 3!LOAN-PICKUP
RETURN

S14:
  J = 3
RETURN

S15:
  J = 3
RETURN

S16:
 J=3
RETURN

S20:
  J = 3
RETURN

S21:
  J = 3
RETURN

S99:
  J=3

  GoSub GETUNPK
  If A$ = "20060508" Then Begin 								! Modulo de Convenios 

     GoSub GETUNPK															! Codigo del Convenio - Plan
     Nro.Convenio$ = A$
     GoSub GETUNPK															! Codigo del medico
     Nro.Medico$   = A$
     GoSub GETUNPK															! Codigo de la eps
     Nro.Eps$      = A$
     GoSub GETUNPK															! Codigo de la formula
     Nro.Formula$  = A$
     GoSub GETUNPK															! Codigo del beneficiario
     Nro.Beneficiario$ = A$
     GoSub GETUNPK															! Codigo del diagnostico
     Nro.Diagnostico$  = A$
     GoSub GETUNPK															! Codigo Categoria Estrato
     Nro.Estrato$  = A$ 
     GoSub GETUNPK                              ! Vlr del credito
     Vlr.Coopago$ = A$
     Vlr.Coopago$ = Str$( Vlr.Venta% - Val(Vlr.Coopago$) )
     Call Plano
  EndIf 
  
RETURN

!*********************************************************************
GETUNPK:
  K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  A$ = UNPACK$(MID$(B$,J,K-J)) ! UNPACK FIELD
  J=K+1 ! POINT TO BeginNING OF NEXT FIELD
RETURN
!*********************************************************************

GETUNPK2:
  K = MATCH(";",B$,J) ! SEARCH FOR FIELD SEPERATOR
  A$ = UNPACK$(MID$(B$,J,K-J)) ! UNPACK FIELD
  J=K+1 ! POINT TO BeginNING OF NEXT FIELD
RETURN
!*********************************************************************

REPORTE.FINAL:
	TOT.LEIDO = TOT.TAMANO
	Call BARRA.ESTADO
	TOTAL.REG = TOTAL.REG + 1
	MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
	Call MSG.ERR(5,MEN$)
        Call PLANO
        If FIN$="1" Then Begin ! Genero movimiento
           Call MSG.ERR(5,"Organizando Archivo para el reporte ...")
           CLOSE 32
           Call OSSHELL("SORT <ADX_UDT1:TMPORD.TXT> ADX_UDT1:TMPOK.TXT")   
           Call MSG.ERR(5,"Archivo Organizado ...")
           Call MSG.ERR(5,"ImprimiEndo, Espere Por Favor ...")
           Call REPORTE
        EndIf
        Close AREA2%: CLOSE AREA3%: CLOSE 4
        Call TERMINE.PROG
RETURN
!*********************************************************************
!

IO.ARCHIVOS:
  ERRORCOD$ = ERR
  P=0
If ERR = "SS" Then RESUME NXTRCD:
If ERR = "IH" Then RESUME 
If ERRF% = AREA1% AND                                \! Validacion si existe 
   (ERR = "OE" OR ERR = "FU") Then Begin              ! el archivo de control
   CREATE POSFILE ARCH1$ KEYED 3,,,1000 RECL 104     \! si no existe lo crea.
          AS AREA1% 			       ! 
   BAN.PRG$ = "1"
   RESUME                                             ! retorna ejecucion
EndIf                                                 ! del programa
If ERRF% = AREA2% AND                                \! Validacion si existe 
   (ERR = "OE" OR ERR = "FU") Then Begin              ! el archivo de control
   MEN$="Error: Archivo BNEFICIA No Existe ..."
   Call ENTRADA.LOG
   STOP
EndIf                                                 ! del programa
If ERRF% = AREA3% AND                                \! Validacion si existe 
   (ERR = "OE" OR ERR = "FU") Then Begin              ! el archivo de control
   MEN$="Error: Archivo CONVENIO No Existe ..."
   Call ENTRADA.LOG
   STOP
EndIf                                                 ! del programa
If ERRF% = 4 AND   	                             \! Validacion si existe 
   (ERR = "OE" OR ERR = "FU") Then Begin              ! el archivo de control
   MEN$="Error: en apertura del Archivo EAMITEMR ..."
   Call ENTRADA.LOG
   STOP
EndIf                                                 ! del programa

If ERRF% = AREA1% AND ERR = "EF" Then Begin          \! Si encuentra fin de 
   BAN.PRG$ = "1"				      ! ejecucion normal del
   RESUME					      !
EndIf				                      !
If ERRF% = AREA2% AND ERR = "EF" Then Begin          \! Si encuentra fin de 
   BAN.PRG$ = "1"				      ! ejecucion normal del
   RESUME					      !
EndIf				                      !
If ERRF% = AREA3% AND ERR = "EF" Then Begin          \! Si encuentra fin de 
   BAN.PRG$ = "1"				      ! ejecucion normal del
   RESUME					      !
EndIf				                      !
If ERRF% = 4      AND ERR = "EF" Then Begin          \! Si encuentra fin de 
   BAN.PRG$ = "1"				      ! ejecucion normal del
   RESUME					      !
EndIf				                      !
If ERRF% = 28     AND ERR = "OE" Then Begin          \! Si encuentra fin de 
   MEN$="Error: Archivo EAMOPERA No Existe ..."
   Call ENTRADA.LOG
   Call TERMINE.PROG 				      !
EndIf				                      !
If ERRF% = 28     AND ERR = "EF" Then Begin          \! Si encuentra fin de 
   BAN.PRG$ = "1"				      ! archivo en busqueda
   RESUME					      !
EndIf				                      !
If ERRF% = AREA4% AND ERR = "EF" Then Begin          \! Si encuentra fin de 
   BAN.PRG$ = "1"				      ! ejecucion normal del
   RESUME					      !
EndIf				                      !
If ERRF% = 19 AND ERR = "EF" OR ERR="OE" Then Begin  \! Valida la lectura
   BAN.PRG$ = "1"				       ! del archivo de 
   RESUME					       ! help del aplicativo
EndIf						       !

If ERRF% = 23 AND ERR="OE" Then Begin                 \! Valida la lectura
   MEN$="Error: Archivo ADX_IDT1:DESCUENT.DAT No Existe ..."
   Call ENTRADA.LOG
   Call TERMINE.PROG 				      !
EndIf						      !

If ERRF% = 37 AND                                    \! Validacion si existe 
   (ERR = "OE" OR ERR = "FU") Then Begin              ! el archivo EAMCSCF1
   MEN$="Error: Archivo EAMCSCF1 No Existe ..."       !
   Call ENTRADA.LOG
   Call TERMINE.PROG 				      !
EndIf
If ERRF% = TLOG AND (ERR = "OE" OR ERR = "FU") Then \
 Begin
    MEN$="Error: No Se Logr¢ Abrir Log de Transacciones "
    Call MSG.ERR(5,MEN$)
    WAIT;3000
    Call TERMINE.PROG
 EndIf
If ERRF% = TLOG   AND ERR = "EF" Then Begin          \! Si encuentra fin de 
     MEN$ = "No se han registrado ventas para este per¡odo ..."
     Call ENTRADA.LOG
     WAIT;3000
     Call TERMINE.PROG
EndIf				                      !
Call TRADUCE.ERROR
MEN$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
Call ENTRADA.LOG
STOP

!--- Fin del programa EPSP0406.BAS
