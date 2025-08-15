!                (GRCREBAT.BAS)      (Ver 1.0)
!
! Este programa sirve para RENOMBRAR los archivos de las Ventas
! del Per¡odo ANTERIOR :
!
!        Los Archivos son los Siguientes :
!
!             EAMTRANx.DAT  EAMTRANx.IDX  EAMTRANx.MDX 
!             EAMIMOVO.DAT  EAMTPROO.DAT  EAMTLISP.DAT
!             EAMEXCPO.DAT  EAMITEMR.DAT  EAMOPERA.DAT
!             EAMOPTNA.DAT
!
! La entrada  es  cualquier  archivo Transaction Summary Log
! A, B, o C .DAT en C:\ADX_IDT4.
!
! *********************** CFRENAM.BAS ***************************
! Mod 12Nov2009
! Se modifica el modulo para que la fecha de movimiento la tome
! del Log de transacciones, desarollado por Grupo Retail - OVS
!-----------------------------------------------------------------
! Mod 19Oct2012
! Se modifica la generacion de copias de TLOG y journal electronicos
! para generarse con los siguientes formatos:
! TLOG
! TLaammdd.ccc donde aammdd fecha del movimiento ccc centro de costo
!
! Journal Electronico
! JEaammdd.ccc donde aammdd fecha del movimiento ccc centro de costo
!
! Desarrollador por Grupo Retail Ltda - OVS
!------------------------------------------------------------------

 %ENVIRON C
  STRING GLOBAL VALOR$,COJ$,COJ1$,          \!
               RESER, CAD$,CAD1$,           \!
               MESES(1),CAD2$, AA$,         \!
               ALMAS(1),COJ2$,              \!
               FORMAT,  COJ3$,              \!
               FORMATO, SORT$, SORT2$,      \!
               DD1$, DD2$, DD3$, DD4$,DD5$, \!
               DD6$,ftp$,ftp2$,ftp1$,ftp3$, \!
	             COPI$, ARCHIV$, BORRAOLD$,BORSIM$,\
	             MES3$, COP1$, COP2$, COP3$, COP4$ 
  INTEGER*1                                 \
           A%,       B%,          C%,       \
           D%,       E%,          F%,       \
           G%,       H%,          I%,       \
           J%,       K%,          L%
  INTEGER*2                                 \
           MES,      MESPRO


%INCLUDE ADX_UPGM:BASROUT.J86			    ! Rutinas aplicacion

! -------------  Rutina para crear compaginar Hora y Fecha --------------

  SUB CREAFECHA
      FRA.TIM$ = TIME$                              ! Toma Hora Sistema
      FRA.H$   = LEFT$(FRA.TIM$,2)                  ! Arma horas
      FRA.M$   = MID$(FRA.TIM$,3,2)                 ! Arma minutos
      FRA.S$   = RIGHT$(FRA.TIM$,2)                 ! Arma segundos
      FRA.FEC$ = DATE$                              ! Toma Fecha Sistema
      FRA.AA$  = LEFT$(FRA.FEC$,2)                  ! Arma ano
      FRA.MM$  = MID$(FRA.FEC$,3,2)                 ! Arma mes
      FRA.DD$  = RIGHT$(FRA.FEC$,2)                 ! Arma dia
      MES      = VAL(FRA.MM$)                       ! Toma Mes
      IF FRA.AA$ > "94" THEN SIGLO$ = " de 19"      ! Si es a£n Siglo XX
      IF FRA.AA$ < "95" THEN SIGLO$ = " del 20"     ! Si es Siglo XXI
      FHOY$    = MESES(MES)+" "+STR$(VAL(FRA.DD$)) \! Arma Fecha Hoy
                 + SIGLO$ + FRA.AA$                 !
      FRA.LIN$ =FRA.H$+":"+FRA.M$+":"+FRA.S$     + \! Arma la Linea
               "    de "+FHOY$                     ! con Hora y Fecha
      FRA.MD$  = RIGHT$(FRA.FEC$,4)                 ! Salva Mes y Dia 
  END SUB
!****************************************************************
! ******               INICIO DEL PROGRAMA              ******
!****************************************************************

   DIM MESES(12)
   FECHA$=DATE$
   MESPRO=VAL(MID$(FECHA$,3,2))
   DIAPRO$=RIGHT$(FECHA$,2)
   ANOPRO$=LEFT$(FECHA$,2)
!----------------------   Llena arreglo de meses   ---------------------------
   MESES(1) = "ENERO"     :  MESES(2) = "FEBRERO"    :  MESES(3)  = "MARZO"
   MESES(4) = "ABRIL"     :  MESES(5) = "MAYO"       :  MESES(6)  = "JUNIO"
   MESES(7) = "JULIO"     :  MESES(8) = "AGOSTO"     :  MESES(9)  = "SEPTIEMBRE"
   MESES(10)= "OCTUBRE"   :  MESES(11)= "NOVIEMBRE"  :  MESES(12) = "DICIEMBRE"
!----------------------------------------------------------------------------

EMPEZAR:
DATA$ = COMMAND$
If UCASE$(DATA$) = "VERSION" Then BEGIN 
   Print "Programa Generacion Copias Seguridad Comfandi Manual Comp.31-Oct-2014"
   Stop 
ENDIF 
ON ERROR GOTO ERRTN
FINR$ = CHR$(13)+CHR$(10)
CALL CREAFECHA
FEC1$ = FRA.LIN$
If END #25 THEN FINOK
CONSOLE
!Call OSSHELL("ADX_UPGM:GRCALFEC.286")

CLEARS
PRINT "  GRCREBAT.286 -- Compilacion 31/Octubre/2014"
PRINT 
PRINT "             PROGRAMA QUE PREPARA LOS ARCHIVOS DE COMANDO PARA"
PRINT "                 EJECUCION DE LAS SIGUIENTES TAREAS DIARIAS "
PRINT "  "
PRINT "   C:\COMFANDI\COPIABU.BAT   Elaborar Copias de Seguridad en ADX_UDT1:"
PRINT "   C:\COMFANDI\COPIADK.BAT   Tomar sobre Diskette, Copias de Soporte "
PRINT "   C:\COMFANDI\COPIAIN.BAT   Recoger Inventario de Ventas para SIGED "
PRINT "   C:\COMFANDI\COPIACR.BAT   Recoger Medios de Pago para CR y COBRANZAS"
PRINT "   C:\COMFANDI\COPIAXL.BAT   Archivo Diario de Var/Precio y Reintegros"
PRINT "   C:\COMFANDI\COPICLI.BAT   Archivo Diario de VECINO FIEL            "
PRINT "  "
PRINT "                A L E R T A :  Fecha de Hoy : ";FHOY$
PRINT
GETNAME:
Input "    Indique Fecha para el proceso del cierre (AAMMDD) " ; RESP$

!Open "TF:FECDIA" AS 77                              ! Fecha procesamiento de trx
!Read #77; RESP$					                            !
!Close 77					                                  !

AA$   = RESP$                                       ! Fecha movimiento AAMMDD
RESP$ = RIGHT$(RESP$,4)                             ! Asigna fecha
DIA$  = RIGHT$(RESP$,2)
MES3$ = LEFT$(RESP$,2)

C$ = "COPY ADX_IDT4:EAMEXCPO.DAT ADX_UDT1:EAMX"+MD$+".DAT"
PRINT                                               !
OPEN "C:\ADXALMA.DAT" DIRECT RECL 80 AS 29          ! Archivo Control Almacen
DIM ALMAS$(100)                                     ! Para Salvar ADXALMA.DAT
FOR K% = 1 TO 100                                   ! 
    READ FORM "C80";#29,K%;AL$                      ! Lee Control Almac
    ALMAS$(K%) = AL$                                ! y lo guarda
NEXT K%                                             !
AL$ = ALMAS$(1)                                     ! Lo Recupera
POS%  = MATCH("!",AL$,1)                            ! Verif Cant Digitos del Reg
IF POS%  < 12 THEN BEGIN                            ! Detecta Dise¤o Anterior
   AL$ = LEFT$(AL$,8)+RESP$+RIGHT$(AL$,LEN(AL$)-8)  ! Inserta Fecha Digitada
ENDIF                                               !
NUAL$ = STR$(VAL(LEFT$(AL$, 4)))                    ! Guarda Num Almacen
ANON$ = MID$(AL$,5,4)                               ! Guarda Ano Actual
AM$   = MID$(AL$,7,4)                               ! Guarda Ano/Mes Actual
AYER$ = MID$(AL$,5,8)                               ! A¤o Mes y Dia de Proceso
MES   = VAL(MID$(AL$,9,2))                          ! Guarda Indice Mes
FAYER$= MID$(AL$,11,2) + " de " +                  \! Format Fecha Ayer
        MESES(MES) + " del A¤o " + MID$(AL$,5,4)    !
NOAL$ = MID$(AL$,13,38)                             ! Guarda Nombre
ALM$  = NUAL$                                       ! 
RESP$ = MID$(AL$,5,4) + RESP$                       ! Agrega Ano Proceso
IF FRA.AA$ < "95" THEN BEGIN                        ! Si es Siglo XXI
   FACTU$ = "20" + DATE$                            ! A¤o Empieza por 20
ENDIF ELSE BEGIN                                    ! de lo contrario
   FACTU$ = "19" + DATE$                            ! A¤o Empieza por 19
ENDIF                                               !
IF LEN(RESP$)      <>  8  OR                       \! Si no son 8 Dig 
   MID$(RESP$,5,2) < "01" OR                       \! o mes < 01
   MID$(RESP$,5,2) > "12" OR                       \! o mes > 12
   RIGHT$(RESP$,2) < "01" OR                       \! o dia < 01
   RIGHT$(RESP$,2) > "31" THEN BEGIN                ! o dia > 31
   PRINT " "                                        ! Avanza Linea
   PRINT "  La fecha debe tener CUATRO D¡gitos....."! Mensaje de Error
   PRINT "  en el Formato MMDD (Mes y D¡a).        "! de Formato
   INPUT "       Corrija el ERROR..." ; RESP$       ! Espera teclado
   CLOSE 29                                         !
   GOTO EMPEZAR                                     ! y vuelve atras 
ENDIF                                               !
IF LEFT$(FACTU$,4) > MID$(AL$,5,4) AND             \! Si Cambia de A¤o
   RIGHT$(RESP$,4) = "0101" THEN BEGIN              ! y es 1 ¢ 2 de Enero
   RESP$ = LEFT$(FACTU$,4) + "0101"                 ! Inicia Nuevo A¤o
   ANON$ = LEFT$(FACTU$,4)                          ! Guarda Ano Nuevo
ENDIF ELSE BEGIN                                    !
   IF LEFT$(FACTU$,4) > MID$(AL$,5,4) AND          \! Si Cambia de A¤o
      RIGHT$(RESP$,4) = "0102" THEN BEGIN           ! y es 1 ¢ 2 de Enero
      RESP$ = LEFT$(FACTU$,4) + "0102"              ! Inicia Nuevo A¤o
      ANON$ = LEFT$(FACTU$,4)                       ! Guarda Ano Nuevo
   ENDIF
ENDIF

PRINT "    Procesando Movimiento De Cierre Para El Periodo   :"
PRINT "  "                                          !
MES = VAL(MID$(RESP$,5,2))                          !
MES$ = MID$(RESP$,5,2)                              ! 
MES1$ = MID$(MES$,2,1)
IF MES$ = "10" THEN MES1$ = "A"
IF MES$ = "11" THEN MES1$ = "B"
IF MES$ = "12" THEN MES1$ = "C"

PRINT TAB(18);VAL(RIGHT$(RESP$,2));TAB(23);        \!
      "de ";TAB(27);MESES(MES);" del A¤o ";        \!
       LEFT$(RESP$,4)
PRINT "  "                                          !
WAIT ; 2500

!Input "       Confirme  SI/NO :  ";FAYER$           !

FAYER$ = "SI"
IF FAYER$ = "si" OR FAYER$ = "SI" THEN GOTO CONTI   !
CLOSE 29                                            !    
GOTO EMPEZAR                                        !
CONTI:
INVEN$ = ALM$ + INVEN$                              ! Adiciona No Almac
PRINT
PRINT "       Proceso para el Almac‚n No. " ; ALM$;"  ";NOAL$
PRINT "               Inicia a las: ";FEC1$         ! Indica comienzo
PRINT                                               ! Avanza Linea
PRINT "    Por Favor Esperar..."                    !
CREATE "C:\COMFANDI\COPIABU.BAT" AS 26
CREATE "C:\COMFANDI\COPIADK.BAT" AS 27
CREATE "C:\COMFANDI\COPIAIN.BAT" AS 28
CREATE "C:\COMFANDI\COPIACH.BAT" AS 34
CREATE "C:\COMFANDI\COPIAXL.BAT" AS 31
CREATE "C:\COMFANDI\COPICLI.BAT" AS 35
CREATE "C:\COMFANDI\COPIACR.BAT" AS 30
CREATE "C:\COMFANDI\IMPRIMA.BAT" AS 33
CREATE "C:\COMFANDI\IMPSUBD.BAT" AS 37
CREATE "C:\COMFANDI\COPJOUR.BAT" AS 36
create "c:\comfandi\transfe" as 38 
CREATE "C:\COMFANDI\COPLOG.BAT" AS 39
TERMS = 25
OPEN "EAMTERMS" KEYED RECL 72 AS TERMS NOWRITE NODEL
KEY$ = PACK$("9999")
READ FORM "C2 C8 C62";#TERMS KEY KEY$;KEY$,TRAN$,RESTO$
CLOSE TERMS
IF TRAN$ = "EAMTRANA" THEN         \
   TRAN$ = "EAMTRANC"              \
ELSE                               \
IF TRAN$ = "EAMTRANB" THEN         \
   TRAN$ = "EAMTRANA"              \
ELSE                               \
   TRAN$ = "EAMTRANB"              
MD$ = RIGHT$(RESP$,4)

ftp$ = "mdtfgc"
ftp1$ = "quit"
ftp2$ = "cd adx_udt1"
ftp3$ = "mget"+" "+"C:\?"+ALM$+MD$+".DAT"
ftp4$ = "mget"+" "+"C:\ADX_UDT1\?"+ALM$+MD$+".DAT"
!A$  = "COPY ADX_IDT4:"+TRAN$+".* ADX_UDT1:EAMT"+MD$+".*"

A$  = "COPY ADX_IDT4:"+TRAN$+".DAT ADX_UDT1:TL"+AA$+"."+NUAL$   ! Cambio respaldo TLOG formato TLaammdd.ccc 

B$  = "ADXCSK0L 2 3 N Y ADX_IDT4:EAMIMOVO.DAT ADX_UDT1:EAMI"+MD$+".DAT 1"
F$  = "ADXCSK0L 2 3 N Y ADX_IDT1:EAMITEMR.DAT ADX_UDT1:EAMITEMR.COP 1"
G$  = "COPY ADX_IDT1:EAMOPERA.DAT ADX_UDT1:EAMOPERA.COP"
H$  = "COPY ADX_IDT1:EAMOPTNA.DAT ADX_UDT1:EAMIPTNA.COP"
HH$ = "DIR ADX_UDT1:EAM?"+MD$+".DAT"
C$ = "COPY ADX_IDT4:EAMEXCPO.DAT ADX_UDT1:EAMX"+MD$+".DAT"
I$ = "ADX_UDT1:"
CAD$ = "CD \"
CAD1$ ="CD EJ"
CAD2$ ="CD J"
!D$ = "ADXCS30L 1"
CD$ = "ADXNSXZL C EAMB"+MD$+".DAT EAM?"+MD$+".DAT"

!COJ$ = "ADXNSXZL C ADX_UDT1:AUD"+MD$+".DAT  J"+MES1$+RIGHT$(MD$,2)+"*"
!COPI$="copy c:/adx_udt1/aud"+MD$+".dat c:/adx_udt1/aud"+MD$+".cop"
!ARCHIV$="c:/adx_udt1/aud"+MD$  
!BORRAOLD$="del c:/adx_udt1/aud"+MD$+".cop"

COJ$      = "ADXNSXZL C ADX_UDT1:JE"+AA$+"."+NUAL$+" C:\EJ\J\J"+MES1$+RIGHT$(MD$,2)+"*.*"   ! Se copian journals formato JEaammdd.dat

COPI$     = "copy c:/adx_udt1/JE"+AA$+"."+NUAL$+" c:/adx_udt1/JE"+AA$+".cop"
ARCHIV$   = "c:/adx_udt1/JE"+AA$  
BORRAOLD$ = "del c:/adx_udt1/JE"+AA$+".cop"
borra$    = "del adx_udt1:JE"+AA$+"."+NUAL$

D$ = "COPY EAMB"+MD$+".DAT A:"
!COJ1$ = "COPY ADX_UDT1:AUD"+MD$+".DAT A:"
COJ1$ = " "
COJ2$ = "CD COMFANDI"
COJ3$ = "CD \"
J$  = "COPY C:\ADX_UDT1\I"+ALM$+MD$+".DAT A:"
JJ$ = "COPY C:\ADX_UDT1\C"+ALM$+MD$+".DAT A:"
FF$ = "COPY C:\ADX_UDT1\R"+ALM$+MD$+".DAT A:"
DD$ = "TYPE A:\ADX_UDT1\I"+ALM$+MD$+".DAT"
DD1$ = "COPY C:\ADX_UDT1\T"+ALM$+MD$+".DAT A:"
DD2$ = "COPY C:\ADX_UDT1\F"+ALM$+MD$+".DAT A:"
DD3$ = "COPY C:\ADX_UDT1\G"+ALM$+MD$+".DAT A:"
DD4$ = "COPY C:\ADX_UDT1\E"+ALM$+MD$+".DAT A:"
DD5$ = "COPY C:\ADX_UDT1\A"+ALM$+MD$+".DAT A:"
DD6$ = "COPY C:\ADX_UDT1\P"+ALM$+MD$+".DAT A:"
IMPRI$ = "PRINT C:\ADX_UDT1\Z"+ALM$+MD$+".DAT"
SORT$  = "SORT -+57 < ADX_UDT1:P"+ALM$+MD$+".DAT > ADX_UDT1:IVACRED2.DAT"
!SORT2$ = "SORT -+57 < ADX_UDT1:IVACRE1.DAT > ADX_UDT1:IVACRED2.DAT"
IMPRI1$ = "COPY C:\ADX_UDT1\B"+ALM$+MD$+".DAT ADX_UDT1:BONOS.DAT"
K$ = "IF NOT EXIST C:\I"+ALM$+MD$+".DAT GOTO ERROR"
L$ =  "ECHO          "+"C:\I"+ALM$+MD$+".DAT"
KK$ = "ECHO          "+"C:\C"+ALM$+MD$+".DAT"
MM$ = "ECHO          "+"C:\R"+ALM$+MD$+".DAT"
LL$= "SORT -+48 < C:\V"+ALM$+MD$+".DAT > C:\W"+ALM$+MD$+".DAT"

M$ = "COPY C:\ADX_UDT1\H"+ALM$+MD$+".DAT A:"
M1$ = "COPY C:\ADX_UDT1\B"+ALM$+MD$+".DAT A:"
N1$ = "IF NOT EXIST C:\ADX_UDT1\T"+ALM$+MD$+".DAT GOTO ERROR"
O$ = "ECHO          "+"C:\ADX_UDT1:P"+ALM$+MD$+".DAT"
OO1$ = "ECHO          "+"C:\ADX_UDT1:H"+ALM$+MD$+".DAT"
O1$ = "ECHO     ADX_UDT1"+"C:\T"+ALM$+MD$+".DAT"
OO$= "SORT -+36 < C:\X"+ALM$+AM$+".DAT >> C:\Y"+ALM$+AM$+".DAT"

WRITE FORM "C70 C2";#26;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#26;"CLS     ",FINR$
WRITE FORM "C70 C2";#26;"ECHO   ",FINR$
WRITE FORM "C70 C2";#26;"ECHO   ",FINR$
WRITE FORM "C70 C2";#26;"ECHO   ",FINR$
WRITE FORM "C70 C2";#26;"ECHO               Proceso de Copias de Seguridad en ADX_UDT1.",FINR$
WRITE FORM "C70 C2";#26;"ECHO   ",FINR$
WRITE FORM "C70 C2";#26;"ECHO   ",FINR$
WRITE FORM "C70 C2";#26;"ECHO                       Espere un Momento.. Por Favor..!",FINR$
WRITE FORM "C70 C2";#26;A$,FINR$
WRITE FORM "C70 C2";#26;B$,FINR$
!WRITE FORM "C70 C2";#26;F$,FINR$
!WRITE FORM "C70 C2";#26;G$,FINR$
!WRITE FORM "C70 C2";#26;H$,FINR$
WRITE FORM "C70 C2";#26;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#26;"CLS     ",FINR$
WRITE FORM "C70 C2";#26;"ECHO   ",FINR$ 
IF DIA$ >= "01" AND DIA$<= "03" THEN BEGIN
	IF DIA$="01" THEN BEGIN 
		BORSIM$ = "DEL ADX_UDT1:RSIM"+MES3$+"*.*"            ! BORRAR SIEMPRE COMIENZO MES 
		WRITE FORM "C70 C2";#26;BORSIM$,FINR$  
	ENDIF 
	IF DIA$="02" THEN BEGIN 
		COP1$ = "COPY C:/ADX_UDT1/RSIM"+MES$+"01.* C:/ADX_UDT1/*.COP"
		BORSIM$ = "DEL ADX_UDT1:RSIM"+MES3$+"*."+NUAL$            ! BORRAR SIEMPRE COMIENZO MES 
		COP3$ = "COPY C:/ADX_UDT1/RSIM"+MES$+"01.COP C:/ADX_UDT1/*."+NUAL$ 
		WRITE FORM "C70 C2";#26;COP1$,FINR$  
		WRITE FORM "C70 C2";#26;BORSIM$,FINR$  
		WRITE FORM "C70 C2";#26;COP3$,FINR$   
	ENDIF 
	IF DIA$="03" THEN BEGIN 
		COP1$ = "COPY C:/ADX_UDT1/RSIM"+MES$+"01.* C:/ADX_UDT1/*.COP"
		COP2$ = "COPY C:/ADX_UDT1/RSIM"+MES$+"02.* C:/ADX_UDT1/*.COP"
		BORSIM$ = "DEL ADX_UDT1:RSIM"+MES3$+"*."+NUAL$            ! BORRAR SIEMPRE COMIENZO MES 
		COP3$ = "COPY C:/ADX_UDT1/RSIM"+MES$+"01.COP C:/ADX_UDT1/*."+NUAL$ 
		COP4$ = "COPY C:/ADX_UDT1/RSIM"+MES$+"02.COP C:/ADX_UDT1/*."+NUAL$ 
		WRITE FORM "C70 C2";#26;COP1$,FINR$  
		WRITE FORM "C70 C2";#26;COP2$,FINR$  
		WRITE FORM "C70 C2";#26;BORSIM$,FINR$  
		WRITE FORM "C70 C2";#26;COP3$,FINR$  
		WRITE FORM "C70 C2";#26;COP4$,FINR$   
	ENDIF 
ENDIF ELSE BEGIN
BORSIM$ = ""
ENDIF 
WRITE FORM "C70 C2";#26;"ECHO   ",FINR$
WRITE FORM "C70 C2";#26;"ECHO   ",FINR$
WRITE FORM "C70 C2";#26;"ECHO   ",FINR$
WRITE FORM "C70 C2";#26;"ECHO               BACKUP en ADX_UDT1: Termina OK..!",FINR$
WRITE FORM "C70 C2";#26;"ECHO   ",FINR$
WRITE FORM "C70 C2";#26;"ECHO               No olvide elaborar las Copias    ",FINR$
WRITE FORM "C70 C2";#26;"ECHO               Respectivas en DISKETTE.         ",FINR$
!WRITE FORM "C70 C2";#26;HH$,FINR$
!WRITE FORM "C70 C2";#26;C$,FINR$
!WRITE FORM "C70 C2";#26;BORSIM$,FINR$

WRITE FORM "C70 C2";#27;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#27;"CLS     ",FINR$
WRITE FORM "C70 C2";#27;"ECHO   ",FINR$
WRITE FORM "C70 C2";#27;"ECHO   ",FINR$
WRITE FORM "C70 C2";#27;"ECHO   ",FINR$
WRITE FORM "C70 C2";#27;"ECHO               Proceso de Copia en Diskette del LOG Diario ..!",FINR$
WRITE FORM "C70 C2";#27;"ECHO   ",FINR$
WRITE FORM "C70 C2";#27;"ECHO   ",FINR$
WRITE FORM "C70 C2";#27;"ECHO                       Espere un Momento.. Por Favor..!",FINR$
WRITE FORM "C70 C2";#27;"ECHO ON",FINR$
WRITE FORM "C70 C2";#27;I$,FINR$
WRITE FORM "C70 C2";#27;CD$,FINR$
WRITE FORM "C70 C2";#27;D$,FINR$
WRITE FORM "C70 C2";#27;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#27;"CLS     ",FINR$
WRITE FORM "C70 C2";#27;"ECHO   ",FINR$
WRITE FORM "C70 C2";#27;"ECHO   ",FINR$
WRITE FORM "C70 C2";#27;"ECHO   ",FINR$
WRITE FORM "C70 C2";#27;"ECHO         BACKUP en DISKETTE Termina OK..!",FINR$
WRITE FORM "C70 C2";#27;"PAUSE",FINR$
WRITE FORM "C70 C2";#27;"C:",FINR$
WRITE FORM "C70 C2";#27;"BATCH DIRDKT.BAT",FINR$
WRITE FORM "C70 C2";#27;"AYUDA",FINR$


WRITE FORM "C70 C2";#36;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#36;"CLS     ",FINR$
WRITE FORM "C70 C2";#36;"ECHO   ",FINR$
WRITE FORM "C70 C2";#36;"ECHO   ",FINR$
WRITE FORM "C70 C2";#36;"ECHO   ",FINR$
WRITE FORM "C70 C2";#36;"ECHO               Proceso de Compresion de  ROLLO AUDITORIA..!",FINR$
WRITE FORM "C70 C2";#36;"ECHO   ",FINR$
WRITE FORM "C70 C2";#36;"ECHO   ",FINR$
WRITE FORM "C70 C2";#36;"ECHO                       Espere un Momento.. Por Favor..!",FINR$
WRITE FORM "C70 C2";#36;"ECHO ON",FINR$
WRITE FORM "C70 C2";#36;CAD$,FINR$
WRITE FORM "C70 C2";#36;CAD1$,FINR$
WRITE FORM "C70 C2";#36;CAD2$,FINR$ 
WRITE FORM "C70 C2";#36;"IF EXIST "+ARCHIV$+".cop GOTO COPIAR1",FINR$
WRITE FORM "C70 C2";#36;"      ",FINR$
WRITE FORM "C70 C2";#36;":COPIAR1",FINR$
WRITE FORM "C70 C2";#36;"del c:/adx_udt1/JE"+AA$+".COP",FINR$  
WRITE FORM "C70 C2";#36;"      ",FINR$ 
WRITE FORM "C70 C2";#36;"IF EXIST "+ARCHIV$+"."+NUAL$+" GOTO COPIAR",FINR$ 
WRITE FORM "C70 C2";#36;"      ",FINR$
WRITE FORM "C70 C2";#36;":COPIAR",FINR$ 
WRITE FORM "C70 C2";#36;COPI$,FINR$  
WRITE FORM "C70 C2";#36;BORRA$,FINR$
WRITE FORM "C70 C2";#36;COJ$,FINR$
WRITE FORM "C70 C2";#36;COJ1$,FINR$
WRITE FORM "C70 C2";#36;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#36;"CLS     ",FINR$
WRITE FORM "C70 C2";#36;"ECHO   ",FINR$
WRITE FORM "C70 C2";#36;"ECHO   ",FINR$
WRITE FORM "C70 C2";#36;"ECHO   ",FINR$
WRITE FORM "C70 C2";#36;"ECHO         Compresion de ROLLO  O.K ..!",FINR$
!WRITE FORM "C70 C2";#36;"C:",FINR$
!WRITE FORM "C70 C2";#36;COJ3$,FINR$
!WRITE FORM "C70 C2";#36;COJ2$,FINR$
!WRITE FORM "C70 C2";#36;"AYUDA",FINR$

WRITE FORM "C70 C2";#28;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#28;"CLS     ",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO               Proceso de Copia de Inventarios..!",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO                       Espere un Momento.. Por Favor..!",FINR$
WRITE FORM "C70 C2";#28;"ECHO ON",FINR$
WRITE FORM "C70 C2";#28;J$,FINR$
WRITE FORM "C70 C2";#28;JJ$,FINR$
WRITE FORM "C70 C2";#28;FF$,FINR$
WRITE FORM "C70 C2";#28;DD2$,FINR$
WRITE FORM "C70 C2";#28;DD3$,FINR$
WRITE FORM "C70 C2";#28;DD4$,FINR$
WRITE FORM "C70 C2";#28;DD5$,FINR$
WRITE FORM "C70 C2";#28;DD6$,FINR$
WRITE FORM "C70 C2";#28;DD$,FINR$
WRITE FORM "C70 C2";#28;"PAUSE",FINR$
WRITE FORM "C70 C2";#28;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#28;"CLS     ",FINR$
WRITE FORM "C70 C2";#28;K$,FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO               COPIA en DISKETTE Termina OK..!",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO               Enviar el DISKETTE para Actualizar",FINR$
WRITE FORM "C70 C2";#28;"ECHO               Saldos de Inventarios en SIGED.",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO               Clasifica Variaciones de Precio..!",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO                       Espere un Momento.. Por Favor..!",FINR$
WRITE FORM "C70 C2";#28;LL$,FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO               Clasificaci¢n Ventas Termina OK..!",FINR$
WRITE FORM "C70 C2";#28;"PAUSE",FINR$
WRITE FORM "C70 C2";#28;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#28;"BATCH DIRDKT.BAT",FINR$
WRITE FORM "C70 C2";#28;"GOTO SALIR",FINR$
WRITE FORM "C70 C2";#28;":ERROR",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO     No EXISTE el Archivo especificado como:",FINR$
WRITE FORM "C70 C2";#28;L$, FINR$
WRITE FORM "C70 C2";#28;KK$,FINR$
WRITE FORM "C70 C2";#28;MM$,FINR$
WRITE FORM "C70 C2";#28;"ECHO   ",FINR$
WRITE FORM "C70 C2";#28;"ECHO           Ejecute ANTES la Opc=1 del Men£ de",FINR$
WRITE FORM "C70 C2";#28;"ECHO           Procesos Administrativos",FINR$
WRITE FORM "C70 C2";#28;"PAUSE",FINR$
WRITE FORM "C70 C2";#28;":SALIR",FINR$
WRITE FORM "C70 C2";#28;"AYUDA",FINR$

WRITE FORM "C70 C2";#34;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#34;"CLS     ",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO               Proceso de Separacion de los CHEQUES ..!",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO                       Espere un Momento.. Por Favor..!",FINR$
WRITE FORM "C70 C2";#34;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#34;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#34;"CLS     ",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO               Se Inicia la COPIA en Diskette de los CHEQUES ..!",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO                       Espere un Momento.. Por Favor..!",FINR$
WRITE FORM "C70 C2";#34;"ECHO ON",FINR$
WRITE FORM "C70 C2";#34;M$,FINR$
WRITE FORM "C70 C2";#34;M1$,FINR$
WRITE FORM "C70 C2";#34;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#34;"CLS     ",FINR$
WRITE FORM "C70 C2";#34;N$,FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO               COPIA en DISKETTE Termina OK..!",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO               Enviar el DISKETTE para Actualizar",FINR$
WRITE FORM "C70 C2";#34;"ECHO               al Depto FINANCIERO.              ",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"PAUSE",FINR$
WRITE FORM "C70 C2";#34;"BATCH DIRDKT.BAT",FINR$
WRITE FORM "C70 C2";#34;"GOTO SALIR",FINR$
WRITE FORM "C70 C2";#34;":ERROR",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO     No EXISTE el Archivo especificado como:",FINR$
WRITE FORM "C70 C2";#34;OO1$,FINR$
WRITE FORM "C70 C2";#34;"ECHO   ",FINR$
WRITE FORM "C70 C2";#34;"ECHO     Ejecute ANTES la Opc=1 del Men£ de Procesos Diarios",FINR$
WRITE FORM "C70 C2";#34;"ECHO     (UTILIDADES,..INTERFASES..etc)..Medios de Pago..   ",FINR$
WRITE FORM "C70 C2";#34;"PAUSE",FINR$
WRITE FORM "C70 C2";#34;":SALIR",FINR$
WRITE FORM "C70 C2";#34;"AYUDA",FINR$

WRITE FORM "C70 C2";#35;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#35;"CLS     ",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO               Proceso Para Generar Archivo de clientes",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO                       Espere un Momento.. Por Favor..!",FINR$
WRITE FORM "C70 C2";#35;"ADX_UPGM:CFCLIFTE    ",FINR$
WRITE FORM "C70 C2";#35;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#35;"CLS     ",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO               Se Inicia la COPIA en Diskette de los ARCHIVOS..!",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO                       Espere un Momento.. Por Favor..!",FINR$
WRITE FORM "C70 C2";#35;"ECHO ON",FINR$
WRITE FORM "C70 C2";#35;DD1$,FINR$
WRITE FORM "C70 C2";#35;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#35;"PAUSE",FINR$
WRITE FORM "C70 C2";#35;"CLS     ",FINR$
WRITE FORM "C70 C2";#35;N1$,FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO               COPIA en DISKETTE Termina OK..!",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO               Enviar el DISKETTE para Actualizar",FINR$
WRITE FORM "C70 C2";#35;"ECHO               al CENTRO DE INFORMACION       ",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO               Adjunte Memorando con Cifras de Control..!",FINR$
WRITE FORM "C70 C2";#35;"PAUSE",FINR$
WRITE FORM "C70 C2";#35;"BATCH DIRDKT.BAT",FINR$
WRITE FORM "C70 C2";#35;"GOTO SALIR",FINR$
WRITE FORM "C70 C2";#35;":ERROR",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO     No EXISTE el Archivo especificado como:",FINR$
WRITE FORM "C70 C2";#35;O1$,FINR$
WRITE FORM "C70 C2";#35;"ECHO   ",FINR$
WRITE FORM "C70 C2";#35;"ECHO     Ejecute ANTES la Opc=1 del Men£ de Procesos Diarios",FINR$
WRITE FORM "C70 C2";#35;"ECHO     (UTILIDADES,..INTERFASES..etc)..Medios de Pago..   ",FINR$
WRITE FORM "C70 C2";#35;"PAUSE",FINR$
WRITE FORM "C70 C2";#35;":SALIR",FINR$
WRITE FORM "C70 C2";#35;"AYUDA",FINR$

WRITE FORM "C70 C2";#39;A$,FINR$
WRITE FORM "C6 C2";#38;ftp$,FINR$
WRITE FORM "C6 C2";#38;ftp$,FINR$
WRITE FORM "C20 C2";#38;ftp3$,FINR$
WRITE FORM "C30 C2";#38;ftp4$,FINR$
WRITE FORM "C4 C2";#38;ftp1$,FINR$
WRITE FORM "C70 C2";#33;IMPRI$,FINR$
WRITE FORM "C70 C2";#33;SORT$,FINR$
!WRITE FORM "C70 C2";#33;SORT2$,FINR$
WRITE FORM "C70 C2";#37;IMPRI1$,FINR$

WRITE FORM "C70 C2";#30;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#30;"CLS     ",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO               Proceso de Separaci¢n de los CREDITOS..!",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO                       Espere un Momento.. Por Favor..!",FINR$
WRITE FORM "C70 C2";#30;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#30;"CLS     ",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO               Se Inicia la COPIA en Diskette de los CREDITOS..!",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO                       Espere un Momento.. Por Favor..!",FINR$
WRITE FORM "C70 C2";#30;"ECHO ON",FINR$
WRITE FORM "C70 C2";#30;DD6$,FINR$
WRITE FORM "C70 C2";#30;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#30;"CLS     ",FINR$
WRITE FORM "C70 C2";#30;N$,FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO               COPIA en DISKETTE Termina OK..!",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO               Enviar el DISKETTE para Actualizar",FINR$
WRITE FORM "C70 C2";#30;"ECHO               al Depto de CR y COBRANZAS.       ",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO               Adjunte Memorando con Cifras de Control..!",FINR$
WRITE FORM "C70 C2";#30;"PAUSE",FINR$
WRITE FORM "C70 C2";#30;"BATCH DIRDKT.BAT",FINR$
WRITE FORM "C70 C2";#30;"GOTO SALIR",FINR$
WRITE FORM "C70 C2";#30;":ERROR",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO     No EXISTE el Archivo especificado como:",FINR$
WRITE FORM "C70 C2";#30;O$,FINR$
WRITE FORM "C70 C2";#30;"ECHO   ",FINR$
WRITE FORM "C70 C2";#30;"ECHO     Ejecute ANTES la Opc=1 del Men£ de Procesos Diarios",FINR$
WRITE FORM "C70 C2";#30;"ECHO     (UTILIDADES,..INTERFASES..etc)..Medios de Pago..   ",FINR$
WRITE FORM "C70 C2";#30;"PAUSE",FINR$
WRITE FORM "C70 C2";#30;":SALIR",FINR$
WRITE FORM "C70 C2";#30;"AYUDA",FINR$

WRITE FORM "C70 C2";#31;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#31;"CLS     ",FINR$
WRITE FORM "C70 C2";#31;"ECHO   ",FINR$
WRITE FORM "C70 C2";#31;"ECHO   ",FINR$
WRITE FORM "C70 C2";#31;"ADX_UPGM:CFPLULOG    ",FINR$
WRITE FORM "C70 C2";#31;"ECHO   ",FINR$
WRITE FORM "C70 C2";#31;"ECHO   ",FINR$
WRITE FORM "C70 C2";#31;"ECHO   ",FINR$
WRITE FORM "C70 C2";#31;"ECHO               Clasifica Archivo XLOG..!",FINR$
WRITE FORM "C70 C2";#31;"ECHO   ",FINR$
WRITE FORM "C70 C2";#31;"ECHO   ",FINR$
WRITE FORM "C70 C2";#31;"ECHO                       Espere un Momento.. Por Favor..!",FINR$
WRITE FORM "C70 C2";#31;OO$,FINR$

GOTO FINOK
!*************************************************************
  ERRTN:
  ERRORCOD$ = ERR
  
IF ERRF% = 29 THEN BEGIN
   CLEARS
   PRINT
   PRINT
   PRINT "    ERROR..!   NO EXISTE EL ARCHIVO DE CONTROL DE ALMACEN..!"
   PRINT "        Arch =  ";ERRF%;"   Error = ";ERR;"   Cod = ";ERRN
   PRINT "               Consulte con el Depto de Sistemas."
   PRINT "                 Crear  C:\ADXALMA.DAT"
   STOP
ENDIF
  IF ERR <> "EF" AND                \
     ERRF% = 25 THEN BEGIN            ! Si NO es END OF FILE Sess#25
     CONSOLE
     PRINT
     PRINT "VERIFIQUE ARCHIVO ASIGNADO ..!"
     PRINT "DATOS INVALIDOS DESDE CONTROL "
     PRINT"  "
     STOP
  ENDIF  
  IF ERR = "EF" AND                \
     ERRF% = 25 THEN BEGIN           ! Si es END OF FILE Sess#25
        PRINT "     NORMAL EOJ..."
        PRINT "FIN DEL PROCESO NORMAL..!"
        STOP
  ENDIF    
!**************************************************************
FINOK:
RESP$ = ANON$ + RIGHT$(RESP$,4)                     ! A¤o en ADXALMA.DAT
AL$ = LEFT$(AL$,4)+RESP$+RIGHT$(AL$,LEN(AL$)-12)    ! Actualiza nuevo Dia
WRITE FORM "C80" ;#29,1;AL$                         ! Graba ADXALMA.DAT
CLOSE 26
CLOSE 27
CLOSE 29
CLOSE 30
CLOSE 34
CLOSE 31
CLOSE 35 
STOP
END
  !*****************   FIN DEL PROGRAMA PRINCIPAL  *************
