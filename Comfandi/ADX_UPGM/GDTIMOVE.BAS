!------------------------------------------------------------------------------
!               Proyecto : IBM  -  
!               Programa : GDTMOVIN.BAS
!
!      Programa para EXTRAER Ventas desde el Archivo de Totales de 
!      Ventas de Art죅ulos EAMIMOVE.DAT
!
!------------------------------------------------------------------------------

!                       Ambiente de Controlador
  %ENVIRON C 

!              Definicion de variables de usuario locales

  REAL          AV,        FALTA                                  !
  INTEGER*1     INT1%,     INT2%,        II%(1)                   !
  STRING GLOBAL BATCH$,    SEQUENCE$,    COUNTER$,               \!
                FEC$,      ALM$,         ALMA$,       NUAL$,     \!
                NOAL$,     REGI$,        DEP$,        HORA$,     \!
                QUANPRIC$, OPCION$,      TIVAS$(1),   ETI$(1),   \!
                           REGISTRO$,                 DEPTOW$,   \!
                KEY$,                                            \!
                FAMILYN$,    MPGROUP$,  SALEQUAN$,             \!  
                SALEPRIC$,   LINKEDTO$, ITEMNAME$,             \!
                INDICAT2$,   DEPARTME$,                        \! Depto
                DFAMILYN$,   DMPGROUP$, DSALEQUAN$,            \!  
                DSALEPRIC$,  DLINKEDTO$,DITEMNAME$,            \!
                DINDICAT2$,  DDEPARTME$,DKEY$                   ! Depto

  STRING GLOBAL H$                                                !

  INTEGER*1 GLOBAL                                               \!
                DMCREQ,    DMCMT,        DMCPO,       INDICAT0,  \!
                INDICAT1,  DMCESTAT,     SW.FIN,      SWREP,     \!
                INDICAT1A, SWERROR,      CON.LIN%,    FIS.TARIFA  !

  INTEGER*2 GLOBAL                                               \!
                DMCRCSF,   DMCSTSF,      DMCERRCT,    UEX1,  UEX2 !

  INTEGER*2 GLOBAL                                               \!
                PCOD%,     PDES%,        PDEP%,       PVAL%,     \!
                PBAR%,     PPLU%,        FIS.FIV%,    JJ%,       \!
                IVA%,      LIGA%,        WLIGA%,      ICORN%,    \!
                ICOG%,     ICOL%,        P%,          FIS.CNT%,  \!
                REC.LEN,   KEY.LEN,                   FIS.TIC%    !

  INTEGER*4 GLOBAL VALVTA%, DMCRCSFW,     TOPEI%,      TOPEF%      !

  INTEGER*4 GLOBAL                                               \!
                REG29%,    GRA29%,       OTROAL%,                \!
                DMCERROF,  TOTBUE,       TOTCERO,     TOTLEI,    \!
                SINDES%,   PREREQ%,      PEPRREQ%,    PLUPRE%,   \!
                TOTBAR,    DEP4%,        TIPOV%,      CORRE%,    \!
                HX%,       S%,           SX%,         SUM%,      \!
                BLK.NUM,   LEI%,         ASIZE,       POSIC%,    \!
                LB%(1),    VRIC%,        VRVTA%,      PLIN%,     \!
                CHA.TRE,   LON.CHA,      RES.UNO,     TOT.REA,   \!
                REA.UPD,   TOT.ADD,      WRI.UPD,     RES.DOS,   \!
                TOT.DEL,   TWR.1DE,      TWR.2DE,     TWR.3DE,   \!
                TWR.MAS,   TOT.OPE,      TOT.CLO,     TOT.CLW,   \!
                TOT.REL,   REC%,         R%                       ! FOR K/FILE


INTEGER*4    I%, NUM%,    VAL%,   ALI%,   COLI%,      SIZ%   !


INTEGER*1    DINDICAT0,   DINDICAT1, DINDICAT1A              !

INTEGER*2    DUEX1,       DUEX2                              !
INTEGER*4    LEID%,       RET%,      CORI%,          PAUSA%,\!
             TINIC%,      TACTU%,    TTOTA%                  !
INTEGER*4    PESA%,       MEDI%,     DEPA%,          CONC%   !
!
  %INCLUDE EAMASMCT.J86
!------------------------------------------------------------------------------
!                         S U B R U T I N A S   
!------------------------------------------------------------------------------
!          Rutina para Restaurar campos de Datos de EAMFBACT
!
!               1- Total Puntos Acumulados
!               2- N즡ero de Transacciones Totales
!               3- Total Puntos Redimidos
!               4- Valor Total Cupones de almac굈 (Autom쟴ico)
!               5- N즡ero de Puntos dela Ultima Transacci줻
!               6- Bruto(+) MENOS Bruto(-) Total
!
!------------------------------------------------------------------------------
SUB BORRA.ACTIVI                                       !
        REG$ = PACK$(STRING$(26,"0")) +               \! Tot/Pts,Tr,Red,Cpn
               MID$(REG$,14,03) + PACK$("000000") +   \! Pts/Ult.Trx
               MID$(REG$,20,32) + PACK$("00000000") + \! Datos, Pts/RedAct 
               MID$(REG$,25,03) + PACK$("00000000") + \! Datos, ta/Tot
               RIGHT$(REG$,30)                         ! Resto del Reg
END SUB
!------------------------------------------------------------------------------
!          Rutina para Restaurar campos de Datos de EAMFBCUS
!
!               1- N즡ero de Ajustes
!               2- N즡ero de Redenciones
!               3- Total Puntos Ajustados +'s
!               4- Total Puntos Ajustados -'s
!
!------------------------------------------------------------------------------
SUB BORRA.ENRROL                                       !
        REG$ = LEFT$(REG$,170)                     +  \! Datos Inicio
               PACK$(STRING$(24,"0"))              +  \! #Aj,#Red,Ptos-Ajus
               MID$(REG$,183,29) + PACK$("000000") +  \! Datos y Ult.RainChk
               RIGHT$(REG$,31)                         ! Resto del Reg
END SUB
!------------------------------------------------------------------------------
!                 Subrutinas de Inicio de Tareas de Background
!------------------------------------------------------------------------------
FUNCTION ADXSTART (NAME$,PARM$,MESS$) EXTERNAL         ! Funcion de Inicio de
    INTEGER*2 ADXSTART                                 ! Tareas de Background
    STRING    NAME$,   PARM$,      MESS$               ! Variables
END FUNCTION                                           !
!----------------------------------------------------------------------------
!            Funci줻 para Solicitar Servicios de la Aplicaci줻
!----------------------------------------------------------------------------

SUB ADXSERVE(RET,FUNC,PARM1,PARM2$) EXTERNAL             !
    STRING       PARM2$                                  !
    INTEGER*4    RET                                     !
    INTEGER*2    FUNC,   PARM1                           !
END SUB                                                  !
!----------------------------------------------------------------------------
!            Funci줻 para Temporizar elaboraciion de Archivo PSS
!----------------------------------------------------------------------------
SUB TEMPORIZAR                                         !
    CLOSE 1   :  CLOSE 5                               !
    IF VAL(LEFT$(TIME$,4)) > 1830 THEN BEGIN           !
       PARAM2$="Stopped At: "+LEFT$(TIME$,2)+         \!
               ":"+MID$(TIME$,3,2)+ ":"+RIGHT$(TIME$,2)!
       IF OPOC% = 1 THEN                              \!
          CALL ADXSERVE(RETU%,CFUN%,PARAM1,PARAM2$)    !
       IF OPC% > 1 THEN BEGIN                          !
          LOCATE 18,05 : PRINT PARAM2$                 !
          LOCATE 22,10                                 !
          PRINT " Translation Process Complete..!"     !
          INPUT "    Press ENTER to Continue..";ZZ$    !
       ENDIF                                           !
       STOP                                            !
    ENDIF                                              !
END SUB                                                !
!------------------------------------------------------------------------------
!                           Programa Principal  
!------------------------------------------------------------------------------
TEXTO$ = "Este proceso  s줹o  puede  ser ejecutado" + \!
         "como una Tarea de Background.           " + \!
         "                                        " + \!
         "Opcionalmente, puede ejecutarse  bajo la" + \!
         "modalidad de Comando desde ADX_UPGM para" + \!
         "preparar interfaces tipo  PSS  desde los" + \!
         "Archivos de Movimiento de Ventas Totales" + \!
         "EAMIMOVE y/o EAMIMOVO.                  " + \!
         "                                        " + \!
         "El Archivo resultante se denomina as�:  " + \!
         "              C:\ITMnnn                 " + \!
         "       donde nnn = Store Number         " + \!
         "                                        " + \!
         "Consulte la opci줻  apropiada � SOLICITE" + \!
         "Licencia de  USO, si  desea traducir m쟳" + \!
         "Archivos tipo EAMIMOVO.                 " + \!
         "                                        " + \!
         "          Contin즕 con ENTER...         "    !
    DIM II%(9)                                         ! Integer Low Byte's:
    II%(00)=15   :   II%(01)=31   :   II%(02)=47       ! 0F, 1F, 2F
    II%(03)=63   :   II%(04)=79   :   II%(05)=95       ! 3F, 4F, 5F, 
    II%(06)=111  :   II%(07)=127  :   II%(08)=143      ! 6F, 7F, 8F
    II%(09)=159                                        ! 9F
    L1$ = "旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커"         !
    L2$ = "�                                �"         !
    L3$ = "읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸"         !
    FR$ = CHR$(13) + CHR$(10)                          !
    ON ERROR GOTO RETORNO                              !
    OPC$ = LEFT$(COMMAND$+"        ",8)                !
    IF LEFT$(OPC$,1) = "1" THEN OPC$ = "4"             !
    IF LEFT$(OPC$,8) = "BACKGRND" THEN OPC$ = "1"      !
    OPC% = VAL(LEFT$(OPC$,1))                          !
    CFUN% = 4                                          !
    CALL ADXSERVE(RETU%,CFUN%,PARAM1,PARAM2$)          !
    ALMAC$ = MID$(PARAM2$,2,3)                         !
LOOP:                                                  !
    PAUSA% = 15                                        ! Pausa por Defecto
    OPEN "C:\ADX_IDT1\EAMITEMR.DAT" KEYED RECL 46 AS 7 !
    KEY$ = PACK$("777777777701")                       ! Plu temporizador
    EOF$ = "  "                                        !
    READ FORM "C17 C5 C24";#7KEY KEY$;C1$,C2$,C3$      !
    IF EOF$ = "  " THEN PAUSA% = VAL(UNPACK$(C2$))     ! Pausa por Sistema
    CLOSE 7                                            !
    PARAM2$ = "Program will Restart in " +            \!
               STR$(PAUSA%) + " Min."                  !
    PAUSA%  = PAUSA% * 60000                           !
    TINIC% = VAL(LEFT$(TIME$,2))*3600                  !
    TINIC% = TINIC% + VAL(MID$(TIME$,3,2))*60          !
    TINIC% = TINIC% + VAL(RIGHT$(TIME$,2))             !
    PARAM2$= PARAM2$ + " from " +                     \!
             LEFT$(TIME$,2)+":"+MID$(TIME$,3,2)+":" + \!
             RIGHT$(TIME$,2)                           !
    IF FT% = 1 THEN BEGIN                              !
       IF OPC% = 1 THEN WAIT ; 3000  :                \!
          CALL ADXSERVE(RETU%,CFUN%,PARAM1,PARAM2$)    !
       IF OPC% > 1 THEN                               \!
          PARAM2$ = CHR$(27)+"p"+CHR$(27)+"b3"+       \!
                    PARAM2$+CHR$(27)+"q"+CHR$(27)+"b7" !
          LOCATE 22,15 : PRINT PARAM2$                 !
       WAIT;PAUSA%                                     !
    ENDIF                                              !
    FT% = 1                                            !
    CFUN% = 26                                         !
    PARAM2$=" Host Translation for Store "+ALMAC$      !
    CLEARS                                             !
    IF OPC% = 1 THEN  BEGIN                            !
       CALL ADXSERVE(RETU%,CFUN%,PARAM1,PARAM2$)       !
       WAIT ; 5000                                     !
    ENDIF                                              !
  INICIO:                                              !
    REC% = 0                                           !
    CLEARS                                             !
    IF OPC% <> 1 AND                                  \!
       OPC% <> 2 AND                                  \!
       OPC% <> 3 THEN BEGIN                            !
          PARAM2$ = " Programa bajo Licencia.! Se suspende."
          CALL ADXSERVE(RETU%,CFUN%,PARAM1,PARAM2$)    !
          IF OPC% > 1 THEN BEGIN                       !
          CLEARS                                       !
          FOR I% = 1 TO 18                             !
              LOCATE 5+I%,10                           !
              PRINT MID$(TEXTO$,I%*40-39,40)           !
          NEXT I%                                      !
          INPUT "  ";OPC$ : CLEARS                     !
       ENDIF                                           !
       STOP                                            !
    ENDIF                                              !
    A1$ = "C:\ADX_IDT4\EAMIMOVE.DAT"                   !
    IF OPC% = 3 THEN A1$ = "C:\ADX_IDT4\EAMIMOVO.DAT"  !
    A2$ = "C:\ITM"+ALMAC$+".WK4"                       !
    EOF$ = "  "                                        !
    OPEN A1$ DIRECT RECL 512 AS 1 BUFFSIZE 51200       !
    IF EOF$ <> "  " THEN GOTO INICIO                   !
    REC% = REC% +1                                     !
    READ FORM "T43 I4 I2 T55 I2 17I4 C388";#1,REC%;   \! HEADER DATA 
               BLK.NUM,REC.LEN,KEY.LEN,CHA.TRE,       \!
               LON.CHA,RES.UNO,TOT.REA,REA.UPD,       \!
               TOT.ADD,WRI.UPD,RES.DOS,TOT.DEL,       \!
               TWR.1DE,TWR.2DE,TWR.3DE,TWR.MAS,       \!
               TOT.OPE,TOT.CLO,TOT.CLW,TOT.REL,H$      ! FOR KEYED FILE
    ASIZE = 508/REC.LEN * BLK.NUM                      ! MAX A$ RECORDS 
    IF OPC% > 1 THEN BEGIN                             !
       LOCATE 2 , 15                                   !
       PRINT "Controller to Host Translation Task Interface"                       !
       LOCATE 3 , 17                                   !
       PRINT "for Item Movement Totals File (EAMIMOVE)"!
       EOF$ = "  "                                     !
       LOCATE 06,05 : PRINT "Started At: ";            !
       PRINT LEFT$(TIME$,2) + ":";                     !
       PRINT MID$(TIME$,3,2)+ ":";                     !
       PRINT RIGHT$(TIME$,2)                           !
       LOCATE 08,02                                    !
       PRINT PARAM2$                                   !
       LOCATE 10,05                                    !
       PRINT "Source Path: ";RIGHT$(A1$,12)            !
       LOCATE 05,35 : PRINT L1$                        !
       LOCATE 06,35 : PRINT L2$                        !
       LOCATE 07,35 : PRINT L2$                        !
       LOCATE 08,35 : PRINT L2$                        !
       LOCATE 09,35 : PRINT L2$                        !
       LOCATE 10,35 : PRINT L2$                        !
       LOCATE 11,35 : PRINT L2$                        !
       LOCATE 12,35 : PRINT L2$                        !
       LOCATE 13,35 : PRINT L2$                        !
       LOCATE 14,35 : PRINT L2$                        !
       LOCATE 15,35 : PRINT L2$                        !
       LOCATE 16,35 : PRINT L2$                        !
       LOCATE 17,35 : PRINT L2$                        !
       LOCATE 18,35 : PRINT L2$                        !
       LOCATE 19,35 : PRINT L3$                        !           
       LOCATE 11,05 : PRINT "Target File:  ";A2$       !
       LOCATE 07,40 : PRINT "  Keyed File Statistics"  !
       LOCATE 09,40 : PRINT "    Cant/Bloques: ";BLK.NUM
       LOCATE 10,40 : PRINT "   Long/Registro: ";REC.LEN
       LOCATE 11,40 : PRINT "      Long/Llave: ";KEY.LEN
       LOCATE 12,40 : PRINT "   Capacidad M쟸: ";ASIZE !
       LOCATE 13,40 : PRINT "Tot Reg/Grabados: ";TOT.ADD
       LOCATE 14,40 : PRINT "Tot Reg/Borrados: ";TOT.DEL
       LOCATE 16,40 : PRINT "  Nueva Cant/Reg: ";TOT.NUE$
    ENDIF                                              !
    SIZ% = BLK.NUM                                     !
    IF TOT.ADD = 0 THEN TOT.ADD = ASIZE - ASIZE/4      !
    IF VAL(TOT.NUE$) > ASIZE THEN ASIZE = VAL(TOT.NUE$)!
    IF ASIZE < (TOT.ADD+TOT.ADD/4) THEN BEGIN          !
       LOCATE 22,1                                     !
       PRINT "  Not enough Space Allocation. ";        !
       PRINT "Add 25% to fit ";                        !
       PRINT TOT.ADD-TOT.DEL;" Records..!"             !
       INPUT "  " ; A1$                                !
       LOCATE 22,1                                     !
       PRINT STRING$(80," ")                           !
       CLOSE 1                                         !
       GOTO INICIO                                     !
    ENDIF                                              !
    IF OPC% > 1 THEN BEGIN                             !
       LOCATE 12,59 : PRINT STR$(ASIZE);"  "           !
       LOCATE 16,59 : PRINT STR$(ASIZE);"  "           !
       LOCATE 21,42 : PRINT "Creando: ";A2$            !
       FOR2$="C"+STR$(KEY.LEN)+" C"+STR$(REC.LEN-KEY.LEN) 
       LOCATE 17,37 : PRINT "Write Record FORMAT:  ";FOR2$
    ENDIF                                              !
    CREATE A2$ RECL 21 AS 5 BUFFSIZE 56120             !
 R1:                                                   !
    REC% = REC% + 1                                    ! BUMP SECTOR NO
    PARAM2$ = "  Read Block Count: " + STR$(REC%)      !
!    IF OPC% = 1 THEN CALL ADXSERVE(RETU%,CFUN%,PARAM1,PARAM2$)
    IF OPC% > 1 THEN BEGIN                             !
       LOCATE 13,05                                    !
       PRINT "  Read Block Count:";REC%                !
    ENDIF                                              !
    IF REC% > BLK.NUM THEN BEGIN                       ! DONE SO CLOSE
       CALL TEMPORIZAR                                 !
       GOTO LOOP                                       !
    ENDIF                                              !
    READ FORM "T5 C508"; #1, REC% ; H$                 ! READ SECTOR 
    AVA% = REC% * 10 / SIZ%                            !
    AV   = FLOAT(REC% * 100) / SIZ%                    !
    AVA$ = LEFT$(STRING$(AVA%,CHR$(254)) +            \!
           STRING$(10,CHR$(046)),10)                   !
    AVA$ = AVA$ + "  "                                 !
    TACTU% = VAL(LEFT$(TIME$,2))*3600                  !
    TACTU% = TACTU% + VAL(MID$(TIME$,3,2))*60          !
    TACTU% = TACTU% + VAL(RIGHT$(TIME$,2))             !
    IF TACTU% < TINIC% THEN TACTU% = TACTU% + TINIC%   !
    TTOTA% = TACTU% - TINIC%                           !
    MINU%  = TTOTA% / 60                               !
    SEGU%  = MOD(TTOTA%,60)
    FALTA  = FLOAT(TTOTA% * 100) / AV - TTOTA%         !
    FALTA  = FALTA / 3600                              !
    PARAM2$ = " Progress:"+AVA$+STR$(INT(AV))+        \!
              "% "+"Duration: "+STR$(MINU%)+":"+      \!
              STR$(SEGU%)                              !
    IF OPC% = 1 THEN CALL ADXSERVE(RETU%,CFUN%,PARAM1,PARAM2$)
    IF OPC% > 1 THEN BEGIN                             !
       LOCATE 20,38                                    !
       PRINT "Progress: " ; AVA$ ;                     !
       PRINT USING "###.##" ; AV;                      !
       PRINT "%      "                                 !
       LOCATE 21,40                                    !
       PRINT USING "###.##" ; FALTA ;                  !
       PRINT " Hrs. Remaining..  "                     !
    ENDIF                                              !
    FOR X = 1 TO 507 STEP  REC.LEN                     ! CHECK IT OUT
        KEY$   = MID$(H$,X,KEY.LEN)                    ! EXTRACT KEY
        REG$   = MID$(H$,X+KEY.LEN,REC.LEN-KEY.LEN)    ! EXTRACT REC REMAIN
        IF RESET% = 1 THEN CALL BORRA.ACTIVI           !
        IF RESET% = 2 THEN CALL BORRA.ENRROL           !

        CERO%=MATCH(STRING$(KEY.LEN,"00"),UNPACK$(KEY$),1) 
        IF CERO% = 1  THEN GOTO R1                     ! NO MORE KEYS
        IF KEY$ = PACK$(STRING$(KEY.LEN,"99")) THEN   \! NO MORE KEYS
        GOTO RDNXT                                     !
        LEI% = LEI% + 1                                !
        IF OPC% > 1 THEN BEGIN                         !
           LOCATE 14,05                                !
           PRINT " Read Record Count:";LEI%            !
        ENDIF                                          !
        CODIN$ = UNPACK$(KEY$)                         !
!        INT1%  = II%(VAL(RIGHT$(CODIN$,1)))            !
!        CODIN$ = MID$(CODIN$,6,6)                      !
!        CODIN$ = PACK$(CODIN$)                         !
!        CODIN$ = RIGHT$(CODIN$,7)                      !
        CANVE$=RIGHT$("0000000"+STR$(GETN4(REG$,0)),7)  !
        KGSVE$=RIGHT$("0000000"+STR$(GETN4(REG$,8)),7)  !
        IF VAL(KGSVE$) <> 0 THEN CANVE$ = KGSVE$
!        INT2%  = II%(VAL(RIGHT$(CANVE$,1)))            !
!        CANVE$ = MID$(CANVE$,2,4)                      !
!        CANVE$ = PACK$(CANVE$)                         !
        CANVE$ = RIGHT$(CANVE$,7)                      !
        WRITE FORM "C12 C7 C2";#5;                     \!
              CODIN$,CANVE$,FR$                        !
!              CODIN$,INT1%,CANVE$,INT2%,","            !
        R% = R% + 1                                    !
        PARAM2$ = "Write Record Count: "+STR$(R%)      !
        IF OPC% = 1 THEN CALL ADXSERVE(RETU%,CFUN%,PARAM1,PARAM2$)
        IF OPC% > 1 THEN BEGIN                         !
           LOCATE 15,05                                !
           PRINT PARAM2$                               !
        ENDIF                                          !
 RDNXT:                                                ! TRY NEXT SECTOR
    NEXT X                                             ! TRY NEXT RECORD
  GOTO R1                                              !

!------------------------------------------------------------------------------
!             Rutina de Reporte de Error por falta de Reg CONTROL
!------------------------------------------------------------------------------

RETORNO:                                               !
    EOF$ = ERR                                         !
    IF ERRF% = 7 THEN RESUME                           !
    HX% = ERRN                                         !
    ERRFX$=""                                          !
    FOR S% = 28 TO 0 STEP -4                           !
        SX% = SHIFT(HX%,S%)                            !
        SUM% = SX% AND 000FH                           !
        IF SUM% > 9 THEN                              \!
           SUM% = SUM% + 55                           \!
        ELSE                                          \!
           SUM% = SUM% + 48                            !
        Z$ = CHR$(SUM%)                                !
        ERRFX$ = ERRFX$ + Z$                           !
    NEXT S%                                            !
   IF ERRF% = 1 THEN BEGIN                             !
      IF ERR="OE" OR ERR="DZ" OR ERR="FU" THEN BEGIN   !
         LOCATE 22,5                                   !
         PRINT "File Specification Error..  Retry..";  !  
         INPUT "  " ; A1$                              !
         LOCATE 22,1                                   !
         PRINT STRING$(80," ")                         !
         RESUME                                        !
      ENDIF                                            !
   ENDIF                                               !
   IF OPC% > 1 THEN LOCATE 22,1 : PRINT ERR;"  ";ERRF%;"  ";ERRFX$
END                                                    !
!                ********************************************
!             ***       FIN  DEL  PROGRAMA  PRINCIPAL        ***
!                ********************************************

