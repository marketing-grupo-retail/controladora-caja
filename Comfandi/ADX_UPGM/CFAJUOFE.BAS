\------------------------------------------------------------------------------
\                   NOMBRE DEL MODULO : CFAJUOFE.BAS
\         Modulo de Ajuste de Descuentos para Ofertas y/o Mercados
\  
\               VERSION : General Consolidada Enero del 2000
\
\       Primer Revision : Enero del 2000 by Global DataTel
\  
\  Resumen de Cambios
\
\       NOTA: Este programa debe ligarse de la siguiente manera:
\
\                  LINK86 CFAJUOFE,ADXACRCL.L86
!-----------------------------------------------------------------
!            ***    Descripci¢n del Fuente      ***
!
!    Los PLU's de Ofertas/Promociones estan en el rango de 1 a 9999
!    Este RANGO de Ofertas/Promociones existen en EAMITEMR con el 
!    PREFIJO '99999999' para conformar la siguiente estructura:
!
!               99999999pppp, donde:    pppp = No. de Of/Promoc (1-9999)
!
!    Adicionalmente todos los PLU's de los Articulos que conforman la Promocion
!    se numeran de 01-99. Tambien utilizan un PREFIJO y se identifican de la
!    siguiente manera:
!
!               999999ppppnn, donde:    pppp = No. de Of/Promoc   (1-9999)
!                                         nn = No. de PLU Of/Prom (1-99)
!
!    La caracteristica principal es: PRICING METHOD = 5 con un 'Alias Code'
!    que identifica el verdadero PLU de Venta.
!    
!    Toda la CADENA de 'Linked Items' se hace a traves de los PLU's con
!    PRFIJO 999999pppp para evitar el encadenamiento en la Venta Individual
!    de cualquiera de los Items de la Cadena.
!------------------------------------------------------------------------------
!-----------------------------------------------------------------------------
!  14/03/98     Consolidar rutinas para version COMFANDI

  REAL      GLOBAL         VAR

  STRING    GLOBAL                                               \!
                BATCH$,    SEQUENCE$,    COUNTER$,    INDICAT2$, \!
                FEC$,      ALM$,         ALMA$,       NUAL$,     \!
                NOAL$,     REGI$,        DEP$,        HORA$,     \!
		            QUANPRIC$, OPCION$,      TIVAS$(1),   ETI$(1),   \!
                REL1$,     PARAM2$,      GR.DATA$,    GR.DAT2$    !

  INTEGER*1 GLOBAL                                               \!
                IND1OF%,                                         \!
                DMCREQ,    DMCMT,        DMCPO,       INDICAT0,  \!
                INDICAT1,  DMCESTAT,     SW.FIN,      SWREP,     \!
                INDICAT1A, SWERROR,      CON.LIN%,    FIS.TARIFA  !

  INTEGER*2 GLOBAL                                               \!
                CFUN%,     PARAM1,       NA%,         NAA%,      \!
                DMCRCSF,   DMCSTSF,      DMCERRCT,    UEX1,  UEX2 !

  INTEGER*2 GLOBAL                                               \!
                PCOD%,     PDES%,        PDEP%,       PVAL%,     \!
                PBAR%,     PPLU%,        FIS.FIV%,    II%,   JJ%,\!
                IVA%,      LIGA%,        WLIGA%,      ICORN%,    \!
                ICOG%,     ICOL%,        P%,          FIS.CNT%,  \!
                REC.LEN,   KEY.LEN,      REC%,        FIS.TIC%    !

  INTEGER*4 GLOBAL                                               \!
                VALVTA%,   DMCRCSFW,     TOPEI%,      TOPEF%      !

  INTEGER*4 GLOBAL                                               \!
                DES.NUE%,  DES.99%,                              \!
                VTA.TO%,   VTA.OF%                                !

  INTEGER*4 GLOBAL                                               \!
                RETU%,                                           \!
                REG29%,    GRA29%,                               \!
                DMCERROF,  TOTBUE,       TOTCERO,     TOTLEI,    \!
                SINDES%,   PREREQ%,      PEPRREQ%,    PLUPRE%,   \!
                TOTBAR,    DEP4%,        TIPOV%,      CORRE%,    \!
                HX%,       S%,           SX%,         SUM%,      \!
                BLK.NUM,   LEI%,         ASIZE,       POSIC%,    \!
                LB%(1),    VRIC%,        VRVTA%,      PLIN%       !
                
%INCLUDE EAMASMCT.J86                      ! Include packbin functions
!------------------------------------------------------------------------------
!                 Subrutinas de Inicio de Tareas de Background
!------------------------------------------------------------------------------
FUNCTION ADXSTART (NAME$,PARM$,MESS$) EXTERNAL         ! Funcion de Inicio de
    INTEGER*2 ADXSTART                                 ! Tareas de Background
    STRING    NAME$,   PARM$,      MESS$               ! Variables
END FUNCTION                                           !
!----------------------------------------------------------------------------
!            Funci¢n para Solicitar Servicios de la Aplicaci¢n
!----------------------------------------------------------------------------

SUB ADXSERVE(RET,FUNC,PARM1,PARM2$) EXTERNAL             !
    STRING       PARM2$                                  !
    INTEGER*4    RET                                     !
    INTEGER*2    FUNC,   PARM1                           !
END SUB                                                  !
!----------------------------------------------------------------------------
!           Rutina para armar Fecha y Hora de Transaccion
!------------------------------------------------------------------------------
SUB TOMA.FECHORA                                       !
    FRA.TIM$ = TIME$                                   ! Toma Hora Sistema
    FRA.H$  = LEFT$(FRA.TIM$,2)                        ! Arma horas
    FRA.M$  = MID$(FRA.TIM$,3,2)                       ! Arma minutos
    FRA.S$  = RIGHT$(FRA.TIM$,2)                       ! Arma segundos
    FRA.FEC$= DATE$                                    ! Toma Fecha Sistema
    FRA.AA$ = LEFT$(FRA.FEC$,2)                        ! Arma ano
    FRA.MM$ = MID$(FRA.FEC$,3,2)                       ! Arma mes
    FRA.DD$ = RIGHT$(FRA.FEC$,2)                       ! Arma dia
    FRA.LIN$= "Hora: "+FRA.H$+":"+FRA.M$+             \!
              ":"+FRA.S$+" Fecha: "+FIS.DL$            ! Arma la Linea
END SUB                                                !
!------------------------------------------------------------------------------
!              Subrutina para Grabar Ofertas en EAMITEMR
!------------------------------------------------------------------------------
SUB GRABA.OFE                                          !
    VALOF$=PACK$(RIGHT$(STRING$(12,"0") +             \! Reemp Vr Vta por 
           STR$(VTA.TO%),12))                          ! VrOferta
    WRITE FORM "C6 C2 I1 C7 C6 C2 C18 C4 C31";#4;     \! Graba RegInic Oferta 
          KEY.MER$,IND.012$,                          \!
          IND1OF%,INDIS$,VALOF$,LIG99$,               \!
          NOMOFE$,CUSER$, GR.DATA$                     ! 
END SUB                                                !
!------------------------------------------------------------------------------
!           Subrutina para Borrar Descto/Ofertas de EAMITEMR
!------------------------------------------------------------------------------
SUB BORRA.DESC                                         !
    IF LIG2% = 1 THEN BEGIN                            ! Si tenia Descuento
       LIG2% = 0                                       !
       KEY.OF$=RIGHT$("0000"+STR$(OFE%),4)             ! arma llave Descu
       KEY.OF$=PACK$("999999"+KEY.OF$+"99")            ! y BORRA Descto
       DELREC 4;KEY.OF$                                !
    ENDIF                                              !
END SUB                                                !
!------------------------------------------------------------------------------
!          Subrutina para Reporte de Variaci¢n de Ofertas 
!------------------------------------------------------------------------------
SUB REPORTE                                            !
       LPRINTER             
       print LEOM$;" Des.nue=";DES.NUE%;" Vta.to=";VTA.TO%;
       print " Vta.of=";VTA.OF%;" Des99=";DES.99%;
       print " Hr:";left$(time$,2);":";mid$(time$,3,2);":";right$(time$,2)
END SUB                                                !
!------------------------------------------------------------------------------
!          Subrutina para explosionar Ofertas en LOG de Transacciones
!------------------------------------------------------------------------------

SUB VERIFICA.OFERTA PUBLIC                             !
FOR OFE% = 1 TO 999                                    ! Para todas las Ofertas
    LIG2% = 0                                          ! Ind para Desc 99
    PARAM2$ = " Leyendo Oferta: " + STR$(OFE%)         ! Mensaje BG de Lec
    CALL ADXSERVE(RETU%,CFUN%,PARAM1,PARAM2$)          ! Rutina Serv
    PREFOF$="999999"+RIGHT$("0000"+STR$(OFE%),4)       ! Prefijo ofertas
    KEY.OF$ = RIGHT$("0000" + STR$(OFE%),4)            ! Arma llave Ofertas
    KEY.OF$ = PACK$("99999999" + KEY.OF$)              ! con prefijo
    EOF$ = "  "                                        ! Condic Fin ARch Ok
    READ FORM "C8 I1 C7 C6 C2 C18 C4 C31";            \! Lectura Oferta desde
         #4 KEY KEY.OF$;KEYIND$,IND1OF%,              \! EAMITEMR
         INDIS$,VALOF$,LIG99$,NOMOFE$,CUSER$, GR.DATA$ !
    KEY.MER$ = LEFT$(KEYIND$,6)                        ! Toma Llave Ofer/Merc
    IND.012$ = RIGHT$(KEYIND$,2)                       ! Toma Indicat
    VTA.TO%  = 0                                       ! Inicia Acumulador
    IF EOF$ = "  " THEN BEGIN                          ! Si existe Ofer/Merc
       VTA.OF% = VAL(UNPACK$(VALOF$))                  ! Toma Valor Oferta
       FOR LIG1% = 1 TO 99                             ! Explosiona la Oferta
           LIGA$=RIGHT$("00"+STR$(LIG1%),2)            ! Arma llave para
           FIS.KIT$ = PACK$(PREFOF$ + LIGA$)           ! lectura de Regs MP5
           EOF$ = "  "                                 ! 
           READ FORM "C16 C6 C2 C18 C4 C31";          \! Lectura de Reg Liga
                #4 KEY FIS.KIT$;                      \! EAMITEMR
                RELE1$,VALOF$,LIGO$,DESOFE$,RELE2$,   \!
                GR.DAT2$
           DES.99% = VAL(UNPACK$(VALOF$))              ! Toma Vr Liga/Descto
           IF EOF$ = "  " THEN BEGIN                   ! Lectura Ok Liga/Desc
              IF LIG1% <> 99 THEN BEGIN                ! Si no es Descto
                 PLUOF$ = UNPACK$(VALOF$)              ! Toma Cod Alias
                 KEY.OF$ = VALOF$                      ! y arma Llave Lec
                 READ FORM "C8 C9 C5 C2 C18 C4 C31";  \! Lee los Reg ALIAS
                      #4 KEY KEY.OF$;                 \! verdaderos de Oferta
                      RELE1$,RELE3$,                  \!
                      VALOF$,LIGA$,DESOFE$,RELE2$,    \!
                      GR.DAT2$
                 VTA.TO%=VTA.TO%+VAL(UNPACK$(VALOF$))  ! Acum Prec/Vta Real
                 PARAM2$=" Lee Of.: "            +    \! Despliega Inf de
                         STR$(OFE%)+"   Link: "  +    \! cada Plu ALIAS
                         STR$(LIG1%)+"  PLU: "   +    \! y su Valor
                         STR$(VAL(PLUOF$))+" Vr: " +  \!
                         STR$(VAL(UNPACK$(VALOF$)))    !
                 CALL ADXSERVE(RETU%,CFUN%,PARAM1,PARAM2$)
!                 WAIT ; 500                            !
              ENDIF                                    !
              IF LIG1% = 99 THEN LIG2% = 1             ! La Ofer trae Descto 
           ENDIF ELSE BEGIN                            ! Si NO hay mas Ligas
              IF LIG1% <> 99 THEN LIG1% = 98           ! y NO es la Ult Liga
           ENDIF                                       ! Bypass hasta 98
       NEXT LIG1%                                      ! siguiente Liga
       DES.NUE% = VTA.TO% - VTA.OF%                    ! Calcula Nuevo Descto
       LEOM$ = "Mercado"                               ! DEscrip Default
       IF IND1OF% AND 01H THEN BEGIN                   !
          LEOM$ = " Oferta"                            !
          IF VTA.OF% > VTA.TO% THEN BEGIN              ! Si Ofer>Venta PLU's
             LIG99$ = PACK$("0000")                    !
             CAMBIO$ = "Oferta Mayor a la Venta"       !
             CALL REPORTE                              !
             CALL GRABA.OFE                            !
             CALL BORRA.DESC                           !
          ENDIF ELSE BEGIN                             !
             IF DES.99% = DES.NUE% AND                \!
                DES.99% = 0 THEN BEGIN                 !
                   CALL BORRA.DESC                     !
                   GOTO SALEOF                         !
             ENDIF                                     !    
             IF DES.99% > DES.NUE% THEN BEGIN          !
                CAMBIO$ = "Of. Dif a Vta. Baja Desc.!" !
                CALL REPORTE                           !
                DES.99% = DES.99% - (DES.99%-DES.NUE%) !
                IF DES.99% = 0 THEN BEGIN              !
                   LIG99$ = PACK$("0000")              !
                   CALL GRABA.OFE                      !
                   CALL BORRA.DESC                     !
                   GOTO SALEOF                         !
                ENDIF                                  !
             ENDIF ELSE BEGIN                          !
                CAMBIO$ = "Of. Dif a Vta. Sube Desc.!" !
                CALL REPORTE                           !
                DES.99% = DES.NUE%                     !
             ENDIF                                     !
             LIG99$ = PACK$("0099")                    !
             VTA.TO% = VTA.OF%                         !
             CALL GRABA.OFE                            !
             KEY.OF$=RIGHT$("0000"+STR$(OFE%),4)       ! arma llave Descu
             KEY.OF$=PACK$("999999"+KEY.OF$+"99")      ! y GRABA Descto
             FIL1$ = PACK$("00020050")                 ! Indicat Miscel.Refu
             DEPT$ = PACK$("0000")                     ! Sin Dpto
             FIL2$ = "0000000000"                      ! Fam,Mult,Cant CERO
             FIL3$ = PACK$("00000000")                 ! Fam,Mult,Cant CERO
             PRIC$ = PACK$(RIGHT$(FIL2$+STR$(DES.99%),10)) ! Valor DESCUENTO
             FIL2$ = "0009950000"                      ! Fam,Mult,Cant CERO
             FIL2$ = PACK$(FIL2$)                      ! User Data
             LNK$  = PACK$("0000")                     ! Link para Desc.Misc
             GR.DAT2$ = STRING$(31,"0")                ! Datos adicionales GR-OVS 9 Abr 2010
             DESC$ = "Desc/Espec."+LEOM$               ! Descrip Oferta
             LEC$ = "C6 C4 C2 2C5 C2 C18 C4 C31"       ! Formato Grabac
             WRITE FORM LEC$;#4;KEY.OF$,FIL1$,DEPT$,  \! Graba EAMITEMR
                   FIL2$,PRIC$,LNK$,DESC$,FIL3$,      \!
                   GR.DAT2$
          ENDIF                                        !
       ENDIF ELSE BEGIN                                !
          DES.NUE% = VTA.OF% + DES.99% - VTA.TO%       ! Calcula Nuevo Descto
!             print "Antes: Des.nue=";DES.NUE%;"  VtaOf=";VTA.OF%;"  Des99=";DES.99%;"  VtaTo=";VTA.TO%
          VAR=FLOAT(DES.NUE%*100)/(VTA.OF%+DES.99%)    !
          VTA.OF% = VTA.OF% - VTA.OF% * VAR / 100      !
          DES.99% = VTA.TO% - VTA.OF%                  !
          VALOF$=PACK$(RIGHT$(STRING$(12,"0")    +    \! Reemp Vr Vta por 
                 STR$(VTA.OF%),12))                    ! VrOferta

!         print VAR
!         print "Desp: Des.nue=";DES.NUE%;"  VtaOf=";VTA.OF%;"  Des99=";DES.99%;"  VtaTo=";VTA.TO%
!         print "Llave="; UNPACK$(KEY.MER$);"  Ind012=";UNPACK$(IND.012$)
!         print "Indof=";IND1OF%;"  Otros=";UNPACK$(INDIS$);" Valor=";UNPACK$(VALOF$)
!         print "Liga=";UNPACK$(LIG99$);" ";NOMOFE$;"  User=";UNPACK$(CUSER$)

          LIG99$ = PACK$("0099")                       !
          WRITE FORM "C6 C2 I1 C7 C6 C2 C18 C4 C31";#4;\! Graba RegInic Oferta 
                KEY.MER$,IND.012$,                    \!
                IND1OF%,INDIS$,VALOF$,LIG99$,         \!
                NOMOFE$,CUSER$, GR.DATA$               ! 
          KEY.OF$=RIGHT$("0000"+STR$(OFE%),4)          ! arma llave Descu
          KEY.OF$=PACK$("999999"+KEY.OF$+"99")         ! y GRABA Descto
          FIL1$ = PACK$("00020050")                    ! Indicat Miscel.Refu
          DEPT$ = PACK$("0000")                        ! Sin Dpto
          FIL2$ = "0000000000"                         ! Fam,Mult,Cant CERO
          FIL3$ = PACK$("00000000")                    ! Fam,Mult,Cant CERO
          PRIC$ = PACK$(RIGHT$(FIL2$+STR$(DES.99%),10))! Valor DESCUENTO
          FIL2$ = "0009950000"                         ! Fam,Mult,Cant CERO
          FIL2$ = PACK$(FIL2$)                         ! User Data
          LNK$  = PACK$("0000")                        ! Link para Desc.Misc
          DESC$ = "Desc/Espec."+LEOM$                  ! Descrip Oferta
          GR.DAT2$ = STRING$(31,"0")                ! Datos adicionales GR-OVS 9 Abr 2010          
          LEC$ = "C6 C4 C2 2C5 C2 C18 C4 C31"              ! Formato Grabac
          WRITE FORM LEC$;#4;KEY.OF$,FIL1$,DEPT$,     \! Graba EAMITEMR
                FIL2$,PRIC$,LNK$,DESC$,FIL3$,         \!
                GR.DAT2$
   SALEOF:                                             !
          PARAM2$ = " Of:"+STR$(OFE%)+" Vr:"+         \!
                    STR$(VTA.OF%)+" N/Vr: " +         \!
                    STR$(VTA.TO%)+" Dsc: " +          \!
                    STR$(DES.99%)                      ! Calcula Neto Oferta
          CALL ADXSERVE(RETU%,CFUN%,PARAM1,PARAM2$)    !
!          WAIT ; 1500                                  !
       ENDIF                                           !
    ENDIF                                              !
NEXT OFE%                                              !

END SUB                                                !
!------------------------------------------------------------------------------
!                         FIN DE RUTINAS COMUNES 
!------------------------------------------------------------------------------
SUB GUARDA.FLAG                                        !
       EOF$ = "  "                                     !
       FO$="C6 3I1 C1 C2 C3 2C1 C5 C2 C18 2I2"         !
       READ FORM FO$;#1 KEY LLAVE$;                   \! Lee Arch EAMITEMR
            KEY$,INDICAT0,INDICAT1,                   \! Llave, Indicativos
            INDICAT1A,INDICAT2$,DEPARTME$,            \! Depto
            FAMILYN$,MPGROUP$,SALEQUAN$,              \!  
            SALEPRIC$,LINKEDTO$,ITEMNAME$,            \!
            UEX1,UEX2                                  !
END SUB 
! ---------------- Rutina de Impresion de errores y Totales -------------------

SUB REPORTER                                           !
   IF CON.LIN% > 56 THEN BEGIN                         !
      WRITE FORM "C2 C2";#6;CHR$(12),FR$               !
      SWREP = 0                                        !
      CON.LIN% = 7                                     !
   ENDIF                                               !
   IF SWREP = 0 THEN BEGIN                             !
      SWREP = 1                                        !
      WRITE FORM "C2 C2";#6;" ",FR$                    !
      WRITE FORM "C80 C2";#6;                         \!
      "   PROCESO DE MANTENIMIENTO DE PRECIOS"   +    \!
      " DESDE EL SISTEMA CENTRAL AL POS",FR$           !
      WRITE FORM "C80 C2";#6;                         \!
      "            SUPERMERCADO  No : "+ALM$     +    \!
      "  " + NOAL$,FR$                                 !
      WRITE FORM "C80 C2";#6;                         \!
      "  ----------------------------------------"+   \!
      "----------------------------------",FR$         !
      WRITE FORM "C80 C2";#6;TIT1$,FR$                 !
      WRITE FORM "C80 C2";#6;                         \!
      "  ----------------------------------------"+   \!
      "----------------------------------",FR$         !
   ENDIF                                               !
   IF SWERROR = 1 THEN BEGIN                           !
      CODI$  = RIGHT$("      0" + COD$ ,7)             !
      WRITE FORM                                      \!
            "C37 C6 PIC(##) PIC(#####,###) C40 C2";   \!
      #6;"  "+TIP$+" "+CODI$+"  "+DES$+"  "+DEPW$,    \!
      RIGHT$(DPLIN$,5),VAL(UBT$),VAL(PRE$),"  " +     \!
      MENS$+NOM$,FR$                                   !
      CON.LIN% = CON.LIN% + 1                          !
      NOM$ = ""  :   MENS$ = ""                        !
   ENDIF                                               !
END SUB                                                !
!------------------------------------------------------------------------------
!                   Rutina de Reporte de Errores de Dise¤o
!------------------------------------------------------------------------------
SUB ERRDIS                                             !
    CALL REPORTER                                      ! 
    CON.LIN% = CON.LIN% + 1                            !
    TOTCERO = TOTCERO + 1                              ! Acum Cont Errores
    WRITE FORM "C65 C40 C2";#6;"Reg="+                \!
          LEFT$(STR$(TOTLEI)+"        ",8)+           \!
          REGI$,MENS$,FR$                              !
    TIP$ = "99"                                        !
    SWERROR = 0                                        !
    NOM$ = ""  :   MENS$ = ""                          !
END SUB                                                !
!                    Asigna Sufijo de Archivo Recibido
!----------------------------------------------------------------------------
!          Rutina de buscar Tarifa IVA con el equivalente INDICAT1
!------------------------------------------------------------------------------

SUB FIS.BUSTARIFA PUBLIC                               ! Shifting de Vr I2
   IF (FIS.FIV% AND 01H) THEN                         \! para obtener bits
      FIS.A$ = "1" ELSE FIS.A$ = "0"                   ! en 1 ¢ 0 de las 
   FOR II% = 1 TO 7                                    ! Tarifas de IVA
       FIS.FIV% = SHIFT(FIS.FIV%,1)                    ! Inicializa sig. BIT
       IF (FIS.FIV% AND 01H) THEN FIS.A$="1"+FIS.A$   \!
           ELSE FIS.A$ ="0"+FIS.A$                     !
   NEXT II%                                            !
END SUB                                                !

SUB GRABAR.DDM2                                        !
   FO$="C6 C1 C6 3I1 C1 C2 C3 2C1 C5 C2 C18 2I2 C2"    !
   DMCRCSFW = DMCRCSFW + 1                             ! Incrementa Contador
   WRITE FORM FO$;#11;                                \! Graba Batch EAMDDnnn
         KEY$,TIP$,KEY$,INDICAT0,INDICAT1,            \!
         INDICAT1A,INDICAT2$,DEPARTME$,               \!
         FAMILYN$,MPGROUP$,SALEQUAN$,                 \!
         SALEPRIC$,LINKEDTO$,ITEMNAME$,               \!
         UEX1,UEX2,  FR$                               !
       FO$="C6 3I1 C1 C2 C3 C1 C6 C2 C18 2I2"          !
       WRITE FORM FO$;#1;                             \! Graba EAN en EAMITEMR
             KEY$,INDICAT0,INDICAT1,                  \!
             INDICAT1A,INDICAT2$,DEPARTME$,           \!
             FAMILYN$,MPGROUP$,QUANPRIC$,             \!
             LINKEDTO$,ITEMNAME$,UEX1,UEX2             !
   IF EOF$ <> "  " THEN CREATE POSFILE A4$ AS 6        ! Crea Arch Txt Error
END SUB                                                !
!------------------------------------------------------------------------------
!                        Programa Principal
!------------------------------------------------------------------------------
CFUN% = 26                                             !
ON ERROR GOTO RUT.ERROR                                !

WHILE NOT VAL(A$)                                      !
   OPEN "EAMITEMR" KEYED RECL 77 AS 4 BUFF 10          !
   A1$ = LEFT$(TIME$,2) + ":" + MID$(TIME$,3,2) +     \! 
         ":" + RIGHT$(TIME$,2)                         !
   PARAM2$ = " Inicia Verif/Precio Ofer/Mercados.."+A1$!
   CALL ADXSERVE(RETU%,CFUN%,PARAM1,PARAM2$)           !
   WAIT ; 1000                                         !
   PARAM2$ = " Inicia Validaci¢n Ofertas.. " + A1$     !
   CALL ADXSERVE(RETU%,CFUN%,PARAM1,PARAM2$)           !
   CALL VERIFICA.OFERTA                                !
   WAIT ; 5000 
    !900000                                       !
    
   CLOSE 4                                             !
   Stop 
WEND                                                   !
Stop 
RUT.ERROR:                                             !
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
    EOF$ = "**"                                        !
    IF ERRF% = 4 AND ERR = "EF" THEN RESUME            !
!-----------------------------------------------------------------------------
    PARAM2$="Err P2 "+"ER="+ERR+" FN="+               \!
            STR$(ERRF%)+" ERN="+ERRFX$+" "+UNPACK$(KEY$)
    CALL ADXSERVE(RETU%,CFUN%,PARAM1,PARAM2$)          !
    STOP                                               !
!------------------------------------------------------------------------------

!          ********************************************
!       ***       FIN  DEL  PROGRAMA  PRINCIPAL        ***
!          ********************************************
