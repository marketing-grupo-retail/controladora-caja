!             PROGRAMA CFCREBA.BAS
!           PROYECTO *** COMFANDI Fase 2 ***
!----------------------------------------------------------
!  Este programa  crea  sobre un archivo  vac¡o C:\EAMITE.DAT
!  (que tiene las mismas  caracter¡sticas  del EAMITEMR.DAT),
!  la totalidad  de los art¡culos que se  van  a mercadear en
!  los diferentes puntos de Venta.
!
!  El programa realiza lo siguiente:
!
!  1.- S¢lo se seleccionan aquellos art¡culos que pertenezcan
!      al C¢digo del Almac‚n del archivo ADXALMA.DAT
!
!  2.- Se  ha  implementado  un nuevo Registro  de Datos para
!      incluir la Creacion/Modific de Codigos de Barras
! 
!  3.- Se  ha inclu¡do un control extra para verificar el No.
!      de departamento  que  trae la  novedad, para evitar la
!      contabilizaci¢n  errada  de  las  Ventas en el Informe
!      por departamentos.
! 
!------------------------------------------------------------
! DEBE COMPILARSE COAS.BAT
! Ambiente de Controlador
  %ENVIRON C 

! Definicion de variables de usuario Globales
  INTEGER*1     GLOBAL VALID                              ! Variable externa

! Definicion de variables de usuario locales
  STRING        BATCH$,    SEQUENCE$,    COUNTER$,       \!
                INDICAT2$, FEC$,         ALM$,           \!
                ALMA$,     NUAL$,        NOAL$,          \!
                REGI$,     DEP$,         HORA$,          \!
                QUANPRIC$, OPCION$

  INTEGER*1     DMCREQ,    DMCMT,        DMCPO,          \!
                INDICAT0,  INDICAT1,     DMCESTAT,       \!
                SW.FIN,    SWREP,        INDICAT1A,      \!
                SWERROR,   CON.LIN%

  INTEGER*2     DMCRCSF,   DMCSTSF,      DMCERRCT,       \!
                UEX1,      UEX2,         RETORNO%

  INTEGER*4     DMCERROF,  TOTBUE,       TOTCERO,        \!
                TOTLEI,    PEPRREQ%,     PREREQ%,        \!
                PLUPRE%,   SINDES%,      ICON%,          \!
                IV00%,     IV15%,        IV20%,     IV35%,\
                BUENO,     TOTMAL,       TOTBARM  
! ------------- Rutina de Impresion de errores y Totales -------------
FUNCTION ADXSTART (NAME$,PARM$,MESS$) EXTERNAL
INTEGER *2 ADXSTART
STRING NAME$,PARM$,MESS$
END FUNCTION

SUB REPORTER
   LPRINTER
   IF CON.LIN% > 56 THEN BEGIN
      PRINT CHR$(12)
      SWREP = 0
      CON.LIN% = 7
   ENDIF
   IF SWREP = 0 THEN BEGIN
      SWREP = 1
      PRINT " "
      PRINT "   PROCESO DE MANTENIMIENTO DE PRECIOS DESDE EL SISTEMA CENTRAL AL POS"
      PRINT "            SUPERMERCADO  No : ";ALM$; "  ";NOAL$
      PRINT "  --------------------------------------------------------------------"
      PRINT "  "
      PRINT "  "
      PRINT "    CODIGO     DESCRIPCION      DEPTO   VR/VENTA    OBSERVACIONES "
      PRINT "  --------------------------------------------------------------------"
   ENDIF 
   IF SWERROR = 1 THEN BEGIN
       CODI$  = RIGHT$("      0" + COD$ ,7)
       PRINT "  ";TIP$;" ";CODI$;"  ";DES$;"  ";DEP$;
       PRINT USING FORMA$;VAL(PRE$);
       PRINT "    ";MENS$+NOM$
       CON.LIN% = CON.LIN% + 1
   ENDIF
END SUB

! ------------- Rutina de ERROR de Inicio  por falta de CONTROL -------------

SUB NOINICIA
   CONSOLE
   CLEARS
   PRINT " "
   PRINT "                 Se Suspende en el Registro: ";TOTLEI       !
   PRINT " "
   PRINT "                 --------------------------------------------"
   PRINT "                |  No  se  puede  CONTINUAR  el proceso por  |"
   PRINT "                |  falta del registro de CONTROL de Fecha ¢  |"
   PRINT "                |  porque el LOTE NO termina con el registro |"
   PRINT "                |  de control de 99999999999's               |"
   PRINT "                |                                            |"
   PRINT "                |  Verifique el Archivo EAMNVPL.DAT  en C:   |"
   PRINT "                |  e incluya la informaci¢n del CONTROL en   |"
   PRINT "                |  el FORMATO   rAAAAMMDDHHMMnnnn , donde :  |"
   PRINT "                |                                            |"
   PRINT "                |  Ejemplo:                                  |"
   PRINT "                |        r = 4        (Tipo Control Fecha)   |"
   PRINT "                |                                            |"
   PRINT "                | AAAAMMDD = 19940722 (Fecha de los Datos)   |"
   PRINT "                |                        Para Jul. 22/94     |"
   PRINT "                |     HHMM = 1500     (Hora 3:00 PM)         |"
   PRINT "                |     nnnn = 0011     (No. del Almac‚n)      |"
   PRINT "                |                                            |"
   PRINT "                |                 Digite INTRO por Favor..!  |"
   INPUT "                 --------------------------------------------";NOM$
END SUB

!  -------- Rutina para crear Registros del BATCH de actualizacion ---------

SUB LLENAREG
    IF END #25 THEN SALIR                   ! Cuando se terminan Datos
    READ #25;LINE REGI$                     ! Lee Registro
    TOTLEI = TOTLEI + 1                     ! Suma en Contador
    CONSOLE                                 ! Asigna Consola
    LOCATE 18,25                            ! Coloca Cursor
    PRINT "Registros Le¡dos: ";TOTLEI       ! Despliega Contad
    TIP$ = LEFT$(REGI$,1)                   ! Toma Tipo Reg 1, 2, 3, o 4
    IF TIP$    = "4" THEN BEGIN             ! Si hay Fecha de Control
       IF PRIMO = 0  THEN BEGIN             ! y es 1er Lectura
          PRIMO = 1                         ! Marca Inicio OK
          FEC$  = MID$(REGI$,4,6)           ! Guarda Fecha
          HORA$ = MID$(REGI$,10,4)          ! Hora del Batch
          ALM$  = MID$(REGI$,10,4)          ! Guarda Almacen
          ALM$  = STR$(VAL(ALM$))           ! Guarda Almacen
          UEX2  = VAL(MID$(REGI$,6,4) +    \! Toma Mes y Dia
                  MID$(REGI$,5,1))          ! y Ult. Digito del Ano
          SWERROR = 1                       ! Indica By-Pass en Grabacion
          EXIT SUB                          ! Termina Sub-Rutina
       ENDIF ELSE BEGIN                     ! Fin 1er Lect
          PRIMO = 0                         ! Resest de Lectura
       ENDIF                                ! Fin de NO es 1er Lect
    ENDIF                                   ! Fin no es reg Ctrl
    IF NUAL$ <> ALM$  THEN EXIT SUB         ! Omite Reg de Otro Almacen
    IF TIP$ = "9" THEN EXIT SUB             ! Se omite Control
    IF PRIMO = 0 THEN BEGIN                 ! Si NO hubo Reg Control
       CALL NOINICIA                        ! Notifica Error
       STOP                                 ! PARA o TERMINA
    ENDIF                                   ! Fin
    COD$ = STR$(VAL(MID$(REGI$,2,7)))       ! Codigo Articulo o PLU
    DES$ = MID$(REGI$,9,18)                 ! Descripcion del Articulo
    IF LEFT$(DES$,1) = "." THEN            \! Sin Descripcion
       SINDES% = SINDES% + 1                ! Suma Errores
    DEP$ = MID$(REGI$,27,3)                 ! Codigo de Seccion o Depto
    IF DEP$ = "003" THEN DEP$ = "016"       ! Cambia Dep Fruver SIGED
    PRE$ = STR$(VAL(MID$(REGI$,30,11)))     ! Prec/Entero de Venta + Iva
    UVT$ = MID$(REGI$,43,3)                 ! Contenido o Unidades de Vta
    TDA$ = MID$(REGI$,46,1)                 ! Tipo Artic 0=Norm;    1=Grs;
                                            !            2=ReqPrec; 3=Cupon;
                                            !            4=Consec     
    BAR$ = MID$(REGI$,47,12)                ! Codigo de Barras del PLU 
    LOCATE 19,25                            ! Coloca Cursor
    PRINT "Long. = ";STR$(LEN(REGI$))
    IVA$ = "00" :  ICO$ = "0000000"         ! Restaura Inf Iva Imp/Cons
    IF LEN(REGI$) > 60 THEN BEGIN           ! Si es nuevo Diseno
       IVA$ = MID$(REGI$,60,2)              ! Toma Iva
       ICO$ = MID$(REGI$,62,7)              ! Toma Impo Consumo
    ENDIF                                   !
    MEP$ = "0"                              ! Metodo de Precio
    VRE$ = "0"                              ! Prec.de Venta Reducido + Iva
    PRQ$ = "0"                              ! Precio Requerido
    PPR$ = "0"                              ! Peso/Precio Requerido
    IF TDA$ = "1" THEN PPR$ = "1"           ! Peso/Precio Requerido
    IF TDA$ = "2" THEN PRQ$ = "1"           ! Precio Requerido
    IF TDA$ = "3" THEN PRQ$ = "1"           ! Cupon de Fabric
    IF TDA$ = "4" THEN PRQ$ = "1"           ! Cupon de almacen
    CNP$ = "0"                              ! Cantidad NO Permitida
    CRQ$ = "0"                              ! Cantidad Requerida
    ADE$ = "0"                              ! Artic de Excep
    AAV$ = "0"                              ! Autoriz para Venta
    ARV$ = "0"                              ! Rechaz en la Venta
    GMA$ = "0"                              ! Grupo Multiprec Artic
    ADS$ = "1"                              ! Aplicable Descuento a PLU
    POE$ = "0"                              ! Pago Efect Obligatorio o Fs
    IF TDA$ = "1" OR UVT$ = "  " THEN      \! Si NO trae Indic. de Vta Und
       UVT$ = "0"                          \! Venta Unitaria PLU
    ELSE                                   \! de lo Contrario
       UVT$ = STR$(VAL(UVT$))               ! Toma el Contenido que viene 
    SWERROR =  1                            ! Inicia Validacion
    LLAVE$ = PACK$("000099999" + DEP$)      ! Arma KEY Plu Dptal
    NOM$ = "  "                             ! Reset Nombre
    READ FORM "C46"; #1 KEY LLAVE$;REGI$    ! Lectura de EAMITEMR
    MENS$ = ""                              ! Restaura Mensaje Error
    IF NOM$ = "**" THEN BEGIN               ! Si no leyo Depto
       MENS$ = "*DEPTO*  "                  ! Coloca Mensaje
       NOM$ = "*DPTO SIN DEFINIR*"          ! No esta en Store Opts?
    ENDIF ELSE BEGIN                        ! Si hubo Lectura
       SWERROR = 0                          ! Pasan todos OK
    ENDIF                                   ! Termina Validac de Depto
    IF TIP$ = "2" AND                      \! Si es Modific
       VAL(PRE$) = 0 THEN BEGIN             ! y trae Valor CERO
          LLAVE$ = "000000000000"           ! Arma llave
          LLAVE$ = RIGHT$(LLAVE$+COD$,12)   ! de 12 acract y
          LLAVE$ = PACK$(LLAVE$)            ! empaquete KEY Plu 
          NOM$ = "  "                       ! Borra Nombre
          READ FORM "C46";                 \! Lee el arch de Artic
               #1 KEY LLAVE$;REGI$          ! EAMITEMR
          MENS$ = ""                        ! Restaura Mensaje Error
          IF NOM$ = "**" THEN BEGIN         ! Si no leyo PLU
             MENS$ = "*MOD*  "              ! Coloca Mensaje
             NOM$ = "*NO EXISTE*"           ! No esta en Store Opts?
             SWERROR = 1                    ! Marca ERROR
          ENDIF ELSE BEGIN                  ! Si esta en Arch
             PRE$=UNPACK$(MID$(REGI$,18,5)) ! restaura valor Vta
             SWERROR = 0                    ! Cuando pasan todos OK
          ENDIF                             ! Termina Validac de Depto
    ENDIF                                   ! Termina Validac de Depto

    IF LEN(COD$) =  0  OR                  \! Si no hay PLU o
       PRE$      = "0" OR                  \! Si no hay PLU o
       COD$ = "     " THEN BEGIN            ! viene en espacios
          MENS$ = MENS$ + "*VR/VTA* "       ! Coloca Mensaje
          SWERROR = 1                       ! Marca ERROR
    ENDIF                                   ! Termina Validac de PLU
    IF SWERROR = 1 THEN BEGIN               ! Si hay errores
       TOTCERO = TOTCERO + 1                ! Acumula en Contador Errores
       CALL REPORTER                        ! Efectua Reporte
       EXIT SUB                             ! y se omite Grabacion
    ENDIF                                   !
    KEY$=RIGHT$(PACK$(STRING$(12,"0")+     \! Arma Llave en formato PD
         COD$),6)                           ! Traduccion de COD$ a llave KEY$
    IF TIP$ = "3" THEN EXIT SUB             ! Los borrados no se llena el Reg


! ----------- Llenar el registro para los agregados y actualizados -------
    INDICAT0  = 0                                   ! Grabar Venta/Autoriz Vta
    INDICAT1  = 0                                   ! Plan Iva/Desc/Fct Cupon NO
    INDICAT1A = 0                                   ! Puede hacer Etiqueta


    IF IVA$ = "16" THEN BEGIN                       !
       INDICAT1 = 80H                               ! Si hay Plan Iva A
       IV15% = IV15% +1                             ! Acumula Contad
    ENDIF                                           !
    IF IVA$ = "20" THEN BEGIN                       !
       INDICAT1 = 40H                               ! Si hay Plan Iva B
       IV20% = IV20% +1                             ! Acumula Contad
    ENDIF                                           !
    IF IVA$ = "35" THEN BEGIN                       !
       INDICAT1 = 20H                               ! Si hay Plan Iva C
       IV35% = IV35% +1                             ! Acumula Contad
    ENDIF                                           !
    IF IVA$ = "10" THEN BEGIN                       !
       INDICAT1 = 10H                               ! Si hay Plan Iva C
       IV10% = IV10% +1                             ! Acumula Contad
    ENDIF                                           !
    IF IVA$ = "00" THEN BEGIN                       ! *** DISPONIBLE *****
!      INDICAT1 = 10H                               ! No hay Plan Iva D
       IV00% = IV00% +1                             ! Acumula Contad
    ENDIF                                           !
    UEX1 = 0                                        ! Impuesto Consumo
    IF VAL(ICO$) > 0 THEN BEGIN                     !
       UEX1 = VAL(ICO$)
       ICON% = ICON% +1                             ! Acumula Contad
     ENDIF   
!   IF VAL(COD$) = 1 THEN INDICAT1A = 02H
    IF VAL(UVT$) > 1 AND VAL(UVT$) < 99  THEN  INDICAT0 = 20H              ! Canti > 1 requiere canti.
    IF PPR$ = "1" THEN  INDICAT0 = INDICAT0 OR  40H ! Peso/Precio Requerido
    IF PRQ$ = "1" THEN  INDICAT0 = INDICAT0 OR  10H ! Precio Requerido
    IF PPR$ = "1" THEN  PEPRREQ% = PEPRREQ% + 1     ! Suma Peso/Precio Req
    IF PRQ$ = "1" THEN  PREREQ%  = PREREQ%  + 1     ! Suma Precio Requerido
    IF TDA$ = "0" THEN  PLUPRE%  = PLUPRE%  + 1     ! Suma Plu por Precio

!    IF ADS$ = "1" AND POE$="1" THEN INDICAT1 = 08H  ! Si hay Descto y Pago Oblig
!    IF ADS$ = "0" AND POE$="1" THEN INDICAT1 = 10H  ! Si no Hay Descto y SI Pag Ob
!    IF DEP$ = "021"  THEN INDICAT1 = 10H             ! Si hay Descto y NO Pag Oblig
!    IF ADS$ = "0" AND POE$="0" THEN INDICAT1 = 02H  ! Si no hay ninguno de los dos

    IF VAL(TDA$) < 3 THEN TDA$ = "0"                ! Articulos Normales
    INDICAT2$ = PACK$(TDA$+MEP$)                    ! Item Vta Norm/met.prec=0
    DEPARTME$ = RIGHT$(PACK$("0000"+DEP$),2)        ! Numero departamento
    FAMILYN$  = PACK$("000000")                     ! Familia cupones(Cur/Prev)
    MPGROUP$  = PACK$("00")                         ! Precio en grupo
    SALEQUAN$ = PACK$(UVT$)                         ! Cantidad de venta
    SALEPRIC$ = RIGHT$(PACK$("0000000000"+PRE$),5)  ! Precio de venta
    LINKEDTO$ = PACK$("0000")                       ! Articulo enlazado
    ITEMNAME$ = LEFT$(DES$+STRING$(18," "),18)      ! Descripcion articulo
  EXIT SUB
  SALIR:
    SW.FIN = 1
END SUB

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
      FRA.LIN$ =FRA.H$+":"+FRA.M$+":"+FRA.S$     + \! Arma la Linea
               "    El: "+FRA.DD$+"/"+FRA.MM$+"/"+ \! de Franqueo con
               FRA.AA$                              ! Hora y Fecha
  END SUB
!  -------------  Rutina para grabar en el archivo EAMD:nnn ---------------

SUB GRABAREG
    IF TIP$ = "4" THEN EXIT SUB                  ! Se omite grabar por Fecha
    IF TIP$ = "9" THEN BEGIN                     ! Se omite grabar por Control
       PRIMO = 0                                 ! Restaura 1er lectura
       EXIT SUB                                  ! Y regresa
    ENDIF                                        !
    IF NUAL$ <> ALM$  THEN EXIT SUB              ! Omite Reg de Otro Almacen
    IF SWERROR = 1 THEN BEGIN                    ! Si hubo error en Datos
         SWERROR = 0                             ! Se restaura condicion
         EXIT SUB                                ! y se omite Grabacion
    ENDIF                                        ! Fin de error
    FINR$ = CHR$(13)+CHR$(10)                    ! Marca de fin de registro
    DMCRCSF=DMCRCSF + 1                          ! Incrementa Contador
    IF TIP$ = "3" THEN BEGIN                     ! Si es registro a eliminar
       DELREC 1;KEY$                             ! Borra Reg
       TOTMAL = TOTMAL + 1
    ENDIF                                        !
    IF TIP$ = "3" AND                           \! Si es registro a eliminar
       VAL(BAR$) > 0 THEN BEGIN                  ! y si hay codigo de Barras
       KEY$=RIGHT$(PACK$("000000000000"+BAR$),6) ! Arma Key Barras en form PD
       DELREC 1;KEY$                             ! Borra Reg
       TOTBARM = TOTBARM + 1                       !
    ENDIF                                        !
    IF VAL(TIP$) < 3 THEN BEGIN                  ! Si es registro 1 o 2
       IF OPCION$ = "1" THEN BEGIN               ! Actualiza PLU's y COD/BAR
          !IF LEFT$(BAR$,8)="88888888" THEN      \!
           ! SALEPRIC$=PACK$("0000000000")       !
          WRITE FORM "C6 3I1 C1 C2 C3 2C1 C5 C2 C18 2I2";#1;   \
          KEY$,INDICAT0,INDICAT1,               \!
          INDICAT1A,INDICAT2$,DEPARTME$,        \!
          FAMILYN$,MPGROUP$,SALEQUAN$,          \!
          SALEPRIC$,LINKEDTO$,ITEMNAME$,        \!
          UEX1,UEX2                              !
       ENDIF                                     !
       IF VAL(BAR$) > 0 THEN BEGIN               ! y si hay EAN/UPC
          KEY$=RIGHT$(PACK$("000000000000"+     \!
               BAR$),6)                          ! Arma Key Barras PD
          INDICAT2$=PACK$(TDA$+"5")              ! Item Vta Norm/MP=5
          INDICAT0 =01H                          !
          QUANPRIC$=RIGHT$(PACK$("000000000000" \!
                    + COD$),6)                   ! Arma ALIAS en Form PD
          TIP$ = "1"                             ! CREA reg nuevo de PM=5
          WRITE FORM "C6 3I1 C1 C2 C3 C1 C6 C2 C18 2I2";#1;\
             KEY$,INDICAT0,INDICAT1,            \!
             INDICAT1A,INDICAT2$,DEPARTME$,     \!
             FAMILYN$,MPGROUP$,QUANPRIC$,       \!
             LINKEDTO$,ITEMNAME$,UEX1,UEX2       !
             TOTBAR = TOTBAR + 1                 !
       ENDIF                                     !
       IF LEFT$(BAR$,8)="88888888" THEN BEGIN    ! ES OFERTA
          KEY$=PACK$("99999999")+RIGHT$(KEY$,2)  ! ARMA PLU OFERTA
          INDICAT2$ = PACK$(TDA$+MEP$)           ! Item Vta Norm/met.prec=0
          DEPARTME$ = PACK$("995")               !
          SALEQUAN$ = PACK$("00")                !  
          INDICAT1A  = 01H                       ! Plan Iva/Desc/Fct Cupon NO
          WRITE FORM "C6 3I1 C1 C2 C3 2C1 C5 C2 C18 2I2";#1;\
          KEY$,INDICAT0,INDICAT1,               \!
          INDICAT1A,INDICAT2$,DEPARTME$,        \!
          FAMILYN$,MPGROUP$,SALEQUAN$,          \!
          SALEPRIC$,LINKEDTO$,ITEMNAME$,        \!
          UEX1,UEX2                              !
       ENDIF                                     !
    ENDIF                                        !
  P2:                                            !
    TOTBUE = TOTBUE + 1                          !
END SUB

!  --------------   Rutina final de cierre del archivo   ----------------

  SUB FINAL
    SW.FIN = 1
    CLOSE 1
  END SUB

!----------------------  PROGRAMA  PRINCIPAL  ----------------------------
   FORMA$ = "###,###,###"
   ON ERROR GOTO ERRTRAP                       ! Rutina de Salida ERROR
   CALL CREAFECHA                              ! Busqueda de Fecha
   FEC1$ = FRA.LIN$                            ! Guarda Fecha
   OPCION$ = COMMAND$                          ! Opcion de corrida Progr
   IF OPCION$ = "" THEN OPCION$ = "1"          ! Valor por defecto = 1
   CON.LIN% = 60                               ! Cont de lineas
   OPEN "EAMITEMR" KEYED RECL 46 AS 1          ! Archivo de Art¡culos
   OPEN "C:\EAMNVPL.DAT"AS 25 BUFFSIZE 80      ! Archivo de Novedades
   OPEN "C:\ADXALMA.DAT" AS 26 BUFFSIZE 80     ! Archivo Control Almacen
   READ #26;LINE ALMA$                         ! Lee Control Almac
   NUAL$ = STR$(VAL(LEFT$(ALMA$, 4)))          ! Guarda Num Almacen
   NOAL$ = RIGHT$(ALMA$,LEN(ALMA$)-12)         ! Guarda Nombre
   FECHA$ = MID$(ALMA$,9,4)
   IF LEN(NOAL$) < 35 THEN BEGIN               ! Si es letrero Corto
      NOAL$=NOAL$+STRING$(35-LEN(NOAL$)," ")   ! Le agrega espacios
   ENDIF                                       ! Fin
   NOAL$ = LEFT$(NOAL$,35)                     ! Toma 35 Pos.de Nombre
   CLOSE 26                                    !
   CONSOLE
   CLEARS
   PRINT " "
   PRINT " "
   PRINT "         Almac‚n : ";NUAL$;"   ";NOAL$
   PRINT " "
   PRINT " "
   PRINT "                 ------------------------------------------"
   PRINT "                |                                          |"
   PRINT "                |  Proceso de conversi¢n de  EAMNVPL.DAT   |"
   PRINT "                |  para crear DIRECTAMENTE el Archivo de   |"
   PRINT "                |  Art¡culos EAMITEMR.DAT                  |"
   PRINT "                |                                          |"
   PRINT "                |                 Programa CFCREBA.286     |"
   PRINT "                |                                          |"
   PRINT "                |  Inicia a las: ";FEC1$;"  |"
   PRINT "                |                                          |"
   PRINT "                |               Digite INTRO por Favor..!  |"
   INPUT "                 ------------------------------------------";NOM$
   PRIMO   = 0                            ! Ctrl Lectura 1er Registro
   DMCRCSF = 0                            ! Numero de registros del archivo
   SW.FIN = 0                             ! Indica fin archivo del AS/400
   TOTCERO = 0                            ! Numero de registros ERRADOS
   WHILE SW.FIN = 0                       ! Ciclo para pasar la informacion
         CALL LLENAREG                    ! Pasa Datos de AS/400 a EAMD:nnn
         CALL GRABAREG                    ! Graba en el archivo EAMD:nnn
   WEND                                   ! Fin del ciclo
   CALL FINAL                             ! Fin operaciones y grabar 
                                          ! regsitro de control
   CALL CREAFECHA
   FEC2$ = FRA.LIN$
   FINR$ = CHR$(13)+CHR$(10)
   CREATE POSFILE "C:\ADX_UDT1\N"+ALM$+FECHA$+".DAT" AS 20 LOCAL 
       BUENO = TOTBUE+TOTBAR
       NUMRE$ =RIGHT$("00000000"+STR$(BUENO),8)
       AV$ =  "PROCESO DE MANTENIMIENTO DE PRECIOS" 
       INI$ = "INICIADO A LAS :    "
       COS$ = "CENTRO DE COSTO:    "
       TERM$ ="TERMINA A LAS  :    "
       REG$  ="REGISTROS GRAVADOS: "
       FECH$ ="FECHA DEL PROCESO : "
       BARS$ ="CODIGOS DE BARRA  : "
       NUBAR$ =RIGHT$("00000000"+STR$(TOTBAR),8)
       FECH1$ =RIGHT$(FEC1$,8)
       WRITE FORM "C35 C35 C2" ;#20;AV$,NOAL$,FINR$
       WRITE FORM "C21 C4 C2" ;#20;COS$,ALM$,FINR$
       WRITE FORM "C21 C8 C2" ;#20;INI$,FEC1$,FINR$
       WRITE FORM "C21 C8 C2" ;#20;TERM$,FEC2$,FINR$
       WRITE FORM "C21 C8 C2" ;#20;BARS$,NUBAR$,FINR$
       WRITE FORM "C21 C8 C2" ;#20;REG$,NUMRE$,FINR$
       WRITE FORM "C21 C8 C2" ;#20;FECH$,FECH1$,FINR$
              
   LPRINTER
   IF CON.LIN% > 40 THEN BEGIN
      PRINT CHR$(12)
      PRINT " "
      PRINT "   PROCESO DE MANTENIMIENTO DE PRECIOS DESDE EL SISTEMA CENTRAL AL POS"
      PRINT "            SUPERMERCADO  No : ";ALM$; "  ";NOAL$
      PRINT "  --------------------------------------------------------------------"
      PRINT " "  :   PRINT "  "
   ENDIF
      PRINT " "  :   PRINT " "
      PRINT "                   ITEMS SIN DESCRIPCION POS : ";
      PRINT USING FORMA$;SINDES%
      PRINT "                  ITEMS CON PRECIO REQUERIDO : ";
      PRINT USING FORMA$;PREREQ%
      PRINT "           ITEMS CON PESO Y PRECIO REQUERIDO : ";
      PRINT USING FORMA$;PEPRREQ%
      PRINT "           ITEMS CON PRECIO DESDE EL SISTEMA : ";
      PRINT USING FORMA$;PLUPRE%
      PRINT "    TOTAL REGISTROS LEIDOS DESDE EAMNVPL.DAT : ";
      PRINT USING FORMA$;TOTLEI
      PRINT "                   ARTICULOS CREADOS CON PLU : ";
      PRINT USING FORMA$;TOTBUE
      PRINT "           CODIGOS DE BARRAS CREADOS (ALIAS) : ";
      PRINT USING FORMA$;TOTBAR
      PRINT "                    TOTAL REGISTROS GRABADOS : ";
      PRINT USING FORMA$;TOTBUE+TOTBAR
      PRINT "                 TOTAL REGISTROS CON ERRORES : ";
      PRINT USING FORMA$;TOTCERO
      PRINT
      PRINT "                 TOTAL REGISTROS BORRADOS    : ";
      PRINT USING FORMA$;TOTMAL+TOTBARM
      PRINT
      PRINT "                       ARTICULOS CON IVA 16% : ";
      PRINT USING FORMA$;IV15% 
      PRINT "                       ARTICULOS CON IVA 10% : ";
      PRINT USING FORMA$;IV10% 
      PRINT "                       ARTICULOS CON IVA 20% : ";
      PRINT USING FORMA$;IV20% 
      PRINT "                       ARTICULOS CON IVA 35% : ";
      PRINT USING FORMA$;IV35% 
      PRINT "                       ARTICULOS SIN IVA     : ";
      PRINT USING FORMA$;IV00% 
      PRINT "                   ARTICULOS CON IMP/CONSUMO : ";
      PRINT USING FORMA$;ICON% 
  PRINT "      ----------------------------------------------------------- "
  PRINT "      ** INICIA  A LAS: ";FEC1$
  PRINT "                          ** ARCHIVO EAMNVPL.DAT"
  PRINT "      ** TERMINA A LAS: ";FEC2$;"       ALMACEN = ";ALM$
  PRINT "                        "; NOAL$
  PRINT "      ----------------------------------------------------------- "
   
   PROG$ = "ADX_UPGM:CFAJUOFE.286"
   BAR22$ = "  "
   BAR33$ = "INICIA TAREA AJUSTE OFERTAS  " + TIME$
   RETORNO% = ADXSTART(PROG$,BAR22$,BAR33$)
   
  !WAIT;5000

  
  STOP

ERRTRAP:
    HX% = ERRN
    ERRFX$=""
    FOR S% = 28 TO 0 STEP -4
        SX% = SHIFT(HX%,S%)
        SUM% = SX% AND 000FH
        IF SUM% > 9 THEN     \
           SUM% = SUM% + 55  \
        ELSE                 \
           SUM% = SUM% + 48
        Z$ = CHR$(SUM%)
        ERRFX$ = ERRFX$ + Z$
    NEXT S%
    IF ERRF% = 26 THEN BEGIN
       CLEARS
       PRINT
       PRINT
       PRINT "    ERROR..!   NO EXISTE EL ARCHIVO DE CONTROL DE ALMACEN..!"
       PRINT
       PRINT "               Consulte con el Depto de Sistemas."
       PRINT "                 Crear  C:\ADXALMA.DAT"
       STOP
    ENDIF
    IF ERRF% = 1 THEN BEGIN
       IF ERR = "EF" THEN BEGIN
          NOM$ = "**"
          RESUME
       ENDIF ELSE BEGIN
          PRINT "Error en Archivo de EAMITEMR.."
          PRINT "ERR = ";ERR," ERRL =";ERRL
          PRINT "ERRF = ";ERRF%," ERRN =";ERRFX$
          PRINT REGI$
       ENDIF
    ENDIF ELSE BEGIN
          PRINT "Error en Archivo de Novedades EAMNVPL.."
          PRINT "ERR = ";ERR," ERRL =";ERRL
          PRINT "ERRF = ";ERRF%," ERRN =";ERRFX$
          PRINT REGI$
    ENDIF 
END
! ********************************************
!***     FIN  DEL  PROGRAMA  PRINCIPAL      ***
! ********************************************
