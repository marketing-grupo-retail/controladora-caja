!                    PROGRAMA CFPAGOCR.BAS
!                   ** PROYECTO COMFANDI **
!----------------------------------------------------------------
! Programa para mantenimiento de los Archivos que intervienen en
! el Control de la Verificacion de los Medios de Pago.
!
!   Forma como operan los Codigos de Tipo de Registro: TIP$
!    -----------------------------------------------------
!         1 = Para Crear. Si ya existe, lo REEMPLAZA. 
!         2 = Para Modificar. Si NO existe lo CREA.
!         3 = Para BORRAR. Si no existe, tampoco hace NADA.
!    -----------------------------------------------------
!
! El programa permite registrar fechas de creacion y/o cambios en
! los datos. En los casos de RETIROS, tambien queda  identificada
! la condicion de RETIRADO y la fecha de la novedad.
!
!  El programa realiza lo siguiente:
!
!  1.- Crea ¢ Actualiza los siguientes Archivos en ADX_IDT1:
!      EMPRE.DAT, EAMTENDV.DAT, EAMCUSTA.DAT,  CUPOS.DAT
!      con base en un archivo secuencial creado en el AS/400
!      y transmitido al controlador via comunicaciones. 
!
!  2.- Adicionalmente  se  incluyen  los  siguientes STATUS
!      para informar por la terminal de Venta el estado del
!      Beneficiario y sus Cupos.
!
! Estados Normales :
!     Noved. Significado             STATUS   MENSAJE PANTALLA
!     ----- ------------------------ ------- ------------------------!
!      99    Factura Permitida         50   "RECLAME TIQUETE DE CUPO"
!      50    Empresa sin Prohibicion   50   "RECLAME TIQUETE DE CUPO"
!
! Estados Anormales:
!     Noved. Significado             STATUS   MENSAJE PANTALLA
!     ----- ------------------------ ------- ------------------------!
!      55    Retener Tarjeta Comfandi  55   "PAGO RECHAZADO   **RT**"
!      56    Tarjeta Comfandi Suspend  56   "PAGO RECHAZADO   **SU**"
!      57    Empresa con CR Cerrado    57   "PAGO RECHAZADO   **CR**"
!      58    Girador en Lista Negra    58   "PAGO RECHAZADO   **CH**"
!--------------------------------------------------------------------

! Ambiente de Controlador
 %ENVIRON C

 STRING      COD1$,     KEY$,     NOM$,      REGISTRO$, BATCH$,   \
             CODC$,     CERO$,    FECHA$,    FECHAPD$ , NOMD$,    \
             ERRFX$,    Z$,       LLAV1$,    REG$,      FINAL$,   \
             SIRAN$,    FECRE$,   FEMOD$,    FECRED$,   FEMODD$,  \
             NOVE$,     VARI$,    ERRORCOD$, SEQUENCE$, COUNTER$, \ 
             FEC$,      ALM$,     INDICAT2$, IND$,      MESES(1)

 INTEGER*1   DMCREQ,    CONLI,    DMCPO,     INDICAT0,  SWERROR,  \
             INDICAT1,  DMCESTAT, SW.FIN,    SWREP,     INDICAT1A

 INTEGER*2   DMCRCSF,   DMCSTSF,  DMCERRCT,  UEX1,      UEX2,     \
             NUMTRA%,   NUCO%,    MES,       PAGI%,     RECH%

 INTEGER*4   TOT1%,     \ ! Cantidad Total de Reg. Grab.
             TOT2%,     \ ! Cantidad de Reg. Leidos
             TOT3%,     \ ! Cantidad de Reg. con I/O Error
             CANERRA%,  \ ! Cantidad de Reg. Empl. Comfam
             CANCOMF%,  \ ! Cantidad de Reg. Empl. Comfam
             CANFONC%,  \ ! Cantidad de Reg. Empl. Foncaja
             CANEMPR%,  \ ! Cantidad de Reg. Otras Epresas
             CANESCO%,  \ ! Cantidad de Reg. Temporada Escolar
             CANSUBC%,  \ ! Cantidad de Reg. Subs/Comfam
             CANSUBE%,  \ ! Cantidad de Reg. Subs/Otras Emp
             ELIMICC%,  \ ! Cantidad de Reg. Sin Cedula
             ELIMINA%,  \ ! Cantidad de Reg. para Eliminar
             ELIMIOK%,  \ ! Cantidad de Reg. Borrados EAMCUS9.DAT
             I%,        \
             S%,        \
             REG%,      \
             SX%,       \
             HX%,       \
             SUM%,      \
             REGC%,     \
             RESTA%,    \ ! Campo de RESTART
             USERE%,    \ ! Para Acumular Compras (Vr.Diario)
             VRTRAN%,   \ ! Valor de las Compras Acumuladas
             COD1%,     \ ! Codigo Inicial o Cupo Parcial
             COD2%,     \ ! Codigo Final o Cupo Total
             COMP%,     \ ! Valor Compras Acumul
             DMCERROF,  \ !
             TOTBUE,    \ !
             TOTCERO,   \ !
             MAXTRU,    \ !
             MAXTRT,    \ !
             USREX

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
!  END SUB


!********************
REPO$="C:/ADX_UDT1/RPCR"+FRA.MD$+".TXT"
CREATE REPO$ AS 10 
OPEN REPO$ AS 11 APPEND
!PRINT "ESTE ES EL NUEVO ARCHIVO: *************** ",REPO$
!WAIT;5000
FINR$=CHR$(13)+CHR$(10)		!CREA UN FIN DE REGISTRO
!******************** 
   END SUB

! ------------- Rutina para Control del Archivo Recibido -------------

SUB ERRALM
      CLEARS
      PRINT : PRINT : PRINT : PRINT : PRINT : PRINT
      PRINT "             E R R O R .....!"
      PRINT
      PRINT
      PRINT "             Los datos contenidos en el Diskette  "
      PRINT "             le¡do NO CORRESPONDEN a ‚ste Almac‚n."
      PRINT
      PRINT "             El Proceso se SUSPENDE..!            "
      PRINT "             Verifique con el Depto de Cr‚dito ..."
      PRINT
      INPUT "                         Digite INTRO para Continuar..";REG$
END SUB

! ------------- Rutina de Impresion de Encabezado de Pagina ----------
SUB CABEZA 

   LPRINTER
   IF CONLI > 55 THEN BEGIN                            ! Si Hoja Full
      CONLI = 5                                        ! Inicia Cont Lin
      PAGI% = PAGI% + 1
      PRINT CHR$(12)                                   ! Salto de Pag
      PRINT " "                                        ! 
      PRINT "       MANTENIMIENTO DE FORMATOS DE CREDITO DESDE AS/400 AL POS        Pag. ";PAGI% 
      PRINT "        SUPERMERCADO COMFANDI No : ";ALM$;"  ";NOAL$ 
      PRINT "  "                                       ! 
      PRINT "TIP P/V/NOV      NIT     No.INICIAL  No. FINAL      OBSERVACIONES           EMPRESA" 
      PRINT "---------------------------------------------------------------------------------------"
   ENDIF                                               ! Fin Cond
END SUB

! ------------- Rutina de Impresion de errores y Totales -------------

SUB RESUMEN
   IF RECH% > 0 THEN BEGIN  
      CALL CABEZA
      CONLI = CONLI + 1                                ! Contad Lineas
      TOTCERO = TOTCERO + 1                            ! Acumula Errores
      PRINT "          ";                              ! Arma Linea con
      PRINT USING FORMA$;VAL(CEDA$);TAB(62);           ! NIT Empresa
      PRINT "Rango Elim.= ";                           !
      PRINT USING FORMAN$;RECH%                        ! Pago, Variedad
      RECH%    = 0                                     !
      OBSER$   = "               "                     ! Restaura Mens
      OBERROR$ = "           "                         ! Restaura Mens
   ENDIF                                               !
END SUB                                                ! Fin Subrutina

! ------------- Rutina de Impresion de errores y Totales -------------

SUB REPORTER
   CALL CABEZA
   CONLI = CONLI + 1                                   ! Contad Lineas
   TOTCERO = TOTCERO + 1                               ! Acumula Errores
   PRINT " ";TIP$;"   ";PAG$;VARI$;" ";NOVE$;          ! Arma Linea con
   PRINT USING FORMA$;VAL(CEDA$);                      ! NIT Empresa
   PRINT USING FORMAT$;COD1%;                          ! Pago, Variedad
   PRINT USING FORMAT$;COD2%;                          ! Rango, Observac
   PRINT "  ";OBSER$;OBERROR$;NOM$                     ! Empresa
   OBSER$   = "               "                        ! Restaura Mens
   OBERROR$ = "           "                            ! Restaura Mens
END SUB                                                ! Fin Subrutina

! -------  Rutina para Verificar Existencia de EMPRE1.DAT --------------

SUB VERIFICA
    ERRORCOD$ = "  "                                   ! Inicia Estado Leer
    KEY$ = PACK$(PAG$+VARI$+CED$)                      ! Arma llave para leer
    READ FORM "C8 C30 2C3 C1";#26 KEY KEY$;          \ ! Lee EMPRE1.DAT
         KEY$,NOMD$,FECRED$,FEMODD$,NOVED$             ! Nomb;F/Crea;F/Mod;Nov
    IF ERRORCOD$ = "  " THEN BEGIN                     ! Si existe Registro
       FECRE$ = FECRED$                                ! Guarda Fecha Creacion
    ENDIF ELSE BEGIN                                   ! Fin Cond
       OBSER$ = "Emp No Existe; "                      ! Error Empresa
    ENDIF                                              ! Fin Cond
END SUB

! -------------  Rutina para Crear/Modif EMPRE1.DAT --------------

SUB CREAEMPRE
   IF TIP$ = "2" OR                                  \ ! Si es Modificacion o
      TIP$ = "3" THEN BEGIN                            ! Si es para Retirar
      CALL VERIFICA                                    ! Verifica Empresa
   ENDIF                                               ! Fin Cond
   IF ERRORCOD$ <> "  " AND                          \ ! Si no Existe Emp
      TIP$ = "2"  THEN BEGIN                           ! y es Modificacion,
      OBERROR$ = "Se Crea    "                         ! 
      CALL REPORTER                                    ! Reporta Error
      TIP$ = "1"                                       ! entonces, Lo CREA 
   ENDIF                                               ! Fin Cond
   IF ERRORCOD$ <> "  " AND                          \ ! Si no Existe Emp
      TIP$ = "3"  THEN BEGIN                           ! y es Retiro
      OBERROR$ = "Se Elimina "                         ! 
      CALL REPORTER                                    ! Reporta Error
      EXIT SUB                                         ! entonces, Lo Elimina
   ENDIF                                               ! Fin Cond
   IF TIP$ = "1" OR                                  \ ! Si es Nuevo ¢
      TIP$ = "2" THEN BEGIN                            ! Modificaci¢n
         NOVED$ = PACK$(NOVE$)                         ! Empaq. Status
   ENDIF                                               ! Fin Cond
   IF TIP$ = "3" THEN BEGIN                            ! Si es Retiro
      COD1% = 0                                        ! Cierra Compra Parcial
      COD2% = 0                                        ! Cierra Compra Total
      NOVED$ = PACK$("58")                             ! Suspende Credito
   ENDIF                                               ! Fin Cond

   KEY$ = PACK$("36"+CED$)                             ! Arma llave (mf)
   WRITE FORM "C8 C30 2C3 C1";#26;                   \ ! Graba o Regraba
         KEY$,NOM$,FECRE$,FEMOD$,NOVED$                ! EMPRE.DAT

   KEY$ = PACK$(PAG$+VARI$+CED$)                       ! Arma llave para Grab
   WRITE FORM "C8 C30 2C3 C1";#26;                   \ ! Graba o Regraba
         KEY$,NOM$,FECRE$,FEMOD$,NOVED$                ! EMPRE.DAT

   WRITE FORM "C13 C1 3I4";#27;CODC$,NOVED$,         \ ! Graba EAMTENDV.DAT
                               COD1%,COD2%,USERE%      ! con Cupos y Status
   WRITE FORM "C13 C1 3I4";#27;PACK$("36") +         \ ! Graba EAMTENDV.DAT
         RIGHT$(CODC$,12),NOVED$,COD1%,COD2%,USERE%    ! con Cupos y St (mf)

   KEY$ = PACK$(PAG$+"6"+CED$)                         ! Arma llave para Grab
   WRITE FORM "C8 C30 2C3 C1";#26;                   \ ! Graba o Regraba
         KEY$,NOM$,FECRE$,FEMOD$,NOVED$                ! EMPRE.DAT

   CODC$ = PACK$(PAG$+"6"+"0000000000"+CED$)           ! Arma Llave Tend/Custa
   WRITE FORM "C13 C1 3I4";#27;CODC$,NOVED$,         \ ! Graba EAMTENDV.DAT
                               COD1%,COD2%,USERE%      ! con Cupos y Status
   CODC$ = PACK$(PAG$+VARI$+"0000000000"+CED$)         ! Arma Llave Tend/Custa


!*************************************
WRITE FORM "C70 C2";#11;"NIT: 	"+CED$+" "+NOM$,FINR$
!*************************************


   IF TIP$ = "1" THEN BEGIN                            ! Si es Creaci¢n
      REST%  = 0                                       ! Restart en 0
      COMP%  = 0                                       ! Abre Acumul Compras
      NUCO%  = 0                                       ! Abre Cant Compras
      USERE% = 0                                       ! Campo Exits
      IND$ = " "                                       ! Espacio
     ! WRITE FORM "C13 2I4 I2 I4 C1";#28;CODC$,       \ ! Graba EAMCUSTA.DAT
      !            REST%,COMP%,NUCO%,USERE%,IND$        ! con CEROS

      CODC$ = PACK$(PAG$+"6"+"0000000000"+CED$)        ! Arma Llave Tend/Custa
      !WRITE FORM "C13 2I4 I2 I4 C1";#28;CODC$,       \ ! Graba EAMCUSTA.DAT
      !            REST%,COMP%,NUCO%,USERE%,IND$        ! con CEROS

      CODC$ = PACK$("36"+"0000000000"+CED$)            ! Arma Llave Tend/Custa
     ! WRITE FORM "C13 2I4 I2 I4 C1";#28;CODC$,       \ ! Graba EAMCUSTA.DAT
      !            REST%,COMP%,NUCO%,USERE%,IND$        ! con CEROS para (mf)
   ENDIF
   TOT1% = TOT1% + 2
END SUB

!  -------------  Rutina para grabar en el archivo CUPOS1.DAT ---------------

  SUB GRABAREG
    IF CED$ <> CEDA$ THEN BEGIN                        ! Si es Nuevo NIT
       CALL RESUMEN                                    ! Asocia Rep Rangos
       CEDA$ = CED$                                    ! Restaura NIT
       CALL VERIFICA                                   ! Verifica Empresa
    ENDIF                                              ! Fin Cond
    IF ERRORCOD$ = "  " THEN BEGIN                     ! Si existe Empresa
       FOR I% = COD1% TO COD2%                         ! Inicia Loop


           KEY$ = PACK$(PAG$+VARI$         +         \ ! Arma Llave Cupos
                  RIGHT$("00000000"+STR$(COD1%),8))    ! por cada Numero
           CODC$ = PACK$(CED$)                         ! Empaqueta NIT
           !WRITE FORM "C5 C7";#25;KEY$,CODC$           ! y se graba Factura
           TOTBUE = TOTBUE + 1                         ! Facturas OK
           CONSOLE                                     ! 
           LOCATE 19,5                                 ! Se ubica en fila 18
           PRINT "Reg. Grabag.:  ";TOTBUE              ! Despliega Contador
           LOCATE 18,40                                ! Se ubica en fila 18
           PRINT "Factura No.:  ";COD1%                ! Despliega Factura
           LOCATE 19,40                                ! Se ubica en fila 19
           PRINT "    NIT No.:  ";CED$                 ! Despliega Factura
           LOCATE 21,40                                ! Se ubica en fila 20
           PRINT NOMD$                                 ! Despliega Nombre
           COD1% = COD1% + 1      		       ! Incrementa Rango 

       NEXT I%                                         ! Termina Loop
    ENDIF ELSE BEGIN                                   !
       OBERROR$ = "Se Elim Fac"                        ! 
       ELIMICC% = ELIMICC% + 1                         ! Suma Factur Elimin
       RECH%    = RECH% + 1                            ! Suma Rangos Elimin
       IF OBSER$ <> ESPACIO$ THEN BEGIN                ! Si es Nuevo NIT
           CALL REPORTER                               ! Reporta Error
       ENDIF                                           ! Fin Cond
    ENDIF                                              ! Fin Cond
  END SUB
                                                                        
!  -------- Rutina para crear Registros del BATCH de actualizacion ---------

SUB LLENAREG
    IF END #24 THEN SALIR                              ! Si EOF
LEA:
    READ #24;LINE REGI$                                ! Lee Registro
    TOT2% = TOT2% + 1                                  ! Registros Leidos
    CONSOLE                                            !
    LOCATE 18,5                                        ! Se ubica en fila 18
    PRINT "Registro No.:  ";TOT2%                      ! Despliega Contador
    IF LEFT$(REGI$,1) = " " THEN BEGIN                 ! Si Reg empieza en Col 2
       REGI$=RIGHT$(REGI$,LEN(REGI$)-1)                ! Corre el Registro
    ENDIF                                              !
    TIP$ = LEFT$(REGI$,1)                              ! Toma Tip Reg 1,2,3,4   
    IF TIP$ < "1" OR TIP$ > "4" THEN BEGIN             ! Si Fecha Control
       CALL CABEZA                                     !
       CANERRA% = CANERRA% + 1                         ! Incr Cont Errados
       OBSER$   = "Novedad Errada "                    ! Restaura Mens
       OBERROR$ = "Se Elimina "                        ! 
       PRINT LEFT$(REGI$,46);" ";OBSER$;OBERROR$;      ! Arma Linea con
       PRINT " ";MID$(REGI$,47,LEN(REGI$)-46)          ! Datos Registro
       OBSER$   = "               "                    ! Restaura Mens
       OBERROR$ = "           "                        ! Restaura Mens

       GOTO LEA                                        ! Lo elimina
    ENDIF                                              !
    IF TIP$ = "4" THEN BEGIN                           ! Si Fecha Control
       FEC$ = MID$(REGI$,4,6)                          ! Guarda Fecha
       ALM$ = MID$(REGI$,10,4)                         ! Guarda Almacen
       IF ALM$ <> NUAL$ THEN BEGIN                     ! Verifica Diskt OK
          CALL ERRALM                                  ! 
          SW.FIN = 1                                   ! Marca Fin
          EXIT SUB                                     ! y Termina
       ENDIF                                           ! Fin Cond
       UEX2 = VAL(MID$(REGI$,6,4) +                   \! Toma Mes y Dia
              MID$(REGI$,5,1))                         ! y Ult. Dig Ano
       EXIT SUB                                        ! Termina Sub-Rutina
    ENDIF                                              !
    PAG$  = MID$(REGI$,3,1)                            ! Guarda Tipo Pago
    VARI$ = MID$(REGI$,5,1)                            ! Guarda Variedad
    CED$  = RIGHT$("00000000000"            +         \! Inserta ceros
            STR$(VAL(MID$(REGI$,7,11))),11) +         \! Guarda NIT 
            RIGHT$("000"                    +         \! Inserta ceros
            STR$(VAL(MID$(REGI$,19,3))),3)             ! y toma Sucursal
    NOVE$ = MID$(REGI$,23,2)                           ! Toma Novedad
    COD1% = VAL(MID$(REGI$,27,9))                      ! No.Formato 1
    COD1$ = RIGHT$("0000000000"+STR$(COD1%),10)        ! String Formato 1
    COD2% = VAL(MID$(REGI$,38,9))                      ! No.Formato 2
    COD2$ = RIGHT$("0000000000"+STR$(COD2%),10)        ! String Formato 2
    NOM$  = "  "                                       ! Reset Nombre
    IF NOVE$ <> "99" THEN BEGIN                        ! Si no son Facturas
       NOM$ = RIGHT$(REGI$, LEN(REGI$) - 46)           ! Toma Nombre Emp
    ENDIF                                              ! Fin Cond
    IF CED$  < "00000000001000" THEN BEGIN             ! Si es Registro sin CC.
       ELIMICC% = ELIMICC% + 1                         ! Incr. Contador
       EXIT SUB                                        ! Elimina el Registro
    ENDIF                                              ! Fin Cond
    IF TIP$ = "3" THEN BEGIN                           ! Si es para Retirar
       ELIMINA% = ELIMINA% + 1                         ! Incr. Contador
    ENDIF                                              ! Fin Cond
    IF VARI$ = "1" THEN BEGIN                          ! Si es vrdad 1
       IF COD$ = "08903032085000" THEN               \ ! Si es Cod Comfamil
          CANFONC% = CANFONC% + 1                    \ ! Empl. Foncaja
       ELSE                                          \ ! de lo contrario
          CANEMPR% = CANEMPR% + 1                      ! Reg. Otras Epresas
    ENDIF                                              ! Fin Cond
    IF VARI$ = "2" THEN BEGIN                          ! Si es vrdad 2
       CANESCO% = CANESCO% + 1                         ! Temporada Escolar
    ENDIF                                              ! Fin Cond
    IF VARI$ = "3" THEN BEGIN                          ! Si es vrdad 3
       CANCOMF% = CANCOMF% + 1                       \ ! Cr. Interno
    ENDIF                                              ! Fin Cond
    IF VARI$ = "4" THEN BEGIN                          ! Si es vrdad 4
       CANSUBC% = CANSUBC% + 1                       \ ! Cr. Especial Empresas
    ENDIF                                              ! Fin Cond
    IF VARI$ = "5" THEN BEGIN                          ! Si es vrdad 5
         CANSUBE% = CANSUBE% + 1                     \ ! Cr. CVC 60%
    ENDIF                                              ! Fin Cond
    IF NOVE$ <> "99" THEN BEGIN                        ! Si no son Facturas
       KEY$  = PACK$(PAG$+VARI$+CED$)                  ! Arma Llave EMPRE
       CODC$ = PACK$(PAG$+VARI$+"0000000000"+CED$)     ! Arma Llave Tend/Custa 
       CALL CREAEMPRE                                  ! Gestiona Empresa
       EXIT SUB                                        !
    ENDIF                                              ! Fin Cond
    CALL GRABAREG                                      ! Graba Factura/Cupo
    EXIT SUB                                           !
SALIR:                                                 !
  SW.FIN = 1                                           !
END SUB                                                !

!  --------------   Rutina final de cierre del archivo   ----------------
SUB FINAL

   SW.FIN = 1                                          !
   CLOSE 24                                            ! Cierra EAMNVMP.DAT
   CLOSE 25                                            ! Cierra CUPOS.DAT
   CLOSE 26                                            ! Cierra EMPRE.DAT
   CLOSE 27                                            ! Cierra EAMTENDV.DAT
   CLOSE 28                                            ! Cierra EAMCUSTA.DAT 
   CLOSE 11
END SUB

!----------------------  PROGRAMA  PRINCIPAL  ----------------------------
   DIM MESES(12)
   FECHA$=DATE$
   MESPRO=VAL(MID$(FECHA$,3,2))
   DIAPRO$=RIGHT$(FECHA$,2)
   ANOPRO$=LEFT$(FECHA$,2)
!----------------------   Llena arreglo de meses   ---------------------------
   MESES(1)="ENERO"     :  MESES(2)="FEBRERO"    :  MESES(3)="MARZO"
   MESES(4)="ABRIL"     :  MESES(5)="MAYO"       :  MESES(6)="JUNIO"
   MESES(7)="JULIO"     :  MESES(8)="AGOSTO"     :  MESES(9)="SEPTIEMBRE"
   MESES(10)="OCTUBRE"  :  MESES(11)="NOVIEMBRE" :  MESES(12)="DICIEMBRE"
!----------------------------------------------------------------------------

INICIO:
   ON ERROR GOTO ERRTRAP 
   CALL CREAFECHA
   FEC1$ = FRA.LIN$
   ESPACIO$ = "               "                        ! No CAMBIAR !!!!!
   ARCH0$ = "C:\ADXALMA.DAT"                           ! Define Ctrl Almac
   ARCH1$ = "C:\EAMNVMP.DAT"                           ! Define Novedades
   ARCH2$ = "C:\ADX_IDT1\CUPOS.DAT"                    ! Define Cupos
   ARCH3$ = "C:\ADX_IDT1\EMPRE.DAT"                    ! Define Empresas
   ARCH4$ = "C:\ADX_IDT1\EAMTENDV.DAT"                 ! Define Tender Verif
   ARCH5$ = "C:\ADX_IDT4\EAMCUSTA.DAT"                 ! Define Status Clientes
   OPEN ARCH0$               AS 23 BUFFSIZE 80         ! Abre Control Almacen
   OPEN ARCH1$               AS 24 BUFFSIZE 100        ! Abre Novedades
   !OPEN ARCH2$ KEYED RECL 12 AS 25                     ! Abre Cupos KL=5
   OPEN ARCH3$ KEYED RECL 45 AS 26                     ! Abre Empresas KL=8
   OPEN ARCH4$ KEYED RECL 26 AS 27                     ! Abre Verific Medios Pago
   OPEN ARCH5$ KEYED RECL 28 AS 28                     ! Abre Status y Compras
   READ FORM "C80";#23;AL$                             ! Lee Control Almac
   NUAL$ = LEFT$(AL$, 4)                               ! Guarda Num Almacen
   NOAL$ = RIGHT$(AL$, LEN(AL$) - 12)                  ! Guarda Nombre
   IF VAL(NOAL$) > 25 THEN BEGIN                       !
      NOAL$ = LEFT$(NOAL$, 25)                         ! Guarda Nombre
   ENDIF                                               !
   ALM$  = RIGHT$(NUAL$,3)                             !
   CONSOLE                                             !
   CLEARS                                              !
   PRINT :  PRINT :  PRINT : PRINT                     !
   PRINT "       Proceso para el Almac‚n ";ALM$;"  ";NOAL$
   PRINT                                               ! Avanza Linea
   PRINT "       Inicia a las: ";FEC1$                 ! Indica comienzo
   PRINT                                               ! Avanza Linea
   PRINT "                        Por Favor Esperar..."!
   PRINT                                               ! Avanza Linea
   PRINT "       Se est  procesando :  ";ARCH1$        ! Archivo en Proceso
   CONLI = 60
   SW.FIN = 0                                          ! Indica EOF Novedades
   TOTCERO = 0                                         ! No. registros ERRADOS
   FORMA$ = "##############"                           ! Compaginador
   FORMAT$ = "##########"                              ! Compaginador
   FORMAN$ = "#####"                                   ! Compaginador 
!*************************************
WRITE FORM "C70 C2";#11;"*************************************",FINR$
WRITE FORM "C70 C2";#11;"*******  EMPRESAS APLICADAS  ********",FINR$
WRITE FORM "C70 C2";#11;"*************************************",FINR$
WRITE FORM "C70 C2";#11;" ",FINR$
!*************************************
   WHILE SW.FIN = 0                                    ! Ciclo paso informacion
         FECRE$ = FECHAPD$                             ! Restaura Fec Creac
         FEMOD$ = FECHAPD$                             ! Restaura Fec Noved
         CALL LLENAREG                                 ! Pasa Datos de AS/400
   WEND                                                ! Fin del ciclo
CALL FINAL                                             ! Fin operaciones
CALL CREAFECHA                                         ! Rutina Formato Fecha
FEC2$ = FRA.LIN$                                       !
    LPRINTER                                           !
    IF CONLI > 30 THEN BEGIN                           ! Si Hoja Full
      PRINT CHR$(12)                                   ! Salto de Pag
      PRINT " "                                        !
    ENDIF                                              !
    PRINT"-------------------------------------------------------------------------------" 
    PRINT                                              ! 
    PRINT                                              !
	WRITE FORM "C50 C2";#11;" ",FINR$   
    PRINT                                              !
	WRITE FORM "C50 C2";#11;"	************************************************* ",FINR$   
    PRINT "       ---- CREACION DEL MAESTRO DE FORMATOS DE CREDITO ---"
	WRITE FORM "C50 C2";#11;"	***** REPORTE CARGA DE CREDITOS SISTEMA POS *****   ",FINR$   
    PRINT "           Y COMPRAS  DIARIAS PARA INICIO DE QUINCENA"
	WRITE FORM "C50 C2";#11;" 	************************************************* ",FINR$   
    PRINT " "
	WRITE FORM "C50 C2";#11;" ",FINR$   
    PRINT "           Archivos : CUPOS1 y EAMNVMP.DAT "
	WRITE FORM "C50 C2";#11;"ARCHIVOS: CUPOS1 Y EAMNVMP.DAT ",FINR$   
    PRINT "            --------------------------------------- "
	WRITE FORM "C50 C2";#11;"------------------------------------------ ",FINR$   
    PRINT "     Registros con Tipo Nov ERRADA = ";CANERRA%
	WRITE FORM "C50 C2";#11;"Registros con Tipo Nov ERRADA = 		"+STR$(CANERRA%),FINR$
    PRINT "            Medicamentos Empleados = ";CANFONC%
	WRITE FORM "C50 C2";#11;"Medicamentos Empleados = 			"+STR$(CANFONC%),FINR$   
    PRINT "             Medicamentos Empresas = ";CANEMPR%
	WRITE FORM "C50 C2";#11;"Medicamentos Empresas = 			"+STR$(CANEMPR%),FINR$   
    PRINT "                 Temporada Escolar = ";CANESCO%
	WRITE FORM "C50 C2";#11;"Temporada Escolar = 				"+STR$(CANESCO%),FINR$   
    PRINT "                   Credito Interno = ";CANCOMF%
	WRITE FORM "C50 C2";#11;"Credito Interno = 				"+STR$(CANCOMF%),FINR$   
    PRINT "      Credito Especial de Empresas = ";CANSUBC%
	WRITE FORM "C50 C2";#11;"Credito Especial de Empresas = 			"+STR$(CANSUBC%),FINR$   
    PRINT "         Credito Empleados CVC 60% = ";CANSUBE%
	WRITE FORM "C50 C2";#11;"Credito Empleados CVC 60% = 			"+STR$(CANSUBE%),FINR$   
    PRINT "          ---------------------------------------"
	WRITE FORM "C50 C2";#11;"------------------------------------ ",FINR$   
    PRINT "            Total Registros Leidos = ";TOT2%
	WRITE FORM "C50 C2";#11;"Total Registros Leidos = 			"+STR$(TOT2%),FINR$   
    PRINT "           Total Facturas Grabadas = ";TOTBUE
	WRITE FORM "C50 C2";#11;"Total Facturas Grabadas = 			"+STR$(TOTBUE%),FINR$   
    PRINT "           Registros para Eliminar = ";ELIMINA%
	WRITE FORM "C50 C2";#11;"Registros para Eliminar = 			"+STR$(ELIMINA%),FINR$   
    PRINT "    Rangos/Factura sin NIT Empresa = ";ELIMICC%
	WRITE FORM "C50 C2";#11;"Rango/Factura sin NIT Empresa = 		"+STR$(ELIMICC%),FINR$   
    PRINT "(EAMCUS9.DAT) Registros Eliminados = ";ELIMIOK%
	WRITE FORM "C50 C2";#11;"Registros Eliminados = 				"+STR$(ELIMIOK%),FINR$   
    PRINT " Total Registros Empresas Grabados = ";TOT1%
	WRITE FORM "C50 C2";#11;"Total Registros Empresas Grabadas = 		"+STR$(TOT1%),FINR$   
    PRINT "Tot Lect Reg/Rango sin NIT Empresa = ";TOT3%
	WRITE FORM "C50 C2";#11;"Total Lectura Reg/Rango sin NIT Empresa = 	"+STR$(TOT3%),FINR$   
  PRINT "      ----------------------------------------------------------- "
	WRITE FORM "C50 C2";#11;"--------------------------------- ",FINR$   
  PRINT "      ** INICIA  A LAS: ";FEC1$;" **ARCHIVO EAMNVMP.DAT"
	WRITE FORM "C50 C2";#11;"Inicia a las  "+FEC1$,FINR$   
  PRINT "      ** TERMINA A LAS: ";FEC2$;"      ALMAC=";ALM$
	WRITE FORM "C50 C2";#11;"Termina a las  "+FEC2$,FINR$   
  PRINT "      ----------------------------------------------------------- "
	WRITE FORM "C50 C2";#11;"---------------------------- ",FINR$   

STOP

ERRTRAP:
   TOT3% = TOT3% + 1
!   LOCATE 18,5 : PRINT "No. de Conflictos:  ";TOT3%
   HX% = ERRN
   ERRFX$ =""
   FOR S% = 28 TO 0 STEP -4
       SX% = SHIFT(HX%,S%)
       SUM% = SX% AND 000FH
   IF SUM% > 9 THEN         \
        SUM% = SUM% + 55    \ 
   ELSE                     \
        SUM% = SUM% + 48
        Z$ = CHR$(SUM%)
        ERRFX$ = ERRFX$ + Z$
   NEXT S%
   
IF ERRF% = 26 AND ERR <> "  " THEN BEGIN
   ERRORCOD$ = ERR
ENDIF
IF ERRF% = 24 AND (ERR = "OE" OR ERR = "FU" OR ERR = "EF") THEN BEGIN
   CONSOLE
   CLEARS
   PRINT : PRINT : PRINT : PRINT : PRINT : PRINT
   PRINT "               ------------------------------------------------"
   PRINT "              |  No  olvide  que  para  este  proceso debe     |"
   PRINT "              |  haber copiado en C: el archivo EAMNVMP.DAT    |"
   PRINT "              |                                                |"
   PRINT "              |               Digite INTRO por Favor..!        |"
   INPUT "               ------------------------------------------------";NOM$
   CLOSE 24                                   ! Cierra EAMNVMP.DAT
   !CLOSE 25                                   ! Cierra CUPOS1.DAT
   CLOSE 26                                   ! Cierra EMPRE1.DAT
   CLOSE 27                                   ! Cierra EAMTENDV.DAT
   CLOSE 28                                   ! Cierra EAMCUST9.DAT
   STOP
ENDIF
IF (ERRF% = 26 AND ERR = "EF") THEN BEGIN
    ERRORCOD$ = "NO"
ENDIF ELSE BEGIN
   IF (ERRF% = 25 OR ERRF% = 26 OR               \!
       ERRF% = 27 OR ERRF% = 28) AND             \!
       (ERR = "OE" OR ERR = "FU" OR ERR = "EF") THEN BEGIN
       CONSOLE
       CLEARS
       PRINT : PRINT : PRINT : PRINT : PRINT : PRINT
       PRINT "               ------------------------------------------------"
       PRINT "              |  Este Proceso SOLO puede ejecutarse sobre las  |"
       PRINT "              |  COPIAS de los Archivos:                       |"
       PRINT "              |                                                |"
       PRINT "              |     ADX_IDT1:   EAMTENDV.DAT  CUPOS.DAT        |"
       PRINT "              |                 EMPRE.DAT       y              |"
       PRINT "              |     ADX_IDT4:   EAMCUSTA.DAT                   |"
       PRINT "              |                                                |"
       PRINT "              |  Efect£e las Copias y luego Repita el Proceso  |"
       PRINT "              |                                                |"
       PRINT "              |                       Digite INTRO, Por Favor..|"
       INPUT "               ------------------------------------------------";NOM$
       PRINT ERR;"  ";ERRF%;"  ";ERRN 
       STOP
   ENDIF
ENDIF
RESUME  
END
!------------------------------------------------------------------------
!              ***     FIN  DEL  PROGRAMA  PRINCIPAL      ***
!------------------------------------------------------------------------
