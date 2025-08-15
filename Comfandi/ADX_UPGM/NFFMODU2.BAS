\------------------------------------------------------------------------------
\                   NOMBRE DEL MODULO : NFFMODUL.BAS      
\          Modulo de Rutinas de Front-Store - Numeraci¢n Fiscal
\  
\               VERSION : General Consolidada Julio de 1999
\
\       Primer Revision : Global DataTel      Marzo del 2000
\  
\    Resumen de Cambios : Cambio String '99' Resoluc 1974 de Oct 12/99
\ 
!-----------------------------------------------------------------------------
!  14/03/98     Consolidar rutinas para version fiscal            
!-----------------------------------------------------------------------------
\
!-----------------------------------------------------------------------------
!  FAVOR  ORGANIZAR LOS TS.LINETYPE EN LA USER-20 DE ACUERDO AL 
!         SIGUIENTE ORDEN:
!-----------------------------------------------------------------------------
\
\         TS.LINETYPE        ETAPA TRANSACCION
\         -----------       -------------------------------------------
\             18             Header  (Detectar "HEADER DE DETECCION 1")     
\              1             Venta de articulos
\              7             Balance due  =  Calculo de saldo
\              2             Pagos
\              4             Cambio despues del pago
\             22             Anulacion en el trailer
\             31             Suspension de transaccion
\              6             Trailer de la Tienda
\             18             Trailer  (Detectar "TRAILER DE DETECCION 2")    
\
\
\ NOTA: Favor avisar en caso de incluir otros TS.LINETYPE
\ 
!-----------------------------------------------------------------------------
!                   RUTINAS GENERALES
!-----------------------------------------------------------------------------
!
! E1    SAVE.PRINT              Salvar variables de Impresion     
! E2    RESTORE.PRINT           Restaurar variables de Impresion
! E3    SAVE.DISPLAY            Salvar variables del DISPLAY
! E4    RESTORE.DISPLAY         Restaurar variables del DISPLAY
! 
!-----------------------------------------------------------------------------

%Environ T                                        !

INTEGER*1 Global U.IMP.NIT%, UE.RECUPERA.TRX%
String    Global FIN.LIN$

%INCLUDE EAMTSWKG.J86                                  ! working storage
%INCLUDE EAMETWKA.j86                                  !
%INCLUDE EAMTRANS.j86                                  !
%INCLUDE EAMTERMS.J86                                  !
%INCLUDE EAMITEMR.J86                                  !

\%INCLUDE NFFTSUVA.011                                  ! working user storage

%INCLUDE EAMTSUVA.J86                                  ! VARIABLE
!------------------------------------------------------------------------------

%INCLUDE EAMTOPTS.J86                                  !
%INCLUDE EAMSOPTS.J86                                  !
%INCLUDE EAMASMRT.J86                                  ! assembler SUBroutines
%INCLUDE EAMADXRT.J86                                  ! EXTERNAL SUBroutines

!Parametros a pasar UE.STRING$ = String a Imprimir
!  	            UE.STATION = Destino de la impresion
!                		4000H  - print on CR
!                		2000H  - print on SJ
!                		1000H  - print on DI
!                   6000h  - print on CR and SJ 
!                		0100H  - number of linefeeds (eg here = 1)
Function U.IMPRIME(UE.STRING$,UE.STATION) External					! Rutina de Impresion
String UE.STRING$, LF$ 											! Variables temporales
INTEGER*2 UE.STATION											!
End Function
!--- Fin rutina de impresion 

Function  VISORES4690(U.D.VISOR%, U.D.LINEA1$, U.D.LINEA2$,U.D.TIEMPO%,U.D.POSICION$) External ! Msg Display
String U.d.Linea1$, U.d.Linea2$, U.d.Posicion$						! Variables de
Integer*2 U.d.visor%, U.d.tiempo%   							! trabajo
String U.D.Linea1$, U.D.Linea2$		
End Function
!--- Fin manejo de visores

Function U.CORTACR External
End Function 

!------------------------------------------------------------------------------
!                  Subrutina de Despliegue de ARRAY Datos
!------------------------------------------------------------------------------
 SUB VER PUBLIC                                        !
!  WRITE FORM "C38";#34;PASO$                          !
!  WRITE FORM "C38";#34;"Estado: "+STR$(TS.IO.STATE)   !
!  WRITE FORM "C38";#34;"Proced: "+STR$(TS.PROCEDURE)  !
!  WRITE FORM "C38";#34;"Motor: "    +                \!
!        STR$(TS.IO.MOTORKEY) +"   Varie: "+VARIE$     !
!------------------------------------------------------v-----------------------

!LFEED$ = CHR$(1BH)+CHR$(33H)+CHR$(204)
!LFEED$  = CHR$(0AH)

!FONPH$  = CHR$(1BH)+CHR$(47H)+CHR$(1)
!FONPHN$ = CHR$(1BH)+CHR$(47H)+CHR$(00)
!CENTRA$ = CHR$(1BH)+CHR$(61H)+CHR$(01)

   NFF.S34$ = PASO$                                    !    Ses34 Cambio 1
   CALL U.IMPRIME(NFF.S34$,4100H)                     !
   NFF.S34$ = "Estado: "+STR$(TS.IO.STATE)             !
   CALL U.IMPRIME(NFF.S34$,4100H)                     !
   NFF.S34$ = "Proced: "+STR$(TS.PROCEDURE)            !
   CALL U.IMPRIME(NFF.S34$,4100H)                     !
   NFF.S34$ = "Motor: " +STR$(TS.IO.MOTORKEY) +       \!
              "   Varie: " + VARIE$                    !
   CALL U.IMPRIME(NFF.S34$,4100H)                     !
!------------------------------------------------------!-----------------------
   FOR NFF.II% = 0 TO 10                               !
!       WRITE FORM "C38";#34;"TECLA "          +      \!
!       RIGHT$("  "+STR$(NFF.II%),2) + " "     +      \!
!       RIGHT$("   "+STR$(TS.IO.KEYS(NFF.II%)),3)  +  \!
!       "  DATO " + TS.IO.DATA$(NFF.II%)               !
!------------------------------------------------------v-----------------------
        NFF.S34$ = "TECLA " + RIGHT$("  "        +    \!    Ses34 Cambio 2
                   STR$(NFF.II%),2) + " "        +    \!
                   RIGHT$("   "                  +    \!
                   STR$(TS.IO.KEYS(NFF.II%)),3)  +    \!
                   "  DATO "+TS.IO.DATA$(NFF.II%)      !
        CALL U.IMPRIME(NFF.S34$,4100H)                !
   NEXT NFF.II%                                        !
!------------------------------------------------------!-----------------------
 END SUB                                               !
!------------------------------------------------------------------------------
SUB QXL.TSUPEC21(prtfis) EXTERNAL                      !
    STRING prtfis
END SUB                                                !

SUB TSTPEC01 EXTERNAL                                  ! Rut. escritura en TSL
END SUB                                                !
                                                       !
SUB TSTPEC06 EXTERNAL                                  ! Rut. Escrit XLOG y TSL
END SUB                                                !
                                                       !
SUB TSHIECET EXTERNAL                                  ! Rut. para mandar tono
END SUB                                                !
                                                       !
SUB TSPREC01 EXTERNAL                                  ! Rutina de Impresion
END SUB                                                !
                                                       !
SUB TSCSEC03 EXTERNAL                                  ! Despliega Error
END SUB                                                !

SUB SAVE.PRINT EXTERNAL
END SUB

SUB RESTORE.PRINT EXTERNAL
END SUB

!
!----------------------------------------------------------------------------
!            Funci¢n para Solicitar Servicios de la Aplicaci¢n
!----------------------------------------------------------------------------
! SUB ADXSERVE(NFF.RET,NFF.FUNC,NFF.PARM1,NFF.PARM2$) EXTERNAL
!     STRING       NFF.PARM2$                          !
!     INTEGER*4    NFF.RET                             !
!     INTEGER*2    NFF.FUNC,   NFF.PARM1               !
! END SUB                                              !
!------------------------------------------------------------------------------
!                 Subrutinas del Modulo Fiscal NFFMODUL.BAS
!------------------------------------------------------------------------------
!-----------------------------------------------------------------------------
!                Subrutina para Desempaquetar Datos    
!------------------------------------------------------------------------------
SUB GETUNPK PUBLIC                                     !
    K% = MATCH(":",B$,J%)                              ! Busca separador Campo
    A$ = UNPACK$(MID$(B$,J%,K%-J%))                    ! y lo desempaqueta
    J% = K% + 1                                        ! Marca inicio del 
END SUB                                                ! siguiente campo 
!------------------------------------------------------------------------------
!                  Subrutina para Obtener Flags 
!------------------------------------------------------------------------------
SUB GETFLAG PUBLIC                                     !
    NFF.FLAG = VAL(A$)                                 ! Conv. NFF.FLAG a INTEGER
                                                       ! FLAGS individuales
    IF (NFF.FLAG AND 00000001H) THEN                  \!
       A$ = "1" ELSE A$ = "0"                          !
    FOR I% = 1 TO 15                                   !
      NFF.FLAG = SHIFT(NFF.FLAG,1)                     ! Inic siguiente BIT
      IF (NFF.FLAG AND 00000001H) THEN                \!
         A$ = "1" + A$                                \!
      ELSE                                            \!
        A$ = "0" + A$                                  !
    NEXT I%                                            !
END SUB                                                ! 

!------------------------------------------------------------------------------
!               Restaura Variables en Recovery/Retreieve STR-1
!------------------------------------------------------------------------------

SUB NFF.STRING1 PUBLIC                                 !
   J% = 3                                              ! Inicio de Datos
   CALL GETUNPK                                        ! Codigo del Producto
   NFF.PLU$  = A$                                      !
   CALL GETUNPK                                        ! Precio de Venta
   NFF.VRU% = VAL(A$)                                  !
   CALL GETUNPK                                        ! Codigo del Dpto.
   NFF.DEPA$ = A$                                      !
   CALL GETUNPK                                        ! Cupones Fam. Corriente
   CALL GETUNPK                                        ! Indicativo 1
   CALL GETFLAG                                        ! Busca BITS en 1
   NFF.INDICAT1 = VAL(MID$(A$,15,1))                   ! Toma Cond Descto
   CALL GETUNPK                                        ! Indicativo 2
   NFF.VOREF% = VAL(A$)                                ! Indica Void/Refund
   CALL GETFLAG                                        ! Busca BITS
   CALL GETUNPK                                        ! Indicativo 3
   NFF.ITIPO = VAL(LEFT$(A$,1))                        ! Tipo de Articulo
   IF NFF.ITIPO < 6 THEN                              \! Si es item de venta 
         NFF.ITEM = -1 ELSE NFF.COUP = -1              !
END SUB                                                !
!------------------------------------------------------------------------------
!               Restaura Variables en Recovery/Retreieve STR-99
!------------------------------------------------------------------------------

SUB NFF.STRING99 PUBLIC                                !
   J% = 3                                              ! Inicio de Datos
   CALL GETUNPK                                        ! Tipo de string
   NFF.STYPE = VAL(A$)                                 !
   IF NFF.STYPE = 21 THEN BEGIN                        ! String Nro Transaccion
      NFF.TRTYPE$ = MID$(B$,J%,2)                      ! Datos de Transaccion 
      J% = J% + 2                                      !
      NFF.PRF$ = MID$(B$,J%,4)                         ! Prefijo
      J% = J% + 4                                      !
      NFF.NTRA = VAL(UNPACK$(MID$(B$,J%,8)))           ! Nro de Factura
   ENDIF                                               !
END SUB                                                !
!------------------------------------------------------------------------------
!        Subrutina para presentar la Fecha en Formato MMM/DD/AAAA
!------------------------------------------------------------------------------

SUB NFF.ANO2000 PUBLIC                                 !
                                                       !
    NFF.M$="EneFebMzoAbrMyoJnoJloAgoSepOctNovDic"      ! Tabla Meses
    NFF.DL$ = "20"                                     ! Siglo XXI
    IF LEFT$(DATE$,2) = "99" THEN NFF.DL$ = "19"       ! Siglo XX
    NFF.DL$ = NFF.DL$ + LEFT$(DATE$,2) + " "           ! Fecha para Store
    NFF.DL$ = MID$(NFF.M$,VAL(MID$(DATE$,3,2))*3-2,3)+\! Data Line de
              "/" + STR$(VAL(RIGHT$(DATE$,2)))    +   \! DI and CR
              "/" + NFF.DL$                            ! 
    IF TS.LINETYPE = 6 THEN BEGIN                      ! Si Store Data/Trans
       NFF.SLA% = MATCH("/",TS.PRTBUF$,1)              ! Busca SLASH
       IF NFF.SLA% <> 0 THEN BEGIN                     ! Si lo encuentra
          NFF.SLA% = MATCH(" ",TS.PRTBUF$,NFF.SLA%)    ! ubica sig espacio
          IF NFF.SLA% < 10 THEN BEGIN                  ! Si hay espacio
             TS.PRTBUF$=NFF.DL$+MID$(TS.PRTBUF$, 10 , \! reformatea parte
                        LEN(TS.PRTBUF$)-NFF.SLA%-3)    ! izq de PRTBUF$
          ENDIF ELSE BEGIN                             ! y termina
             TS.PRTBUF$=LEFT$(TS.PRTBUF$,NFF.SLA%-3)+ \!
                        NFF.DL$ + RIGHT$(TS.PRTBUF$,  \! reformatea parte
                        LEN(TS.PRTBUF$)-NFF.SLA%-11)   ! izq de PRTBUF$
          ENDIF                                        !
       ENDIF                                           !
       NFF.SLA% = MATCH("/",TS.SJDATA$,1)              ! Busca SLASH
       IF NFF.SLA% = 3 THEN BEGIN                      ! Si lo encuentra
          NFF.SLA% = MATCH(" ",TS.SJDATA$,NFF.SLA%)    ! ubica sig espacio
          IF NFF.SLA% < 10 THEN BEGIN                  ! Si hay espacio
             NFF.DL$ = "20"                            ! Siglo XXI
             IF LEFT$(DATE$,2)="99" THEN NFF.DL$="19"  ! Siglo XX
             NFF.DL$=NFF.DL$ + LEFT$(DATE$,2) + " "    ! Fecha para Store
             NFF.DL$=                                 \!
             MID$(NFF.M$,VAL(MID$(DATE$,3,2))*3-2,2)+ \! Data Line de
             "/"+RIGHT$(DATE$,2) + "/" + NFF.DL$       ! DI and SJ
             TS.SJDATA$=NFF.DL$+MID$(TS.SJDATA$,10,10) ! reformatea parte
          ENDIF                                        !
       ENDIF                                           !
    ENDIF                                              !
END SUB                                                !

!------------------------------------------------------------------------------
!          Funcion para Recuperar Codigo de Error Compatible BASIC
!--- ---------------------------------------------------------------------------
FUNCTION ERRNSTR$(ERRNUM)                              !
    INTEGER*1 I                                        ! Denfine Variables
    INTEGER*4 ERRNUM,WORK                              !
    STRING HEX$,ERRNSTR$,WORK$                         !
    HEX$="0123456789ABCDEF"                            ! 
    ERRNSTR$="":WORK$=""                               !
    FOR I=1 TO 8                                       ! Inicia Loop
        WORK=ERRNUM AND 0000000FH                      ! Todos OFF menos Ultimo
        WORK$=MID$(HEX$,WORK+1,1)+WORK$                ! Adic Vr Hex a String
        ERRNUM=SHIFT(ERRNUM,4)                         ! Asigna sig Ultimo
    NEXT I                                             ! Continua Loop
    ERRNSTR$=WORK$                                     ! Toma Cod Error Definitivo
FEND                                                   !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Date       :  Mar. de 1999                                         **
!   **    Project    :  NUMERAC/FISCAL/2002-4022                             **
!   **    User Exit  :                                                       **
!   **    Inclu¡r en :  EAMTSUSU.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  Global DataTel                                       **
!   **                                                                       **
!   **    User defined variables for printing IVA on Customer Receipt        **
!   ***************************************************************************
!
FUNCTION FORMAT.AMOUNT(AMT1) EXTERNAL                  !
  INTEGER*1 FORMAT.AMOUNT                              !
  INTEGER*4 AMT1                                       !
END FUNCTION                                           ! 
                                                       !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Date       :  Mar. de 1999                                         **
!   **    Project    :  NUMERAC/FISCAL/2002-4022                             **
!   **    Responsible:                                                       **
!   **    User Exit  :                                                       **
!   **    Inclu¡r en :  EAMTSUSU.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  Global DataTel                                       **
!   **                                                                       **
!   ***************************************************************************

!------------------------------------------------------------------------------
!                   Subrutina para Intercalar I*4 en STRING  PUTN4
!------------------------------------------------------------------------------
!FUNCTION PUTN4(P1$,P2,P3) EXTERNAL                    !
!   STRING P1$                                         !
!   INTEGER*2 P2                                       !
!   INTEGER*4 P3                                       !
!END FUNCTION                                          !

!------------------------------------------------------------------------------
!                Subrutina para Armar String XLog & TLog
!------------------------------------------------------------------------------

SUB NFF.ARMA PUBLIC                                    ! Intercala una String
  CALL SUBSTR(SL.LOGDATA$,NFF.DIR% + NFF.D% -         \! en Otra String con
              LEN(NFF.X$),NFF.X$,0,LEN(NFF.X$))        ! long predefinida
  NFF.DIR% = NFF.DIR% + NFF.D%                         ! Asigna Sig posic de
END SUB                                                ! inicio de Intercalac

!----------------------------------------------------------------------------
!            Funci¢n para Conversion de Fecha y Hora
!----------------------------------------------------------------------------

SUB NFF.FEC(FH$) PUBLIC                                ! 
    STRING FH$,  HH$,  ME$,  NFF.FE$                   !
    NFF.M$ = "EneFebMarAbrMayJunJulAgoSepOctNovDic"    ! Tabla Meses
    IF VAL(MID$(FH$,3,2)) = 0 THEN BEGIN               ! Si es Mes Invalido
       NFF.FE$ = "           ****"                     ! coloca stars
       EXIT SUB                                        ! y sale de rutina
    ENDIF                                              !
    HH$ =  MID$(FH$,7,2) + ":"  +                     \! Compagina Hora
           MID$(FH$,9,2) + ":"+ RIGHT$(FH$,2)          !
    ME$ = MID$(NFF.M$,VAL(MID$(FH$,3,2))*3-2,3)  + " " ! Traduce Mes
    NFF.FE$ = "/19"+ LEFT$(FH$,2)                      ! Coloca siglo XX
    IF VAL(LEFT$(FH$,2)) < 98 THEN BEGIN               ! Coloca siglo XXI
       NFF.FE$ = "/20"+ LEFT$(FH$,2)                   !
    ENDIF                                              !
    FH$ = ME$+RIGHT$(" "+STR$(VAL(MID$(FH$,5,2))),2)+ \! Compagina Fecha
          NFF.FE$ + "     " + HH$                      !
END SUB                                                ! 
!----------------------------------------------------------------------------
!            Rutina para definir SIGNO del Item en Proceso
!----------------------------------------------------------------------------

SUB NFF.TIPITEM PUBLIC                                 ! Rutina Valida SIGNO
                                                       ! 
    IF NFF.ITIPO = 0 OR                               \! Si es Item Vta normal
       NFF.ITIPO = 1 OR                               \! o Deposito
       NFF.ITIPO = 4 THEN NFF.SIGNO = 1               \! o Misc en Vta 
    ELSE NFF.SIGNO = -1                                ! Marca Flag (+ o -)
    IF (NFF.VOREF% AND 0080H) THEN                    \! Chequea Si Cancelac
       IF NFF.ITIPO <> 8 AND                          \! Y NO flag Item Cancel
          NFF.ITIPO <> 9 THEN                         \! Y NO flag item deposito
        NFF.SIGNO = NFF.SIGNO * -1                     ! Cambia Signo
    IF (NFF.VOREF% AND 0040H) THEN BEGIN               ! Chequea Si Refund
       IF NFF.ITIPO <> 2  AND                         \! y NO Deposito
          NFF.ITIPO <> 3  THEN                        \! y NO Refund Deposito
              NFF.SIGNO = NFF.SIGNO * -1               ! Cambia Signo
    ENDIF                                              ! 
    IF NFF.VOREF% AND 0004H THEN                      \! Indicativo Prec Neg
       NFF.SIGNO = NFF.SIGNO * -1                      ! Precio(-), Camb Signo
END SUB                                                !
!------------------------------------------------------------------------------
!           Rutina para Verificar Registro de Control 
!------------------------------------------------------------------------------

SUB VERICLI                                             !
    NFF.ERCO$ = "  "                                    ! Inicia Estado Leer
    LEC$ = "C3 C6 2C1 4C20 C3 C10 C15 2C6 C2 6C1 " +   \!
           "2I4 C6 C2 2C3 2C12 C07 2C4"                 !
    NFF.KEY$ = PACK$("999999")                          ! Arma llave para leer
    READ FORM LEC$;#NFF.REC% KEY NFF.KEY$;FISKEY$,     \! Lee Reg Var/Fiscales
         FISNIT$,FISCAL$,FISDEP$,FISNOM$,FISDIR$,      \! Llave,Nit,CF,Nom,Dir
         FISSUC$,FISDOM$,                              \!
         FISPRO$,FISSOL$,FISLIC$,FISFRI$,FISFUM$,      \! Prov,Solic,Licen,RCF
         FISNCA$,FISIV1$,FISIV2$,FISIV3$,FISIV4$,      \!
         FISIV5$,FISIV6$,FISTCO%,FISTCD%,FISFUC$,      \!
         FISCIU$,FISFRA$,FISFRU$,FISNRA$,FISNRU$,      \!
         FISRES$,FISNAU$,FISPWD$                        ! Ult/Camb,Rsrv,Cod/Pw
    IF NFF.ERCO$ = "  " THEN BEGIN                      ! Si existe Registro
       NFF.OBSER$ = "Y"                                 !
       IF TO.HEAD1.FONT = 1 THEN                       \!
          FISNOM$ = FISNOM$+"  "                        !
       IF TO.HEAD1.FONT = 2 THEN                       \!
          FISNOM$ = FISNOM$+"  "      :                \!
          FISNOM$ = CHR$(27)+CHR$(58)+FISNOM$           !
       IF TO.HEAD1.FONT = 3 THEN                       \!
          FISNOM$ = LEFT$(FISNOM$,19)+"  "      :      \!
          FISNOM$ = CHR$(27)+CHR$(14)+FISNOM$           !
       IF TO.HEAD1.FONT = 4 THEN                       \!
          FISNOM$ = LEFT$(FISNOM$,19)+"  "      :      \!
          FISNOM$ = CHR$(27)+CHR$(23)+FISNOM$           !
!       NFF.RAZON$ = FISNOM$                             !
       
       NFF.RAZON$ = TO.HEADERLINE1$
       NFF.FON = MATCH("  ",NFF.RAZON$,1)               ! Busca longitud del
       IF NFF.FON <> 0 THEN BEGIN                       ! nombre, le quita
          NFF.RAZON$ = LEFT$(NFF.RAZON$,NFF.FON-1)      ! los espacios der.
          IF LEN(NFF.RAZON$) < 20 THEN BEGIN            ! y centra el nombre
             IF TO.HEAD1.FONT=1 THEN NFF.FON=2.5*15     !
             IF TO.HEAD1.FONT=2 THEN NFF.FON=2.5*12     !
             IF TO.HEAD1.FONT=3 THEN NFF.FON=2.5*7.5    !
             IF TO.HEAD1.FONT=4 THEN NFF.FON=2.5*7.5    !
             NFF.RAZON$ = STRING$((NFF.FON -           \!
                 LEN(NFF.RAZON$))/2," ") +             \!
                 LEFT$(NFF.RAZON$,19)                   !
          ENDIF                                         !
       ENDIF                                            !
!       TO.HEADERLINE1$ = NFF.RAZON$                     !
       NFF.NITR$ = RIGHT$(UNPACK$(FISNIT$),10)          !
       NFF.NITR$ = LEFT$(NFF.NITR$,3)+"."+MID$(NFF.NITR$,4,3)+"."+ \!
            MID$(NFF.NITR$,7,3)+"-"+RIGHT$(NFF.NITR$,1) !
       IF TO.HEAD2.FONT = 2 THEN                       \!
          NFF.NITR$ = CHR$(27)+CHR$(58) + NFF.NITR$     !
       IF TO.HEAD2.FONT = 3 THEN                       \!
          NFF.NITR$ = CHR$(27)+CHR$(14) + NFF.NITR$     !
       IF TO.HEAD2.FONT = 4 THEN                       \!
          NFF.NITR$ = CHR$(27)+CHR$(23) + NFF.NITR$     !
       NFF.NITR$="    NIT: " + NFF.NITR$                !
!       TO.HEADERLINE2$ = NFF.NITR$                      !
    ENDIF ELSE BEGIN                                    ! Fin Cond
       NFF.OBSER$ = "N"                                 ! Error Empresa
    ENDIF                                               ! Fin Cond
END SUB                                                 !
!------------------------------------------------------------------------------
!                Rutina de Registro De Transaccion       
!------------------------------------------------------------------------------

SUB NFF.GRATER PUBLIC                                     !

    IF NFF.RELEV% > 0 THEN BEGIN                          !
       NFF.KEY$  = PACK$(RIGHT$("000000"+NFF.TER$,6))     !
       N.INICIAL$=LEFT$(N.INICIAL$,7)+NIV$                ! No. Inicial Serie
       N.FINAL$  =LEFT$(N.FINAL$,7)+NFV$                  ! No. Final Serie
       N.INICIO.DIA$ = PACK$(NIV$)                        ! No. Inicio Dia
       AUTH.DIAN$ = NFFNRA$                               ! No. Autoriz DIAN
       FECHA.AUTH$= PACK$("20")+NFFNRA$+                 \!
                    PACK$(LEFT$(TIME$,4))                 ! Toma Fech Aut y Hoy
    ENDIF                                                 !  
    !ON ERROR GOTO NFF.ERTOF                            !

    N.TRX$ = PACK$(RIGHT$(UNV$,8))                     ! Ult Num Trans Usado
    FECHA.TRX$ = "19"                                  ! Siglo XX
    IF LEFT$(DATE$,2) <> "99" THEN FECHA.TRX$="20"     ! Fecha SIGLO XXI
    FECHA.TRX$ = FECHA.TRX$ + DATE$ + TIME$            ! Agrega Fecha y Hora
    FECHA.TRX$ = PACK$(FECHA.TRX$)                     ! Empaqueta Fecha
    FOR1$ = "C2 C10 2C15 C3 C6 2C4 C6"                 ! Formato lect Terminal
    TS.ER.RETURN = -1                                  ! Retorna si NO Abre OK
    WRITE FORM FOR1$;#NFF.CTR%;TERMINAL$,N.ACTIVO$,   \! Graba Listterm
               N.INICIAL$,N.FINAL$,AUTH.DIAN$,        \! actualizando SOLO
               FECHA.AUTH$,N.INICIO.DIA$,             \! N.TRX$ y FECHA.TRX$
               N.TRX$,FECHA.TRX$                       !
  NFF.OCUPADO:                                         !
    IF TS.ER.RETURN THEN BEGIN                         ! 
       IF NFF.RELEV% <> 2 THEN BEGIN                   !
          OPEN NFF.A2$ KEYED RECL 200 AS NFF.REC%      !
       ENDIF                                           !
       IF TS.ER.RETURN THEN BEGIN                      ! Si Apertura esta OK
          NFF.KEY$ = PACK$(RIGHT$("000000"+NFF.TER$,6))! No de Terminal
          NFFKEY$ = NFF.KEY$                           ! No de Terminal
          FISACT$ = ACT$                               ! No Activo
          FISNIV$ = PACK$(NIV$)                        ! No Inicial Vigente
          FISNFV$ = PACK$(NFV$)                        ! No Final Vigente
          FISUNV$ = PACK$(UNV$)                        ! Ultimo No. Usado
          FISNIN$ = PACK$(NIN$)                        ! No Inicial Nuevo
          FISNFN$ = PACK$(NFN$)                        ! No Final Nuevo
          FISFNV$ = PACK$(FNV$)                        ! Fecha Numerac Vigente
          FISFNN$ = PACK$(FNN$)                        ! Fecha Numerac Nueva
          FISFEQ$ = PACK$(FEQ$)                        ! Fecha X
          FISFZE$ = PACK$(FZE$)                        ! Fecha Z
          FISAUV$ = AUV$                               ! Autoriz Vigente
          FISAUN$ = AUN$                               ! Autoriz Nueva
          FISPR1$ = NFF.PF1$                           ! Prefijo 1
          FISPR2$ = PF2$                               ! Prefijo 2
          LEC$ = "C3 C8 5C4 4C6 3I4 2C15 2C4 C95"      !
          WRITE FORM LEC$;#NFF.REC%;                  \! Graba Reg Terminales
                FISKEY$,FISACT$,FISNIV$,FISNFV$,      \! Llave,Actv,Rango,Fec
                FISUNV$,FISNIN$,FISNFN$,FISFNV$,      \! Ult/Usd,Rango/Nue,Fec
                FISFNN$,FISFEQ$,FISFZE$,FISCON%,      \!
                FISNEQ%,FISNZE%,FISAUV$,FISAUN$,      \! Fecha X y Z Contad.
                FISPR1$,FISPR2$,FISUSR$                ! Autorizac,Usuario
          IF NFF.RELEV% <> 2 THEN CLOSE NFF.REC%       !
       ENDIF ELSE BEGIN                                !
          TS.ER.RETURN = -1                            !
          WAIT ; 500                                   !
          GOTO NFF.OCUPADO                             !
       ENDIF                                           !
    ENDIF ELSE BEGIN                                   !
       ts.temp1$="   **Trx/Offline.."+NFF.PF1$+"-"+UNV$+"**"
       Call U.Imprime(ts.temp1$,6100H)
!       CALL QXL.TSUPEC21(ts.temp1$)                       !
!       WRITE FORM "C38";#36;ts.temp1$                     !
    ENDIF                                              !
    NFF.RELEV% = 0 
!  NFF.ERTOF:
!    IF ERR = "TO" AND ERRF% = 77 THEN RESUME

END SUB                                                !

!------------------------------------------------------------------------------
!             Rutina para Control de la Numeracion       
!------------------------------------------------------------------------------

SUB NFF.VALNUM                                         !
  NFF.NEWSERIE:                                        !
!   write #34;"Ult:"+unv$+" NTra:"+str$(nff.ntra)
    IF VAL(UNV$) = 0 THEN BEGIN                        ! Si Ult Num Asig =0
       UNV$=RIGHT$("00000000"+STR$(VAL(NIV$)-1),8)     ! Toma Inicio de Serie
    ENDIF                                              !
    NFF.NTRA = VAL(UNV$)                               ! Toma No 1er Transac 
    IF VAL(NFV$)-NFF.NTRA < 5001 THEN BEGIN            ! Toma No 1er Transac
       NFF.CADA10% = MOD(VAL(NFV$)-NFF.NTRA,10)        ! Calcula Residuo
       IF (NFF.CADA10% = 0 OR                         \!
           VAL(NFV$)-NFF.NTRA < 1500) AND             \!
           VAL(NIN$) = 0 THEN BEGIN                    !
              CLEARS NFF.DIS%                          !
              Call VISORES4690(1,"SE AGOTA LA SERIE   ","FISCAL. QUEDAN: "+ \! por el Visor
                                RIGHT$("000"+STR$(VAL(NFV$)-NFF.NTRA-1),3),1200,"L")
              ts.temp1$ =                             \!
                "**SE AGOTA SERIE FISCAL. QUEDAN: "+  \! y suspend
                 RIGHT$("000"+STR$(VAL(NFV$)       -  \!
                 NFF.NTRA-1),3) + "**"                 !
!              WRITE FORM "C38 A1";#34;ts.temp1$           ! Imprime en SJ
!              WRITE FORM "C38 A1";#36;ts.temp1$           ! Imprime en SJ
!------------------------------------------------------v-----------------------
              CALL U.IMPRIME(ts.temp1$,6100H)            !
!------------------------------------------------------!-----------------------
       ENDIF                                           ! 
    ENDIF                                              ! 
    IF VAL(UNV$) >= VAL(NFV$) THEN BEGIN               ! Si se agota la Serie
       IF VAL(NIN$) = 0 THEN BEGIN                     ! y NO hay Serie Relevo
          CLEARS NFF.DIS%                              ! avisa el Error
          Call VISORES4690(1," NO HAY NUMERACION  ","FISCAL. CONSULTE..!",1200,"L")
          NFF.S34$ = "NO HAY NUMERACION FISCAL..! "    !    Ses34 Cambio 3
!         WRITE FORM "C38 A1";#34;NFF.S34$             ! sesion de Vta
!------------------------------------------------------v-----------------------
          CALL U.IMPRIME(NFF.S34$,4100H)              !
!------------------------------------------------------!-----------------------
          STOP                                         ! Vta en la Terminal
       ENDIF ELSE BEGIN                                ! sig Transac 
          CLEARS NFF.DIS%                              ! avisa el Error
          Call VISORES4690(1,"RELEVO DE NUMERACION  ","FISCAL..!",2000,"L")
          NIV$ = NIN$                                  !
          NFV$ = NFN$                                  !
          NIN$ = "00000000"                            !
          NFN$ = "00000000"                            !
          UNV$ = "00000000"                            !
          NFF.PF1$ = PF2$                              !
          PF2$ = "0000"                                !
          NFF.RELEV% = NFF.RELEV% + 1                  !
          GOTO NFF.NEWSERIE                            !
       ENDIF                                           !
    ENDIF                                              !
END SUB                                                !
!------------------------------------------------------------------------------
!             Rutina para Reasignar Variables  de Terminal   
!------------------------------------------------------------------------------
SUB NFF.WRIF                                           !

    FECHA.TRX$ = "19"                                  ! Siglo XX
    IF LEFT$(DATE$,2) <> "99" THEN FECHA.TRX$="20"     ! Fecha SIGLO XXI
    FECHA.TRX$ = FECHA.TRX$ + DATE$ + TIME$            ! Agrega Fecha y Hora
    FECHA.TRX$ = PACK$(FECHA.TRX$)                     ! Empaqueta Fecha

    FISACTRT$ = LEFT$(N.ACTIVO$,7)                     !
    FISNIVRT$ = N.INICIAL$                             !
    FISNFVRT$ = N.FINAL$                               !
    FISAUTRT$ = AUTH.DIAN$                             !
    FISFAURT$ = FECHA.AUTH$                            !
    FISINIRT$ = PACK$(N.INICIO.DIA$)                   !
    FISNTRRT$ = PACK$(N.TRX$)                          !
    FISFTRRT$ = FECHA.TRX$                             !
    GoTo GRABACION.HT
    
    CALL U.IMPRIME(" ",4100H)                      !
    ts.temp1$=STRING$(19," -")
    CALL U.IMPRIME(ts.temp1$,6100H)                      !
    ts.temp1$="     Registro de Numeracion Inicial"
    CALL U.IMPRIME(ts.temp1$,6100H)                      !
    ts.temp1$=STRING$(19," -")
    CALL U.IMPRIME(ts.temp1$,6100H)                      !
    ts.temp1$="      Terminal: "+NFF.TER$                 !
    CALL U.IMPRIME(ts.temp1$,6100H)                      !
    ts.temp1$="    No. Activo: "+N.ACTIVO$          !
    CALL U.IMPRIME(ts.temp1$,6100H)                      !
    ts.temp1$="    Numeracion: "                          !
    CALL U.IMPRIME(ts.temp1$,6100H)                      !

    IF LEFT$(N.INICIAL$,3) = "000" THEN BEGIN          !
       ts.temp1$="       Prefijo: "+               \!
                            MID$(N.INICIAL$,4,4)       !
       CALL U.IMPRIME(ts.temp1$,6100H)                   !
       ts.temp1$="      #Inicial: "+               \!
                    MID$(FISNIVRT$,4,4)+              \!
                    "-" + RIGHT$(FISNIVRT$,8)          !
       CALL U.IMPRIME(ts.temp1$,6100H)                      !
       ts.temp1$="        #Final: "+               \!
                    MID$(FISNFVRT$,4,4)+              \!
                    "-"+RIGHT$(FISNFVRT$,8)            !
       CALL U.IMPRIME(ts.temp1$,6100H)                   !
    ENDIF ELSE BEGIN                                   !
       ts.temp1$="       Prefijo: "+"0000"          !
       CALL U.IMPRIME(ts.temp1$,6100H)                      !
       ts.temp1$="      #Inicial: "+               \!
                    LEFT$(FISNIVRT$,7)+               \!
                    "-" + RIGHT$(FISNIVRT$,8)          !
       CALL U.IMPRIME(ts.temp1$,6100H)                      !
       ts.temp1$="        #Final: " +              \!
                    LEFT$(FISNFVRT$,7)+               \!
                    "-" + RIGHT$(FISNFVRT$,8)          !
        CALL U.IMPRIME(ts.temp1$,6100H)                   !
    ENDIF                                              !
    ts.temp1$=" Fecha Autoriz: "+"20"+UNPACK$(FISFRA$)
    CALL U.IMPRIME(ts.temp1$,6100H)                      !

    IF LEFT$(N.INICIAL$,3) = "000" THEN BEGIN          !
       ts.temp1$="  #Inicial Hoy: "+               \!
                    MID$(FISNIVRT$,4,4) + "-" +       \!
                    RIGHT$("00000000" + STR$(VAL(     \!
                    UNPACK$(N.INICIO.DIA$))),8)        !
       CALL U.IMPRIME(ts.temp1$,6100H)                   !
    ENDIF ELSE BEGIN                                   !
       ts.temp1$="  #Inicial Hoy: "+               \!
                    LEFT$(FISNIVRT$,7) + "-" +        \!
                    RIGHT$("00000000" + STR$(VAL(     \!
                    UNPACK$(N.INICIO.DIA$))),8)        !
       CALL U.IMPRIME(ts.temp1$,6100H)                   !

    ENDIF                                              !
    ts.temp1$=STRING$(19," -")                            ! Arma Linea de Texto
    CALL U.IMPRIME(ts.temp1$,6100H)                      !
    Call U.Imprime(" ",4200H)
    Call U.CORTACR
    GRABACION.HT:    
    NFF.F16$ = "C7 2C15 C3 C6 2C4 C6"                  ! Fmto Reg 16 
    NFF.F99$ = NFF.F16$ + " Wri"                       ! Formato de Lect
    WRITE FORM NFF.F16$;#NFF.HTO%,NFF.H16%;           \!
          FISACTRT$,FISNIVRT$,FISNFVRT$,FISAUTRT$,    \!
          FISFAURT$,FISINIRT$,FISNTRRT$,FISFTRRT$      !
END SUB                                                !
!------------------------------------------------------------------------------
!                 Rutina para Leer Variables  de Terminal   NFFINREC
!------------------------------------------------------------------------------
SUB NFF.LEANUMER                                       !
    NFF.ERCO$ = "  "                                   !
    NFF.KEY$  = PACK$(RIGHT$("000000"+NFF.TER$,6))     !
    LEC$ = "C3 C8 5C4 4C6 3I4 2C15 2C4 2C3 C89"        !
    READ FORM LEC$;#NFF.REC% KEY NFF.KEY$;            \! Lee Reg Var/Fiscales
         FISKEY$,FISACT$,FISNIV$,FISNFV$,FISUNV$,     \! Llave,Actv,Rango,Fec
         FISNIN$,FISNFN$,FISFNV$,FISFNN$,FISFEQ$,     \! Ult/Usd,Rango/Nue,Fec
         FISFZE$,FISCON%,FISNEQ%,FISNZE%,FISAUV$,     \! Fecha X y Z Contad.
         FISAUN$,FISPR1$,FISPR2$,FISFUV$,FISFUN$,     \!
         FISUSR$                                       ! Autorizac,Usuario
    IF NFF.ERCO$ = "  " THEN BEGIN                     ! Si existe Registro
       ACT$ = FISACT$                                  !
       NIV$ = UNPACK$(FISNIV$)                         !
       NFV$ = UNPACK$(FISNFV$)                         !
       FNV$ = UNPACK$(FISFNV$)                         !
       UNV$ = UNPACK$(FISUNV$)                         !
       NIN$ = UNPACK$(FISNIN$)                         !
       NFN$ = UNPACK$(FISNFN$)                         !
       FNN$ = UNPACK$(FISFNN$)                         !
       FEQ$ = UNPACK$(FISFEQ$)                         ! Fecha X
       FZE$ = UNPACK$(FISFZE$)                         ! Fecha Z
       AUV$ = FISAUV$                                  !
       AUN$ = FISAUN$                                  !
       NFF.PF1$ = FISPR1$                              !
       PF2$ = FISPR2$                                  !
       NFF.PF1$ = TRANSLATE$(NFF.PF1$," ","0")         ! Reemp. Espac por CERO
       PF2$ = TRANSLATE$(PF2$," ","0")                 ! Reemp. Espac por CERO
!       write #34;"paso3:"+unpack$(nff.key$)+ " St:"+nff.erco$+":"
       CALL NFF.VALNUM                                 !
!       TS.temp1$="LECT REG/NFFINREC OK. Ter:"+NFF.TER$!
!       Call U.Imprime(TS.TEMP1$,4100H)
!       write #34;ts.temp1$
!       WRITE FORM "C38 A1";#34;TS.SAVPRT$              !
!------------------------------------------------------v-----------------------
       ts.temp1$=TS.temp1$                               !
!       CALL QXL.TSUPEC21(ts.temp1$)                       !
       CALL U.IMPRIME(ts.temp1$,4100H)                   !
!------------------------------------------------------------------------------
! write form "c38";#34;unpack$(nff.key$)+"uvig="+unv$+" Inic="+nin$+" Final="+nfn$
    ENDIF                                              !
END SUB                                                !
!------------------------------------------------------------------------------
!                 Rutina para Leer Variables  de Terminal   
!------------------------------------------------------------------------------
SUB NFF.LEATER PUBLIC                                  !
    CALL NFF.LEANUMER                                  !
    NFF.ERCO$ = "  "                                   !
    NFF.KEY$  = PACK$(RIGHT$("0000"+NFF.TER$,4))       !
    FOR1$ = "C2 C10 2C15 C3 C6 2C4 C6"                 ! Formato lect Terminal
    TERMINAL$ = NFF.KEY$                               !
    TS.ER.RETURN = -1                                  !
    READ FORM FOR1$;#NFF.CTR% KEY TERMINAL$;          \!
         FILLER$,N.ACTIVO$,N.INICIAL$,N.FINAL$,       \!
         AUTH.DIAN$,FECHA.AUTH$,N.INICIO.DIA$,        \!
         N.TRX$,FECHA.TRX$                             !
    IF NFF.RELEV% = 1 THEN BEGIN                       !
       NFF.RELEV% = 2                                  !
       CALL NFF.GRATER                                 !
    ENDIF                                              !
    IF TS.ER.RETURN THEN BEGIN                         !
       IF OPC.TV$="0" THEN NFF.IDE$=NFF.AL$+NFF.TER$   !
       IF OPC.TV$="1" THEN  NFF.IDE$="0000"             !
       IF NFF.ERCO$ = "  " THEN BEGIN                  ! Si existe Registro
          ACT$ = N.ACTIVO$                             ! No. de Activo
          NIV$ = RIGHT$(N.INICIAL$,8)                  ! No. Inicial Serie
          NFV$ = RIGHT$(N.FINAL$,8)                    ! No. Final Serie
          AUV$ = LEFT$(UNPACK$(FECHA.AUTH$),8)+       \! No. Autoriz DIAN
                 "0"+UNPACK$(AUTH.DIAN$)               !
          FNV$ = MID$(UNPACK$(FECHA.AUTH$),3,10)+"00"  ! Fecha de Autoriz
          UNV$ = UNPACK$(N.TRX$)                       ! Ultimo No.Asignado
          NFF.PF1$ = "0000"                            !
          IF LEFT$(N.INICIAL$,3)="000" THEN           \!
             NFF.PF1$ = MID$(N.INICIAL$,4,4)           !
          PF2$ = NFF.PF1$                              !
          NFF.PF1$ = TRANSLATE$(NFF.PF1$," ","0")      ! Reemp. Espac por CERO
          PF2$ = TRANSLATE$(PF2$," ","0")              ! Reemp. Espac por CERO
!          write #34;"paso2:"+unpack$(nff.key$)+ " St:"+nff.erco$+":"
          CALL NFF.VALNUM                              !
!          TS.TEMP1$="LECTURA REGISTRO NFFTTERM OK..! "!
!          CALL U.IMPRIME(ts.temp1$,4100H)                !
!------------------------------------------------------------------------------
       ENDIF                                           ! 
    ENDIF                                              ! 
    NFF.ERCO$ = "  "                                   !
    NFF.KEY$  = PACK$(RIGHT$("000000"+TS.TERMINAL$,6)) !
    LEC$ = "C3 C8 5C4 4C6 3I4 2C15 2C4 2C3 C89"        !
    READ FORM LEC$;#NFF.REC% KEY NFF.KEY$;            \! Lee Reg Var/Fiscales
         FISKEY$,FISACT$,FISNIV$,FISNFV$,FISUNV$,     \! Llave,Actv,Rango,Fec
         FISNIN$,FISNFN$,FISFNV$,FISFNN$,FISFEQ$,     \! Ult/Usd,Rango/Nue,Fec
         FISFZE$,FISCON%,FISNEQ%,FISNZE%,FISAUV$,     \! Fecha X y Z Contad.
         FISAUN$,FISPR1$,FISPR2$,FISFUV$,FISFUN$,     \!
         FISUSR$                                       ! Autorizac,Usuario
    IF NFF.ERCO$ = "  " THEN BEGIN                     ! Si existe Registro
       ACT$ = FISACT$                                  !
       NIV$ = UNPACK$(FISNIV$)                         !
       NFV$ = UNPACK$(FISNFV$)                         !
       FNV$ = UNPACK$(FISFNV$)                         !
       UNV$ = UNPACK$(FISUNV$)                         !
       NIN$ = UNPACK$(FISNIN$)                         !
       NFN$ = UNPACK$(FISNFN$)                         !
       FNN$ = UNPACK$(FISFNN$)                         !
       FEQ$ = UNPACK$(FISFEQ$)                         ! Fecha X
       FZE$ = UNPACK$(FISFZE$)                         ! Fecha Z
       AUV$ = FISAUV$                                  !
       AUN$ = FISAUN$                                  !
       NFF.PF1$ = FISPR1$                              !
       PF2$ = FISPR2$                                  !
       NFF.PF1$ = TRANSLATE$(NFF.PF1$," ","0")         ! Reemp. Espac por CERO
       PF2$ = TRANSLATE$(PF2$," ","0")                 ! Reemp. Espac por CERO
!       write #34;"paso4:"+unpack$(nff.key$)+ " St:"+nff.erco$+":"
       CALL NFF.VALNUM                                 !
!       TS.TEMP1$="LECTURA DE REGISTRO NFFINREC OK..! "!
!       CALL U.IMPRIME(ts.temp1$,4100H)                      !
!------------------------------------------------------------------------------
    ENDIF                                              !
END SUB                                                !

!------------------------------------------------------------------------------
!                 Rutina para Leer Hard Totals Reg 16
!------------------------------------------------------------------------------

SUB NFF.LEC16                                          ! Lectura Totales X
                                                       !
  NFF.F16$ = "C7 2C15 C3 C6 2C4 C6"                    ! Fmto Reg 16 
  NFF.F99$ = NFF.F16$ + " Lec"                         ! Formato de Lect
  NFF.ERCO$ = "  "                                     !
  READ  FORM NFF.F16$;#NFF.HTO%,NFF.H16%;             \!
             FISACTRT$,FISNIVRT$,FISNFVRT$,FISAUTRT$, \!
             FISFAURT$,FISINIRT$,FISNTRRT$,FISFTRRT$   !
  IF NFF.ERCO$ = "TR" THEN BEGIN                       !
     CALL NFF.WRIF                                     !
     NFF.ERCO$ = "  "                                  !
  ENDIF                                                !
END SUB                                                !
!------------------------------------------------------------------------------
!            Verifica Registros en Hard Totals vs. NFFTTERM
!------------------------------------------------------------------------------
SUB FNFF.CLI PUBLIC                                    !
  CLEARS NFF.DIS%                                      ! Borra Display
  WRITE #NFF.DIS%  ;                                  \!
        " VERIFICACION DE LA  NUMERACION FISCAL  "     ! Mensaje de Inicio 
  WAIT;NFF.DUR%                                        ! Pausa
  CALL NFF.LEC16                                       !
  CLEARS NFF.DIS%                                      !
  IF FISACTRT$ = LEFT$(N.ACTIVO$,7) AND               \!
     FISNIVRT$ = N.INICIAL$         AND               \!
     FISNFVRT$ = N.FINAL$   THEN BEGIN                 !
!        WRITE #NFF.DIS%;                              \!
!           "TERMINAL ACTIVA CON  NUMERACION FISCAL.!"  !
!        WAIT;NFF.DUR%                                  ! Pausa

        TS.TEMP1$="POS ACTIVA CON RANGO FISCAL.! " + \! Arma Linea de Texto
                   LEFT$(TIME$,2)   + ":"           + \!
                   MID$(TIME$,3,2)  + ":"           + \!
                   RIGHT$(TIME$,2)                     !
!        TS.FORMCR$ = "C38 A1"                          !
!        TS.SAVPRT.OPT = 6100H                          !
!        TS.LINETYPE   = 29                             !
!        WRITE FORM "C38 A1";#34;TS.SAVPRT$            !
!        WRITE FORM "C38 A1";#36;TS.SAVPRT$
!        CALL TSPREC01                                  ! Imp CR Lin Buff 6100H
!------------------------------------------------------v-----------------------
!        ts.temp1$=TS.TEMP1$                              !
!        CALL U.IMPRIME(ts.temp1$,6100H)                  !
!------------------------------------------------------------------------------
        IF NFF.IDE$ = "0000" THEN BEGIN                ! Si Identif=Alma-Term
           NFF.RAN$ = "   RANGO:"                      !
           NFF.RAN$=NFF.RAN$ + RIGHT$(FISNIVRT$,12)+  \!
                    "-" + RIGHT$(FISNFVRT$,12)         ! IMPR RANGO NUMERAC
        ENDIF ELSE BEGIN                               !
           NFF.RAN$ = "RANGO:"                         !
           NFF.RAN$=NFF.RAN$ + FISNIVRT$ +            \!
                    "-" + FISNFVRT$                    ! IMPR RANGO NUMERAC
        ENDIF                                          !

!WRITE #34;FISNFVRT$+ "RANGO" 
        TS.TEMP1$ = NFF.RAN$                          ! Arma Linea de Texto
!        TS.SAVPRT.OPT = 6200H                          !
!        TS.LINETYPE=29                                 !
!        WRITE FORM "C38 A2";#34;TS.SAVPRT$             !
!        WRITE FORM "C38 A2";#36;TS.SAVPRT$             !
!        CALL TSPREC01                                  ! Imp CR Lin Buff 6100H
!------------------------------------------------------v-----------------------
! OJO        ts.temp1$=TS.TEMP1$                              !
!        CALL U.IMPRIME(ts.temp1$,4100H)                  !
!------------------------------------------------------------------------------
  ENDIF ELSE BEGIN                                     !
     CLEARS NFF.DIS%                                   !
     WRITE #NFF.DIS%;                                 \!
           " TERMINAL ACTIVA SIN  NUMERAC/FISCAL.!  "  !
!     TS.TEMP1$ = "TERMINAL ACTIVA SIN NUMERAC/FISCAL.!" ! Arma Linea de Texto
!     ts.temp1$=TS.TEMP1$                                 !
!     CALL U.IMPRIME(ts.temp1$,6100H)                     !
!------------------------------------------------------------------------------
!     TS.TEMP1$ = "** REQUIERE RESET TERMINAL Y STC.!" !
!     ts.temp1$=TS.TEMP1$                                 !
!     CALL U.IMPRIME(ts.temp1$,6100H)                     !
!------------------------------------------------------------------------------
!     TS.TEMP1$ = "** ERROR NUMERACION FISCAL..! "     !
!     ts.temp1$=TS.TEMP1$                                 !
!     CALL U.IMPRIME(ts.temp1$,6100H)                     !
!     TS.TEMP1$ = "        DATOS DESDE NFFTTERM...."   !
!     ts.temp1$=TS.TEMP1$                                 !
!     CALL U.IMPRIME(ts.temp1$,6100H)                     !
!------------------------------------------------------------------------------
!     TS.TEMP1$ = "  #de Activo: "+N.ACTIVO$           !
!     ts.temp1$=TS.TEMP1$                                 !
!     CALL U.IMPRIME(ts.temp1$,6100H)                     !
!------------------------------------------------------------------------------
!     TS.TEMP1$ = "#Tr Inic/Dia: "+                   \!
!                RIGHT$("00000000" +                   \!
!                STR$(VAL(UNPACK$(N.INICIO.DIA$))),8)   !
!     ts.temp1$=TS.TEMP1$                                  !
!     CALL U.IMPRIME(ts.temp1$,6100H)                      !
!------------------------------------------------------------------------------
!     WAIT;NFF.DUR%                                     ! Pausa

     CALL NFF.WRIF                                     !
!    STOP                                              !
  ENDIF                                                !

!  TO.FRANK.TXT$(NFF.FOR%,1)=RIGHT$(TO.HEADERLINE1$,34) ! Linea de Razon Social
!  TO.FRANK.TXT$(NFF.FOR%,3)=RIGHT$(TO.HEADERLINE2$,34) ! Linea de NIT

  TO.FRANK.TXT$(NFF.FOR%,1)=NFF.RAZON$                 !Linea de Razon Social
  TO.FRANK.TXT$(NFF.FOR%,2)=NFF.NITR$                  !Linea de Nit
  TO.FRANK.TXT$(NFF.FOR%,3)="  "+NOALM$                !Linea de Almacen
                                                       ! Linea de Direcc
  NFF.N4$ = "  "                                       ! ELIMINA No.Ctrl Fiscal
  TO.FRANK.TXT$(NFF.FOR%,04) = NFF.N4$                 ! Lin Ctrl Fiscal
  NFF.N4$ = TO.FRANK.TXT$(11,2)                        !

  NFF.N5$ = "        "                                 ! ELIMINA Lin Prov/Sol
  TO.FRANK.TXT$(NFF.FOR%,05) = NFF.N5$                 ! Imprime Nro Ctrl Fiscal
  TO.FRANK.TXT$(NFF.FOR%,06) = STRING$(38," ")         !
  TO.FRANK.TXT$(NFF.FOR%,12)=STRING$(38," ")           !
  TO.FRANK.TXT$(NFF.FOR%,13)=STRING$(38," ")           !
  TO.FRANK.TXT$(NFF.FOR%,14) = STRING$(38,"-")         !
  TO.FRANK.TXT$(NFF.FOR%,15) = "  c.c:@aaaaaaaaaaaa de:"      
  TO.FRANK.TXT$(NFF.FOR%,16) = "      Firma del Cliente"  
  TO.FRANK.TXT$(NFF.FOR%,17) = "    @g"                !
!  for ii% = 1 to 20
!      write form "c38";#35;to.frank.txt$(fis.for%,ii%)
!  next ii%
END SUB
!------------------------------------------------------------------------------
!               Subrutina para Incrementar Num de Factura  
!------------------------------------------------------------------------------
SUB NFF.ASIGNANUM PUBLIC                               !
    IF NOT TS.STANDALONE THEN BEGIN                    ! Si esta en Linea 
       CALL NFF.VALNUM                                 ! Valida Rangos Numerac
    ENDIF                                              !
    IF TS.TRX.STATUS <> 100 THEN BEGIN                 !
       NFF.NTRA = NFF.NTRA + 1                         ! Incr No Transac
    ENDIF                                              !
    UNV$ = RIGHT$("00000000"+STR$(NFF.NTRA),8)         ! Obtiene Nuevo No.
    IF NFF.IDE$ <> "0000" THEN BEGIN                   ! Si Identif=Alma-Term
       NFF.NUT$ = NFF.IDE$ + RIGHT$                   \! Arma Fctura con
                  ("00000000"+STR$(NFF.NTRA),8)        ! Identificador
       NFF.TRLOG$ = PACK$("0" + NFF.NUT$)              ! Factura va al LOG
    ENDIF                                              !
    IF NFF.IDE$ = "0000" THEN BEGIN                    ! Si NO hay IDENTIF
       NFF.NUT$ = NFF.PF1$ + " " + RIGHT$             \! Asume Prefijo ASC 4
                  ("00000000000"+STR$(NFF.NTRA),11)    ! y No de Factura
       NFF.TRLOG$ = PACK$(RIGHT$("0000000000000000"+  \! Factura va al LOG
                    STR$(NFF.NTRA),16))                ! 
    ENDIF                                              !
    TO.FRANK.TXT$(NFF.FOR%,07) =                      \! Num de Factura con
             "**" + NFF.NUT$ + "**"                    ! formato Preimpr
    TO.FRANK.TXT$(NFF.FOR%,07) =                      \!
    TO.FRANK.TXT$(NFF.FOR%,07)+"   "+STR$(NFF.TVTA%)   !
END SUB
!------------------------------------------------------------------------------
!             Subrutina de Impresion del Numero Fiscal
!------------------------------------------------------------------------------
SUB NFF.IMPFAC PUBLIC                                  !
!Call SAVE.PRINT

 IF NFF.IDE$ = "0000" THEN BEGIN                    ! Si NO hay IDENTIF
       NFF.LINF$= "        DOCUM"   +                         \!
                "/EQUIVALENTE:"+MID$(NFF.NUT$,1,2)+   \!
                "-" + RIGHT$(NFF.NUT$,8) + "**        "!
    ENDIF ELSE BEGIN                                   !
       NFF.LINF$=                      "DOCUM"      + \!
                "/EQUIVALENTE:"+MID$(NFF.NUT$,2,4)+   \!
                "-" + RIGHT$(NFF.NUT$,8) + "**"        !
    ENDIF                                              !
    IF TS.TRX.STATUS <> 100 THEN BEGIN                 ! y NO hay VOID en curso

       TS.TEMP1$  = "DOCUM/EQUIVALENTE:"+MID$(NFF.NUT$,1,2)+\!
                   "-" + RIGHT$(NFF.NUT$,8)+"**"
       Call U.IMPRIME(TS.TEMP1$,6100H)
!      Call U.IMPRIME(TS.TEMP1$,2100H)                          
       Call U.IMPRIME(NFF.RAN$,6100H)
!      Call U.IMPRIME(NFF.RAN$,2100H)       
      
       NFF.AUX$ = " -Resol.:"+FISNRA$+" de "     +    \!
       MID$(NFF.M$,VAL(MID$(UNPACK$(FISFRA$),3,2)) *  \!
       3-2,3)+" "+MID$(UNPACK$(FISFRA$),5,2)+"/" +    \!
        "20"+MID$(UNPACK$(FISFRA$),1,2)+"-"            ! 
        
        Call U.IMPRIME(NFF.AUX$,6100H)
!       Call U.IMPRIME(NFF.AUX$,2100H)
        
    ENDIF                                              !
    NFF.AUX$ = ""                                      !
    
!Call RESTORE.PRINT
END SUB                                                !
!------------------------------------------------------------------------------
!            Obtiene Numero        y prepara Logo DIAN
!------------------------------------------------------------------------------
SUB FNFF.NUM PUBLIC                                    !
    CALL NFF.ASIGNANUM                                 !
    IF NOT TS.STANDALONE THEN BEGIN                    ! Si esta en Linea 
       CALL NFF.GRATER                                 ! Graba Term en Ctrldr
    ENDIF                                              !
    IF VAL(NFF.MAT$) > 6 THEN BEGIN                    ! *** NO HAY LOGO DIANDO
!       WRITE FORM "C38 A2";#34;" "                     ! Avanza espacios
!------------------------------------------------------v-----------------------
       ts.temp1$=" "                                   !
       CALL U.IMPRIME(ts.temp1$,6100H)                 !
!------------------------------------------------------------------------------
    ENDIF ELSE BEGIN                                   !
    
       CALL NFF.IMPFAC                                 !
       
    ENDIF                                              !
    TS.TEMP1I1 = 99                                    ! Envia al LOG Num.Fact
    NFF.AUX$ = "TR"                                    !
    IF NFF.SOLODV% = 2 THEN NFF.AUX$ = "DV"            !
    NFF.AUX$ = NFF.AUX$ + "+"                          !
    IF (TS.GROSSPOS - TS.GROSSNEG) < 0 THEN           \!
       NFF.AUX$ = LEFT$(NFF.AUX$,2) + "-"              !
    NFF.NET$ = STR$(ABS(TS.GROSSPOS + TS.GROSSNEG))    !
    NFF.NET$ = PACK$(NFF.NET$)                         ! Vta Neta
    NFF.IT1$ = PACK$(NFF.IT1$)                         ! Plan A
    NFF.IT2$ = PACK$(NFF.IT2$)                         ! Plan B
    NFF.IT3$ = PACK$(NFF.IT3$)                         ! Plan C
    NFF.IT6$ = PACK$(NFF.IT6$)                         ! Plan D
    NFF.IT7$ = PACK$(NFF.EXE$)                         ! Vr Exento
    NFF.IT8$ = PACK$(NFF.IT8$)                         ! Impto Consumo
    TS.USERDATA$ = PACK$("21") + ":" + NFF.AUX$     + \! Arma Str Usuario con
                 NFF.PF1$+NFF.TRLOG$+":"+NFF.NET$   + \! #Fact, Vr/Vta Neto
                 ":" + NFF.IT1$ + ":" + NFF.IT2$    + \! y Vrs de Iva
                 ":" + NFF.IT3$ + ":" + NFF.IT4$    + \!
                 ":" + NFF.IT5$ + ":" + NFF.IT6$    + \!
                 ":" + NFF.IT7$ + ":" + NFF.IT8$       !
    CALL TSTPEC01                                      ! Invoca Subrut WR-LOG
    NFF.IT1$ = "" : NFF.IT2$ = "" : NFF.IT3$ = ""      !
    NFF.IT4$ = "" : NFF.IT5$ = "" : NFF.IT6$ = ""      !
    NFF.IT7$ = "" : NFF.IT8$ = ""                      !
    TS.USERDATA$ = ""                                  !
END SUB                                                !
!------------------------------------------------------------------------------
!            Acumula Transaccion Actual en Hard Totals Diario
!------------------------------------------------------------------------------
SUB FNFF.VTR PUBLIC                                    !
  CALL NFF.LEC16                                       ! Lectura desde HT 
  FISNTRRT$ = STR$(NFF.NTRA)                           !
  FISNTRRT$ = PACK$(RIGHT$("0000"+FISNTRRT$,4))        !
  FECHA.TRX$ = "19"                                    ! Siglo XX
  IF LEFT$(DATE$,2) <> "99" THEN FECHA.TRX$="20"       ! Fecha SIGLO XXI
  FECHA.TRX$ = FECHA.TRX$ + DATE$ + TIME$              ! Agrega Fecha y Hora
  FECHA.TRX$ = PACK$(FECHA.TRX$)                       ! Empaqueta Fecha
  FISFTRRT$  = FECHA.TRX$                              !

  NFF.F16$ = "C7 2C15 C3 C6 2C4 C6"                    ! Fmto Reg 16 
  NFF.F99$ = NFF.F16$ + " Wri"                         ! Formato de Lect
  WRITE FORM NFF.F16$;#NFF.HTO%,NFF.H16%;             \!
             FISACTRT$,FISNIVRT$,FISNFVRT$,FISAUTRT$, \!
             FISFAURT$,FISINIRT$,FISNTRRT$,FISFTRRT$   !
END SUB
!----------------------------------------------------------------------------
!       Funci¢n para Desplegar Mensajes y hacer Entradas al LOG
!----------------------------------------------------------------------------
! FUNCTION ADXERROR(TERM,MSGGRP,MSGNUM,SEVERITY,EVENT,UNIQUE) EXTERNAL
!    INTEGER*1    TERM,   MSGNUM,  ADXERROR            !
!    INTEGER*2    EVENT,  MSGGRP,  SEVERITY            !
!    STRING       UNIQUE                               !
! END FUNCTION                                         !


!------------------------------------------------------------------------------
!            Rutina para Cargar Variables del Registro de Control 
!------------------------------------------------------------------------------

SUB NFF.LEACON                                         !
    TERMINAL$ = PACK$("1001")                          ! Llave para Pagos
    FOR2$ = "C2 C1 2C2 C18 C40"                        ! Formato de Lectura
    READ FORM FOR2$;#NFF.CTR% KEY TERMINAL$;          \! Lee NFFTTERM
                    TERMINAL$,                        \! No. Terminal
                    OPC.TV$,                          \! NFF 1=Prf;0=SinPrf
                    TI.VA$,                           \! Tip-Variedad Pago
                    NFF.CAU$,                         \! Causac Pago por Dev
                    DESC.VRD$,                        \! Descrip Variedad
                    FILLER$                            ! Espacio libre
    NFF.MPA$ = TI.VA$                                  ! Toma Vrdad/Tipo-Pag
    FOR NFF.II% = 1 TO TO.NUMTNDR                      ! Loop para Frank/Format
        IF TO.TENDOPTS(NFF.II%,0) =                   \! Busca Tipo de Pago
           VAL(LEFT$(NFF.MPA$,1)) THEN BEGIN           ! en Terminal Opts
           IF TO.TENDOPTS(NFF.II%,1) =                \! Si es Vrdad de Cntrl
              VAL(RIGHT$(NFF.MPA$,1)) THEN BEGIN       ! en Terminal Opts
                 IF TO.TENDOPTS(NFF.II%,5) = 0 THEN   \! Si el Format es CERO
                    TO.TENDOPTS(NFF.II%,5) = 12        ! Asigna Default
                 NFF.FOR% = TO.TENDOPTS(NFF.II%,5)     ! Toma No.Frmto 
                 NFF.II% = TO.NUMTNDR                  ! Asigna Pagos Definidos
           ENDIF                                       !
        ENDIF                                          !
    NEXT NFF.II%                                       !
!   write form "c38";#34;"Ftmo "+str$(fis.for%)+"  "+ti.va$
!    OPEN "R::EAMRDESC" RECL 49 AS 64 NOWRITE NODEL     ! Archivo Descr-Report
!    READ #64, 1173;TO.FRANK.TXT$(NFF.FOR%,2),X$        ! Lectura de Nombr-Alm
END SUB                                                !

SUB NFFTSU02.011 PUBLIC                                !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Date       :  Mar. de 1999                                         **
!   **    Project    :  NUMERAC/FISCAL/2002-4022                            **
!   **    User Exit  :  NFFTSU02.011                                         **
!   **    Inclu¡r en :  EAMTSU02.J86  -  Despues de Checkout Transaction     **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  Global DataTel                                       **
!   **                                                                       **
!   **    Restaura valor de variables al finalizar el Checkout               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
NFF.SOLODV% = 0                                        !
NFF.ANUL% = 1                                          !
U.IMP.NIT% = -1
UE.RECUPERA.TRX% = 0
!------------------------------------------------------------------------------
END SUB                                                !         

SUB NFFTSU07.011 PUBLIC                                !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Date       :  Mar. de 1999                                         **
!   **    Project    :  NUMERAC/FISCAL/2002-4022                            **
!   **    User Exit  :  NFF0701.TER                                          **
!   **    Inclu¡r en :  EAMTSU07.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  Global DataTel                                       **
!   **                                                                       **
!   **    Numeros de Sesi¢n Usados en el Manejo de Archivos de               **
!   **                 Numeraci¢n Fiscal                                     **
!   **                                                                       **
!   **    Estas lineas se insertan para abrir y verificar los                **
!   **    siguientes datos y Archivos de ADX_IDT1:                           **
!   **                                                                       **
!   **            Nombre Logico                  Path-Name                   **
!   **             R::NFFTTERM           77      ADX_UDT1:NFFTTERM.DAT       ** 
!   **             R::NFFINREC           47      ADX_UDT1:NFFINREC.DAT       ** 
!   **             R::TOTRET:            45      HARD TOTALS AREA            **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
!
!  NFF.RAZON$=CHR$(27)+CHR$(69)+"      C O M F A N D I         " 
! NFF.NITR$="        NIT. 890.303.208-5"           
!  ON ERROR GOTO NFF.ERROR                              ! Si ERROR 

  NFF.RAZON$ = TO.HEADERLINE1$

  NFF.FUNC = 4                                         ! Funcion Appl Serv
  CALL ADXSERVE(NFF.RET,NFF.FUNC,NFF.PARM1,NFF.PARM2$) ! Invoca Datos Term
  NFF.AL$  = MID$(NFF.PARM2$,1,4)                      ! Toma No. Almacen
  NFF.TER$ = MID$(NFF.PARM2$,8,3)                      ! Toma No. Terminal
  NFF.OFF$ = MID$(NFF.PARM2$,11,1)                     ! Toma Status Online
  NFF.RET$ = MID$(NFF.PARM2$,12,1)                     ! Toma Status Tot.Ret
  NFF.MAT$ = MID$(NFF.PARM2$,52,1)                     ! Toma Tipo de Terminal
  
  NFF.H16%=16315                                       ! Offset H/T Long=65
                                                       ! para 4683,4684
  IF VAL(NFF.MAT$) < 3 THEN BEGIN                      ! Offset H/T Num Fiscal
     NFF.H16%=16315                                    ! para 4693,4694 Long=65
  ENDIF                                                !

  NFF.DIS% = 30                                        ! No. Ses Display
  NFF.IMP% = 34                                        ! No. Ses Ipmres CR
  NFF.HTO% = 45                                        ! No. Ses Hard Totals
  NFF.CTR% = 77                                        ! Sesion NFFTTERM
  NFF.REC% = 47                                        ! Sesion NFFINREC
  NFF.DUR% = 1500                                      ! Pausa
  U.IMP.NIT% = -1 
  UE.RECUPERA.TRX% = 0
  
  If PRT4610.ENABLE = 0 Then FIN.LIN$ = CHR$(13) + CHR$(10) Else \
                             FIN.LIN$ = CHR$(0DH)
  
  
  NFF.A1$ = "R::NFFTTERM"                              ! Define Reg Terminales
  NFF.A2$ = "R::NFFINREC"                              ! Define Reg Inicializac

!  TO.CANCELPREV = 0                                    ! Altera Manager/Ovrr
 SESION.ABIERTA:                                       !
  TS.ER.RETURN = -1                                    ! Retorna si NO Abre OK
  OPEN NFF.A1$ KEYED RECL 65 AS NFF.CTR%               ! Abre Arch Terminales
  IF TS.ER.RETURN THEN BEGIN                           ! Si Apertura esta OK
     CALL NFF.LEACON                                   !
     TS.TEMP1$="LECTURA DE REG/CONTROL OK..!"         !
!     WRITE FORM "C38";#34;TS.SAVPRT$                   !
!------------------------------------------------------v-----------------------
!     ts.temp1$=TS.TEMP1$                                 !
!     CALL U.IMPRIME(ts.temp1$,4100H)                     !
!------------------------------------------------------------------------------
  ENDIF                                                !

  TS.ER.RETURN = -1                                    ! Retorna si NO Abre OK
  OPEN NFF.A2$ KEYED  RECL 200 AS NFF.REC%             ! Abre Variabl/Fiscales
  !  ON ERROR GOTO NFF.ERROR
  !Write #34;"vericli "
  CALL VERICLI
  !Write #34;"leater "
  CALL NFF.LEATER                                      ! Lect Var.Term 
  !Write #34;"cli "
  CALL FNFF.CLI                                        ! Lect Reg HTot
  !Write #34;"cerrar "
  CLOSE NFF.REC%                                       !
!  TS.TEMP1$="TERMINA VALIDACION HARD/TOTALS OK..!"    !
!  ts.temp1$=TS.TEMP1$
!  CALL U.IMPRIME(ts.temp1$,4100H)                        !
  NFF.FE$ = DATE$ + TIME$                              !
  CALL NFF.FEC(NFF.FE$)                                !
  NFF.FE$ = "   Fecha:" + NFF.FE$                      !
!  TS.TEMP1$="TERMINAL "+NFF.TER$+" "+"OK.!"+NFF.FE$   !
!  ts.temp1$=TS.TEMP1$                                    !
!  CALL U.IMPRIME(ts.temp1$,4100H)                        !

EXIT SUB                                               !

!------------------------------------------------------------------------------
!                 Rutina de Manejo de Errores de Sincronismo
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
END SUB                                                !

SUB NFFTSU08.011 PUBLIC                                !
!------------------------------------------------------------------------------
!         **  PROYECTO NUMERAC/FISCAL/2002-4022   **
!
!     User Exit          =  NFF081A.VRF
!     Punto de Entrada   =  EAMTSU08.J86
!     Fecha de Revisi¢n  =  Mar. de 1999
!     Unidad Responsable =  T‚cnicos de Sistemas POS
!------------------------------------------------------------------------------
!            ***    Descripci¢n del Fuente      ***
!
!     Estas lineas se insertan en la USER EXIT 08  para detectar los
!     PLU's que son Items de Vta Normal ¢ los que son Cupones
!------------------------------------------------------------------------------
   NFF.FIV% = IR.INDICAT1                              ! Asigna Indicats IVA
   NFF.ITIPO=VAL(LEFT$(UNPACK$(IR.INDICAT2$),1))       ! Toma Tipo Item
   IF IR.INDICAT1A AND 02H THEN BEGIN                  ! Flag de User 3
      NFF.ITEM = 0                                     ! Reset de Item
      NFF.COUP = 0                                     ! Reset Coupon
   ENDIF                                               !
   IF NFF.ITIPO < 6 THEN                              \! Si es item de venta 
         NFF.ITEM = -1 ELSE NFF.COUP = -1              !
!------------------------------------------------------------------------------
END SUB                                                !

SUB NFFTSU14.011 PUBLIC                                !
!------------------------------------------------------------------------------
!         **  PROYECTO NUMERAC/FISCAL/2002-4022   **
!
!     User Exit          =  NFFTSU14.011
!     Punto de Entrada   =  EAMTSU14.J86
!     Fecha de Revisi¢n  =  Mar. de 1999
!     Unidad Responsable =  T‚cnicos de Sistemas POS
!-----------------------------------------------------------------------------
!                    ***    Descripci¢n del Fuente      ***       
!
!     El siguiente Control permite el manejo de las Siguientes Operaciones
! 
!         1 - Registro de Articulos en Venta Normal SIEMPRE y CUANDO
!             no se haya iniciado un Proceso de Devoluciones
!
!         2 - Registro de DEVOLUCION de Articulos que se HAYAN registrado
!             en la Transaccion en curso
!
!         3 - Registro de SOLO Devoluciones para formar una Transaccion
!             NEGATIVA, siempre y cuando NO se HAYAN registrado Articulos 
!             de Venta Normal
!
!----------------------------------------------------------------------------

IF TS.TOTALS(0,0,0) = 0 AND                           \! Si Saldo CERO y no
   TS.IO.KEYS(10)   = 0 THEN NFF.SOLODV% = 0           ! hay Autoriz, Reset 

! IF TS.IO.KEYS(8) = 67   AND                           \! Si es REFUND
!    NFF.LIK% = 0 THEN BEGIN                             ! y NO hay Liq/Retenc
!       IF TS.TOTALS(0,0,0) > 0  THEN BEGIN              ! y hay Saldo
!          WRITE FORM "C40";#30;                        \!
!                "NO SE PERMITEN DEVOL"        +        \! Despliega Error
!                "EN ESTA TRANSACCION!"                  !
!          WAIT ; 1000                                   ! Hace Pausa
!          TS.IO.MOTORKEY = 0                            ! Suspende Proceso
!       ENDIF ELSE BEGIN                                 ! de lo contrario
!          NFF.SOLODV% = 2                               ! inicia Proc REFUND
!       ENDIF                                            !  
! ENDIF                                                  !

! IF TS.IO.KEYS(2) = 80   AND                           \! Si es ENTER
!    TS.IO.DATA$(2) <> "" AND                           \! se digita PLU 
!    TS.TOTALS(0,0,0) < 0 THEN BEGIN                     ! Saldo Negativo
!       IF NFF.SOLODV% = 2 AND                          \! Ya hubo REFUNF's
!          TS.IO.KEYS(8) <> 67 AND                      \! y NO es REFUND
!          TS.IO.KEYS(1) <> 70 THEN BEGIN                ! y NO es VOID
!              WRITE FORM "C40";#30;                    \!
!                   "VENTA NO PERMITIDA.!"  +           \! Despliega Error
!                   "ACEPTE SOLO DEVOLS.!"               !
!          WAIT ; 1000                                   !
!          TS.IO.MOTORKEY = 0                            !
!       ENDIF                                            !
! ENDIF                                                  !
!------------------------------------------------------------------------------
!              Asigna Tipo y Variedad de Pago para Devoluciones
!------------------------------------------------------------------------------

! IF TS.IO.MOTORKEY > 90   AND                          \! Si cualquier Motor
!    TS.IO.MOTORKEY < 97   THEN BEGIN                    ! en Rango de Pagos
!       IF NFF.SOLODV% = 2 THEN BEGIN                    ! Si son REFUNDS
!          NFF.TVTA%   = TS.BALDUE(0) * -1               ! 
!          CALL NFF.ASIGNANUM                            !
!          NFF.NTRA = NFF.NTRA - 1                       ! Ajusta No Transac
!          UNV$ = RIGHT$("00000000"+STR$(NFF.NTRA),8)    ! Obtiene Nuevo No.
!          TS.IO.MOTORKEY = 90 + VAL(LEFT$(NFF.MPA$,1))  ! Toma Tipo Pago
!          TS.IO.DATA$(3) = RIGHT$(NFF.MPA$,1)           ! distinto a CERO
!          TS.IO.DATA$(7) = STR$(TS.BALDUE(0))           ! Coloca Saldo en Pago
!          TS.IO.KEYS(3) = 78                            ! Asigna SLASH
!          TS.IO.KEYS(7) = TS.IO.MOTORKEY                ! 
!          CALL FORMAT.AMOUNT(NFF.TVTA%)                 !
!          FON75$ = CHR$(27) + CHR$(14)                  !
!          TO.FRANK.TXT$(NFF.FOR%,11) =                 \!
!                   FON75$ + "    $" + TS.TEMP1$         !
!          IF NOT TS.STANDALONE THEN BEGIN               !
!             TS.VF.KEY$ = PACK$(NFF.CAU$ +             \!
!             RIGHT$(STRING$(24,"0")+NFF.NUT$,24))       ! Coloca Tipo y Vrdad
!             TS.VF.MAXTRANS = NFF.TVTA%                 ! Maximo Pago 
!             TS.VF.LIMTRANS = NFF.TVTA%                 ! Maximo todos los Pgs
!             TS.ER.RETURN   = 0                         ! Estado OK Acceso Arch
!             WHILE NOT TS.ER.RETURN                     !
!                TS.ER.RETURN = -1                       ! Estado OK Acceso Arch
!                OPEN "R::EAMTENDV" KEYED RECL 26 AS 7   !
!                IF TS.ER.RETURN THEN BEGIN              !
!                   WRITE FORM "C13 C1 3I4";#7;         \! Graba No. para Verif
!                   TS.VF.KEY$,PACK$("50"),             \! proximo Pago con
!                   TS.VF.MAXTRANS,TS.VF.LIMTRANS,0      ! Bono por Retorno
!                ENDIF                                   !
!                CLOSE 7                                 !
!             WEND                                       !
!          ENDIF                                         !
!       ENDIF                                            ! Fin de Saldo (-)
! ENDIF                                                  ! Fin de Pago

!------------------------------------------------------------------------------
END SUB                                                !

SUB NFFTSU20.011 PUBLIC                                !
!   ***************************************************************************
!   **    Date       :  Mar. de 1999                                         **
!   **    Project    :  NUMERAC/FISCAL/2002-4022                            **
!   **    User Exit  :  NFFTSU20.011                                         **
!   **    Inclu¡r en :  EAMTSU20.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  Global DataTel                                       **
!   **                                                                       **
!   **    Imprime datos adicionales con algunos mensajes exigidos por        **
!   **    la Ley, como Nit, No. de Factura y Fecha de Expedicion.            **
!   **                                                                       **
!   ***************************************************************************

   IF TS.LINETYPE = 1 OR                              \! Si es Vta Item
      TS.LINETYPE = 5 THEN BEGIN                       ! 
         NFF.ITIPO=VAL(LEFT$(UNPACK$(IR.INDICAT2$),1)) ! Toma Tipo Item
         NFF.VOREF% = SL.IT.INDICAT2                   ! Chequea Si Cancelac
         CALL NFF.TIPITEM                              ! Invoca Validac Signo
   ENDIF                                               !


IF TS.LINETYPE = 1 OR TS.LINETYPE = 5 THEN \ 
   IF U.IMP.NIT% = -1 THEN BEGIN
      U.IMP.NIT% = 0 

      If (UE.RECUPERA.TRX% = -1 ) AND (PRT4610.ENABLE EQ 0)  Then Begin 
          Call U.IMPRIME(TO.HEADERLINE1$,4100H)
          Call U.IMPRIME(TO.HEADERLINE2$,4100H)
          UE.RECUPERA.TRX% = 2
      EndIf       
      
!      If PRT4610.ENABLE NE 0 Then Begin 
!         TS.TEMP1$ = FISSUC$ + FISDOM$  
!         Call U.IMPRIME("     " +TS.TEMP1$+ "   ",6100H)
!      EndIf Else Begin 
!         TS.TEMP1$ = Left$(FISSUC$ + FISDOM$,37)              !
!         Call U.IMPRIME(TS.TEMP1$,6100H)
!      EndIf 

     If UE.EPSS.ON <> -1 Then Begin       ! Si No registra  Convenios 
      Call U.IMPRIME("COMPRADOR:_________________________",6100H)
      Call U.IMPRIME("NIT:_______________________________",6100H)
      Call U.IMPRIME(FIN.LIN$,6100H)
     EndIf  
   EndIf 

   CALL NFF.ANO2000                                    !
   IF NOT TS.TRAINING THEN BEGIN                       ! Si no es Ses. Entrenam
    If TS.PROCEDURE < 1 Then                          \!       
      IF TS.LINETYPE = 6 AND                          \! Store Data y Transacc
         TS.LINEDATA = 1 THEN BEGIN                    ! Fecha, Hora, etc
            IF TS.TENDERED (0) <> 0 AND               \! Si hay pagos
               TS.TRX.STATUS   <> 100 THEN BEGIN       ! y NO hay VOID en curso
               
                  IF NFF.HAYV% = 0 THEN CALL FNFF.NUM  ! Si no hay Vouch,
                  
                  NFF.HAYV% = 0                        ! busca Num Fiscal
                  CALL FNFF.VTR                        ! Graba Jornada Dia
            ENDIF                                      !
      ENDIF                                            ! End if taxes > 0 
   ENDIF                                               !
!------------------------------------------------------------------------------
END SUB                                                !

SUB NFFTSU21.011 PUBLIC                                !
!   ***************************************************************************
!   **    Date       :  Nov. de 1999                                         **
!   **    Project    :  NUMERAC/FISCAL/2002-4022                            **
!   **    User Exit  :  NFFTSU21.011                                         **
!   **    Inclu¡r en :  EAMTSU21.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  Global DataTel                                       **
!   **                                                                       **
!   **    Imprime fecha con ajuste Siglo XXI                                 **
!   **                                                                       **
!   ***************************************************************************

  CALL NFF.ANO2000                                     ! Fecha Siglo XXI
                                                       !
END SUB                                                !

SUB NFFTSU23.011 PUBLIC                                !

!------------------------------------------------------------------------------
!         **  PROYECTO NUMERAC/FISCAL/2002-4022   **
!
!     User Exit          =  NFF231A.REF 
!     Punto de Entrada   =  EAMTSU23.J86
!     Fecha de Revisi¢n  =  Mar. de 1999
!     Unidad Responsable =  T‚cnicos de Sistemas POS
!------------------------------------------------------------------------------
!            ***    Descripci¢n del Fuente      ***
!
!     Exit para desplegar Datos en el Visor del Operador
!
!     Variables:                                                 
!              TS.LINETYPE:  Tipo de display.  6 ==> Prompt de Ventas
!              TS.DISP1$:    Primera linea de despliegue                  
!------------------------------------------------------------------------------

!   IF LEFT$(TS.DISP1$,4)  = "B067" OR                 \!
!      LEFT$(TS2.DISP1$,4) = "B067" THEN BEGIN          !
!         TS.DISP1$ = "NO SE ACEPTA ANULAR"             ! Despliega Error
!         TS.DISP2$ = "ITEMS NO VENDIDOS !"             !
!         TS2.DISP1$ = TS.DISP1$                        !
!         TS2.DISP2$ = TS.DISP2$                        !
!   ENDIF                                               !

!   IF NFF.SOLODV% = 2 THEN BEGIN                       !
!      IF LEFT$(TS.DISP1$,4)  = "B048" OR              \!
!         LEFT$(TS2.DISP1$,4) = "B048" THEN BEGIN       !
!            TS.DISP1$ = "PAGO POR DEVOLUC.!"           !
!            TS.DISP2$ = "REQUIERE DOC/IDENT "          ! Despliega Error
!            TS2.DISP1$ = TS.DISP1$                     !
!            TS2.DISP2$ = TS.DISP2$                     !
!      ENDIF                                            !
!   ENDIF                                               !
!------------------------------------------------------------------------------
END SUB                                                !
 
SUB NFFTSU32.011 PUBLIC                                !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Date       :  Octubre de 1999                                      **
!   **    Project    :  NUMERAC/FISCAL/2002-4022                             **
!   **    User Exit  :  NFFTSU32.011                                         **
!   **    Inclu¡r en :  EAMTSU32.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  Global DataTel                                       **
!   **                                                                       **
!   **             ***    Descripci¢n del Fuente      ***                    **
!   **                                                                       **
!   **   Deshabilita los Pagos por Devolucion desde EAMTENDV                 **
!   **   cuando se CAUSA en una Transaccion de Venta NORMAL el               **
!   **   Pago por Devolucion definido por el Cliente                         **
!   **                                                                       **
!   ***************************************************************************
!------------------------------------------------------------------------------
! write form "c38";#34;"Dv:"+str$(fis.solodv%)+         \!
!           " St:"+str$(ts.vf.maxtrans)      +         \!
!           " "+unpack$(ts.vf.key$)                     !

! IF NFF.SOLODV%     = 0 AND                            \! NO es Trans REFUND
!    TS.VF.MAXTRANS <> 0 AND                            \! Con Status OK
!    LEFT$(UNPACK$(TS.VF.KEY$),2)=NFF.CAU$ THEN BEGIN    ! Si es Pago por Dev
!       IF NOT TS.STANDALONE THEN BEGIN                  ! Si no hay Standal
!          TS.ER.RETURN   = 0                            ! Estado OK Acceso Arch
!          WHILE NOT TS.ER.RETURN                        ! Loop para Bloqueo Pago
!            TS.ER.RETURN = -1                           ! Estado OK Acceso Arch
!            OPEN "R::EAMTENDV" KEYED RECL 26 AS 7       ! Abre Arch Verif Pagos
!            IF TS.ER.RETURN THEN BEGIN                  ! Si Apertura OK
!               WRITE FORM "C13 C1 3I4";#7;TS.VF.KEY$,  \! Graba Reg. Verif
!                     PACK$("58"),0,TS.VF.LIMTRANS,0     ! con Status Bloqueo
!            ENDIF                                       ! y Borra Vr del Bono
!            CLOSE 7                                     ! Cierra Archivo
!          WEND                                          ! Fin del Loop
!       ENDIF                                            !
! ENDIF                                                  ! 
!------------------------------------------------------------------------------
END SUB                                                !

SUB NFFTSU53.011 PUBLIC                                !
!   ***************************************************************************
!   **    Date       :  Mar. de 1999                                         **
!   **    Project    :  NUMERAC/FISCAL/2002-4022                            **
!   **    User Exit  :                                                       **
!   **    Inclu¡r en :  EAMTSU53.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  Global DataTel                                       **
!   **                                                                       **
!   **    Se agregan las  siguientes instrucciones para recuperar los        **
!   **    posibles Datos de Usuario, cuando el sistema efect£a               **
!   **    Transaction Recovery                                               **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If UE.RECUPERA.TRX% = 0 Then Begin 
   UE.RECUPERA.TRX% = -1 
   UE.IVA.IMPRESO% = 1
EndIf 

B$ = SL.STR.ENTRY$ + ":"                               ! Agrega Separador STR
IF UNPACK$(LEFT$(SL.STR.ENTRY$,1)) = "01" THEN BEGIN   ! Si DATA/ENTRY
   CALL NFF.STRING1                                    ! Extraer Datos Str 1
   CALL NFF.TIPITEM                                    ! Invoca Buscar Signo
ENDIF                                                  !
IF UNPACK$(LEFT$(SL.STR.ENTRY$,1)) = "99" THEN BEGIN   ! Si String Datos 99
   CALL NFF.STRING99                                   ! Extraer Datos Str 99
ENDIF                                                  !
END SUB                                                !

!------------------------------------------------------------------------------
!                    Fin de las User Exits de Usuario
!------------------------------------------------------------------------------

SUB NFFTSU27.011 PUBLIC			! Rutinas de Error
!   ***************************************************************************
!   **    Date       :  Mar. de 1999                                         **
!   **    Project    :  NUMERAC/FISCAL/2002-4022                            **
!   **    User Exit  :                                                       **
!   **    Inclu¡r en :  EAMTSU53.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  Global DataTel                                       **
!   **                                                                       **
!   **    Se agregan las  siguientes instrucciones para recuperar los        **
!   **    posibles Datos de Usuario, cuando el sistema efect£a               **
!   **    Transaction Recovery                                               **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

!NFF.ERROR:                                             !
!   NFF.ERCO$ = ERR                                     !
!   NFF.ERRFX$ = ERRNSTR$(ERRN)                         !
!   IF ERR   = "IH" THEN RESUME                         !
!   IF ERR   = "AE" AND ERRF% =  0 THEN RESUME          !
!   IF ERR   = "FU" AND ERRF% = 44 THEN RESUME          !
!   TS.SAVPRT$ = "ERROR.. " + ERR + " " +              \!
!                 STR$(ERRF%) + " " + NFF.ERRFX$        !
!   WRITE FORM "C38 A1";#34;TS.SAVPRT$                  !
!------------------------------------------------------v-----------------------
!   ts.temp1$=TS.SAVPRT$                                   !
!   CALL U.IMPRIME(ts.temp1$,4100H)                       !
!------------------------------------------------------------------------------

!   IF ERR   = "DF" AND ERRF%=77 THEN BEGIN             !
!      CLOSE 77                                         !
!      GOTO SESION.ABIERTA                              !
!   ENDIF                                               !

!   IF ERR   = "DF" THEN RESUME                        !

   IF ERRF% = 45 THEN BEGIN                            !
   
!      CLEARS NFF.DIS%                                  !
!      WRITE #NFF.DIS%;"  TERMINAL ACTIVA  "+          \!
!                      "  SIN SERIE FISCAL.!"           !
!      TS.TEMP1$ = "** TERMINAL ACTIVA SIN"+          \!
!                   " SERIE FISCAL.! **"                !
!      ts.temp1$=TS.TEMP1$                                !
!      CALL U.IMPRIME(ts.temp1$,6100H)                    !
!      TS.TEMP1$ = "** SE ASIGNARA SERIE INICIAL..!"   !
!      ts.temp1$=TS.TEMP1$                                !
!      CALL U.IMPRIME(ts.temp1$,6100H)                    !
!      WAIT;NFF.DUR%                                    ! Pausa

      CALL NFF.WRIF                                    !
   ENDIF                                               !   
END SUB
