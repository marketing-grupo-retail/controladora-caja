!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : MRKTPRIM.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Carga promociones especiales para*
!                clientes y productos modulo de   *
!                mercadeo                         *
!**************************************************
! Mod 08Ago2011
! Se adiciona en la carga del archivo el control de la 
! tipificacion del cliente.
! Desarrollado por Grupo Retail - OVS 
!--------------------------------------------------

%Environ C					! Ambiente Controlador

STRING    GLOBAL ERRFX$, MSG$, DATA$, ARCHIVO$
INTEGER*1 GLOBAL BAN.PRG%, POS%, Terror%, Topen%
INTEGER*4 GLOBAL M1%

STRING GLOBAL UE.LOG$, UE.COD$, M1$, M2$, M3$, M4$, M5$, M6$, UE.CUPON$, KEY$

FUNCTION TRADUCE.ERROR                                       !
INTEGER*4 HX%, S%, SX%, SUM%
STRING Z$
    HX% = ERRN                                               ! Traduccion de los
    ERRFX$=""                                                ! codigos de error
    FOR S% = 28 TO 0 STEP -4                                 ! del sistema operativo
        SX% = SHIFT(HX%,S%)                                  !
        SUM% = SX% AND 000FH                                 !
        IF SUM% > 9 THEN SUM% = SUM% + 55                   \!
        ELSE SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                      !
        ERRFX$ = ERRFX$ + Z$                                 !
    NEXT S%                                                  !
END FUNCTION

SUB PANTALLA
   CLEARS
   LOCATE 2, 4: PRINT CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   LOCATE 3, 4: PRINT CHR$(179)
   LOCATE 4, 4: PRINT CHR$(179)
   LOCATE 3,12: PRINT "**** CARGA PROMOCIONES DE ARTICULOS PARA CLIENTES   ****"
   LOCATE 3,75: PRINT CHR$(179)
   LOCATE 4,10: PRINT CHR$(27)+"b3"
   LOCATE 4,12: PRINT "***  Ultima Revision Software Agosto     08 2011 G.R. ***"
   LOCATE 4, 7: PRINT CHR$(27)+"b7"
   LOCATE 4,75: PRINT CHR$(179)
   LOCATE 5, 4: PRINT CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   LOCATE 23,1: PRINT "Msg:"
END SUB

SUB BARRA
   POS% = POS% + 1
   IF POS% = 1 THEN LOCATE  12,45: PRINT "|"
   IF POS% = 2 THEN LOCATE  12,45: PRINT "/"
   IF POS% = 3 THEN LOCATE  12,45: PRINT "-"
   IF POS% = 4 THEN LOCATE  12,45: PRINT "\"
   IF POS% = 4 THEN POS% = 0
END SUB
!--- Fin barra de estado

Sub PROCESA
 STRING X$, PROMOS$(1)
 INTEGER*1 IND.GRABA%, II%, X%, Y%
 STRING C1$, C2$, C3$, C4$, C6$, C7$, C5$, C8$, C9$
 TOpen% = 0
 Again.Procesa:
 Terror% = -1
 Open ARCHIVO$ As 1
 If Terror% = 0 Then Begin 
 	  Wait ; 1800
 	  TOpen% = TOpen% + 1 
 	  If TOpen% > 5 Then Stop 
 	  GoTo Again.Procesa
 Endif 
 IF END #1 THEN FIN.CARGA
 LOCATE 12,15: PRINT "Procesando Informacion ..."
 POS% = 0
!--- Estructura del archivo de carga
!--- Codigo del articulo, codigo promocion, Tipo promocion, Vlr promocion, Linkedto, Promocode, Ind.Registro, Codigo promocion
!-- tipo de promocion que aplica
!-- 1. Descuento en pesos
!-- 2. Descuento en porcentaje
!-- 3. Precio Final
!---4. Asignacion de LINKEDTO Y PROMOCODE
!--- Indicador de registro 1. Add/Replace 0. Delete

 While (1)
   Call BARRA
   READ #1;C1$, C2$, C3$, C4$, C5$, C6$, C7$, C9$
   C1$ = RIGHT$("000000000000"+STR$(VAL(C1$)),12) ! Plu de promocion
   !C2$ = RIGHT$("0000"+STR$(VAL(C2$)),4)          ! Numero de la promocion
   
   C2$ = RIGHT$("0000"+C2$,4)          						! Numero de la promocion / Tipificacion de cliente Add 8 Ago 2011
   
   C3$ = RIGHT$("0"+STR$(VAL(C3$)),1)             ! tipo de promocion
   C4$ = PACK$(RIGHT$("00000000"+STR$(VAL(C4$)),8))   ! Valor de la promocion
   C5$ = PACK$(RIGHT$("0000"+STR$(VAL(C5$)),4))   ! Cupon de liga
   C6$ = PACK$(RIGHT$("0000"+STR$(VAL(C6$)),4))   ! Promocode Ligado
   C9$ = PACK$(RIGHT$("0000"+STR$(VAL(C9$)),4))   ! Codigo de la promocion 

   C8$ = STRING$(12,"0")   
   UE.COD$ = PACK$(C1$+ C2$)                      ! Codigo producto promocionado
   WRITE FORM "C8 C1 C4 2C2 C12 C2";#33;    \!
             UE.COD$,		                 \! Codigo del articulo 
             C3$,                        \! tipo de promocion
             C4$,                        \! Valor del dscto
			       C6$,                        \! Link de promocion
			       C7$,                        \! Promocode 
			       C8$,                        \! Uso futuro
			       C9$                          ! Codigo de la promocion
 Wend
 FIN.CARGA:

END SUB

SUB TERMINAR
 LOCATE 23,1: PRINT STRING$(78," ")     	        ! si no existe lo crea. 
 LOCATE 23,1: PRINT "Msg: Fin Ejecucion Programa ..."	!
 CLOSE 33						! Cierra archivo
 STOP							! Fin del programa
END SUB
!--- Fin de terminar


!------
!--- Bloque Principal
!------

ON ERROR GOTO ERRORES
ARCHIVO$ = COMMAND$   ! TOMA archivo de carga
Call PANTALLA					! Pantalla principal
Open "MKTP:MKPPRITM.DAT" KEYED RECL 31 AS 33 ! Apertura logos
Call PROCESA					! Procesa Carga
Call TERMINAR

!--- Definicion de las rutinas de error
ERRORES:
         IF ERRF% = 4 AND                                 \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            LOCATE 23,1: PRINT STRING$(78," ")             ! si no existe lo crea.
            LOCATE 23,1: PRINT "Msg: Error, Maestro de Promociones no Existe ..."
            STOP                                           ! retorna ejecucion
         ENDIF                                             ! del programa
         IF ERRF% = 4 AND ERR = "EF" THEN BEGIN           \! Si encuentra fin de 
            BAN.PRG% = 0				   ! ejecucion normal del
            RESUME
         ENDIF

!         IF ERRF% = 33 AND ERR = "EF" THEN BEGIN           \! Si encuentra fin de 
!            BAN.PRG% = 0				   ! ejecucion normal del
!            RESUME
!         EndIf

         IF ERRF% = 1 AND                                 \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            Terror% = 0 
            LOCATE 23,1: PRINT STRING$(78," ")             ! si no existe lo crea.
            LOCATE 23,1: PRINT "Msg: Archivo de Carga No Existe ..."+ARCHIVO$
            Resume                                           ! retorna ejecucion
         ENDIF                                             ! del programa
         CALL TRADUCE.ERROR
         MSG$ = "Msg: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 23,1:PRINT MSG$                            ! Presenta Error pantalla
