!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : MSGCARGA.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Agosto 21 de 2.002               *
!Observaciones : Carga Mensajes publicitarios para*
!                clientes y articulos modulo de   *
!                mercadeo                         *
!**************************************************

%ENVIRON C						     ! Ambiente Controlador

STRING    GLOBAL ERRFX$, MSG$, DATA$, Ifile$
INTEGER*1 GLOBAL BAN.PRG%, POS%

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
   LOCATE 3,12: PRINT "****ANUL. MAESTRO MENSAJES PUBLICITARIOS  Ver. 1.00 ****"
   LOCATE 3,75: PRINT CHR$(179)
   LOCATE 4,10: PRINT CHR$(27)+"b3"
   LOCATE 4,12: PRINT "***  Ultima Revision Software Noviembre 12 2008 GR    ***"
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

SUB PROCESA
 STRING C1$, C2$, C3$, C4$, C5$, C6$, C7$, C8$, C9$, LEC$, C10$
 STRING UE.MCD.COD$, \ Codigo
        UE.MCD.LOG$, \ Logo
        UE.MCD.CPN$, \ Cupon
        UE.MCD.PRM$, \ Parametrizacion
        UE.MCD.MSG$(1), \ Mensaje Operador
        UE.MCD.MST$(1) \ Mensaje Tirilla


 OPEN Ifile$ AS 1
 IF END #1 THEN FIN.CARGA
 LOCATE 12,15: PRINT "Procesando Informacion ..."
 POS% = 0
 While (1)
   Call BARRA
   Read #1;LINE DATA$
   If LEN(DATA$) = 94 Then Begin 
     C1$ = MID$(DATA$,1,4)	! Codigo mensaje
     C1$ = PACK$(C1$)		! Empaqueta dato    
     DelRec 33; C1$
   EndIf

 WEND
 FIN.CARGA:
    Delete 1
END SUB

SUB TERMINAR
 LOCATE 23,1: PRINT STRING$(78," ")     	        ! si no existe lo crea. 
 LOCATE 23,1: PRINT "Msg: Fin Ejecucion Programa ..."	!
 Close 33						! Cierra archivo
 Stop							! Fin del programa
END SUB
!--- Fin de terminar


!------
!--- Bloque Principal
!------

ON ERROR GOTO ERRORES
Ifile$ = Command$ 
CALL PANTALLA					! Pantalla principal
OPEN "MKTP:MKPMSGPR.DAT" KEYED RECL 279 AS 33   ! Apertura Mensajes
CALL PROCESA					! Procesa Carga
CALL TERMINAR

!--- Definicion de las rutinas de error
ERRORES:
         IF ERRF% = 33 AND                                 \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            LOCATE 23,1: PRINT STRING$(78," ")             ! si no existe lo crea.
            LOCATE 23,1: PRINT "Msg: Error, Maestro Mensajes Publicitarios no Existe ..."
            STOP                                           ! retorna ejecucion
         ENDIF                                             ! del programa
         IF ERRF% = 33 AND ERR = "EF" THEN BEGIN           \! Si encuentra fin de 
            BAN.PRG% = 0				   ! ejecucion normal del
            RESUME
         ENDIF
         IF ERRF% = 1 AND                                 \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            LOCATE 23,1: PRINT STRING$(78," ")             ! si no existe lo crea.
            LOCATE 23,1: PRINT "Msg: Archivo de Carga "+Ifile$+" No Existe ..."
            STOP                                           ! retorna ejecucion
         ENDIF                                             ! del programa
         CALL TRADUCE.ERROR
         MSG$ = "Msg: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 23,1:PRINT MSG$                            ! Presenta Error pantalla
