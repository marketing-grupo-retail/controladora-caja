!---------------------------------------------------------
!               PROYECTO : Comfamiliar ANDI
!               Programa : CFMEDSOR.BAS
!
!     Medios de Pago para cuadre de Movimiento diario. 
!     (Tipo Pago 2, Variedades 1 a 5)
!
!
!     Archivo C:\ADX_UDT1\MED1.DAT  para obtener relacion de cheques por 
!     medio de pago con su respectiva variedad, y tipo de operador.
!
!---------------------------------------------------------
%ENVIRON C                                      ! Ambiente de Controlador

!Definicion de Variables de usuario Locales

  STRING       VALOR$,RESER,MESES(1),FORMAT, FORMATO,   \
               EFE$, CHE$, OTR$, RED$, TCR$, CRC$,BASUR$

  INTEGER*1    SW.FIN
  INTEGER*2    MES,MESPRO
  INTEGER*4    LIMTRAN%,MAXTRAN%,SUMTRAN%,NUMTRA%,      \
               EFE%, CHE%, OTR%, RED%, TCR%, CRC%

! ------------------ PROGRAMA PRINCIPAL -------------------------------

   ON ERROR GOTO ERRTRAP 
   OPEN "C:\ADX_UDT1\MEDPAG2.DAT" AS 25 BUFFSIZE 1000     ! Archivo Medios de Pago
   PAGO$ = "C:\ADX_UDT1\MEDPAG3.DAT"                    ! Crea Etiqueta y
   CREATE POSFILE PAGO$ AS 29  BUFFSIZE 5120          ! Arch Pagos Din mico
   FINR$ = CHR$(13) + CHR$(10)
   IF END #25 THEN SALIR
   READ #25; LINE REGI$
   REGI$ = REGI$ + FINR$
   WRITE FORM "C76";#29;REGI$ 
   EFE$ = MID$(REGI$,14,9)
   CHE$ = MID$(REGI$,24,9)
   OTR$ = MID$(REGI$,34,9)
   RED$ = MID$(REGI$,44,9)
   TCR$ = MID$(REGI$,54,9)
   CRC$ = MID$(REGI$,64,9)
   READ #25; LINE REGI$
   REGI$ = REGI$ + FINR$
   WRITE FORM "C76";#29;REGI$                                ! Pasa Contrl 4
LECTURA:
   READ #25; LINE REGI$                                         ! Lee MED.>DAT
  BASUR$ = MID$(REGI$,42,2) 
   IF BASUR$ = "=;"  OR   \
   BASUR$ = ">>" OR BASUR$ = "==" THEN BEGIN
   BASUR$ = "00" 
   REGI$ = LEFT$(REGI$,41)+BASUR$+MID$(REGI$,44,70)+FINR$
   WRITE FORM "C76";#29;REGI$                                ! Graba Cr/Cob
   ENDIF ELSE BEGIN
   REGI$ = REGI$ + FINR$
   WRITE FORM "C76";#29;REGI$                                ! Graba Cr/Cob
   ENDIF 
 GOTO LECTURA                                                  ! Retorna
SALIR:
   CLOSE 25	
   CLOSE 29
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
    IF ERR="EF" THEN ERRORCOD$="EF"   !Fin de archivo
    IF ERR="OE" THEN ERRORCOD$="OE"   !Archivo no existe
    PRINT "ERR = ";ERR," ERRL =";ERRL
    PRINT "ERRF = ";ERRF%," ERRN =";ERRFX$
    RESUME 
   END
!--------------- Fin del Programa Principal -------------------- 
