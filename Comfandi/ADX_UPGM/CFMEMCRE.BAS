!---------------------------------------------------------
!               PROYECTO : Comfamiliar ANDI
!               Programa : CFMEMCRE.BAS
!
!     Medios de Pago para cuadre de Movimiento diario. 
!     (Tipo Pago 6, Variedades 1 a 5)
!
!     Pagos recibidos por la L¡nea de Cr‚dito Comfandi.
!
!     Archivo C:\Pcccmmdd.DAT  y Memorando para 
!     CREDITO y COBRANZAS
!
!     Archivo C:\Dcccmmdd.DAT para depurar Nos de Factura
!     en CUPOS1.DAT (Elimina cada dia los Nos Usados)
!---------------------------------------------------------
%ENVIRON C                                      ! Ambiente de Controlador

!Definicion de Variables de usuario Locales

  STRING       VALOR$,RESER,MESES(1),FORMAT, FORMATO,   \
               EFE$, CHE$, OTR$, RED$, TCR$, CRC$, BASUR$

  INTEGER*1    SW.FIN
  INTEGER*2    MES,MESPRO
  INTEGER*4    LIMTRAN%,MAXTRAN%,SUMTRAN%,NUMTRA%,      \
               EFE%, CHE%, OTR%, RED%, TCR%, CRC%


!REM ------------------ PROGRAMA PRINCIPAL -------------------------------

   ON ERROR GOTO ERRTRAP 
   OPEN "C:\ADX_UDT1\IVACRE1.DAT" AS 25 BUFFSIZE 5120     ! Archivo Medios de Pago
   OPEN "C:\ADXALMA.DAT"       AS 26 BUFFSIZE 80      ! Archivo Control Almacen
   READ FORM "C80";#26;AL$                            ! Lee Control Almac
   ALMA$ = LEFT$(AL$,4)+"  "+RIGHT$(AL$,LEN(AL$)-12)  ! Guarda Almac y Nombre
   PAGOS$ = "C:\ADX_UDT1\P"+MID$(AL$,2,3)+MID$(AL$,9,4)+".DAT" ! Crea Etiqueta y
   BORRA$ = "C:\ADX_UDT1\D"+MID$(AL$,2,3)+MID$(AL$,9,4)+".DAT" ! Crea Etiqueta y
   CREATE POSFILE BORRA$ AS 30 BUFFSIZE 5120          ! Arch Retiros Dinamico
   CREATE PAGOS$  DIRECT RECL 185 AS 29 BUFFSIZE 5120 !   
   CLOSE 26
   FORMATO = "######"
   FORMAT = "####,###,###"
   FINR$ = CHR$(13) + CHR$(10)
   DIM MESES(12)
   FECHA$=DATE$
   MESPRO=VAL(MID$(FECHA$,3,2))
   DIAPRO$=STR$(VAL(RIGHT$(FECHA$,2)))
   ANOPRO$=LEFT$(FECHA$,2)
   IF ANOPRO$ < "95" THEN BEGIN                        ! Si es Siglo XXI
      ANOPRO$ = "20" + ANOPRO$                         ! A¤o Empieza por 20
   ENDIF ELSE BEGIN                                    ! de lo contrario
      ANOPRO$ = "19" + ANOPRO$                         ! A¤o Empieza por 19
   ENDIF                                               !

!----------------------   Llena arreglo de meses   ---------------------------
   MESES(1)="ENERO"     :  MESES(2)="FEBRERO"    :  MESES(3)="MARZO"
   MESES(4)="ABRIL"     :  MESES(5)="MAYO"       :  MESES(6)="JUNIO"
   MESES(7)="JULIO"     :  MESES(8)="AGOSTO"     :  MESES(9)="SEPTIEMBRE"
   MESES(10)="OCTUBRE"  :  MESES(11)="NOVIEMBRE" :  MESES(12)="DICIEMBRE"
!----------------------------------------------------------------------------
   IF END #25 THEN SALIR
   READ #25; LINE REGI$
     REGI$ = REGI$ + FINR$
   WRITE FORM "C185";#29;REGI$                                ! Pasa Contrl 4
   REGD$ = LEFT$(REGI$,13) + STRING$(35," ") + FINR$         ! Toma Contrl
   WRITE FORM "C50";#30;REGD$                                ! Graba Control
   MES  = VAL(MID$(REGI$,6,2))
   DIA$ = MID$(REGI$,8,2)
   EFE% = VAL(MID$(REGI$,14,9))
   CHE% = VAL(MID$(REGI$,24,9))
   OTR% = VAL(MID$(REGI$,34,9))
   RED% = VAL(MID$(REGI$,44,9))
   TCR% = VAL(MID$(REGI$,54,9))
   CRC% = VAL(MID$(REGI$,64,9))
   SG1$ = MID$(REGI$,23,1)
   SG2$ = MID$(REGI$,33,1)
   SG3$ = MID$(REGI$,43,1)
   SG4$ = MID$(REGI$,53,1)
   SG5$ = MID$(REGI$,63,1)
   SG6$ = MID$(REGI$,73,1)
   RESTO$ = MID$(REGI$,74,64)
   READ #25; LINE REGI$
     REGI$ = REGI$ + FINR$
   WRITE FORM "C185";#29;REGI$                                ! Pasa Contrl 4
   EFE$ = MID$(REGI$,14,9)
   CHE$ = MID$(REGI$,24,9)
   OTR$ = MID$(REGI$,34,9)
   RED$ = MID$(REGI$,44,9)
   TCR$ = MID$(REGI$,54,9)
   CRC$ = MID$(REGI$,64,9)
!   LPRINTER

LECTURA:
   READ #25; LINE REGI$                                         ! Lee MED.>DAT
   IF MID$(REGI$,57,2) > "59" THEN BEGIN                       ! Si credito 
  BASUR$ = MID$(REGI$,42,2) 
   IF BASUR$ = ">>" OR LEFT$(BASUR$,1) = ">" THEN BEGIN
     BASUR$ = "00" 
     REGI$ = LEFT$(REGI$,41)+BASUR$+MID$(REGI$,44,183)+FINR$
     REGI$ = REGI$ + FINR$
   WRITE FORM "C185";#29;REGI$                                ! Pasa Contrl 4
   ENDIF ELSE BEGIN 
     REGI$ = REGI$ + FINR$
   WRITE FORM "C185";#29;REGI$                                ! Pasa Contrl 4
   ENDIF
   ENDIF                                                        !
   IF MID$(REGI$,57,2) > "60" THEN BEGIN                        ! Si es CR
      TV$ = " " + MID$(REGI$,57,1) + " " + MID$(REGI$,58,1)     ! Tipo/Varied 
      NI$ = " " + RIGHT$(STRING$(11," ") +                     \! Nit Empresa
                  STR$(VAL(MID$(REGI$,61,11))),11)              !
      SU$ = " " + RIGHT$(STRING$(03," ") +                     \! Sucursal
                  STR$(VAL(MID$(REGI$,72,03))),03)              !
      RA$ = " " + RIGHT$(STRING$(10," ") +                     \! Rango 1-2
                  STR$(VAL(MID$(REGI$,22,08))),10)              !
      REGD$ =  " 3" + TV$ + NI$ + SU$ +                        \! Arma Regist
              " 99" + RA$ + RA$ + " " + FINR$                   !
      WRITE FORM "C50";#30;REGD$                                ! Graba Elimin
   ENDIF                                                        ! Fin Cond
  GOTO LECTURA                                                  ! Retorna
SALIR:
   CONSOLE
   CLEARS
   PRINT:PRINT:PRINT:PRINT
   PRINT TAB(10);"COMFANDI";
   PRINT TAB(43);DIAPRO$;TAB(46);"de ";TAB(49);MESES(MESPRO);" del A¤o ";ANOPRO$
   PRINT : PRINT
   PRINT TAB(30);"MEMORANDO"
   PRINT
   PRINT TAB(10);"DE  :";TAB(17);"Supermercado: ";ALMA$
   PRINT:PRINT:PRINT
   
   PRINT TAB(12);"      EFECTIVO : ";TAB(30);
   PRINT USING FORMATO;VAL(EFE$);TAB(40);
   PRINT USING FORMAT;EFE%;
   PRINT SG1$

   PRINT TAB(12);"       CHEQUES : ";TAB(30);
   PRINT USING FORMATO;VAL(CHE$);TAB(40);
   PRINT USING FORMAT;CHE%;
   PRINT SG2$

   PRINT TAB(12);"   OTROS PAGOS : ";TAB(30);
   PRINT USING FORMATO;VAL(OTR$);TAB(40);
   PRINT USING FORMAT;OTR%;
   PRINT SG3$

   PRINT TAB(12);"  PAGO RED TEF : ";TAB(30);
   PRINT USING FORMATO;VAL(RED$);TAB(40);
   PRINT USING FORMAT;RED%;
   PRINT SG4$

   PRINT TAB(12);"TARJETA CREDIT : ";TAB(30);
   PRINT USING FORMATO;VAL(TCR$);TAB(40);
   PRINT USING FORMAT;TCR%,
   PRINT SG5$

   PRINT TAB(12);"CRED. COMFANDI : ";TAB(30);
   PRINT USING FORMATO;VAL(CRC$);TAB(40);
   PRINT USING FORMAT;CRC%;
   PRINT SG6$
   !PRINT  :   PRINT
   !INPUT "                                  Digite INTRO para Continuar..";SG6$
   CLOSE 25	
   CLOSE 29
   CLOSE 30
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

