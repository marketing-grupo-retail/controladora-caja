!   ***************************************************************************
!   **    Date       :  Marzo de 2004                                        **
!   **    Program    :  EAMCSUPC.BAS                                         **
!   **    Executable :  EAMCSMLL.286                                         **
!   **    Author     :  JORGE ALBERTO CADENA  jcadena@sky.net.co             **
!   **                                                                       **
!   ***************************************************************************
!
!-------------------------------------------------------------------
!  Subrutinas comunes para la extraccion de info del tslog
!-------------------------------------------------------------------
! If you have any questions or problems please call or send
! a e-mail to:
!
! Jorge Alberto Cadena  57-315-337 67 56
!                       jcadena@sky.net.co
! ********************************************************************
!                         RELEASE HISTORY
!
! VER 1.00 - INITIAL RELEASE   2004/03/20
!
! *************************** UEAPPLC.BAS ***************************
!
!
%ENVIRON C
!
! Supermarket application global data
!
%INCLUDE EAMTRANS.J86
%INCLUDE EAMCSGLO.J86 
!
!  User data
!
integer*1 global debug%
!
  STRING              \
    EP.DATO$,         \
    EP.INPUT$,        \
    EP.TIPO.REG$,     \ 
    EP.APPL$,         \ 
    EP.FUNCION$,      \ 
    EP.EFT.CLAVE$,    \
    EP.EFT.DATA1$,    \
    EP.EFT.DATA2$,    \
    EP.EFT.DATA3$,    \
    EP.EFT.DATA4$,    \
    EP.EFT.DATA5$,    \
    EP.EFT.DATA6$,    \
    EP.EFT.DATA99$,   \
    EP.EFT.KEY$,      \
    EP.EPAY.TRANSNUM$,\
    EP.FECHA.PROCESO$,\
    EP.FECHA.POSTEO$, \
    EP.FINR$,         \
    EP.ANUL.TRX$
   
  INTEGER*1           \ 
    EP.EFT.OK%

  INTEGER*2           \
    EP.SESION1%,      \                                                 
    EP.SESION2%,      \
    EP.SESION3%,      \
    EP.RECLEN%,       \
    EP.L%,            \
    EP.J%,            \
    EP.K%,            \
    EP.LINEA%,        \
    EP.SECUEN%,       \ 
    EP.MAX.TRX%,      \
    EP.LEN.CLAVE%
    
   INTEGER*4          \
     EP.RETURNCODE%
!------------------------------------------------------------------- FIN UE.APPLCSVA
  SUB ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL
   INTEGER*4 RET
   INTEGER*2 FUNC, PARM1
   STRING PARM2
  END SUB
  
  SUB ADXCOPYF (RETC,INFILE,OUTFILE,OPT0,OPT1) EXTERNAL
    INTEGER*4 RETC
    STRING INFILE,OUTFILE
    INTEGER*2 OPT0,OPT1
  END SUB
   
  FUNCTION ADXERROR (TERM,MSGGRP,MSGNUM,SEVERITY,EVENT,UNIQUE$) EXTERNAL
    INTEGER*2     TERM,MSGNUM
    INTEGER*1     SEVERITY,MSGGRP,EVENT
    STRING                UNIQUE$
  END FUNCTION

  FUNCTION CSMLEC03 EXTERNAL         ! Error logging routine
    STRING CSMLEC03
  END FUNCTION
! 
  SUB CSMLEW01 EXTERNAL              ! Common error routine
  END SUB
! 
  FUNCTION CSVFEC02( P1 ) EXTERNAL
    INTEGER*2 P1
  END FUNCTION
!
  FUNCTION PACKBIN2(P1$,P2,P3) EXTERNAL
    INTEGER*2 PACKBIN2
    STRING P1$
    INTEGER*2 P2,P3
  END FUNCTION
!
  SUB UE.LIMPIA.DATO
    FOR EP.K% = 1 TO LEN(EP.DATO$)
      IF ASC(MID$(EP.DATO$,EP.K%,1)) < ASC("0") OR \ 
         ASC(MID$(EP.DATO$,EP.K%,1)) > ASC("9") THEN \ 
      BEGIN  
        IF EP.K% = 1 THEN EP.DATO$ = "0" + RIGHT$(EP.DATO$,LEN(EP.DATO$)-1) \
        ELSE \
        IF EP.K% = LEN(EP.DATO$) THEN EP.DATO$ = LEFT$(EP.DATO$,LEN(EP.DATO$)-1) + "0" \
        ELSE \
        EP.DATO$ = LEFT$(EP.DATO$,EP.K%-1)+"0"+RIGHT$(EP.DATO$,LEN(EP.DATO$)-EP.K%)
      ENDIF
    NEXT EP.K% 
  END SUB
!
  SUB UE.EXTRAE.DATOS
!
    ON ERROR GOTO EFT.ERR3
    EP.LINEA% = 0
    EP.EFT.DATA1$ = ""
    EP.EFT.DATA2$ = ""
    EP.EFT.DATA3$ = "" 
    EP.EFT.DATA4$ = ""
    EP.EFT.DATA5$ = "" 
    EP.EFT.DATA6$ = ""
    EP.FECHA.POSTEO$ = STRING$(4," ")
    EP.FECHA.PROCESO$ = STRING$(12," ")
    If debug% then print #ep.sesion3%; "uxtrae datos="
    FOR EP.L% = 1 TO SL.HD.NUMSTRIN + 1
       if debug% then print #ep.sesion3%; "cs.type="+ \
       str$(cs.type(ep.l%))+ " l%="+str$(ep.l%)+" strings="+str$(sl.hd.numstrin)    
       print #ep.sesion3%; "CS.TYPE(EP.L%)=11"+STR$(CS.TYPE(EP.L%))
      IF CS.TYPE(EP.L%) = 11 THEN \
      BEGIN
      	print #ep.sesion3%; "CS.TYPE(EP.L%)=11"
        if debug% then print #ep.sesion3%; "cs.type="+ \
        str$(cs.type(ep.l%))+ " l%="+str$(ep.l%)+" strings="+str$(sl.hd.numstrin)
        CALL CSVFEC02(EP.L%)
        EP.INPUT$ = UNPACK$(MID$(CS.INPUT$,CS.FLD.POS(1)+1,CS.FLD.LEN(1)))    
        if debug% then print #ep.sesion3%; "01-POS="+STR$(CS.FLD.POS(1))+ \
          " LEN="+STR$(CS.FLD.LEN(1))+\ 
          " WRK="+EP.INPUT$
        print #ep.sesion3%; "EP.EFT.CLAVE$->"+EP.EFT.CLAVE$
        print #ep.sesion3%; "EP.INPUT$,LEN(EP.EFT.CLAVE$)->"+EP.INPUT$,LEN(EP.EFT.CLAVE$)  
        IF LEFT$(EP.INPUT$,LEN(EP.EFT.CLAVE$)) = EP.EFT.CLAVE$ THEN \ !eft data entry found
        BEGIN
          if debug% then print #ep.sesion3%; "clave="+mid$(EP.INPUT$, 9, 12)
          EP.EPAY.TRANSNUM$ = MID$(EP.INPUT$,EP.LEN.CLAVE% + 5, 6)
          EP.TIPO.REG$      = MID$(EP.INPUT$,EP.LEN.CLAVE% + 11,2)
          EP.APPL$          = MID$(EP.INPUT$,EP.LEN.CLAVE% + 1,2)
          EP.FUNCION$       = MID$(EP.INPUT$,EP.LEN.CLAVE% + 3,2)
          if debug% then print #ep.sesion3%; "clave="+EP.TIPO.REG$
          
          IF EP.TIPO.REG$ = "01" THEN \
          BEGIN
            WRITE FORM "C4 C12 5C20 C40 C2"; #EP.SESION1%;                                     \
              UNPACK$(SL.HD.TERMINAL$), STRING$(12,"9"), \
              LEFT$(EP.EPAY.TRANSNUM$ + STRING$(20,"0"),20),  \
              STRING$(20," "), STRING$(20," "), STRING$(20," "), STRING$(20," "),\
              STRING$(40," "), EP.FINR$          
          ENDIF   
          EP.LINEA% = EP.LINEA% + 1
          EP.EFT.DATA1$ = MID$(EP.INPUT$, EP.LEN.CLAVE% + 1 , 12)
          EP.INPUT$ = UNPACK$(MID$(CS.INPUT$,CS.FLD.POS(2)+1,CS.FLD.LEN(2)))    
          EP.EFT.DATA2$ = LEFT$(EP.INPUT$ + STRING$(20," "),20)
          EP.INPUT$ = UNPACK$(MID$(CS.INPUT$,CS.FLD.POS(3)+1,CS.FLD.LEN(3)))    
          IF EP.TIPO.REG$ = "02" THEN \ ! Cobro
          BEGIN
            EP.FECHA.POSTEO$ = LEFT$(EP.INPUT$,4)
          ENDIF
          EP.EFT.DATA3$ = LEFT$(EP.INPUT$ + STRING$(20," "),20)
          EP.INPUT$ = UNPACK$(MID$(CS.INPUT$,CS.FLD.POS(4)+1,CS.FLD.LEN(4)))    
          IF EP.TIPO.REG$ = "03" THEN  \ ! Anulación
          BEGIN 
            EP.ANUL.TRX$ = LEFT$(EP.INPUT$,6)
          ENDIF
          EP.EFT.DATA4$ = LEFT$(EP.INPUT$ + STRING$(20," "),20)
          EP.INPUT$ = UNPACK$(MID$(CS.INPUT$,CS.FLD.POS(5)+1,CS.FLD.LEN(5)))    
          EP.FECHA.PROCESO$ = LEFT$(EP.INPUT$,12)
          EP.EFT.DATA5$ = LEFT$(EP.INPUT$ + STRING$(20," "),20)
          EP.INPUT$ = UNPACK$(MID$(CS.INPUT$,CS.FLD.POS(6)+1,CS.FLD.LEN(6)))    
          EP.EFT.DATA6$ = LEFT$(EP.INPUT$ + STRING$(20," "),20)
          if debug% then print #ep.sesion3%; "sesion 1="+str$(EP.SESION1%)+  \
                                 " linea="+str$(EP.LINEA%)+                  \
                                 " terminal="+UNPACK$(SL.HD.TERMINAL$)+      \ 
                                 " clave="+ EP.EFT.DATA1$ 
  !       
          WRITE FORM "C4 C12 5C20 C40 C2"; #EP.SESION1%; \
            UNPACK$(SL.HD.TERMINAL$), EP.EFT.DATA1$, EP.EFT.DATA2$, \
            EP.EFT.DATA3$, EP.EFT.DATA4$,                  \
            EP.EFT.DATA5$, EP.EFT.DATA6$, STRING$(40," "), EP.FINR$
          if debug% then print #ep.sesion3%; "proceso="+EP.FECHA.PROCESO$+       \
                                 " posteo="+EP.FECHA.POSTEO$
          IF EP.TIPO.REG$ = "03" AND             \
             EP.ANUL.TRX$ <> STRING$(6,"0") THEN \
          BEGIN 
            WRITE FORM "C4 C12 5C20 C40 C2"; #EP.SESION1%;                 \
              UNPACK$(SL.HD.TERMINAL$), EP.APPL$ + "10" + EP.ANUL.TRX$ + "03" ,     \
              EP.EFT.DATA2$, EP.EFT.DATA3$,          \
              EP.EFT.DATA4$, EP.EFT.DATA5$,          \
              EP.EFT.DATA6$, STRING$(40," "), EP.FINR$
          if debug% then print #ep.sesion3%; "anulada=" + EP.ANUL.TRX$+    \
                                            " posteo="  + EP.FECHA.POSTEO$
          ENDIF
!          WRITE FORM "C12 C4 C4 C12 5C20 C40 C2"; #EP.SESION2%;                  \
!            EP.FECHA.PROCESO$, EP.FECHA.POSTEO$, UNPACK$(SL.HD.TERMINAL$),       \
!            EP.EFT.DATA1$, EP.EFT.DATA2$,                  \
!            EP.EFT.DATA3$, EP.EFT.DATA4$,                  \ 
!            EP.EFT.DATA5$, EP.EFT.DATA6$, STRING$(40," "), EP.FINR$
        ENDIF
      ENDIF
      IF CS.TYPE(EP.L%) = 99 THEN \
      BEGIN
        if debug% then print #ep.sesion3%; "cs.type="+ \
        str$(cs.type(ep.l%))+ " l%="+str$(ep.l%)+" strings="+str$(sl.hd.numstrin)
        CALL CSVFEC02(EP.L%)
        EP.INPUT$ = UNPACK$(MID$(CS.INPUT$,CS.FLD.POS(1)+1,CS.FLD.LEN(1)))    
        if debug% then print #ep.sesion3%; "01-POS="+STR$(CS.FLD.POS(1))+ \
          " LEN="+STR$(CS.FLD.LEN(1))+\ 
          " WRK="+EP.INPUT$
        IF LEFT$(EP.INPUT$,LEN(EP.EFT.CLAVE$)) = EP.EFT.CLAVE$ THEN \ !eft data entry found
        BEGIN
          if debug% then print #ep.sesion3%; "clave="+mid$(EP.INPUT$, 9, 12)
          EP.EPAY.TRANSNUM$ = MID$(EP.INPUT$,EP.LEN.CLAVE% + 5, 6)
          EP.TIPO.REG$      = MID$(EP.INPUT$,EP.LEN.CLAVE% + 11,2)
        ENDIF  
        IF EP.TIPO.REG$ = "07" THEN \
        BEGIN 
          EP.EFT.KEY$ = MID$(EP.INPUT$, EP.LEN.CLAVE% + 1 , 12)
          EP.INPUT$ = MID$(CS.INPUT$,CS.FLD.POS(2)+1,CS.FLD.LEN(2))    
          EP.EFT.DATA99$ = EP.INPUT$
          WRITE FORM "C4 C12 C140 C2"; #EP.SESION1%;               \
            UNPACK$(SL.HD.TERMINAL$), EP.EFT.KEY$, EP.EFT.DATA99$, EP.FINR$
          if debug% then print #ep.sesion3%; "D99="+ EP.EFT.KEY$ + \
                                 " datal="+ STR$(len(EP.EFT.DATA99$))
        ENDIF
      ENDIF
    NEXT EP.L% 
    EP.EFT.OK% = -1
  !
    EXIT SUB
!
  EFT.ERR3:
    WORK2I2 = ADXERROR( 0,           \ 0 for controller
                        0,           \ no msg displayed
                        0,           \ no msg displayed
                        5,           \ severity level
                        2,           \ 2 = checkout support
         CSMLEC03+RIGHT$(""+STR$(ERRF%),2)+ERR )

    CALL CSMLEW01             ! Process other errors
    IF CS.RESUME THEN \
    BEGIN
      RESUME                  ! resume after error
    ENDIF ELSE BEGIN
      RESUME RETRY            ! retry failing instn.
    ENDIF

END SUB
!
!
SUB UE.APPLCS01 PUBLIC
!-------------------------------------------------------------------
!  Inicializa proceso de EFT para una transaccion
!-------------------------------------------------------------------
    if debug% then print # EP.SESION3%; "1-tipo="+ \
       unpack$(MID$(CS.INPUT$,1,1))+\
       " trantype="+unpack$(MID$(CS.INPUT$,15,1))
    EP.EFT.OK% = 0 
!
END SUB
!--------------------------------------------------------- FIN UE.APPLCS01

SUB UE.APPLCS02 PUBLIC
!-------------------------------------------------------------------
!  Inicializa proceso de EFT para una transaccion
!-------------------------------------------------------------------
  if debug% then print # EP.SESION3%; "2-tipo="+ \
       unpack$(MID$(CS.INPUT$,1,1))+\
       " trantype="+unpack$(MID$(CS.INPUT$,15,1))
  EP.EFT.OK% = 0 
!
END SUB
!--------------------------------------------------------- FIN UE.APPLCS02

SUB UE.APPLCS12 PUBLIC
    ON ERROR GOTO EFT.ERR1
!
    EP.SESION1% = 81
    EP.SESION2% = 82
    EP.SESION3% = 83
    EP.RECLEN%  = 158
    EP.FINR$ = CHR$(13) + CHR$(10)
    EP.MAX.TRX% = 100
    EP.EFT.CLAVE$ = "51000000"
    EP.LEN.CLAVE% = LEN(EP.EFT.CLAVE$)
!  
  OPEN.EFT.DEBUG:
    debug% = 0                           ! -1=Activa,  0=desactiva debug
    create posfile "adx_udt1:dbefttrx.dat" as EP.SESION3%
! 
  OPEN.EFT.TRX:
    OPEN "EFTTRX" KEYED RECL EP.RECLEN% AS EP.SESION1%
!
  OPEN.EFT.LOG:
!    OPEN "EFTLOG"  AS EP.SESION2% NODEL APPEND
!
    EXIT SUB
!
  EFT.ERR1:
    IF (ERR = "OE") THEN \             ! Process open error
    BEGIN       
      WORK2I2 = ADXERROR( 0,           \ 0 for controller
                          0,           \ no msg displayed
                          0,           \ no msg displayed
                          5,           \ severity level
                          2,           \ 2 = checkout support
         CSMLEC03+RIGHT$(""+STR$(ERRF%),2)+ERR )
!  
      IF ERRF% = EP.SESION1% THEN \
      BEGIN
        CREATE POSFILE "EFTTRX" KEYED 16 ,,,60000 RECL EP.RECLEN% AS EP.SESION1% MIRRORED \
          ATCLOSE
        CLOSE EP.SESION1%
        RESUME OPEN.EFT.TRX
      ENDIF
!
      IF ERRF% = EP.SESION2% THEN \
      BEGIN
        CREATE POSFILE "EFTLOG" AS EP.SESION2% MIRRORED \
          ATCLOSE 
        CLOSE EP.SESION2%
        RESUME OPEN.EFT.LOG
      ENDIF
!
      IF ERRF% = EP.SESION3% THEN \
      BEGIN
        CREATE POSFILE "ADX_UDT1:DBEFTTRX.DAT" AS EP.SESION3%  
        CLOSE EP.SESION3%
        RESUME OPEN.EFT.DEBUG
      ENDIF
!
    ENDIF
! 
    CALL CSMLEW01             ! Process other errors
    IF CS.RESUME THEN BEGIN
      RESUME                  ! resume after error
    ENDIF ELSE BEGIN
      RESUME RETRY            ! retry failing instn.
    ENDIF
!
END SUB
!--------------------------------------------------------- FIN UE.APPLCS12

SUB UE.APPLCS14 PUBLIC
!-------------------------------------------------------------------
!  Extrae informacion del data entry
!-------------------------------------------------------------------
    if debug% then print # EP.SESION3%; "14 tipo="+ unpack$(MID$(CS.INPUT$,  \ Get field 1 into a string
             CS.FLD.POS(1)+1,1))+":"+ UNPACK$(MID$(CS.INPUT$,CS.FLD.POS(1)+1,4))
    WORK1$ = UNPACK$(MID$(CS.INPUT$,CS.FLD.POS(1)+1,4))   
    IF (WORK1$ = EP.EFT.CLAVE$) AND \
       (NOT EP.EFT.OK%) THEN \
    BEGIN 
      CALL UE.EXTRAE.DATOS
    ENDIF
!
END SUB
!--------------------------------------------------------- FIN UE.APPLCS14

SUB UE.APPLCS15 PUBLIC
!-------------------------------------------------------------------
!  Extrae informacion del data entry
!-------------------------------------------------------------------
    if debug% then print # EP.SESION3%; "15 tipo="+ unpack$(MID$(CS.INPUT$,  \ Get field 1 into a string
             CS.FLD.POS(1)+1,1))+":"+ UNPACK$(MID$(CS.INPUT$,CS.FLD.POS(1)+1,4))
    WORK1$ = UNPACK$(MID$(CS.INPUT$,CS.FLD.POS(1)+1,4))   
    print # EP.SESION3%; "WORK1$->"+WORK1$
    print # EP.SESION3%; "EP.EFT.CLAVE$->"+EP.EFT.CLAVE$
    IF (WORK1$ = EP.EFT.CLAVE$) AND \
       (NOT EP.EFT.OK%) THEN \
    BEGIN 
      CALL UE.EXTRAE.DATOS
    ENDIF
!
END SUB
!--------------------------------------------------------- FIN UE.APPLCS15

Sub UE.APPLCS16 PUBLIC
!-------------------------------------------------------------------
!  Cierre diario
!-------------------------------------------------------------------
 Integer*1 Xerror%
 On ERROR GOTO EFT.ERR2     ! SET ERROR ADDRESS
 Xerror% = -1
 Delete EP.SESION1%         ! Delete old data
 If Xerror% <> -1 Then Begin 
    Call ADXSERVE(0,26,1,"Paila Borrando EFTTRX")
    Wait ; 3500
 EndIf 
 Wait ; 300
 Xerror% = -1
 Call ADXSERVE(0,26,1,"Creando Nuevo EFTTRX")
 CREATE POSFILE "EFTTRX" KEYED 16 ,,,60000 RECL EP.RECLEN% AS EP.SESION1% MIRRORED ATCLOSE
 If Xerror% <> -1 Then Begin 
    Call ADXSERVE(0,26,1,"Paila Creando Nuevo EFTTRX")
    Wait ; 3500
 EndIf 
 Exit Sub

  EFT.ERR2:
      CALL CSMLEW01             ! Process other errors
      If CS.RESUME Then Begin
        RESUME                  ! resume after error
      EndIf Else Begin
        RESUME RETRY            ! retry failing instn.
      EndIf
End Sub 
!--------------------------------------------------------- FIN UE.APPLCS16

SUB UE.APPLCS17 PUBLIC
!-------------------------------------------------------------------
!  Cierra el archivo cada hora
!-------------------------------------------------------------------
  TCLOSE EP.SESION1%           ! Close the file hourly
!  TCLOSE EP.SESION2%
!
END SUB
!------------------------------------------------------------- FIN UE.APPLCS17
!
!---------------------------------------------------------------- FIN DE UEAPPLC.BAS
END
