
IF TS.LINETYPE = 9   AND                              \! Si linea Franq y
   TS.LINEDATA = 9   THEN BEGIN                        ! Formato=9, Credito
!   VRDAD$      = "35" THEN BEGIN  
   IF NO.LIN.FRAN% > 6 THEN BEGIN                      ! Si lin impr > 6
     TS.FORMCR$ = "C86 A1"                             !
     WRITE FORM "C86 A4";#35;                         \! Impr segun formato
     ALMA$ 
    !FRA.FAC$+ALMA$                                    ! Escribe Emp Fec y Alm
      FOR USR.IND1% = 1 TO 499                         ! Loop para vaciar CR
        IF USR.ITEMS$(USR.IND1%) = "" THEN BEGIN       ! Si es Lin en Blanco
           FOR USR.IND1% = 450 TO 489                  ! Loop para Vaciar TOTs
               TS.FORMCR$ = "C38 A1"                   ! Formato Impresion
               IF USR.ITEMS$(USR.IND1%)<>"" THEN BEGIN ! Si hay Subtotales
                  WRITE FORM "C38 A1";#35;            \! Imprime todos
                  LEFT$(USR.ITEMS$(USR.IND1%),        \! los pagos del
                  38-(38-LEN(USR.ITEMS$(USR.IND1%))))  ! CR hasta ese momento
                  LINCR% = LINCR% - 2                  ! Decrem Lineas Disp
               ENDIF                                   ! Fin de Lin TOTs
               NO.CUO$ = ""                            ! Reset de %EPS
               NO.MOD$ = ""                            ! Reset de CTAMOD
           NEXT USR.IND1%                              ! Sig Loop
           USR.IND1% = 499                             ! Asigna final Loop
        ENDIF ELSE BEGIN                               !
           TS.FORMCR$ = "C86 A1"                       ! Cambia a Fmto 38
           WRITE FORM "C86 A2";#35;                   \! Imprime en 86 posic
                 MID$(USR.ITEMS$(USR.IND1%),47,3)  +  \! Cantidad      
                 "     "                           +  \!             
                 LEFT$(USR.ITEMS$(USR.IND1%),10)   +  \! C¢digo
                 "      "                          +  \!                 
                 MID$(USR.ITEMS$(USR.IND1%),17,25) +  \! Descripci¢n
                 "     "                           +  \!             
                 MID$(USR.ITEMS$(USR.IND1%),56,10) +  \! Valor Unitario
                 "     "                           +  \!               
                 RIGHT$(USR.ITEMS$(USR.IND1%),10)      ! Valor Total
           LINCR% = LINCR% - 2                         ! Decrem Cont Linea
           IF LINCR% = 0 THEN BEGIN                    ! Si se acaba Forma
              WRITE FORM "2C1 A0";#35;CHR$(27),CHR$(12)! Retira el Doc Val
              WRITE FORM "C1 A10";#35;" "              ! Avanza 10 espacios
              LINCR% = 26                              ! 
           ENDIF                                       !
        ENDIF                                          !
      NEXT USR.IND1%                                   !
      NOM$ = STRING$(30," ")                           ! Borra Nombre
      FOR USR.IND1% = 1 TO LINCR% + 8                  ! Posiciona Total 
        WRITE FORM "C8  A1";#35;" "                    ! Avanza 1 espacio
      NEXT USR.IND1%                                   ! Sigue en Loop
      WRITE FORM "C38 A2";#35;RIGHT$(USR.ITEMS$(490), \! Escribe Total
                  38-(38-LEN(USR.ITEMS$(490))))        !  
      DIM USR.ITEMS$(500)                              ! Dim/Reset Array
      USR.IND1% = 0                                    ! 
      USR.IND2% = 0                                    !
      LINCR%    = 30                                   !
   ENDIF                                               !
ENDIF                                                  !
