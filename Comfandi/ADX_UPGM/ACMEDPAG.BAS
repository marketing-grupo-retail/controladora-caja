!************************************************** 
!Empresa       : Grupo Retail S.A                 *
!Programa      : ACMEDPAG.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Acumulacion Fiscal Comfandi      *
!**************************************************

String GLOBAL FRA.TIM$, FRA.H$, FRA.M$, FRA.S$, FRA.FEC$, FRA.AA$, FRA.MM$, FRA.DD$, SIGLO$, FHOY$, FRA.LIN$, FRA.MD$, TM$(1)
Integer*1 GLOBAL MES

  SUB CREAFECHA Public 
      FRA.TIM$ = TIME$                                        ! Toma Hora Sistema
      FRA.H$   = LEFT$(FRA.TIM$,2)                            ! Arma horas
      FRA.M$   = MID$(FRA.TIM$,3,2)                           ! Arma minutos
      FRA.S$   = RIGHT$(FRA.TIM$,2)                           ! Arma segundos
      FRA.FEC$ = DATE$                                        ! Toma Fecha Sistema
      FRA.AA$  = LEFT$(FRA.FEC$,2)                            ! Arma a¤o
      FRA.MM$  = MID$(FRA.FEC$,3,2)                           ! Arma mes
      FRA.DD$  = RIGHT$(FRA.FEC$,2)                           ! Arma dia
      MES      = VAL(FRA.MM$)                                 ! Toma Mes
      SIGLO$   = "20"
      FHOY$    = TM$(MES)+" "+STR$(VAL(FRA.DD$))             \! Arma Fecha Hoy
                 + SIGLO$ + FRA.AA$                           !
      FRA.LIN$ =FRA.H$+":"+FRA.M$+":"+FRA.S$     +           \! Arma la Linea
               "    de "+FHOY$                                ! con Hora y Fecha
      FRA.MD$  = RIGHT$(FRA.FEC$,4)                           ! Salva Mes y Dia 
  END Sub
