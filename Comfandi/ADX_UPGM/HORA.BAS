String PROG.TIME$, TIME.STRING$, TB.SCHD.CHECK$, PARM2$
Integer*4 EXEC.DIFF, RET%, RTA%
Integer*2 PARM1%

SUB ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL
  INTEGER*4 RET
  INTEGER*2 FUNC,PARM1
  STRING PARM2
END SUB



On Error GoTo PAILA
CLEARS
CALL ADXSERVE (RET%, 26, 0,"Validando Pantalla Conectadas")
CALL ADXSERVE (RET%, 20,15, "")! Inactividad Pantalla
Call ADXSERVE (RTA%, 26, 0,"DATO MOUSE "+STR$(RET%))
WAIT ; 500
CALL ADXSERVE (RTA%, 23, 0, "")! Get status for date format
CALL ADXSERVE (RET%, 26, 0,"Rta: "+Str$(RTA%))
Call ADXSERVE (RET%, 26, 0,"Apagando Servidor...")
Call ADXSERVE (RET%,  5, 5,"061146")
wait ; 500
Stop 


!       TB.SCHD.CHECK$ = "30" 
!       CLEARS
!       Input "HORA CALCULO ";Time.STRING$
!       Print "HORA SISTEMA "+Time$
!       PROG.TIME$ = LEFT$(TIME$,4)
!       EXEC.DIFF =                        \   Calculate time left for exec in min.
!      ((VAL(LEFT$(PROG.TIME$,2))*60) +    \   Convert hours to minutes and
!       VAL(RIGHT$(PROG.TIME$,2)))    -    \   Add the minutes and subtract
!      ((VAL(LEFT$(TIME.STRING$,2))*60) +  \   Convert hours to minutes and
!       VAL(RIGHT$(TIME.STRING$,2)))       !   Add the minutes
!      
!      Print "DIFERENCIA CALCULADA ",EXEC.DIFF
!       
!      IF EXEC.DIFF > VAL(TB.SCHD.CHECK$) THEN BEGIN ! If time greater
!         Print "SE SOBREPASO DEL TIEMPO PERMITIDO"
!      ENDIF Else Print "DENTRO DEL RANGO DE HORA "
!      Stop 
 
 
      
PAILA:
Print "ERROR APLICACION "+ERR
Stop 
