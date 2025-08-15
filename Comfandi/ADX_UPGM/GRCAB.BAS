!************************************************** 
!Empresa       : Grupo Retail S.A                 *
!Programa      : INMOVITM.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Movimiento de Items vendidos     *
!**************************************************
!

%ENVIRON C						   												! Ambiente de controlador

Function LEER.CABECERA
String X.Tmp$
     Open "c:\adxalma.dat" as 91
     Read # 91 ; X.Tmp$
     DM.ALMACEN$ = Left$(X.Tmp$,4)						   ! Numero del almacen
     FECMOV$     = MID$(X.TMP$,5,8)              ! Fecha de movimiento
     Close 91
End Function
!--- Fin de la funcion de lectura

Sub Write.Data
String Rbuffer$, X.Lec$, X.Costo$, X.Recibo$, X.Vlr$, X.Signo$
String X.Pago$, X.Caja$, X.Trx$, X.Tmp$, X.Forma$
Integer*4 X.Len%, X.J%, X.Comp%

      FINR$=CHR$(13)+CHR$(10)
      Rbuffer$ = Ue.Data2$
      X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
      X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
      Write form X.Lec$; #51 ; Rbuffer$, Finr$           							! Graba registro
End Sub 

Sub Cabecera.Reporte
  Ue.Salida$ = "ADX_UDT1:VTSISMED."+Right$(DM.ALMACEN$,3)
  Create Ue.Salida$ AS 51

  UE.DATA2$ = "1"            + \ Caracter Fijo
              FECMOV$        + \ Fecha generacion archivo
              DM.ALMACEN$      ! Centro de Costo
  Call Write.Data
End Sub 

On Error GoTo Fail.Proc
Call LEER.CABECERA
Call Cabecera.Reporte
Close 51
Stop 

Fail.Proc:
MEN$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)
Print MEN$
