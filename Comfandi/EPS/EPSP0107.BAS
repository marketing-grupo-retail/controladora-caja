!************************************************** 
!Empresa       : Asic Consulting Group S.A Retail *
!Programa      : EPSP0107.bas                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Mantenimiento Clientes Convenios *
!**************************************************
! ModIficaciones:
! 03May2005
! Se adiciona un campo adicional a la tabla de parametros de
! planes, indicando si el plan definido es de Capitacion o no.
! desarrollado por Oscar Valencia 
!------------------------------------------------------
! Se ajusta la longitud del plan de 12 a 16 caracteres segun
! requerimiento de Comfandi, 12 julio de 2006
! desarrollado por Oscar Valencia
! Nota: Se cambia pantalla del DM de la 207 a la 230
!------------------------------------------------------

%INCLUDE EPSPVARI.BAS				 	       ! Definicion variables
%INCLUDE ADX_UPGM:DMEXTR.J86   	     ! Inclucion Libreria Display Manager
%INCLUDE EPSPRUTI.BAS				  	     ! Rutinas Comunes

!--- Fin  definicion de variables

Function BUSQ.LIGCON					     ! Valida convenio
  BAN.PRG$  = "0"					     !
  LEC$="C5 C40 C12"					     !
  DM.NIT$=Pack$(Right$("0000000000"+DM.NIT$,10))           ! Creacion llave proyecto
  Read Form LEC$;#AREA2% KEY DM.NIT$;                       \! Lee Reg Cabecera 
       DM.LLAVE$,DM.RAZON$, DM.CODALT$			     !
  If BAN.PRG$ = "1" Then Call MSG.ERR(04,"No Definido...")
  If BAN.PRG$ = "0" Then Call MSG.ERR(04,Left$(DM.RAZON$,30))
END Function
!--- Fin del procedimiento busqueda de ligas

Function MNT.REG4
  BAN.PRG$  = "0"
  DM.NOMPRO$ = "   "
  LEC$="C8 C40 2C3 C5 C4 4C1 4C3 C5 3C1 4C3 C3 4C1"
  DM.CONVEN$=(Right$("0000000000000"+DM.CONVEN$,13))         ! Creacion llave proyecto
  DM.PLAN$=(Right$("000"+DM.PLAN$, 3))                       ! Creacion llave proyecto
  DM.NEWKEY$ = Pack$(DM.CONVEN$ + DM.PLAN$)
  Read Form LEC$;#AREA1% KEY DM.NEWKEY$;                    \! Lee Reg Cabecera 
       DM.LLAVE$,  DM.RAZON$,  DM.FECINI$, DM.FECFIN$,      \!
       DM.VLRCON$, DM.VLRDES$, DM.VALMED$, DM.VALMCA$,      \!
       DM.VALIPS$, DM.VALMET$, DM.METPRA$, DM.METPRB$,      \!
       DM.METPRC$, DM.METPRD$, DM.NIT$,    DM.VALDIA$,      \!
       DM.FACTURA$, DM.DSCVLR$, DM.PTG1$,  DM.PTG2$,        \!
       DM.PTG3$,   DM.PTG4$, DM.DSCTO$, DM.VALFAC$,         \!
       DM.VALBEN$, DM.CAPITACION$, DM.COPIATIQ$
  DM.FECINI$=Unpack$(DM.FECINI$)			     ! Fecha inicial contrato
  DM.FECFIN$=Unpack$(DM.FECFIN$)	 	  	     ! Fecha final contrato
  DM.VLRCON$=Unpack$(DM.VLRCON$)			     ! Valor del contrato
  DM.VLRDES$=Unpack$(DM.VLRDES$)			     ! Valor del despacho maximo
  DM.METPRA$=Unpack$(DM.METPRA$)			     ! Valor copago A
  DM.METPRB$=Unpack$(DM.METPRB$)			     ! Valor copago B
  DM.METPRC$=Unpack$(DM.METPRC$)			     ! Valor copago C
  DM.METPRD$=Unpack$(DM.METPRD$)			     ! Valor copago D
  DM.NIT$=Unpack$(DM.NIT$)				     ! Nit del convenio
  Call MSG.ERR(43,DM.NIT$)				     ! Nit del convenio
  Call MSG.ERR( 5,DM.RAZON$)                                 ! Presenta descripcion
  Call MSG.ERR( 7,DM.FECINI$)                                ! Mensaje a pantalla
  Call MSG.ERR( 8,DM.FECFIN$)                                ! Mensaje a pantalla
  Call MSG.ERR( 9,DM.VLRCON$)                                ! Mensaje a pantalla
  Call MSG.ERR(10,DM.VLRDES$)                                ! Mensaje a pantalla
  Call MSG.ERR(12,DM.VALMED$)                                ! Mensaje a pantalla
  Call MSG.ERR(13,DM.VALMCA$)                                ! Mensaje a pantalla
  Call MSG.ERR(14,DM.VALIPS$)                                ! Mensaje a pantalla
  Call MSG.ERR(15,DM.VALDIA$)                                ! Mensaje a pantalla
  Call MSG.ERR(16,DM.VALFAC$)                                ! Mensaje a pantalla
  Call MSG.ERR(17,DM.VALBEN$)                                ! Mensaje a pantalla
  Call MSG.ERR(18,DM.FACTURA$)				     ! Mensaje a pantalla
  Call MSG.ERR(19,DM.DSCTO$)				     ! Mensaje a pantalla
  Call MSG.ERR(21,DM.VALMET$)                                ! Mensaje a pantalla
  Call MSG.ERR(22,DM.DSCVLR$)				     ! Mensaje a pantalla
  Call MSG.ERR(23,DM.METPRA$)                                ! Mensaje a pantalla
  Call MSG.ERR(24,DM.METPRB$)                                ! Mensaje a pantalla
  Call MSG.ERR(25,DM.METPRC$)                                ! Mensaje a pantalla
  Call MSG.ERR(26,DM.METPRD$)                                ! Mensaje a pantalla
  Call MSG.ERR(27,DM.PTG1$)                                  ! Mensaje a pantalla
  Call MSG.ERR(28,DM.PTG2$)                                  ! Mensaje a pantalla
  Call MSG.ERR(29,DM.PTG3$)                                  ! Mensaje a pantalla
  Call MSG.ERR(30,DM.PTG4$)                                  ! Mensaje a pantalla
  Call MSG.ERR(31,MSG$)                                      ! Mensaje a pantalla
  Call MSG.ERR(42,DM.CAPITACION$)                            ! Mensaje a pantalla
  Call MSG.ERR(44,DM.COPIATIQ$)                              ! Mensaje a pantalla
  DM.NIT$ = GET.DATOS		        		     ! Captura dato en pantalla
  If (ENDF = F3.SALIR) Then Exit Function                    ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                    ! Si presiona tecla ESC
  If VAL(DM.NIT$) <> 0 Then Call BUSQ.LIGCON
  While DM.NIT$ = " " OR BAN.PRG$ = "1"
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
      DM.NIT$    = GET.DATOS                                 ! Asigna valor capturado
      If (ENDF = F3.SALIR) Then Exit Function                ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  Then Exit Function                ! Si presiona tecla ESC
      If VAL(DM.NIT$) <> 0 Then Call BUSQ.LIGCON
  Wend
  DM.NIT$=Unpack$(DM.NIT$)
  Call MSG.ERR(31,"Nombre del Plan ")
  DM.RAZON$  = GET.DATOS				     ! Captura dato en pantalla
  If (ENDF = F3.SALIR) Then Exit Function                    ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                    ! Si presiona tecla ESC
  While DM.RAZON$  = " "
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
      DM.RAZON$  = GET.DATOS                                 ! Asigna valor capturado
      If (ENDF = F3.SALIR) Then Exit Function                ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  Then Exit Function                ! Si presiona tecla ESC
  Wend
  Call MSG.ERR(31,"Fecha Inicio de Plan   AAMMDD")
  DM.FECINI$ = GET.DATOS
  Call VAL.FECHA(DM.FECINI$)
  If (ENDF = F3.SALIR) Then Exit Function                     ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                     ! Si presiona tecla ESC
  While (DM.FECINI$ = " " OR BAN.PRG$ = "1")
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.FECINI$ = GET.DATOS
     Call VAL.FECHA(DM.FECINI$)
     If (ENDF = F3.SALIR) Then Exit Function                  ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function                  ! Si presiona tecla ESC
  Wend
  Call MSG.ERR(31,"Fecha Final del Plan  AAMMDD")
  DM.FECFIN$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                     ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                     ! Si presiona tecla ESC
  Call VAL.FECHA(DM.FECFIN$)
  While (DM.FECFIN$ = " " OR VAL(DM.FECFIN$) < VAL(DM.FECINI$) OR \
         BAN.PRG$ = "1")
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.FECFIN$ = GET.DATOS
     Call VAL.FECHA(DM.FECFIN$)
     If (ENDF = F3.SALIR) Then Exit Function                  ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function                  ! Si presiona tecla ESC
  Wend
  Call MSG.ERR(31,"Digite el Valor del Contrato")
  DM.VLRCON$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  While DM.VLRCON$ = " "
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.VLRCON$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
  Wend
  Call MSG.ERR(31,"Digite el Valor Maximo de despacho")
  DM.VLRDES$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  While DM.VLRDES$ = " "
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.VLRDES$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
  Wend
  Call MSG.ERR(31,"1. Valida Medico 0. No Valida Medico  2. No Captura Dato")
  DM.VALMED$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  While (MATCH(DM.VALMED$,"120",1) = 0 OR DM.VALMED$ = " ")
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.VALMED$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
  Wend
  Call MSG.ERR(31,"1. Por Articulo     0. Por Departamento                         ")
  DM.VALMCA$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  While (MATCH(DM.VALMCA$,"10",1) = 0 OR DM.VALMCA$ = " ")
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.VALMCA$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
  Wend
  Call MSG.ERR(31,"1. Valida IPS  0. No Valida IPS 2. No Captura Dato")
  DM.VALIPS$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  While (MATCH(DM.VALIPS$,"120",1) = 0 OR DM.VALIPS$ = " ")
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.VALIPS$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
  Wend
  Call MSG.ERR(31,"1. Valida Diagnosticos  0. No Valida Diagnosticos  2.No Captura Dato")
  DM.VALDIA$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  While (MATCH(DM.VALDIA$,"120",1) = 0 OR DM.VALDIA$ = " ")
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.VALDIA$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
  Wend
  Call MSG.ERR(31,"1. Valida Documento/Formula 0. No Valida Documento/Formula")
  DM.VALFAC$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  While (MATCH(DM.VALFAC$,"10",1) = 0 OR DM.VALFAC$ = " ")
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.VALFAC$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
  Wend
  Call MSG.ERR(31,"1. Valida Beneficiario  0. No Valida Beneficiario")
  DM.VALBEN$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  While (MATCH(DM.VALBEN$,"10",1) = 0 OR DM.VALBEN$ = " ")
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.VALBEN$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
  Wend

  Call MSG.ERR(31,"1. Si es Capitacion   0. No es Capitacion")
  DM.CAPITACION$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  While (MATCH(DM.CAPITACION$,"10",1) = 0 OR DM.VALBEN$ = " ")
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.CAPITACION$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
  Wend
  Call MSG.ERR(31,"1. Valor Comercial  0. Valor Catalogo  2. Porcentaje")
  DM.FACTURA$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  While (MATCH(DM.FACTURA$,"012",1) = 0 OR DM.FACTURA$ = " ")
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.FACTURA$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
  Wend

!-- Control Adicionado 24 Jul 2008
  Call MSG.ERR(31,"1. Genera Copia Tiquete 0. No Genera Copia Tiquete")
  DM.COPIATIQ$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  While (MATCH(DM.COPIATIQ$,"01",1) = 0 OR DM.COPIATIQ$ = " ")
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.COPIATIQ$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
  Wend

!-- Fin lineas adicionadas

 If DM.FACTURA$ = "2" Then Begin
  Call MSG.ERR(31,"Digite el valor del descuento ")
  DM.DSCTO$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  While DM.DSCTO$ = " " OR VAL(DM.DSCTO$) = 0
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.DSCTO$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
  Wend
EndIf ELSE Begin
      RET.ERR% = NXTF(2)				 ! no solicita el porcentaje
      Call DM.ERR(RET.ERR%,NXTF$)			 !
      DM.DSCTO$ = "000"				 	 ! Asigna porcentaje en cero
      Call MSG.ERR(17,DM.DSCTO$)			 ! presenta dato en pantalla
EndIf
  Call MSG.ERR(31,"Metodo Calculo Copago 1. Porcentaje  0. Valor ")
  DM.VALMET$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  While (MATCH(DM.VALMET$,"10",1) = 0 OR DM.VALMET$ = " ")
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.VALMET$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
  Wend
 If DM.VALMET$ = "0" Then Begin
  Call MSG.ERR(31,"1. Descuento por menor valor 0. Valor comercial producto")
  DM.DSCVLR$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  While (MATCH(DM.DSCVLR$,"10",1) = 0 OR DM.DSCVLR$ = " ")
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.DSCVLR$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
  Wend
 EndIf ELSE Begin
   RET.ERR% = NXTF(2)				 ! no solicita dato
   Call DM.ERR(RET.ERR%,NXTF$)			 !
   DM.DSCVLR$ = "0"
   DM.PTG1$ = "000"
   DM.PTG2$ = "000"
   DM.PTG3$ = "000"
   DM.PTG4$ = "000"
   Call MSG.ERR(20,DM.DSCVLR$)
   Call MSG.ERR(25,DM.PTG1$)
   Call MSG.ERR(26,DM.PTG2$)
   Call MSG.ERR(27,DM.PTG3$)
   Call MSG.ERR(28,DM.PTG4$)
 EndIf
  Call MSG.ERR(31,"Digite el Valor del Plan A                                      ")
  DM.METPRA$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  BAN.PRG$ = "0"
  If DM.VALMET$ = "1" Then Begin			  ! Valido porcentajes
     If VAL(DM.METPRA$) > 1000 Then BAN.PRG$ = "1"
  EndIf
  While DM.METPRA$ = " " OR BAN.PRG$ = "1"
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.METPRA$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
     BAN.PRG$ = "0"
     If DM.VALMET$ = "1" Then Begin			  ! Valido porcentajes
        If VAL(DM.METPRA$) > 1000 Then BAN.PRG$ = "1"
     EndIf
  Wend
  Call MSG.ERR(31,"Digite el Valor del Plan B                                      ")
  DM.METPRB$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  BAN.PRG$ = "0"
  If DM.VALMET$ = "1" Then Begin			  ! Valido porcentajes
     If VAL(DM.METPRB$) > 1000 Then BAN.PRG$ = "1"
  EndIf
  While DM.METPRB$ = " " OR BAN.PRG$ = "1"
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.METPRB$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
     BAN.PRG$ = "0"
     If DM.VALMET$ = "1" Then Begin			  ! Valido porcentajes
        If VAL(DM.METPRB$) > 1000 Then BAN.PRG$ = "1"
     EndIf
  Wend
  Call MSG.ERR(31,"Digite el Valor del Plan C                                      ")
  DM.METPRC$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  BAN.PRG$ = "0"
  If DM.VALMET$ = "1" Then Begin			  ! Valido porcentajes
     If VAL(DM.METPRC$) > 1000 Then BAN.PRG$ = "1"
  EndIf
  While DM.METPRC$ = " " OR BAN.PRG$ = "1"
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.METPRC$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
     BAN.PRG$ = "0"
     If DM.VALMET$ = "1" Then Begin			  ! Valido porcentajes
        If VAL(DM.METPRC$) > 1000 Then BAN.PRG$ = "1"
     EndIf
  Wend
  Call MSG.ERR(31,"Digite el Valor del Plan D                                      ")
  DM.METPRD$ = GET.DATOS
  If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
  If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
  BAN.PRG$ = "0"
  If DM.VALMET$ = "1" Then Begin			  ! Valido porcentajes
     If VAL(DM.METPRD$) > 1000 Then BAN.PRG$ = "1"
  EndIf
  While DM.METPRD$ = " " OR BAN.PRG$ = "1"
     RET.ERR% = NXTF(-2)
     Call DM.ERR(RET.ERR%,NXTF$)
     DM.METPRD$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
     BAN.PRG$ = "0"
     If DM.VALMET$ = "1" Then Begin			  ! Valido porcentajes
        If VAL(DM.METPRD$) > 1000 Then BAN.PRG$ = "1"
     EndIf
  Wend
  If DM.DSCVLR$ = "1" Then Begin
     Call MSG.ERR(31,"Digite descuento plan A ")
     DM.PTG1$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
     BAN.PRG$ = "0"
     If VAL(DM.PTG1$) > 100 Then BAN.PRG$ = "1"
     While DM.PTG1$ = " " OR BAN.PRG$ = "1"
        RET.ERR% = NXTF(-2)
        Call DM.ERR(RET.ERR%,NXTF$)
        DM.PTG1$ = GET.DATOS
        If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
        BAN.PRG$ = "0"
        If VAL(DM.PTG1$) > 100 Then BAN.PRG$ = "1"
     Wend
     Call MSG.ERR(31,"Digite descuento plan B ")
     DM.PTG2$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
     BAN.PRG$ = "0"
     If VAL(DM.PTG2$) > 100 Then BAN.PRG$ = "1"
     While DM.PTG2$ = " " OR BAN.PRG$ = "1"
        RET.ERR% = NXTF(-2)
        Call DM.ERR(RET.ERR%,NXTF$)
        DM.PTG2$ = GET.DATOS
        If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
        BAN.PRG$ = "0"
        If VAL(DM.PTG2$) > 100 Then BAN.PRG$ = "1"
     Wend
     Call MSG.ERR(31,"Digite descuento plan C ")
     DM.PTG3$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
     BAN.PRG$ = "0"
     If VAL(DM.PTG3$) > 100 Then BAN.PRG$ = "1"
     While DM.PTG3$ = " " OR BAN.PRG$ = "1"
        RET.ERR% = NXTF(-2)
        Call DM.ERR(RET.ERR%,NXTF$)
        DM.PTG3$ = GET.DATOS
        If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
        BAN.PRG$ = "0"
        If VAL(DM.PTG3$) > 100 Then BAN.PRG$ = "1"
     Wend
     Call MSG.ERR(31,"Digite descuento plan D ")
     DM.PTG4$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Exit Function                 ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Exit Function                 ! Si presiona tecla ESC
     BAN.PRG$ = "0"
     If VAL(DM.PTG4$) > 100 Then BAN.PRG$ = "1"
     While DM.PTG4$ = " " OR BAN.PRG$ = "1"
        RET.ERR% = NXTF(-2)
        Call DM.ERR(RET.ERR%,NXTF$)
        DM.PTG4$ = GET.DATOS
        If (ENDF = F3.SALIR) Then Exit Function              ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  Then Exit Function              ! Si presiona tecla ESC
        BAN.PRG$ = "0"
        If VAL(DM.PTG4$) > 100 Then BAN.PRG$ = "1"
     Wend
  EndIf ELSE Begin
     DM.PTG1$ = "000"
     DM.PTG2$ = "000"
     DM.PTG3$ = "000"
     DM.PTG4$ = "000"
  EndIf
  DM.FECINI$=Pack$(Right$("000000"+DM.FECINI$,6))            ! Fecha inicial del contrato
  DM.FECFIN$=Pack$(Right$("000000"+DM.FECFIN$,6))            ! Fecha final del contrato
  DM.VLRCON$=Pack$(Right$("0000000000"+DM.VLRCON$,10))       ! Valor contrato
  DM.VLRDES$=Pack$(Right$("00000000"+DM.VLRDES$,8))          ! Valor despacho
  DM.NIT$   =Pack$(Right$("0000000000"+DM.NIT$,10))          ! Numero de nit
  DM.METPRA$=Pack$(Right$("000000"+DM.METPRA$,6))            ! Metodo precio A
  DM.METPRB$=Pack$(Right$("000000"+DM.METPRB$,6))            ! Metodo precio B
  DM.METPRC$=Pack$(Right$("000000"+DM.METPRC$,6))            ! Metodo precio C
  DM.METPRD$=Pack$(Right$("000000"+DM.METPRD$,6))            ! Metodo precio D
  DM.PTG1$  =Right$("000"+DM.PTG1$,3)			     ! Descto < valor A
  DM.PTG2$  =Right$("000"+DM.PTG2$,3)			     ! Descto < valor B
  DM.PTG3$  =Right$("000"+DM.PTG3$,3)			     ! Descto < valor C
  DM.PTG4$  =Right$("000"+DM.PTG4$,3)			     ! Descto < valor D
  DM.DSCTO$ =Right$("000"+DM.DSCTO$,3)			     ! Descto al convenio
  LEC$="C8 C40 2C3 C5 C4 4C1 4C3 C5 3C1 4C3 C3 4C1"	     !
  Write Form LEC$;#AREA1%;                                  \! Graba registro Convenio
       DM.NEWKEY$, DM.RAZON$,  DM.FECINI$, DM.FECFIN$,      \!
       DM.VLRCON$, DM.VLRDES$, DM.VALMED$, DM.VALMCA$,      \!
       DM.VALIPS$, DM.VALMET$, DM.METPRA$, DM.METPRB$,      \!
       DM.METPRC$, DM.METPRD$, DM.NIT$,    DM.VALDIA$,      \!
       DM.FACTURA$, DM.DSCVLR$, DM.PTG1$,  DM.PTG2$,        \!
       DM.PTG3$,   DM.PTG4$,  DM.DSCTO$,   DM.VALFAC$,      \!
       DM.VALBEN$, DM.CAPITACION$, DM.COPIATIQ$
  If (ENDF = F7.KEY)   Then Call VISUAL.REG2                 ! Si presiona tecla F7
END Function
!--- Fin del mantenimiento del registro

Function INICIO3
      Call.ORDER% = 230                                    ! Llamado Primera Pantalla D.M
      RET.ERR% = DISPD(Call.ORDER%)                        ! Llamado de la pantalla en DM
      Call DM.ERR(RET.ERR%,DISPD$)
      Call MSG.ERR(1,MSG$)				   ! Mensaje cabecera pantalla
      MSG$ = "Digite el Codigo del Convenio"
END Function
!--- Fin del inicio de pantalla

!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GoTo E0107
Call INICIADM 				                   ! Inicializacion Variables Display Manager
AREA1% = 11: AREA2%=12				           ! Definicion area de trabajo archivo
ARCH1$="CONVENIO"	                       	   ! Asignar nombre archivo control
ARCH2$="LIGCONVE"	                       	   ! Asignar nombre archivo 
Open ARCH2$ KEYED RECL 57  AS AREA2%                        ! Abre archivo de ligas convenios
Open ARCH1$ KEYED RECL 106 AS AREA1%                        ! Abre archivo de planes
TERM$ = " "					           ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   !
Call DM.ERR(RET.ERR%,INITDM$)				   !
RET.ERR% = OPNDIS("ADX_UPGM:EPSPANTA.PNT")	           ! Apertura de la forma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
While (FIN$ = "0") 
 CICLO:
      DM.OPCION$   = "  "
      MSG$ = "MANTENIMIENTO DE PLAN/CONVENIO "
      Call INICIO3
      Call MSG.ERR(31,MSG$)				   ! Mensaje cabecera pantalla
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.CONVEN$ = INP$                                    ! Asigna valor capturado
      If (ENDF = F1.AYUDA) Then Begin \
         Call HELP("DEFINICION PLAN/CONVENIO ","EPSP0107.TXT") !
         Call INICIO3
      EndIf
      If (ENDF = F3.SALIR) Then Call QUIT                  ! Si presiona tecla F3
      If (ENDF = F7.KEY)   Then Begin
         Close AREA1%
         Open ARCH1$ DIRECT RECL 512 AS AREA1% NODEL       ! Open file
         Call VISUAL.REG2                                  ! Si presiona tecla F7
         Close AREA1%
         Open ARCH1$ KEYED RECL 106 AS AREA1%               ! Abre archivo de 
         GoTo CICLO
      EndIf
      If (ENDF = ESC.KEY)  Then Call QUIT                  ! Si presiona tecla ESC
      While (DM.CONVEN$ = " ")
         RET.ERR% = NXTF(-20)
         Call DM.ERR(RET.ERR%,NXTF$)
         INP$ = UPDF
         DM.CONVEN$ = INP$
         If (ENDF = F1.AYUDA) Then Begin \
            Call HELP("DEFINICION PLAN/CONVENIO ","EPSP0107.TXT") !
            Call INICIO3
         EndIf
         If (ENDF = F3.SALIR) Then Call QUIT                      ! Si presiona tecla F3
         If (ENDF = ESC.KEY)  Then Call QUIT                      ! Si presiona tecla ESC
         If (ENDF = F7.KEY)   Then Begin
            Close AREA1%
            Open ARCH1$ DIRECT RECL 512 AS AREA1% NODEL       ! Open file
            Call VISUAL.REG2                                  ! Si presiona tecla F7
            Close AREA1%
            Open ARCH1$ KEYED RECL 106 AS AREA1%               ! Abre archivo de control
            GoTo CICLO
         EndIf
     Wend

     Call MSG.ERR(31,"Digite Codigo del Plan  ")
     DM.PLAN$ = GET.DATOS
     If (ENDF = F3.SALIR) Then Call Quit                     ! Si presiona tecla F3
     If (ENDF = ESC.KEY)  Then Call Quit                     ! Si presiona tecla ESC
     While DM.PLAN$ = " " OR Val(DM.PLAN$) < 0 
        RET.ERR% = NXTF(-2)
        Call DM.ERR(RET.ERR%,NXTF$)
        DM.PLAN$ = GET.DATOS
        If (ENDF = F3.SALIR) Then Call Quit                  ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  Then Call Quit                  ! Si presiona tecla ESC
     Wend     
     MSG$="Digite Codigo del Convenio "
     Call MNT.REG4					   ! Mantenimiento Registro
Wend

!----
!---- Fin del bloque principal
!----

E0107:
         If ERRF% = AREA1% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") Then Begin          ! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 8,,,5000 RECL 106 \! si no existe lo crea.
                        AS AREA1% compound perupdate 		   ! 
            Resume                                         ! retorna ejecucion
         EndIf                                             ! del programa

         If ERRF% = AREA2% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") Then Begin          ! el archivo
            CREATE POSFILE ARCH2$ KEYED 5,,,1000 RECL 57  \! si no existe lo crea.
                        AS AREA2% compound perupdate 			   ! 
            Resume                                         ! retorna ejecucion
         EndIf                                             ! del programa
         If ERRF% = AREA2% AND ERR = "EF" Then Begin      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! ejecucion normal del
            Resume
         EndIf

         If ERRF% = AREA1% AND ERR = "EF" Then Begin      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! ejecucion normal del
            Resume
         EndIf
         If ERRF% = 19 AND ERR = "EF" OR ERR="OE" Then Begin  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            Resume					       ! help del aplicativo
         EndIf						       !
         Call TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         Locate 24,1:PRINT MSG$                          ! Presenta Error pantalla
!--- Fin despliegue de errores

!--- Fin del programa EPSP0107.BAS
