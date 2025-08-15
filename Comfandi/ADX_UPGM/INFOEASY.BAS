!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : InfoEasy.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Septiembre 6 de 2.004            *
!Observaciones : Reporte Fiscal Para la DIAN      *
!**************************************************
! Modificaciones:
! Mod21092004
! Se modifica el diseño de la planilla fiscal para 
! que los cupones se tomen como descuentos dentro 
! de la transaccion y los reporte dentro de cada
! uno de los departamentos que afecto.
! desarrollado por Oscar Valencia 21 Sept 2004
!----------------------------------------------------

%ENVIRON C						           ! Ambiente de controlador

String    Global U.Fecha$, Buffer$, Deptos$(1),Dsctos$(1), Pagos$(1), Meses$(1), Miscel$(1)
String    Global U.Prefijo$, Rastro$, Entrada$, E.Salida$, EMsg$, E.Interfaz$
Integer*1 Global Ban.Prg%, IND.BKG%
Integer*4 Global UE.Tablaiva(1), TOT1%, TOT2%, TOT3%, TOT4%

%Include POSPVARI.BAS					   ! Variables del programa
%Include DMEXTR.J86          		 ! Inclucion Libreria Display Manager
%INCLUDE PORPRUTI.BAS				  	 ! Rutinas Comunes
%INCLUDE BASROUT.J86					   ! 

SUB ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  ! Msg background
   INTEGER*4 RET
   INTEGER*2 FUNC,PARM1
   STRING PARM2
END SUB

Sub Msg.Bkg
Integer*4 V%
  CALL ADXSERVE(0,26,1,EMsg$)
  Print EMsg$
End Sub 

Function INICIO1
  CALL.ORDER% = 2                                          ! Llamado Primera Pantalla D.M
  RET.ERR% = DISPD(CALL.ORDER%)                            ! Llamado de la pantalla en DM
  CALL DM.ERR(RET.ERR%,DISPD$)
End Function 
!--- Fin de la funcion de inicio

Function LEER.CABECERA
 String PARM2$
 INTEGER*2 PARM1,RET
 Call ADXSERVE(RET,4,PARM1,PARM2$)                 ! 
 DM.ALMACEN$ = RIGHT$("000"+STR$(VAL(MID$(PARM2$,1,4))),3)
End Function 
!--- Fin de la funcion de lectura

Sub Datos.Tienda
String XALM$
  Ban.Prg% = -1
  Open "ADX_IDT1:ALMACEN.DAT" As 78
  If Ban.Prg% = -1 Then BEGIN 
  	 READ # 78 ; XALM$
  	 If Ban.Prg% = -1 Then \
  	 	  DM.ALMACEN$ = RIGHT$("000"+STR$(VAL(XALM$)),3)
     Close 78
  EndIf 
End Sub 

Function Termine.Prog
  EMsg$ = "Fin Reporte "+E.Salida$  
  If IND.BKG% = 0 Then \
     Call msg.err(6,EMsg$) Else \
     Call Msg.Bkg
  Wait ; 1200
 If Ind.Bkg% = 0 Then Begin 
  Call SETF("0000000")				   !
  Call CLRSCR					   !
  RET.ERR%= CLSDIS				   !
  Call DM.ERR(RET.ERR%,CLSDIS$)	                   !  
  Close 91 
  Create E.Interfaz$ As 77
  Close 77
  Stop 
 EndIf 
End Function
!--- Fin de la ejecucion del programa

Function PANTALLA.PRINCIPAL
String U.AA$, U.MM$, U.DD$
AREA1% = 11: AREA2% = 12: AREA3%=13: AREA4%=14              ! Definicion area de trabajo archivo
FIN$="0"						    ! Variable control creacion archivo
ARCH1$="EAMACFIS"			                          ! Archivo Acumulador Fiscal
ARCH2$="EAMNMFIS"				                        ! Archivo Definicion Numeracion Fiscal
Open ARCH1$ KEYED RECL 15 AS AREA1% 	          ! Apertura acumulador fiscal
Open ARCH2$ KEYED RECL 42 AS AREA2%             ! Apertura datos numeracion fiscal
If Ind.Bkg% = 0 Then Begin 
  Call INICIADM 				                    ! Inicializacion Variables Display Manager
  TERM$ = " "					            ! Inicializo Libreria
  RET.ERR%=INITDM(TERM$)					    !
  CALL DM.ERR(RET.ERR%,INITDM$)				    !
  RET.ERR% = OPNDIS("ADX_UPGM:FISCAL.PNT")	            ! Apertura de la forma de pantalla
  CALL DM.ERR(RET.ERR%,OPNDIS$)
  CALL INICIO1 
EndIf 
      If Len(Entrada$) = 6 Then GoTo Procesa.Dia            ! Una fecha valida formato AAMMDD

      CALL MSG.ERR(06,"Digite el destino del reporte")
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.CODIGO$ = INP$                                    ! Asigna valor capturado
      IF (ENDF = F1.AYUDA) THEN BEGIN \
         CALL HELP("Reporte Movimietos Almacen","infosri.TXT") !
         CALL INICIO1
      ENDIF
      IF (ENDF = F3.SALIR) THEN CALL TERMINE.PROG                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN CALL TERMINE.PROG                 ! Si presiona tecla ESC
      WHILE (MATCH(DM.CODIGO$,"13",1) = 0 OR DM.CODIGO$ = " ")
         CALL MSG.ERR(06,"Digite el destino del reporte")
         RET.ERR% = NXTF(-20) 			                  ! Primer campo
         CALL DM.ERR(RET.ERR%,NXTF$)
         ATTR$ = SETF(PRM.ON$)                                
         INP$ = UPDF                                              ! Captura dato en pantalla
         DM.CODIGO$ = INP$                                        ! Asigna valor capturado
         IF (ENDF = F1.AYUDA) THEN BEGIN \
            CALL HELP("Reporte Movimientos Almacen","infosri.TXT") !
            CALL INICIO1
         ENDIF
         IF (ENDF = F3.SALIR) THEN CALL TERMINE.PROG              ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN CALL TERMINE.PROG              ! Si presiona tecla ESC
      Wend
      CALL MSG.ERR(06,"Digite la Fecha del Reporte")
      U.AA$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN CALL TERMINE.PROG               ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN CALL TERMINE.PROG               ! Si presiona tecla ES
      U.MM$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN CALL TERMINE.PROG               ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN CALL TERMINE.PROG               ! Si presiona tecla ES
      U.DD$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN CALL TERMINE.PROG               ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN CALL TERMINE.PROG               ! Si presiona tecla ESC
      U.Fecha$ = U.AA$ + U.MM$ + U.DD$
      If DM.CODIGO$ = "3" Then BEGIN
         CREATE POSFILE "ADX_UDT1:RP"+U.Fecha$+"."+DM.ALMACEN$ AS 91
         E.Salida$ = "ADX_UDT1:RP"+U.Fecha$+"."+DM.ALMACEN$
         E.Interfaz$ = "ADX_UDT1:RP"+U.Fecha$+".TRG"
      EndIf
      Exit Function 
      Procesa.Dia:
!      U.Fecha$ = DATE$
!      CREATE POSFILE "ADX_UDT1:RP"+U.Fecha$+"."+DM.ALMACEN$ AS 91

      IND.BKG% = -1
      U.Fecha$ = ENTRADA$
      E.Salida$ = "ADX_UDT1:RP"+U.Fecha$+"."+DM.ALMACEN$
      E.Interfaz$ = "ADX_UDT1:RP"+U.Fecha$+".TRG"
      CREATE POSFILE "ADX_UDT1:RP"+U.Fecha$+"."+DM.ALMACEN$ AS 91
         
End Function 

Sub Cabecera.Reporte
 CONLI% = 0: PAGI% = 0: I%=0
 LET.CON$ = CHR$(15)            				  ! Condensar  letra de impresion
 LET.NOR$ = CHR$(18)            				  ! Normalizar letra de impresion 
 LET.GRA$ = CHR$(14)            				  ! Agrandar   letra de impresion
 LINEA$   = STRING$(131,"-")        		  ! Linea Continua en la impresion
 If DM.CODIGO$ = "1" Then LPRINTER
End Sub
!--- Cabecera del reporte

Function Detalle.Linea(Rbuffer$)
String Rbuffer$, X.Lec$
Integer*4 X.Len%
FINR$=CHR$(13)+CHR$(10)
If DM.CODIGO$ = "1" Then BEGIN
   Print Rbuffer$
EndIf Else BEGIN
   X.Len% = Len(Rbuffer$)														  								! Toma longitud del registro
   X.Lec$ = "C"+Str$(X.len%)+" C2"										  							! Arma estructura de grabacion
   Write form X.Lec$; #91 ; Rbuffer$, Finr$              							! Graba registro
EndIf
End Function
!--- Detalle reporte fiscal

Function Detalle.Reporte(U.MSG$, U.L%)
String f.rp$, U.MSG$, U.Fec2$, X.LEN$
Integer*1 U.L%  
Integer*4 X.LEC%

  F.RP$ = "20"+DATE$
  
  If CONLI% > 55 OR PAGI% = 0 Then Begin  			  												! Final de Hoja o Inicio pagina
     If CONLI% > 55 Then Begin																						! Control salto pagina
       Call detalle.linea(LINEA$)      																		! Impresion linea
       Call detalle.linea(CHR$(12))  																			! Salto de Pagina
     EndIf																																!
 PAGI%=PAGI%+1			  	  																								! Inicializa numero de pagina
 !Call Detalle.Linea( LET.CON$)			      																								! Activa la letra condensada
 Call Detalle.Linea( LINEA$)				  																								! Imprime linea continua
 !Call Detalle.Linea( LET.NOR$)					  																								! Activa la letra normal
 Call Detalle.Linea( String$(25," ")+"COMPROBANTE DE INFORME DIARIO")                    ! Titulo del reporte
 !Call Detalle.Linea( LET.CON$)			      																								! Activa la letra condensada
 LISTA$ =DM.RAZON$+STRING$(33," ")+U.MSG$+STRING$(39," ") +              \! Arma linea de impresion
 "Pagina No. "+RIGHT$(STRING$(04,"0")+STR$(PAGI%),04)                     !
 Call Detalle.Linea( LISTA$)  																														!
 Call Detalle.Linea( DM.SIGLA$+"        "+String$(85," ")+"Fecha  : "+F.RP$)      			  !
 Call Detalle.Linea( LINEA$	)																														!
 !Call Detalle.Linea( LET.NOR$)					  																								! Activa la letra normal
 U.Fec2$ = Mid$(U.Fecha$,1,2) + "/" + \
           Meses$(Val(Mid$(U.Fecha$,3,2))) + "/" +\
           Mid$(U.Fecha$,5,2)
 Call Detalle.Linea( "Nit Empresa "+DM.NIT$+" Fecha Movimiento : 20"+U.Fec2$)
 Call Detalle.Linea( " ")
  If U.MSG$ = "REPORTE DEPARTAMENTAL" Then Begin 
!                    1         2         3         4         5         6         7         8         9        10        11        12        13
!           123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012  
      !Call Detalle.Linea( LET.CON$)			      																								! Activa la letra condensada
      Call Detalle.Linea("Departamento                  Tarifa       Base Compra      Vlr. Neto        IVA             Total")
!                                                       xxxxxxxxxx     xxxxxxxxxx     xxxxxxxxxx     xxxxxxxxxx    xxxxxxxxxx             
  EndIf 
  If U.MSG$ = "INFORME TARIFAS IVA  "  Then Begin
     Call Detalle.Linea( "                           T O T A L   T R I B U T A D O                        ")
     Call Detalle.Linea( " ")
     !Call Detalle.Linea( " Tarifa         IVA%        Base Gravada        Vlr.Impuesto        Tot IVA Fact")
     
     Call Detalle.Linea( " Tarifa         IVA%        Base Gravada        Vlr.Impuesto        Tot IVA Fact      Tot Dscto Tarifa")
     Call Detalle.Linea( "--------       ------       ------------        ------------        ------------      ----------------") 
  EndIf 
  If U.MSG$ = "INFORME VTAS X CAJA  "  Then Begin
     Call Detalle.Linea( "          V E N T A S   R E G I S T R A D A S    P O R    T E R M I N A L       ")
     Call Detalle.Linea( " ")
     Call Detalle.Linea( " Nro.      Fac. Inicial   Fac.Final     Venta       Descuento       Total Venta ")
     Call Detalle.Linea( "-------    ------------   ----------  ----------   ----------       ------------")
     Call Detalle.Linea( " ")
  EndIf 
  If U.MSG$ = "DESCUENTOS OTORGADOS " Then Begin
     Call Detalle.Linea( "                     D E S C U E N T O S    O T O R G A D O S                   ")
     Call Detalle.Linea( " ")
     Call Detalle.Linea( " Concepto Descuento                                              Vlr. Descuento ")
     Call Detalle.Linea( "----------------------                                           ---------------" ) 
  EndIf
  If U.MSG$ = "   MEDIOS DE PAGO    " Then Begin 
     Call Detalle.Linea( "                         M E D I O S    D E    P A G O                          ")
     Call Detalle.Linea( " ")
     Call Detalle.Linea( "    Tipo de Pago                 Nro. Transac.                   Vlr. Recaudado ")
     Call Detalle.Linea( "----------------------          --------------                   ---------------" ) 
  EndIf 
  If U.MSG$ = "REPORTE MISCELANEOS" Then Begin
     Call Detalle.Linea( "                     M I S C E L A N E O S    O T O R G A D O S                 ")
     Call Detalle.Linea( " ")
     Call Detalle.Linea( " Concepto Miscelaneo                                             Vlr. Miscelaneo")
     Call Detalle.Linea( "----------------------                                           ---------------" ) 
  EndIf
  
  CONLI% = 11
 EndIf 
 !If U.L% = 1 Then Call Detalle.Linea( Let.Con$) Else Call Detalle.Linea( Let.Nor$)
 Call Detalle.Linea( buffer$)
 CONLI% = 1 + CONLI%
End Function  
!-- Detalle del reporte

Sub REPORTE.MISCELANEOS
Integer*4 U.d1%, U.d2%, U.I%, U.J%, U.T1%, U.T2%, U.T3%
String    U.Key$, U.Depto$, U.dpt$, U.Iva$, TB$
  Rastro$ = "reporte.miscel"
  EMsg$ = "Generando Reporte Miscelaneos"
  If Ind.Bkg% = 0 Then \
     Call MSG.ERR(06,EMsg$) Else \
     Call Msg.Bkg
  
  Call Cabecera.Reporte 
  For U.I% = 1 to 999
      U.Iva$ = Right$("000"+Str$(U.I%),3)         ! Tariva de descuento
      U.Key$ = PACK$("M"+U.FECHA$+U.Iva$)         ! P+Fecha Movimiento+descuento
      BAN.PRG% = -1
      Read Form "C5 2I4 C2" ; #Area1% Key U.Key$ ; \! Si ya existe informacion
           U.Dpt$, U.d1%, U.d2%, TB$            ! 
       If Ban.Prg%  = -1 Then Begin               ! Si no existe init variables
          
          If U.I% <> 9 Then u.d1% = u.d1% * -1		! Es una donacion 
          
          BUFFER$ = U.Iva$+" "+Left$(Miscel$(u.i%)+String$(20," "),20) + \
                    String$(6," ")+"          "+"          "+ \
                    "          "+"          "+Right$("          "+Str$(u.d1%),10)

          Call DETALLE.REPORTE("REPORTE MISCELANEOS",0)
          U.T1% = U.T1% + U.D1%
          U.T2% = U.T2% + U.D2%
          U.T3% = U.T3% + (U.D1% - U.D2%)
       EndIf 
  Next U.I%
  Call Detalle.Linea( " ")
  Call Detalle.Linea( "TOTALES GENERALES"+String$(23," ") + "          " + "          " + \
        "          " + Right$("          "+Str$(U.T3%),10))
  Call Detalle.Linea( LINEA$)						      ! Impresion de linea
  Call Detalle.Linea( CHR$(12))    					  ! Salto de Pagina
  TOT1% = U.T3%  ! Total Miscelaneos
End Sub 

Sub REPORTE.DEPARTAMENTOS
Integer*4 U.d1%, U.d2%, U.I%, U.J%, U.T1%, U.T2%, U.T3%, U.d11%, U.d22%
Integer*4 Vta.Depto%, U.T4%, U.T5%, U.Base%, U.Iva%
String    U.Key$, U.Depto$, U.dpt$, Alterno$, U.dpt2$, Tiva$, TB$, Uj$
  Rastro$ = "reporte.deptos"
  EMsg$ = "Generando Reporte Departamental"
  If Ind.Bkg% = 0 Then \
     Call MSG.ERR(06,EMsg$) Else \
     Call Msg.Bkg

  Call Cabecera.Reporte
  For U.I% = 1 to 99
      U.Depto$ = Right$("00"+Str$(U.i%),2)
   For U.J% = 0 to 99
       UJ$ = Right$("00"+Str$(U.J%),2)
       U.Key$ = Pack$(U.Fecha$+U.Depto$+UJ$)         ! Fecha Movimiento+Departamento
       !locate 18,1 : Print "Buscando "+UnPack$(U.Key$) 
       Ban.Prg% = -1 
       Read Form "C5 2I4 C2" ; #Area1% Key U.Key$ ; \! Si ya existe informacion
           U.Dpt$, U.d1%, U.d2%, TB$            ! 
       If Ban.Prg%  = -1 Then Begin                  ! Si no existe init variables
          
          !Alterno$ = "ABCDEFGH"
          !Tiva$ = MID$(ALTERNO$,u.j%,1)                  ! Toma alterno tarifa de IVA
          !U.Key$ = PACK$(Tiva$+U.Fecha$+U.Depto$)        ! Fecha Movimiento+Departamento+Iva
          !Ban.Prg% = -1 
!--- Busqueda cupones de descuento
          !Read Form "C5 2I4 C2" ; #Area1% Key U.Key$ ;     \! Si ya existe informacion
          !     U.Dpt2$, U.d11%, U.d22%, TB$                   ! 
          !If Ban.prg% <> -1 Then Begin                   ! No hay descuento

          !EndIf 

          U.D11% = 0 : U.D22% = 0          
          
        ! Right$("          "+Str$(u.d11%),10)+"     "+                 ! Descuento Otorgado          

          U.Base%  = ROUND(FLOAT(U.d1%)/(1.+FLOAT(U.J%)/100.),0,0)
          U.Iva% = ROUND(FLOAT(U.d1%)/(1.+FLOAT(U.J%)/100.)*FLOAT(U.J%)/100.,0,0)
          
         
!          Vta.Depto% = (U.d1% - U.d11%) + (U.d2% - U.d22%)

           Vta.Depto% = U.d1%
        
          Tiva$ = Right$("00"+Str$(U.J%),2) + "%"

!          BUFFER$ = U.DEPTO$+" "+Left$(Deptos$(Val(u.depto$))+"                    ",20) + \
!                    "      " + Tiva$ + "           "   +                         \! Tarifa de Iva
!                    Right$("          "+Str$(u.Base%),10)+"     "+                 \! Base Compra
!                    "          "                 +"     "+                       \! Descuento Otorgado
!                    Right$("          "+Str$(u.Base% - u.d11%),10)+"     "+        \! Valor Neto
!                    Right$("          "+Str$(u.Iva% - u.d22%),10)+"    "+         \! Vlr Iva
!                    Right$("          "+Str$(Vta.Depto%),10)                      ! Total

          BUFFER$ = U.DEPTO$+" "+Left$(Deptos$(Val(u.depto$))+"                    ",20) + \
                    "      " + Tiva$ + "           "   +                           \! Tarifa de Iva
                    Right$("          "+Str$(u.Base%),10)+"     "+                 \! Base Compra
                    Right$("          "+Str$(u.Base% - u.d11%),10)+"     "+        \! Valor Neto
                    Right$("          "+Str$(u.Iva% - u.d22%),10)+"    "+          \! Vlr Iva
                    Right$("          "+Str$(Vta.Depto%),10)                        ! Total

          Call DETALLE.REPORTE("REPORTE DEPARTAMENTAL",1)
          U.T1% = U.T1% + U.Base%
          U.T2% = U.T2% + U.D11%
          U.T3% = U.T3% + (U.Base% - U.D11%)
          U.T4% = U.T4% + (U.Iva% - U.D22%)
          U.T5% = U.T5% + Vta.Depto%
       EndIf 
   Next U.J% 
  Next U.I%
  Call Detalle.Linea( " ")
!  Call Detalle.Linea( "TOTALES GENERALES"+String$(27," ") + \
!        Right$("          "+Str$(U.T1%),10) + "     " + \ Total de Base de Compra
!        Right$("          "+Str$(U.T2%),10) + "     " + \ Total Descuentos         
!        Right$("          "+Str$(U.T3%),10) + "     " + \ Total Vlr Neto              
!        Right$("          "+Str$(U.T4%),10) + "    "  + \ Total de Base de Iva
!        Right$("          "+Str$(U.T5%),10))             ! Total General

  Call Detalle.Linea( "TOTALES GENERALES"+String$(27," ") + \
        Right$("          "+Str$(U.T1%),10) + "     " + \ Total de Base de Compra
         Right$("          "+Str$(U.T3%),10) + "     " + \ Total Vlr Neto              
        Right$("          "+Str$(U.T4%),10) + "    "  + \ Total de Base de Iva
        Right$("          "+Str$(U.T5%),10))             ! Total General
  
  
  
  Call Detalle.Linea( LINEA$)						      ! Impresion de linea
  
  Call Detalle.Linea( CHR$(12) )   					  ! Salto de Pagina
  TOT2% = U.T5%    ! Total General
  
End Sub


Sub REPORTE.DESCUENTOS
Integer*4 U.d1%, U.d2%, U.I%, U.J%, U.T1%, U.T2%, U.T3%
String    U.Key$, U.Depto$, U.dpt$, U.Iva$, TB$
  Rastro$ = "reporte.dsctos"
  EMsg$ = "Generando Reporte Descuentos"
  If Ind.Bkg% = 0 Then \
     Call MSG.ERR(06,EMsg$) Else \
     Call Msg.Bkg
  
  Call Cabecera.Reporte 
  For U.I% = 1 to 99
      U.Iva$ = Right$("000"+Str$(U.I%),3)         ! Tariva de descuento
      U.Key$ = PACK$("P"+U.FECHA$+U.Iva$)         ! P+Fecha Movimiento+descuento
      BAN.PRG% = -1
      Read Form "C5 2I4 C2" ; #Area1% Key U.Key$ ; \! Si ya existe informacion
           U.Dpt$, U.d1%, U.d2%, TB$            ! 
       If Ban.Prg%  = -1 Then Begin               ! Si no existe init variables
       
          BUFFER$ = U.Iva$+" "+Left$(Dsctos$(u.i%)+String$(20," "),20) + \
                    String$(6," ")+"          "+"          "+ \
                    "          "+"          "+Right$("          "+Str$(u.d1%),10)

          Call DETALLE.REPORTE("DESCUENTOS OTORGADOS ",0)
          U.T1% = U.T1% + U.D1%
          U.T2% = U.T2% + U.D2%
          U.T3% = U.T3% + (U.D1% - U.D2%)
       EndIf 
  Next U.I%
  Call Detalle.Linea( " ")
  Call Detalle.Linea( "TOTALES GENERALES"+String$(23," ") + "          " + "          " + \
        "          " + Right$("          "+Str$(U.T3%),10))
  Call Detalle.Linea( LINEA$)						  ! Impresion de linea
  Call Detalle.Linea( CHR$(12))    					  ! Salto de Pagina
  TOT3% = U.T3%   ! Total Descuentos
End Sub 

Sub REPORTE.IVAS
Integer*4 U.d1%, U.d2%, U.I%, U.J%, U.T1%, U.T2%, U.T3%, U.T4%
String    U.Key$, U.Depto$, U.dpt$, U.Iva$, TB$
Integer*4 U.Base%, U.Iva%
  Rastro$ = "reporte.ivas"  
  EMsg$ = "Generando Reporte Impuestos"
  If Ind.Bkg% = 0 Then \
     Call MSG.ERR(06,EMsg$) Else \
     Call Msg.Bkg
  
  Call Cabecera.Reporte 
  For U.I% = 0 to 99 
      U.Iva$ = Right$("000"+Str$(U.I%),3)        ! Tariva de Iva
      U.key$ = PACK$("I"+U.FECHA$+U.Iva$)     ! I+Fecha Movimiento+tarifa iva
      BAN.PRG% = -1
      Read Form "C5 2I4 C2" ; #Area1% Key U.Key$ ; \! Si ya existe informacion
           U.Dpt$, U.d1%, U.d2%, TB$            ! 
       If Ban.Prg%  = -1 Then Begin               ! Si no existe init variables
          
          U.Base%  = ROUND(FLOAT(U.d1%)/(1.+FLOAT(U.I%)/100.),0,0)
          U.Iva% = ROUND(FLOAT(U.d1%)/(1.+FLOAT(U.I%)/100.)*FLOAT(U.I%)/100.,0,0)

          BUFFER$ = "   "+Right$("00"+Str$(U.I%),2)+String$(13," ")+Right$("00"+Str$(U.I%),2)+ \
                    String$(11," ")+Right$("          "+Str$(U.Base%),10)+"          "+ \ 
                    Right$("          "+Str$(U.Iva%),10)+"          "+\
                    Right$("          "+Str$(U.Base% + U.Iva%),10) + "           " + \
                    Right$("          "+Str$(U.d2%),10)

          Call DETALLE.REPORTE("INFORME TARIFAS IVA  ",0)
          U.T1% = U.T1% + U.Base%
          U.T2% = U.T2% + U.Iva%
          U.T3% = U.T3% + (U.D1%)
          U.T4% = U.T4% + (U.D2%)
       EndIf 
  Next  u.i%
  Call Detalle.Linea( " ")
  Call Detalle.Linea( "TOTALES GENERALES "+String$(13," ") + Right$("          "+Str$(U.T1%),10) + \
         "          " + Right$("          "+Str$(U.T2%),10) + "          " + Right$("          "+Str$(U.T3%),10) + \
         "           " + Right$("          "+Str$(U.T4%),10))
         
  Call Detalle.Linea( CHR$(15)+LINEA$	)	      ! Impresion de linea
  
  TOT4% = TOT1% - TOT3% + TOT2%   ! Total Movimiento
  If TOT4% <> U.T3% Then Begin    ! Diferencia 
     Tot1% = U.T3% - TOT4%
     Call Detalle.Linea( "Diferencia Totales "+Right$("          "+Str$(TOT1%),10) )
  EndIf 
  Call Detalle.Linea( CHR$(12) )   					  ! Salto de Pagina
End Sub 

Function Busca.Fiscal(u.key$)
String U.KEY$,U.SERIAL$, U.PREF$, U.AUT$, U.FEC$, LEC$, U.K$, U.TIPO$, U.RESOL$
Integer*4 U.INI%, U.FIN%, U.ACT%, U.NOTA%, U.NC%
  Rastro$ = "busca.fiscal"
  BAN.PRG% = -1 
  U.KEY$ = Pack$(Right$("0000"+U.Key$,4))
  
  LEC$="C2 C15 C4 2I4 C6 C3 I4"	                           !
  Read FORM LEC$;#AREA2% KEY U.KEY$;                      \! Lee Reg Cabecera 
  U.K$,U.SERIAL$, U.PREF$, U.INI%, U.FIN%, U.AUT$, U.FEC$, U.ACT%
  
  !LEC$="C2 3C15 C4 2I4 C6 C3 3I4"	                       !
  !READ FORM LEC$;#AREA2% KEY U.KEY$;                      \! Lee Reg Cabecera 
  !U.KEY$, U.TIPO$, U.SERIAL$, U.RESOL$, U.PREF$, U.INI%, U.FIN%, U.AUT$, U.FEC$, U.ACT%,U.NOTA%, U.NC%
  
  IF BAN.PRG% = -1 THEN BEGIN
     U.PREFIJO$ = Right$("    "+U.PREF$,4)
  ENDIF ELSE U.PREFIJO$ = "NDEF"
End FUNCTION
!---

Sub REPORTE.TERMINALES
Integer*4 U.d1%, U.d2%, U.I%, U.J%, U.T1%, U.T2%, U.T3%,U.d11%, U.d22%
String    U.Key$, U.Depto$, U.dpt$, U.Iva$, U.dpt2$
String    Fini$, Ffin$, TB$
  Rastro$ = "reporte.terminales"
  EMsg$ = "Generando Reporte Venta por Terminal"
  If Ind.Bkg% = 0 Then \
     Call MSG.ERR(06,EMsg$) Else \
     Call Msg.Bkg
  
  Call Cabecera.Reporte 
  For U.I% = 1 to 99
      U.Iva$ = Right$("000"+Str$(U.I%),3)            ! Numero de terminal
      Call Busca.Fiscal(U.Iva$)                      ! Busca Prefijo fiscal
      U.Iva$ = Right$("000"+Str$(900 + U.I%),3)      ! Numero de terminal
      Ban.Prg% = -1
      U.Key$ = Pack$("1"+U.FECHA$+U.Iva$)            ! 1+Fecha Movimiento+terminal 
      Read Form "C5 2I4 C2" ; #Area1% Key U.Key$ ;  \! Si ya existe informacion
           U.Dpt$, U.d1%, U.d2%, TB$                 ! 
       If Ban.Prg%  = -1 Then Begin                  ! Si no existe init variables     
          U.Iva$ = Right$("000"+Str$(U.I%),3)        ! Numero de terminal
          U.Key$ = Pack$("1"+U.FECHA$+U.Iva$)        ! F+Fecha Movimiento+terminal

          Read Form "C5 2I4 C2" ; #Area1% Key U.Key$ ;   \! Si ya existe informacion
                     U.Dpt2$, U.d11%, U.d22%, TB$         !                  
          If Ban.Prg%  = -1 Then Begin
             Fini$ = U.PREFIJO$ + Right$("000000"+Str$(u.d11%),6) ! Factura Inicial
             Ffin$ = U.PREFIJO$ + Right$("000000"+Str$(u.d22%),6) ! Factura Final 
          EndIf Else Begin
             Fini$ = U.PREFIJO$ + "000001"
             Ffin$ = U.PREFIJO$ + "000002"
          EndIf 
          
          BUFFER$ = Right$("000"+Str$(U.I%),3)+String$(6," ") + Fini$ + "       " + Ffin$ +  \
                    String$(2," ")+Right$("          "+Str$(u.d1%+U.d2%),10)+"   "+ \ 
                    Right$("          "+Str$(u.d2%),10)+"         " + \
                    Right$("          "+Str$(u.d1%),10)

          Call DETALLE.REPORTE("INFORME VTAS X CAJA  ",0)
          U.T1% = U.T1% + U.D1% + U.d2%
          U.T2% = U.T2% + U.D2%
          U.T3% = U.T3% + (U.D1%)
       EndIf     
  Next U.I%
  Call Detalle.Linea( " " )
  Call Detalle.Linea( "TOTALES GENERALES"+String$(21," ") + Right$("          "+Str$(U.T1%),10) + \
         "   " + Right$("          "+Str$(U.T2%),10) + "         " + Right$("          "+Str$(U.T3%),10))
  Call Detalle.Linea( CHR$(15)+LINEA$	)	      ! Impresion de linea
  Call Detalle.Linea( CHR$(12) )   					  ! Salto de Pagina
End Sub 

Sub REPORTE.MEDIOS
Integer*4 U.d1%, U.d2%, U.I%, U.J%, U.T1%, U.T2%, U.T3%
String    U.Key$, U.Depto$, U.dpt$, U.Iva$, TB$ 
  Rastro$ = "reporte.medios"
  EMsg$ = "Generando Reporte Medios de Pago"
  If Ind.Bkg% = 0 Then \
     Call MSG.ERR(06,EMsg$) Else \
     Call Msg.Bkg

  Call Cabecera.Reporte 
  For U.I% = 1 to 999 
      U.Iva$ = Right$("000"+Str$(U.I%),3)         ! Tariva de descuento
      U.Key$ = PACK$("X"+U.FECHA$+U.Iva$)         ! X+Fecha Movimiento+tipo de pago
      BAN.PRG% = -1
      Read Form "C5 2I4 C2" ; #Area1% Key U.Key$ ; \! Si ya existe informacion
           U.Dpt$, U.d1%, U.d2%, TB$            ! 
       If Ban.Prg%  = -1 Then Begin               ! Si no existe init variables
          BUFFER$ = "    "+Left$(Pagos$(u.i%)+String$(20," "),20) + \
                    String$(6," ")+Right$("          "+Str$(u.d2%),10)+"          "+ \
                    "          "+"          "+Right$("          "+Str$(u.d1%),10)

          Call DETALLE.REPORTE("   MEDIOS DE PAGO    ",0)
          U.T1% = U.T1% + U.D1%
          U.T2% = U.T2% + U.D2%
          U.T3% = U.T3% + (U.D1%)
       EndIf 
  Next U.I%
  Call Detalle.Linea( " ")
  Call Detalle.Linea( "TOTALES GENERALES"+String$(23," ") + "          " + "          " + \
        "          " + Right$("          "+Str$(U.T3%),10))
  Call Detalle.Linea( CHR$(15)+LINEA$	)	      ! Impresion de linea
  Call Detalle.Linea( CHR$(12) )   					  ! Salto de Pagina
End Sub 

Sub Carga.deptos
String U.Dpt$, U.Des$
Dim DEPTOS$(999)
   OPEN "ADX_IDT1:DEPTOS.DAT" AS 91 NOWRITE NODEL                       ! Abre las tarifas de IVA
   IF END # 91 THEN  CONT.DPT                                ! Recorre el archivo
   LEER.DPT:
    READ # 91 ; U.Dpt$, U.Des$
    Deptos$(Val(U.dpt$)) = U.Des$
    GoTo LEER.DPT 
   CONT.DPT:
     CLOSE 91
End Sub 

Sub CARGA.IVA                                                ! Carga las tarifas de IVA
   Dim  UE.TABLAIVA(10)
End Sub 

Sub Carga.Dsctos
String U.Dpt$, U.Des$
Dim DSCTOS$(99)
Dim MISCEL$(999)
   Open "ADX_IDT1:DESCUENT.DAT" AS 91 NOWRITE NODEL                       ! Abre las tarifas de IVA
   IF END # 91 THEN  CONT.DPT                                ! Recorre el archivo
   LEER.DPT:
    READ # 91 ; U.Dpt$, U.Des$
    Dsctos$(Val(U.dpt$)) = U.Des$
    GoTo LEER.DPT 
   CONT.DPT:
     CLOSE 91
     
   Open "ADX_IDT1:MISCELAN.DAT" AS 91 NOWRITE NODEL                       ! Abre las tarifas de IVA
   IF END # 91 THEN  CONT.MIS                                ! Recorre el archivo
   LEER.MIS:
    READ # 91 ; U.Dpt$, U.Des$
    Miscel$(Val(U.dpt$)) = U.Des$
    GoTo LEER.MIS 
   CONT.MIS:
     CLOSE 91
     
     
End Sub 

Sub Carga.Pagos
String U.Dpt$, U.Des$
Dim PAGOS$(100)
   OPEN "ADX_IDT1:TIPOPAGO.DAT" AS 91 NOWRITE NODEL             ! Abre las tarifas de IVA
   IF END # 91 THEN  CONT.DPT                                   ! Recorre el archivo
   LEER.DPT:
    READ # 91 ; U.Dpt$, U.Des$
    PAGOS$(Val(U.dpt$)) = U.Des$
    GoTo LEER.DPT 
   CONT.DPT:
     CLOSE 91
End Sub 

Sub Carga.Descriptores
String WORK$, Xdat1$, Xdat2$, Xdat3$
   Dim Meses$(12)
   Meses$(01) = "Ene"
   Meses$(02) = "Feb"
   Meses$(03) = "Mzo"
   Meses$(04) = "Abr"
   Meses$(05) = "May"
   Meses$(06) = "Jun"
   Meses$(07) = "Jul"
   Meses$(08) = "Ago"
   Meses$(09) = "Sep"
   Meses$(10) = "Oct"
   Meses$(11) = "Nov"
   Meses$(12) = "Dic"
   BAN.PRG% = -1
   Open "ADX_IDT1:RPPARAM.DAT" AS 91 NOWRITE NODEL
   If BAN.PRG% = -1 Then Begin 
      Read # 91 ; Xdat1$, Xdat2$, Xdat3$
      Close 91
      DM.RAZON$ = Left$(Xdat1$+"                    ",20)
      DM.NIT$   = Left$(Xdat2$+"                    ",20)
      DM.SIGLA$ = Left$(Xdat3$+"                    ",20)
   EndIf Else Begin 
      DM.RAZON$ = "EMPRESA NO DEFINIDA "
      DM.NIT$   = "900.123.456-1       "
      DM.SIGLA$ = "SIGLA SIN DEFINIR   "
   EndIf 

End Sub 


!--------------
!-------------- Programa Principal
!--------------

  ON ERROR GOTO ERRRPFIS								! En caso de Error 
  IND.BKG% = 0 
  Entrada$ = Command$
  If Len(Entrada$) >= 6 Then Begin 
     Ind.Bkg% = -1  ! Una fecha valida formato AAMMDD
     Entrada$ = Right$(Entrada$,6)
  EndIf 

  Call LEER.CABECERA                    ! Datos de la tienda
  Call Datos.Tienda                     ! Numero de la tienda SAP
  Call CARGA.DEPTOS                     ! Carga departamentos
  Call CARGA.IVA                        ! Carga las tarifas de IVA
  Call CARGA.Dsctos                     ! Carga descuentos
  Call Carga.Pagos                      ! Carga Tipos de Pago
  Call Carga.Descriptores               ! Carga Descriptores Reporte Fiscal
  Call PANTALLA.PRINCIPAL 	            ! Captura de datos
  Call REPORTE.DEPARTAMENTOS	          ! Reporte Venta departamental
  Call REPORTE.MISCELANEOS  	          ! Reporte Miscelaneos
  Call REPORTE.DESCUENTOS               ! Descuentos otorgados
  Call REPORTE.IVAS	                    ! Detalles de impuestos
  Call REPORTE.TERMINALES               ! Reporte de terminales
  Call REPORTE.MEDIOS                   ! Medios de pagos
  Call Termine.Prog                     ! Final del programa
  
ERRRPFIS:																! Rutina de control de errores
  If ERRF% = 19 And (ERR = "EF") Then Begin 
     Ban.Prg% = 0
     Resume  
  EndIf 
  If ERRF% = 19 And (ERR = "OE" OR ERR = "FU") Then Begin 
     Ban.Prg% = 0
     Resume
  EndIf 

  If ERRF% = 91 And (ERR = "OE" OR ERR = "FU") Then Begin 
     Ban.Prg% = 0
     Resume
  EndIf 
  
  If ERRF% = 78 And (ERR = "OE" OR ERR = "FU") Then Begin
     Ban.Prg% = 0
     Resume  
  EndIf   
  
  If ERRF% = Area1% AND (ERR = "OE" OR ERR = "FU") THEN Begin
     Men$ = "Archivo Acumulador Fiscal EAMACFIS No Existe..."
     Print men$
     Wait ; 2000
     Create "ADX_UDT1:RP"+U.Fecha$+".ERR" AS 78
     Write #78;Men$
     Close 78
     Stop
  EndIf 
    If ERRF% = Area2% AND (ERR = "OE" OR ERR = "FU") THEN Begin
     Men$ = "Archivo Numeracion Fiscal EAMNMFIS No Existe..."
     Print men$
     Wait ; 2000
     Create "ADX_UDT1:RP"+U.Fecha$+".ERR" AS 78
     Write #78;Men$
     Close 78
     Stop
  EndIf 
  If ERRF% = Area1% AND ERR = "EF" THEN Begin
     Ban.Prg% = 0
     Resume
  EndIf 
  If ERRF% = Area2% AND ERR = "EF" THEN Begin
     Ban.Prg% = 0
     Resume
  EndIf 
  
  Call TRADUCE.ERROR										! Traduccion del Error
  EMsg$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$+" "+Rastro$ ! Msg Error
  If IND.BKG% = 0 Then \
     Call msg.err(6,EMsg$) Else \
     Locate 24,1 : Print EMsg$  ! Presenta Error
  Create "ADX_UDT1:RP"+U.Fecha$+".ERR" AS 78
  Write #78;EMsg$
  Close 78
  Stop 				! Stop del programa


