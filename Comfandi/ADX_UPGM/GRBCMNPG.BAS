!************************************************** 
!Empresa       : GRUPO RETAIL LTDA                *
!Programa      : GRBCMNPG.BAS                     *
!Autor         : OSCAR VALENCIA SARMIENTO         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Mantenimiento Parametros Bonos   *
!                recompra forma de pago           *
!**************************************************
! Mod 18May2017
! Se adiciona en archivo de parametros de bono recompra
! la definición del valor maximo de entrega del bono
! desarrollado Grupo Retail - OVS
!--------------------------------------------------
! Mod 17 MAY 2018
! Se ajusta modulo para funcionamiento con motor de 
! promociones, se ajusta para manejo de porcentaje
! de emisión para multiples formas de pago
! desarrollado Grupo Retail - OVS
!--------------------------------------------------
! Mod 27Nov2019
! Se modifica la creacion del bono en linea con la 
! siguiente estructura:
! 23 + id evento(8)+tipo bono(2)+fecha y hora(12)+ 
! fecha vencimiento(6) total 30 caracteres
! Solicitado por Comfandi para consumo de bonos en linea
! desarrollado Grupo Retail - OVS/CAC
!--------------------------------------------------
! 3/Oct/2022
! Se ajusta modulo para manejo de monto minimo de 
! generación de bono y control de lista de items 
! excluidos para calculo de bonos 
! Solicitado por Comfandi
! desarrollado Grupo Retail - OVS
!--------------------------------------------------


!--- Definicion de Variables de trabajo

STRING    Global U.SERIAL$, U.TIPO$, U.PREF$, U.AUT$, U.FEC$, U.TMP$, U.HOY$, U.RESOL$
String    Global U.OLD$, U.NEW$, U.CAJERO$, Gr.BonCmp.Opt$, U.Nbono$
INTEGER*4 Global U.INI%, U.FIN%, U.ACT% ,U.NOTA%, U.NC%
String    Global U.TipoBon$, U.Consec$, U.pago$, U.montomax$, U.fecveni$, U.fecvenf$, U.Tipou$, \
                 U.montomin$
String Global Gr.BonCmp.Fini$,                               \! Fecha inicial proyecto
              Gr.BonCmp.Ffin$,                               \! Fecha final del proyecto
              Gr.BonCmp.Tipo$(1),                            \! Tipificacion de cliente 
              Gr.BonCmp.Ptg$(1)                               ! Porcentaje descuento 

%INCLUDE FISPVARI.BAS				  	   ! Variables programa
%INCLUDE ADX_UPGM:DMEXTR.J86       ! Inclucion Libreria Display Manager
%INCLUDE FISPRUTI.BAS				  	   ! Rutinas Comunes

Function ADXERROR(TERM,GRP,NUM,SVRTY,EVENT,UNQ$) EXTERNAL
 INTEGER*2 TERM,NUM
 INTEGER*1 SVRTY,GRP,EVENT
 STRING    UNQ$
End Function

Function QUIT2     					       ! Funcion salida programa
    CLOSE AREA1%
    Call SETF("0000000")				   !
    Call CLRSCR						         !
    RET.ERR%= CLSDIS					     !
    Call DM.ERR(RET.ERR%,CLSDIS$)	 !
    STOP
END FUNCTION
!--- Fin de la funcion de salida

FUNCTION INICIO01
   Call.ORDER% = 51                                           ! Llamado Primera Pantalla D.M
   RET.ERR% = DISPD(Call.ORDER%)                              ! Llamado de la pantalla en DM
   Call DM.ERR(RET.ERR%,DISPD$)
END FUNCTION
!--- Fin de la funcion de inicio


SUB CAPTURA.DATOS
Integer*2 XI%

      MSG$="Fecha Inicial Generacion Bono" 		   															    !
      Call MSG.ERR(23,MSG$)				   																				! Mensaje 
      RET.ERR% = NXTF(-20) 			           																	! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)			   																		!
      ATTR$ = SETF(PRM.ON$)                                									!
      INP$ = UPDF                                          									! Captura dato en pantalla
      Gr.BonCmp.Fini$ = INP$
      IF (ENDF = F1.AYUDA) THEN BEGIN \
         Call HELP("Bonos Recompra","GRBCMNTO.TXT")   							  !
         Call INICIO01
      ENDIF
      IF (ENDF = F3.SALIR) THEN  EXIT SUB                   ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN  EXIT SUB                   ! Si presiona tecla F3
      While Val( Gr.BonCmp.Fini$ ) <= 0 
        RET.ERR% = NXTF(-20)
        Call DM.ERR(RET.ERR%,NXTF$)
        ATTR$ = SETF(PRM.ON$)                                
        INP$ = UPDF                                        									! Captura dato en pantalla
        Gr.BonCmp.Fini$ = INP$
        IF (ENDF = F1.AYUDA) THEN BEGIN 
           Call HELP("Bonos Recompra","GRBCMNTO.TXT")   							  !
           Call INICIO01
        ENDIF
        IF (ENDF = F3.SALIR) THEN  EXIT SUB                 ! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN  EXIT SUB                      ! Si presiona tecla F3
      Wend
     
      Call MSG.ERR(23,"Fecha Final Generacion Bono")
      Gr.BonCmp.Ffin$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT Sub                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT Sub                      ! Si presiona tecla ESC
      WHILE Val(Gr.BonCmp.Ffin$) <= 0
         Call MSG.ERR(23,"Fecha Final Generacion Bono")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         Gr.BonCmp.Ffin$ = GET.DATOS                          ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT Sub                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT Sub                   ! Si presiona tecla ESC
      WEND

      For XI% = 1 To 5 
        Call MSG.ERR(23,"Tipo y Variedad Pago "+Str$(Xi%))
        Gr.BonCmp.Tipo$(XI%) = GET.DATOS
        IF (ENDF = F3.SALIR) THEN EXIT Sub                      ! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN EXIT Sub                      ! Si presiona tecla ESC
        While Gr.BonCmp.Tipo$(XI%) = "    "
         Call MSG.ERR(23,"Tipo y Variedad Pago "+Str$(Xi%))
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         Gr.BonCmp.Tipo$(XI%) = GET.DATOS                     ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT Sub                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT Sub                   ! Si presiona tecla ESC
        Wend
        Gr.BonCmp.Tipo$(XI%) = Ucase$(Gr.BonCmp.Tipo$(XI%))   ! Almacena Mayusculas
        
        Call MSG.ERR(23,"Porcentaje Beneficio "+Str$(Xi%))
        Gr.BonCmp.Ptg$(XI%) = GET.DATOS
        IF (ENDF = F3.SALIR) THEN EXIT Sub                      ! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN EXIT Sub                      ! Si presiona tecla ESC
        While Val(Gr.BonCmp.Ptg$(XI%)) < 0
         Call MSG.ERR(23,"Porcentaje Beneficio "+Str$(Xi%))
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         Gr.BonCmp.Ptg$(XI%) = GET.DATOS                      ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT Sub                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT Sub                   ! Si presiona tecla ESC
        Wend
         
      Next XI%

      Call MSG.ERR(23,"Tipo de Validacion en caja ")
      Gr.BonCmp.Opt$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT Sub                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT Sub                      ! Si presiona tecla ESC
      WHILE Match(Gr.BonCmp.Opt$,"123",1) <= 0
         Call MSG.ERR(23,"Tipo de Validacion en caja ")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         Gr.BonCmp.Opt$ = GET.DATOS                           ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT Sub                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT Sub                   ! Si presiona tecla ESC
      WEND

      Call MSG.ERR(23,"Numero de bonos a generar por evento")
      U.Nbono$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT Sub                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT Sub                      ! Si presiona tecla ESC
      While Val(U.Nbono$) < 0
         Call MSG.ERR(23,"Numero de bonos a generar por evento")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.Nbono$ = GET.DATOS																	! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT Sub                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT Sub                   ! Si presiona tecla ESC
      Wend

      Call MSG.ERR(23,"Tipo de Bono a definir valor fijo 02 ")
      U.Tipobon$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT Sub                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT Sub                      ! Si presiona tecla ESC
      While U.Tipobon$ <> "02"
         Call MSG.ERR(23,"Tipo de Bono a definir valor fijo 02")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.Tipobon$ = GET.DATOS																! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT Sub                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT Sub                   ! Si presiona tecla ESC
      Wend

      Call MSG.ERR(23,"Consecutivo Bono a definir ")
      U.Consec$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT Sub                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT Sub                      ! Si presiona tecla ESC
      While Val(U.Consec$) < 0
         Call MSG.ERR(23,"Consecutivo Bono a definir ")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.Consec$ = GET.DATOS																! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT Sub                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT Sub                   ! Si presiona tecla ESC
      Wend

      Call MSG.ERR(23,"Lista Tipificaciones a validar ")
      U.Pago$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT Sub                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT Sub                      ! Si presiona tecla ESC
      While U.Pago$ = " "
         Call MSG.ERR(23,"Lista Tipificaciones a validar ")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.Pago$ = GET.DATOS																! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT Sub                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT Sub                   ! Si presiona tecla ESC
      Wend

      Call MSG.ERR(23,"Valor minimo bono a entregar")
      U.montomin$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT Sub                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT Sub                      ! Si presiona tecla ESC
      While Val(U.montomin$) < 0
         Call MSG.ERR(23,"Valor minimo bono a entregar")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.montomin$ = GET.DATOS																! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT Sub                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT Sub                   ! Si presiona tecla ESC
      Wend

      Call MSG.ERR(23,"Valor maximo bono a entregar")
      U.montomax$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT Sub                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT Sub                      ! Si presiona tecla ESC
      While Val(U.montomax$) < 0
         Call MSG.ERR(23,"Valor maximo bono a entregar")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.montomax$ = GET.DATOS																! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT Sub                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT Sub                   ! Si presiona tecla ESC
      Wend

      Call MSG.ERR(23,"Fecha vigencia inicial bono AAMMDD")
      U.fecveni$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT Sub                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT Sub                      ! Si presiona tecla ESC
      While Val(U.fecveni$) <= 0
         Call MSG.ERR(23,"Fecha vigencia inicial bono AAMMDD")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.fecveni$ = GET.DATOS																! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT Sub                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT Sub                   ! Si presiona tecla ESC
      Wend

      Call MSG.ERR(23,"Fecha vigencia final bono AAMMDD")
      U.fecvenf$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT Sub                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT Sub                      ! Si presiona tecla ESC
      While Val(U.fecvenf$) <= 0
         Call MSG.ERR(23,"Fecha vigencia final bono AAMMDD")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.fecvenf$ = GET.DATOS																! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT Sub                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT Sub                   ! Si presiona tecla ESC
      Wend
      U.montomin$ = Pack$(Right$("00000000"+U.montomin$,8))
      Gr.BonCmp.Fini$ = Pack$(Right$("000000"+Gr.BonCmp.Fini$,6)) ! Fecha inicial proyecto
      Gr.BonCmp.Ffin$ = Pack$(Right$("000000"+Gr.BonCmp.Ffin$,6)) ! Fecha final   proyecto
      U.Nbono$ = Right$("000"+U.Nbono$,3)                         ! Ajusta  3 digitos 
      U.TipoBon$ = Right$("00"+U.TipoBon$,2)                      ! Ajusta  2 digitos 
      U.Consec$  = Right$("000000"+U.Consec$,6)                   ! Ajusta  6 digitos 
      U.Pago$ = Left$(U.Pago$+"0000000000",10)                    ! Ajusta 10 digitos 
      U.montomax$ = Pack$(Right$("00000000"+U.montomax$,8))       ! ajusta PD 4
      U.fecveni$ = Pack$(Right$("000000"+U.fecveni$,6))           ! ajusta PD 3
      U.fecvenf$ = Pack$(Right$("000000"+U.fecvenf$,6))           ! ajusta PD 3
      
      Write Form "C4 2C3 5C4 5C2 C1 C3 C10 C2 C6 C4 2C3 C1 C4";#Area1% ;         \! Almacena registro
       "7778",                                        \! Llave del proyecto
       Gr.BonCmp.Fini$,                               \! Fecha inicial proyecto
       Gr.BonCmp.Ffin$,                               \! Fecha final del proyecto
       Gr.BonCmp.Tipo$(1),                            \! Tipo y variedad de pago 1
       Gr.BonCmp.Tipo$(2),                            \! Tipo y variedad de pago 2
       Gr.BonCmp.Tipo$(3),                            \! Tipo y variedad de pago 3
       Gr.BonCmp.Tipo$(4),                            \! Tipo y variedad de pago 4
       Gr.BonCmp.Tipo$(5),                            \! Tipo y variedad de pago 5
       Gr.BonCmp.Ptg$(1),                             \! Porcentaje descuento 1
       Gr.BonCmp.Ptg$(2),                             \! Porcentaje descuento 2
       Gr.BonCmp.Ptg$(3),                             \! Porcentaje descuento 3
       Gr.BonCmp.Ptg$(4),                             \! Porcentaje descuento 4
       Gr.BonCmp.Ptg$(5),                             \! Porcentaje descuento 5
       Gr.BonCmp.Opt$,                                \! Tipo de validacion de compra
       U.Nbono$,                                      \! Numero de bonos a generar
       U.Pago$,                                       \! Lista de tipificaciones a validar
       U.TipoBon$,                                    \! Tipo de Bono
       U.Consec$,                                     \! Consecutivo bono
       U.montomax$,                                   \! Valor maximo bono
       U.Fecveni$,                                    \! Fecha de vigencia inicial
       U.fecvenf$,                                    \! Fecha de vigencia final
       U.Tipou$,                                      \! Forma aplicacion bono 0.proxima compra 1.dentro de la compra
       U.montomin$                                     ! Valor minimo de expedicion 
       
End Sub

Sub VALIDA.DATOS
String X.Tmp$
  Call INICIO01					   														 !
  Dim Gr.BonCmp.Tipo$(6)                               ! Tipificacion de cliente 
  Dim Gr.BonCmp.Ptg$(6)                                ! Porcentaje descuento 
  BAN.PRG$ = "0"				                               ! archivo            
  Read Form "C4 2C3 5C4 5C2 C1 C3 C10 C2 C6 C4 2C3 C1 C4";#Area1% KEY "7778"; \!
       X.Tmp$,                                        \! Llave del proyecto
       Gr.BonCmp.Fini$,                               \! Fecha inicial proyecto
       Gr.BonCmp.Ffin$,                               \! Fecha final del proyecto
       Gr.BonCmp.Tipo$(1),                            \! Tipificacion de cliente 1
       Gr.BonCmp.Tipo$(2),                            \! Tipificacion de cliente 2
       Gr.BonCmp.Tipo$(3),                            \! Tipificacion de cliente 3
       Gr.BonCmp.Tipo$(4),                            \! Tipificacion de cliente 4
       Gr.BonCmp.Tipo$(5),                            \! Tipificacion de cliente 5
       Gr.BonCmp.Ptg$(1),                             \! Porcentaje descuento 1
       Gr.BonCmp.Ptg$(2),                             \! Porcentaje descuento 2
       Gr.BonCmp.Ptg$(3),                             \! Porcentaje descuento 3
       Gr.BonCmp.Ptg$(4),                             \! Porcentaje descuento 4
       Gr.BonCmp.Ptg$(5),                             \! Porcentaje descuento 5
       Gr.BonCmp.Opt$,                                \! Tipo de validacion compra
       U.Nbono$,                                      \! Numero de bonos a generar
       U.Pago$,                                       \! Tipo y variedad de pago validar
       U.TipoBon$,                                    \! Tipo de Bono
       U.Consec$,                                     \! Consecutivo bono
       U.montomax$,																	  \! Valor maximo bono
       U.Fecveni$,                                    \! Fecha de vigencia inicial
       U.fecvenf$,                                    \! Fecha de vigencia final
       U.Tipou$,                                      \! Forma aplicacion bono 0.proxima compra 1.dentro de la compra
       Gr.BonCmp.Ptg$(6)                               ! valor minimo expedicion

   If BAN.PRG$ = "0" Then BEGIN 											 ! Proyecto Definido
   	  Call Msg.Err( 2,UnPack$(Gr.BonCmp.Fini$))
   	  Call Msg.Err( 3,UnPack$(Gr.BonCmp.Ffin$))
   	  Call Msg.Err( 4,Gr.BonCmp.Tipo$(1))
   	  Call Msg.Err( 5,Gr.BonCmp.Ptg$(1))
   	  Call Msg.Err( 6,Gr.BonCmp.Tipo$(2))
   	  Call Msg.Err( 7,Gr.BonCmp.Ptg$(2))
   	  Call Msg.Err( 8,Gr.BonCmp.Tipo$(3))
   	  Call Msg.Err( 9,Gr.BonCmp.Ptg$(3))
   	  Call Msg.Err(10,Gr.BonCmp.Tipo$(4))
   	  Call Msg.Err(11,Gr.BonCmp.Ptg$(4))
   	  Call Msg.Err(12,Gr.BonCmp.Tipo$(5))
   	  Call Msg.Err(13,Gr.BonCmp.Ptg$(5))
   	  Call Msg.Err(14,Gr.BonCmp.Opt$)
   	  Call Msg.Err(15,U.Nbono$)
      Call Msg.Err(16,U.Tipobon$)
      Call Msg.Err(17,U.Consec$)
      Call Msg.Err(18,U.Pago$)
      Call Msg.Err(19,Unpack$(Gr.BonCmp.Ptg$(6)))
      Call Msg.Err(20,Unpack$(U.montomax$))
      Call Msg.Err(21,Unpack$(U.fecveni$))
      Call Msg.Err(22,Unpack$(U.fecvenf$))
   EndIf
     
End SUB


!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0101
Call INICIADM 				                   																		! Inicializacion Variables Display Manager
AREA1% = 1
Open "TF:PBONPG" KEYED RECL 77 AS AREA1%                                    ! Parametros para bono recompra
TERM$ = " "					                     																		! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					       																			!
Call DM.ERR(RET.ERR%,INITDM$)				   																			!
RET.ERR% = OPNDIS("ADX_UPGM:GRBCPNTA.PNT")	           																! Apertura de la forma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
Call VALIDA.DATOS
Call CAPTURA.DATOS
Call QUIT2
Stop 


!----
!---- Fin del bloque principal
!----

E0101:
         IF ERRF% = AREA1% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            Create Posfile "TF:PBONPG" KEYED 4,,,50 RECL 77 AS AREA1% COMPOUND PERUPDATE ! Creacion control proyecto
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa
       
         IF ERRF% = AREA1% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         EndIf
         
         IF ERRF% = 19 AND ERR = "EF" OR ERR="OE" THEN BEGIN  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         ENDIF						       !
         Call TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla
!--- Fin despliegue de errores

!--- Fin del programa NUMFIS.BAS
