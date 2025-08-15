!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LDITBONO.BAS                     *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Alimentacion masiva Articulos    *
!                para bonos recompra              *
!**************************************************
! Modificaciones:
! Mod 11Jun2014
! Se adiciona proceso de validacion para generación
! de cupones en línea, desarrollado por Grupo Retail
! OVS-CAC 
!--------------------------------------------------
! Mod 14Abr2015
! Se adiciona rastro de auditoria para control de las
! aplicaciones de movimiento de bonos recompra.
! desarrollado por Grupo Retail - OVS 
!--------------------------------------------------
! Mod 19Ene2016
! Se modifica modulo para manejo en conjunto con 
! modulo bonos recompra validando forma de pago
! desarrollado por Grupo Retail - OVS 
!--------------------------------------------------
! Mod 18 Mayo 2017
! Se adiciona campo para manejo de valor maximo de 
! entrega de bono.
! desarrollado por Grupo Retail - OVS 
!--------------------------------------------------
! Mod 11 Jun 2018
! Se modifica para busqueda del archivo entrando
! por parámetro para su administración en el motor
! de promociones.
! desarrollado por Grupo Retail - OVS 
!--------------------------------------------------
! Mod 30Jul2019
! Se adiciona validación registro cabecera si no esta
! presente, no genera carga del archivo.
! desarrollado por Grupo Retail - OVS 
!--------------------------------------------------
! Mod 27Nov2019
! Se modifica la creacion del bono en linea con la 
! siguiente estructura:
! 23 + id evento(8)+tipo bono(2)+fecha y hora(12)+ 
! fecha vencimiento(6) total 30 caracteres
! Solicitado por Comfandi para consumo de bonos en linea
! desarrollado Grupo Retail - OVS/CAC
!--------------------------------------------------
! Mod 28Nov2023
! Version especial para carga bonos en proceso de 
! despliegue carga con icui
! desarrollado Grupo Retail - OVS
!--------------------------------------------------


%Environ C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$, Ue.Data2$, FileOut$, Param2$, \
                  XParam$, Ifile$
Integer*1  Global U.Error%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty%

Sub MSG.LOG
    Locate 23,1 : Print String$(78," ") 
    Locate 23,1 : Print "Msg: "+UE.MSG$
End Sub 

Function TRADUCE.ERROR                                       !
Integer*4 HX%, S%, SX%, SUM%
String    Z$
    HX% = ERRN                                               ! Traduccion de los
    ERRFX$=""                                                ! codigos de error
    FOR S% = 28 TO 0 STEP -4                                 ! del sistema operativo
        SX% = SHIFT(HX%,S%)                                  !
        SUM% = SX% AND 000FH                                 !
        IF SUM% > 9 THEN SUM% = SUM% + 55                   \!
        ELSE SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                      !
        ERRFX$ = ERRFX$ + Z$                                 !
    NEXT S%                                                  !
END Function


Sub Apertura.Archivos
    !Open "ADX_UDT1:LDITBONO.TXT"   AS 1								       ! Plano maestro de carga
    Ifile$ = "PROMODAT:"+Ifile$
    Open Ifile$ As 1																												! Carga archivos parámetros bono recompra

!   Create POSFILE "TF:COMPRA" KEYED 6,,,50000 RECL  6 AS 4 COMPOUND PERUPDATE  ! Creacion Articulos bonos recompra
!   Create Posfile "TF:PBONOS" KEYED 4,,,50    RECL 56 AS 3 COMPOUND PERUPDATE ! Creacion control proyecto
!   Create Posfile "TF:BLOCAL" KEYED 9,,,50000 RECL 17 AS 5 COMPOUND PERUPDATE ! Creacion contingencia

    Create "ADX_UDT1:ERRBONOS.TXT" As 2                      ! Errores proceso de carga
End Sub

Sub Carga.Masiva.Datos
String Rbuffer$, XReg$, XFini$, XFfin$, Xtip1$, Xtip2$, Xptg1$, Xptg2$, Xsku$, xvlr$, xfven$
String Xtip3$, Xtip4$, Xtip5$, Xptg3$, Xptg4$, Xptg5$, Xcla$, Xind$, Xnum$, Xtip$, Xcon$
String Xfinib$, Xfvenb$, Xtcomp$, xtipo$ 
Integer*2 Xloc%
  U.Qty% = 0
  While(1)

      Read #1; Line XReg$
      U.Qty% = U.Qty% + 1
      Locate 12,27 : Print Str$(U.Qty%)
      U.Error% = -1 
      If U.Qty% = 1 Then Begin           ! Registro de parametros
         If Len(Xreg$) = 82 Then Begin																			! Si registro cabecera OK
            Create POSFILE "TF:COMPRA" KEYED 6,,,50000 RECL  6 AS 4 COMPOUND PERUPDATE  ! Creacion Articulos bonos recompra
            Create Posfile "TF:PBONOS" KEYED 4,,,50    RECL 63 AS 3 COMPOUND PERUPDATE ! Creacion control proyecto
            Create Posfile "TF:BLOCAL" KEYED 9,,,50000 RECL 17 AS 5 COMPOUND PERUPDATE ! Creacion contingencia
         EndIf Else Begin
      	 	  Write #2; "Linea :"+Str$(U.Qty%),Xreg$,"FALLA REGISTRO CABECERA",FINR$
            UE.MSG$ = "FALLA REGISTRO CABECERA"
            Call MSG.LOG
      	 	  Close 2
      	 	  Stop 
         EndIf
         XFini$ = Mid$(Xreg$, 1,6)       ! Fecha inicial del evento 
         XFfin$ = Mid$(Xreg$, 7,6)       ! Fecha final   del evento 
         Xtip1$ = Mid$(Xreg$,13,4)       ! Tipificacion 1
         Xtip2$ = Mid$(Xreg$,17,4)       ! Tipificacion 2
         Xtip3$ = Mid$(Xreg$,21,4)       ! Tipificacion 3
         Xtip4$ = Mid$(Xreg$,25,4)       ! Tipificacion 4
         Xtip5$ = Mid$(Xreg$,29,4)       ! Tipificacion 5
         Xptg1$ = Mid$(Xreg$,33,2)       ! Descuento 1
         Xptg2$ = Mid$(Xreg$,35,2)       ! Descuento 2
         Xptg3$ = Mid$(Xreg$,37,2)       ! Descuento 3
         Xptg4$ = Mid$(Xreg$,39,2)       ! Descuento 4
         Xptg5$ = Mid$(Xreg$,41,2)       ! Descuento 5
         Xcla$  = Mid$(Xreg$,43,1)       ! Tipo operacion aplicar 1.Valida Item 2. Valida estruc comercial 3. toda la tienda
         Xnum$  = Mid$(Xreg$,44,3)       ! Numero de bonos a generar por evento
         Xtip$  = Mid$(Xreg$,47,2)       ! Tipo de bono 
         Xcon$  = Mid$(Xreg$,49,6)       ! ID consecutivo del bono
         Xvlr$  = Mid$(Xreg$,55,8)       ! Valor maximo del bono
         Xfinib$ = Mid$(Xreg$,63,6)      ! Fecha inicial cobro bono
         Xfvenb$ = Mid$(Xreg$,69,6)      ! Fecha final   cobro bono 
         Xtcomp$ = Mid$(Xreg$,75,1)      ! Forma aplicacion bono 0.proxima compra 1.dentro de la compra

         Xvlr$  = Pack$(Right$("00000000"+Xvlr$,8))
         xfvenb$ = Right$("000000"+Xfvenb$,6)
         xfinib$ = Right$("000000"+Xfinib$,6)
         
         XParam$ = Xreg$								 ! Rastro de auditoria
         
         Write Form "C4 2C3 5C4 5C2 C1 C3 C2 C6 C4 2C3 C1" ; #3;  \! Graba el registro
               "9998",                      \! Llave del registro     Asc 4
               Pack$(Xfini$),               \! Fecha inicial          PD  3
               Pack$(Xffin$),               \! Fecha Final            PD  3
               Xtip1$,                      \! Tipificacion 1         Asc 4
               Xtip2$,                      \! Tipificacion 2         Asc 4
               Xtip3$,                      \! Tipificacion 3         Asc 4
               Xtip4$,                      \! Tipificacion 4         Asc 4
               Xtip5$,                      \! Tipificacion 5         Asc 4
               Xptg1$,                      \! Porcentaje   1         Asc 2
               Xptg2$,                      \! Porcentaje   2         Asc 2
               Xptg3$,                      \! Porcentaje   3         Asc 2
               Xptg4$,                      \! Porcentaje   4         Asc 2
               Xptg5$,                      \! Porcentaje   5         Asc 2
               Xcla$,                       \! Tipo validacion        Asc 1
               Xnum$,                       \! Numero bonos x evento  Asc 3
               Xtip$,                       \! Tipo de bono           Asc 2
               Xcon$,                       \! ID de promocion        Asc 6
               Xvlr$,                       \! Monto maximo bono      PD  4
               Pack$(Xfinib$),              \! Fecha inicio vencimiento bomo PD  3
               Pack$(Xfvenb$),              \! Fecha final  vencimiento bomo PD  3
               Xtcomp$                       ! Forma aplicacion Bono
      EndIf 
      
      If U.Qty% >=2 And U.Qty% <= 4 Then Begin   ! Mensajes a imprimir
      	 Xreg$ = Left$(Xreg$+String$(37," "),37) ! Ajusta dato
      	 If U.Qty% = 2 Then Xind$ = "9995"
      	 If U.Qty% = 3 Then Xind$ = "9996"
      	 If U.Qty% = 4 Then Xind$ = "9997"	
         Write Form "C4 C37 C22" ; #3;   \! Graba el registro
               Xind$ ,                      \! Llave del registro     Asc 4
               Xreg$,                       \! Mensaje Impreso        Asc 37
               "0000000000000000000000"      ! Filler registro        Asc 22
               
      EndIf


      If U.Qty% > 4 Then Begin           ! Lista departamentos o items a validar
      	 Xloc% = Match(",",Xreg$,1)
      	 Xsku$ = Left$(Xreg$,(Xloc%-1))
      	 Xtipo$ = Mid$(Xreg$,(Xloc%+1),1)
      	 Xtipo$ = Right$("00"+Xtipo$,2)
      	 XSku$ = Pack$(Right$("000000000000"+Xsku$,12))
         Write Form "C6" ; #4;          \! Graba el registro
          XSku$                          ! Articulo controlado        UPD 6
      EndIf
       
       If U.Error% = -1 Then Begin
         U.QtyOk% = U.QtyOk% + 1 
         Locate 13,27 : Print Str$(U.QtyOk%)
       EndIf Else Begin
         U.QtyNok% = U.QtyNok% + 1
         Locate 14,27 : Print Str$(U.QtyNok%)
         Write #2; "Linea :"+Str$(U.Qty%),Xreg$,UE.MSG$,FINR$
       EndIf    
  Wend
End Sub


Sub Auditoria.Bonos
Integer*4 X.Len%
String    X.Lec$, X.data$
    FileOut$ = "ADX_UDT1:BR"+DATE$+".TXT"														! Auditoria bonos recompra
    Open FileOut$ As 51 Append																			! Acceso al archivo
    X.data$ = "BONO RECOMPRA APLICADO : 20"+Left$(DATE$,2)+"/"+    \! Reporta la fecha y 
              Mid$(DATE$,3,2)+"/"+Right$(DATE$,2) + " - " +        \! hora de aplicacion
              Left$(TIME$,2)+":"+Mid$(TIME$,3,2)+":"+Right$(TIME$,2)! del evento

    X.Len% = Len(X.data$)						  					  								  ! Toma longitud del registro
    X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
    Write form X.Lec$; #51 ; X.data$, Finr$            							! Graba registro


    X.Len% = Len(Xparam$)						  					  								  ! Toma longitud del registro
    X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
    Write form X.Lec$; #51 ; Xparam$, Finr$            							! Graba registro
    
    Xparam$ = String$(77,"-")
    X.Len% = Len(Xparam$)						  					  								  ! Toma longitud del registro
    X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
    Write form X.Lec$; #51 ; Xparam$, Finr$            							! Graba registro
    
    
    Close 51
End Sub 

Sub Fin.Proceso
    UE.MSG$ = "Fin Carga Items Recompra Verifique Errores en ADX_UDT1:ERRBONOS.TXT"
    Call Msg.Log
    Call Auditoria.Bonos
    Close 4 : Close 1 : Close 2
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "**** CARGA MASIVA PRODUCTOS RECOMPRA TEMPORAL**"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Noviembre 28 2023 GR.**"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : PROMODAT:"+Ifile$
   Locate 9, 1: Print "Actualizando Archivo   : TF:COMPRA   "
   Locate 10,1: Print "Actualizando Parametros: TF:PBONOS   "
   Locate 11,1: Print "Creando Contingencia   : TF:BLOCAL   "
   Locate 12,1: Print "Registros Procesados   : "
   Locate 13,1: Print "Registros Actualizados : " 
   Locate 14,1: Print "Registros No Procesados: " 
   Locate 23,1: Print "Msg : "
   FINR$ = CHR$(13) + CHR$(10)   ! Marca de fin de registro
End Sub


!---
!------- Bloque Principal
!---

On Error GoTo Errores.Aplicativo																					! Control de proceso de errores
Ifile$ = command$
If Ucase$(Ifile$) = "VERSION" Then Begin
Print "Grupo Retail Ltda"
Print "CARGA MASIVA PRODUCTOS RECOMPRA Ver 1.0"
Print "Ultima Revision Software Noviembre 28 2023"
Stop 
EndIf
Call Pantalla.Principal																									  ! Pantalla Principal
Call Apertura.Archivos																										! Apertura de archivos
Call Carga.Masiva.Datos																										! Carga Masiva de Informacion
Call Fin.Proceso
Stop 


Errores.Aplicativo:
  U.Error% = 0
  CALL TRADUCE.ERROR
  UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  
  If ERR = "IH" Then Resume 
  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
  EndIf 

  If ERR = "EF" Then Resume 

  If ERRF% = 51 AND (ERR = "OE" OR ERR = "FU") Then Begin
    Create FileOut$ As 51
    Resume
  EndIf 
  
  If ERRF% = 1 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO ADX_UDT1:LDITMPFZ.TXT NO EXISTE... "
     Call Msg.Log
     Stop
  EndIf 

  If ERRF% = 4 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
  If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "FALLA EN CREACION ARCHIVO ITEM BONOS.."
     CALL MSG.LOG
  EndIf 

  If ERRF% = 3 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "FALLA EN CREACION ARCHIVO PARAMETROS BONOS.."
     CALL MSG.LOG
  EndIf 

  CALL MSG.LOG
  Stop 
