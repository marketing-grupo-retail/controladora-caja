!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : MRKTLOGO.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Control de logos para impresoras *
!                SureMark 4610                    *
!**************************************************

%ENVIRON T					                ! Ambiente de terminal
Integer*4 GLOBAL UE.LOGO.MAXMEN%
String    GLOBAL Ue.Loading.Logo

%INCLUDE EAMP4VAJ.J86        				!4610 Printer
%INCLUDE EAMTRANS.J86        				!Transaction summary log
%INCLUDE EAMTSWKG.J86        				!Working Storage
%INCLUDE EAMP4VGJ.J86	       				!Variables rutinas 4610
%INCLUDE RECATSSU.011          					      															! RUTINAS GENERICAS APLICACION     
  
Function GETN4(P1$,P2) EXTERNAL
 Integer*4 GETN4
 String P1$
 Integer*2 P2
End Function

Sub Carga.Logos(FILE.LOGO$,POS.M.LOGO%) Public								          		! Carga logos Publicitarios 
String File.Logo$
Integer*2 POS.M.LOGO%
Integer*4 LogoChkSum,LOGO1i2

String    Logo$,        \ 
          LogoCntrl$,   \ strings para lectura de logos
          LogoFormat$,  \
          LogoChkSum$,  \
          MCTChkSum$,   \
          PrinterStatus$

Integer*4 LOGO.SIZE,     \
          LogoWidth,     \
          LogoLength,Logo1I4,Logo2I4


If (PRT4610.ENABLE) Then Begin             									            		! Solamente en 4610
 If POS.M.LOGO% < 1 OR POS.M.LOGO% > 40 Then Begin					  			    		! Fuera posicion de los logos
		  Exit Sub                                                              ! Sale de la funcion 
 EndIf                                                                      !
 
 If TS.IN.IPL Then Begin 													    											! Inicializacion de logos solamente en IPL
     TS.ER.RETURN = -1                                                    	! Ctrl de errores
     File.Logo$ = "R::ADX_UPGM:"+File.Logo$																	! Arma archivo de entrada
     Open File.Logo$ AS 94 NOWrite NODEL								    								! Apertura del archivo de logos
     If TS.ER.RETURN <> -1 Then Begin 																			! Si encuentra Error
     	    Call VISOR.AND.BORRAR("FALLA CARGA LOGO QR")
	        Exit Sub                                                          ! Fin de la funcion
     EndIf
     LOGO.SIZE = SIZE(File.Logo$) 																					! Toma el tamaño del archivo
     Close 94																																! Cierra el archivo
	   UE.LOGO.MAXMEN% = UE.LOGO.MAXMEN% + LOGO.SIZE													!
	   If UE.LOGO.MAXMEN% > 64000 Then Begin																	!
        Call Visores4690(1,"LOGO MUY GRANDE",File.Logo$,1500,"L")
	      Exit Sub 
	   EndIf
     If (LOGO.SIZE > 31) Then Begin																					! Lectura archivo en 2 strings
          OPEN File.Logo$ RECL LOGO.SIZE AS 94 BUFFSIZE LOGO.SIZE noWrite nodel ! Apertura del archivo
          LogoFormat$ = "C31C" + STR$(LOGO.SIZE-31)													!
          Read Form LogoFormat$; #94; LogoCntrl$,Logo$  										!
          Close 94                                           								! Listo con archivo de logo
          If (MID$(LogoCntrl$,16,1) = CHR$(0CCH)) Then Begin 				    		! chksum saved
              LogoChkSum$ = MID$(LogoCntrl$,18,1) +            				     \! parse chksum
              MID$(LogoCntrl$,17,1)																					!
          EndIf Else Begin                                   								! No Salvado
              LOGO.SIZE = LEN(Logo$)																				! Calcula checksum para logo en archivo
              LogoChkSum = 0																								!
              FOR LOGO1I2 = 1 TO LOGO.SIZE																	!
                LogoChkSum = LogoChkSum + ASC(MID$(Logo$,LOGO1I2,1))				!
              Next LOGO1I2																									!
          EndIf                                             								!
          Read #34; LINE PrinterStatus$ 																		! Read the checksum from the MCT
		      TClose 34																													!
          MCTChkSum$ = MID$(PrinterStatus$,35,2)														!
          If MCTChkSum$ <> LogoChkSum$ Then Begin 													! Checksums dIffer need to download logo
             LogoWidth = GETN4(LogoCntrl$,23)        												! Parse LogoWidth and LogoLength
             LOGO1I4 = LogoWidth / 8																				!
             If MOD(LogoWidth,8) <> 0 Then LOGO1I4 = LOGO1I4 + 1						!
             LogoLength = GETN4(LogoCntrl$,27)															!
             If MOD(LogoLength,8) <> 0 Then LOGO2I4 = LOGO2I4 + 1						!
             LOGO2I4 = LogoLength / 8 + MOD(LogoLength,8)										!
             WHILE LEN(Logo$) < ((LOGO1I4*8) * LOGO2I4)   									! Pad logo with low values as logo must be be squared off
               Logo$ = Logo$ + CHR$(00H)																		!
             Wend																														!
             LOGO.SIZE = LEN(Logo$)																					!
             Write #34;CHR$(1DH) +CHR$(2AH) +CHR$(POS.M.LOGO%) +           \!
                       CHR$(LOGO1I4) + CHR$(LOGO2I4)                        ! Grabacion del logo en la posicion deseada
             FOR LOGO1I2 = 1 TO LOGO.SIZE STEP 200													!
               If (LOGO.SIZE - LOGO1I2) < 200 Then Begin										!
                  Write #34; CHR$(1BH) + CHR$(2EH) +                       \!
                             MID$(Logo$,LOGO1I2,(LOGO.SIZE-LOGO1I2)+1)			!
               EndIf Else Begin																							!
                  Write #34; CHR$(1BH) + CHR$(2EH) + MID$(Logo$,LOGO1I2,200)!
               EndIf																												!
             Next LOGO1I2																										!
          EndIf               																							! Inicializacion de variables
          Logo$ = ""        																								! 
          LogoCntrl$ = ""   																								!
          LogoFormat$ = ""																									!
          LogoChkSum$ = ""																									!
          MCTChkSum$ = ""																										!
          PrinterStatus$ = ""																								!
	        Ue.Loading.Logo = CHR$(1BH) + CHR$(61H) + CHR$(01H) +            \! Retorna Logo Centrado
                   CHR$(1DH) + CHR$(2FH) + CHR$(01H) + CHR$(POS.M.LOGO%)		!
	      Exit Sub 																														!
       EndIf                 																								! End of logo file too small
 EndIf Else Begin 															    												! No esta en IPL
       Write #34; CHR$(1BH) + CHR$(22H) + CHR$(POS.M.LOGO%)					    		! Request the checksum of downloaded graphics
       Read #34; LINE PrinterStatus$     																		! Read the checksum from the MCT
       MCTChkSum$ = MID$(PrinterStatus$,35,2)																!
       If ( MCTChkSum$ <> (CHR$(00h) + CHR$(00h)) ) Then Begin							! If Checksum for Down Loaded Graphics is not a No-LOGO Checksum, AppEnd LOGO
           Ue.Loading.Logo = CHR$(1BH) + CHR$(61H) + CHR$(01H) +           \! Center
                         CHR$(1DH) + CHR$(2FH) + CHR$(00H) + CHR$(POS.M.LOGO%)
       EndIf Else Ue.Loading.Logo = ""										                  ! If LOGO Checksum does not equal No-LOGO Checksum
       MCTChkSum$ = "" 														                          ! Cleanup
       PrinterStatus$ = ""
 EndIf 
EndIf  
End Sub 
!--- Fin de la rutina de carga de logos

!--- Carga masiva de Logos en memoria de las impresoras 4610
Sub Alimenta.Logos Public																		        					! Carga logos memoria 4610
INTEGER*2 X.I%																          											! Variables temporales
STRING    X.EXT$, X.A$															    											!
 Write #34; CHR$(1BH) + CHR$(22H) + CHR$(01)	   			  											! Request the checksum of downloaded graphics
 Write #34; CHR$(04H) + CHR$(1BH) + CHR$(23H) + CHR$(01)          						! Erase the downloaded graphics sector
 UE.LOGO.MAXMEN% = 0
 X.EXT$ = "BARCODE1.IMG"
 Call Visores4690(1,"Loading Image ...",X.EXT$,0,"L")                         !
 Call Carga.Logos(X.EXT$,2)                                						  		  ! Carga logo en la impresora
End Sub 																																			!

Sub Impresion.Logo(X.I%) Public                                  							! Rutina impresion logos
INTEGER*1 X.I%, X.J%                                              						! Variable temporal
STRING    X.M$																																!
 If PRT4610.ENABLE <> 0 Then Begin																						! Manejo impresora Termica
    X.M$ = CHR$(1BH) + CHR$(61H) + CHR$(01H) +                               \! Centrado
           CHR$(1DH) + CHR$(2FH) + CHR$(00H) + CHR$(X.I%)         						!
    Write #34;X.M$		   																											! Imprime Logo
 EndIf 																																				!
End Sub 
