!************************************************** 
!Programa      : TRXFTP.BAS                       *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Septimbre 3 de 2.004             *
!Observaciones : Transferencia de archivos via FTP*
!**************************************************
! Modificaciones

%ENVIRON C						   																										! Ambiente de controlador

Integer*1 Global FALSE, TRUE, BAN.PRG%           											! Variables aplicacion
Integer*4 Global POSMEM%
String    Global MENERR$, FILE$, STAT$, ERRFX$, MEN$, FINR$, buffer$, lec$
String    Global UE.IP$, UE.USER$, UE.PASSWD$

%INCLUDE ADX_UPGM:BASROUT.J86																								! Rutinas aplicacion

Sub ADXSERVE(RET,FUNC,PARM1,PARM2) External                  								! Msg background
   Integer*4 RET
   Integer*2 FUNC,PARM1
   String PARM2
End Sub

Function ADXSTART (NAME$,PARM$,MESS$) External
Integer*2 ADXSTART
String NAME$,PARM$,MESS$
End Function


Sub TRADUCE.ERROR                                       										!
Integer*4 HX%, S%, SUM%, SX%
String Z$ 
    HX% = ERRN                                               								! Traduccion de los
    ERRFX$=""                                                								! codigos de error
    For S% = 28 TO 0 STEP -4                                 								! del sistema operativo
        SX% = SHIFT(HX%,S%)                                  								!
        SUM% = SX% AND 000FH                                 								!
        If SUM% > 9 THEN SUM% = SUM% + 55                   							 \!
        Else SUM% = SUM% + 48                                								!
        Z$ = CHR$(SUM%)                                      								!
        ERRFX$ = ERRFX$ + Z$                                 								!
    Next S%                                                  								!
End Sub 

Sub ENTRADA.LOG																															! Mensajes al background
 Call ADXSERVE(0,26,1,MEN$)
 Wait;1200
End Sub 

Sub APERTURA.ARCHIVOS
  BAN.PRG% = TRUE
  Open "ADX_UDT1:TRXTRANS.DAT" KEYED RECL 13 AS 12
End Sub 

Function Valida.Comunicacion(Xfile$)																				! Valida si hay comunicacion
Integer*1 Valida.Comunicacion																								!
Integer*4 TSA																																!
String    Xcon$, Xopen$, Xfile$

Create "ADX_UDT1:ENVIO.TXT" AS 3
  
 buffer$ = "open "+UE.IP$+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "user "+UE.USER$+" "+UE.PASSWD$+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "prompt"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

buffer$ = "bin"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "cd demo"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 
 
 buffer$ = "lcd .."+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 
 
 buffer$ = "lcd adx_udt1"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 
 
 buffer$ = "send "+Xfile$+ " " +finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "dir "+Xfile$+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$  

 buffer$ = "by"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                             ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							 ! Grabacion del registro 

 !buffer$ = "" + finr$
 !LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 !Write Form Lec$; #3 ;buffer$               							 ! Grabacion del registro 
 
 Close 3
 Wait ; 1200

 Call OSSHELL("TYPE ADX_UDT1:ENVIO.TXT | ftp > adx_udt1:rta.txt") 					! Ejecuta el FTP
 Wait ; 1200 																																! 
 BAN.PRG% = TRUE																														! Init variable
 Open "ADX_UDT1:RTA.TXT" AS 4																								! Abre respuesta
 If BAN.PRG% = TRUE  Then Begin 																						! Si apertura correcta
  If End #4 Then SALIDA.RTA:
  While (1)
    Read #4 ; Line BUFFER$
    TSA = 0 
    TSA = MATCH("Transfer complete",BUFFER$,1)
    If TSA > 0 Then Begin 
      Valida.Comunicacion = TRUE																						! Proceso OK
      GoTo SALIDA.RTA
    EndIf 
  Wend 
  SALIDA.RTA:
    If TSA <= 0 Then Valida.Comunicacion = FALSE														! Retorna falla 
    Delete 4 
 EndIf Else Begin 																													! Falla en la apertura
   Valida.Comunicacion = FALSE																							! Retorna falla 
 EndIf 																																			!

End Function 

Sub FINALTRX
     MEN$ = "En Espera de Requerimiento   "+Left$(TIME$,2)+":"+Mid$(TIME$,3,2)
     Call ENTRADA.LOG
     Wait ; 20000
End Sub 


Sub PROCESA.MOVIMIENTOS
Integer*1 XLine%, XRta%
String    XBuffer$, XFile$, Xstat$, Key$, Xhora$, Xfec$
 MEN$ = "En Espera de Requerimiento   "+Left$(TIME$,2)+":"+Mid$(TIME$,3,2)
 Call ENTRADA.LOG
 
 Open "ADX_UDT1:TRXSEND.DAT" AS 39 
 If End #39 Then PASAR																											! Si EOF
 While (1) 																																	! Ciclo de lectura
  NXTLEC:																																		!
     BAN.PRG% = TRUE 																												!
     READ #39; XBuffer$																											! Lectura registro
     If BAN.PRG% = TRUE Then Begin 																					! Si lectura correcta
        FILE$ = Left$(XBuffer$,12)																					! Toma nombre del archivo
        STAT$ = Mid$(XBuffer$,13,1)																					! Status file
        Key$  = Pack$(FILE$)																								! Arma llave 
        Read Form "C6 C3 C3 C1";#12 KEY Key$  ; Xfile$,Xfec$,Xhora$,Xstat$  ! Busca archivo
        If BAN.PRG% = FALSE Then Begin 																			! Si file pendiente por trx
         Xrta% = FALSE 																											! Ctrl de ciclo
         While (XRta% = FALSE)																							! Mientras no trx
          MEN$ = "Transmitiendo "+FILE$
          Call ENTRADA.LOG
          Xline% = VALIDA.COMUNICACION(FILE$)				  											! Valida si hay linea
          If Xline% = TRUE Then Begin 																			! Si hay comunicacion
             MEN$ = "Transmision Exitosa "																	! Msg de alerta
             Call ENTRADA.LOG																								!
             XRta% = TRUE 																									! Trnsm OK
             Write Form "C6 2C3 C1";#12;KEY$,Pack$(DATE$),Pack$(TIME$),"T"
             GoTo PASAR																											!
          EndIf Else Begin 																									! No hay Comunicacion
             MEN$ = "Transmision Fallida "
             Call ENTRADA.LOG 
             MEN$ = "Reintentando Conectar a "+UE.IP$
             Call ENTRADA.LOG 
             Wait ; 20000
          EndIf  
         Wend 
        EndIf 
     EndIf 
  
 Wend 
 
PASAR:
  Call FINALTRX
  GoTo NXTLEC
Return 
 
End Sub 



Sub INIT.VARIABLES
FALSE   = 00 																																! Variables booleana
TRUE    = -1 																																! Variables booleana
POSMEM% = 0																																	! Posicion lectura archivo
FINR$   = CHR$(13)+CHR$(10)																									! Fin de registro
End Sub 

Sub CARGA.PARAMETROS																												! Parametros del producto
Integer*1 XLISTO%
String    XTmp$
  BAN.PRG% = TRUE																														!
  Open "ADX_UDT1:TRCONFIG.DAT" AS 38																				! Apertura parametros
  If BAN.PRG% = FALSE Then Begin 																						! Error en apertura
     MEN$ = "ERROR: Archivo Parametros No Definido"													!
     Call ENTRADA.LOG																												!
     Stop 																																	!
  EndIf 																																		!
  If End #38 Then SALIDA.PARAMETROS																					! Si EOF
  XLISTO% = FALSE																														! Ctrl Ciclo
  While (1)																																	!
     Read #38; Xtmp$																												! Cabecera
     Read #38; Xtmp$																												! Direccion IP
     UE.IP$ = Mid$(XTMP$,30,38)																							!
     Read #38; Xtmp$																												! Usuario FTP
     UE.USER$ = Mid$(XTMP$,30,38)																						! 
     Read #38; Xtmp$																												!
     UE.PASSWD$ = Mid$(XTMP$,30,38)																					! Password
     XLISTO% = TRUE																													!
     GoTo SALIDA.PARAMETROS																									!
  Wend 
  SALIDA.PARAMETROS:
    If Xlisto% = FALSE Then Begin																						! Si parametros incorrectos
     MEN$ = "ERROR: Archivo Parametros Incorrecto "													!
     Call ENTRADA.LOG																												!
     Stop 																																	!
    EndIf 																																	!
    Close 38																																!
End Sub 

!
!--- Bloque Principal
!

ON ERROR GoTo IO.ARCHIVOS
Call INIT.VARIABLES																													! Inicia variables
Call APERTURA.ARCHIVOS																											! Apertura de archivos
Call CARGA.PARAMETROS																												! Parametros del modulo
Call PROCESA.MOVIMIENTOS																										! Procesa movimientos

IO.ARCHIVOS:																																! Rutina control de errores

If ERRF% = 12 and (ERR = "OE" OR ERR = "FU") Then  Begin 										! Error apertura 
    Create PosFile "ADX_UDT1:TRXTRANS.DAT" KEYED 6,,,60000 RECL 13 AS 12   !
    BAN.PRG% = FALSE 
    Resume
 EndIf

If (ERR = "OE" OR ERR = "FU") Then  Begin 																	! Error apertura 
    BAN.PRG% = FALSE 
    Resume
 EndIf

If (ERR = "EF") Then  Begin 										! Error apertura 
    BAN.PRG% = FALSE 
    Resume
 EndIf

 Call TRADUCE.ERROR																													! Arma mensaje de Error
 MEN$ = MENERR$+" "+ERR+"-"+ERRFX$+"-"+Str$(ERRF%)													! Presenta mensaje 
 Call ENTRADA.LOG
 Stop

