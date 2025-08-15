!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRRESMOV.BAS                     *
!Autor         : Oscar Valencia                   *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Respaldo movimientos en cierre   *
!**************************************************
! Observaciones
!
! Mod 2dic2016
! Se adiciona respaldo archivo de numeración fiscal
! TFNUMFIS
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%ENVIRON C

String    GLOBAL MSG$, ERRFX$, PROFREE(1), TMP$, IFILE$, COSTO$, \
                 JOURNAL$, TLOG$, LOG$, UE.IP$, UE.USER$, UE.PASSWD$, UE.RUTA$, \
                 FecMov$, Fiscal$
Integer   GLOBAL I%, CTRL%, BAN.PRG%, True, False
Integer*2 GLOBAL  WI2%, RTA.PROMO%

String GLOBAL AA$, BB$, CC$, DD$, LLAVE$

%INCLUDE ADX_UPGM:BASROUT.J86

Sub Adxserve(RET,FUNC,PARM1,PARM2) EXTERNAL
Integer*4 RET
Integer*2 FUNC,PARM1
String PARM2
End Sub

Function ADXERROR(TERM,MSGGRP,MSGNUM,SEVERITY,EVENT,UNIQUE$) EXTERNAL
Integer*2 	TERM,MSGNUM
Integer*1	SEVERITY,MSGGRP,EVENT
String		UNIQUE$
End Function

Sub ADXCOPYF(RETC,INFILE,OUTFILE,OPT0,OPT1) EXTERNAL
   Integer*4 RETC
   String INFILE, OUTFILE
   Integer*2 OPT0, OPT1
End Sub

Function TRADUCE.ERROR                                       !
Integer*4 HX%, S%, SX%, SUM%
String    Z$
    HX% = ERRN                                               ! Traduccion de los
    ERRFX$=""                                                ! codigos de error
    FOR S% = 28 TO 0 STEP -4                                 ! del sistema operativo
        SX% = SHIfT(HX%,S%)                                  !
        SUM% = SX% AND 000FH                                 !
        If SUM% > 9 Then SUM% = SUM% + 55                   \!
        Else SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                      !
        ERRFX$ = ERRFX$ + Z$                                 !
    NEXT S%                                                  !
End Function

Sub Mensaje(Xmsg$)
String XMSG$
 If IFILE$ = "BACKGRND" Then Call ADXSERVE(0,26,1,XMSG$) \
  Else Print XMSG$
 
End Sub 

Function LEER.CABECERA
String X.Tmp$
     Open "c:\adxalma.dat" as 91
     Read # 91 ; Line X.Tmp$
     COSTO$ = Left$(X.Tmp$,4)						   ! Numero del almacen
     Costo$ = Right$(Costo$,3)
     FecMov$ = Mid$(X.TMP$,5,8)            ! Fecha de movimiento
     Close 91
End Function
!--- Fin de la funcion de lectura

Function Valida.Comunicacion        																				! Valida si hay comunicacion
Integer*1 Valida.Comunicacion																								!
Integer*4 TSA																																!
String    Xcon$, Xopen$, Xfile$, finr$, buffer$, Lec$
          
finr$ = chr$(13)+chr$(10)

Call Mensaje("Generando envio servidor ftp")
Create "ADX_SDT1:ADXHSIGF.DAT" AS 11                      ! Borra basura existente 
Close 11                                                  !

Create "ADX_UDT1:GRRESMOV.TXT" AS 3
  
 buffer$ = "open "+UE.IP$+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "prompt"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "bin"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "hash"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "ls"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "user "+UE.USER$+" "+UE.PASSWD$+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "cd "+UE.RUTA$+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 
  
 buffer$ = "put "+Journal$+ " " +finr$                   ! Envio rollos auditoria
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "put "+Log$+ " " +finr$                       ! Envio log de trx
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 
 
 buffer$ = "put "+fiscal$+ " " +finr$                     ! Envio numeracion fiscal
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "close "+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$  

 buffer$ = "by"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                             ! Formatea la salida
 Write Form Lec$; #3 ;buffer$  				   ! Grabacion del registro 

 Close 3
 Wait ; 1200
 Call Mensaje("Enviando Informacion...") 
 Call OSSHELL("TYPE ADX_UDT1:GRRESMOV.TXT | FTP > ADX_UDT1:COP.TXT") 					! Ejecuta el FTP
 Wait ; 1200 																																! 
 BAN.PRG% = TRUE																														! Init variable
 Valida.Comunicacion = False
 wait ; 2000
 Open "ADX_UDT1:COP.TXT" AS 4																								! Abre respuesta
 If BAN.PRG% = TRUE  Then Begin 																						! Si apertura correcta
  If End #4 Then SALIDA.RTA:
  While (1)
    Read #4 ; Line BUFFER$
    TSA = 0 
    BUFFER$ = UCASE$(BUFFER$)
    TSA = MATCH("226 FILE TRANSFER COMPLETED",BUFFER$,1)										! Para V5
    
    !TSA = MATCH("226 TRANSFER COMPLETE",BUFFER$,1)                          ! Para V6
    
    If TSA > 0 Then Begin 
      Valida.Comunicacion = TRUE																						! Proceso OK
      GoTo SALIDA.RTA
    EndIf Else Valida.Comunicacion = False
  Wend 
  SALIDA.RTA:
    If TSA <= 0 Then Valida.Comunicacion = FALSE														! Retorna falla 
    Delete 4 
 EndIf Else Begin 																													! Falla en la apertura
   Valida.Comunicacion = FALSE																							! Retorna falla 
 EndIf 																																			!
 Call Mensaje("Proceso Terminado..")
End Function 

Sub TLOG.ACTUAL    																													! DEVUELVE EL ARCHIVO ACTUAL  
String LLAVE$, TERM$, \
       SLOGNAME$,OSLOGNAME$,FECHA.CIER$,RESERVED$
Integer*4 ClosePNT
BAN.PRG% = True 
Open "EAMCSCF1" KEYED RECL 36 AS 37 NOWRITE NODEL														! Busca Eamtran Activo 
LLAVE$ = PACK$("9998")
READ Form "C2 C8 C8 I4 C5 C9";#37  KEY LLAVE$; TERM$, \
       SLOGNAME$,OSLOGNAME$,ClosePNT,FECHA.CIER$,RESERVED$  
Close 37
TLOG$ = OSLOGNAME$
Call mensaje("Tlog a respaldar "+tlog$)
End Sub 

Sub Generar.Copia
String PROC$, MM$, DD$, HH$, X.men$, Mov$, SOP$
Integer*1 RtaTrx%
  Sop$ = Mid$(FecMov$,3,6)                   ! Fecha para soporte
  Call TLOG.ACTUAL	
  Journal$ = "C:\ADX_UDT1\JE"+Sop$+"."+COSTO$
  Log$     = "C:\ADX_UDT1\TL"+Sop$+"."+COSTO$
  Fiscal$  = "C:\ADX_UDT1\NF"+Sop$+"."+COSTO$
  DD$ = Right$(Fecmov$,2)			  ! toma dia del movimiento
  MM$ = Mid$(FecMov$,5,2)       ! Toma el mes
  MM$ = Str$(Val(MM$))
  If MM$ = "10" Then MM$ = "A"
  If MM$ = "11" Then MM$ = "B"
  If MM$ = "12" Then MM$ = "C"
  
  X.Men$ = "Generando Respaldo Seguridad... Ver 1.2"
  Call Mensaje(X.Men$)
  Call Mensaje(Log$)
  Call Mensaje(Journal$)

  RtaTrx% = Valida.Comunicacion
  If RtaTrx% = True Then \
   X.Men$ = "Proceso Ejecutado OK.." Else \
   X.Men$ = "Proceso Ejecutado con falla.."	
  Call Mensaje(X.Men$)
  
End Sub 

Sub CARGA.PARAMETROS																												! Parametros del producto
Integer*1 XLISTO%
String    XTmp$
  BAN.PRG% = True 
  Open "TF:RESMOV" AS 38																										! Apertura parametros
  If End #38 Then SALIDA.PARAMETROS																					! Si EOF
  XLISTO% = 0																																! Ctrl Ciclo
  While (1)																																	!
     Read #38; Xtmp$		
     Read #38; Xtmp$																												! Direccion IP
     UE.IP$ = Mid$(XTMP$,30,38)	
     Read #38; Xtmp$																												! Usuario FTP
     UE.USER$ = Mid$(XTMP$,30,38)    
     Read #38; Xtmp$		        																						! Password
     UE.PASSWD$ = Mid$(XTMP$,30,38)     
     Read #38; Xtmp$                    																		! Ruta destino archivo
     UE.RUTA$ = Mid$(XTMP$,30,50)
     XLISTO% = -1
     GoTo SALIDA.PARAMETROS
  Wend 
  SALIDA.PARAMETROS:
    If Xlisto% = 0 Then Begin			    																			! Si parametros incorrectos
     Call MENSAJE("ERROR: Archivo Parametros Incorrecto ") 									!
     Stop 																																	!
    EndIf 																																	!
    Close 38																																!
End Sub 

!---
! Bloque Principal
!---

On Error GoTo ERROR.PRG
IFILE$ = Command$                						 ! Como esta ejecutando
IF IFILE$ = "VERSION" THEN BEGIN 
	 PRINT "MODULO RESPALDO MOVIMIENTOS CIERRE Ver 1.2 02/Dic/2016 10:56 am"
	 Stop
ENDIF
Call LEER.CABECERA
Call CARGA.PARAMETROS
True = -1
False = 0
Call Generar.Copia             				     ! Generar Copia Seguridad
Stop

ERROR.PRG:
     Ban.Prg% = False 
     Call TRADUCE.ERROR
     If ERR = "EF" Then Resume
     If ERR = "IH" Then Resume 
     If ERR = "*I" Then Resume 
     If (ERR = "OE" OR ERR = "FU") Then Resume

     MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
     Call Mensaje(MSG$)
		 Stop
