!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRCOPSEG.BAS                     *
!Autor         : Oscar Valencia                   *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Copia Seguridad Tienda           *
!**************************************************
! Observaciones
!
! Version 1.0 Noviemnre/2015  Grupo Retail - OVS
!--------------------------------------------------
! Mod 2dic2016
! Se adiciona respaldo archivo de numeración fiscal
! TFNUMFIS
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 16Ago2024
! Se adiciona linea ADXADMBL.L86[STACK[MAX[FFE]] 
! en el archivo .INP de compilcacion para problemas
! de Stack.
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 01Mzo205
! Se modifica copia archivo numeracion fiscal
! TFNUMFIS por TFNMFECO
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%ENVIRON C

String    GLOBAL MSG$, ERRFX$, PROFREE(1), TMP$, IFILE$, COSTO$, \
                 JOURNAL$, TLOG$, LOG$, UE.IP$, UE.USER$, UE.PASSWD$, UE.RUTA$, \
                 FISCAL$, EJOURNAL$, ELOG$, EFISCAL$ 
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
 wait ; 2000
End Sub 

Function LEER.CABECERA
String X.Tmp$
     Open "c:\adxalma.dat" as 91
     Read # 91 ; X.Tmp$
     COSTO$ = Left$(X.Tmp$,4)						   ! Numero del almacen
     Costo$ = Right$(Costo$,3)
     Close 91
End Function
!--- Fin de la funcion de lectura

Sub CALCULO.HORA
String Xh$, Xa$, X.Men$
Integer*4 Xa%, Xb%
Xh$ = Left$(TIME$,4)             ! Toma hora y minutos
Xa% = Val(Xh$)                   ! Toma Valor Numerico
Xa% = Round(Xa%,-2,1)            ! Calcula Proxima Hora de ejecucion
Xa$ = Right$("0000"+Str$(Xa%),4) ! Ajusta Formato
Xa$ = Left$(Xa$,2) + ":" + Right$(Xa$,2)
Xh$ = Mid$(TIME$,3,2)            ! Toma Minutos
Xa% = val(Xh$)                   ! Convierte valor numerico
Xa% = 60 - Xa%                   ! Calcula resto de tiempo para proxima hora
Xb% = (Xa% * 60) * 1000          ! Calcula tiempo en tiempos de Relog
Xa% = Val(Left$(time$,2))        ! Toma Hora 
If Xa% + 1 > 23 Then Xa% = 00 Else \
   Xa% = Xa% + 1 
X.Men$ = "Proximo Proceso "+Right$("00"+Str$(Xa%),2)+":00"
Call Mensaje(X.Men$)
Print "TIEMPO DE ESPERA "+STR$(XB%)
Wait ; Xb%

End Sub
!--- Fin calculo hora

Function Valida.Comunicacion        																				! Valida si hay comunicacion
Integer*1 Valida.Comunicacion																								!
Integer*4 TSA																																!
String    Xcon$, Xopen$, Xfile$, finr$, buffer$, Lec$
          
finr$ = chr$(13)+chr$(10)

Call Mensaje("Generando envio servidor ftp")
Create "ADX_SDT1:ADXHSIGF.DAT" AS 11                      ! Borra basura existente 
Close 11                                                  !

Create "ADX_UDT1:RESPALDO.TXT" AS 3
  
 buffer$ = "open "+UE.IP$+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "prompt"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "bin"+finr$
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

 buffer$ = "send "+Journal$+ " " +finr$                   ! Envio rollos auditoria
 
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "send "+Log$+ " " +finr$                       ! Envio log de trx
 
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "send "+fiscal$+ " " +finr$                    ! Envio numeracion fiscal
 
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
 Call OSSHELL("TYPE ADX_UDT1:RESPALDO.TXT | ftp > adx_udt1:rtacop.txt")   	! Ejecuta el FTP
 Wait ; 2000 																																! 
 BAN.PRG% = TRUE																														! Init variable
 Valida.Comunicacion = False
 Open "ADX_UDT1:RTACOP.TXT" AS 4																						! Abre respuesta
 If BAN.PRG% = TRUE  Then Begin 																						! Si apertura correcta
  If End #4 Then SALIDA.RTA:
  While (1)
    Read #4 ; Line BUFFER$
    TSA = 0 
    BUFFER$ = UCASE$(BUFFER$)
    !TSA = MATCH("226 TRANSFER OK",BUFFER$,1)																! V5
    
    TSA = MATCH("226 TRANSFER COMPLETE",BUFFER$,1)                          ! Para V6
    If TSA > 0 Then Begin 																									!
      Valida.Comunicacion = TRUE																						! Proceso OK
      GoTo SALIDA.RTA
    EndIf Else Valida.Comunicacion = False
  Wend 
  SALIDA.RTA:
    If TSA <= 0 Then Valida.Comunicacion = FALSE														! Retorna falla 
    Close 4 
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
TLOG$ = SLOGNAME$
Call mensaje("Tlog a respaldar "+tlog$)
End Sub 

Sub Generar.Copia
String PROC$, MM$, DD$, HH$, X.men$, Mov$
Integer*1 RtaTrx%
  MM$ = Mid$(Date$,3,2)                   ! Toma el mes
  DD$ = Mid$(Date$,5,2)                   ! Toma el dia
  HH$ = Left$(Time$,2)                    ! Toma la hora
  MM$ = Str$(Val(MM$))
  If MM$ = "10" Then MM$ = "A"
  If MM$ = "11" Then MM$ = "B"
  If MM$ = "12" Then MM$ = "C"
  Call TLOG.ACTUAL	
  Journal$ = "JE"+MM$+DD$+HH$+"."+COSTO$
  Log$     = "LT"+MM$+DD$+HH$+"."+COSTO$
  Fiscal$  = "NF"+MM$+DD$+HH$+"."+COSTO$

  X.Men$ = "Generando Respaldo Seguridad..."
  Call Mensaje(X.Men$)
  !-- Respaldo rollos auditoria
  Call Mensaje(Journal$)
  Proc$ = "ADXNSXZL C "+Journal$+" C:/EJ/J/J"+MM$+DD$+"*.*"   ! Se copian journals formato JEmmddHH.CCC
  Call Osshell(Proc$)
  !-- Respaldo Tlog
  Call Mensaje(Log$)
  Proc$ = "ADXNSXZL C "+Log$+" ADX_IDT4:"+TLOG$+".DAT"      ! Se copia Log Trx Actual   TLmmddHH.CCC
  Call Osshell(Proc$)
  !-- Respaldo numeracion fiscal
  Call Mensaje(Fiscal$)
! Proc$ = "ADXNSXZL C "+Fiscal$+" C:\ADX_IDT1\TFNUMFIS"        ! Se copia numeración fiscal NFmmddHH.CCC

  Proc$ = "ADXNSXZL C "+Fiscal$+" C:\ADX_IDT1\TFNMFECO"        ! Se copia numeración fiscal NFmmddHH.CCC
  Call Osshell(Proc$)

  RtaTrx% = Valida.Comunicacion
  If RtaTrx% = True Then \
   X.Men$ = "Proceso Ejecutado OK.." Else \
   X.Men$ = "Proceso Ejecutado con falla.."	
  Call Mensaje(X.Men$)
  Call Mensaje("Eliminando Temporales...")
  !--- Retira archivo en disco para posterior respaldo
  Proc$ = "del "+Journal$
  Call Osshell(Proc$)
  Proc$ = "del "+Log$
  Call Osshell(Proc$)
  Proc$ = "del "+fiscal$
  Call Osshell(Proc$)
  
End Sub 

Sub CARGA.PARAMETROS																												! Parametros del producto
Integer*1 XLISTO%
String    XTmp$
  BAN.PRG% = True 
  Open "TF:COPSEG" AS 38																										! Apertura parametros
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
IF Ucase$(IFILE$) = "VERSION" THEN BEGIN 
	 !PRINT "MODULO COPIA DE SEGURIDAD TIENDA Ver 1.5 05/Abr/2018 09:05 am"
	 !PRINT "MODULO COPIA DE SEGURIDAD TIENDA Ver 2.1 09/Ago/2023 01:05 am"
	 PRINT "MODULO COPIA DE SEGURIDAD TIENDA Ver 2.2 01/Mzo/2025 11:45 am"
	 Print "Programa para Sistema Operacional V6R2 o superior"
	 Stop
ENDIF
Call LEER.CABECERA
Call CARGA.PARAMETROS
True = -1
False = 0

While (1)                                    ! Ciclo infinito
 Call Calculo.Hora               						 ! Cada Hora Verifica si encuentra archivos para cargar
 Call Generar.Copia             				     ! Generar Copia Seguridad
Wend
Stop

ERROR.PRG:
     Ban.Prg% = False 
     Call TRADUCE.ERROR
     If ERR = "EF" Then Resume
     If ERR = "IH" Then Resume 
     If ERR = "*I" Then Resume 
     If ERR = "SL" Then Resume 
     If (ERR = "OE" OR ERR = "FU") Then Resume

     MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
     Call Mensaje(MSG$)
		 Stop
