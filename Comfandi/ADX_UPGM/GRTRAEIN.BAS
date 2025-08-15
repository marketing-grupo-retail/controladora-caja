!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRTRAEIN.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Recupera archivos de tiendas     *
!**************************************************
! Modificacion
! < Versión 1.0 26 Nov 2019 >


%ENVIRON C						   												! Ambiente de controlador

String    Global ovs$, OLD.TRX$, OLD.CAJA$, Ue.Convenio$, Ue.Plan$
String    Global Ue.Recibo$, Recibos$(2), Ue.Salida$, Dato$, Ind.Mov$
String    Global Ue.Ip$, Ue.User$, Ue.Passwd$, Ue.Tiendas$(2), UE.Costo$
Integer*4 Global NRO.PAGOS%, Ind.Colegio%, X.Cpag%, X.tot%, XI%
Integer*1 Global Ind.Trx%, PROC%

%Include BKCNVVAR.011					                  ! Variables del programa
%Include DMEXTR.J86    		            ! Inclucion Libreria Display Manager
%Include POSPRUTI.BAS				  	                ! Rutinas Comunes
%Include BASROUT.J86


Sub Carga.tiendas
String XA$,XB$,XC$
  LOCATE 8,1 : Print "Cargando Lista de Tiendas... "
  Open "ADX_UDT1:TIENDAS.TXT" As 11 NOWRITE NODEL
  If End #11 Then SALIDA.TIENDAS																					! Si EOF
  Dim ue.tiendas$(500,2)
  X.TOT% = 1
  While (1)																																	!
     Read #11; XA$,XB$,XC$
     ue.tiendas$(X.TOT%,0) = XA$   ! Centro costo
     ue.tiendas$(X.TOT%,1) = XB$   ! Nombre
     ue.tiendas$(X.TOT%,2) = XC$   ! IP
     x.tot% = x.tot% + 1
  Wend 
  SALIDA.TIENDAS:
  Close 11 
End Sub 


Function Valida.Comunicacion																								! Transmite credito social
Integer*1 Valida.Comunicacion																								!
Integer*4 TSA																																!
String    Xcon$, Xopen$, Xfile$, buffer$, Lec$

 Create "ADX_SDT1:ADXHSIGF.DAT" AS 11                      ! Borra basura existente 
 Close 11                                                  !
 FINR$ = Chr$(13) + Chr$(10)
 Create "C:\merca\FLRECUP.TXT" AS 3
 Xfile$ = "F"+ue.costo$+"110?.DAT"
  
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

 buffer$ = "cd adx_udt1 "+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "get f"+ue.costo$+"1102.DAT"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "get f"+ue.costo$+"1103.DAT"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "get f"+ue.costo$+"1104.DAT"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "get f"+ue.costo$+"1105.DAT"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "get f"+ue.costo$+"1106.DAT"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "close "+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$  

 buffer$ = "by"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$  				                    ! Grabacion del registro 

 Close 3
 Wait ; 1200
  LOCATE 10,1  : Print String$(78," ")
  LOCATE 10,1  : Print "Recuperando  "+Xfile$
  LOCATE 11,1  : Print "-------------"
  Call OSSHELL("TYPE C:\merca\FLRECUP.TXT | ftp > C:\merca\rtatrae.txt") 					! Ejecuta el FTP
 Wait ; 1200 																																! 
 BAN.PRG$ = "1"																														! Init variable
 Valida.Comunicacion = 0     
 Open "C:\merca\rtatrae.txt" AS 4																								! Abre respuesta
 If BAN.PRG$ = "1"  Then Begin 																						! Si apertura correcta
  If End #4 Then SALIDA.RTA:
  While (1)
    Read #4 ; Line BUFFER$
    TSA = 0 
    BUFFER$ = UCASE$(BUFFER$)
    TSA = MATCH("TRANSFER COMPLETED",BUFFER$,1)
    If TSA > 0 Then Begin 
      Valida.Comunicacion = -1  																						! Proceso OK
      GoTo SALIDA.RTA
    EndIf Else Valida.Comunicacion = 0    
  Wend 
  SALIDA.RTA:
    If TSA <= 0 Then Valida.Comunicacion = 0     													! Retorna falla 
    Delete 4 
 EndIf Else Begin 																													! Falla en la apertura
   Valida.Comunicacion = 0    																							! Retorna falla 
 EndIf 																																			!

End Function 

Sub Pantalla
   Clears
   Locate 2, 4: Print CHR$(218)+String$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****       RECUPERACION INTERFACES TIENDAS           ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Noviembre 26 2019  G.R. ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+String$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
End Sub 

On Error GoTo PROCESA 
Call Pantalla  
Call Carga.tiendas
For XI% = 1 To x.tot%
    lOCATE 9, 1 : Print "PROCESANDO TIENDA "+ue.tiendas$(XI%,0)+" "+ue.tiendas$(XI%,1)
    UE.Costo$ = ue.tiendas$(XI%,0)
    UE.IP$ = ue.tiendas$(XI%,2)
    UE.USER$ = "anonymous"
    UE.PASSWD$ = ""
    PROC% = Valida.Comunicacion
Next XI%
Locate 22,1 : Print "PROCESO TERMINADO "
Stop 

PROCESA:
ERRORCOD$ = ERR
BAN.PRG$ = "0"
If ERR = "EF" Then Resume
If ERRF% = 11 AND (ERR = "OE" OR ERR = "FU") Then  Begin
    MEN$="Error: No Se Logro Abrir lista de tiendas"
    Print MEN$
    Stop 
EndIf

Call TRADUCE.ERROR
MEN$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
Locate 22,1: Print String$(78," ")
Locate 22,1: Print MEN$
Stop
!*********************************************************************