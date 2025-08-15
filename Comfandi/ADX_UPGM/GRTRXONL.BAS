!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMOVTRX.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   *
!Observaciones : Decodificacion trx online        *
!**************************************************
! Observaciones:
! Decodificador de Transacciones TrxOnline


String    Global  Men$, Gtmp$, A$, B$,																		 \! Variables aplicacion
                  Str.Log$,																								 \! Trx a procesar
                  Nro.Term$,                                               \! Nro Terminal
                  Nro.Trans$,                                              \! Nro transaccion
                  Fec.Hor$,                                                \! Fecha y hora trx
                  Tipo.Trx$,                                               \! Tipo de trx
                  Nro.String$,                                             \! Nro Strings
                  Nro.Cajero$,                                             \! Operador
                  Pwd.Cajero$,                                             \! Pwd cajero
                  Grossplus$,                                              \! Gross +
                  GrossMinus$,                                             \! Gross -
                  Nro.Strings$,                                            \! Nro de strings
                  Cod.Item$,																							 \! Item vendido
                  Price.Item$,																						 \! Precio de venta
                  Qty.Venta$,																							 \! Cantidad Vendida
                  Cabecera$,                                               \! Cabecera archivo
                  Gr.File.Ruta$,																				   \! ruta encolamiento
                  Nro.fiscal$,                                             \! Nro. Factura fsical
                  Ue.Data2$,                                               \! Temporal trabajo
                  Yfile$                                                    ! Archivo salida
                  
Integer*4 Global  Puntero%,																								 \! Puntero aplicacion
                  PP, K%, J%,																							 \! De control
                  Item.Apnt%                                                ! Item Apuntador

Integer*1 Global  Estado.Ejec%,																						 \! Estado ejecucion aplicacion
                  GrVenta%,                                                \! Control si es una venta
                  Sgn.Vta%,                                                \! Signo venta Item
                  GrError%                                                  ! Control Errores aplicacion

%INCLUDE BASROUT.J86

Sub ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  								! Msg background
   Integer*4 RET
   Integer*2 FUNC,PARM1
   String PARM2
End Sub																																			! Fin Adxserve

Function BIGLEN(A$) EXTERNAL
  Integer*4 BIGLEN
  String A$
End Function

Function ENTRADA.LOG
 If Estado.Ejec% = -1 Then Call ADXSERVE(PP,26,1,MEN$) 										 \! Mensaje Background
  Else Print MEN$																														! Mensaje a pantalla
End Function 

Sub Valide.Directorio(gfec$)																								! Valida directorio encolamiento
String Gfec$  																														  !
Gfec$ = "GRQTX:20" + Left$(Gfec$,6)                                   			! Arma fecha 
Call osshell("Md "+Gfec$)																								    !
Gr.File.Ruta$ = Gfec$ + "\"																									!
End Sub 

Function APERTURA.LOG.TRX     																							! Consulta log actual de trx
String XA$, Trans.log$
Open "EAMTERMS" Keyed Recl 72 as 11         																! Apertura status de la terminal
Read Form "C2 C8 C62";#11 Key Pack$("9999"); XA$, Trans.Log$, XA$						! Lectura registro
Close 11																																		! Cierre archivo

Input "Log a procesar:";Trans.Log$
Men$ = "Archivo TSL [" + Trans.Log$ + "]"																		! Mensaje Tlog a procesar
Call Entrada.Log																														! Msg de alerta
Open Trans.Log$ as 12 Buffsize 1024 Unlocked Nowrite                        ! Apertura log de transacciones
End Function 																																! Fin apertura.log.trx

Sub Graba.Control																											      ! Control consecutivos
String Grcontrol$																													  ! 
Grcontrol$ = right$("000000000" + str$(Puntero%), 8)												! Nro trx procesada
Grcontrol$ = Grcontrol$ + string$(30, "0")																  ! Arma registro
Write Form "C38 C2"; #13, 1; Grcontrol$, chr$(13) + chr$(10)							  ! Almacena registro
End Sub 																																		! Fin Graba.Control

Sub Lectura.Apuntador																												! Lectura apuntador
String Grcontrol$																														!
GrError% = -1																																! Control lectura
Read Form "C40"; #13, 1; Grcontrol$																					! Lectura apuntador
If GrError% = -1 Then 																										 \! Toma dato archivo
   Puntero% = Val(Left$(Grcontrol$, 8)) Else 															 \! Toma apuntador
   Puntero% = 0																															! Inicio archivo	
End Sub																																			! Fin lectura apuntador

Sub Control.Apuntador.Trx																										! Control apuntador registro tlog
GrError% = -1																																! Control de errores
Create Posfile "ADX_UDT1:TXONLPOS.DAT" Direct 1 Recl 40 as 13               ! Crea archivo apuntador
If GrError% <> -1 Then Begin																								! Error de apertura
   Puntero% = 0																															! Reset para barrido log trx
   Men$ = "Creando Apuntador Tlog"																					! Msg alerta
   Call Entrada.Log																													!
   Call Graba.Control																												! Almacena puntero
Endif																																				! Fin control.apuntador.trx
End Sub 																																		! Fin Control.Apuntador.Trx

Sub Graba.Movimiento																												! Genera archivo salida
Integer*4 X.len%
String    Xcab$, Yfinr$, X.Lec$
  YFinr$ = CHR$(13)+CHR$(10)
  YFile$ = Gr.File.Ruta$ + Right$("0000"+Nro.Term$,4) +                    \! Archivo de salida
           Right$("0000"+Nro.Trans$,4) + ".00Q"															!
  Men$ = "Graba:"+Yfile$
  Call Entrada.Log 
  GrError% = -1
  CREATE POSFILE Yfile$ AS 26																								! Crea archivo encolado
  If GrError% = -1 Then Begin 																							!
     X.Len% = bigLen(Str.Log$) - 3  													              
     Xcab$  = "9995" + Right$("000000"+Str$(X.Len%),6)											!
     X.Len% = bigLen(Xcab$)		           																	  ! Toma longitud del registro
     X.Lec$ = "C"+Str$(X.len%)+" C2"										  									! Arma estructura de grabacion
     Write form X.Lec$ ; #26 ; Xcab$, YFINR$                      					! Grabacion del registro
     
     X.Len% = bigLen(Str.Log$) - 1  													              ! Longitud de la trama
     X.Lec$ = "C"+Str$(X.len%) + " C2"  								  									! Arma estructura de grabacion
     Write form X.Lec$ ; #26 ; Str.Log$, Yfinr$                    					! Grabacion del registro
     Close 26																																!
  EndIf																																			!
  MEN$ = "PROCESAMIENTO TRXONLINE Ver. 1.0, Esperando..."
  Call Entrada.Log 
End Sub 																																		!


Sub GETUNPK																																	! Desempaqueta dato String
  K% = Match(":",B$,J%) 																										! Busca separador de campo
  If (K%-J%) > 0 Then 																										 \! Si hay datos
     A$ = UnPack$(Mid$(B$,J%,K%-J%)) Else A$ = ""														! desempaqueta campo
  J% = K% + 1 																															! Apuntador proximo String
End Sub 																																		! Fin desempaqueta String


Sub S0																																			! Cabecera de trx
  J% = 3																																		! Apuntador para extraer datos
  Item.Apnt% = 0
  GrVenta% = 0																															! Control trx venta
  Nro.fiscal$ = ""
  Call GetUnpk																															! Toma Nro Terminal
  Nro.Term$ = A$
  Call GetUnpk																															! Toma Nro Trx
  Nro.Trans$ = A$
  Call GetUnpk																															! Fecha y hora movimiento
  Fec.Hor$ = A$
  Call GetUnpk																															! Tipo de transaccion
  Tipo.Trx$ = A$  
  Call GetUnpk																															! Nro de strings
  Nro.Strings$ = A$
  Call GetUnpk																															! Operador
  Nro.Cajero$ = A$
  Call GetUnpk																															! Password
  Pwd.Cajero$ = A$
  Call GetUnpk																															! Gross +
  Grossplus$ = A$
  Call GetUnpk																															! Gross -
  GrossMinus$ = A$
  GrVenta% = -1              
  men$ = "Procesando "+Nro.term$+" - "+Nro.Trans$+" OFFSET "+STR$(PUNTERO%)
  Call Entrada.Log																												  ! Procesa trx 
  Call Valide.Directorio(Fec.Hor$)																				  ! Valida directorio
  Call Graba.Movimiento																								      ! Genera encolamiento
End Sub 																																		! Fin cabecera trx

Sub S1 																																			! Venta de Item
End Sub 																																		! Fin venta Item

Sub S2																																			! Extension de Item
End Sub 																																		! Fin Extension Item

Sub S99																																			! User data
  J% = 3																																		! Apuntador para extraer datos
End Sub 																																		! Fin user data

Sub S21																																			! Proceso cierre de dia
String XA$, Trans.Log$
  Men$ = "PROCESANDO CIERRE DIARIO..."																			!
  Call Entrada.Log																													! Msg sistema
  Call Graba.Movimiento																								      ! Genera encolamiento
  Puntero% = 0																															! Inicializa Puntero
  Call Graba.Control																												! Actualiza apuntador
  Men$ = "PROCESANDO FINALIZADO..."																			    !
  Call Entrada.Log																													! Msg sistema
  Stop 
End Sub 

Sub Recorre.Tlog																														! Barre Tlog
Integer*4 Q%, P%, A%
  While (1)																																	! Ciclo Infinito
   Lectura.Tlog:
    Read #12; Line Str.Log$																									! Lectura Tlog
    Puntero% = PTRRTN																												! Actualiza posicion Tlog
    Call Graba.Control																											! Actualiza apuntador
    If Len(Str.Log$) < 12 Then GoTo Lectura.Tlog														! Si basura en Tlog
    Str.Log$ = Str.Log$ + ","																								!
    Q% = 1 																																	! Reset Variable
    While (Q% < Len(Str.Log$))																							! Validar si String completos
      P% = Match(",",Str.Log$,Q%) 																					! Encontrar delimitador coma (,)
      If (P% - Q%) < 3 Then Begin 																					! Revisar falta de strings
	      GoTo Lectura.Tlog		   																							! Lectura siguiente registro
	    EndIf																																	!
      B$ = Mid$(Str.Log$,Q%+1,(P%-Q%)-2) 																		! Analiza cadena de String
      B$ = B$ + ":" 																												! Suma un semi colon
      Q% = P% + 1 																													! Prepara siguiente informacion
      A% = Val(Unpack$(Left$(B$,1))) 																				! Determina tipo de String
      If A% =  0 Then Call  S0																							! Cabecera de trx
      If A% = 21 Then Call  S21																						! Cierre de dia
      If GrVenta% = -1 Then Begin
       If A% =  1 Then Call  S1																							! Venta de Item
       If A% =  2 Then Call  S2																							! Extension de Item
       If A% = 99 Then Call  S99																						! User data
      EndIf Else GoTo Lectura.Tlog  	
    Wend 																																		! Fin analisis tlog
  Wend 																																			! Fin del ciclo
End Sub 																																		! Fin recorre Tlog

!--- Bloque principal
On Error GoTo Err.Trx
Estado.Ejec% = 0																														! Modo ejecucion interactivo
Gtmp$ = COMMAND$                                             								! Dato S.O
If UCase$(Gtmp$) = "VERSION" Then Begin																			! Consulta Version
   Print "Encolamiento transacciones TrxOnline Ver.1.0 20/Ene/2022 Gretail"
   Stop 
EndIf
If Gtmp$ = "BACKGRND" Then Estado.Ejec% = -1																! Ejecucion Background
Call Apertura.Log.Trx																												! Apertura Log de trx
Call Control.Apuntador.Trx																									! Apertura contador trx
Call Lectura.Apuntador 																											! Toma posicion Tlog
Point 12; Puntero%																													!	Toma Offset Tlog
MEN$ = "PROCESAMIENTO TRXONLINE Ver. 1.0, Esperando..."
Call Entrada.Log
Call Recorre.Tlog																														! Recorrer Tlog
Stop 																																				! Fin de programa

Err.Trx:
GrError% = 0																																! Se reporta Error en Appl

If ERR = "EF" Then Resume 																									! Error lectura
If ERR = "IH" Then Resume 																									! Error basura
If ERRF% = 11 AND (ERR = "OE" OR ERR = "FU") Then Begin											! Error apertura eamterms
    MEN$="Error: APERTURA ESTADO DE TERMINALES "														! Msg de Error
    Call Entrada.Log																												! Presentacion mensaje
    Stop 																																		! Fin programa
EndIf																																			  !
If ERRF% = 12 AND (ERR = "OE" OR ERR = "FU") Then Begin											! Error apertura eamterms
    MEN$="Error: APERTURA LOG DE TRANSACCIONES "														! Msg de Error
    Call Entrada.Log																												! Presentacion mensaje
    Stop 																																		! Fin programa
EndIf																																			  !
If ERRF% = 13 AND (ERR = "OE" OR ERR = "FU") Then Begin											! Error apertura eamterms
    Create Posfile "ADX_UDT1:GRTRXPOS.DAT" Direct 1 Recl 40 as 13          \! Crea archivo apuntador
           compound perupdate																								!
    Resume 																																	! Retorna ejecucion programa
EndIf																																			  !
If ERRF% = 26 AND (ERR = "OE" OR ERR = "FU") Then Begin											! Error apertura eamterms
    MEN$="Err: No Graba Mov "+Yfile$
    Call Entrada.Log																												! Presentacion mensaje
    WAIT ; 1000
    Resume 																																	! Retorna ejecucion programa
EndIf																																			  !

MEN$ = "Error de Aplicacion "+ERR+" Sesion "+Str$(ERRF%) 
Call Entrada.Log																														! Presentacion mensaje

Stop 																																				! Fin de programa

