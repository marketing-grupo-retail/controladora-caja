! LISTA EL MAESTRO DE ARTICULOS, EJEMPLO DE LECTURA DIRECT FILE
! Modulo Modificado por Grupo Retail - OVS 10 Mzo 2009
!---------------------------------------------------------------
! Se modifica el archivo, de salida adicionado la tarifa del iva,
! nit del proveedor, y si el articulo pertenece a la lista de condensa
! Ejemplo : 013,000000117160,0000010000,16,890123456,T
! Ajustado por Grupo Retail 10 Julio 2.009 OVS
!--------------------------------------------------------------------

%ENVIRON C						   																		 ! Ambiente de controlador
Integer*1 Global U.Error%
Integer*2 Contador%
Integer*4 Total.Itm%, Xval%
String Global UE.MSG$, ERRFX$, Finr$, XFecha$,Almacen$,Lista$, Archivo$, Men$, Tiva$(1)
Integer*1 Global Dptos(1)
String Global DptoIni$, Dptofin$, Detalla$

Sub ADXSERVE(RET,FUNC,PARM1,PARM2) External                  ! Funciones S.O.
   Integer*4 RET
   Integer*2 FUNC,PARM1
   String PARM2
End Sub

Function GETN1(P1$,P2) External
     Integer*1 GETN1
     String P1$
     Integer*2 P2
End Function 

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
End Function

Sub RECORRER.ITEMR
Integer*4 REC.NO, I%, Blk.Num, MAX.R.SEC, X, R.S, R, Ncli%, X.Len%, XIR1%, XIR1A%, UE.INDI1%, UE.TARIFA%
String H$, PZERO$, REG$, C01$, C02$, C03$, C04$, C05$, C06$, KEY$, X.Lec$, XCODENSA$, Prov$, IND1$, UE.BIN$, Tarifa$
String Rbuffer$, tag$
Integer*2 Key.Len, Rec.Len
Dim Dptos(5000)
Total.Itm% = 0
Ncli% = 0 
Contador%=0
Open "EAMITEMR" DIRECT RECL 512 AS 33 buffsize 32640  	     ! Apertura control de promociones
REC.NO = 1					             ! Initialize Counter
I% = 1						             ! Initialize Counter
Read Form "T43 I4 I2 T55 I2 C456"; #33,REC.NO;              \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		             ! 
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record
U.Error% = -1                                                ! Control de errores
Locate 8,1 : Print "TOTAL BLOQUES PROCESAR :"+STR$(BLK.NUM)
For REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  REG$=""
  Read Form "T5 C508"; #33, REC.NO;  H$                      ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = Mid$(H$,X,KEY.LEN)                ! Extract First key
  Locate 9,1 : Print "PROCESANDO BLOQUE :"+STR$(REC.NO)
  While KEY$  NE  PZERO$                                     ! Inside sector loop 
    R.S = R.S + 1                                            ! Records On This Sector 
    R = R + 1                                                !
    C01$= Unpack$(MID$(H$,X+0,6))                            ! Codigo 
    ind1$ = MID$(H$,X+0,10)
    UE.INDI1% = GETN1(ind1$,7)                               ! INDICAT 1
    XIR1A% = Val(Unpack$(MID$(H$,X+8,1)))                    ! IR INDICAT1A
    C04$= Unpack$(MID$(H$,X+10,2))                           ! Dpto
    C06$= Unpack$(MID$(H$,X+17,5))                           ! PRECIO
    C05$= (MID$(H$,X+24,18))                                 ! Descripcion
    C03$= Unpack$(MID$(H$,X+9,1))                            ! INDICAT2
    
    If UE.INDI1% AND 80H Then                               \! Tax plan A
      UE.TARIFA% = 1                                        \!
    Else IF UE.INDI1% AND 40H Then                          \! Tax plan B
      UE.TARIFA% = 2                                        \! 
    Else If UE.INDI1% AND 20H Then                          \! Tax plan C
      UE.TARIFA% = 3                                        \!
    Else If UE.INDI1% AND 10H Then                          \! Tax plan D
      UE.TARIFA% = 4                                        \!
    Else UE.TARIFA% = 8                                      ! Excluido
    Total.Itm% = Total.Itm% + 1  														 ! Total de items
    
    TARIFA$ = STR$(UE.TARIFA%)
    Dptos(Val(C04$)) = 1
    
    If Ucase$(Detalla$) = "S" Then BEGIN  
     If (Val(C04$) >= Val(DptoIni$)) And (Val(C04$) <= Val(DptoFin$)) Then Begin
        Rbuffer$ = Almacen$+","+C01$+","+C05$+","+C04$+","+C06$+","+TARIFA$
        locate 20,1 : Print Rbuffer$
        X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
        X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
        Write form X.Lec$; #77 ; Rbuffer$, Finr$           							! Graba registro
     EndIF
    ENDIF 

    X = X + REC.LEN                                          ! Index to next key
    KEY$ = Mid$(H$,X,KEY.LEN)                                ! Pick up next key
    If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
  Wend
 Next REC.NO
 Close 33
 I% = 0
 If Ucase$(Detalla$) = "N" Then Begin 
    Print "Generando Plano Departamentos"
    For I% = 1 To 5000
       If Dptos(i%) = 1 Then Begin
          Rbuffer$ = Almacen$+","+str$(i%)
          X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
          X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
          Write form X.Lec$; #77 ; Rbuffer$, Finr$           							! Graba registro
       EndIf
    Next I%
 EndIf
 Print "Procedimiento Terminado"
End Sub 

Sub LEER.CABECERA
 String PARM2$
 Integer*2 PARM1,RET
 Call ADXSERVE(RET,4,PARM1,PARM2$)                 ! 
 ALMACEN$ = RIGHT$("000"+STR$(VAL(MID$(PARM2$,1,4))),3)
End Sub 
!--- Fin de la funcion de lectura


On Error GoTo Error.PRG				! Control de errores
CLEARS
Call Leer.Cabecera				! Toma numero de tienda del store system
FINR$=CHR$(13)+CHR$(10)
ARCHIVO$="ADX_UDT1:PLANODPT."+Almacen$        ! Nombre archivo de salida
CREATE ARCHIVO$ As 77
Print "Grupo Retail Ltda - Generación Archivo Plano Departmentos Maestro Articulos"
LOCATE 2,1 : Print "Creando Estructura :"+archivo$
Input "Departamento inicial  :";DptoIni$
Input "Departamento Final    :";Dptofin$
Input "Detalla Productos S/N :";Detalla$
U.Error% = -1
Xval% = Val(Dptoini$)
If U.Error% = 0 Then Begin
   Print "Dato Depto Inicial Invalido"
   Stop 
EndIF	
U.Error% = -1
Xval% = Val(Dptofin$)
If U.Error% = 0 Then Begin
   Print "Dato Depto Final Invalido"
   Stop 
EndIf
	
If Val(Dptofin$) < Val(DptoIni$) Then Begin
	 Print "Rango Departamentos Invalidos"
	 Close 77 : Stop 
EndIf

Call RECORRER.ITEMR				! Recorre maestro articulos
Close 77
Stop						! Para Ejecucion del programa


Error.Prg:
  U.Error% = 0
  If ERR = "IH" Then Resume
  CALL TRADUCE.ERROR
  UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  Print UE.MSG$
  Stop 
